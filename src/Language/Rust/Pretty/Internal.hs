{-|
Module      : Language.Rust.Pretty.Internal
Description : Rust pretty printer
Copyright   : (c) Alec Theriault, 2017-2018
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : portable

The pretty printing facilities in this file are re-exported to 'Language.Rust.Pretty' via the
'Pretty' and 'PrettyAnnotated' classes. There may be invariants in this module that are not properly
documented. 
-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# OPTIONS_HADDOCK hide, not-home #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Rust.Pretty.Internal (

  -- * Pretty printers
  -- As much as possible, these functions should say which function in the @rustc@ printer they are
  -- emulating.

  -- ** Top level
  printSourceFile,
  
  -- ** General
  printMutability,
  printUnsafety,
  printFnArgsAndRet,
  printIdent,
  
  -- ** Paths
  printPath,
  
  -- ** Attributes
  printAttr,
  
  -- ** Literals
  printLit,
  printLitSuffix,
  printLitTok,
  
  -- ** Expressions
  printExpr,
  printAbi,
  printUnOp,
  printBinOp,
  printField,
  printRangeLimits,
 
  -- ** Types and lifetimes
  printType,
  printGenerics,
  printLifetime,
  printGenericParam,
  printBound,
  printWhereClause,
  printWherePredicate,
  printPolyTraitRef,
  printTraitRef,
  
  -- ** Patterns
  printPat,
  printBindingMode,
  printFieldPat,
  
  -- ** Statements
  printStmt,
  
  -- ** Items
  printItem,
  printForeignItem,
  printImplItem,
  printTraitItem,
  printPolarity,
  printStructField,
  printVariant,
  printUseTree,
  printVis,
  
  -- ** Blocks
  printBlock,

  -- ** Token trees
  printToken,
  printTt,
  printTokenStream,
  printNonterminal,
  printMac,

  -- * Utilities
  module Language.Rust.Pretty.Util,
) where

import Language.Rust.Pretty.Literals
import Language.Rust.Pretty.Util

import Language.Rust.Data.Ident
import Language.Rust.Data.Position

import Language.Rust.Syntax.AST
import Language.Rust.Syntax.Token

import Data.Text.Prettyprint.Doc hiding ( (<+>), hsep, indent, vsep )

import Data.Maybe               ( maybeToList, fromMaybe )
import Data.Foldable            ( toList )
import Data.List                ( unfoldr )
import qualified Data.List.NonEmpty as N

-- | Print a source file
printSourceFile :: SourceFile a -> Doc a
printSourceFile (SourceFile shebang as items) = foldr1 (\x y -> x <#> "" <#> y) ls
  where ls = concat [ maybeToList ((\s -> "#!" <> pretty s) <$> shebang)
                    , [ vcat [ printAttr a False | a <- as ] ]
                    , punctuate line' (printItem `map` items)
                    ]

-- | Print a name
printName :: Name -> Doc a
printName = pretty

-- | Print an identifier
printIdent :: Ident -> Doc a
printIdent (Ident s False _) = pretty s
printIdent (Ident s True _) = "r#" <> pretty s

-- | Print a type (@print_type@ with @print_ty_fn@ inlined)
-- Types are expected to always be only one line
printType :: Ty a -> Doc a
printType (Slice ty x)          = annotate x ("[" <> printType ty <> "]")
printType (Array ty v x)        = annotate x ("[" <> printType ty <> ";" <+> printExpr v <> "]")
printType (Ptr mut ty x)        = annotate x ("*" <> printFullMutability mut <+> printType ty)
printType (Rptr lt mut ty x)    = annotate x ("&" <> perhaps printLifetime lt <+> printMutability mut <+> printType ty)
printType (Never x)             = annotate x "!"
printType (TupTy [elt] x)       = annotate x (block Paren True ""  mempty [ printType elt <> "," ])
printType (TupTy elts x)        = annotate x (block Paren True "," mempty (printType `map` elts))
printType (PathTy Nothing p x)  = annotate x (printPath p False)
printType (PathTy (Just q) p x) = annotate x (printQPath p q False)
printType (TraitObject bs x)    = let prefix = if null (N.tail bs) then "dyn" else mempty
                                  in annotate x (printBounds prefix (toList bs))
printType (ImplTrait bs x)      = annotate x (printBounds "impl" (toList bs))
printType (ParenTy ty x)        = annotate x ("(" <> printType ty <> ")")
printType (Typeof e x)          = annotate x ("typeof" <> block Paren True mempty mempty [ printExpr e ])
printType (Infer x)             = annotate x "_"
printType (MacTy m x)           = annotate x (printMac False mempty m)
printType (BareFn u a ps d x)   = annotate x (printForParams ps
                                               <+> printFnHeader (FnHeader u NotAsync NotConst a)
                                               <> printFnArgsAndRet d)

-- | Print a macro (@print_mac@)
printMac :: Bool  -- ^ add a semicolon unless the delimiter is a brace
         -> Doc a -- ^ put after the @!@ but before the body
         -> Mac a
         -> Doc a
printMac d s (Mac path del ts x) = annotate x (printPath path False <> "!" <> s <> body <> end)
  where body = block (intoDelim del) True mempty mempty [ printTokenStream ts ]
        end | d, del /= BraceDelim = ";"
            | otherwise = mempty

intoDelim :: Delimiter -> Delim
intoDelim ParenthesisDelim = Paren
intoDelim BracketDelim = Bracket
intoDelim BraceDelim = Brace

-- | Given two positions, find out how to print appropriate amounts of space between them
printSpaceBetween :: Bool -> Span -> Span -> Maybe (Doc a)
printSpaceBetween spaceNeeded (Span _ (Position _ y1 x1)) (Span (Position _ y2 x2) _)
  | y2 == y1 && x2 > x1 = Just $ hcat (replicate (x2 - x1) space)
  | y2 > y1             = Just $ hcat (replicate (y2 - y1) line) <> column (\x1' -> hcat (replicate (x2 - x1') space))
  | spaceNeeded         = Just space 
  | otherwise           = Just mempty
printSpaceBetween _ _ _ = Nothing 

-- | Print a token tree (@print_tt@)
printTt :: TokenTree -> Doc a
printTt (Token _ t) = printToken t
printTt (Delimited _ d ts) = block (intoDelim d) True mempty mempty [ printTokenStream ts ] 

-- | Print a list of token trees, with the right amount of space between successive elements
printTokenTrees :: [TokenTree] -> Doc a
printTokenTrees [] = mempty
printTokenTrees [tt] = printTt tt
printTokenTrees (tt1:tt2:tts) = printTt tt1 <> sp <> printTokenTrees (tt2:tts)
  where
  extremities :: TokenTree -> (Token, Token)
  extremities (Token _ t ) = (t,t)
  extremities (Delimited _ d _) = (OpenDelim (intoDelim d), CloseDelim (intoDelim d))

  -- extremities in this particular situation
  (lastTt1, firstTt2) = (snd $ extremities tt1, fst $ extremities tt2)
  spNeeded = lastTt1 `spaceNeeded` firstTt2

  -- Spacing of tokens, as informed by positional information on tokens
  spPos = printSpaceBetween spNeeded (spanOf tt1) (spanOf tt2)

  -- Spacing of tokens, as informed by 'spaceNeeded' and special cases
  spTok = case (snd $ extremities tt1, fst $ extremities tt2) of
            (Comma, _) -> space
            (Colon, _) -> space
            (Semicolon, _) -> space
            (_, OpenDelim Brace) -> space
            (CloseDelim Brace, _) -> space
            (t1, t2) | t1 `elem` toksRequiringSp || t2 `elem` toksRequiringSp -> space 
                     | otherwise -> if spNeeded then space else mempty

  -- List of tokens that want to have space on either side of them
  toksRequiringSp = [ Equal, GreaterEqual, GreaterGreaterEqual, EqualEqual, NotEqual, LessEqual,
                      LessLessEqual, MinusEqual, AmpersandEqual, PipeEqual, PlusEqual, StarEqual,
                      SlashEqual, CaretEqual, PercentEqual, RArrow, LArrow, FatArrow ]
  
  -- Use 'spPos' with 'spTok' as a fallback
  sp = fromMaybe spTok spPos

-- | Print the space between a token stream and the mod-path before it
printTokenStreamSp :: TokenStream -> Doc a
printTokenStreamSp (Tree Token{}) = space
printTokenStreamSp (Tree Delimited{}) = mempty
printTokenStreamSp (Stream []) = mempty
printTokenStreamSp (Stream (t:_)) = printTokenStreamSp t

-- | Print a token stream
printTokenStream :: TokenStream -> Doc a
printTokenStream = printTokenTrees . unfoldr unconsTokenStream

-- | Print a token (@token_to_string@)
-- Single character expression-operator symbols.
printToken :: Token -> Doc a
printToken Equal = "="
printToken Less = "<"
printToken Greater = ">"
printToken Ampersand = "&"
printToken Pipe = "|"
printToken Exclamation = "!"
printToken Tilde = "~"
printToken Plus = "+"
printToken Minus = "-"
printToken Star = "*"
printToken Slash = "/"
printToken Percent = "%"
-- Multi character eexpression-operator symbols
printToken GreaterEqual = ">="
printToken GreaterGreaterEqual = ">>="
printToken AmpersandAmpersand = "&&"
printToken PipePipe = "||"
printToken LessLess = "<<"
printToken GreaterGreater = ">>"
printToken EqualEqual = "=="
printToken NotEqual = "!="
printToken LessEqual = "<="
printToken LessLessEqual = "<<="
printToken MinusEqual = "-="
printToken AmpersandEqual = "&="
printToken PipeEqual = "|="
printToken PlusEqual = "+="
printToken StarEqual = "*="
printToken SlashEqual = "/="
printToken CaretEqual = "^="
printToken PercentEqual = "%="
printToken Caret = "^"
-- Structural symbols
printToken At = "@"
printToken Dot = "."
printToken DotDot = ".."
printToken DotDotDot = "..."
printToken DotDotEqual = "..="
printToken Comma = ","
printToken Semicolon = ";"
printToken Colon = ":"
printToken ModSep = "::"
printToken RArrow = "->"
printToken LArrow = "<-"
printToken FatArrow = "=>"
printToken Pound = "#"
printToken Dollar = "$"
printToken Question = "?"
-- Delimiters, eg. @{@, @]@, @(@
printToken (OpenDelim Paren) = "("
printToken (OpenDelim Bracket) = "["
printToken (OpenDelim Brace) = "{"
printToken (OpenDelim NoDelim) = ""
printToken (CloseDelim Paren) = ")"
printToken (CloseDelim Bracket) = "]"
printToken (CloseDelim Brace) = "}"
printToken (CloseDelim NoDelim) = ""
-- Literals
printToken (LiteralTok l s) = noIndent $ printLitTok l <> perhaps printName s
-- Name components
printToken (IdentTok i) = printIdent i
printToken (LifetimeTok i) = "'" <> printIdent i
printToken (Space Whitespace _) = " "
printToken (Space Comment n) = "/*" <> printName n <> " */"
printToken (Doc d Inner True) = "/*!" <> printName d <> "*/"
printToken (Doc d Outer True) = "/**" <> printName d <> "*/"
printToken (Doc d Inner False) = "//!" <> printName d
printToken (Doc d Outer False) = "///" <> printName d
printToken Shebang = "#!"
-- Macro related 
printToken (Interpolated n) = unAnnotate (printNonterminal n)
-- Other
printToken t = error $ "printToken: " ++ show t


-- | Print a literal token
printLitTok :: LitTok -> Doc a
printLitTok (ByteTok n)         = "b'" <> printName n <> "'"
printLitTok (CharTok n)         = "'" <> printName n <> "'"
printLitTok (IntegerTok n)      = printName n
printLitTok (FloatTok n)        = printName n
printLitTok (StrTok n)          = "\"" <> string hardline n <> "\""
printLitTok (StrRawTok n m)     = let pad = pretty (replicate m '#')
                                  in "r" <> pad <> "\"" <> string hardline n <> "\"" <> pad
printLitTok (ByteStrTok n)      = "b\"" <> string hardline n <> "\""
printLitTok (ByteStrRawTok n m) = let pad = pretty (replicate m '#') in "br" <> pad <> "\"" <> string hardline n <> "\"" <> pad

-- | Print a nonterminal
printNonterminal :: Nonterminal a -> Doc a
printNonterminal (NtItem item) = printItem item
printNonterminal (NtBlock blk) = printBlock blk
printNonterminal (NtStmt stmt) = printStmt stmt
printNonterminal (NtPat pat) = printPat pat
printNonterminal (NtExpr expr) = printExpr expr
printNonterminal (NtTy ty) = printType ty
printNonterminal (NtIdent ident) = printIdent ident
printNonterminal (NtPath path) = printPath path False
printNonterminal (NtTT tt) = printTt tt
printNonterminal (NtArm arm) = printArm arm
printNonterminal (NtImplItem item) = printImplItem item
printNonterminal (NtTraitItem item) = printTraitItem item
printNonterminal (NtGenerics generics) = printGenerics generics
printNonterminal (NtWhereClause clause) = printWhereClause True clause
printNonterminal (NtArg arg) = printArg arg True
printNonterminal (NtLit lit) = printLit lit

-- | Print a statement (@print_stmt@)
printStmt :: Stmt a -> Doc a
printStmt (ItemStmt item x)   = annotate x (printItem item)
printStmt (NoSemi expr x)     = annotate x (printExprOuterAttrStyle expr False <> when (requiresSemi expr) ";")
  where
  -- @parse::classify::expr_requires_semi_to_be_stmt@
  requiresSemi :: Expr a -> Bool
  requiresSemi If{} = False
  requiresSemi IfLet{} = False
  requiresSemi While{} = False
  requiresSemi WhileLet{} = False
  requiresSemi ForLoop{} = False
  requiresSemi Loop{} = False
  requiresSemi Match{} = False
  requiresSemi BlockExpr{} = False
  requiresSemi _ = True
printStmt (Semi expr x)       = annotate x (printExprOuterAttrStyle expr False <> ";")
printStmt (MacStmt m as x) = annotate x (printOuterAttrs as </> printMac True mempty m)
printStmt (Local p ty i as x) = annotate x (printOuterAttrs as <#> group ("let" <+> binding <+> initializer <> ";"))
  where binding = group (printPat p <> perhaps (\t -> ":" <#> indent n (printType t)) ty)
        initializer = perhaps (\e -> "=" <#> indent n (printExpr e)) i

-- | Print an expression
printExpr :: Expr a -> Doc a
printExpr expr = printExprOuterAttrStyle expr True

-- | Print an expression (@print_expr_outer_attr_style@)
-- Inlined @print_expr_in_place@, @print_expr_call@, @print_expr_method_call@, @print_expr_tup@,
-- @print_expr_binary@, @print_expr_unary@, @print_expr_addr_of@, @print_if@, @print_if_let@,
-- @print_expr_repeat@
--
-- TODO: attributes on chained method calls
printExprOuterAttrStyle :: Expr a -> Bool -> Doc a
printExprOuterAttrStyle expr isInline = glue (printEitherAttrs (expressionAttrs expr) Outer isInline) $
  case expr of
    Box _ e x                   -> annotate x ("box" <+> printExpr e)
    InPlace _ place e x         -> annotate x (hsep [ printExpr place, "<-", printExpr e ])
    Vec as exprs x              -> annotate x (block Bracket True "," (printInnerAttrs as) (printExpr <$> exprs))
    Call _ func [arg] x         -> annotate x (printExpr func <> parens (printExpr arg))
    Call _ func args x          -> annotate x (printExpr func <> block Paren True "," mempty (printExpr <$> args))
    MethodCall{}                -> chainedMethodCalls expr False id
    TupExpr as [e] x            -> annotate x (block Paren True ""  (printInnerAttrs as) [ printExpr e <> "," ])
    TupExpr as es x             -> annotate x (block Paren True "," (printInnerAttrs as) (printExpr <$> es))
    Binary _ op lhs rhs x       -> annotate x (hsep [ printExpr lhs, printBinOp op, printExpr rhs ])
    Unary _ op e x              -> annotate x (printUnOp op <> printExpr e)
    Lit _ lit x                 -> annotate x (printLit lit)
    Cast _ e ty x               -> let f = case e of { Cast{} -> printExpr; _ -> printExpr }
                                   in annotate x (hsep [ f e, "as", printType ty ])
    TypeAscription _ e ty x     -> annotate x (printExpr e <> ":" <+> printType ty)
    If _ test blk els x         -> annotate x (hsep [ "if", printExpr test, printBlock blk, printElse els ])
    IfLet _ pats e blk els x    -> annotate x (hsep [ "if let", printPats pats, "=", printExpr e, printBlock blk, printElse els ])
    While as test blk lbl x     -> annotate x (hsep [ printLbl lbl, "while", printExpr test, printBlockWithAttrs True blk as ])
    WhileLet as ps e blk lbl x  -> annotate x (hsep [ printLbl lbl, "while let", printPats ps, "=", printExpr e, printBlockWithAttrs True blk as ])
    ForLoop as pat e blk lbl x  -> annotate x (hsep [ printLbl lbl, "for", printPat pat, "in", printExpr e, printBlockWithAttrs True blk as ])
    Loop as blk lbl x           -> annotate x (hsep [ printLbl lbl, "loop", printBlockWithAttrs True blk as ])
    Match as e arms x           -> annotate x (hsep [ "match", printExpr e, block Brace False "," (printInnerAttrs as) (printArm `map` arms) ])
    Closure _ a s c decl body x -> annotate x (hsep [ when (a == IsAsync) "async"
                                                    , when (s == Immovable) "static"
                                                    , when (c == Value) "move"
                                                    , printFnBlockArgs decl <+> printExpr body])
    Async attrs c blk x         -> annotate x ("async" <+> when (c == Value) "move" <+> printBlockWithAttrs True blk attrs)
    BlockExpr attrs l blk x     -> annotate x (printLbl l <> printBlockWithAttrs True blk attrs)
    Catch attrs blk x           -> annotate x ("do catch" <+> printBlockWithAttrs True blk attrs)
    Assign _ lhs rhs x          -> annotate x (hsep [ printExpr lhs, "=", printExpr rhs ])
    AssignOp _ op lhs rhs x     -> annotate x (hsep [ printExpr lhs, printBinOp op <> "=", printExpr rhs ])
    FieldAccess{}               -> chainedMethodCalls expr False id
    TupField{}                  -> chainedMethodCalls expr False id
    Index{}                     -> chainedMethodCalls expr False id
    Range _ start end limits x  -> annotate x (perhaps printExpr start <+> printRangeLimits limits <+> perhaps printExpr end)
    PathExpr _ Nothing path x   -> annotate x (printPath path True)
    PathExpr _ (Just qs) path x -> annotate x (printQPath path qs True)
    AddrOf _ mut e x            -> annotate x ("&" <> printMutability mut <+> printExpr e)
    Break _ brk e x             -> annotate x ("break" <+> printLbl' brk <+> perhaps printExpr e)
    Continue _ cont x           -> annotate x ("continue" <+> printLbl' cont)
    Ret _ result x              -> annotate x ("return" <+> perhaps printExpr result)
    Yield _ result x            -> annotate x ("yield" <+> perhaps printExpr result)
    MacExpr _ m x               -> annotate x (printMac False mempty m)
    Struct as p fs Nothing x    -> annotate x (printPath p True <+> block Brace True "," (printInnerAttrs as) (printField `map` fs))
    Struct as p fs (Just d) x   -> let body = [ printField f <> "," | f <- fs ] ++ [ ".." <> printExpr d ] 
                                   in annotate x (printPath p True <+> block Brace True mempty (printInnerAttrs as) body)
    Repeat attrs e cnt x        -> annotate x (brackets (printInnerAttrs attrs <+> printExpr e <> ";" <+> printExpr cnt))
    ParenExpr attrs e x         -> annotate x (parens (printInnerAttrs attrs <+> printExpr e))
    Try{}                       -> chainedMethodCalls expr False id
  where
  printLbl = perhaps (\(Label n x) -> annotate x ("'" <> printName n) <> ":")
  printLbl' = perhaps (\(Label n x) -> annotate x ("'" <> printName n))
  glue = if isInline then (<+>) else (</>)

  -- | From an expression, seperate the first non-method-call from the list of method call suffixes
  --
  -- Collects in the list
  --   * methods
  --   * field accesses
  --
  -- Allows interweaving
  --   * tuple field accesses
  --   * array indexes
  --   * try
  --
  chainedMethodCalls :: Expr a            -- ^ expression
                     -> Bool              -- ^ last expression was a 'TupField' (if we have two
                                          -- successive 'TupField's, we need a space between them
                                          -- to prevent them from looking like a float literal)
                     -> (Doc a -> Doc a)  -- ^ suffix to the expression
                     -> Doc a
  chainedMethodCalls (MethodCall _ s i ts' as x) _ fdoc
    = let tys = perhaps (\ts -> "::<" <> commas ts printType <> ">") ts'
          as' = case as of
                  [a] -> parens (printExpr a)
                  _ -> block Paren True "," mempty (printExpr <$> as)
      in chainedMethodCalls s False (annotate x . (<##> fdoc (indent n (hcat [ ".", printIdent i, tys, as' ]))))
  chainedMethodCalls (FieldAccess _ s i x) _ fdoc
    = chainedMethodCalls s False (annotate x . (<##> fdoc (indent n (hcat [ ".", printIdent i ]))))
  chainedMethodCalls (Try _ s x) _ fdoc
    = chainedMethodCalls s False (annotate x . (<> fdoc "?"))
  chainedMethodCalls (Index _ s i x) _ fdoc
    = chainedMethodCalls s False (annotate x . (<> fdoc ("[" <> block NoDelim True mempty mempty [printExpr i] <> "]")))
  chainedMethodCalls (TupField _ s i x) t fdoc
    = chainedMethodCalls s True (annotate x . (<> fdoc ("." <> pretty i <> when t " ")))
  chainedMethodCalls e _ fdoc = group (fdoc (printExpr e))

-- | Print a string literal
printStr :: StrStyle -> String -> Doc a
printStr sty str = unAnnotate (printLit (Str str sty Unsuffixed ()))

-- | Extract from an expression its attributes
expressionAttrs :: Expr a -> [Attribute a]
expressionAttrs (Box as _ _) = as
expressionAttrs (InPlace as _  _ _) = as
expressionAttrs (Vec as _ _) = as
expressionAttrs (Call as _  _ _) = as
expressionAttrs (MethodCall as _ _  _ _ _) = as
expressionAttrs (TupExpr as _ _) = as
expressionAttrs (Binary as _ _ _ _) = as
expressionAttrs (Unary as _ _ _) = as
expressionAttrs (Lit as _ _) = as
expressionAttrs (Cast as _ _ _) = as
expressionAttrs (TypeAscription as _  _ _) = as
expressionAttrs (If as _ _ _ _) = as
expressionAttrs (IfLet as _ _ _ _ _) = as
expressionAttrs (While as _  _ _ _) = as
expressionAttrs (WhileLet as _ _ _ _ _) = as
expressionAttrs (ForLoop as _ _ _ _ _) = as
expressionAttrs (Loop as _ _ _) = as
expressionAttrs (Match as _ _ _) = as
expressionAttrs (Closure as _ _ _ _ _ _) = as
expressionAttrs (Async as _ _ _) = as
expressionAttrs (BlockExpr as _ _ _) = as
expressionAttrs (Catch as _ _) = as
expressionAttrs (Assign as _ _ _) = as
expressionAttrs (AssignOp as _ _ _ _) = as
expressionAttrs (FieldAccess as _ _ _) = as
expressionAttrs (TupField as _ _ _) = as
expressionAttrs (Index as _ _ _) = as
expressionAttrs (Range as _ _ _ _) = as
expressionAttrs (PathExpr as _ _ _) = as
expressionAttrs (AddrOf as _ _ _) = as
expressionAttrs (Break as _ _ _) = as
expressionAttrs (Continue as _ _) = as
expressionAttrs (Ret as _ _) = as
expressionAttrs (MacExpr as _ _) = as
expressionAttrs (Struct as _ _ _ _) = as
expressionAttrs (Repeat as _ _ _) = as
expressionAttrs (ParenExpr as _ _) = as
expressionAttrs (Try as _ _) = as
expressionAttrs (Yield as _ _) = as

-- | Print a field
printField :: Field a -> Doc a
printField (Field ident Nothing x) = annotate x (printIdent ident)
printField (Field ident (Just expr) x) = annotate x (printIdent ident <>":" <+> printExpr expr)

-- | Print range limits
printRangeLimits :: RangeLimits -> Doc a
printRangeLimits HalfOpen = ".."
printRangeLimits ClosedDot = "..."
printRangeLimits ClosedEq = "..="

-- | Print a closure function declaration (@print_fn_block_args@)
printFnBlockArgs :: FnDecl a -> Doc a
printFnBlockArgs (FnDecl args ret _ x) = annotate x ("|" <> args' <> "|" <+> ret')
  where ret' = perhaps (\ty -> "->" <+> printType ty) ret
        args' = commas args (`printArg` True)

-- | Print the arm of a match expression (@print_arm@)
printArm :: Arm a -> Doc a
printArm (Arm as pats guard body x) = annotate x $ printOuterAttrs as
  </> printPats pats 
  <+> perhaps (\e -> "if" <+> printExpr e) guard
  <+> "=>"
  <+> printExpr body

printPats :: Foldable f => f (Pat a) -> Doc a
printPats pats = group (foldr1 (\a b -> a <+> "|" <#> b) (printPat `map` toList pats))

-- | Print a block
printBlock :: Block a -> Doc a
printBlock blk = printBlockWithAttrs True blk []

-- | Print a block with attributes (@print_block_with_attrs@ or @print_block_maybe_unclosed@)
printBlockWithAttrs :: Bool -> Block a -> [Attribute a] -> Doc a
printBlockWithAttrs b (Block stmts rules x) as = annotate x (printUnsafety rules <+> block Brace b mempty (printInnerAttrs as) stmts')
  where
  stmts' | null stmts = []
         | otherwise = body ++ [ lastStmt ]

  body = printStmt `map` Prelude.init stmts
  
  lastStmt = case last stmts of
               NoSemi expr _ -> printExprOuterAttrStyle expr False
               stmt -> printStmt stmt

-- | Print an @else@ expression (@print_else@)
printElse :: Maybe (Expr a) -> Doc a
printElse Nothing = mempty
printElse (Just (If _ e t s x))      = annotate x (hsep [ "else if", printExpr e, printBlock t, printElse s ])
printElse (Just (IfLet _ p e t s x)) = annotate x (hsep [ "else if let", printPats p, "=", printExpr e, printBlock t, printElse s ])
printElse (Just (BlockExpr _ Nothing blk x)) = annotate x (hsep [ "else", printBlock blk ])
printElse _ = error "printElse saw `if` with a weird alternative"

-- | Print a binary operator 
printBinOp :: BinOp -> Doc a
printBinOp AddOp = "+"
printBinOp SubOp = "-"
printBinOp MulOp = "*"
printBinOp DivOp = "/"
printBinOp RemOp = "%"
printBinOp AndOp = "&&"
printBinOp OrOp = "||"
printBinOp BitXorOp = "^"
printBinOp BitAndOp = "&"
printBinOp BitOrOp = "|"
printBinOp ShlOp = "<<"
printBinOp ShrOp = ">>"
printBinOp EqOp = "=="
printBinOp LtOp = "<"
printBinOp LeOp = "<="
printBinOp NeOp = "!="
printBinOp GeOp = ">="
printBinOp GtOp = ">"

-- | Print a unary operator
printUnOp :: UnOp -> Doc a
printUnOp Deref = "*"
printUnOp Not = "!"
printUnOp Neg = "-" 

-- | Print inner attributes (@print_inner_attributes@ or @print_inner_attributes_inline@
-- or @print_inner_attributes_nodbreak@ - distinction has to be made at callsite
-- whether to force newline)
printInnerAttrs :: [Attribute a] -> Doc a
printInnerAttrs attrs = printEitherAttrs attrs Inner False

-- | Print outer attributes (@print_outer_attributes@ or @print_outer_attributes_no_trailing_hardbreak@)
printOuterAttrs :: [Attribute a] -> Doc a
printOuterAttrs attrs = printEitherAttrs attrs Outer False

-- | Print either type of attributes (@print_either_attributes@)
printEitherAttrs :: [Attribute a] -> AttrStyle -> Bool -> Doc a
printEitherAttrs attrs kind inline = unless (null attrs') (glue attrs')
  where glue = if inline then hsep else vcat
        attrs' = [ printAttr attr inline | attr <- attrs, style attr == kind ]

        style (Attribute sty _ _ _) = sty
        style (SugaredDoc sty _ _ _) = sty

-- | Print an attribute (@print_attribute_inline@ or @print_attribute@)
printAttr :: Attribute a -> Bool -> Doc a
printAttr (Attribute Inner p ts x) _     = annotate x ("#![" <> printPath p True <> printTokenStreamSp ts <> printTokenStream ts <> "]")
printAttr (Attribute Outer p ts x) _     = annotate x ("#[" <> printPath p True <> printTokenStreamSp ts <> printTokenStream ts <> "]")
printAttr (SugaredDoc Inner True c x)  _ = annotate x (noIndent ("/*!" <> string hardline c <> "*/"))
printAttr (SugaredDoc Outer True c x)  _ = annotate x (noIndent ("/**" <> string hardline c <> "*/"))
printAttr (SugaredDoc Inner False c x) _ = annotate x (flatAlt ("//!" <> pretty c) ("/*!" <> pretty c <> "*/"))
printAttr (SugaredDoc Outer False c x) _ = annotate x (flatAlt ("///" <> pretty c) ("/**" <> pretty c <> "*/"))

-- | Print an identifier as is, or as cooked string if containing a hyphen
printCookedIdent :: Ident -> Doc a
printCookedIdent ident@(Ident str raw _)
  | '-' `elem` str && not raw = printStr Cooked str
  | otherwise = printIdent ident 


-- | Print an item (@print_item@)
printItem :: Item a -> Doc a
printItem (ExternCrate as vis ident p x) = annotate x $ align $ printOuterAttrs as <#>
  hsep [ printVis vis, "extern", "crate", perhaps (\p' -> printCookedIdent p' <+> "as") p, printIdent ident <> ";" ]

printItem (Use as vis ut x) = annotate x $ align $ printOuterAttrs as <#>
  hsep [ printVis vis, "use", printUseTree ut <> ";" ]

printItem (Static as vis ident ty m e x) = annotate x $ align $ printOuterAttrs as <#>
  hsep [ printVis vis, "static", printMutability m, printIdent ident <> ":", printType ty, "=", printExpr e <> ";" ]

printItem (ConstItem as vis ident t e x) = annotate x $ align $ printOuterAttrs as <#>
  hsep [ printVis vis, "const", printIdent ident <> ":", printType t, "=", printExpr e <> ";" ]

printItem (Fn as vis header ident d g b x) = annotate x $ align $ printOuterAttrs as <#>
  printFn vis header (Just ident) d g (Just (b, as))

printItem (Mod as vis ident items x) = annotate x $ align $ printOuterAttrs as <#>
  hsep [ printVis vis, "mod", printMod ident items as ]

printItem (ForeignMod as vis a i x) = annotate x $ align $ printOuterAttrs as <#>
  hsep [ printVis vis, printAbi a, printForeignMod i as ]

printItem (TyAlias as vis ident ty ps x) = annotate x $ align $ printOuterAttrs as <#>
  let wc = printWhereClause True (whereClause ps)
      leading = printVis vis <+> "type" <+> printIdent ident <> printGenerics ps
  in emptyElim (group (leading <+> "=" <#> indent n (printType ty <> ";")))
               (\w -> vsep [leading, w, "=" <+> printType ty <> ";"])
               wc

printItem (Existential as vis ident ps bds x) = annotate x $ align $ printOuterAttrs as <#>
  printVis vis <+> "existential type" <+> printIdent ident <> printGenerics ps <> printBounds ":" bds

printItem (Enum as vis ident vars ps x) = annotate x $ align $ printOuterAttrs as <#>
  printEnumDef vars ps ident vis

printItem (StructItem as vis ident s g x) = annotate x $ align $ printOuterAttrs as <#>
  hsep [ printVis vis, "struct", printStruct s g ident True True ]

printItem (Union as vis ident s g x) = annotate x $ align $ printOuterAttrs as <#>
  hsep [ printVis vis, "union", printStruct s g ident True True ]

printItem (Trait as vis ident a u g tys i x) = annotate x $ align $ printOuterAttrs as <#> 
  let leading = hsep [ printVis vis, printUnsafety u, when a "auto", "trait"
                     , printIdent ident <> printGenerics g <> printBounds ":" tys
                     ]
      lagging = block Brace False mempty (printInnerAttrs as) (printTraitItem `map` i)
      wc = printWhereClause True (whereClause g)
  in emptyElim (leading <+> lagging)
               (\w -> vsep [leading, w, lagging])
               wc

printItem (TraitAlias as vis ident g bds x) = annotate x $ align $ printOuterAttrs as <#>
  let leading = printVis vis <+> "trait" <+> printIdent ident <> printGenerics g
  in group (leading <#> indent n (printBounds "=" (toList bds)) <> ";")

printItem (Impl as vis d u p g t ty i x) = annotate x $ align $ printOuterAttrs as <#> 
  let generics = case g of { Generics [] _ _ -> mempty; _ -> printGenerics g }
      traitref = perhaps (\t' -> printPolarity p <> printTraitRef t' <+> "for") t
      leading = hsep [ printVis vis, printDef d, printUnsafety u
                     , "impl" <> generics, traitref, printType ty
                     ]
      lagging = block Brace False mempty (printInnerAttrs as) (printImplItem `map` i)
      wc = printWhereClause True (whereClause g)
  in emptyElim (leading <+> lagging)
               (\w -> vsep [leading, w, lagging])
               wc

printItem (MacItem as i m x) = annotate x $ align $ printOuterAttrs as <#>
  printMac False (perhaps (\i' -> " " <> printIdent i' <> " ") i) m

printItem (MacroDef as i ts x) = annotate x $ align $ printOuterAttrs as <#>
  ("macro_rules" <> "!" <+> printIdent i <+> block Brace True mempty mempty [ printTokenStream ts ])


-- | Print a trait item (@print_trait_item@)
printTraitItem :: TraitItem a -> Doc a
printTraitItem (ConstT as ident ty expr x) = annotate x $ printOuterAttrs as <#> printAssociatedConst ident ty expr InheritedV
printTraitItem (MethodT as ident g sig body x) = annotate x $ printOuterAttrs as <#> printMethodSig ident g sig InheritedV (fmap (\b -> (b, as)) body)
printTraitItem (TypeT as ident bounds ty x) = annotate x $ printOuterAttrs as <#> printAssociatedType ident (Just bounds) ty
printTraitItem (MacroT as m x) = annotate x $ printOuterAttrs as <#> printMac True mempty m

-- | Print type parameter bounds with the given prefix, but only if there are any bounds (@print_bounds@)
printBounds :: Doc a -> [GenericBound a] -> Doc a
printBounds _ [] = mempty
printBounds prefix bs = group (prefix <#> ungroup (block NoDelim False " +" mempty (printBound `map` bs)))

-- | Print a type parameter bound
printBound :: GenericBound a -> Doc a
printBound (Outlives lt x) = annotate x $ printLifetime lt
printBound (TraitBound tref modi x) = annotate x $ when (modi == Maybe) "?" <> printPolyTraitRef tref

-- | Print the formal lifetime list (@print_formal_lifetime_list@)
printForParams :: [GenericParam a] -> Doc a
printForParams [] = mempty
printForParams defs = "for" <> angles (align (fillSep (punctuate "," (printGenericParam `map` defs))))

-- | Print an impl item (@print_impl_item@)
printImplItem :: ImplItem a -> Doc a
printImplItem (ConstI as vis def ident ty expr x) = annotate x $ printOuterAttrs as <#>
  (printVis vis <+> printDef def <+> printAssociatedConst ident ty (Just expr) InheritedV)
printImplItem (MethodI as vis def ident g sig body x) = annotate x $ printOuterAttrs as <#>
  (printVis vis <+> printDef def <+> printMethodSig ident g sig InheritedV (Just (body, as)))
printImplItem (TypeI as vis def ident ty x) = annotate x $ printOuterAttrs as <#>
  (printVis vis <+> printDef def <+> printAssociatedType ident Nothing (Just ty))
printImplItem (ExistentialI as vis def ident bds x) = annotate x $ printOuterAttrs as <#>
  (printVis vis <+> printDef def <+> printIdent ident <> printBounds ":" bds)
printImplItem (MacroI as def mac x) = annotate x $ printOuterAttrs as <#>
  (printDef def <+> printMac True mempty mac)

-- | Print defaultness (@Defaultness@)
printDef :: Defaultness -> Doc a
printDef Default = "default"
printDef Final = mempty 

-- | Print an associated type (@printAssociatedType@)
printAssociatedType :: Ident ->  Maybe [GenericBound a] -> Maybe (Ty a) -> Doc a
printAssociatedType ident bounds_m ty_m = "type" <+> (printIdent ident
  <> perhaps (printBounds ":") bounds_m)
  <+> perhaps (\ty -> "=" <+> printType ty) ty_m
  <> ";"

-- | Print a method signature (@print_method_sig@)
printMethodSig :: Ident -> Generics a -> MethodSig a -> Visibility a -> Maybe (Block a, [Attribute a]) -> Doc a
printMethodSig ident generics (MethodSig header decl) vis
  = printFn vis header (Just ident) decl generics

-- | Print an associated constant (@print_associated_const@)
printAssociatedConst :: Ident -> Ty a -> Maybe (Expr a) -> Visibility a -> Doc a
printAssociatedConst ident ty default_m vis = printVis vis
  <+> "const" <+> printIdent ident <> ":" <+> printType ty
  <+> perhaps (\expr -> "=" <+> printExpr expr) default_m
  <> ";"

-- | Print the polarity of a trait
printPolarity :: ImplPolarity -> Doc a
printPolarity Negative = "!"
printPolarity Positive = mempty

-- | Print visibility (@print_visibility@)
printVis :: Visibility a -> Doc a
printVis PublicV = "pub"
printVis CrateV = "pub(crate)"
printVis (RestrictedV path@(Path False [PathSegment "super" Nothing _] _)) = "pub(" <> printPath path False <> ")"
printVis (RestrictedV path@(Path False [PathSegment "self" Nothing _] _)) = "pub(" <> printPath path False <> ")"
printVis (RestrictedV path) = "pub(" <> "in" <+> printPath path False <> ")"
printVis InheritedV = mempty

-- | Print a foreign item (@print_foreign_item@)
printForeignItem :: ForeignItem a -> Doc a
printForeignItem (ForeignFn attrs vis ident decl gen x) = annotate x $
  printOuterAttrs attrs <#> printFn vis (FnHeader Normal NotAsync NotConst Rust) (Just ident) decl gen Nothing
printForeignItem (ForeignStatic attrs vis ident ty mut x) = annotate x $
  printOuterAttrs attrs <#> printVis vis <+> "static" <+> printMutability mut <+> printIdent ident <> ":" <+> printType ty <> ";"
printForeignItem (ForeignTy attrs vis ident x) = annotate x $
  printOuterAttrs attrs <#> printVis vis <+> "type" <+> printIdent ident <> ";"

-- | Print a struct definition (@print_struct@)
printStruct :: VariantData a -> Generics a -> Ident -> Bool -> Bool -> Doc a
printStruct structDef generics ident printFinalizer annotateGenerics =
  printIdent ident <> gen
    <> case (structDef, whereClause generics) of 
          (StructD fields x, WhereClause [] _) -> annotate x $ space <> block Brace False "," mempty (printStructField `map` fields)
          (StructD fields x, wc) -> annotate x $ line <> printWhereClause True wc <#> block Brace False "," mempty (printStructField `map` fields)
          (TupleD fields x, WhereClause [] _) -> annotate x $ block Paren True "," mempty (printStructField `map` fields) <> when printFinalizer ";" 
          (TupleD fields x, wc) -> annotate x $ block Paren True "," mempty (printStructField `map` fields) <#> printWhereClause (not printFinalizer) wc <> when printFinalizer ";" 
          (UnitD x, WhereClause [] _) -> annotate x $ when printFinalizer ";"
          (UnitD x, wc) -> annotate x $ line <> printWhereClause (not printFinalizer) wc <> when printFinalizer ";"
  where gen = if annotateGenerics then printGenerics generics else unAnnotate (printGenerics generics)

-- | Print a struct field
printStructField :: StructField a -> Doc a
printStructField (StructField i v t as x) = annotate x (printOuterAttrs as <#> hsep [ printVis v, perhaps (\i' -> printIdent i' <> ":") i, printType t ])

-- | Pretty print unsafety (@print_unsafety@)
printUnsafety :: Unsafety -> Doc a
printUnsafety Normal = mempty
printUnsafety Unsafe = "unsafe"

-- | Print an enum definition (@print_enum_def@)
printEnumDef :: [Variant a] -> Generics a -> Ident -> Visibility a -> Doc a
printEnumDef variants generics ident vis =
   case whereClause generics of
     WhereClause [] _ -> leading <+> lagging
     wc -> leading <#> printWhereClause True wc <#> lagging
  where leading = printVis vis <+> "enum" <+> (printIdent ident <> printGenerics generics)
        lagging = block Brace False "," mempty [ printOuterAttrs as <#> printVariant v | v@(Variant _ as _ _ _) <- variants ]

-- | Print a variant (@print_variant@)
printVariant :: Variant a -> Doc a
printVariant (Variant i _ _data e x) = annotate x (body <+> disc)
  where body = printStruct _data (Generics [] (WhereClause [] undefined) undefined) i False False
        disc = perhaps (\e' -> "=" <+> printExpr e') e

-- | Print a where clause (@print_where_clause@). The 'Bool' argument indicates whether to have a
-- trailing comma or not.
printWhereClause :: Bool -> WhereClause a -> Doc a
printWhereClause trailing (WhereClause predicates x)
  | null predicates = mempty
  | trailing = annotate x ("where" <#> block NoDelim False "," mempty (printWherePredicate `map` predicates))
  | otherwise = annotate x ("where" <#> block NoDelim False mempty mempty (punctuate "," (printWherePredicate `map` predicates)))

-- | Print a where clause predicate
printWherePredicate :: WherePredicate a -> Doc a
printWherePredicate (BoundPredicate blt ty bds y) = annotate y (printForParams blt <+> printType ty <> printBounds ":" bds)
printWherePredicate (RegionPredicate lt bds y) = annotate y (printLifetime lt <> printBounds ":" bds)
printWherePredicate (EqPredicate lhs rhs y) = annotate y (printType lhs <+> "=" <+> printType rhs)

-- | Print a function (@print_fn@)
printFn :: Visibility a
        -> FnHeader
        -> Maybe Ident
        -> FnDecl a
        -> Generics a
        -> Maybe (Block a, [Attribute a])
        -> Doc a
printFn vis header name decl generics blkAttrs =
  printVis vis <+> printFnHeader header <+> perhaps printIdent name
    <> printGenerics generics
    <> printFnArgsAndRet decl
    <+#> whereBlk
  where
  ((<+#>), whereBlk) = case (whereClause generics, blkAttrs) of
                         (WhereClause [] _, Nothing) -> ((<>), ";")
                         (WhereClause [] _, Just (blk, attrs)) -> ((<+>), printBlockWithAttrs False blk attrs)
                         (wc,               Nothing) -> ((<#>), printWhereClause False wc <> ";")
                         (wc,               Just (blk, attrs)) -> ((<#>), printWhereClause True wc <#> printBlockWithAttrs False blk attrs)


-- | Print the function arguments and the return type (@print_fn_args_and_ret@)
printFnArgsAndRet :: FnDecl a -> Doc a
printFnArgsAndRet (FnDecl args ret var x)
  | var = annotate x (block Paren True mempty mempty ([ printArg a False <> "," | a <- args ] ++ [ "..." ]) <+> ret')
  | otherwise = annotate x (block Paren True "," mempty [ printArg a False | a <- args ] <+> ret')
  where ret' = perhaps (\t -> "->" <+> printType t) ret

-- | Print an argument (@print_arg@)
printArg :: Arg a -> Bool -> Doc a
printArg (Arg (Just pat) (Infer x') x) True = annotate x $ annotate x' (printPat pat)
printArg (Arg Nothing ty x) _ = annotate x (printType ty)
printArg (Arg (Just pat) ty x) _ = annotate x (printPat pat <> ":" <+> printType ty)
printArg (SelfValue mut x) _ = annotate x (printMutability mut <+> "self")
printArg (SelfRegion lt mut x) _ = annotate x ("&" <> hsep [perhaps printLifetime lt, printMutability mut, "self"])
printArg (SelfExplicit ty mut x) _ = annotate x (printMutability mut <+> "self" <> ":" <+> printType ty)

-- | Print a lifetime (@print_lifetime@)
printLifetime :: Lifetime a -> Doc a
printLifetime (Lifetime n x) = annotate x ("'" <> printName n)

-- | Print mutability (@print_mutability@)
printMutability :: Mutability -> Doc a
printMutability Mutable = "mut"
printMutability Immutable = mempty

-- | Like 'printMutability', but prints @const@ in the immutable case 
printFullMutability :: Mutability -> Doc a
printFullMutability Mutable = "mut"
printFullMutability Immutable = "const"

-- | Print a pattern (@print_pat@)
printPat :: Pat a -> Doc a
printPat (WildP x)                      = annotate x "_"
printPat (IdentP bm p s x)              = annotate x (printBindingMode bm <+> printIdent p <+> perhaps (\p' -> "@" <+> printPat p') s)
printPat (StructP p fs False x)         = annotate x (printPath p True <+> block Brace True "," mempty (printFieldPat `map` fs))
printPat (StructP p fs True x)          = annotate x (printPath p True <+> block Brace True mempty mempty ([ printFieldPat f <> "," | f <- fs ] ++ [ ".." ]))
printPat (TupleStructP p es Nothing x)  = annotate x (printPath p True <> "(" <> commas es printPat <> ")")
printPat (TupleStructP p es (Just d) x) = let (before,after) = splitAt d es
  in annotate x (printPath p True <> "(" <> commas before printPat <> when (d /= 0) ","
                    <+> ".." <> when (d /= length es) ("," <+> commas after printPat) <> ")")
printPat (PathP Nothing path x)         = annotate x (printPath path True)
printPat (PathP (Just qself) path x)    = annotate x (printQPath path qself True)
printPat (TupleP [elt] Nothing x)        = annotate x ("(" <> printPat elt <> ",)")
printPat (TupleP elts Nothing x)        = annotate x ("(" <> commas elts printPat <> ")")
printPat (TupleP elts (Just ddpos) _) = let (before,after) = splitAt ddpos elts
  in "(" <> commas before printPat <> unless (null before) ","
      <+> ".." <> unless (null after) ("," <+> commas after printPat) <> ")"
printPat (BoxP inner x)                 = annotate x ("box" <+> printPat inner)
printPat (RefP inner mutbl x)           = annotate x ("&" <> printMutability mutbl <+> printPat inner)
printPat (LitP expr x)                  = annotate x (printExpr expr)
printPat (RangeP lo hi x)               = annotate x (printExpr lo <+> "..=" <+> printExpr hi)
printPat (SliceP pb Nothing pa x)       = annotate x ("[" <> commas (pb ++ pa) printPat <> "]")
printPat (SliceP pb (Just ps) pa x)     = annotate x ("[" <> commas pb printPat <> ps' <+> commas pa printPat <> "]")
  where ps' = hcat [ unless (null pb) ","
                   , space
                   , case ps of WildP{} -> mempty
                                _ -> printPat ps
                   , ".."
                   , unless (null pa) ","
                   ]
printPat (MacP m x)                     = annotate x (printMac False mempty m)

-- | Print a field pattern
printFieldPat :: FieldPat a -> Doc a
printFieldPat (FieldPat i p x) = annotate x (perhaps (\i -> printIdent i <> ":") i <+> printPat p)

-- | Print a binding mode
printBindingMode :: BindingMode -> Doc a
printBindingMode (ByRef mutbl) = "ref" <+> printMutability mutbl
printBindingMode (ByValue Immutable) = mempty
printBindingMode (ByValue Mutable) = "mut"

-- | Print the prefix of a function - all the stuff after the visibility and up to and including
-- the @fn@ (@print_fn_header_info@)
printFnHeader :: FnHeader -> Doc a
printFnHeader (FnHeader u as c a) = hsep [ when (c == Const) "const"
                                         , when (as == IsAsync) "async"
                                         , printUnsafety u
                                         , printAbi a
                                         , "fn" ]

-- | Print the ABI
printAbi :: Abi -> Doc a
printAbi Rust = mempty
printAbi abi = "extern" <+> "\"" <> root abi <> "\""
  where
    root :: Abi -> Doc a
    root Cdecl = "cdecl"
    root Stdcall = "stdcall"
    root Fastcall = "fastcall"
    root Vectorcall = "vectorcall"
    root Aapcs = "aapcs"
    root Win64 = "win64"
    root SysV64 = "sysv64"
    root PtxKernel = "ptx-kernel"
    root Msp430Interrupt = "msp430-interrupt"
    root X86Interrupt = "x86-interrupt"
    root Rust = "Rust"
    root C = "C"
    root System = "system"
    root RustIntrinsic = "rust-intrinsic"
    root RustCall = "rust-call"
    root PlatformIntrinsic = "platform-intrinsic"
    root Unadjusted = "unadjusted"

 
-- | Print the interior of a module given the list of items and attributes in it (@print_mod@)
printMod :: Ident -> Maybe [Item a] -> [Attribute a] -> Doc a
printMod i (Just items) attrs = printIdent i <+> block Brace False mempty (printInnerAttrs attrs) (punctuate line' (printItem `map` items))
printMod i Nothing _ = printIdent i <> ";"

-- | Print the interior of a foreign module given the list of items and attributes in it (@print_mod@)
printForeignMod :: [ForeignItem a] -> [Attribute a] -> Doc a
printForeignMod items attrs = block Brace False mempty (printInnerAttrs attrs) (printForeignItem `map` items)

-- | Print generics (@print_generics@)
-- Remark: we are discarding the where clause because it gets printed seperately from the rest of
-- the generic.
printGenerics :: Generics a -> Doc a
printGenerics (Generics [] _ x) = annotate x mempty
printGenerics (Generics params _ x) = annotate x $ group ("<" <##> ungroup blk <##> ">")
  where ps = map printGenericParam params
        blk = block NoDelim True "," mempty ps

-- | Print a poly-trait ref (@print_poly_trait_ref@)
printPolyTraitRef :: PolyTraitRef a -> Doc a
printPolyTraitRef (PolyTraitRef lts tref x) = annotate x (printForParams lts <+> printTraitRef tref)

-- | Print a trait ref (@print_trait_ref@)
printTraitRef :: TraitRef a -> Doc a
printTraitRef (TraitRef path) = printPath path False

-- | Print a path parameter and signal whether its generic (if any) should start with a colon (@print_path_parameters@)
printGenericArgs :: GenericArgs a -> Bool -> Doc a
printGenericArgs (Parenthesized ins out x) colons = annotate x $
  when colons "::" <> parens (commas ins printType) <+> perhaps (\t -> "->" <+> printType t) out
printGenericArgs (AngleBracketed lts tys bds x) colons = annotate x (when colons "::" <> "<" <> hsep (punctuate "," (lts' ++ tys' ++ bds')) <> ">")
  where
    lts' = printLifetime <$> lts
    tys' = printType <$> tys
    bds' = (\(ident,ty) -> printIdent ident <+> "=" <+> printType ty) <$> bds


-- | Print a path, specifiying explicitly whether to include colons (@::@) before generics
-- or not (so expression style or type style generics) (@print_path@)
printPath :: Path a -> Bool -> Doc a
printPath (Path global segs x) colons = annotate x (when global "::" <> hcat (punctuate "::" (map (`printSegment` colons) segs)))

-- | Print a path segment
printSegment :: PathSegment a -> Bool -> Doc a
printSegment (PathSegment i ps x) colons = annotate x (printIdent i <> params)
  where params = perhaps (\p -> printGenericArgs p colons) ps


-- | Print a qualified path, specifiying explicitly whether to include colons (@::@) before
-- generics or not (so expression style or type style generics) (@print_qpath@)
printQPath :: Path a -> QSelf a -> Bool -> Doc a
printQPath (Path global segs x) (QSelf ty n) colons = hcat [ "<", printType ty <+> aliasedDoc, ">", "::", restDoc ]
  where
  (aliased, rest) = splitAt n segs
  
  aliasedDoc = case aliased of
                 [] -> mempty
                 segs -> "as" <+> printPath (Path global segs x) False

  restDoc = printPath (Path False rest x) colons

-- | Print a use tree
printUseTree :: UseTree a -> Doc a
printUseTree (UseTreeSimple p Nothing x) = annotate x (printPath p True)
printUseTree (UseTreeSimple p (Just i) x) = annotate x (printPath p True <+> "as" <+> printIdent i)
printUseTree (UseTreeGlob p@(Path _ [] _) x) = annotate x (printPath p True <> "*")
printUseTree (UseTreeGlob p x) = annotate x (printPath p True <> "::*")
printUseTree (UseTreeNested p@(Path _ [] _) n x) = annotate x (printPath p True <> "{" <> hcat (punctuate ", " (map printUseTree n)) <> "}")
printUseTree (UseTreeNested p n x) = annotate x (printPath p True <> "::{" <> hcat (punctuate ", " (map printUseTree n)) <> "}")

-- | Print a type parameters (@print_ty_param@)
printGenericParam :: GenericParam a -> Doc a
printGenericParam (TypeParam as i bds def x) = annotate x $ hsep
  [ printOuterAttrs as
  , printIdent i <> printBounds ":" bds
  , perhaps (\def' -> "=" <+> printType def') def
  ]
printGenericParam (LifetimeParam as i bds x) = annotate x $ hsep
  [ printOuterAttrs as
  , printIdent i <> printBounds ":" bds
  ]

