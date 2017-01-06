{-# LANGUAGE RecordWildCards, OverloadedStrings, DuplicateRecordFields, DefaultSignatures #-}

module Language.Rust.Pretty
  ( printType
  , printExpr
  , printItem
  , printLit
  , printAttr
  ) where

import Language.Rust.Syntax.AST
import Language.Rust.Syntax.Token
import Language.Rust.Syntax.Ident

import Text.PrettyPrint.Annotated.WL (Pretty(..), hcat, cat, indent, group, angles, space, line, flatten, align, fillSep, fillCat, punctuate, text, vcat, char, annotate, noAnnotate, parens, brackets, (<>), (<//>), Doc)
import qualified Text.PrettyPrint.Annotated.WL as WL

import Data.ByteString (unpack)
import Data.Word (Word8)
import Data.Maybe (listToMaybe)
import Data.List (intersperse)
import Data.Char (intToDigit, ord, chr)

-- | comma delimited
commas :: [a] -> (a -> Doc b) -> Doc b
commas xs f = hsep (punctuate "," (map f xs))

when :: Bool -> Doc a -> Doc a
when False _ = mempty
when True d = d

fold :: Foldable f => (Doc a -> Doc a -> Doc a) -> f (Doc a) -> Doc a
fold f xs | null xs = mempty
          | otherwise = foldr1 f xs

liftOp :: (Doc a -> Doc a -> Doc a) -> Doc a -> Doc a -> Doc a
liftOp (*) WL.Empty d = d
liftOp (*) d WL.Empty = d
liftOp (*) d d' = d * d'

(<+>) :: Doc a -> Doc a -> Doc a
(<+>) = liftOp (WL.<+>)

hsep :: Foldable f => f (Doc a) -> Doc a
hsep = fold (<+>)

(<#>) :: Doc a -> Doc a -> Doc a
(<#>) = liftOp (WL.<#>)

vsep :: Foldable f => f (Doc a) -> Doc a
vsep = fold (<#>)

(</>) :: Doc a -> Doc a -> Doc a
(</>) = liftOp (WL.</>)

sep :: Foldable f => f (Doc a) -> Doc a
sep = group . vsep

unless :: Bool -> Doc a -> Doc a
unless b = when (not b)

perhaps :: (a -> Doc b) -> Maybe a -> Doc b
perhaps = maybe mempty

isEmpty :: Doc a -> Bool
isEmpty WL.Empty = True
isEmpty _ = False

-- | Asserts a 'Doc a' cannot render on multiple lines. 
oneLine :: Doc a -> Bool
oneLine (WL.FlatAlt d _) = oneLine d
oneLine (WL.Cat a b) = oneLine a && oneLine b
oneLine (WL.Union a b) = oneLine a && oneLine b
oneLine (WL.Annotate _ d) = oneLine d
oneLine WL.Line = False
oneLine _ = True

-- | Make a curly-brace delimited block. When possible, permit fitting everything on one line
block :: Doc a -> Doc a
block b | oneLine b = hsep ["{", b, "}"] `WL.Union` vsep [ "{", indent 2 b, "}" ]
        | otherwise = vsep [ "{", indent 2 b, "}" ]

-- Useful stuff from based on https://aturon.github.io/README.html

-- Brace only if not single-line (as per "match arms get braces, except for single-line
-- expressions.")
block' :: Doc a -> Doc a
block' b | oneLine b = b
         | otherwise = vsep [ "{", indent 2 b, "}" ]

-- Comma-delimited and brace-enclosed (as per "a trailing comma should be included whenever the
-- closing delimiter appears on a separate line")
braceComma :: [Doc a] -> Doc a
braceComma [] = "{}"
braceComma ds = let ds' = Prelude.init ds
                    d' = Prelude.last ds
                in ("{" <+> hsep [ d <> "," | d <- ds' ] <+> d' <+> "}")
                     `WL.Union`
                   ("{" <#> indent 2 (vsep [ d <> "," | d <- ds ]) <#> "}")

braceCommaAttrs :: Doc a -> [Doc a] -> Doc a
braceCommaAttrs as [] = "{" <+> as <+> "}"
braceCommaAttrs as ds = let ds' = Prelude.init ds
                            d' = Prelude.last ds
                        in ("{" <+> hsep (as : [ d <> "," | d <- ds' ]) <+> d' <+> "}")
                             `WL.Union`
                           ("{" <#> indent 2 (vsep (as : [ d <> "," | d <- ds ])) <#> "}")

braceAttrs :: Doc a -> [Doc a] -> Doc a
braceAttrs as [] = "{" <+> as <+> "}"
braceAttrs as ds = ("{" <+> hsep (as : ds) <+> "}")
                             `WL.Union`
                   ("{" <#> indent 2 (vsep (as : ds)) <#> "}")

instance Pretty Name where pretty (Name s) = text s
instance Pretty Ident where pretty = printIdent
instance Pretty TokenTree where pretty = printTt
instance Pretty Token where pretty = printToken
instance Pretty LitTok where pretty = printLitTok
instance Pretty RangeLimits where pretty = printRangeLimits
instance Pretty BindingMode where pretty = printBindingMode

class PrettyAnnotated p where
  prettyAnn :: p a -> Doc a

  default prettyAnn :: Show (p a) => p a -> Doc a
  prettyAnn = text . show

instance PrettyAnnotated Ty where prettyAnn = printType
instance PrettyAnnotated Nonterminal where prettyAnn = printNonterminal
instance PrettyAnnotated Stmt where prettyAnn = printStmt
instance PrettyAnnotated Expr where prettyAnn = printExpr
instance PrettyAnnotated Lit where prettyAnn = printLit
instance PrettyAnnotated Pat where prettyAnn = printPat
instance PrettyAnnotated MetaItem where prettyAnn = printMetaItem
instance PrettyAnnotated NestedMetaItem where prettyAnn = printMetaListItem
-------------------------------------------

-- | Pretty print an identifier
printIdent :: Ident -> Doc a
printIdent Ident{..} = let Name s = name in text s

-- | Pretty print a type
-- aka 'print_type' (with 'print_ty_fn' inlined)
printType :: Ty a -> Doc a
printType (Slice ty x)          = annotate x ("[" <> printType ty <> "]")
printType (Array ty v x)        = annotate x ("[" <> printType ty <> ";" <+> printExpr v <> "]")
printType (Ptr mut ty x)        = annotate x ("*" <> printFullMutability mut <+> printType ty)
printType (Rptr lt mut ty x)    = annotate x ("&" <> perhaps printLifetime lt <+> printMutability mut <+> printType ty)
printType (Never x)             = annotate x "!"
printType (TupTy [elt] x)       = annotate x ("(" <> printType elt <> ",)")
printType (TupTy elts x)        = annotate x ("(" <> align (fillSep (punctuate "," (printType `map` elts))) <> ")")
printType (PathTy Nothing p x)  = annotate x (printPath p False)
printType (PathTy (Just q) p x) = annotate x (printQPath p q False)
printType (ObjectSum ty bs x)   = annotate x (printType ty <+> printBounds "+" bs)
printType (PolyTraitRefTy bs x) = annotate x (printBounds mempty bs)
printType (ImplTrait bs x)      = annotate x (printBounds "impl" bs)
printType (ParenTy ty x)        = annotate x ("(" <> printType ty <> ")")
printType (Typeof e x)          = annotate x ("typeof(" <> printExpr e <> ")")
printType (Infer x)             = annotate x "_"
printType (ImplicitSelf x)      = annotate x "Self"
printType (MacTy m x)           = annotate x (printMac m Bracket)
printType (BareFn u a l d x)    = annotate x (printFormalLifetimeList l
                                                </> printFnHeaderInfo u NotConst a InheritedV
                                                <//> printFnArgsAndRet d)

-- aka print_mac
printMac :: Mac a -> DelimToken -> Doc a
printMac (Mac path tts x) d = annotate x (printPath path False <> "!" <> delimiter d body)
  where body = align (fillSep (punctuate "," [ printTt tt | tt <- tts ]))
 
-- | Given a delimiter token, this wraps the 'Doc' with that delimiter. For the 'Brace' case, tries
-- to fit everything on one line, but otherwise indents everything nicely.
delimiter :: DelimToken -> Doc a -> Doc a
delimiter Paren   = parens
delimiter Bracket = brackets
delimiter Brace   = block
delimiter NoDelim = id

-- aka print_tt
printTt :: TokenTree -> Doc a
printTt (Token _ t) = printToken t 
printTt (Delimited _ d _ tts _) = delimiter d (hcat (printTt <$> tts))
printTt (Sequence _ tts s op _) = "$" <> parens body <> perhaps printToken s <> suf
  where body = cat [ printTt tt | tt <- tts ]
        suf = case op of ZeroOrMore -> "*"
                         OneOrMore -> "+"

-- aka token_to_string
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
printToken Caret = "^"
printToken At = "@"
printToken Dot = "."
printToken DotDot = ".."
printToken DotDotDot = "..."
printToken Comma = ","
printToken Semicolon = ";"
printToken Colon = ":"
printToken ModSep = "::"
printToken RArrow = "<-"
printToken LArrow = "->"
printToken FatArrow = "=>"
printToken Pound = "#"
printToken Dollar = "$"
printToken Question = "?"
printToken (OpenDelim Paren) = "("
printToken (OpenDelim Bracket) = "["
printToken (OpenDelim Brace) = "{"
printToken (OpenDelim NoDelim) = mempty
printToken (CloseDelim Paren) = ")"
printToken (CloseDelim Bracket) = "]"
printToken (CloseDelim Brace) = "}"
printToken (CloseDelim NoDelim) = mempty
printToken (LiteralTok l s) = printLitTok l <> pretty s
printToken (IdentTok i) = printIdent i
printToken Underscore = "_"
printToken (LifetimeTok i) = "'" <> printIdent i
printToken (Space _ _) = error "Unimplemented"        -- ^ Whitespace
printToken (Doc _ _) = error "Unimplemented"      -- ^ Doc comment, contents, whether it is outer or not
printToken Shebang = "#!"
printToken Eof = mempty
printToken (Interpolated n) = noAnnotate (printNonterminal n)
printToken (MatchNt i s _ _) = "$" <> printIdent i <> ":" <> printIdent s
printToken (SubstNt s _) = "$" <> printIdent s
printToken _ = error "printToken"

printLitTok :: LitTok -> Doc a
printLitTok (ByteTok (Name v))         = text v
printLitTok (CharTok (Name v))         = text v
printLitTok (IntegerTok (Name v))      = text v
printLitTok (FloatTok (Name v))        = text v
printLitTok (StrTok (Name v))          = text v
printLitTok (StrRawTok (Name v) _)     = text v
printLitTok (ByteStrTok (Name v))      = text v
printLitTok (ByteStrRawTok (Name v) _) = text v

printNonterminal :: Nonterminal a -> Doc a
printNonterminal (NtItem item) = printItem item
printNonterminal (NtBlock blk) = printBlock blk
printNonterminal (NtStmt stmt) = printStmt stmt
printNonterminal (NtPat pat) = printPat pat
printNonterminal (NtExpr expr) = printExpr expr
printNonterminal (NtTy ty) = printType ty
printNonterminal (NtIdent ident) = printIdent ident
printNonterminal (NtMeta meta) = printMetaItem meta
printNonterminal (NtPath path) = printPath path False
printNonterminal (NtTT tt) = printTt tt
printNonterminal (NtArm arm) = printArm False arm
printNonterminal (NtImplItem item) = printImplItem item
printNonterminal (NtTraitItem item) = printTraitItem item
printNonterminal (NtGenerics generics) = printGenerics generics
printNonterminal (NtWhereClause clause) = printWhereClause clause
printNonterminal (NtArg arg) = printArg arg True -- todo double check True

-- aka print_stmt
printStmt :: Stmt a -> Doc a
printStmt (ItemStmt item x)   = annotate x (printItem item)
printStmt (NoSemi expr x)     = annotate x (printExprOuterAttrStyle expr False <> when (exprRequiresSemiToBeStmt expr) ";")
  where
  -- aka parse::classify::expr_requires_semi_to_be_stmt
  exprRequiresSemiToBeStmt :: Expr a -> Bool
  exprRequiresSemiToBeStmt If{} = False
  exprRequiresSemiToBeStmt IfLet{} = False
  exprRequiresSemiToBeStmt While{} = False
  exprRequiresSemiToBeStmt WhileLet{} = False
  exprRequiresSemiToBeStmt ForLoop{} = False
  exprRequiresSemiToBeStmt Loop{} = False
  exprRequiresSemiToBeStmt Match{} = False
  exprRequiresSemiToBeStmt BlockExpr{} = False
  exprRequiresSemiToBeStmt _ = True
printStmt (Semi expr x)       = annotate x (printExprOuterAttrStyle expr False <> ";")
printStmt (MacStmt m ms as x) = annotate x (printOuterAttrs as <+> printMac m delim <> end)
  where delim = case ms of { BracesMac -> Brace; _ -> Paren }
        end   = case ms of { SemicolonMac -> ";"; _ -> mempty }
printStmt (Local p ty i as x) = annotate x (printOuterAttrs as <+> "let" <+> binding <+> initializer <> ";")
  where binding = printPat p <> perhaps (\t -> ":" <+> printType t) ty
        initializer = perhaps (\e -> "=" <+> printExpr e) i

-- | Pretty print an expression.
printExpr :: Expr a -> Doc a
printExpr expr = printExprOuterAttrStyle expr True

-- print_expr_outer_attr_style
-- Inlined 'print_expr_in_place', 'print_expr_call, 'print_expr_method_call', 'print_expr_tup',
-- 'print_expr_binary', 'print_expr_unary', 'print_expr_addr_of', 'print_if', 'print_if_let',
-- 'print_expr_repeat'
printExprOuterAttrStyle :: Expr a -> Bool -> Doc a
printExprOuterAttrStyle expr isInline = glue (printEitherAttrs (expressionAttrs expr) Outer isInline) $
  case expr of
    Box _ expr' x               -> annotate x ("box" <+> printExpr expr')
    InPlace _ place expr x      -> annotate x (hsep [ printExprMaybeParen place, "<-", printExprMaybeParen expr ])
    Vec attrs exprs x           -> annotate x (brackets (printInnerAttrs attrs <+> commas exprs printExpr))
    Call _ func args x          -> annotate x (printExprMaybeParen func <> "(" <> commas args printExpr <> ")")
    MethodCall _ i tys (s:as) x -> let tys' = unless (null tys) ("::<" <> commas tys printType <> ">")
                                   in annotate x (hcat [ printExpr s, ".", printIdent i, tys', "(", commas as printExpr, ")" ])
    TupExpr as es x             -> annotate x ("(" <> printInnerAttrs as <+> commas es printExpr <> when (length es == 1) "," <> ")")
    Binary _ op lhs rhs x       -> annotate x (hsep [ checkExprBinNeedsParen lhs op, printBinOp op, checkExprBinNeedsParen rhs op ])
    Unary _ op expr x           -> annotate x (printUnOp op <> printExprMaybeParen expr)
    Lit _ lit x                 -> annotate x (printLit lit)
    Cast _ expr ty x            -> let expr' = case expr of { Cast{} -> printExpr expr; _ -> printExprMaybeParen expr }
                                   in annotate x (hsep [ expr', "as", printType ty ])
    TypeAscription _ expr ty x  -> annotate x (printExpr expr <> ":" <+> printType ty)
    If _ test blk els x         -> annotate x (hsep [ "if", printExpr test, printBlock blk, printElse els ])
    IfLet _ pat expr blk els x  -> annotate x (hsep [ "if let", printPat pat, "=", printExpr expr, printBlock blk, printElse els ])
    While as test blk lbl x     -> annotate x (hsep [ printLbl lbl, "while", printExpr test, printBlockWithAttrs blk as ])
    WhileLet as p e blk lbl x   -> annotate x (hsep [ printLbl lbl, "while let", printPat p, "=", printExpr e, printBlockWithAttrs blk as ])
    ForLoop as pat e blk lbl x  -> annotate x (hsep [ printLbl lbl, "for", printPat pat, "in", printExpr e, printBlockWithAttrs blk as ])
    Loop as blk lbl x           -> annotate x (hsep [ printLbl lbl, "loop", printBlockWithAttrs blk as ])
    Match attrs e arms x        -> let arms' = if null arms
                                                 then [] 
                                                 else (printArm True `map` Prelude.init arms) ++ [ printArm False (Prelude.last arms) ]
                                   in annotate x (hsep [ "match", printExpr e, braceAttrs (printInnerAttrs attrs) arms' ])
    Closure _ cap decl body x   -> annotate x (when (cap == Value) "move" <+> printFnBlockArgs decl <+> printBlockUnclosed body)
    BlockExpr attrs blk x       -> annotate x (printBlockWithAttrs blk attrs)
    Assign _ lhs rhs x          -> annotate x (hsep [ printExpr lhs, "=", printExpr rhs ])
    AssignOp _ op lhs rhs x     -> annotate x (hsep [ printExpr lhs, printBinOp op <> "=", printExpr rhs ])
    FieldAccess _ expr ident x  -> annotate x (hcat [ printExpr expr, ".", printIdent ident ])
    TupField _ expr num x       -> annotate x (hcat [ printExpr expr, ".", pretty num ])
    Index _ expr index x        -> annotate x (hcat [ printExpr expr, "[", printExpr index, "]" ])
    Range _ start end limits x  -> annotate x (hcat [ perhaps printExpr start, printRangeLimits limits, perhaps printExpr end ])
    PathExpr _ Nothing path x   -> annotate x (printPath path True)
    PathExpr _ (Just qs) path x -> annotate x (printQPath path qs True)
    AddrOf _ mut expr x         -> annotate x ("&" <> printMutability mut <+> printExprMaybeParen expr)
    Break _ brk x               -> annotate x ("break" <+> perhaps printIdent brk)
    Continue _ cont x           -> annotate x ("continue" <+> perhaps printIdent cont)
    Ret _ result x              -> annotate x ("return" <+> perhaps printExpr result)
    InlineAsmExpr _ inlineAsm x -> annotate x (printInlineAsm inlineAsm)
    MacExpr _ m x               -> annotate x (printMac m Paren)
    Struct as p fs Nothing x    -> annotate x (printPath p True <+>  braceCommaAttrs (printInnerAttrs as) (printField `map` fs))
    Struct as p fs (Just d) x   -> let body = [ printField f <> "," | f <- fs ] ++ [ ".." <> printExpr d ] 
                                   in annotate x (printPath p True <+> braceAttrs (printInnerAttrs as) body)
    Repeat attrs e cnt x        -> annotate x (brackets (printInnerAttrs attrs <+> printExpr e <> ";" <+> printExpr cnt))
    ParenExpr attrs expr x      -> annotate x (parens (printInnerAttrs attrs <+> printExpr expr))
    Try _ expr x                -> annotate x (printExpr expr <> "?")
  where printLbl = perhaps (\i -> printIdent i <> ":")
        glue = if isInline then (<+>) else (</>)

printInlineAsm :: InlineAsm a -> Doc a
printInlineAsm (InlineAsm asm asmSty outputs inputs clobbers volatile alignstack dialect x)
  = annotate x ("asm!" <> align (sep
      [ "(" <>  printStr asmSty asm
      , ":" <+> commas outputs printInlineAsmOutput
      , ":" <+> commas inputs printInlineAsmInput
      , ":" <+> commas clobbers (printStr Cooked)
      , ":" <+> commas options (printStr Cooked) <> ")"
      ]))
  where printInlineAsmInput (constraint, expr) = printStr Cooked constraint <> parens (printExpr expr)
        options = [ "volatile" | volatile ] ++ [ "alignstack" | alignstack ] ++ [ "intel" | dialect == Intel ]
  
printStr :: StrStyle -> String -> Doc a
printStr sty str = noAnnotate (printLit (Str str sty Unsuffixed ()))

printInlineAsmOutput :: InlineAsmOutput a -> Doc a
printInlineAsmOutput (InlineAsmOutput constraint expr isRw _) = printStr Cooked (prefix ++ constraint) <> parens (printExpr expr) 
  where prefix = case listToMaybe constraint of
                   Just '=' | isRw -> "+"
                   _ -> ""

expressionAttrs :: Expr a -> [Attribute a]
expressionAttrs (Box as _ _) = as
expressionAttrs (InPlace as _  _ _) = as
expressionAttrs (Vec as _ _) = as
expressionAttrs (Call as _  _ _) = as
expressionAttrs (MethodCall as _  _ _ _) = as
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
expressionAttrs (Closure as _ _ _ _) = as
expressionAttrs (BlockExpr as _ _) = as
expressionAttrs (Assign as _ _ _) = as
expressionAttrs (AssignOp as _ _ _ _) = as
expressionAttrs (FieldAccess as _ _ _) = as
expressionAttrs (TupField as _ _ _) = as
expressionAttrs (Index as _ _ _) = as
expressionAttrs (Range as _ _ _ _) = as
expressionAttrs (PathExpr as _ _ _) = as
expressionAttrs (AddrOf as _ _ _) = as
expressionAttrs (Break as _ _) = as
expressionAttrs (Continue as _ _) = as
expressionAttrs (Ret as _ _) = as
expressionAttrs (InlineAsmExpr as _ _) = as
expressionAttrs (MacExpr as _ _) = as
expressionAttrs (Struct as _ _ _ _) = as
expressionAttrs (Repeat as _ _ _) = as
expressionAttrs (ParenExpr as _ _) = as
expressionAttrs (Try as _ _) = as


printField :: Field a -> Doc a
printField Field{..} = annotate nodeInfo (printIdent ident <> ":" <+> printExpr expr)

printRangeLimits :: RangeLimits -> Doc a
printRangeLimits HalfOpen = ".."
printRangeLimits Closed = "..."

-- print_fn_block_args
printFnBlockArgs :: FnDecl a -> Doc a
printFnBlockArgs (FnDecl args ret _ x) = annotate x ("|" <> args' <> "|" <+> ret')
  where ret' = perhaps (\ty -> "->" <+> printType ty) ret
        args' = commas args (`printArg` True)

-- print_arm
printArm :: Bool -> Arm a -> Doc a
printArm end (Arm as pats guard body x) = annotate x (printOuterAttrs as
  <+> foldr1 (\x y -> x <+> "|" <+> y) (printPat <$> pats)
  <+> perhaps (\e -> "if" <+> printExpr e) guard
  <+> "=>"
  <+> case body of 
         BlockExpr _ blk _ -> printBlockUnclosed blk <> when (rules blk == UnsafeBlock False && end) ","
         _ -> printExpr body <> when end ",")

printBlock :: Block a -> Doc a
printBlock blk = printBlockWithAttrs blk []

-- print_block_unclosed/print_block_unclosed_indent
printBlockUnclosed :: Block a -> Doc a
printBlockUnclosed = printBlock -- TODO maybe?

-- print_block_unclosed_with_attrs
printBlockUnclosedWithAttrs :: Block a -> [Attribute a] -> Doc a
printBlockUnclosedWithAttrs = printBlockWithAttrs -- TODO maybe?

-- aka print_block_with_attrs
-- Actually print_block_maybe_unclosed
printBlockWithAttrs :: Block a -> [Attribute a] -> Doc a
printBlockWithAttrs (Block stmts rules x) as = annotate x (safety <+> braceAttrs (printInnerAttrs as) (body ++ [ lastStmt ]))
  where
  body = if null stmts then [] else printStmt `map` Prelude.init stmts
  
  lastStmt = unless (null stmts) (case last stmts of
                                    NoSemi expr _ -> printExprOuterAttrStyle expr False
                                    stmt -> printStmt stmt)

  safety = case rules of
             DefaultBlock -> mempty
             UnsafeBlock _ -> "unsafe"

-- print_else
printElse :: Maybe (Expr a) -> Doc a
printElse Nothing = mempty
printElse (Just (If _ e t s x))      = annotate x (hsep [ "else if", printExpr e, printBlock t, printElse s ])
printElse (Just (IfLet _ p e t s x)) = annotate x (hsep [ "else if let", printPat p, "=", printExpr e, printBlock t, printElse s ])
printElse (Just (BlockExpr _ blk x)) = annotate x (hsep [ "else", printBlock blk ])
printElse _ = error "printElse saw `if` with a weird alternative"

-- no similar
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

-- aka util::parser::AssocOp::precedence
opPrecedence :: BinOp -> Int
opPrecedence AddOp = 12
opPrecedence SubOp = 12
opPrecedence MulOp = 13
opPrecedence DivOp = 13
opPrecedence RemOp = 13
opPrecedence AndOp = 6
opPrecedence OrOp = 5
opPrecedence BitXorOp = 9
opPrecedence BitAndOp = 10
opPrecedence BitOrOp = 8
opPrecedence ShlOp = 11
opPrecedence ShrOp = 11
opPrecedence EqOp = 7
opPrecedence LtOp = 7
opPrecedence LeOp = 7
opPrecedence NeOp = 7
opPrecedence GeOp = 7
opPrecedence GtOp = 7

-- no similar
printUnOp :: UnOp -> Doc a
printUnOp Deref = "*"
printUnOp Not = "!"
printUnOp Neg = "-" 

-- aka print_literal
printLit :: Lit a -> Doc a
printLit lit = case lit of
    (Str     str Cooked  s x) -> annotate x (hcat [ "\"", foldMap escapeChar str, "\"", suffix s ])
    (Str     str (Raw n) s x) -> annotate x (hcat [ "r", pad n, "\"", text str, "\"", pad n, suffix s ])
    (ByteStr str Cooked  s x) -> annotate x (hcat [ "b\"", foldMap escapeByte (unpack str), "\"", suffix s ])
    (ByteStr str (Raw n) s x) -> annotate x (hcat [ "br", pad n, "\"", text (map byte2Char (unpack str)), "\"", pad n, suffix s ])
    (Char c s x)              -> annotate x (hcat [ "'",  escapeChar c, "'", suffix s ])
    (Byte b s x)              -> annotate x (hcat [ "b'", escapeByte b, "'", suffix s ])
    (Int i s x)               -> annotate x (hcat [ pretty i, suffix s ])
    (Float d s x)             -> annotate x (hcat [ pretty d,  suffix s ])
    (Bool True s x)           -> annotate x (hcat [ "true",  suffix s ])
    (Bool False s x)          -> annotate x (hcat [ "false", suffix s ])
  where
  pad :: Int -> Doc a
  pad n = text (replicate n '#')

  suffix :: Suffix -> Doc a
  suffix = text . show

-- | Extend a byte into a unicode character
byte2Char :: Word8 -> Char
byte2Char = chr . fromIntegral 
  
-- | Constrain a unicode character to a byte
-- This assumes the character is in the right range already
char2Byte :: Char -> Word8
char2Byte = fromIntegral . ord

-- | Escape a byte. Based on `std::ascii::escape_default`
escapeByte :: Word8 -> Doc a
escapeByte w8 = case byte2Char w8 of
  '\t' -> "\\t" 
  '\r' -> "\\r"
  '\n' -> "\\n"
  '\\' -> "\\\\" 
  '\'' -> "\\'"
  '"'  -> "\\\""
  c | 0x20 <= w8 && w8 <= 0x7e -> char c
  _ -> "\\x" <> padHex 2 w8

-- | Escape a unicode character. Based on `std::ascii::escape_default`
escapeChar :: Char -> Doc a
escapeChar c | c <= '\xff'   = escapeByte (char2Byte c)
             | c <= '\xffff' = "\\u" <> padHex 4 (ord c)
             | otherwise     = "\\U" <> padHex 8 (ord c)
 
-- | Convert a number to its padded hexadecimal form
padHex :: Integral a => Int -> a -> Doc b
padHex n 0 = text (replicate n '0')
padHex n m = let (m',r) = m `divMod` 0x10
             in padHex (n-1) m' <> char (intToDigit (fromIntegral r))

-- similar to check_expr_bin_needs_paren
checkExprBinNeedsParen :: Expr a -> BinOp -> Doc a
checkExprBinNeedsParen e@(Binary _ op' _ _ _) op | opPrecedence op' < opPrecedence op = "(" <> printExpr e <> ")"
checkExprBinNeedsParen e _ = printExpr e

-- aka print_expr_maybe_paren
printExprMaybeParen :: Expr a -> Doc a
printExprMaybeParen expr = let needs = needsParentheses expr
                           in when needs "(" <> printExpr expr <> when needs ")"
  where
  -- aka needs_parentheses
  needsParentheses :: Expr a -> Bool
  needsParentheses Assign{} = True
  needsParentheses Binary{} = True
  needsParentheses Closure{} = True
  needsParentheses AssignOp{} = True
  needsParentheses Cast{} = True
  needsParentheses InPlace{} = True
  needsParentheses TypeAscription{} = True
  needsParentheses _ = False

-- aka  print_inner_attributes / print_inner_attributes_no_trailing_hardbreak / print_inner_attributes_inline
-- (distinction has to be made at callsite whether to force newline)
printInnerAttrs :: [Attribute a] -> Doc a
printInnerAttrs attrs = printEitherAttrs attrs Inner False

-- aka  print_outer_attributes / print_outer_attributes_no_trailing_hardbreak
printOuterAttrs :: [Attribute a] -> Doc a
printOuterAttrs attrs = printEitherAttrs attrs Outer False

-- aka  print_either_attributes
printEitherAttrs :: [Attribute a] -> AttrStyle -> Bool -> Doc a
printEitherAttrs attrs kind inline = unless (null attrs') (glue attrs')
  where glue = if inline then hsep else vcat
        attrs' = [ printAttr attr inline | attr <- attrs, style attr == kind ]

-- aka  print_attribute_inline / print_attribute
printAttr :: Attribute a -> Bool -> Doc a
printAttr a@(Attribute style value isSugaredDoc x) inline
  | isSugaredDoc && inline = annotate x ("/*!" <+> perhaps text (valueStr a) <+> "*/")
  | isSugaredDoc           = annotate x ("//!" <+> perhaps text (valueStr a) <> line)
  | style == Inner         = annotate x ("#![" <> printMetaItem value <> "]")
  | style == Outer         = annotate x ("#[" <> printMetaItem value <> "]")

valueStr :: Attribute a -> Maybe String
valueStr (Attribute _ (NameValue _  (Str s _ _ _) _) _ _) = Just s
valueStr _ = Nothing

-- aka  print_meta_list_item
printMetaListItem :: NestedMetaItem a -> Doc a
printMetaListItem (MetaItem item x) = annotate x (printMetaItem item)
printMetaListItem (Literal lit x) = annotate x (printLit lit)

-- aka  print_meta_item
printMetaItem :: MetaItem a -> Doc a
printMetaItem (Word name x) = annotate x (printIdent name)
printMetaItem (NameValue name lit x) = annotate x (printIdent name <+> "=" <+> printLit lit)
printMetaItem (List name items x) = annotate x (printIdent name <> "(" <> commas items printMetaListItem <> ")")

-- | Synthesizes a comment that was not textually present in the original source file.
-- aka  synth_comment
synthComment :: String -> Doc a
synthComment com = "/*" <+> text com <+> "*/"

-- aka print_item
printItem :: Item a -> Doc a
printItem (Item ident attrs node vis x) = annotate x $ case node of
  ExternCrate p -> printVisibility vis <+> "extern" <+> "crate" <+> perhaps (error "unimplemented") p <+> printIdent ident <> ";"
  Use vp -> printVisibility vis <+> "use" <+> printViewPath vp <> ";"
  Static ty m expr -> printVisibility vis <+> "static" <+> when (m == Mutable) "mut" <+> printIdent ident <> ":" <+> printType ty <+> "=" <+> printExpr expr <> ";"
  ConstItem ty expr -> printVisibility vis <+> "const" <+> printIdent ident <> ":" <+> printType ty <+> "=" <+> printExpr expr <> ";"
  Fn decl unsafety constness abi tyParams body -> printFn decl unsafety constness abi (Just ident) tyParams vis <+> printBlockWithAttrs body attrs
  Mod items -> printVisibility vis <+> "mod" <+> printIdent ident <+> block (printMod items attrs)
  ForeignMod abi foreignItems -> "extern" <+> printAbi abi <+> block (printForeignMod foreignItems attrs)
  TyAlias ty params -> printVisibility vis <+> "type" <+> printIdent ident <> printGenerics params <> printWhereClause (whereClause params) <+> "=" <+> printType ty <> ";"
  Enum variants params -> printEnumDef variants params ident vis
  StructItem structDef generics -> printVisibility vis <+> "struct" <+> printStruct structDef generics ident True
  Union structDef generics -> printVisibility vis <+> "union" <+> printStruct structDef generics ident True
  DefaultImpl unsafety traitRef -> printVisibility vis <+> printUnsafety unsafety <+> "impl" <> printTraitRef traitRef <+> "for" <+> ".." <+> "{ }"
  Impl unsafety polarity generics traitRef_m ty implItems ->
      printVisibility vis <+> printUnsafety unsafety <+> "impl"
        <+> (case generics of { Generics [] [] _ _ -> mempty; _ -> printGenerics generics })
        <+> perhaps (\t -> printTraitRef t <+> "for") traitRef_m
        <+> printType ty
        <+> printWhereClause (whereClause generics)
        <+> block (vsep (printInnerAttrs attrs  : (printImplItem `map` implItems)))
  Trait unsafety generics typarambounds traitItems ->
      let seperateBounds :: [TyParamBound a] -> ([Doc a], [TyParamBound a])
          seperateBounds [] = ([],[])
          seperateBounds (t:ts) = case t of
                                    TraitTyParamBound ptr Maybe -> (("for ?" <+> printTraitRef (traitRef ptr)) : ds, tybs)
                                    _ -> (ds, t:tybs)
            where ~(ds,tybs) = seperateBounds ts

          (bounds,realBounds) = seperateBounds typarambounds
      in printVisibility vis <+> printUnsafety unsafety <+> "trait" <+> printIdent ident
            <+> printGenerics generics
            <+> hsep bounds
            <+> printBounds ":" realBounds
            <+> printWhereClause (whereClause generics)
            <+> "{" <+> hsep (printTraitItem <$> traitItems) <+> "}"
  MacItem m -> error "Unimplemented (pprust.rs line 1333)"


-- aka print_trait_item
printTraitItem :: TraitItem a -> Doc a
printTraitItem TraitItem{..} = annotate nodeInfo $ printOuterAttrs attrs <+>
  case node of
    ConstT ty default_m -> printAssociatedConst ident ty default_m InheritedV
    MethodT sig block_m -> printMethodSig ident sig InheritedV <+> maybe ";" (`printBlockWithAttrs` attrs) block_m
    TypeT bounds default_m -> printAssociatedType ident (Just bounds) default_m
    MacroT m -> error "Unimplemented (pprust.rs line 1572)"


-- aka print_bounds
printBounds :: Doc a -> [TyParamBound a] -> Doc a
printBounds _ [] = mempty
printBounds prefix (b:bs) = align (fillSep ((prefix <+> printBound b) : [ "+" <+> printBound b | b <- bs ]))
  where
  printBound :: TyParamBound a -> Doc a
  printBound (RegionTyParamBound lt) = printLifetime lt
  printBound (TraitTyParamBound PolyTraitRef{..} modi) =
    case modi of { Maybe -> "?"; _ -> mempty }
      <> printFormalLifetimeList boundLifetimes
      <+> printTraitRef traitRef

-- aka print_formal_lifetime_list
printFormalLifetimeList :: [LifetimeDef a] -> Doc a
printFormalLifetimeList [] = mempty
printFormalLifetimeList defs = "for" <> angles (align (fillSep (punctuate "," (printDef `map` defs))))
  where printDef LifetimeDef{..} = printOuterAttrs attrs <+> flatten (printLifetimeBounds lifetime bounds)

-- aka print_impl_item
printImplItem :: ImplItem a -> Doc a
printImplItem ImplItem{..} =
  printOuterAttrs attrs
    <+> (case defaultness of { Default -> "default" <> space; Final -> mempty })
    <> (case node of
            ConstI ty expr -> printAssociatedConst ident ty (Just expr) vis
            MethodI sig body -> printMethodSig ident sig vis <+> printBlockWithAttrs body attrs
            TypeI ty -> printAssociatedType ident Nothing (Just ty) 
            MacroI m -> error "Unimplemented (pprust.rs line 1608")

-- aka printAssociatedType
printAssociatedType :: Ident ->  Maybe [TyParamBound a] -> Maybe (Ty a) -> Doc a
printAssociatedType ident bounds_m ty_m = "type" <+> printIdent ident
  <+> perhaps (printBounds ":") bounds_m
  <+> perhaps (\ty -> "=" <+> printType ty) ty_m
  <> ";"

-- aka print_method_sig
printMethodSig :: Ident -> MethodSig a -> Visibility a -> Doc a
printMethodSig ident MethodSig{..} = printFn decl unsafety constness abi (Just ident) generics

-- aka print_associated_const
printAssociatedConst :: Ident -> Ty a -> Maybe (Expr a) -> Visibility a -> Doc a
printAssociatedConst ident ty default_m vis = printVisibility vis
  <+> "const" <+> printIdent ident <> ":" <+> printType ty
  <+> perhaps (\expr -> "=" <+> printExpr expr) default_m
  <> ";"

-- no aka
printPolarity :: ImplPolarity -> Doc a
printPolarity Negative = "!"
printPolarity Positive = mempty

-- aka print_visibility
printVisibility :: Visibility a -> Doc a
printVisibility PublicV = "pub"
printVisibility CrateV = "pub(crate)"
printVisibility (RestrictedV path) = "pub(" <> printPath path False <> ")"
printVisibility InheritedV = mempty

-- aka print_foreign_item
printForeignItem :: ForeignItem a -> Doc a
printForeignItem ForeignItem{..} = printOuterAttrs attrs <+>
  case node of
    ForeignFn decl generics -> printFn decl Normal NotConst Rust (Just ident) generics vis <> ";"
    ForeignStatic ty mut -> printVisibility vis <+> "static" <+> when mut "mut" <+> printIdent ident <> ":" <+> printType ty <> ";"


-- aka print_struct
printStruct :: VariantData a -> Generics a -> Ident -> Bool -> Doc a
printStruct structDef generics@Generics{..} ident printFinalizer =
  printIdent ident <+> printGenerics generics
    <+> case structDef of 
          StructD fields _ -> printWhereClause whereClause <+> braceComma (printStructField `map` fields)
          TupleD fields _ -> "(" <> commas fields printStructField <> ")" <+> printWhereClause whereClause <+> when printFinalizer ";" 
          UnitD _ -> "()" <+> printWhereClause whereClause <+> when printFinalizer ";"


printStructField :: StructField a -> Doc a
printStructField (StructField i v t as x) = annotate x (printOuterAttrs as <+> printVisibility v <+> perhaps (\i -> printIdent i <> ":") i <+> printType t)

-- aka print_unsafety
printUnsafety :: Unsafety -> Doc a
printUnsafety Normal = mempty
printUnsafety Unsafe = "unsafe"

-- aka print_enum_def
printEnumDef :: [Variant a] -> Generics a -> Ident -> Visibility a -> Doc a
printEnumDef variants generics ident vis =
  printVisibility vis <+> "enum" <+> printIdent ident <+> printGenerics generics
    <+> printWhereClause (whereClause generics) <+> "{"
    <+> sep ((\v@Variant{..} -> printOuterAttrs attrs <+> printVariant v <> ",") <$> variants)
    <+> "}"

-- print_variant
printVariant :: Variant a -> Doc a
printVariant Variant{..} = 
  perhaps (\e -> "=" <+> printExpr e) disrExpr

-- aka print_where_clause
printWhereClause :: WhereClause a -> Doc a
printWhereClause WhereClause{..}
  | null predicates = mempty
  | otherwise = "where" <+> commas predicates printWherePredicate
  where
  printWherePredicate :: WherePredicate a -> Doc a
  printWherePredicate BoundPredicate{..} = printFormalLifetimeList boundLifetimes <+> printType boundedTy <> printBounds ":" traitLifetimeBounds 
  printWherePredicate RegionPredicate{..} = printLifetimeBounds lifetime lifetimeBounds
  printWherePredicate EqPredicate{..} = printPath path False <+> "=" <+> printType ty

-- aka  print_fn
printFn :: FnDecl a -> Unsafety -> Constness -> Abi -> Maybe Ident -> Generics a -> Visibility a -> Doc a
printFn decl unsafety constness abi name generics vis =
  printFnHeaderInfo unsafety constness abi vis
    <+> perhaps printIdent name
    <> printGenerics generics
    <> printFnArgsAndRet decl
    <+> printWhereClause (whereClause generics)

-- aka print_fn_args_and_ret
printFnArgsAndRet :: FnDecl a -> Doc a
printFnArgsAndRet (FnDecl args ret var x) = annotate x ("(" <> align (fillSep args') <> ")" </> ret')
  where ret' = perhaps (\t -> "->" <+> printType t) ret
        args' = if var then [ printArg a False <> "," | a <- args ] ++ [ "..." ]
                       else punctuate "," ((`printArg` False) `map` args)

-- aka print_arg TODO double check this
printArg :: Arg a -> Bool -> Doc a
printArg Arg{..} isClosure = case ty of
  Infer _ | isClosure -> printPat pat
  _ -> let (IdentP _ ident _ _) = pat
           invalid = ident == mkIdent ""
       in unless invalid (printPat pat <> ":") <+> printType ty

-- print_explicit_self
printExplicitSelf :: SelfKind a -> Doc a
printExplicitSelf (ValueSelf mut) = printMutability mut <+> "self"
printExplicitSelf (Region lifetime_m mut) = "&" <> perhaps printLifetime lifetime_m <+> printMutability mut <+> "self"
printExplicitSelf (Explicit ty mut) = printMutability mut <+> "self" <> ":" <+> printType ty

-- aka print_lifetime
printLifetime :: Lifetime a -> Doc a
printLifetime (Lifetime n x) = annotate x ("'" <> printName n)

-- print_mutability
printMutability :: Mutability -> Doc a
printMutability Mutable = "mut"
printMutability Immutable = mempty

printFullMutability :: Mutability -> Doc a
printFullMutability Mutable = "mut"
printFullMutability Immutable = "const"

-- print_pat
printPat :: Pat a -> Doc a
printPat (WildP x)                      = annotate x "_"
printPat (IdentP bm p s x)              = annotate x (printBindingMode bm <+> printIdent p <+> perhaps (\p -> "@" <> printPat p) s)
printPat (StructP p fs b x)             = annotate x (printPath p True <+> braceComma body)
  where body = (printFieldPat `map` fs) ++ (if b then [] else [ ".." ])
printPat (TupleStructP p es Nothing x)  = annotate x (printPath p True <> "(" <> commas es printPat <> ")")
printPat (TupleStructP p es (Just d) x) = let (before,after) = splitAt d es
  in printPath p True <> "(" <> commas before printPat <> unless (null es) ","
      <+> ".." <> when (d /= length es) ("," <+> commas after printPat) <> ")"
printPat (PathP Nothing path x)         = annotate x (printPath path True)
printPat (PathP (Just qself) path x)    = annotate x (printQPath path qself False)
printPat (TupleP elts Nothing x)        = annotate x ("(" <> commas elts printPat <> ")")
printPat (TupleP elts (Just ddpos) _) = let (before,after) = splitAt ddpos elts
  in "(" <> commas before printPat <> unless (null elts) ","
      <+> ".." <> when (ddpos /= length elts) ("," <+> commas after printPat) <> ")"
printPat (BoxP inner x)                 = annotate x ("box" <+> printPat inner)
printPat (RefP inner mutbl x)           = annotate x ("&" <> printMutability mutbl <+> printPat inner)
printPat (LitP expr x)                  = annotate x (printExpr expr)
printPat (RangeP lo hi x)               = annotate x (printExpr lo <+> "..." <+> printExpr hi)
printPat (SliceP pb ps pa x)            = annotate x ("[" <> commas pb printPat <> ps' <> commas pa printPat <> "]")
  where ps' = unless (null pb) "," <+> perhaps slicePat ps <> ".." <+> unless (null pa) ","
        slicePat p = case p of { WildP{} -> mempty; _ -> printPat p }
printPat (MacP m x)                     = annotate x (printMac m Paren)

printFieldPat :: FieldPat a -> Doc a
printFieldPat (FieldPat i p s x) = annotate x (when s (printIdent i <> ":") <+> printPat p)

printBindingMode :: BindingMode -> Doc a
printBindingMode (ByRef mutbl) = "ref" <+> printMutability mutbl
printBindingMode (ByValue Immutable) = mempty
printBindingMode (ByValue Mutable) = "mut"


-- aka  print_fn_header_info
printFnHeaderInfo :: Unsafety -> Constness -> Abi -> Visibility a -> Doc a
printFnHeaderInfo u c a v = foldr1 (<+>) parts
  where parts = [ printVisibility v, case c of { Const -> "const"; _ -> mempty }, printUnsafety u, printAbi a, "fn" ]

printAbi :: Abi -> Doc a
printAbi Rust = mempty
printAbi abi = "extern" <+> raw abi
  where
  raw Cdecl = "\"cdecl\""
  raw Stdcall = "\"stdcall\""
  raw Fastcall = "\"fastcall\""
  raw Vectorcall = "\"\""
  raw Aapcs = "\"aapcs\""
  raw Win64 = "\"win64\""
  raw SysV64 = "\"sysv64\""
  raw Rust = "\"Rust\""
  raw C = "\"C\""
  raw System = "\"system\""
  raw RustIntrinsic = "\"rust-intrinsic\""
  raw RustCall = "\"rust-call\""
  raw PlatformIntrinsic = "\"platform-intrinsic\""


-- aka print_mod
printMod :: [Item a] -> [Attribute a] -> Doc a
printMod items attrs = vsep (printInnerAttrs attrs : (printItem `map` items))

-- aka print_foreign_mod
printForeignMod :: [ForeignItem a] -> [Attribute a] -> Doc a
printForeignMod items attrs = vsep (printInnerAttrs attrs : (printForeignItem `map` items))

-- aka  print_generics
printGenerics :: Generics a -> Doc a
printGenerics Generics{..}
  | null lifetimes && null tyParams = mempty
  | otherwise =  let lifetimes' = [ printOuterAttrs as <+> printLifetimeBounds lt bds | LifetimeDef as lt bds _ <- lifetimes ]
                     bounds' = [ printTyParam param | param<-tyParams ]
                 in "<" <> hsep (punctuate "," (lifetimes' ++ bounds')) <> ">"


-- aka  print_poly_trait_ref
printPolyTraitRef :: PolyTraitRef a -> Doc a
printPolyTraitRef PolyTraitRef{..} = printFormalLifetimeList boundLifetimes <+> printTraitRef traitRef

-- aka  print_trait_ref
printTraitRef :: TraitRef a -> Doc a
printTraitRef TraitRef{..} = printPath path False

-- aka print_path_parameters
printPathParameters :: PathParameters a -> Bool -> Doc a
printPathParameters (AngleBracketed [] [] [] _) _ = mempty
printPathParameters Parenthesized{..} colons = when colons "::" <> "(" <> commas inputs printType <> ")" <+> perhaps (\t -> "->" <+> printType t) output
printPathParameters AngleBracketed{..} colons = when colons "::" <> "<" <> hsep (punctuate "," (lifetimes' ++ types' ++ bindings')) <> ">"
  where
    lifetimes' = printLifetime <$> lifetimes
    types' = printType <$> types
    bindings' = (\(ident,ty) -> printIdent ident <+> "=" <+> printType ty) <$> bindings

-- | second argument says whether to put colons before params
-- aka print_path
printPath :: Path a -> Bool -> Doc a
printPath Path{..} colons = when global "::" <> hcat (punctuate "::" (printSegment <$> segments))
  where
  printSegment :: (Ident, PathParameters a) -> Doc a
  printSegment (ident,parameters) = printIdent ident <> printPathParameters parameters colons

-- print_qpath
printQPath :: Path a -> QSelf a -> Bool -> Doc a
printQPath Path{..} QSelf{..} colons =  
  "<" <> printType ty <+> when (position > 0) ("as" <+> printPath (Path global (take position segments) nodeInfo) False) 
      <> ">" <> "::" <> printIdent ident <> printPathParameters parameters colons
  where
  (ident, parameters) = last segments 

-- aka print_view_path
printViewPath :: ViewPath a -> Doc a
printViewPath (ViewPathSimple ident path _) = printPath path False <+> when (fst (last (segments path)) /= ident) ("as" <+> printIdent ident)
printViewPath (ViewPathGlob path _) = printPath path False <> "::*"
printViewPath (ViewPathList path idents _) = prefix <> "::{" <> commas idents printPathListItem <> "}"
  where
  prefix = if null (segments path) then "{" else printPath path False

  printPathListItem :: PathListItem a -> Doc a
  printPathListItem (PathListItem name (Just rename) _) = printIdent name <+> "as" <+> printIdent rename
  printPathListItem (PathListItem name Nothing _) = printIdent name


-- aka print_ty_param
printTyParam :: TyParam a -> Doc a
printTyParam TyParam{..} = printOuterAttrs attrs
  <+> printIdent ident <> printBounds ":" bounds
  <+> perhaps (\d -> "=" <+> printType d) default_

printName :: Name -> Doc a
printName (Name s) = text s

-- aka print_lifetime_bounds
printLifetimeBounds :: Lifetime a -> [Lifetime a] -> Doc a
printLifetimeBounds lifetime bounds = printLifetime lifetime
  <> unless (null bounds) (":" <+> foldr1 (\x y -> x <+> "+" <+> y) (printLifetime <$> bounds))
