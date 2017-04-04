{-|
Module      : Language.Rust.Pretty.Internal
Description : Rust pretty-printer
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : portable

The pretty-printing facilities in this file are re-exported to 'Language.Rust.Pretty' via the
'Pretty' and 'PrettyAnnotated' classes. There may be invariants in this module that are not properly
documented. 
-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# OPTIONS_HADDOCK hide, not-home #-}
{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module Language.Rust.Pretty.Internal where

import Language.Rust.Syntax.AST
import Language.Rust.Syntax.Token
import Language.Rust.Syntax.Ident

import Text.PrettyPrint.Annotated.WL (pretty, hcat, cat, indent, punctuate, group, angles, space, flatten, align, fillSep, text, vcat, char, annotate, noAnnotate, parens, brackets, (<>), Doc)
import qualified Text.PrettyPrint.Annotated.WL as WL

import Data.ByteString (unpack)
import Data.Char (intToDigit, ord, chr)
import Data.Either (lefts, rights)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as N
import Data.Maybe (listToMaybe)
import Data.Word (Word8)

-- Rust style guide to explore:
-- Useful stuff from based on https://aturon.github.io/README.html

-- * Utility functions
-- Wadler's take on pretty-printing is super useful because it allows us to print thinks like blocks
-- much more nicely (see [this](http://stackoverflow.com/a/41424946/3072788)) that Hughes'.
-- Unfortunately, unlike Hughes', Wadler does not have 'mempty' as the identity of @<+>@ - the
-- space between the arguments of @<+>@ does not go away even if either argument is empty. The
-- same problem shows up for @hsep@, @<#>@, @vsep@, @</>@, etc.
--
-- My solution has been to redefine my versions of these functions which _do_ treat 'mempty' as a
-- neutral element for @<+>@, @hsep@, @<#>@, @vsep@, and @</>@.

-- | Map the list of items into 'Doc's using the provided function and add comma punctuation
commas :: [a] -> (a -> Doc b) -> Doc b
commas xs f = hsep (punctuate "," (map f xs))

-- | Take a binary operation on docs and lift it to one that has (left and right) identity 'mempty'
liftOp :: (Doc a -> Doc a -> Doc a) -> Doc a -> Doc a -> Doc a
liftOp _ WL.Empty d = d
liftOp _ d WL.Empty = d
liftOp (#) d d' = d # d'

-- | Lifted version of Wadler's @<+>@
(<+>) :: Doc a -> Doc a -> Doc a
(<+>) = liftOp (WL.<+>)

-- | Lifted version of Wadler's @hsep@
hsep :: Foldable f => f (Doc a) -> Doc a
hsep = foldr (<+>) mempty

-- | Lifted version of Wadler's @<#>@
(<#>) :: Doc a -> Doc a -> Doc a
(<#>) = liftOp (WL.<#>)

-- | Lifted version of Wadler's @vsep@
vsep :: Foldable f => f (Doc a) -> Doc a
vsep = foldr (<#>) mempty

-- | Lifted version of Wadler's @</>@
(</>) :: Doc a -> Doc a -> Doc a
(</>) = liftOp (WL.</>)

-- | Unless the condition holds, print the document
unless :: Bool -> Doc a -> Doc a
unless b = when (not b)

-- | When the condition holds, print the document
when :: Bool -> Doc a -> Doc a
when cond d = if cond then d else mempty

-- | Apply a printing function to an optional value. If the value is 'Nothing', 'perhaps' returns
-- the empty 'Doc'.
perhaps :: (a -> Doc b) -> Maybe a -> Doc b
perhaps = maybe mempty

-- | Checks that a 'Doc' will for sure render on a single line 
oneLine :: Doc a -> Bool
oneLine (WL.FlatAlt d _) = oneLine d
oneLine (WL.Cat a b) = oneLine a && oneLine b
oneLine (WL.Union a b) = oneLine a && oneLine b
oneLine (WL.Annotate _ d) = oneLine d
oneLine WL.Line = False
oneLine _ = True

-- | Make a curly-brace delimited block, where the body is just one 'Doc'. When possible, permit
-- fitting everything on one line.
block :: Doc a -> Doc a
block b | oneLine b = hsep [ "{", b, "}" ] `WL.Union` vsep [ "{", indent 2 b, "}" ]
        | otherwise = vsep [ "{", indent 2 b, "}" ]
 
-- | Same as 'block', but the lines are passed in as a list and the elements of the output are comma
-- delimited
blockComma :: [Doc a] -> Doc a
blockComma = blockAttrsPunctuated mempty ","

-- | Same as 'blockComma', but with the possibility of adding an attribute 'Doc'
blockAttrsComma :: Doc a -> [Doc a] -> Doc a
blockAttrsComma as = blockAttrsPunctuated as ","

-- | Same as 'blockAttrsComma', but without the commas
blockAttrs :: Doc a -> [Doc a] -> Doc a
blockAttrs as = blockAttrsPunctuated as mempty

-- | The usual C-style language brace block, with the hanging indent, but adapted for Rust. The
-- first argument is the attributes, then the seperator between lines (between every element if
-- everything fits on one line, but otherwise also after the last element), and finally the lines
-- themselves.
blockAttrsPunctuated :: Doc a -> Doc a -> [Doc a] -> Doc a
blockAttrsPunctuated WL.Empty _ [] = "{ }"
blockAttrsPunctuated as _ [] = "{" <+> as <+> "}"
blockAttrsPunctuated as s ds = if all oneLine ds then WL.Union singleLine multiLine else multiLine
  where
    (ds',d') = (Prelude.init ds, Prelude.last ds)
    singleLine = "{" <+>           hsep (as : [ d <> s | d <- ds' ])  <+> d' <+> "}"
    multiLine = "{" <#> indent 2 (vsep (as : [ d <> s | d <- ds  ])) <#>        "}"

-- | Given a delimiter token, this wraps the 'Doc' with that delimiter. For the 'Brace' case, tries
-- to fit everything on one line, but otherwise indents everything nicely.
delimiter :: Delim -> Doc a -> Doc a
delimiter Paren   = parens
delimiter Bracket = brackets
delimiter Brace   = block
delimiter NoDelim = id

-- * Pretty printing the AST
-- As much as possible, these functions should say which function in the @rustc@ printer they are
-- emulating.

-- | Print a crate
printCrate :: Crate a -> Doc a
printCrate (Crate items as macs x) = annotate x (vcat ls)
  where ls = [ printAttr a False | a <- as ] ++ [""] ++ [ printItem item | item <- items ]

-- | Print a name
printName :: Name -> Doc a
printName = text

-- | Print an identifier
printIdent :: Ident -> Doc a
printIdent (Ident s _) = text s

-- | Print a type (@print_type@ with @print_ty_fn@ inlined)
-- Types are expected to always be only one line
printType :: Ty a -> Doc a
printType (Slice ty x)          = annotate x ("[" <> printType ty <> "]")
printType (Array ty v x)        = annotate x ("[" <> printType ty <> ";" <+> printExpr v <> "]")
printType (Ptr mut ty x)        = annotate x ("*" <> printFullMutability mut <+> printType ty)
printType (Rptr lt mut ty x)    = annotate x ("&" <> perhaps printLifetime lt <+> printMutability mut <+> printType ty)
printType (Never x)             = annotate x "!"
printType (TupTy [elt] x)       = annotate x ("(" <> printType elt <> ",)")
printType (TupTy elts x)        = annotate x ("(" <> hsep (punctuate "," (printType `map` elts)) <> ")")
printType (PathTy Nothing p x)  = annotate x (printPath p False)
printType (PathTy (Just q) p x) = annotate x (printQPath p q False)
printType (TraitObject bs x)    = annotate x (printBounds mempty (toList bs))
printType (ImplTrait bs x)      = annotate x (printBounds "impl" (toList bs))
printType (ParenTy ty x)        = annotate x ("(" <> printType ty <> ")")
printType (Typeof e x)          = annotate x ("typeof(" <> printExpr e <> ")")
printType (Infer x)             = annotate x "_"
printType (MacTy m x)           = annotate x (printMac m Bracket)
printType (BareFn u a l d x)    = annotate x (printFormalLifetimeList l
                                               <+> printFnHeaderInfo u NotConst a InheritedV
                                               <> printFnArgsAndRet d)

-- | Print a macro (@print_mac@)
printMac :: Mac a -> Delim -> Doc a
printMac (Mac path tts x) d = annotate x (printPath path False <> "!" <> delimiter d body)
  where body = align (fillSep [ printTt tt | tt <- tts ])
 

-- | Print a token tree (@print_tt@)
printTt :: TokenTree -> Doc a
printTt (Token _ t) = printToken t 
printTt (Delimited _ d _ tts _) = delimiter d (hcat (printTt <$> tts))
printTt (Sequence _ tts s op) = "$" <> parens body <> perhaps printToken s <> suf
  where body = cat [ printTt tt | tt <- tts ]
        suf = case op of ZeroOrMore -> "*"
                         OneOrMore -> "+"

-- | Print a token (@token_to_string@)
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
printToken Dollar = "#"
printToken Question = "?"
printToken (OpenDelim Paren) = "("
printToken (OpenDelim Bracket) = "["
printToken (OpenDelim Brace) = "{"
printToken (OpenDelim NoDelim) = mempty
printToken (CloseDelim Paren) = ")"
printToken (CloseDelim Bracket) = "]"
printToken (CloseDelim Brace) = "}"
printToken (CloseDelim NoDelim) = mempty
printToken (LiteralTok l s) = printLitTok l <> perhaps printName s
printToken (IdentTok i) = printIdent i
printToken Underscore = "_"
printToken (LifetimeTok i) = "'" <> printIdent i
printToken (Space _ _) = error "Unimplemented"        --  Whitespace
printToken (Doc _ _) = error "Unimplemented"      --  Doc comment, contents, whether it is outer or not
printToken Shebang = "#!"
printToken Eof = mempty
printToken (Interpolated n) = noAnnotate (printNonterminal n)
printToken (MatchNt i s) = "$" <> printIdent i <> ":" <> printIdent s
printToken (SubstNt s) = "$" <> printIdent s
printToken _ = error "printToken"

-- | Print a literal token
printLitTok :: LitTok -> Doc a
printLitTok (ByteTok n)         = "b'" <> printName n <> "'"
printLitTok (CharTok n)         = "'" <> printName n <> "'"
printLitTok (IntegerTok n)      = printName n
printLitTok (FloatTok n)        = printName n
printLitTok (StrTok n)          = "\"" <> printName n <> "\""
printLitTok (StrRawTok n m)     = let pad = text (replicate m '#') in "r" <> pad <> "\"" <> printName n <> "\"" <> pad
printLitTok (ByteStrTok n)      = "b\"" <> printName n <> "\""
printLitTok (ByteStrRawTok n m) = let pad = text (replicate m '#') in "rb" <> pad <> "\"" <> printName n <> "\"" <> pad

-- | Print a nonterminal
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
printStmt (MacStmt m ms as x) = annotate x (printOuterAttrs as </> printMac m delim <> end)
  where delim = case ms of { BracesMac -> Brace; _ -> Paren }
        end   = case ms of { SemicolonMac -> ";"; _ -> mempty }
printStmt (Local p ty i as x) = annotate x (printOuterAttrs as </> "let" <+> binding <+> initializer <> ";")
  where binding = printPat p <> perhaps (\t -> ":" <+> printType t) ty
        initializer = perhaps (\e -> "=" <+> printExpr e) i

-- | Print an expression
printExpr :: Expr a -> Doc a
printExpr expr = printExprOuterAttrStyle expr True

-- | Print an expression (@print_expr_outer_attr_style@)
-- Inlined @print_expr_in_place@, @print_expr_call@, @print_expr_method_call@, @print_expr_tup@,
-- @print_expr_binary@, @print_expr_unary@, @print_expr_addr_of@, @print_if@, @print_if_let@,
-- @print_expr_repeat@
printExprOuterAttrStyle :: Expr a -> Bool -> Doc a
printExprOuterAttrStyle expr isInline = glue (printEitherAttrs (expressionAttrs expr) Outer isInline) $
  case expr of
    Box _ e x                   -> annotate x ("box" <+> printExpr e)
    InPlace _ place e x         -> annotate x (hsep [ printExpr place, "<-", printExpr e ])
    Vec as exprs x              -> annotate x (brackets (printInnerAttrs as <+> commas exprs printExpr))
    Call _ func args x          -> annotate x (printExpr func <> "(" <> commas args printExpr <> ")")
    MethodCall _ s i ts' as x   -> let tys' = perhaps (\ts -> "::<" <> commas ts printType <> ">") ts'
                                   in annotate x (hcat [ printExpr s, ".", printIdent i, tys', "(", commas as printExpr, ")" ])
    TupExpr as es x             -> annotate x ("(" <> printInnerAttrs as <+> commas es printExpr <> when (length es == 1) "," <> ")")
    Binary _ op lhs rhs x       -> annotate x (hsep [ printExpr lhs, printBinOp op, printExpr rhs ])
    Unary _ op e x              -> annotate x (printUnOp op <> printExpr e)
    Lit _ lit x                 -> annotate x (printLit lit)
    Cast _ e ty x               -> let f = case e of { Cast{} -> printExpr; _ -> printExpr }
                                   in annotate x (hsep [ f e, "as", printType ty ])
    TypeAscription _ e ty x     -> annotate x (printExpr e <> ":" <+> printType ty)
    If _ test blk els x         -> annotate x (hsep [ "if", printExpr test, printBlock blk, printElse els ])
    IfLet _ pat e blk els x     -> annotate x (hsep [ "if let", printPat pat, "=", printExpr e, printBlock blk, printElse els ])
    While as test blk lbl x     -> annotate x (hsep [ printLbl lbl, "while", printExpr test, printBlockWithAttrs blk as ])
    WhileLet as p e blk lbl x   -> annotate x (hsep [ printLbl lbl, "while let", printPat p, "=", printExpr e, printBlockWithAttrs blk as ])
    ForLoop as pat e blk lbl x  -> annotate x (hsep [ printLbl lbl, "for", printPat pat, "in", printExpr e, printBlockWithAttrs blk as ])
    Loop as blk lbl x           -> annotate x (hsep [ printLbl lbl, "loop", printBlockWithAttrs blk as ])
    Match as e arms x           -> let arms' = if null arms
                                                 then [] 
                                                 else (printArm True `map` Prelude.init arms) ++ [ printArm False (Prelude.last arms) ]
                                   in annotate x (hsep [ "match", printExpr e, blockAttrs (printInnerAttrs as) arms' ])
    Closure _ cap decl body x   -> annotate x (when (cap == Value) "move" <+> printFnBlockArgs decl <+> printExpr body)
    BlockExpr attrs blk x       -> annotate x (printBlockWithAttrs blk attrs)
    Assign _ lhs rhs x          -> annotate x (hsep [ printExpr lhs, "=", printExpr rhs ])
    AssignOp _ op lhs rhs x     -> annotate x (hsep [ printExpr lhs, printBinOp op <> "=", printExpr rhs ])
    FieldAccess _ e ident x     -> annotate x (hcat [ printExpr e, ".", printIdent ident ])
    TupField _ e num x          -> annotate x (hcat [ printExpr e, ".", pretty num ])
    Index _ e index x           -> annotate x (hcat [ printExpr e, "[", printExpr index, "]" ])
    Range _ start end limits x  -> annotate x (hcat [ perhaps printExpr start, printRangeLimits limits, perhaps printExpr end ])
    PathExpr _ Nothing path x   -> annotate x (printPath path True)
    PathExpr _ (Just qs) path x -> annotate x (printQPath path qs True)
    AddrOf _ mut e x            -> annotate x ("&" <> printMutability mut <+> printExpr e)
    Break _ brk e x             -> annotate x ("break" <+> perhaps printLifetime brk <+> perhaps printExpr e)
    Continue _ cont x           -> annotate x ("continue" <+> perhaps printLifetime cont)
    Ret _ result x              -> annotate x ("return" <+> perhaps printExpr result)
    InlineAsmExpr _ inlineAsm x -> annotate x (printInlineAsm inlineAsm)
    MacExpr _ m x               -> annotate x (printMac m Paren)
    Struct as p fs Nothing x    -> annotate x (printPath p True <+>  blockAttrsComma (printInnerAttrs as) (printField `map` fs))
    Struct as p fs (Just d) x   -> let body = [ printField f <> "," | f <- fs ] ++ [ ".." <> printExpr d ] 
                                   in annotate x (printPath p True <+> blockAttrs (printInnerAttrs as) body)
    Repeat attrs e cnt x        -> annotate x (brackets (printInnerAttrs attrs <+> printExpr e <> ";" <+> printExpr cnt))
    ParenExpr attrs e x         -> annotate x (parens (printInnerAttrs attrs <+> printExpr e))
    Try _ e x                   -> annotate x (printExpr e <> "?")
  where printLbl = perhaps (\i -> printLifetime i <> ":")
        glue = if isInline then (<+>) else (</>)

-- | Print inline assembly
printInlineAsm :: InlineAsm a -> Doc a
printInlineAsm (InlineAsm asm asmSty outputs inputs clobbers volatile alignstack dialect x)
  = annotate x ("asm!" <> align (group (vsep
      [ "(" <>  printStr asmSty asm
      , ":" <+> commas outputs printInlineAsmOutput
      , ":" <+> commas inputs printInlineAsmInput
      , ":" <+> commas clobbers (printStr Cooked)
      , ":" <+> commas options (printStr Cooked) <> ")"
      ])))
  where printInlineAsmInput (constraint, expr) = printStr Cooked constraint <> parens (printExpr expr)
        options = [ "volatile" | volatile ] ++ [ "alignstack" | alignstack ] ++ [ "intel" | dialect == Intel ]

-- | Print a string literal
printStr :: StrStyle -> String -> Doc a
printStr sty str = noAnnotate (printLit (Str str sty Unsuffixed ()))

-- | Print inline asm output clauses
printInlineAsmOutput :: InlineAsmOutput a -> Doc a
printInlineAsmOutput (InlineAsmOutput constraint expr isRw _) = printStr Cooked (prefix ++ constraint) <> parens (printExpr expr) 
  where prefix = case listToMaybe constraint of
                   Just '=' | isRw -> "+"
                   _ -> ""

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
expressionAttrs (Break as _ _ _) = as
expressionAttrs (Continue as _ _) = as
expressionAttrs (Ret as _ _) = as
expressionAttrs (InlineAsmExpr as _ _) = as
expressionAttrs (MacExpr as _ _) = as
expressionAttrs (Struct as _ _ _ _) = as
expressionAttrs (Repeat as _ _ _) = as
expressionAttrs (ParenExpr as _ _) = as
expressionAttrs (Try as _ _) = as

-- | Print a field
printField :: Field a -> Doc a
printField (Field ident expr x) = annotate x (printIdent ident <> ":" <+> printExpr expr)

-- | Print range limits
printRangeLimits :: RangeLimits -> Doc a
printRangeLimits HalfOpen = ".."
printRangeLimits Closed = "..."

-- | Print a closure function declaration (@print_fn_block_args@)
printFnBlockArgs :: FnDecl a -> Doc a
printFnBlockArgs (FnDecl args ret _ x) = annotate x ("|" <> args' <> "|" <+> ret')
  where ret' = perhaps (\ty -> "->" <+> printType ty) ret
        args' = commas args (`printArg` True)

-- | Print the arm of a match expression (@print_arm@)
printArm :: Bool -> Arm a -> Doc a
printArm end (Arm as pats guard body x) = annotate x (printOuterAttrs as
  <+> foldr1 (\a b -> a <+> "|" <+> b) (printPat <$> pats)
  <+> perhaps (\e -> "if" <+> printExpr e) guard
  <+> "=>"
  <+> case body of 
         BlockExpr _ blk _ -> printBlockUnclosed blk <> when (rules blk == Unsafe && end) ","
         _ -> printExpr body <> when end ",")

-- | Print a block
printBlock :: Block a -> Doc a
printBlock blk = printBlockWithAttrs blk []

-- | Print a block (@print_block_unclosed@ or @print_block_unclosed_indent@)
printBlockUnclosed :: Block a -> Doc a
printBlockUnclosed = printBlock -- TODO maybe?

-- | Print a block with attributes (@print_block_unclosed_with_attrs@)
printBlockUnclosedWithAttrs :: Block a -> [Attribute a] -> Doc a
printBlockUnclosedWithAttrs = printBlockWithAttrs -- TODO maybe?

-- | Print a block with attributes (@print_block_with_attrs@ or @print_block_maybe_unclosed@)
printBlockWithAttrs :: Block a -> [Attribute a] -> Doc a
printBlockWithAttrs (Block stmts rules x) as = annotate x (safety <+> blockAttrs (printInnerAttrs as) (body ++ [ lastStmt ]))
  where
  body = if null stmts then [] else printStmt `map` Prelude.init stmts
  
  lastStmt = unless (null stmts) (case last stmts of
                                    NoSemi expr _ -> printExprOuterAttrStyle expr False
                                    stmt -> printStmt stmt)

  safety = case rules of
             Normal -> mempty
             Unsafe -> "unsafe"

-- | Print an @else@ expression (@print_else@)
printElse :: Maybe (Expr a) -> Doc a
printElse Nothing = mempty
printElse (Just (If _ e t s x))      = annotate x (hsep [ "else if", printExpr e, printBlock t, printElse s ])
printElse (Just (IfLet _ p e t s x)) = annotate x (hsep [ "else if let", printPat p, "=", printExpr e, printBlock t, printElse s ])
printElse (Just (BlockExpr _ blk x)) = annotate x (hsep [ "else", printBlock blk ])
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

-- | Find the precedence of a binary operation (@util::parser::AssocOp::precedence@)
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

-- | Print a unary operator
printUnOp :: UnOp -> Doc a
printUnOp Deref = "*"
printUnOp Not = "!"
printUnOp Neg = "-" 

-- | Print a literal (@print_literal@)
printLit :: Lit a -> Doc a
printLit lit = case lit of
    (Str     str Cooked  s x) -> annotate x (hcat [ "\"", foldMap escapeChar str, "\"", suffix s ])
    (Str     str (Raw n) s x) -> annotate x (hcat [ "r", pad n, "\"", text str, "\"", pad n, suffix s ])
    (ByteStr str Cooked  s x) -> annotate x (hcat [ "b\"", foldMap escapeByte (unpack str), "\"", suffix s ])
    (ByteStr str (Raw n) s x) -> annotate x (hcat [ "br", pad n, "\"", text (map byte2Char (unpack str)), "\"", pad n, suffix s ])
    (Char c s x)              -> annotate x (hcat [ "'",  escapeChar c, "'", suffix s ])
    (Byte b s x)              -> annotate x (hcat [ "b'", escapeByte b, "'", suffix s ])
    (Int b i s x)             -> annotate x (hcat [ printIntLit i b, suffix s ])
    (Float d s x)             -> annotate x (hcat [ pretty d,  suffix s ])
    (Bool True s x)           -> annotate x (hcat [ "true",  suffix s ])
    (Bool False s x)          -> annotate x (hcat [ "false", suffix s ])
  where
  pad :: Int -> Doc a
  pad n = text (replicate n '#')

  suffix :: Suffix -> Doc a
  suffix = text . show
  
-- | Print an integer literal
printIntLit :: Integer -> IntRep -> Doc a
printIntLit i r | i < 0     = "-" <> baseRep r <> toNBase (abs i) (baseVal r)
                | i == 0    =        baseRep r <> "0"
                | otherwise =        baseRep r <> toNBase (abs i) (baseVal r)
  where
  baseRep :: IntRep -> Doc a
  baseRep Bin = "0b"
  baseRep Oct = "0o"
  baseRep Dec = mempty
  baseRep Hex = "0x"

  baseVal :: IntRep -> Integer
  baseVal Bin = 2
  baseVal Oct = 8
  baseVal Dec = 10
  baseVal Hex = 16

  toDigit :: Integer -> Char
  toDigit i = "0123456789ABCDEF" !! fromIntegral i

  toNBase :: Integer -> Integer -> Doc a
  i `toNBase` b | i < b = char (toDigit i)
                | otherwise = let ~(d,r) = i `quotRem` b in toNBase d b <> char (toDigit r)


-- | Extend a byte into a unicode character
byte2Char :: Word8 -> Char
byte2Char = chr . fromIntegral 
  
-- | Constrain a unicode character to a byte
-- This assumes the character is in the right range already
char2Byte :: Char -> Word8
char2Byte = fromIntegral . ord

-- | Escape a byte. Based on @std::ascii::escape_default@
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

-- | Escape a unicode character. Based on @std::ascii::escape_default@
escapeChar :: Char -> Doc a
escapeChar c | c <= '\xff'   = escapeByte (char2Byte c)
             | c <= '\xffff' = "\\u" <> padHex 4 (ord c)
             | otherwise     = "\\U" <> padHex 8 (ord c)
 
-- | Convert a number to its padded hexadecimal form
padHex :: Integral a => Int -> a -> Doc b
padHex n 0 = text (replicate n '0')
padHex n m = let (m',r) = m `divMod` 0x10
             in padHex (n-1) m' <> char (intToDigit (fromIntegral r))

-- | Print inner attributes (@print_inner_attributes@ or @print_inner_attributes_inline@
-- or @print_inner_attributes_no_trailing_hardbreak@ - distinction has to be made at callsite
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

-- | Print an attribute (@print_attribute_inline@ or @print_attribute@)
printAttr :: Attribute a -> Bool -> Doc a
printAttr a@(Attribute Inner value isSugaredDoc x) inline
  | isSugaredDoc && inline = annotate x ("/*!" <+> perhaps text (valueStr a) <+> "*/")
  | isSugaredDoc           = annotate x ("//!" <+> perhaps text (valueStr a))
  | otherwise              = annotate x ("#![" <> printMetaItem value <> "]")
printAttr a@(Attribute Outer value isSugaredDoc x) inline
  | isSugaredDoc && inline = annotate x ("/**" <+> perhaps text (valueStr a) <+> "*/")
  | isSugaredDoc           = annotate x ("///" <+> perhaps text (valueStr a))
  | otherwise              = annotate x ("#[" <> printMetaItem value <> "]")

-- | Try to extract a string from an attribute
valueStr :: Attribute a -> Maybe String
valueStr (Attribute _ (NameValue _  (Str s _ _ _) _) _ _) = Just s
valueStr _ = Nothing

-- | Print a nested meta item (@print_meta_list_item@)
printMetaListItem :: NestedMetaItem a -> Doc a
printMetaListItem (MetaItem item x) = annotate x (printMetaItem item)
printMetaListItem (Literal lit x) = annotate x (printLit lit)

-- | Print a meta item (@print_meta_item@)
printMetaItem :: MetaItem a -> Doc a
printMetaItem (Word name x) = annotate x (printIdent name)
printMetaItem (NameValue name lit x) = annotate x (printIdent name <+> "=" <+> printLit lit)
printMetaItem (List name items x) = annotate x (printIdent name <> "(" <> commas items printMetaListItem <> ")")

-- | Synthesizes a comment that was not textually present in the original source file (@synth_comment@)
synthComment :: String -> Doc a
synthComment com = "/*" <+> text com <+> "*/"

-- | Print an identifier as is, or as cooked string if containing a hyphen
printCookedIdent :: Ident -> Doc a
printCookedIdent ident@Ident{ name = str }
  | '-' `elem` str = printStr Cooked str
  | otherwise = printIdent ident 

-- | Print an item (@print_item@)
printItem :: Item a -> Doc a
printItem (Item ident attrs node vis x) = annotate x $ align $ printOuterAttrs attrs <#> case node of
  ExternCrate p     -> hsep [ printVis vis, "extern", "crate", perhaps (\p' -> printCookedIdent p' <+> "as") p, printIdent ident <> ";" ]
  Use vp            -> hsep [ printVis vis, "use", printViewPath vp <> ";" ]
  Static ty m e     -> hsep [ printVis vis, "static", printMutability m, printIdent ident <> ":", printType ty, "=", printExpr e <> ";" ]
  ConstItem t e     -> hsep [ printVis vis, "const", printIdent ident <> ":", printType t, "=", printExpr e <> ";" ]
  Fn d s c a t b    -> printFn d s c a (Just ident) t vis <+> printBlockWithAttrs b attrs
  Mod items         -> hsep [ printVis vis, "mod", printIdent ident, block (printMod items attrs) ]
  ForeignMod a i    -> hsep [ printAbi a, block (printForeignMod i attrs) ]
  TyAlias ty ps     -> hsep [ printVis vis, "type", printIdent ident <> printGenerics ps <> printWhereClause (whereClause ps), "=", printType ty <> ";" ]
  Enum vars ps      -> printEnumDef vars ps ident vis
  StructItem s g    -> hsep [ printVis vis, "struct", printStruct s g ident True True ]
  Union s g         -> hsep [ printVis vis, "union", printStruct s g ident True True ]
  DefaultImpl u t   -> hsep [ printVis vis, printUnsafety u, "impl", printTraitRef t, "for", "..", "{ }" ]
  Impl u p g t ty i -> let generics = case g of { Generics [] [] _ _ -> mempty; _ -> printGenerics g }
                           traitref = perhaps (\t' -> printPolarity p <> printTraitRef t' <+> "for") t
                       in hsep [ printVis vis, printUnsafety u, "impl", generics
                               , traitref <+> printType ty
                               , printWhereClause (whereClause g)
                               , block (vsep (printInnerAttrs attrs : (printImplItem `map` i)))
                               ]
  Trait u g tys i   -> let tys' = map (\t -> case t of
                                               TraitTyParamBound ptr Maybe -> Left ("for ?" <+> printTraitRef (traitRef ptr))
                                               _ -> Right t)
                                      tys
                       in hsep [ printVis vis, printUnsafety u, "trait", printIdent ident <> printGenerics g
                               , hsep (lefts tys'), printBounds ":" (rights tys'), printWhereClause (whereClause g)
                               , block (vsep (printTraitItem `map` i))
                               ]
  MacItem m         -> printMac m Paren <> ";" 


-- | Print a trait item (@print_trait_item@)
printTraitItem :: TraitItem a -> Doc a
printTraitItem (TraitItem ident attrs node x) = annotate x $ printOuterAttrs attrs <#>
  case node of
    ConstT ty default_m -> printAssociatedConst ident ty default_m InheritedV
    MethodT sig block_m -> printMethodSig ident sig InheritedV <+> maybe ";" (`printBlockWithAttrs` attrs) block_m
    TypeT bounds default_m -> printAssociatedType ident (Just bounds) default_m
    MacroT m -> printMac m Paren <> ";"

-- | Print type parameter bounds with the given prefix, but only if there are any bounds (@print_bounds@)
printBounds :: Doc a -> [TyParamBound a] -> Doc a
printBounds _ [] = mempty
printBounds prefix (b:bs) = align (fillSep ((prefix <+> printBound b) : [ "+" <+> printBound b' | b' <- bs ]))

-- | Print a type parameter bound
printBound :: TyParamBound a -> Doc a
printBound (RegionTyParamBound lt) = printLifetime lt
printBound (TraitTyParamBound tref modi) = when (modi == Maybe) "?" <> printPolyTraitRef tref

-- | Print the formal lifetime list (@print_formal_lifetime_list@)
printFormalLifetimeList :: [LifetimeDef a] -> Doc a
printFormalLifetimeList [] = mempty
printFormalLifetimeList defs = "for" <> angles (align (fillSep (punctuate "," (printDef `map` defs))))
  where printDef (LifetimeDef as lt bds x) = annotate x (printOuterAttrs as <+> flatten (printLifetimeBounds lt bds))

-- | Print an impl item (@print_impl_item@)
printImplItem :: ImplItem a -> Doc a
printImplItem (ImplItem ident vis defaultness attrs node x) = annotate x $ printOuterAttrs attrs <#> hsep
  [ when (defaultness == Default) "default"
  , case node of
      ConstI ty expr -> printAssociatedConst ident ty (Just expr) vis
      MethodI sig body -> printMethodSig ident sig vis <+> printBlockWithAttrs body attrs
      TypeI ty -> printAssociatedType ident Nothing (Just ty) 
      MacroI m -> printMac m Paren <> ";" 
  ]

-- | Print an associated type (@printAssociatedType@)
printAssociatedType :: Ident ->  Maybe [TyParamBound a] -> Maybe (Ty a) -> Doc a
printAssociatedType ident bounds_m ty_m = "type" <+> printIdent ident
  <+> perhaps (printBounds ":") bounds_m
  <+> perhaps (\ty -> "=" <+> printType ty) ty_m
  <> ";"

-- | Print a method signature (@print_method_sig@)
printMethodSig :: Ident -> MethodSig a -> Visibility a -> Doc a
printMethodSig ident (MethodSig unsafety constness abi decl generics)
  = printFn decl unsafety constness abi (Just ident) generics

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
printVis (RestrictedV path) = "pub(" <> printPath path False <> ")"
printVis InheritedV = mempty

-- | Print a foreign item (@print_foreign_item@)
printForeignItem :: ForeignItem a -> Doc a
printForeignItem (ForeignItem ident attrs node vis x) = annotate x $ printOuterAttrs attrs <+>
  case node of
    ForeignFn decl generics -> printFn decl Normal NotConst Rust (Just ident) generics vis <> ";"
    ForeignStatic ty mut -> printVis vis <+> "static" <+> when mut "mut" <+> printIdent ident <> ":" <+> printType ty <> ";"


-- | Print a struct definition (@print_struct@)
printStruct :: VariantData a -> Generics a -> Ident -> Bool -> Bool -> Doc a
printStruct structDef generics ident printFinalizer annotateGenerics =
  printIdent ident <> gen
    <> case structDef of 
          StructD fields x -> annotate x $ space <> wc <+> blockComma (printStructField `map` fields)
          TupleD fields x -> annotate x $ parens (commas fields printStructField) <+> wc <+> when printFinalizer ";" 
          UnitD x -> annotate x $ wc <+> when printFinalizer ";"
  where gen = if annotateGenerics then printGenerics generics else noAnnotate (printGenerics generics)
        wc = printWhereClause (whereClause generics)

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
  printVis vis <+> "enum" <+> (printIdent ident <> printGenerics generics)
    <+> printWhereClause (whereClause generics)
    <+> blockComma [ printOuterAttrs as <#> printVariant v  | v@Variant{ attrs = as } <- variants ]

-- | Print a variant (@print_variant@)
printVariant :: Variant a -> Doc a
printVariant (Variant i _ _data e x) = annotate x (body <+> disc)
  where body = printStruct _data (Generics [] [] (WhereClause [] undefined) undefined) i False False
        disc = perhaps (\e' -> "=" <+> printExpr e') e

-- | Print a where clause (@print_where_clause@)
printWhereClause :: WhereClause a -> Doc a
printWhereClause (WhereClause predicates x)
  | null predicates = mempty
  | otherwise = annotate x ("where" <+> commas predicates printPredicate)
  where
  printPredicate :: WherePredicate a -> Doc a
  printPredicate (BoundPredicate blt ty bds y) = annotate y (printFormalLifetimeList blt <+> printType ty <> printBounds ":" bds)
  printPredicate (RegionPredicate lt bds y) = annotate y (printLifetimeBounds lt bds)
  printPredicate (EqPredicate lhs rhs y) = annotate y (printType lhs <+> "=" <+> printType rhs)

-- TODO: think carefully about multiline version of this
-- | Print a function (@print_fn@)
printFn :: FnDecl a -> Unsafety -> Constness -> Abi -> Maybe Ident -> Generics a -> Visibility a -> Doc a
printFn decl unsafety constness abi name generics vis =
  printFnHeaderInfo unsafety constness abi vis
    <+> perhaps printIdent name
    <> printGenerics generics
    <> printFnArgsAndRet decl
    <+> printWhereClause (whereClause generics)

-- | Print the function arguments and the return type (@print_fn_args_and_ret@)
printFnArgsAndRet :: FnDecl a -> Doc a
printFnArgsAndRet (FnDecl args ret var x) = annotate x ("(" <> align (fillSep args') <> ")" </> ret')
  where ret' = perhaps (\t -> "->" <+> printType t) ret
        args' = if var then [ printArg a False <> "," | a <- args ] ++ [ "..." ]
                       else punctuate "," ((`printArg` False) `map` args)

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
printPat (StructP p fs b x)             = annotate x (printPath p True <+> blockComma body)
  where body = (printFieldPat `map` fs) ++ [ ".." | b ]
printPat (TupleStructP p es Nothing x)  = annotate x (printPath p True <> "(" <> commas es printPat <> ")")
printPat (TupleStructP p es (Just d) x) = let (before,after) = splitAt d es
  in annotate x (printPath p True <> "(" <> commas before printPat <> when (d /= 0) ","
                    <+> ".." <> when (d /= length es) ("," <+> commas after printPat) <> ")")
printPat (PathP Nothing path x)         = annotate x (printPath path True)
printPat (PathP (Just qself) path x)    = annotate x (printQPath path qself True)
printPat (TupleP elts Nothing x)        = annotate x ("(" <> commas elts printPat <> ")")
printPat (TupleP elts (Just ddpos) _) = let (before,after) = splitAt ddpos elts
  in "(" <> commas before printPat <> unless (null elts) ","
      <+> ".." <> when (ddpos /= length elts) ("," <+> commas after printPat) <> ")"
printPat (BoxP inner x)                 = annotate x ("box" <+> printPat inner)
printPat (RefP inner mutbl x)           = annotate x ("&" <> printMutability mutbl <+> printPat inner)
printPat (LitP expr x)                  = annotate x (printExpr expr)
printPat (RangeP lo hi x)               = annotate x (printExpr lo <+> "..." <+> printExpr hi)
printPat (SliceP pb Nothing pa x)       = annotate x ("[" <> commas (pb ++ pa) printPat <> "]")
printPat (SliceP pb (Just ps) pa x)     = annotate x ("[" <> commas pb printPat <> ps' <+> commas pa printPat <> "]")
  where ps' = hcat [ unless (null pb) ","
                   , space
                   , case ps of WildP{} -> mempty
                                _ -> printPat ps
                   , ".."
                   , unless (null pa) ","
                   ]
printPat (MacP m x)                     = annotate x (printMac m Paren)

-- | Print a field pattern
printFieldPat :: FieldPat a -> Doc a
printFieldPat (FieldPat i p x) = annotate x (perhaps (\i -> printIdent i <> ":") i <+> printPat p)

-- | Print a binding mode
printBindingMode :: BindingMode -> Doc a
printBindingMode (ByRef mutbl) = "ref" <+> printMutability mutbl
printBindingMode (ByValue Immutable) = mempty
printBindingMode (ByValue Mutable) = "mut"

-- | Print the prefix of a function - all the stuff up to and including @fn@ (@print_fn_header_info@)
printFnHeaderInfo :: Unsafety -> Constness -> Abi -> Visibility a -> Doc a
printFnHeaderInfo u c a v = hsep [ printVis v, case c of { Const -> "const"; _ -> mempty }
                                 , printUnsafety u, printAbi a, "fn" ]

-- | Print the ABI
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


-- | Print the interior of a module given the list of items and attributes in it (@print_mod@)
printMod :: [Item a] -> [Attribute a] -> Doc a
printMod items attrs = vsep (printInnerAttrs attrs : (printItem `map` items))

-- | Print the interior of a foreign module given the list of items and attributes in it (@print_mod@)
printForeignMod :: [ForeignItem a] -> [Attribute a] -> Doc a
printForeignMod items attrs = vsep (printInnerAttrs attrs : (printForeignItem `map` items))

-- | Print generics (@print_generics@)
-- Remark: we are discarding the where clause because it gets printed seperately from the rest of
-- the generic.
printGenerics :: Generics a -> Doc a
printGenerics (Generics lifetimes tyParams _ x)
  | null lifetimes && null tyParams = mempty
  | otherwise =  let lifetimes' = [ printOuterAttrs as <+> printLifetimeBounds lt bds | LifetimeDef as lt bds _ <- lifetimes ]
                     bounds' = [ printTyParam param | param<-tyParams ]
                 in annotate x ("<" <> hsep (punctuate "," (lifetimes' ++ bounds')) <> ">")

-- | Print a poly-trait ref (@print_poly_trait_ref@)
printPolyTraitRef :: PolyTraitRef a -> Doc a
printPolyTraitRef (PolyTraitRef lts tref x) = annotate x (printFormalLifetimeList lts <+> printTraitRef tref)

-- | Print a trait ref (@print_trait_ref@)
printTraitRef :: TraitRef a -> Doc a
printTraitRef (TraitRef path) = printPath path False

-- | Print a path parameter and signal whether its generic (if any) should start with a colon (@print_path_parameters@)
printPathParameters :: PathParameters a -> Bool -> Doc a
printPathParameters (NoParameters x) _ = annotate x mempty
printPathParameters (Parenthesized ins out x) colons = annotate x $
  when colons "::" <> parens (commas ins printType) <+> perhaps (\t -> "->" <+> printType t) out
printPathParameters (AngleBracketed lts tys bds x) colons = annotate x (when colons "::" <> "<" <> hsep (punctuate "," (lts' ++ tys' ++ bds')) <> ">")
  where
    lts' = printLifetime <$> lts
    tys' = printType <$> tys
    bds' = (\(ident,ty) -> printIdent ident <+> "=" <+> printType ty) <$> bds


-- | Print a path, specifiying explicitly whether to include colons (@::@) before generics
-- or not (so expression style or type style generics) (@print_path@)
printPath :: Path a -> Bool -> Doc a
printPath (Path global segs x) colons = annotate x (when global "::" <> hcat (punctuate "::" (printSegment `map` N.toList segs)))
  where
  printSegment :: (Ident, PathParameters a) -> Doc a
  printSegment (ident,parameters) = printIdent ident <> printPathParameters parameters colons

-- | Print a qualified path, specifiying explicitly whether to include colons (@::@) before
-- generics or not (so expression style or type style generics) (@print_qpath@)
printQPath :: Path a -> QSelf a -> Bool -> Doc a
printQPath (Path global segs x) (QSelf ty n) colons = hcat [ "<", printType ty <+> aliasedDoc, ">", "::", restDoc ]
  where
  (aliased, rest) = N.splitAt n segs
  
  aliasedDoc = case aliased of
                 [] -> mempty
                 (s:ss) -> "as" <+> printPath (Path global (s :| ss) x) False

  restDoc = printPath (Path False (N.fromList rest) x) colons

-- | Print a view path (@print_view_path@)
printViewPath :: ViewPath a -> Doc a
printViewPath (ViewPathSimple global segs end x) = annotate x (when global "::" <> hcat (punctuate "::" (map printIdent segs ++ [printPathListItem end])))
printViewPath (ViewPathGlob global segs x) = annotate x (when global "::" <> hcat (punctuate "::" (map printIdent segs ++ ["*"])))
printViewPath (ViewPathList global segs ends x) = annotate x (when global "::" <> hcat (punctuate "::" (map printIdent segs ++ [end])))
  where
  end = "{" <> hsep (punctuate "," (map printPathListItem ends)) <> "}"

-- | Print a path list item (the end of a view path)
printPathListItem :: PathListItem a -> Doc a
printPathListItem (PathListItem name (Just rename) _) = printIdent name <+> "as" <+> printIdent rename
printPathListItem (PathListItem name Nothing _) = printIdent name


-- | Print a type parameters (@print_ty_param@)
printTyParam :: TyParam a -> Doc a
printTyParam (TyParam as i bds def x) = annotate x $ hsep
  [ printOuterAttrs as
  , printIdent i <> printBounds ":" bds
  , perhaps (\def' -> "=" <+> printType def') def
  ]

-- | Print lifetime bounds (@print_lifetime_bounds@)
printLifetimeBounds :: Lifetime a -> [Lifetime a] -> Doc a
printLifetimeBounds lifetime bounds = printLifetime lifetime
  <> unless (null bounds) (":" <+> foldr1 (\x y -> x <+> "+" <+> y) (printLifetime <$> bounds))

