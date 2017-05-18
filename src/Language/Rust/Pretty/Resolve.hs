{-|
Module      : Language.Rust.Pretty.Resolve
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : portable

An AST and its text form /should/ be completely isomorphic, with @parse@ and @pretty@ being the
functions allowing you to go back and forth between these forms. Unfortunately, this cannot really
be the case. The AST form can express programs which cannot be literally pretty-printed and still
make sense. Sometimes, extra parens or semicolons need to be added.

== Simple example

For example, consider the following interaction

>>> import Language.Rust.Quote
>>> import Language.Rust.Pretty
>>> :set -XQuasiQuotes
>>> x = [expr| 2 + 3 |]
>>> y = [expr| 1 * $x |]
>>> pretty y
0 * 1 + 2

The problem is that we haven't introduced the paren AST node (which we would have gotten had we
parsed @1 * (2 + 3)@. This is where 'resolve' steps in.

>>> Right y' = resolve y
>>> pretty y'
0 * (1 + 2)

== More involved example

From the above, it is tempting to say: your pretty-printer should be smarter! However, things are
not always so simple. Consider the less obvious example:

>>> fnBody = [expr| { let y = x; x += 1; y } + x |]
>>> fn = [item| fn foo(mut x: i32) -> i32 { $fnBody } |]
>>> pretty fn
fn foo(mut x: i32) -> i32 {
  { let y = x; x += 1; y } + x
}

This is clearly not the desired output - this won't compile with @rustc@ because of an invariant in
blocks: if the block ends in an expression, that expression cannot start with a block. To fix this,
we call 'resolve' on the AST before pretty-printing it.

>>> Right fn' = resolve fn
>>> pretty fn'
fn foo(mut x: i32) -> i32 {
  ({ let y = x; x += 1; y }) + x
}

And now we have generated valid code.

-}

module Language.Rust.Pretty.Resolve (
  Resolve(..),
) where

import Language.Rust.Syntax

import Language.Rust.Parser.ParseMonad (execParser)
import Language.Rust.Parser.Lexer (lexTokens, lexToken)
import Language.Rust.Data.Position (initPos, Spanned(..))
import Language.Rust.Data.InputStream (inputStreamFromString)

import Data.Either (rights)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as N
import Data.Semigroup ((<>))

{-# ANN module "HLint: ignore Reduce duplication" #-}

-- | Types that can have underlying invariants which can be checked and possibly corrected. 
class Resolve a where
  -- | Convert some value to its /resolved/ form. Informally, resolving a value involves checking
  -- that its invariants hold and, if they don't, report an error message or dajust the value so
  -- that the invariant holds.
  --
  -- Formally, we can say that a value that is an instance of 'Parse' and 'Pretty' is resolved if
  -- @parse . pretty@ is an identity operation on it.
  --
  -- prop> parse . pretty . resolve == id
  --
  -- We further expect that 'resolve' be an identity operation on any output of @parse@.
  resolve :: a -> Either String a


-- | The shebang can be anything
resolveSourceFile :: Monoid a => SourceFile a -> Either String (SourceFile a)
resolveSourceFile (SourceFile sh as is) = do
  sh' <- case sh of
           Just ('[':_) -> Left "shebang cannot start with `['"
           Just s | '\n' `elem` s -> Left "shebang cannot contain newlines"
           _ -> pure sh
  as' <- sequence (resolveAttr InnerAttr <$> as)
  is' <- sequence (resolveItem ModItem <$> is)
  pure (SourceFile sh' as' is')

instance Monoid a => Resolve (SourceFile a) where resolve = resolveSourceFile

-- | An identifier can be invalid if
-- 
--   * it is empty
--   * it does not lex into an identifier
--   * it is a keyword
resolveIdent :: Ident -> Either String Ident
resolveIdent i
    | i `elem` keywords = Left ("identifier is a keyword `" ++ show i ++ "'")
    | otherwise = pure i
  where
  keywords = map mkIdent $ words "as box break const continue crate else enum extern false fn for\
                                \ if impl in let loop match mod move mut pub ref return Self self\
                                \ static struct super trait true type unsafe use where while\
                                \ abstract alignof become do final macro offsetof override priv\
                                \ proc pure sizeof typeof unsized virtual yield" 

-- | Like 'resolveIdent', but without requiring the identifier not be a keyword.
resolveIdent' :: Ident -> Either String Ident
resolveIdent' (Ident "" _) = Left "empty identifier"
resolveIdent' i@(Ident s _) = case toks of
                             Right [Spanned (IdentTok i') _]
                                | i /= i' -> Left "identifier does not lex properly"
                                | otherwise -> pure i
                             _ -> Left "identifier does not lex properly"
  where
  toks = execParser (lexTokens lexToken) (inputStreamFromString s) initPos

instance Resolve Ident where resolve = resolveIdent


----------------
-- Attributes --
----------------

data AttrType
  = EitherAttr -- ^ inner or outer attribute
  | InnerAttr  -- ^ only innner attribute
  | OuterAttr  -- ^ only outer attribute

-- | An attribute is invalid if it claims to be a doc comment but lacks the accompanying structure.
resolveAttr :: AttrType -> Attribute a -> Either String (Attribute a)
resolveAttr OuterAttr (Attribute Inner _ _ _) = Left "only outer attributes are allowed"
resolveAttr InnerAttr (Attribute Outer _ _ _) = Left "only inner attributes are allowed"
resolveAttr _         (Attribute sty met False x) = Attribute sty <$> resolveMetaItem met <*> pure False <*> pure x 
resolveAttr _       a@(Attribute _ (NameValue (Ident "doc" _) (Str _ Cooked Unsuffixed _) _) True _) = pure a
resolveAttr _          _ = Left "attribute cannot be doc comment"

instance Resolve (Attribute a) where resolve = resolveAttr EitherAttr

-- | A meta-item is invalid if a literal has a suffix. Note that identifiers here can even be
-- keywords.
resolveMetaItem :: MetaItem a -> Either String (MetaItem a)
resolveMetaItem (Word i x) = Word <$> resolveIdent' i <*> pure x
resolveMetaItem (List i vals x) = List <$> resolveIdent' i <*> sequence (resolveNestedMetaItem <$> vals) <*> pure x 
resolveMetaItem (NameValue i lit x)
  | suffix lit /= Unsuffixed = Left "literal in meta-item that is not unsuffixed"
  | otherwise = NameValue <$> resolveIdent' i <*> resolveLit lit <*> pure x

instance Resolve (MetaItem a) where resolve = resolveMetaItem

-- | A nested-meta-item is invalid if a literal has a suffix. Note that identifiers here can even
-- be keywords.
resolveNestedMetaItem :: NestedMetaItem a -> Either String (NestedMetaItem a)
resolveNestedMetaItem (MetaItem met x) = MetaItem <$> resolveMetaItem met <*> pure x
resolveNestedMetaItem (Literal lit x)
  | suffix lit /= Unsuffixed = Left "literal in nested-meta-item that is not unsuffixed"
  | otherwise = Literal <$> resolveLit lit <*> pure x

instance Resolve (NestedMetaItem a) where resolve = resolveNestedMetaItem


--------------
-- Literals --
--------------

-- | A literal cannot be invalid
resolveLit :: Lit a -> Either String (Lit a)
resolveLit = pure

instance Resolve (Lit a) where resolve = resolveLit


-----------
-- Paths --
-----------

-- | Recall that paths are reused for expressions, modules, and types. However, these paths have
-- different underlying invariants.
data PathType
  = ModPath
  | TypePath
  | ExprPath
  deriving (Eq)

instance Show PathType where
  show ModPath = "mod path"
  show TypePath = "type path"
  show ExprPath = "expression path"

-- | A path can be invalid if
--
--   * it has path parameters of the wrong type
--   * it has identifiers not meant for paths
resolvePath :: Monoid a => PathType -> Path a -> Either String (Path a)
resolvePath t (Path g segs x)
    | all (isParamsForPath t . snd) segs = Path g <$> sequence (resolveSeg t <$> segs) <*> pure x 
    | otherwise = Left (show t ++ " contains invalid path segment")
  where
  resolveSeg :: Monoid a => PathType -> (Ident, PathParameters a) -> Either String (Ident, PathParameters a)
  resolveSeg ModPath s@(Ident "self" _, NoParameters _) = pure s
  resolveSeg ModPath s@(Ident "super" _, NoParameters _) = pure s
  resolveSeg ExprPath s@(Ident "super" _, NoParameters _) = pure s
  resolveSeg ExprPath s@(Ident "self" _, NoParameters _) = pure s
  resolveSeg TypePath s@(Ident "Self" _, NoParameters _) = pure s
  resolveSeg _ (i, p) = (,) <$> resolveIdent i <*> resolvePathParameters p

  isParamsForPath :: PathType -> PathParameters a -> Bool
  isParamsForPath t' NoParameters{}   = t' `elem` [TypePath, ExprPath, ModPath]
  isParamsForPath t' AngleBracketed{} = t' `elem` [TypePath, ExprPath]
  isParamsForPath t' Parenthesized{}  = t' `elem` [TypePath]

-- | There are three potential instances for resolving a path (depending on what type it is). As a
-- compromise the 'Resolve' instance for 'Path' will let through any path.
instance Monoid a => Resolve (Path a) where
  resolve x = case rights [ resolvePath t x | t <- [ ModPath, TypePath, ExprPath ] ] of
                p : _ -> pure p
                [] -> Left "path is not a valid mod/type/expression path"

-- | A path parameter can be invalid if any of its constituent components are invalid
resolvePathParameters :: Monoid a => PathParameters a -> Either String (PathParameters a)
resolvePathParameters (NoParameters x) = pure (NoParameters x)
resolvePathParameters (AngleBracketed lts tys bds x) = do
  lts' <- sequence (resolveLifetime <$> lts)
  tys' <- sequence (resolveTy AnyType <$> tys)
  bds' <- sequence ((\(i,t) -> (,) <$> resolveIdent i <*> resolveTy NoSumType t) <$> bds)
  pure (AngleBracketed lts' tys' bds' x)
resolvePathParameters (Parenthesized tys tym x) = do
  tys' <- sequence (resolveTy AnyType <$> tys) 
  tym' <- sequence (resolveTy NoSumType <$> tym)
  pure (Parenthesized tys' tym' x)

instance Monoid a => Resolve (PathParameters a) where resolve = resolvePathParameters

-- | A QSelf by itself is only invalid when the underlying type is
resolveQSelf :: Monoid a => QSelf a -> Either String (QSelf a)
resolveQSelf (QSelf t p) = QSelf <$> resolveTy AnyType t <*> pure p

instance Monoid a => Resolve (QSelf a) where resolve = resolveQSelf


-----------
-- Types --
-----------

-- | A lifetime can only be invalid if the underlying identifier is. Note that lifetimes cannot use
-- keywords.
resolveLifetime :: Lifetime a -> Either String (Lifetime a)
resolveLifetime l@(Lifetime n _)
  | n == "static" = pure l 
  | otherwise = resolveIdent (mkIdent n) *> pure l

instance Resolve (Lifetime a) where resolve = resolveLifetime

-- | A trait ref is invalid if the underlying type path is.
resolveTraitRef :: Monoid a => TraitRef a -> Either String (TraitRef a)
resolveTraitRef (TraitRef p) = TraitRef <$> resolvePath TypePath p

instance Monoid a => Resolve (TraitRef a) where resolve = resolveTraitRef

-- | There a a variety of constraints imposed on types, representing different invariants
data TyType
  = AnyType        -- ^ No restrictions
  | NoSumType      -- ^ Any type except for 'TraitObject'
  | PrimParenType  -- ^ Types not starting with '<' or '(', or paren types with no sum types inside
  | NoForType      -- ^ Non-sum types not starting with a 'for'
  | ReturnType     -- ^ Type in a return type position

-- | Resolve a given type, and a constraint on it (see the parser 'Internal.y' for more details on
-- these cases). 
resolveTy :: Monoid a => TyType -> Ty a -> Either String (Ty a)
-- TraitObject
resolveTy NoSumType    o@TraitObject{} = resolveTy NoSumType (ParenTy o mempty)
resolveTy NoForType    o@TraitObject{} = resolveTy NoForType (ParenTy o mempty)
resolveTy ReturnType   o@TraitObject{} = resolveTy ReturnType (ParenTy o mempty)
resolveTy _             (TraitObject bds@(TraitTyParamBound{} :| _) x)
  = TraitObject <$> sequence (resolveTyParamBound ModBound <$> bds) <*> pure x
resolveTy _              TraitObject{} = Left "first bound in trait object should be a trait bound"
-- ParenTy
resolveTy PrimParenType (ParenTy ty' x) = ParenTy <$> resolveTy NoSumType ty' <*> pure x
resolveTy _             (ParenTy ty' x) = ParenTy <$> resolveTy AnyType ty' <*> pure x
-- TupTy
resolveTy PrimParenType t@TupTy{} = resolveTy PrimParenType (ParenTy t mempty)
resolveTy _             (TupTy tys x) = TupTy <$> sequence (resolveTy AnyType <$> tys) <*> pure x
-- ImplTrait
resolveTy ReturnType    (ImplTrait bds x) = ImplTrait <$> sequence (resolveTyParamBound ModBound <$> bds) <*> pure x 
resolveTy _              ImplTrait{} = Left "impl trait type is only allowed as return type"
-- PathTy
resolveTy PrimParenType p@(PathTy (Just _) _ _) = resolveTy PrimParenType (ParenTy p mempty)
resolveTy _             (PathTy q p@(Path _ s _) x)
  = case q of
      Just (QSelf _ i)
        | 0 <= i && i < length s -> PathTy <$> sequence (resolveQSelf <$> q) <*> resolvePath TypePath p <*> pure x
        | otherwise              -> Left "index given by QSelf is outside the possible range"
      Nothing                    -> PathTy Nothing <$> resolvePath TypePath p <*> pure x 
-- BareFn
resolveTy NoForType   f@(BareFn _ _ (_:_) _ _) = resolveTy NoForType (ParenTy f mempty)
resolveTy _             (BareFn u a lts fd x) = BareFn u a <$> sequence (resolveLifetimeDef <$> lts) <*> resolveFnDecl declTy fd <*> pure x
  where declTy = if a == C then VarNoSelf else NoSelf
-- Other types (don't care about the context)
resolveTy _ (Never x) = pure (Never x)
resolveTy _ (Ptr mut ty' x) = Ptr mut <$> resolveTy NoSumType ty' <*> pure x
resolveTy _ (Rptr lt mut ty' x) = Rptr <$> sequence (resolveLifetime <$> lt) <*> pure mut <*> resolveTy NoSumType ty' <*> pure x
resolveTy _ (Typeof e x) = Typeof <$> resolveExpr AnyExpr e <*> pure x
resolveTy _ (Infer x) = pure (Infer x)
resolveTy _ (Slice ty' x) = Slice <$> resolveTy AnyType ty' <*> pure x 
resolveTy _ (Array ty' e x) = Array <$> resolveTy AnyType ty' <*> resolveExpr AnyExpr e <*> pure x
resolveTy _ (MacTy (Mac p t x) x') = do
  p' <- resolvePath TypePath p
  MacTy <$> resolveMac TypePath (Mac p' t x) <*> pure x'

instance Monoid a => Resolve (Ty a) where resolve = resolveTy AnyType

-- In some cases, the first argument of a function declaration may be a 'self'
data FnDeclType
  = NoSelf     -- ^ the first argument cannot be self
  | VarNoSelf  -- ^ the first argument cannot be self, and the function can be variadic
  | AllowSelf  -- ^ the first argument can be self
  deriving (Eq)

-- | A function declaration can be invalid if it has self arguments in the wrong places (or when it
-- shouldn't)
resolveFnDecl :: Monoid a => FnDeclType -> FnDecl a -> Either String (FnDecl a)
resolveFnDecl NoSelf (FnDecl (s : _) _ _ _)  | isSelfArg s = Left "self argument is not allowed in this function declaration"
resolveFnDecl _      (FnDecl (_ : as) _ _ _) | any isSelfArg as = Left "self arguments must always be the first arguments"
resolveFnDecl fn     (FnDecl _ _ True _)     | fn /= VarNoSelf = Left "this function declaration cannot be variadic"
resolveFnDecl _      (FnDecl as o v x) = FnDecl <$> sequence (resolveArg <$> as) <*> sequence (resolveTy ReturnType <$> o) <*> pure v <*> pure x

-- | Check whether an argument is one of the "self" forms
isSelfArg :: Arg a -> Bool
isSelfArg Arg{} = False
isSelfArg _ = True

instance Monoid a => Resolve (FnDecl a) where resolve = resolveFnDecl AllowSelf

-- | Only some type parameter bounds allow trait bounds to start with ?
data TyParamBoundType
  = NoneBound          -- ^ Don't allow '? poly_trait_ref'
  | ModBound           -- ^ Allow '? poly_trait_ref'

-- | A type parameter bound is invalid if
--
--   * an underlying lifetime or traitref is
--   * it is 'NoneBound' but is a trait bound with a '?' (as in 'ObjectTrait')
resolveTyParamBound :: Monoid a => TyParamBoundType -> TyParamBound a -> Either String (TyParamBound a)
resolveTyParamBound _ (RegionTyParamBound lt x) = RegionTyParamBound <$> resolveLifetime lt <*> pure x
resolveTyParamBound NoneBound (TraitTyParamBound _ Maybe _) = Left "? trait is not allowed in this type param bound"
resolveTyParamBound _ (TraitTyParamBound p t x) = TraitTyParamBound <$> resolvePolyTraitRef p <*> pure t <*> pure x 

instance Monoid a => Resolve (TyParamBound a) where resolve = resolveTyParamBound ModBound

-- | The only types of patterns supported by arguments are wild or identifiers
resolveArg :: Monoid a => Arg a -> Either String (Arg a)
resolveArg s@SelfValue{} = pure s
resolveArg (SelfRegion lt m x) = SelfRegion <$> sequence (resolveLifetime <$> lt) <*> pure m <*> pure x
resolveArg (SelfExplicit t m x) = SelfExplicit <$> resolveTy AnyType t <*> pure m <*> pure x
resolveArg (Arg Nothing t x) =  Arg Nothing <$> resolveTy AnyType t <*> pure x
resolveArg (Arg (Just p) t x)
  = case p of
      (WildP _)                      -> Arg (Just p) <$> resolveTy AnyType t <*> pure x
      (IdentP ByValue{} _ Nothing _) -> Arg <$> (Just <$> resolvePat p) <*> resolveTy AnyType t <*> pure x
      _                              -> Left "not all patterns are accepted in arguments"

instance Monoid a => Resolve (Arg a) where resolve = resolveArg

-- | A Poly trait ref is valid whenever the underlying trait ref is.
resolvePolyTraitRef :: Monoid a => PolyTraitRef a -> Either String (PolyTraitRef a)
resolvePolyTraitRef (PolyTraitRef lts t x) = PolyTraitRef <$> sequence (resolveLifetimeDef <$> lts) <*> resolveTraitRef t <*> pure x

instance Monoid a => Resolve (PolyTraitRef a) where resolve = resolvePolyTraitRef

-- | A lifetime def is invalid if it has non-outer attributes 
resolveLifetimeDef :: LifetimeDef a -> Either String (LifetimeDef a)
resolveLifetimeDef (LifetimeDef as l bds x) = do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  l' <- resolveLifetime l
  bds' <- sequence (resolveLifetime <$> bds)
  pure (LifetimeDef as' l' bds' x)

instance Resolve (LifetimeDef a) where resolve = resolveLifetimeDef


--------------
-- Patterns --
--------------

-- | A pattern can be invalid of
--
--   * the index of the '...' in the tuple/tuple-struct is out of range
--   * the index of the qself path is out of range
--   * any underlying component is invalid
--
resolvePat :: Monoid a => Pat a -> Either String (Pat a)
-- TupleStruct
resolvePat (TupleStructP p fs Nothing x) = TupleStructP <$> resolvePath ExprPath p <*> sequence (resolvePat <$> fs) <*> pure Nothing <*> pure x
resolvePat (TupleStructP p fs (Just i) x)
  | 0 <= i && i <= length fs = TupleStructP <$> resolvePath ExprPath p <*> sequence (resolvePat <$> fs) <*> pure (Just i) <*> pure x
  | otherwise = Left "index of ... in tuple struct pattern is outside of field range"
-- PathP
resolvePat (PathP Nothing p x) = PathP Nothing <$> resolvePath ExprPath p <*> pure x 
resolvePat (PathP q@(Just (QSelf _ i)) p@(Path g s x) x')
  | i < 0 || i >= length s = Left "index given by QSelf is outside the possible range"
  | i == 0 = PathP <$> sequence (resolveQSelf <$> q) <*> resolvePath ExprPath p <*> pure x
  | otherwise = do
      tyP <-   resolvePath TypePath $ Path g     (N.fromList (N.take i s)) mempty
      exprP <- resolvePath ExprPath $ Path False (N.fromList (N.drop i s)) x
      q' <- sequence (resolveQSelf <$> q)
      pure (PathP q' (Path g (segments tyP <> segments exprP) x) x')
-- TupleP
resolvePat (TupleP ps Nothing x) = TupleP <$> sequence (resolvePat <$> ps) <*> pure Nothing <*> pure x
resolvePat (TupleP ps (Just i) x)
  | 0 <= i && i <= length ps = TupleP <$> sequence (resolvePat <$> ps) <*> pure (Just i) <*> pure x
  | otherwise = Left "index of ... in tuple pattern is outside of range"
-- Everything else...
resolvePat (LitP e x) = LitP <$> resolveExpr LitExpr e <*> pure x
resolvePat (RangeP l h x) = RangeP <$> resolveExpr LitOrPathExpr l <*> resolveExpr LitOrPathExpr h <*> pure x
resolvePat (WildP x) = pure (WildP x)
resolvePat (IdentP m i p x) = IdentP m <$> resolveIdent i <*> sequence (resolvePat <$> p) <*> pure x
resolvePat (StructP p fs b x) = StructP <$> resolvePath ExprPath p <*> sequence (resolveFieldPat <$> fs) <*> pure b <*> pure x
resolvePat (BoxP p x) = BoxP <$> resolvePat p <*> pure x
resolvePat (RefP p m x) = RefP <$> resolvePat p <*> pure m <*> pure x
resolvePat (SliceP b m a x) = SliceP <$> sequence (resolvePat <$> b) <*> sequence (resolvePat <$> m) <*> sequence (resolvePat <$> a) <*> pure x
resolvePat (MacP m x) = MacP <$> resolveMac ExprPath m <*> pure x

instance Monoid a => Resolve (Pat a) where resolve = resolvePat

-- | Field patterns are only invalid if the underlying pattern / identifier is
resolveFieldPat :: Monoid a => FieldPat a -> Either String (FieldPat a)
resolveFieldPat (FieldPat Nothing p x)
  = case p of (IdentP _ _ Nothing _)          -> FieldPat Nothing <$> resolvePat p <*> pure x
              (BoxP (IdentP _ _ Nothing _) _) -> FieldPat Nothing <$> resolvePat p <*> pure x
              _                               -> Left "patterns for fields without an identifier must be (possibly box) identifiers"
resolveFieldPat (FieldPat i p x) = FieldPat <$> sequence (resolveIdent <$> i) <*> resolvePat p <*> pure x

instance Monoid a => Resolve (FieldPat a) where resolve = resolveFieldPat


-----------------
-- Expressions --
-----------------

-- Invariants on expressions
data ExprType
  = AnyExpr           -- ^ Any expression, no restrictions
  | LitExpr           -- ^ Either an immediate literal, or a negated literal
  | LitOrPathExpr     -- ^ A literal, negated literal, expression path, or qualified expression path
  | NoStructExpr      -- ^ No struct literals are allowed
  | NoStructBlockExpr -- ^ No struct literals or block expressions (block-like things like 'if' are fine)
  | NonBlockExpr      -- ^ Forbids expressions starting with blocks (things like '{ 1 } + 2')

-- | Given the type of expression, what type of expression is allowed on the LHS
lhs :: ExprType -> ExprType
lhs LitExpr = error "literal expressions never have a left hand side"
lhs LitOrPathExpr = error "literal or path expressions never have a left hand side"
lhs AnyExpr = AnyExpr
lhs NoStructExpr = NoStructExpr
lhs NoStructBlockExpr = NoStructBlockExpr
lhs NonBlockExpr = NonBlockExpr

-- | Given the type of expression, what type of expression is allowed on the RHS
rhs :: ExprType -> ExprType
rhs LitExpr = error "literal expressions never have a right hand side"
rhs LitOrPathExpr = error "literal or path expressions never have a right hand side"
rhs AnyExpr = AnyExpr
rhs NoStructExpr = NoStructExpr
rhs NoStructBlockExpr = NoStructExpr
rhs NonBlockExpr = AnyExpr

-- | Given the type of expression, what type of expression is allowed on the RHS (after '..'/'...')
rhs2 :: ExprType -> ExprType
rhs2 LitExpr = error "literal expressions never have a right hand side (2)"
rhs2 LitOrPathExpr = error "literal or path expressions never have a right hand side (2)"
rhs2 AnyExpr = AnyExpr
rhs2 NoStructExpr = NoStructBlockExpr
rhs2 NoStructBlockExpr = NoStructBlockExpr
rhs2 NonBlockExpr = AnyExpr

-- | Resolve an expression of the given type in a general context
resolveExpr :: Monoid a => ExprType -> Expr a -> Either String (Expr a)
resolveExpr = resolveExprP 0

instance Monoid a => Resolve (Expr a) where resolve = resolveExpr AnyExpr

parenthesize :: Monoid a => Expr a -> Either String (Expr a)
parenthesize e = do
  e' <- resolveExprP 0 AnyExpr e
  pure (ParenExpr [] e' mempty)

{-
Precedences (from 'Internal.y')
===============================

0   %nonassoc box return break continue LAMBDA
1   %right '=' '>>=' '<<=' '-=' '+=' '*=' '/=' '^=' '|=' '&=' '%='
2   %right '<-'
    %nonassoc SINGLERNG                 --    '..'
3   %nonassoc INFIXRNG                  -- e1 '..' e2
4   %nonassoc POSTFIXRNG                -- e1 '..'
5   %nonassoc PREFIXRNG                 --    '..' e2
6   %left '||'
7   %left '&&'
8   %left '==' '!=' '<' '>' '<=' '>='
9   %left '|'
10  %left '^'
11  %left '&'
12  %left '<<' '>>'
13  %left '+' '-'
14  %left '*' '/' '%'
15  %left ':' as
16  %nonassoc UNARY                     -- 'UNARY' is introduced here for '*', '!', '-', '&'
17  %nonassoc POSTFIX                   -- 'POSTFIX' is introduced here for things like '?'
-}

-- | This has a double role: to resolve the expression and keep track of precedences
resolveExprP :: Monoid a => Int -> ExprType -> Expr a -> Either String (Expr a)
-- Cover the 'LitExpr' type of expression
resolveExprP p LitExpr l@Lit{} = resolveExprP p AnyExpr l
resolveExprP p LitExpr n@(Unary _ Neg Lit{} _) = resolveExprP p AnyExpr n
resolveExprP _ LitExpr _ = Left "expression is not literal or negated literal"
-- Cover the 'LitOrPathExpr' type of expression
resolveExprP p LitOrPathExpr l@Lit{} = resolveExprP p AnyExpr l
resolveExprP p LitOrPathExpr n@(Unary _ Neg Lit{} _) = resolveExprP p AnyExpr n
resolveExprP p LitOrPathExpr p'@PathExpr{} = resolveExprP p AnyExpr p'
resolveExprP _ LitOrPathExpr _ = Left "expression is not literal, negated literal, path, or qualified path"
-- The following group of expression variants work in all of the remaining contexts (see
-- 'gen_expression' in the parser)
resolveExprP p c (Box as e x) = parenE (p > 0) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 0 (rhs c) e
  pure (Box as' e' x)
resolveExprP p c (Ret as me x) = parenE (p > 0) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  me' <- sequence (resolveExprP 0 (rhs c) <$> me)
  pure (Ret as' me' x)
resolveExprP p c (Break as ml me x) = parenE (p > 0) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  ml' <- sequence (resolveLifetime <$> ml)
  me' <- sequence (resolveExprP 0 (rhs c) <$> me)
  pure (Break as' ml' me' x) 
resolveExprP p _ (Continue as ml x) = parenE (p > 0) $ do 
  as' <- sequence (resolveAttr OuterAttr <$> as)
  ml' <- sequence (resolveLifetime <$> ml)
  pure (Continue as' ml' x)
-- Closures
resolveExprP _ _ (Closure _ _ (FnDecl _ _ True _) _ _) = Left "closures can never be variadic"
resolveExprP p c e@(Closure as cb fn@(FnDecl _ ret _ _) b x)
    = case (c, ret, b) of
      (NoStructExpr,      Just _, BlockExpr{}) -> parenthesize e
      (NoStructBlockExpr, Just _, BlockExpr{}) -> parenthesize e
      (NoStructExpr,      Just _, _          ) -> parenthesize (Closure as cb fn (asBlock b) x) 
      (NoStructBlockExpr, Just _, _          ) -> parenthesize (Closure as cb fn (asBlock b) x)
      (_,                 Just _, BlockExpr{}) -> resolved AnyExpr
      (_,                 Just _, _          ) -> parenthesize (Closure as cb fn (asBlock b) x)
      _                                         -> resolved (rhs c)
  where
  asBlock ex = BlockExpr [] (Block [NoSemi ex mempty] Normal mempty) mempty

  resolved c' = parenE (p > 0) $ do
    as' <- sequence (resolveAttr OuterAttr <$> as)
    fn' <- resolveFnDecl NoSelf fn
    b' <- resolveExprP 0 c' b
    pure (Closure as' cb fn' b' x)
-- Assignment/in-place expressions
resolveExprP p c (Assign as l r x) = parenE (p > 1) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  l' <- resolveExprP 2 (lhs c) l
  r' <- resolveExprP 1 (rhs c) r
  pure (Assign as' l' r' x)
resolveExprP p c (AssignOp as o l r x) = parenE (p > 1) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  l' <- resolveExprP 2 (lhs c) l
  r' <- resolveExprP 1 (rhs c) r
  pure (AssignOp as' o l' r' x)
resolveExprP p c (InPlace as l r x) = parenE (p > 2) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  l' <- resolveExprP 3 (lhs c) l 
  r' <- resolveExprP 2 (rhs c) r 
  pure (InPlace as' l' r' x)
-- Range expressions
resolveExprP _ _ (Range _ _ Nothing Closed _) = Left "inclusive ranges must be bounded at the end"
resolveExprP _ _ (Range as Nothing Nothing rl x) = do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  pure (Range as' Nothing Nothing rl x)
resolveExprP p c (Range as (Just l) (Just r) rl x) = parenE (p > 3) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  l' <- resolveExprP 4 (lhs c) l
  r' <- resolveExprP 4 (rhs2 c) r
  pure (Range as' (Just l') (Just r') rl x)
resolveExprP p c (Range as (Just l) Nothing rl x) = parenE (p > 4) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  l' <- resolveExprP 4 (lhs c) l
  pure (Range as' (Just l') Nothing rl x)
resolveExprP p c (Range as Nothing (Just r) rl x) = parenE (p > 5) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  r' <- resolveExprP 5 (rhs2 c) r
  pure (Range as' Nothing (Just r') rl x)
-- Binary expressions
resolveExprP p c (Binary as o l r x) = parenE (p > p') $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  l' <- resolveExprP p' (lhs c) l 
  r' <- resolveExprP (p' + 1) (rhs c) r 
  pure (Binary as' o l' r' x)
  where
  p' = opPrec o

  opPrec :: BinOp -> Int
  opPrec AddOp = 13
  opPrec SubOp = 13
  opPrec MulOp = 14
  opPrec DivOp = 14
  opPrec RemOp = 14
  opPrec AndOp = 7
  opPrec OrOp = 6
  opPrec BitXorOp = 10
  opPrec BitAndOp = 11
  opPrec BitOrOp = 9
  opPrec ShlOp = 12
  opPrec ShrOp = 12
  opPrec EqOp = 8
  opPrec LtOp = 8
  opPrec LeOp = 8
  opPrec NeOp = 8
  opPrec GeOp = 8
  opPrec GtOp = 8
-- Cast and type ascriptions expressions
resolveExprP p c (Cast as e t x) = parenE (p > 15) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 15 (lhs c) e
  t' <- resolveTy NoSumType t
  pure (Cast as' e' t' x)
resolveExprP p c (TypeAscription as e t x) = parenE (p > 15) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 15 (lhs c) e
  t' <- resolveTy NoSumType t
  pure (TypeAscription as' e' t' x)
-- Unary expressions
resolveExprP p c (Unary as o e x) = parenE (p > 16) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 16 (rhs c) e
  pure (Unary as' o e' x)
resolveExprP p c (AddrOf as m e x) = parenE (p > 16) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 16 (rhs c) e
  pure (AddrOf as' m e' x)
-- Postfix expressions
resolveExprP p c (Index as e i x) = parenE (p > 17) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 17 (lhs c) e
  i' <- resolveExprP 0 AnyExpr i
  pure (Index as' e' i' x)
resolveExprP p c (Try as e x) = parenE (p > 17) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 17 (lhs c) e
  pure (Try as' e' x) 
resolveExprP p c (Call as f xs x) = parenE (p > 17) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  f' <- resolveExprP 17 (lhs c) f
  xs' <- sequence (resolveExprP 0 AnyExpr <$> xs)
  pure (Call as' f' xs' x)
resolveExprP p c (MethodCall as e i mt es x) = parenE (p > 17) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 17 (lhs c) e
  i' <- resolveIdent i
  mt' <- case mt of
           Just t -> Just <$> sequence (resolveTy AnyType <$> t)
           Nothing -> pure Nothing
  es' <- sequence (resolveExprP 0 AnyExpr <$> es)
  pure (MethodCall as' e' i' mt' es' x)
resolveExprP p c (TupField as e i x) = parenE (p > 17) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 17 (lhs c) e
  pure (TupField as' e' i x)
resolveExprP p c (FieldAccess as e i x) = parenE (p > 17) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 17 (lhs c) e
  i' <- resolveIdent i
  pure (FieldAccess as' e' i' x)
-- Immediate expressions
resolveExprP _ _ (Vec as es x) = do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  es' <- sequence (resolveExprP 0 AnyExpr <$> es)
  pure (Vec as' es' x) 
resolveExprP _ _ (PathExpr as Nothing p' x) = do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  p'' <- resolvePath ExprPath p'
  pure (PathExpr as' Nothing p'' x)
resolveExprP _ _ (PathExpr as q@(Just (QSelf _ i)) p'@(Path g s x) x')
  | i < 0 || i >= length s = Left "index given by QSelf is outside the possible range"
  | i == 0 = do
      as' <- sequence (resolveAttr OuterAttr <$> as)
      q' <- sequence (resolveQSelf <$> q)
      p'' <- resolvePath ExprPath p'
      pure (PathExpr as' q' p'' x)
  | otherwise = do
      as' <- sequence (resolveAttr OuterAttr <$> as)
      tyP <-   resolvePath TypePath $ Path g     (N.fromList (N.take i s)) mempty
      exprP <- resolvePath ExprPath $ Path False (N.fromList (N.drop i s)) x
      q' <- sequence (resolveQSelf <$> q)
      pure (PathExpr as' q' (Path g (segments tyP <> segments exprP) x) x') 
resolveExprP _ _ (Lit as l x) = do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  l' <- resolveLit l
  pure (Lit as' l' x)
resolveExprP _ _ (Repeat as e r x) = do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 0 AnyExpr e
  r' <- resolveExprP 0 AnyExpr r
  pure (Repeat as' e' r' x)
-- Macro expressions
resolveExprP _ _ (MacExpr as m x) = do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  m' <- resolveMac ExprPath m
  pure (MacExpr as' m' x)
resolveExprP _ _ (InlineAsmExpr as i x) = do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  i' <- resolveInlineAsm i
  pure (InlineAsmExpr as' i' x)
-- Paren expressions
resolveExprP _ _ (ParenExpr as e x) = do
  as' <- sequence (resolveAttr EitherAttr <$> as)
  e' <- resolveExprP 0 AnyExpr e
  pure (ParenExpr as' e' x)
resolveExprP _ _ (TupExpr as es x) = do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  es' <- sequence (resolveExprP 0 AnyExpr <$> es)
  pure (TupExpr as' es' x)
-- Block expressions
resolveExprP _ NoStructBlockExpr e@BlockExpr{} = parenthesize e
resolveExprP _ NonBlockExpr e@BlockExpr{} = parenthesize e
resolveExprP _ _ (BlockExpr as b x) = do
  as' <- sequence (resolveAttr EitherAttr <$> as)
  b' <- resolveBlock b
  pure (BlockExpr as' b' x) 
-- Struct expressions
resolveExprP _ NoStructExpr e@Struct{} = parenthesize e
resolveExprP _ NoStructBlockExpr e@Struct{} = parenthesize e
resolveExprP _ _ (Struct as p' fs e x) = do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  p'' <- resolvePath ExprPath p'
  fs' <- sequence (resolveField <$> fs)
  e' <- sequence (resolveExprP 0 AnyExpr <$> e)
  pure (Struct as' p'' fs' e' x)
-- Block-like expressions
resolveExprP _ NonBlockExpr e@If{} = parenthesize e
resolveExprP p c (If as e b es x) = do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 0 NoStructExpr e
  b' <- resolveBlock b
  es' <- case es of
           Nothing -> pure Nothing
           (Just If{}) -> sequence (resolveExprP p c <$> es)
           (Just IfLet{}) -> sequence (resolveExprP p c <$> es)
           (Just BlockExpr{}) -> sequence (resolveExprP p c <$> es)
           (Just e'') -> Just <$> resolveExprP p c (BlockExpr [] (Block [NoSemi e'' mempty] Normal mempty) mempty)
  pure (If as' e' b' es' x)
resolveExprP _ NonBlockExpr e@IfLet{} = parenthesize e
resolveExprP p c (IfLet as p' e b es x) = do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  p'' <- resolvePat p'
  e' <- resolveExprP 0 NoStructExpr e
  b' <- resolveBlock b
  es' <- case es of
           Nothing -> pure Nothing
           (Just If{}) -> sequence (resolveExprP p c <$> es)
           (Just IfLet{}) -> sequence (resolveExprP p c <$> es)
           (Just BlockExpr{}) -> sequence (resolveExprP p c <$> es)
           (Just e'') -> Just <$> resolveExprP p c (BlockExpr [] (Block [NoSemi e'' mempty] Normal mempty) mempty)
  pure (IfLet as' p'' e' b' es' x)
resolveExprP _ NonBlockExpr e@While{} = parenthesize e
resolveExprP _ _ (While as e b l x) = do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 0 NoStructExpr e
  b' <- resolveBlock b
  l' <- sequence (resolveLifetime <$> l)
  pure (While as' e' b' l' x)
resolveExprP _ NonBlockExpr e@WhileLet{} = parenthesize e
resolveExprP _ _ (WhileLet as p' e b l x) = do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  p'' <- resolvePat p'
  e' <- resolveExprP 0 NoStructExpr e
  b' <- resolveBlock b
  l' <- sequence (resolveLifetime <$> l)
  pure (WhileLet as' p'' e' b' l' x)
resolveExprP _ NonBlockExpr e@ForLoop{} = parenthesize e
resolveExprP _ _ (ForLoop as p' e b l x) = do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  p'' <- resolvePat p'
  e' <- resolveExprP 0 NoStructExpr e
  b' <- resolveBlock b
  l' <- sequence (resolveLifetime <$> l)
  pure (ForLoop as' p'' e' b' l' x)
resolveExprP _ NonBlockExpr e@Loop{} = parenthesize e
resolveExprP _ _ (Loop as b l x) = do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  b' <- resolveBlock b
  l' <- sequence (resolveLifetime <$> l)
  pure (Loop as' b' l' x)
resolveExprP _ NonBlockExpr e@Match{} = parenthesize e
resolveExprP _ _ (Match as e ar x) = do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 0 NoStructExpr e
  ar' <- sequence (resolveArm <$> ar)
  pure (Match as' e' ar' x)
resolveExprP _ NonBlockExpr e@Catch{} = parenthesize e
resolveExprP _ _ (Catch as b x) = do
  as' <- sequence (resolveAttr EitherAttr <$> as)
  b' <- resolveBlock b
  pure (Catch as' b' x) 

isBlockLike :: Expr a -> Bool
isBlockLike If{} = True
isBlockLike IfLet{} = True
isBlockLike Loop{} = True
isBlockLike ForLoop{} = True
isBlockLike While{} = True
isBlockLike WhileLet{} = True
isBlockLike Match{} = True
isBlockLike BlockExpr{} = True
isBlockLike _ = False

-- | Wrap an expression in parens if the condition given holds
parenE :: Monoid a => Bool -> Either String (Expr a) -> Either String (Expr a)
parenE True e = ParenExpr [] <$> e <*> pure mempty
parenE False e = e

-- | A field just requires the identifier and expression to be valid
resolveField :: Monoid a => Field a -> Either String (Field a)
resolveField (Field i e x) = do
  i' <- resolveIdent i
  e' <- sequence (resolveExpr AnyExpr <$> e)
  pure (Field i' e' x)

instance Monoid a => Resolve (Field a) where resolve = resolveField

-- | Arms are invalid only if the underlying consitutents are
resolveArm :: Monoid a => Arm a -> Either String (Arm a)
resolveArm (Arm as ps g b x) = do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  ps' <- sequence (resolvePat <$> ps)
  g' <- sequence (resolveExpr AnyExpr <$> g)
  b' <- resolveExpr AnyExpr b
  pure (Arm as' ps' g' b' x)

instance Monoid a => Resolve (Arm a) where resolve = resolveArm


----------------
-- Statements --
----------------

-- Invariants on statements
data StmtType
  = TermStmt -- ^ require a statement be terminated (so another statement can follow it)?
  | AnyStmt  -- ^ any statement

-- | Statements are invalid only when the underlying components are.
resolveStmt :: Monoid a => StmtType -> Stmt a -> Either String (Stmt a)
resolveStmt _ (Local p t i as x) = do
  p' <- resolvePat p
  t' <- sequence (resolveTy AnyType <$> t)
  i' <- sequence (resolveExpr AnyExpr <$> i)
  as' <- sequence (resolveAttr OuterAttr <$> as)
  pure (Local p' t' i' as' x)
resolveStmt _ (ItemStmt i x) = ItemStmt <$> resolveItem StmtItem i <*> pure x
resolveStmt _ (Semi e x) = Semi <$> resolveExpr NonBlockExpr e <*> pure x
resolveStmt _ (NoSemi e x) | isBlockLike e = NoSemi <$> resolveExpr AnyExpr e <*> pure x
resolveStmt AnyStmt  (NoSemi e x) = NoSemi <$> resolveExpr NonBlockExpr e <*> pure x
resolveStmt TermStmt (NoSemi e x) = NoSemi <$> resolveExpr AnyExpr (BlockExpr [] (Block [NoSemi e mempty] Normal mempty) mempty) <*> pure x
resolveStmt _ (MacStmt m s as x) = do
  m' <- resolveMac ExprPath m
  as' <- sequence (resolveAttr OuterAttr <$> as)
  pure (MacStmt m' s as' x)

instance Monoid a => Resolve (Stmt a) where resolve = resolveStmt AnyStmt

-- | A block must a a series of terminated statements ended by one possibly unterminated one
resolveBlock :: Monoid a => Block a -> Either String (Block a)
resolveBlock b@(Block [] _ _) = pure b
resolveBlock (Block (s:ss) r x) = do
  ss' <- sequence (resolveStmt TermStmt <$> N.init (s :| ss))
  s' <- resolveStmt AnyStmt (N.last (s :| ss))
  pure (Block (ss' ++ [s']) r x)

instance Monoid a => Resolve (Block a) where resolve = resolveBlock


-----------
-- Items --
-----------

-- Whether the item is a statement item, or a general item
data ItemType
  = StmtItem   -- ^ Corresponds to 'stmt_item' - basically limited visbility and no macros
  | ModItem    -- ^ General item

-- | An item can be invalid if
--
--   * it is a macro but has 'StmtItem' restriction
--   * it has visibility other than public/inherited but has 'StmtItem' restriction
--   * an underlying component is invalid
--
resolveItem :: Monoid a => ItemType -> Item a -> Either String (Item a)
resolveItem t (Item i as n v x) = do
  i' <- case (i,n) of
          (Ident "" _, MacItem{})     -> pure i 
          (Ident "" _, Use{})         -> pure i
          (Ident "" _, ForeignMod{})  -> pure i
          (Ident "" _, Impl{})        -> pure i
          (Ident "" _, DefaultImpl{}) -> pure i
          _ -> resolveIdent i
  as' <- sequence (resolveAttr OuterAttr <$> as)
  n' <- case (t,n) of
          (StmtItem, MacItem{}) -> Left "macro items cannot be in statement items"
          _ -> resolveItemKind n
  v' <- case (t,v) of
          (StmtItem, PublicV)    -> pure PublicV
          (StmtItem, InheritedV) -> pure InheritedV
          (StmtItem, _)          -> Left "statement items can only have public or inherited visibility"
          _ -> resolveVisibility v
  pure (Item i' as' n' v' x)

instance Monoid a => Resolve (Item a) where resolve = resolveItem ModItem

-- | An item kind is invalid only if any of its underlying constituents are 
resolveItemKind :: Monoid a => ItemKind a -> Either String (ItemKind a)
resolveItemKind (Static t m e) = Static <$> resolveTy AnyType t <*> pure m <*> resolveExpr AnyExpr e
resolveItemKind (ConstItem t e) = ConstItem <$> resolveTy AnyType t <*> resolveExpr AnyExpr e
resolveItemKind (TyAlias t g) = TyAlias <$> resolveTy AnyType t <*> resolveGenerics g
resolveItemKind (ExternCrate i) = ExternCrate <$> sequence (resolveIdent <$> i)
resolveItemKind (Fn f u c a g b) = Fn <$> resolveFnDecl NoSelf f <*> pure u <*> pure c <*> pure a <*> resolveGenerics g <*> resolveBlock b
resolveItemKind (Mod is) = Mod <$> sequence (resolveItem ModItem <$> is) 
resolveItemKind (ForeignMod a fs) = ForeignMod a <$> sequence (resolveForeignItem <$> fs)
resolveItemKind (StructItem v g) = StructItem <$> resolveVariantData v <*> resolveGenerics g 
resolveItemKind (Union v g) = Union <$> resolveVariantData v <*> resolveGenerics g
resolveItemKind (Enum vs g) = Enum <$> sequence (resolveVariant <$> vs) <*> resolveGenerics g
resolveItemKind (Impl u p g mt t is) = do
  g' <- resolveGenerics g
  mt' <- sequence (resolveTraitRef <$> mt)
  t' <- resolveTy PrimParenType t
  is' <- sequence (resolveImplItem <$> is)
  pure (Impl u p g' mt' t' is')
resolveItemKind (DefaultImpl u t) = DefaultImpl u <$> resolveTraitRef t
resolveItemKind (Trait u g bds is) = Trait u <$> resolveGenerics g <*> sequence (resolveTyParamBound NoneBound <$> bds) <*> sequence (resolveTraitItem <$> is)
resolveItemKind (MacItem m) = MacItem <$> resolveMac ExprPath m
resolveItemKind (Use v) = Use <$> resolveViewPath v

instance Monoid a => Resolve (ItemKind a) where resolve = resolveItemKind

-- | A foreign item is invalid only if any of its underlying constituents are 
resolveForeignItem :: Monoid a => ForeignItem a -> Either String (ForeignItem a)
resolveForeignItem (ForeignItem i as n v x) = do
  i' <- resolveIdent i
  as' <- sequence (resolveAttr OuterAttr <$> as)
  n' <- resolveForeignItemKind n
  v' <- resolveVisibility v
  pure (ForeignItem i' as' n' v' x)

instance Monoid a => Resolve (ForeignItem a) where resolve = resolveForeignItem

-- | A foreign item kind is invalid only if any of its underlying constituents are 
resolveForeignItemKind :: Monoid a => ForeignItemKind a -> Either String (ForeignItemKind a)
resolveForeignItemKind (ForeignFn fn g) = ForeignFn <$> resolveFnDecl NoSelf fn <*> resolveGenerics g
resolveForeignItemKind (ForeignStatic t b) = ForeignStatic <$> resolveTy AnyType t <*> pure b

instance Monoid a => Resolve (ForeignItemKind a) where resolve = resolveForeignItemKind

-- | A where clause is valid only if the underlying predicates are
resolveWhereClause :: Monoid a => WhereClause a -> Either String (WhereClause a)
resolveWhereClause (WhereClause p x) = WhereClause <$> sequence (resolveWherePredicate <$> p) <*> pure x

instance Monoid a => Resolve (WhereClause a) where resolve = resolveWhereClause

-- | Generics are only invalid if the underlying lifetimes or type parameters are
resolveGenerics :: Monoid a => Generics a -> Either String (Generics a)
resolveGenerics (Generics lts typ wc x) = do
  lts' <- sequence (resolveLifetimeDef <$> lts)
  typ' <- sequence (resolveTyParam <$> typ)
  wc' <- resolveWhereClause wc
  pure (Generics lts' typ' wc' x)

instance Monoid a => Resolve (Generics a) where resolve = resolveGenerics

-- | A type parameter is invalid only when the underlying components are
resolveTyParam :: Monoid a => TyParam a -> Either String (TyParam a)
resolveTyParam (TyParam as i bds t x) = do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  i' <- resolveIdent i
  bds' <- sequence (resolveTyParamBound ModBound <$> bds)
  t' <- sequence (resolveTy AnyType <$> t)
  pure (TyParam as' i' bds' t' x)

instance Monoid a => Resolve (TyParam a) where resolve = resolveTyParam

-- Invariants for struct fields
data StructFieldType
  = IdentStructField  -- ^ struct style field
  | BareStructField   -- ^ tuple-struct style field

-- | A variant is valid if the underlying components are
resolveVariant :: Monoid a => Variant a -> Either String (Variant a)
resolveVariant (Variant i as n e x) = do
  i' <- resolveIdent i
  as' <- sequence (resolveAttr OuterAttr <$> as)
  n' <- resolveVariantData n
  e' <- sequence (resolveExpr AnyExpr <$> e)
  pure (Variant i' as' n' e' x)

instance Monoid a => Resolve (Variant a) where resolve = resolveVariant

-- | A variant data is valid if the underlying components are
resolveVariantData :: Monoid a => VariantData a -> Either String (VariantData a)
resolveVariantData (StructD fs x) = StructD <$> sequence (resolveStructField IdentStructField <$> fs) <*> pure x
resolveVariantData (TupleD fs x) = TupleD <$> sequence (resolveStructField BareStructField <$> fs) <*> pure x
resolveVariantData (UnitD x) = pure (UnitD x)

instance Monoid a => Resolve (VariantData a) where resolve = resolveVariantData

-- | A struct field is invalid if
--
--   * it has the invariant that it needs an identifier, but it doesn't have one
--   * it has the invariant that should not have an identifier, but it doe have one
--   * any of the underlying components are invalid
--
resolveStructField :: Monoid a => StructFieldType -> StructField a -> Either String (StructField a)
resolveStructField IdentStructField (StructField Nothing _ _ _ _) = Left "struct field needs an identifier"
resolveStructField IdentStructField (StructField (Just i) v t as x) = do
  i' <- resolveIdent i
  v' <- resolveVisibility v
  t' <- resolveTy AnyType t
  as' <- sequence (resolveAttr OuterAttr <$> as)
  pure (StructField (Just i') v' t' as' x)
resolveStructField BareStructField (StructField (Just _) _ _ _ _) = Left "tuple-struct field cannot have an identifier"
resolveStructField BareStructField (StructField Nothing v t as x) = do
  v' <- resolveVisibility v
  t' <- resolveTy AnyType t
  as' <- sequence (resolveAttr OuterAttr <$> as)
  pure (StructField Nothing v' t' as' x)

instance Monoid a => Resolve (StructField a) where resolve = resolveStructField IdentStructField

-- | A where predicate is invalid only if the underlying lifetimes are
resolveWherePredicate :: Monoid a => WherePredicate a -> Either String (WherePredicate a)
resolveWherePredicate (EqPredicate t1 t2 x) = EqPredicate <$> resolveTy NoForType t1 <*> resolveTy AnyType t2 <*> pure x
resolveWherePredicate (RegionPredicate l ls x) = do
  l' <- resolveLifetime l
  ls' <- sequence (resolveLifetime <$> ls)
  pure (RegionPredicate l' ls' x)
resolveWherePredicate (BoundPredicate lts t bds x) = do
  lts' <- sequence (resolveLifetimeDef <$> lts)
  t' <- resolveTy NoForType t
  bds' <- sequence (resolveTyParamBound ModBound <$> bds)
  pure (BoundPredicate lts' t' bds' x)

instance Monoid a => Resolve (WherePredicate a) where resolve = resolveWherePredicate

-- | A trait item is valid if the underlying components are
resolveTraitItem :: Monoid a => TraitItem a -> Either String (TraitItem a)
resolveTraitItem (TraitItem i as n x) = do
  (i',n') <- case (i,n) of
               (Ident "" _, MacroT{}) -> (,) i <$> resolveTraitItemKind n
               _ -> (,) <$> resolveIdent i <*> resolveTraitItemKind n
  as' <- sequence (resolveAttr OuterAttr <$> as)
  pure (TraitItem i' as' n' x)

instance Monoid a => Resolve (TraitItem a) where resolve = resolveTraitItem

-- | A trait item kind is valid if the underlying components are
resolveTraitItemKind :: Monoid a => TraitItemKind a -> Either String (TraitItemKind a)
resolveTraitItemKind (ConstT t e) = ConstT <$> resolveTy AnyType t <*> sequence (resolveExpr AnyExpr <$> e)
resolveTraitItemKind (MethodT m b) = MethodT <$> resolveMethodSig m <*> sequence (resolveBlock <$> b)
resolveTraitItemKind (TypeT bds t) = TypeT <$> sequence (resolveTyParamBound ModBound <$> bds) <*> sequence (resolveTy AnyType <$> t)
resolveTraitItemKind (MacroT m) = MacroT <$> resolveMac ModPath m

instance Monoid a => Resolve (TraitItemKind a) where resolve = resolveTraitItemKind

-- | An impl item is valid if the underlying components are
resolveImplItem :: Monoid a => ImplItem a -> Either String (ImplItem a)
resolveImplItem (ImplItem i v d as n x) = do
  (i',n') <- case (i,n) of
               (Ident "" _, MacroI{}) -> (,) i <$> resolveImplItemKind n
               _ -> (,) <$> resolveIdent i <*> resolveImplItemKind n
  v' <- resolveVisibility v
  as' <- sequence (resolveAttr OuterAttr <$> as)
  pure (ImplItem i' v' d as' n' x)

instance Monoid a => Resolve (ImplItem a) where resolve = resolveImplItem

-- | An impl item kind is valid if the underlying components are
resolveImplItemKind :: Monoid a => ImplItemKind a -> Either String (ImplItemKind a)
resolveImplItemKind (ConstI t e) = ConstI <$> resolveTy AnyType t <*> resolveExpr AnyExpr e
resolveImplItemKind (MethodI m b) = MethodI <$> resolveMethodSig m <*> resolveBlock b
resolveImplItemKind (TypeI t) = TypeI <$> resolveTy AnyType t
resolveImplItemKind (MacroI m) = MacroI <$> resolveMac ModPath m

instance Monoid a => Resolve (ImplItemKind a) where resolve = resolveImplItemKind

-- | The 'Monoid' constraint is theoretically not necessary - restricted visibility paths are mod paths,
-- so they should never have generics.
resolveVisibility :: Monoid a => Visibility a -> Either String (Visibility a)
resolveVisibility PublicV = pure PublicV
resolveVisibility InheritedV = pure InheritedV
resolveVisibility CrateV = pure CrateV
resolveVisibility (RestrictedV p) = RestrictedV <$> resolvePath ModPath p

instance Monoid a => Resolve (Visibility a) where resolve = resolveVisibility

-- | A method signature is valid if the underlying components are
resolveMethodSig :: Monoid a => MethodSig a -> Either String (MethodSig a)
resolveMethodSig (MethodSig u c a f g) = MethodSig u c a <$> resolveFnDecl AllowSelf f <*> resolveGenerics g

instance Monoid a => Resolve (MethodSig a) where resolve = resolveMethodSig

-- | A view path is valid if the underlying components are
resolveViewPath :: ViewPath a -> Either String (ViewPath a)
resolveViewPath (ViewPathSimple b is p x) = do
  is' <- sequence (resolveSelfSuperIdent <$> is)
  p' <- resolvePathListItem p 
  pure (ViewPathSimple b is' p' x)
resolveViewPath (ViewPathGlob b is x) = do
  is' <- sequence (resolveSelfSuperIdent <$> is)
  pure (ViewPathGlob b is' x) 
resolveViewPath (ViewPathList b is ps x) = do
  is' <- sequence (resolveSelfSuperIdent <$> is)
  ps' <- sequence (resolvePathListItem <$> ps)
  pure (ViewPathList b is' ps' x)

instance Resolve (ViewPath a) where resolve = resolveViewPath

-- | A path list item is valid if the underlying components are
resolvePathListItem :: PathListItem a -> Either String (PathListItem a)
resolvePathListItem (PathListItem n r x) = do
  n' <- resolveSelfSuperIdent n
  r' <- sequence (resolveIdent <$> r)
  pure (PathListItem n' r' x)

instance Resolve (PathListItem a) where resolve = resolvePathListItem

-- | Accept valid identifiers, as well as identifiers that are 'super' or 'self'
resolveSelfSuperIdent :: Ident -> Either String Ident
resolveSelfSuperIdent i@(Ident "self" _) = pure i
resolveSelfSuperIdent i@(Ident "super" _) = pure i
resolveSelfSuperIdent i = resolveIdent i


-------------------
-- Macro related --
-------------------

-- | A macro call is only invalid if any of the underlying components are
resolveMac :: Monoid a => PathType -> Mac a -> Either String (Mac a)
resolveMac t (Mac p tt x) = Mac <$> resolvePath t p <*> sequence (resolveTt <$> tt) <*> pure x

instance Monoid a => Resolve (Mac a) where
  resolve (Mac p tt x) = Mac <$> resolve p <*> sequence (resolveTt <$> tt) <*> pure x 

-- | A token tree is invalid when
--
--   * there is an open or close delim token (those should be balanced and in 'Delimited')
--   * the underlying token trees are invalid
--
resolveTt :: TokenTree -> Either String TokenTree
resolveTt (Token _ (OpenDelim _)) = Left "open delimiter is not allowed as a token in a token tree"
resolveTt (Token _ (CloseDelim _)) = Left "close delimiter is not allowed as a token in a token tree"
resolveTt t@Token{} = pure t
resolveTt (Delimited s d s' tt s'') = Delimited s d s' <$> sequence (resolveTt <$> tt) <*> pure s''

instance Resolve TokenTree where resolve = resolveTt

resolveInlineAsm :: InlineAsm a -> Either String (InlineAsm a)
resolveInlineAsm = pure
