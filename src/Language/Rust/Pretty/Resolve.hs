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
-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Rust.Pretty.Resolve where

import Language.Rust.Syntax.AST
import Language.Rust.Syntax.Token

import Language.Rust.Parser
import Language.Rust.Syntax.Ident
import Language.Rust.Data.Position

import  Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as N
import Data.Semigroup ((<>))
import Data.Either (rights)

class Resolve a where
  -- | Convert some value to its /resolved/ form. Formally, we can say that a value that is an
  -- instance of 'Parse' and 'Pretty' is resolved if @parse . pretty@ is an identity operation on it.
  --
  -- This function also has the role of validating its input. If the input cannot be resolved, an
  -- error message is returned.
  --
  -- prop> parse . pretty . resolve == id
  resolve :: a -> Either String a

  -- | Strips all of the non-essential tree information. In a sense, one can think of 'strip' as an
  -- inverse of 'resolve': it removes elements that 'resolve' adds (such as parens)
  --
  -- prop> strip . resolve == strip
  strip :: a -> a


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
  keywords = [ "as", "box", "break", "const", "continue", "crate", "else", "enum", "extern"
             , "false", "fn", "for", "if", "impl", "in", "let", "loop", "match", "mod", "move"
             , "mut", "pub", "ref", "return", "Self", "self", "static", "struct", "super", "trait"
             , "true", "type", "unsafe", "use", "where", "while", "abstract", "alignof" , "become"
             , "do", "final", "macro", "offsetof", "override", "priv", "proc", "pure", "sizeof"
             , "typeof", "unsized" , "virtual" , "yield" 
             ]

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

instance Resolve Ident where
  resolve = resolveIdent
  strip = id


----------------
-- Attributes --
----------------

data AttrType
  = EitherAttr -- ^ inner or outer attribute
  | InnerAttr  -- ^ only innner attribute
  | OuterAttr  -- ^ only outer attribute

-- | An attribute is invalid if
--
--   * the underlying meta item is invalid
--   * it claims to be a doc comment but lacks the accompanying structure
resolveAttr :: AttrType -> Attribute a -> Either String (Attribute a)
resolveAttr OuterAttr (Attribute Inner _ _ _) = Left "only outer attributes are allowed"
resolveAttr InnerAttr (Attribute Outer _ _ _) = Left "only inner attributes are allowed"
resolveAttr _         (Attribute sty met False x) = Attribute sty <$> resolveMetaItem met <*> pure False <*> pure x 
resolveAttr _       a@(Attribute _ (NameValue (Ident "doc" _) (Str _ Cooked Unsuffixed _) _) True _) = pure a
resolveAttr _          _ = Left "attribute cannot be doc comment"

instance Resolve (Attribute a) where
  resolve = resolveAttr EitherAttr
  strip = id

-- | A meta-item is invalid if
--
--   * any of its constituents are (Note however that identifiers here can even be keywords here)
--   * a literal has a suffix
resolveMetaItem :: MetaItem a -> Either String (MetaItem a)
resolveMetaItem (Word i x) = Word <$> resolveIdent' i <*> pure x
resolveMetaItem (List i vals x) = List <$> resolveIdent' i <*> sequence (resolveNestedMetaItem <$> vals) <*> pure x 
resolveMetaItem (NameValue i lit x)
  | suffix lit /= Unsuffixed = Left "literal in meta-item that is not unsuffixed"
  | otherwise = NameValue <$> resolveIdent' i <*> resolveLit lit <*> pure x

instance Resolve (MetaItem a) where
  resolve = resolveMetaItem
  strip = id

-- | A nested-meta-item is invalid if
--
--   * any of its constituents are (Note however that identifiers here can even be keywords here)
--   * a literal has a suffix
resolveNestedMetaItem :: NestedMetaItem a -> Either String (NestedMetaItem a)
resolveNestedMetaItem (MetaItem met x) = MetaItem <$> resolveMetaItem met <*> pure x
resolveNestedMetaItem (Literal lit x)
  | suffix lit /= Unsuffixed = Left "literal in nested-meta-item that is not unsuffixed"
  | otherwise = Literal <$> resolveLit lit <*> pure x

instance Resolve (NestedMetaItem a) where
  resolve = resolveNestedMetaItem
  strip = id


--------------
-- Literals --
--------------

-- | A literal cannot be invalid
resolveLit :: Lit a -> Either String (Lit a)
resolveLit = pure

instance Resolve (Lit a) where
  resolve = resolveLit
  strip = id


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
--   * it has invalid components
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
  strip = id

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

instance Monoid a => Resolve (PathParameters a) where
  resolve = resolvePathParameters
  strip = id

-- | A QSelf by itself is only invalid when the underlying type is
resolveQSelf :: Monoid a => QSelf a -> Either String (QSelf a)
resolveQSelf (QSelf t p) = QSelf <$> resolveTy AnyType t <*> pure p

instance Monoid a => Resolve (QSelf a) where
  resolve = resolveQSelf
  strip = id


-----------
-- Types --
-----------

-- | A lifetime can only be invalid if the underlying identifier is. Note that lifetimes cannot use
-- keywords.
resolveLifetime :: Lifetime a -> Either String (Lifetime a)
resolveLifetime l@(Lifetime n _) = resolveIdent (mkIdent n) *> pure l

instance Resolve (Lifetime a) where
  resolve = resolveLifetime
  strip = id

-- | A trait ref is invalid if the underlying type path is.
resolveTraitRef :: Monoid a => TraitRef a -> Either String (TraitRef a)
resolveTraitRef (TraitRef p) = TraitRef <$> resolvePath TypePath p

instance Monoid a => Resolve (TraitRef a) where
  resolve = resolveTraitRef
  strip = id

-- | There a a variety of constraints imposed on types, representing different invariants
data TyType
  = AnyType        -- ^ No restrictions
  | NoSumType      -- ^ Any type except for 'TraitObject'
  | PrimType       -- ^ Types not starting with '(' or '<'
  | NoSumPrimType  -- ^ Non-sum types not starting with '(' or '<'
  | NoForType      -- ^ Non-sum types not starting with a 'for'
  | ReturnType     -- ^ Type in a return type position

-- TODO: document this - just summarize what the different cases are doing (and the motivation)
resolveTy :: Monoid a => TyType -> Ty a -> Either String (Ty a)
-- TraitObject
resolveTy NoSumType    o@TraitObject{} = resolveTy NoSumType (ParenTy o mempty)
resolveTy NoForType    o@TraitObject{} = resolveTy NoForType (ParenTy o mempty)
resolveTy ReturnType   o@TraitObject{} = resolveTy ReturnType (ParenTy o mempty)
resolveTy NoSumPrimType  TraitObject{} = Left "object sum is not allowed here"
resolveTy _             (TraitObject bds@(TraitTyParamBound{} :| _) x)
  = TraitObject <$> sequence (resolveTyParamBound ModBound <$> bds) <*> pure x
resolveTy _              TraitObject{} = Left "first bound in trait object should be a trait bound"
-- ParenTy
resolveTy PrimType       ParenTy{} = Left "paren type is not allowed in primitive type" 
resolveTy NoSumPrimType  ParenTy{} = Left "paren type is not allowed in primitive type" 
resolveTy _             (ParenTy ty' x) = ParenTy <$> resolveTy AnyType ty' <*> pure x
-- TupTy
resolveTy NoSumPrimType  TupTy{} = Left "paren type is not allowed in primitive type"
resolveTy PrimType       TupTy{} = Left "paren type is not allowed in primitive type"
resolveTy _             (TupTy tys x) = TupTy <$> sequence (resolveTy AnyType <$> tys) <*> pure x
-- ImplTrait
resolveTy ReturnType    (ImplTrait bds x) = ImplTrait <$> sequence (resolveTyParamBound ModBound <$> bds) <*> pure x 
resolveTy _              ImplTrait{} = Left "impl trait type is only allowed as return type"
-- PathTy
resolveTy PrimType      (PathTy (Just _) _ _) = Left "qualified path is no allowed in primitive type"
resolveTy NoSumPrimType (PathTy (Just _) _ _) = Left "qualified path is no allowed in primitive type"
resolveTy _             (PathTy q p@(Path _ s _) x)
  = case q of
      Just (QSelf _ i)
        | 0 <= i && i < length s -> PathTy <$> sequence (resolveQSelf <$> q) <*> resolvePath TypePath p <*> pure x
        | otherwise              -> Left "index given by QSelf is outside the possible range"
      Nothing                    -> PathTy Nothing <$> resolvePath TypePath p <*> pure x 
-- BareFn
resolveTy NoForType   f@(BareFn _ _ (_:_) _ _) = resolveTy NoForType (ParenTy f mempty)
resolveTy _             (BareFn u a lts fd x) = BareFn u a <$> sequence (resolveLifetimeDef <$> lts) <*> resolveFnDecl NoSelf fd <*> pure x
-- Other types (don't care about the context)
resolveTy _ (Never x) = pure (Never x)
resolveTy _ (Ptr mut ty' x) = Ptr mut <$> resolveTy NoSumType ty' <*> pure x
resolveTy _ (Rptr lt mut ty' x) = Rptr <$> sequence (resolveLifetime <$> lt) <*> pure mut <*> resolveTy NoSumType ty' <*> pure x
resolveTy _ (Typeof e x) = Typeof <$> resolveExpr AnyExpr e <*> pure x
resolveTy _ (Infer x) = pure (Infer x)
resolveTy _ (Slice ty' x) = Slice <$> resolveTy AnyType ty' <*> pure x 
resolveTy _ (Array ty' e x) = Array <$> resolveTy AnyType ty' <*> resolveExpr AnyExpr e <*> pure x -- TODO: e is only a Lit?
resolveTy _ (MacTy (Mac p t x) x') = do
  p' <- resolvePath TypePath p
  MacTy <$> resolveMac (Mac p' t x) <*> pure x'

instance Monoid a => Resolve (Ty a) where
  resolve = resolveTy AnyType
  strip = id

-- In some cases, the first argument of a function declaration may be a self
-- TODO add check around allowing variadic
data FnDeclType
  = NoSelf     -- ^ the first argument cannot be self
  | AllowSelf  -- ^ the first argument can be self

-- | A function declaration can be invalid if it has self arguments in the wrong places (or when it
-- shouldn't)
resolveFnDecl :: Monoid a => FnDeclType -> FnDecl a -> Either String (FnDecl a)
resolveFnDecl NoSelf (FnDecl (s : _) _ _ _) | isSelfArg s = Left "self argument is not allowed in this function declaration"
resolveFnDecl _      (FnDecl [] o v x) = FnDecl [] <$> sequence (resolveTy ReturnType <$> o) <*> pure v <*> pure x
resolveFnDecl _      (FnDecl (a : as) o v x)
  | any isSelfArg as = Left "self arguments must always be the first arguments"
  | otherwise = FnDecl <$> sequence (resolveArg <$> (a : as)) <*> sequence (resolveTy ReturnType <$> o) <*> pure v <*> pure x

-- | Check whether an argument is one of the "self" forms
isSelfArg :: Arg a -> Bool
isSelfArg Arg{} = False
isSelfArg _ = True

instance Monoid a => Resolve (FnDecl a) where
  resolve = resolveFnDecl AllowSelf
  strip = id

-- | Only some type parameter bounds allow trait bounds to start with ?
data TyParamBoundType
  = NoneBound          -- ^ Don't allow '? poly_trait_ref'
  | ModBound           -- ^ Allow '? poly_trait_ref'

-- | A type parameter bound is invalid if
--
--   * an underlying lifetime or traitref is
--   * it is 'NoneBound' but is a trait bound with a '?' (as in 'ObjectTrait')
resolveTyParamBound :: Monoid a => TyParamBoundType -> TyParamBound a -> Either String (TyParamBound a)
resolveTyParamBound _ (RegionTyParamBound lt) = RegionTyParamBound <$> resolveLifetime lt
resolveTyParamBound NoneBound (TraitTyParamBound _ Maybe) = Left "? trait is not allowed in this type param bound"
resolveTyParamBound _ (TraitTyParamBound p t) = TraitTyParamBound <$> resolvePolyTraitRef p <*> pure t 

instance Monoid a => Resolve (TyParamBound a) where
  resolve = resolveTyParamBound ModBound
  strip = id

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

instance Monoid a => Resolve (Arg a) where
  resolve = resolveArg
  strip = id

-- | A Poly trait ref is valid whenever the underlying trait ref is.
resolvePolyTraitRef :: Monoid a => PolyTraitRef a -> Either String (PolyTraitRef a)
resolvePolyTraitRef (PolyTraitRef lts t x) = PolyTraitRef <$> sequence (resolveLifetimeDef <$> lts) <*> resolveTraitRef t <*> pure x

instance Monoid a => Resolve (PolyTraitRef a) where
  resolve = resolvePolyTraitRef
  strip = id

-- | A lifetime def is invalid if it has non-outer attributes 
resolveLifetimeDef :: LifetimeDef a -> Either String (LifetimeDef a)
resolveLifetimeDef (LifetimeDef as l bds x) = do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  l' <- resolveLifetime l
  bds' <- sequence (resolveLifetime <$> bds)
  pure (LifetimeDef as' l' bds' x)

instance Resolve (LifetimeDef a) where
  resolve = resolveLifetimeDef
  strip = id


--------------
-- Patterns --
--------------

-- TODO: document this - just summarize what the different cases are doing (and the motivation)
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
resolvePat (MacP (Mac p t x) x') = do
  p' <- resolvePath ExprPath p
  MacP <$> resolveMac (Mac p' t x) <*> pure x'

instance Monoid a => Resolve (Pat a) where
  resolve = resolvePat
  strip = id

-- | Field patterns are only invalid if the underlying pattern / identifier is
resolveFieldPat :: Monoid a => FieldPat a -> Either String (FieldPat a)
resolveFieldPat (FieldPat Nothing p x)
  = case p of (IdentP _ _ Nothing _)          -> FieldPat Nothing <$> resolvePat p <*> pure x
              (BoxP (IdentP _ _ Nothing _) _) -> FieldPat Nothing <$> resolvePat p <*> pure x
              _                               -> Left "patterns for fields without an identifier must be (possibly box) identifiers"
resolveFieldPat (FieldPat i p x) = FieldPat <$> sequence (resolveIdent <$> i) <*> resolvePat p <*> pure x

instance Monoid a => Resolve (FieldPat a) where
  resolve = resolveFieldPat
  strip = id


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
  | NonBlockExpr       -- ^ Forbids expressions starting with blocks (things like '{ 1 } + 2')


resolveExpr :: Monoid a => ExprType -> Expr a -> Either String (Expr a)
resolveExpr = resolveExprP 0 

-- TODO Double check the last two cases
rightC :: ExprType -> ExprType
rightC AnyExpr = AnyExpr
rightC NoStructExpr = NoStructExpr
rightC NoStructBlockExpr = AnyExpr
rightC NonBlockExpr = AnyExpr
rightC _ = error "rightC: ExprType should already have been covered!"

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
-- The following group of expression variants work in all of the remaining contexts (see 'gen_expr'
-- in the parser)
resolveExprP p _ (Ret as me x) = parenE (p > 0) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  me' <- sequence (resolveExprP 0 AnyExpr <$> me)
  pure (Ret as' me' x)
resolveExprP p _ (Break as ml me x) = parenE (p > 0) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  ml' <- sequence (resolveLifetime <$> ml)
  me' <- sequence (resolveExprP 0 AnyExpr <$> me)
  pure (Break as' ml' me' x) 
resolveExprP p _ (Continue as ml x) = parenE (p > 0) $ do 
  as' <- sequence (resolveAttr OuterAttr <$> as)
  ml' <- sequence (resolveLifetime <$> ml)
  pure (Continue as' ml' x) 
resolveExprP p _ (Range as Nothing r l x) = parenE (p > 0) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  r' <- sequence (resolveExprP 0 AnyExpr <$> r)
  pure (Range as' Nothing r' l x)
-- Binary expressions
resolveExprP p c (Assign as l r x) = parenE (p > 1) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  l' <- resolveExprP 2 c l
  r' <- resolveExprP 1 (rightC c) r
  pure (Assign as' l' r' x)
resolveExprP p c (AssignOp as o l r x) = parenE (p > 1) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  l' <- resolveExprP 2 c l
  r' <- resolveExprP 1 (rightC c) r
  pure (AssignOp as' o l' r' x)
resolveExprP p c (Range as (Just l) mr rl x) = parenE (p > 1) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  l' <- resolveExprP 2 c l
  mr' <- sequence (resolveExprP 1 NoStructBlockExpr <$> mr)
  pure (Range as' (Just l') mr' rl x)
resolveExprP p c (InPlace as l r x) = parenE (p > 2) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  l' <- resolveExprP 3 c l 
  r' <- resolveExprP 2 (rightC c) r 
  pure (InPlace as' l' r' x)
resolveExprP p c (Binary as o l r x) = parenE (p > p') $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  l' <- resolveExprP p' c l 
  r' <- resolveExprP (p' + 1) (rightC c) r 
  pure (Binary as' o l' r' x)
  where
  p' = opPrec o

  opPrec :: BinOp -> Int
  opPrec AddOp = 10
  opPrec SubOp = 10
  opPrec MulOp = 11
  opPrec DivOp = 11
  opPrec RemOp = 11
  opPrec AndOp = 4
  opPrec OrOp = 3
  opPrec BitXorOp = 7
  opPrec BitAndOp = 8
  opPrec BitOrOp = 6
  opPrec ShlOp = 9
  opPrec ShrOp = 9
  opPrec EqOp = 5
  opPrec LtOp = 5
  opPrec LeOp = 5
  opPrec NeOp = 5
  opPrec GeOp = 5
  opPrec GtOp = 5
-- Cast and type ascriptions expressions pass on their restrictions to their wrapped expression
resolveExprP p c (Cast as e t x) = parenE (p > 12) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 12 c e
  t' <- resolveTy NoSumType t
  pure (Cast as' e' t' x)
resolveExprP p c (TypeAscription as e t x) = parenE (p > 12) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 12 c e
  t' <- resolveTy NoSumType t
  pure (Cast as' e' t' x)
-- Unary expressions pass on their restrictions to their wrapped expression
resolveExprP p c (Unary as o e x) = parenE (p > 13) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 13 c e
  pure (Unary as' o e' x)
resolveExprP p c (AddrOf as m e x) = parenE (p > 13) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 13 c e
  pure (AddrOf as' m e' x)
resolveExprP p c (Box as e x) = parenE (p > 13) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 13 c e
  pure (Box as' e' x)
-- Postfix expressions pass on their restrictions to their wrapped expression (see 'gen_postfix_exp')
resolveExprP p c (Index as e i x) = parenE (p > 14) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 14 c e
  i' <- resolveExprP 0 AnyExpr i
  pure (Index as' e' i' x)
resolveExprP p c (Try as e x) = parenE (p > 14) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 14 c e
  pure (Try as' e' x) 
resolveExprP p c (Call as f xs x) = parenE (p > 14) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  f' <- resolveExprP 14 c f
  xs' <- sequence (resolveExprP 0 AnyExpr <$> xs)
  pure (Call as' f' xs' x)
resolveExprP p c (MethodCall as e i mt es x) = parenE (p > 14) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 14 c e
  i' <- resolveIdent i
  mt' <- case mt of
           Just t -> Just <$> sequence (resolveTy AnyType <$> t)
           Nothing -> pure Nothing
  es' <- sequence (resolveExprP 0 AnyExpr <$> es)
  pure (MethodCall as' e' i' mt' es' x)
resolveExprP p _ (Vec as es x) = parenE (p > 14) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  es' <- sequence (resolveExprP 0 AnyExpr <$> es)
  pure (Vec as' es' x) 
resolveExprP p c (TupField as e i x) = parenE (p > 14) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 14 c e
  pure (TupField as' e' i x)
resolveExprP p c (FieldAccess as e i x) = parenE (p > 14) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 14 c e
  i' <- resolveIdent i
  pure (FieldAccess as' e' i' x)
resolveExprP p _ (PathExpr as Nothing p' x) = parenE (p > 14) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  p'' <- resolvePath ExprPath p'
  pure (PathExpr as' Nothing p'' x)
resolveExprP p _ (PathExpr as q@(Just (QSelf _ i)) p'@(Path g s x) x')
  | i < 0 || i >= length s = Left "index given by QSelf is outside the possible range"
  | i == 0 = parenE (p > 14) $ do
      as' <- sequence (resolveAttr OuterAttr <$> as)
      q' <- sequence (resolveQSelf <$> q)
      p'' <- resolvePath ExprPath p'
      pure (PathExpr as' q' p'' x)
  | otherwise = parenE (p > 14) $ do
      as' <- sequence (resolveAttr OuterAttr <$> as)
      tyP <-   resolvePath TypePath $ Path g     (N.fromList (N.take i s)) mempty
      exprP <- resolvePath ExprPath $ Path False (N.fromList (N.drop i s)) x
      q' <- sequence (resolveQSelf <$> q)
      pure (PathExpr as' q' (Path g (segments tyP <> segments exprP) x) x') 
resolveExprP p _ (Lit as l x) = parenE (p > 14) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  l' <- resolveLit l
  pure (Lit as' l' x)
resolveExprP p _ (Repeat as e r x) = parenE (p > 14) $ do
  as' <- sequence (resolveAttr OuterAttr <$> as)
  e' <- resolveExprP 0 AnyExpr e
  r' <- resolveExprP 0 AnyExpr r
  pure (Repeat as' e' r' x)







parenE :: Monoid a => Bool -> Either String (Expr a) -> Either String (Expr a)
parenE True e = ParenExpr [] <$> e <*> pure mempty
parenE False e = e

{-
resolveExpr (TupExpr [Attribute a] [Expr a] a
resolveExpr (If [Attribute a] (Expr a) (Block a) (Maybe (Expr a)) a
resolveExpr (IfLet [Attribute a] (Pat a) (Expr a) (Block a) (Maybe (Expr a)) a
resolveExpr (While [Attribute a] (Expr a) (Block a) (Maybe (Lifetime a)) a
resolveExpr (WhileLet [Attribute a] (Pat a) (Expr a) (Block a) (Maybe (Lifetime a)) a
resolveExpr (ForLoop [Attribute a] (Pat a) (Expr a) (Block a) (Maybe (Lifetime a)) a
resolveExpr (Loop [Attribute a] (Block a) (Maybe (Lifetime a)) a
resolveExpr (Match [Attribute a] (Expr a) [Arm a] a
resolveExpr (Closure [Attribute a] CaptureBy (FnDecl a) (Expr a) a
resolveExpr (BlockExpr [Attribute a] (Block a) a
resolveExpr (InlineAsmExpr [Attribute a] (InlineAsm a) a
resolveExpr (MacExpr [Attribute a] (Mac a) a
resolveExpr (Struct [Attribute a] (Path a) [Field a] (Maybe (Expr a)) a
resolveExpr (ParenExpr [Attribute a] (Expr a) a
-}

-- The remaining expressions are valid on all (remaining) expression types

resolveMac :: Mac a -> Either String (Mac a)
resolveMac = pure

