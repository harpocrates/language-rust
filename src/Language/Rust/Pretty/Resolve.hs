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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLists #-}

module Language.Rust.Pretty.Resolve (
  Resolve(..),
  Issue(..),
  Severity(..),
  runResolve,
) where

import Language.Rust.Syntax

import Language.Rust.Data.Ident        ( Ident(..), mkIdent )
import Language.Rust.Data.InputStream  ( inputStreamFromString )
import Language.Rust.Data.Position     ( initPos, Spanned(..) )

import Language.Rust.Parser.Lexer      ( lexTokens, lexToken )
import Language.Rust.Parser.ParseMonad ( execParser )

import Control.Monad                   ( when )
import Control.Monad.Trans.RWS

import Data.Dynamic                    ( Dynamic, toDyn, Typeable )
import Data.List                       ( find )
import Data.List.NonEmpty              ( NonEmpty(..) )
import qualified Data.List.NonEmpty as N
import Data.Maybe                      ( fromJust )
import Data.Semigroup                  ( (<>) )


{-# ANN module "HLint: ignore Reduce duplication" #-}

-- TODO:
--  * See where attributes are not allowed
--  * resolve in a better monad (`type ResolveM a = ReaderT [Doc] (Except ErrorType a)`)

-- | Diagnostic for how severe an 'Issue' is 
data Severity
  = Clean      -- ^ Everything is normal (this variant is returned when there was nothing to resolve)
  | Warning    -- ^ There is something fishy looking (AST is valid, but may not be what you expect)
  | Correction -- ^ The AST was invalid, but in a way that could be corrected
  | Error      -- ^ The AST was invalid in some way that could not be automatically fixed
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Localized information about an issue in a syntax tree
data Issue = Issue
  { description :: String    -- ^ Description of the issue
  , severity :: !Severity    -- ^ Severity of the issue
  -- | The first element in this list is the syntax tree where the issue occurs. The next elements
  -- are increasingly zoomed out syntax trees centered on the first element. In lieu of positional
  -- information, this provides a next best way of debugging exactly where the problem is.
  , location :: [Dynamic]
  } deriving (Show)

-- | Monad in which to resolve a syntax tree.
--
--   * Reader is '[Dynamic]' storing the path from root to the current syntax tree node
--   * Writer is '[Issue]' accumulating issues met through the traversal
--   * State is 'Severity' accumulating the most severe 'Issue' found so far in the traversal
--
type ResolveM = RWS [Dynamic] [Issue] Severity

-- | Run a resolve monad, getting back the altered syntax tree, the highest 'Severity' of issues,
-- and the list of 'Issue's found.
runResolve :: ResolveM a -> (a, Severity, [Issue])
runResolve r = runRWS r [] Clean

-- | Log an 'Issue'
logIssue :: String   -- ^ description of the issue
         -> Severity -- ^ severity of the issue
         -> ResolveM ()
logIssue desc sev = do
  loc <- ask
  tell [Issue desc sev loc]
  modify (max sev)

-- | Log a 'Warning'
warn :: String -> ResolveM ()
warn desc = desc `logIssue` Warning

-- | Log a 'Correction'
correct :: String -> ResolveM ()
correct desc = desc `logIssue` Correction

-- | Log an 'Error'
err :: a -> String -> ResolveM a
err x desc = (desc `logIssue` Error) *> pure x

-- | Enter a new syntax tree
scope :: Typeable a => a -> ResolveM b -> ResolveM b
scope x = local (toDyn x :)

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
  resolve x = case runResolve (resolveM x) of
                (_, Error, issues) -> Left $ description (fromJust (find (\i -> severity i == Error) issues))
                (t, _, _) -> Right t

  resolveM :: a -> ResolveM a


-- | A valid sourcfile needs
--
--   * the shebang to be one-line not conflicting with attributes
--   * the attributes to be inner
--   * the items to be 'ModItems'
--
resolveSourceFile :: (Typeable a, Monoid a) => SourceFile a -> ResolveM (SourceFile a)
resolveSourceFile s@(SourceFile sh as is) = scope s $ do
  sh' <- case sh of
           Just ('[':_) -> err sh "shebang cannot start with `['"
           Just s' | '\n' `elem` s' -> err sh "shebang cannot contain newlines"
           _ -> pure sh
  as' <- traverse (resolveAttr InnerAttr) as
  is' <- traverse (resolveItem ModItem) is
  pure (SourceFile sh' as' is')

instance (Typeable a, Monoid a) => Resolve (SourceFile a) where resolveM = resolveSourceFile

-- | An identifier can be invalid if
-- 
--   * it does not lex into an identifier
--   * it is a keyword
--
resolveIdent :: Ident -> ResolveM Ident
resolveIdent i@(Ident s _) =
    scope i $ case toks of
                Right [Spanned (IdentTok i') _]
                   | i /= i' -> err i "identifier does not lex properly"
                   | i `elem` keywords -> err i "identifier is a keyword"
                   | otherwise -> pure i
                _ -> err i "identifier does not lex properly"

  where

  keywords = map mkIdent $ words "as box break const continue crate else enum extern false fn for\
                                \ if impl in let loop match mod move mut pub ref return Self self\
                                \ static struct super trait true type unsafe use where while\
                                \ abstract alignof become do final macro offsetof override priv\
                                \ proc pure sizeof typeof unsized virtual yield" 

  toks = execParser (lexTokens lexToken) (inputStreamFromString s) initPos

instance Resolve Ident where resolveM = resolveIdent


----------------
-- Attributes --
----------------

-- | Attribute type to against which to resolve
data AttrType
  = EitherAttr -- ^ inner or outer attribute
  | InnerAttr  -- ^ only innner attribute
  | OuterAttr  -- ^ only outer attribute

-- | A sugared doc is invalid if
--
--   * the expected attribute style does not match the actual one
--   * its content starts with a '/' / '*' when it is an outer line / inline comment
--   * it is a line comment whose content spans multiple lines
--
-- A regular attribute is invalid if
--
--   * the expected attribute style does not match the actual one
--   * the underlying path / tokenstream are invalid
--   * the tokenstream starts with a '::'
--
resolveAttr :: (Typeable a, Monoid a) => AttrType -> Attribute a -> ResolveM (Attribute a)
resolveAttr typ s@(SugaredDoc sty inl con x) = scope s $ do 
  sty' <- case (typ, sty) of
            (OuterAttr, Inner) -> correct "inner attribute was turned into outer attribute" *> pure Outer
            (InnerAttr, Outer) -> correct "outer attribute was turned into inner attribute" *> pure Inner
            (_,         sty' ) -> pure sty'
  con' <- case (con, inl, sty') of
            ('/':_, False, Outer) -> correct "the content of an outer (line) doc comment cannot start with a `/'" *> pure (' ':con)
            ('*':_, True, Outer) -> correct "the contents of an outer (inline) doc comment cannot start with a a`*'" *> pure (' ':con)
            (_, False, _) | '\n' `elem` con -> err con "a doc comment that is not inline cannot have multiple lines"
            _ -> pure con
  pure (SugaredDoc sty' inl con' x)

resolveAttr typ a@(Attribute sty p ts x) = scope a $ do
  sty' <- case (typ, sty) of
            (OuterAttr, Inner) -> correct "inner attribute was turned into outer attribute" *> pure Outer
            (InnerAttr, Outer) -> correct "outer attribute was turned into inner attribute" *> pure Inner
            (_,         sty' ) -> pure sty'
  p' <- resolvePath ModPath p
  ts' <- resolveTokenStream ts
  case nextTok ts' of
    Just ModSep -> err () "the first token in the token stream `::' will be considered part of the path"
    _ -> pure ()
  pure (Attribute sty' p' ts' x)

  where

  nextTok :: TokenStream -> Maybe Token
  nextTok (Tree (Token _ t)) = Just t
  nextTok (Tree (Delimited _ d _)) = Just (OpenDelim d)
  nextTok (Stream (s:_)) = nextTok s
  nextTok (Stream []) = Nothing

instance (Typeable a, Monoid a) => Resolve (Attribute a) where resolveM = resolveAttr EitherAttr


--------------
-- Literals --
--------------

-- | A literal cannot be invalid
resolveLit :: Lit a -> ResolveM (Lit a)
resolveLit = pure

instance Resolve (Lit a) where resolveM = resolveLit


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
--
-- TODO: guard against no path segments...
resolvePath :: (Typeable a, Monoid a) => PathType -> Path a -> ResolveM (Path a)
resolvePath t p@(Path g segs x) = scope p $
    if null [ () | PathSegment _ (Just a) _ <- segs, not (isParamsForPath t a) ]
      then Path g <$> traverse resolveSeg segs <*> pure x 
      else err p "path parameter is not valid for this type of path"
  where
  resolveSeg :: (Typeable a, Monoid a) => PathSegment a -> ResolveM (PathSegment a)
  resolveSeg (PathSegment i a x') = do
    i' <- case i of
            Ident "self" _ -> pure i
            Ident "Self" _ -> pure i
            Ident "super" _ -> pure i
            _ -> resolveIdent i
    a' <- traverse resolvePathParameters a
    pure (PathSegment i' a' x')

  isParamsForPath :: PathType -> PathParameters a -> Bool
  isParamsForPath t' AngleBracketed{} = t' `elem` ([TypePath, ExprPath] :: [PathType])
  isParamsForPath t' Parenthesized{}  = t' `elem` ([TypePath] :: [PathType])

-- | There are three potential instances for resolving a path (depending on what type it is). The
-- 'Resolve' instance for 'Path' will let through any path.
instance (Typeable a, Monoid a) => Resolve (Path a) where
  resolveM = resolvePath TypePath

-- | A path parameter can be invalid if any of its constituent components are invalid
resolvePathParameters :: (Typeable a, Monoid a) => PathParameters a -> ResolveM (PathParameters a)
resolvePathParameters p@(AngleBracketed lts tys bds x) = scope p $ do
  lts' <- traverse resolveLifetime lts
  tys' <- traverse (resolveTy AnyType) tys
  bds' <- traverse (\(i,t) -> (,) <$> resolveIdent i <*> resolveTy NoSumType t) bds
  pure (AngleBracketed lts' tys' bds' x)
resolvePathParameters p@(Parenthesized tys tym x) = scope p $ do
  tys' <- traverse (resolveTy AnyType) tys
  tym' <- traverse (resolveTy NoSumType) tym
  pure (Parenthesized tys' tym' x)

instance (Typeable a, Monoid a) => Resolve (PathParameters a) where resolveM = resolvePathParameters

-- | A QSelf by itself is only invalid when the underlying type is
resolveQSelf :: (Typeable a, Monoid a) => QSelf a -> ResolveM (QSelf a)
resolveQSelf q@(QSelf t p) = scope q (QSelf <$> resolveTy AnyType t <*> pure p)

instance (Typeable a, Monoid a) => Resolve (QSelf a) where resolveM = resolveQSelf


-----------
-- Types --
-----------

-- | A lifetime can only be invalid if the underlying identifier is. Note that lifetimes cannot use
-- keywords.
resolveLifetime :: Typeable a => Lifetime a -> ResolveM (Lifetime a)
resolveLifetime l@(Lifetime n _)
  | n == "static" = pure l 
  | otherwise = scope l (resolveIdent (mkIdent n) *> pure l)

instance Typeable a => Resolve (Lifetime a) where resolveM = resolveLifetime

-- | A trait ref is invalid if the underlying type path is.
resolveTraitRef :: (Typeable a, Monoid a) => TraitRef a -> ResolveM (TraitRef a)
resolveTraitRef t@(TraitRef p) = scope t (TraitRef <$> resolvePath TypePath p)

instance (Typeable a, Monoid a) => Resolve (TraitRef a) where resolveM = resolveTraitRef

-- | There a a variety of constraints imposed on types, representing different invariants
data TyType
  = AnyType        -- ^ No restrictions
  | NoSumType      -- ^ Any type except for 'TraitObject' with a '+'
  | PrimParenType  -- ^ Types not starting with '<' or '(', or paren types with no sum types inside
  | NoForType      -- ^ Non-sum types not starting with a 'for'

-- | Resolve a given type, and a constraint on it (see the parser 'Internal.y' for more details on
-- these cases). 
resolveTy :: (Typeable a, Monoid a) => TyType -> Ty a -> ResolveM (Ty a)
-- TraitObject
resolveTy NoSumType     o@(TraitObject b _) | length b > 1 = scope o (correct "added parens around trait object type" *> resolveTy NoSumType (ParenTy o mempty))
resolveTy NoForType     o@TraitObject{} = scope o (correct "added parens around trait object type" *> resolveTy NoForType (ParenTy o mempty))
resolveTy _             o@(TraitObject bds@(TraitTyParamBound{} :| _) x)
  = scope o (TraitObject <$> traverse (resolveTyParamBound ModBound) bds <*> pure x)
resolveTy _             o@TraitObject{} = scope o (err o "first bound in trait object should be a trait bound")
-- ParenTy
resolveTy PrimParenType p@(ParenTy ty' x) = scope p (ParenTy <$> resolveTy NoSumType ty' <*> pure x)
resolveTy _             p@(ParenTy ty' x) = scope p (ParenTy <$> resolveTy AnyType ty' <*> pure x)
-- TupTy
resolveTy PrimParenType t@TupTy{} = scope t (correct "added parens around tuple type" *> resolveTy PrimParenType (ParenTy t mempty))
resolveTy _             t@(TupTy tys x) = scope t (TupTy <$> traverse (resolveTy AnyType) tys <*> pure x)
-- ImplTrait
resolveTy _             i@(ImplTrait bds x) = scope i (ImplTrait <$> traverse (resolveTyParamBound ModBound) bds <*> pure x)
-- PathTy
resolveTy PrimParenType p@(PathTy (Just _) _ _) = scope p (correct "added parents around path type" *> resolveTy PrimParenType (ParenTy p mempty))
resolveTy _             p@(PathTy q p'@(Path _ s _) x) = scope p $
  case q of
      Just (QSelf _ i)
        | 0 <= i && i < length s -> PathTy <$> traverse resolveQSelf q <*> resolvePath TypePath p' <*> pure x
        | otherwise              -> err p "index given by QSelf is outside the possible range"
      Nothing                    -> PathTy Nothing <$> resolvePath TypePath p' <*> pure x 
-- BareFn
resolveTy NoForType     f@(BareFn _ _ (_:_) _ _) = scope f (correct "added parens around `for' function type" *> resolveTy NoForType (ParenTy f mempty))
resolveTy _             f@(BareFn u a lts fd x) = scope f (BareFn u a <$> traverse resolveLifetimeDef lts <*> resolveFnDecl declTy GeneralArg fd <*> pure x)
  where declTy = if a == C then VarNoSelf else NoSelf
-- Other types (don't care about the context)
resolveTy _   (Never x) = pure (Never x)
resolveTy _ p@(Ptr mut ty' x) = scope p (Ptr mut <$> resolveTy NoSumType ty' <*> pure x)
resolveTy _ r@(Rptr lt mut ty' x) = scope r (Rptr <$> traverse resolveLifetime lt <*> pure mut <*> resolveTy NoSumType ty' <*> pure x)
resolveTy _ t@(Typeof e x) = scope t (Typeof <$> resolveExpr AnyExpr e <*> pure x)
resolveTy _   (Infer x) = pure (Infer x)
resolveTy _ s@(Slice ty' x) = scope s (Slice <$> resolveTy AnyType ty' <*> pure x)
resolveTy _ a@(Array ty' e x) = scope a (Array <$> resolveTy AnyType ty' <*> resolveExpr AnyExpr e <*> pure x)
resolveTy _ m@(MacTy (Mac p t x) x') = scope m $ do
  p' <- resolvePath TypePath p
  MacTy <$> resolveMac TypePath (Mac p' t x) <*> pure x'

instance (Typeable a, Monoid a) => Resolve (Ty a) where resolveM = resolveTy AnyType

-- In some cases, the first argument of a function declaration may be a 'self'
data FnDeclType
  = NoSelf     -- ^ the first argument cannot be self
  | VarNoSelf  -- ^ the first argument cannot be self, and the function can be variadic
  | AllowSelf  -- ^ the first argument can be self
  deriving (Eq)

-- | A function declaration can be invalid if it has self arguments in the wrong places (or when it
-- shouldn't)
resolveFnDecl :: (Typeable a, Monoid a) => FnDeclType -> ArgType -> FnDecl a -> ResolveM (FnDecl a)
resolveFnDecl fn _  f@(FnDecl (s : _) _ _ _)  | isSelfArg s && fn /= AllowSelf = scope f (err f "self argument is not allowed in this function declaration")
resolveFnDecl _  _  f@(FnDecl (_ : as) _ _ _) | any isSelfArg as = scope f (err f "self arguments must always be the first arguments")
resolveFnDecl fn _  f@(FnDecl _ _ True _)     | fn /= VarNoSelf = scope f (err f "this function declaration cannot be variadic")
resolveFnDecl _  at f@(FnDecl as o v x) = scope f (FnDecl <$> traverse (resolveArg at) as <*> traverse (resolveTy AnyType) o <*> pure v <*> pure x)

-- | Check whether an argument is one of the "self" forms
isSelfArg :: Arg a -> Bool
isSelfArg Arg{} = False
isSelfArg _ = True

instance (Typeable a, Monoid a) => Resolve (FnDecl a) where resolveM = resolveFnDecl AllowSelf NamedArg

-- | Only some type parameter bounds allow trait bounds to start with ?
data TyParamBoundType
  = NoneBound          -- ^ Don't allow '? poly_trait_ref'
  | ModBound           -- ^ Allow '? poly_trait_ref'

-- | A type parameter bound is invalid if
--
--   * an underlying lifetime or traitref is
--   * it is 'NoneBound' but is a trait bound with a '?' (as in 'ObjectTrait')
--
resolveTyParamBound :: (Typeable a, Monoid a) => TyParamBoundType -> TyParamBound a -> ResolveM (TyParamBound a)
resolveTyParamBound _         b@(RegionTyParamBound lt x) =     scope b (RegionTyParamBound <$> resolveLifetime lt <*> pure x)
resolveTyParamBound NoneBound b@(TraitTyParamBound _ Maybe _) = scope b (err b "? trait is not allowed in this type param bound")
resolveTyParamBound _         b@(TraitTyParamBound p t x) =     scope b (TraitTyParamBound <$> resolvePolyTraitRef p <*> pure t <*> pure x)

instance (Typeable a, Monoid a) => Resolve (TyParamBound a) where resolveM = resolveTyParamBound ModBound

-- | There are several restricted forms of arguments allowed
data ArgType
  = GeneralArg  -- ^ Arguments allowed in places without implementation (optional limited pattern followed by a type)
  | NamedArg    -- ^ Arguments allowed in most places (any pattern followed by any type)
                -- This includes lambda arguments

-- | The only types of patterns supported by arguments are wild or identifiers
resolveArg :: (Typeable a, Monoid a) => ArgType -> Arg a -> ResolveM (Arg a)
resolveArg _ s@SelfValue{} = pure s
resolveArg _ a@(SelfRegion lt m x) = scope a (SelfRegion <$> traverse resolveLifetime lt <*> pure m <*> pure x)
resolveArg _ a@(SelfExplicit t m x) = scope a (SelfExplicit <$> resolveTy AnyType t <*> pure m <*> pure x)
resolveArg NamedArg  a@(Arg Nothing _ _) = scope a (err a "named arguments must have patterns")
resolveArg NamedArg  a@(Arg p t x) = scope a $ do
  when (isSelfAlike a) $
    warn "argument looks like a self argument - did you mean to use 'SelfValue', 'SelfRegion', or 'SelfExplicit'?"

  p' <- traverse resolvePat p
  t' <- resolveTy AnyType t
  pure (Arg p' t' x)

resolveArg GeneralArg a@(Arg p t x) = scope a $ do
  when (isSelfAlike a) $
    warn "argument looks like a self argument - did you mean to use 'SelfValue', 'SelfRegion', or 'SelfExplicit'?"
  
  p' <- case p of
          Nothing -> pure Nothing
          Just WildP{} -> traverse resolvePat p
          Just IdentP{} -> traverse resolvePat p
          Just (RefP WildP{} Immutable _) -> traverse resolvePat p
          Just (RefP (IdentP (ByValue Immutable) _ _ _) Immutable _) -> traverse resolvePat p
          Just (RefP (RefP WildP{} Immutable _) Immutable _) -> traverse resolvePat p
          Just (RefP (RefP (IdentP (ByValue Immutable) _ _ _) Immutable _) Immutable _) -> traverse resolvePat p
          _ -> scope p (err p "this pattern is not allowed for this type of argument")
  t' <- resolveTy AnyType t
  pure (Arg p' t' x)

-- | Check whether an argument is one of the "self"-alike forms
isSelfAlike :: Arg a -> Bool
isSelfAlike (Arg Nothing (PathTy Nothing (Path False [PathSegment (Ident "self" _) Nothing _] _) _) _) = True
isSelfAlike (Arg Nothing (Rptr _ _ (PathTy Nothing (Path False [PathSegment (Ident "self" _) Nothing _] _) _) _) _) = True
isSelfAlike _ = False

instance (Typeable a, Monoid a) => Resolve (Arg a) where resolveM = resolveArg NamedArg

-- | A Poly trait ref is valid whenever the underlying trait ref is.
resolvePolyTraitRef :: (Typeable a, Monoid a) => PolyTraitRef a -> ResolveM (PolyTraitRef a)
resolvePolyTraitRef p@(PolyTraitRef lts t x) = scope p $ do
  lts' <- traverse resolveLifetimeDef lts
  t' <- resolveTraitRef t
  pure (PolyTraitRef lts' t' x)

instance (Typeable a, Monoid a) => Resolve (PolyTraitRef a) where resolveM = resolvePolyTraitRef

-- | A lifetime def is invalid if it has non-outer attributes 
resolveLifetimeDef :: (Typeable a, Monoid a) => LifetimeDef a -> ResolveM (LifetimeDef a)
resolveLifetimeDef lts@(LifetimeDef as l bds x) = scope lts $ do
  as' <- traverse (resolveAttr OuterAttr) as
  l' <- resolveLifetime l
  bds' <- traverse resolveLifetime bds
  pure (LifetimeDef as' l' bds' x)

instance (Typeable a, Monoid a) => Resolve (LifetimeDef a) where resolveM = resolveLifetimeDef


--------------
-- Patterns --
--------------

-- | A pattern can be invalid of
--
--   * the index of the '...' in the tuple/tuple-struct is out of range
--   * the index of the qself path is out of range
--   * any underlying component is invalid
--
resolvePat :: (Typeable a, Monoid a) => Pat a -> ResolveM (Pat a)
-- TupleStruct
resolvePat t@(TupleStructP p fs im x) = scope t $ do
  p' <- resolvePath ExprPath p
  fs' <- traverse resolvePat fs
  im' <- case im of
           Nothing -> pure Nothing
           Just i | 0 <= i && i <= length fs -> pure (Just i)
           _ -> err im "index of ... in tuple struct pattern is outside of field range"
  pure (TupleStructP p' fs' im' x)
-- PathP
resolvePat p@(PathP Nothing a x) = scope p (PathP Nothing <$> resolvePath ExprPath a <*> pure x)
resolvePat p@(PathP q@(Just (QSelf _ i)) p'@(Path g s x) x')
  | i < 0 || i >= length s = scope p (err p "index given by QSelf is outside the possible range")
  | i == 0 = scope p (PathP <$> traverse resolveQSelf q <*> resolvePath ExprPath p' <*> pure x)
  | otherwise = scope p $ do
      tyP@(Path _ tyPSegs _) <-   resolvePath TypePath $ Path g (take i s) mempty
      exprP@(Path _ exprPSegs _) <- resolvePath ExprPath $ Path False (drop i s) x
      q' <- traverse resolveQSelf q
      pure (PathP q' (Path g (tyPSegs <> exprPSegs) x) x')
-- TupleP
resolvePat p@(TupleP ps i x) = scope p $ do
  ps' <- traverse resolvePat ps
  i' <- case i of
          Nothing -> pure Nothing
          Just j | 0 <= j && j <= length ps -> pure i
                 | otherwise -> err i "index of ... in tuple pattern is outside of range"
  pure (TupleP ps' i' x)

-- Everything else...
resolvePat p@(LitP e x) = scope p (LitP <$> resolveExpr LitExpr e <*> pure x)
resolvePat p@(RangeP l h x) = scope p (RangeP <$> resolveExpr LitOrPathExpr l <*> resolveExpr LitOrPathExpr h <*> pure x)
resolvePat p@(WildP x) = scope p (pure (WildP x))
resolvePat p@(IdentP m i p' x) = scope p (IdentP m <$> resolveIdent i <*> traverse resolvePat p' <*> pure x)
resolvePat p@(StructP p' fs b x) = scope p (StructP <$> resolvePath ExprPath p' <*> traverse resolveFieldPat fs <*> pure b <*> pure x)
resolvePat p@(BoxP p' x) = scope p (BoxP <$> resolvePat p' <*> pure x)
resolvePat p@(RefP p' m x) = scope p (RefP <$> resolvePat p' <*> pure m <*> pure x)
resolvePat p@(SliceP b m a x) = scope p (SliceP <$> traverse resolvePat b <*> traverse resolvePat m <*> traverse resolvePat a <*> pure x)
resolvePat p@(MacP m x) = scope p (MacP <$> resolveMac ExprPath m <*> pure x)

instance (Typeable a, Monoid a) => Resolve (Pat a) where resolveM = resolvePat

-- | Field patterns are only invalid if the underlying pattern / identifier is
resolveFieldPat :: (Typeable a, Monoid a) => FieldPat a -> ResolveM (FieldPat a)
resolveFieldPat f@(FieldPat Nothing p x) = scope f $
    case p of (IdentP _ _ Nothing _)          -> FieldPat Nothing <$> resolvePat p <*> pure x
              (BoxP (IdentP _ _ Nothing _) _) -> FieldPat Nothing <$> resolvePat p <*> pure x
              _                               -> err f "patterns for fields without an identifier must be (possibly box) identifiers"
resolveFieldPat f@(FieldPat i p x) = scope f (FieldPat <$> traverse resolveIdent i <*> resolvePat p <*> pure x)

instance (Typeable a, Monoid a) => Resolve (FieldPat a) where resolveM = resolveFieldPat


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
  | SemiExpr          -- ^ Forbids expressions starting with blocks (things like '{ 1 } + 2') unless
                      -- the leading block has a postfix expression, allows expressions that are
                      -- just one big block. Forbids '{ 1 }[0]' since it is treated as '{ 1 }; [0]'
                      -- and '{ 1 }(0)' since it is treated as '{ 1 }; (0)'
{-
-- Check if an expression has the form of a "postfix" (if that's the case, then you don't worry
-- about what is inside).
--
-- ie: `if i[0] == j[0] { i } else { j } [1]`
lhsSemiExpr :: (Typeable a, Monoid a) => Int -> Expr a -> ResolveM (Expr a)
lhsSemiExpr p t@(Try _ e _) = resolveExprP p AnyExpr  
                          
lhsSemiExpr p (FieldAccess _ e _ _) | isBlockLike e = 
lhsSemiExpr p (Try _ e _)
lhsSemiExpr p (Try _ e _)

-}

resolveLhsExprP :: (Typeable a, Monoid a) => Int -> ExprType -> Expr a -> ResolveM (Expr a)
resolveLhsExprP p SemiExpr l@Try{}         = resolveExprP p AnyExpr l
resolveLhsExprP p SemiExpr l@FieldAccess{} = resolveExprP p AnyExpr l 
resolveLhsExprP p SemiExpr l@MethodCall{}  = resolveExprP p AnyExpr l
resolveLhsExprP p SemiExpr l@TupField{}    = resolveExprP p AnyExpr l
resolveLhsExprP _ SemiExpr l | isBlockLike l = parenthesize l
resolveLhsExprP p t l = resolveExprP p (lhs t) l
  where
    -- | Given the type of expression, what type of expression is allowed on the LHS
    lhs :: ExprType -> ExprType
    lhs LitExpr = error "literal expressions never have a left hand side"
    lhs LitOrPathExpr = error "literal or path expressions never have a left hand side"
    lhs AnyExpr = AnyExpr
    lhs NoStructExpr = NoStructExpr
    lhs NoStructBlockExpr = NoStructBlockExpr
    lhs SemiExpr = AnyExpr

-- | Given the type of expression, what type of expression is allowed on the RHS
rhs :: ExprType -> ExprType
rhs LitExpr = error "literal expressions never have a right hand side"
rhs LitOrPathExpr = error "literal or path expressions never have a right hand side"
rhs AnyExpr = AnyExpr
rhs NoStructExpr = NoStructExpr
rhs NoStructBlockExpr = NoStructExpr
rhs SemiExpr = AnyExpr

-- | Given the type of expression, what type of expression is allowed on the RHS (after '..'/'...')
rhs2 :: ExprType -> ExprType
rhs2 LitExpr = error "literal expressions never have a right hand side (2)"
rhs2 LitOrPathExpr = error "literal or path expressions never have a right hand side (2)"
rhs2 AnyExpr = AnyExpr
rhs2 NoStructExpr = NoStructBlockExpr
rhs2 NoStructBlockExpr = NoStructBlockExpr
rhs2 SemiExpr = AnyExpr

-- | Resolve an expression of the given type in a general context
resolveExpr :: (Typeable a, Monoid a) => ExprType -> Expr a -> ResolveM (Expr a)
resolveExpr = resolveExprP 0

instance (Typeable a, Monoid a) => Resolve (Expr a) where resolveM = resolveExpr AnyExpr

parenthesize :: (Typeable a, Monoid a) => Expr a -> ResolveM (Expr a)
parenthesize e = do
  correct "added parens around expression"
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
resolveExprP :: (Typeable a, Monoid a) => Int -> ExprType -> Expr a -> ResolveM (Expr a)
-- Cover the 'LitExpr' type of expression
resolveExprP p LitExpr l@Lit{} = resolveExprP p AnyExpr l
resolveExprP p LitExpr n@(Unary _ Neg Lit{} _) = resolveExprP p AnyExpr n
resolveExprP _ LitExpr l = scope l (err l "expression is not literal or negated literal")
-- Cover the 'LitOrPathExpr' type of expression
resolveExprP p LitOrPathExpr l@Lit{} = resolveExprP p AnyExpr l
resolveExprP p LitOrPathExpr n@(Unary _ Neg Lit{} _) = resolveExprP p AnyExpr n
resolveExprP p LitOrPathExpr p'@PathExpr{} = resolveExprP p AnyExpr p'
resolveExprP _ LitOrPathExpr l = scope l (err l "expression is not literal, negated literal, path, or qualified path")
-- The following group of expression variants work in all of the remaining contexts (see
-- 'gen_expression' in the parser)
resolveExprP p c b@(Box as e x) = scope b $ parenE (p > 0) $ do
  as' <- traverse (resolveAttr OuterAttr) as
  e' <- resolveExprP 0 (rhs c) e
  pure (Box as' e' x)
resolveExprP p c r@(Ret as me x) = scope r $ parenE (p > 0) $ do
  as' <- traverse (resolveAttr OuterAttr) as
  me' <- traverse (resolveExprP 0 (rhs c)) me
  pure (Ret as' me' x)
resolveExprP p c r@(Yield as me x) = scope r $ parenE (p > 0) $ do
  as' <- traverse (resolveAttr OuterAttr) as
  me' <- traverse (resolveExprP 0 (rhs c)) me
  pure (Yield as' me' x)
resolveExprP p c b@(Break as ml me x) = scope b $ parenE (p > 0) $ do
  as' <- traverse (resolveAttr OuterAttr) as
  ml' <- traverse resolveLbl ml
  me' <- traverse (resolveExprP 0 (rhs c)) me
  pure (Break as' ml' me' x) 
resolveExprP p _ c@(Continue as ml x) = scope c $ parenE (p > 0) $ do 
  as' <- traverse (resolveAttr OuterAttr) as
  ml' <- traverse resolveLbl ml
  pure (Continue as' ml' x)
-- Closures
resolveExprP _ _ c@(Closure _ _ _ (FnDecl _ _ True _) _ _) = scope c (err c "closures can never be variadic")
resolveExprP p c e@(Closure as m cb fn@(FnDecl _ ret _ _) b x) = scope c $
    case (c, ret, b) of
      (NoStructExpr,      Just _, BlockExpr{}) -> parenthesize e
      (NoStructBlockExpr, Just _, BlockExpr{}) -> parenthesize e
      (NoStructExpr,      Just _, _          ) -> parenthesize (Closure as m cb fn (asBlock b) x) 
      (NoStructBlockExpr, Just _, _          ) -> parenthesize (Closure as m cb fn (asBlock b) x)
      (_,                 Just _, BlockExpr{}) -> resolved AnyExpr
      (_,                 Just _, _          ) -> parenthesize (Closure as m cb fn (asBlock b) x)
      _                                         -> resolved (rhs c)
  where
  asBlock ex = BlockExpr [] (Block [NoSemi ex mempty] Normal mempty) mempty

  resolved c' = parenE (p > 0) $ do
    as' <- traverse (resolveAttr OuterAttr) as
    fn' <- resolveFnDecl NoSelf NamedArg fn
    b' <- resolveExprP 0 c' b
    pure (Closure as' m cb fn' b' x)
-- Assignment/in-place expressions
resolveExprP p c a@(Assign as l r x) = scope a $ parenE (p > 1) $ do
  as' <- traverse (resolveAttr OuterAttr) as
  --l' <- resolveExprP 2 (lhs c) l
  l' <- resolveLhsExprP 2 c l
  r' <- resolveExprP 1 (rhs c) r
  pure (Assign as' l' r' x)
resolveExprP p c a@(AssignOp as o l r x) = scope a $ parenE (p > 1) $ do
  as' <- traverse (resolveAttr OuterAttr) as
  --l' <- resolveExprP 2 (lhs c) l
  l' <- resolveLhsExprP 2 c l
  r' <- resolveExprP 1 (rhs c) r
  pure (AssignOp as' o l' r' x)
resolveExprP p c i@(InPlace as l r x) = scope i $ parenE (p > 2) $ do
  as' <- traverse (resolveAttr OuterAttr) as
  --l' <- resolveExprP 3 (lhs c) l 
  l' <- resolveLhsExprP 3 c l
  r' <- resolveExprP 2 (rhs c) r 
  pure (InPlace as' l' r' x)
-- Range expressions
resolveExprP _ _ r@(Range _ _ Nothing Closed _) = scope r (err r "inclusive ranges must be bounded at the end")
resolveExprP _ _ r@(Range as Nothing Nothing rl x) = scope r $ do
  as' <- traverse (resolveAttr OuterAttr) as
  pure (Range as' Nothing Nothing rl x)
resolveExprP p c a@(Range as (Just l) (Just r) rl x) = scope a $ parenE (p > 3) $ do
  as' <- traverse (resolveAttr OuterAttr) as
 -- l' <- resolveExprP 4 (lhs c) l
  l' <- resolveLhsExprP 4 c l
  r' <- resolveExprP 4 (rhs2 c) r
  pure (Range as' (Just l') (Just r') rl x)
resolveExprP p c r@(Range as (Just l) Nothing rl x) = scope r $ parenE (p > 4) $ do
  as' <- traverse (resolveAttr OuterAttr) as
  -- l' <- resolveExprP 4 (lhs c) l
  l' <- resolveLhsExprP 4 c l
  pure (Range as' (Just l') Nothing rl x)
resolveExprP p c a@(Range as Nothing (Just r) rl x) = scope a $ parenE (p > 5) $ do
  as' <- traverse (resolveAttr OuterAttr) as
  r' <- resolveExprP 5 (rhs2 c) r
  pure (Range as' Nothing (Just r') rl x)
-- Binary expressions
resolveExprP p c b@(Binary as o l r x) = scope b $ parenE (p > p') $ do
  as' <- traverse (resolveAttr OuterAttr) as
  -- l' <- resolveExprP p' (lhs c) l 
  l' <- resolveLhsExprP p' c l
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
resolveExprP p c a@(Cast as e t x) = scope a $ parenE (p > 15) $ do
  as' <- traverse (resolveAttr OuterAttr) as
  --e' <- resolveExprP 15 (lhs c) e
  e' <- resolveLhsExprP 15 c e
  t' <- resolveTy NoSumType t
  pure (Cast as' e' t' x)
resolveExprP p c a@(TypeAscription as e t x) = scope a $ parenE (p > 15) $ do
  as' <- traverse (resolveAttr OuterAttr) as
  --e' <- resolveExprP 15 (lhs c) e
  e' <- resolveLhsExprP 15 c e
  t' <- resolveTy NoSumType t
  pure (TypeAscription as' e' t' x)
-- Unary expressions
resolveExprP p c u@(Unary as o e x) = scope u $ parenE (p > 16) $ do
  as' <- traverse (resolveAttr OuterAttr) as
  e' <- resolveExprP 16 (rhs c) e
  pure (Unary as' o e' x)
resolveExprP p c a@(AddrOf as m e x) = scope a $ parenE (p > 16) $ do
  as' <- traverse (resolveAttr OuterAttr) as
  e' <- resolveExprP 16 (rhs c) e
  pure (AddrOf as' m e' x)
-- Postfix expressions
resolveExprP p c a@(Index as e i x) = scope a $ parenE (p > 17) $ do
  as' <- traverse (resolveAttr OuterAttr) as
  --e' <- resolveExprP 17 (lhs c) e
  e' <- resolveLhsExprP 17 c e
  i' <- resolveExprP 0 AnyExpr i
  pure (Index as' e' i' x)
resolveExprP p SemiExpr t@Try{} = resolveExprP p AnyExpr t
resolveExprP p c t@(Try as e x) = scope t $ parenE (p > 17) $ do
  as' <- traverse (resolveAttr OuterAttr) as
  --e' <- resolveExprP 17 (lhs c) e
  e' <- resolveLhsExprP 17 c e
  pure (Try as' e' x) 
resolveExprP p c a@(Call as f xs x) = scope a $ parenE (p > 17) $ do
  as' <- traverse (resolveAttr OuterAttr) as
  --f' <- resolveExprP 17 (lhs c) f
  f' <- resolveLhsExprP 17 c f
  xs' <- traverse (resolveExprP 0 AnyExpr) xs
  pure (Call as' f' xs' x)
resolveExprP p SemiExpr m@MethodCall{} = resolveExprP p AnyExpr m
resolveExprP p c m@(MethodCall as e i mt es x) = scope m $ parenE (p > 17) $ do
  as' <- traverse (resolveAttr OuterAttr) as
  --e' <- resolveExprP 17 (lhs c) e
  e' <- resolveLhsExprP 17 c e
  i' <- resolveIdent i
  mt' <- case mt of
           Just t -> Just <$> traverse (resolveTy AnyType) t
           Nothing -> pure Nothing
  es' <- traverse (resolveExprP 0 AnyExpr) es
  pure (MethodCall as' e' i' mt' es' x)
resolveExprP p SemiExpr t@TupField{} = resolveExprP p AnyExpr t
resolveExprP p c t@(TupField as e i x) = scope t $ parenE (p > 17) $ do
  as' <- traverse (resolveAttr OuterAttr) as
  --e' <- resolveExprP 17 (lhs c) e
  e' <- resolveLhsExprP 17 c e
  pure (TupField as' e' i x)
resolveExprP p SemiExpr f@FieldAccess{} = resolveExprP p AnyExpr f
resolveExprP p c f@(FieldAccess as e i x) = scope f $ parenE (p > 17) $ do
  as' <- traverse (resolveAttr OuterAttr) as
  --e' <- resolveExprP 17 (lhs c) e
  e' <- resolveLhsExprP 17 c e
  i' <- resolveIdent i
  pure (FieldAccess as' e' i' x)
-- Immediate expressions
resolveExprP _ _ v@(Vec as es x) = scope v $ do
  as' <- traverse (resolveAttr EitherAttr) as
  es' <- traverse (resolveExprP 0 AnyExpr) es
  pure (Vec as' es' x) 
resolveExprP _ _ p@(PathExpr as Nothing p' x) = scope p $ do
  as' <- traverse (resolveAttr OuterAttr) as
  p'' <- resolvePath ExprPath p'
  pure (PathExpr as' Nothing p'' x)
resolveExprP _ _ p@(PathExpr as q@(Just (QSelf _ i)) p'@(Path g s x) x')
  | i < 0 || i >= length s = scope p (err p "index given by QSelf is outside the possible range")
  | i == 0 = scope p $ do
      as' <- traverse (resolveAttr OuterAttr) as
      q' <- traverse resolveQSelf q
      p'' <- resolvePath ExprPath p'
      pure (PathExpr as' q' p'' x)
  | otherwise = scope p $ do
      as' <- traverse (resolveAttr OuterAttr) as
      tyP@(Path _ tyPSegs _) <-   resolvePath TypePath $ Path g (take i s) mempty
      exprP@(Path _ exprPSegs _) <- resolvePath ExprPath $ Path False (drop i s) x
      q' <- traverse resolveQSelf q
      pure (PathExpr as' q' (Path g (tyPSegs <> exprPSegs) x) x') 
resolveExprP _ _ i@(Lit as l x) = scope i $ do
  as' <- traverse (resolveAttr OuterAttr) as
  l' <- resolveLit l
  pure (Lit as' l' x)
resolveExprP _ _ a@(Repeat as e r x) = scope a $ do
  as' <- traverse (resolveAttr OuterAttr) as
  e' <- resolveExprP 0 AnyExpr e
  r' <- resolveExprP 0 AnyExpr r
  pure (Repeat as' e' r' x)
-- Macro expressions
resolveExprP _ _ a@(MacExpr as m x) = scope a $ do
  as' <- traverse (resolveAttr OuterAttr) as
  m' <- resolveMac ExprPath m
  pure (MacExpr as' m' x)
-- Paren expressions
resolveExprP _ _ p@(ParenExpr as e x) = scope p $ do
  as' <- traverse (resolveAttr EitherAttr) as
  e' <- resolveExprP 0 AnyExpr e
  pure (ParenExpr as' e' x)
resolveExprP _ _ t@(TupExpr as es x) = scope t $ do
  as' <- traverse (resolveAttr EitherAttr) as
  es' <- traverse (resolveExprP 0 AnyExpr) es
  pure (TupExpr as' es' x)
-- Block expressions
resolveExprP _ NoStructBlockExpr e@BlockExpr{} = parenthesize e
resolveExprP _ _ l@(BlockExpr as b x) = scope l $ do
  as' <- traverse (resolveAttr EitherAttr) as
  b' <- resolveBlock b
  pure (BlockExpr as' b' x) 
-- Struct expressions
resolveExprP _ NoStructExpr e@Struct{} = parenthesize e
resolveExprP _ NoStructBlockExpr e@Struct{} = parenthesize e
resolveExprP _ _ s@(Struct as p' fs e x) = scope s $ do
  as' <- traverse (resolveAttr OuterAttr) as
  p'' <- resolvePath ExprPath p'
  fs' <- traverse resolveField fs
  e' <- traverse (resolveExprP 0 AnyExpr) e
  pure (Struct as' p'' fs' e' x)
-- Block-like expressions
resolveExprP p c i@(If as e b es x) = scope i $ do
  as' <- traverse (resolveAttr OuterAttr) as
  e' <- resolveExprP 0 NoStructExpr e
  b' <- resolveBlock b
  es' <- case es of
           Nothing -> pure Nothing
           (Just If{}) -> traverse (resolveExprP p c) es
           (Just IfLet{}) -> traverse (resolveExprP p c) es
           (Just BlockExpr{}) -> traverse (resolveExprP p c) es
           (Just e'') -> Just <$> resolveExprP p c (BlockExpr [] (Block [NoSemi e'' mempty] Normal mempty) mempty)
  pure (If as' e' b' es' x)
resolveExprP p c i@(IfLet as p' e b es x) = scope i $ do
  as' <- traverse (resolveAttr OuterAttr) as
  p'' <- resolvePat p'
  e' <- resolveExprP 0 NoStructExpr e
  b' <- resolveBlock b
  es' <- case es of
           Nothing -> pure Nothing
           (Just If{}) -> traverse (resolveExprP p c) es
           (Just IfLet{}) -> traverse (resolveExprP p c) es
           (Just BlockExpr{}) -> traverse (resolveExprP p c) es
           (Just e'') -> Just <$> resolveExprP p c (BlockExpr [] (Block [NoSemi e'' mempty] Normal mempty) mempty)
  pure (IfLet as' p'' e' b' es' x)
resolveExprP _ _ w@(While as e b l x) = scope w $ do
  as' <- traverse (resolveAttr EitherAttr) as
  e' <- resolveExprP 0 NoStructExpr e
  b' <- resolveBlock b
  l' <- traverse resolveLbl l
  pure (While as' e' b' l' x)
resolveExprP _ _ w@(WhileLet as p' e b l x) = scope w $ do
  as' <- traverse (resolveAttr EitherAttr) as
  p'' <- resolvePat p'
  e' <- resolveExprP 0 NoStructExpr e
  b' <- resolveBlock b
  l' <- traverse resolveLbl l
  pure (WhileLet as' p'' e' b' l' x)
resolveExprP _ _ f@(ForLoop as p' e b l x) = scope f $ do
  as' <- traverse (resolveAttr EitherAttr) as
  p'' <- resolvePat p'
  e' <- resolveExprP 0 NoStructExpr e
  b' <- resolveBlock b
  l' <- traverse resolveLbl l
  pure (ForLoop as' p'' e' b' l' x)
resolveExprP _ _ o@(Loop as b l x) = scope o $ do
  as' <- traverse (resolveAttr EitherAttr) as
  b' <- resolveBlock b
  l' <- traverse resolveLbl l
  pure (Loop as' b' l' x)
resolveExprP _ _ m@(Match as e ar x) = scope m $ do
  as' <- traverse (resolveAttr EitherAttr) as
  e' <- resolveExprP 0 AnyExpr e
  ar' <- traverse resolveArm ar
  pure (Match as' e' ar' x)
resolveExprP _ _ c@(Catch as b x) = scope c $ do
  as' <- traverse (resolveAttr EitherAttr) as
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

resolveLbl :: Typeable a => Label a -> ResolveM (Label a)
resolveLbl l@(Label n _) = scope l (resolveIdent (mkIdent n) *> pure l)

-- | Wrap an expression in parens if the condition given holds
parenE :: (Typeable a, Monoid a) => Bool -> ResolveM (Expr a) -> ResolveM (Expr a)
parenE True e = ParenExpr [] <$> e <*> pure mempty
parenE False e = e

-- | A field just requires the identifier and expression to be valid
resolveField :: (Typeable a, Monoid a) => Field a -> ResolveM (Field a)
resolveField f@(Field i e x) = scope f $ do
  i' <- resolveIdent i
  e' <- traverse (resolveExpr AnyExpr) e
  pure (Field i' e' x)

instance (Typeable a, Monoid a) => Resolve (Field a) where resolveM = resolveField

-- | Arms are invalid only if the underlying consitutents are
resolveArm :: (Typeable a, Monoid a) => Arm a -> ResolveM (Arm a)
resolveArm a@(Arm as ps g b x) = scope a $ do
  as' <- traverse (resolveAttr OuterAttr) as
  ps' <- traverse resolvePat ps
  g' <- traverse (resolveExpr AnyExpr) g
  b' <- resolveExpr SemiExpr b
  pure (Arm as' ps' g' b' x)

instance (Typeable a, Monoid a) => Resolve (Arm a) where resolveM = resolveArm


----------------
-- Statements --
----------------

-- Invariants on statements
data StmtType
  = TermStmt -- ^ require a statement be terminated (so another statement can follow it)?
  | AnyStmt  -- ^ any statement

-- | Statements are invalid only when the underlying components are.
resolveStmt :: (Typeable a, Monoid a) => StmtType -> Stmt a -> ResolveM (Stmt a)
resolveStmt _ l@(Local p t i as x) = scope l $ do
  p' <- resolvePat p
  t' <- traverse (resolveTy AnyType) t
  i' <- traverse (resolveExpr AnyExpr) i
  as' <- traverse (resolveAttr OuterAttr) as
  pure (Local p' t' i' as' x)
resolveStmt _ s@(ItemStmt i x) = scope s (ItemStmt <$> resolveItem StmtItem i <*> pure x)
resolveStmt _ s@(Semi e x) | isBlockLike e = scope s (Semi <$> resolveExpr AnyExpr e <*> pure x)
resolveStmt _ s@(Semi e x) = scope s (Semi <$> resolveExpr SemiExpr e <*> pure x)
resolveStmt _ n@(NoSemi e x) | isBlockLike e = scope n (NoSemi <$> resolveExpr AnyExpr e <*> pure x)
resolveStmt AnyStmt  n@(NoSemi e x) = scope n (NoSemi <$> resolveExpr SemiExpr e <*> pure x)
resolveStmt TermStmt n@(NoSemi e x) = scope n (NoSemi <$> resolveExpr AnyExpr (BlockExpr [] (Block [NoSemi e mempty] Normal mempty) mempty) <*> pure x)
resolveStmt _ a@(MacStmt m s as x) = scope a $ do
  m' <- resolveMac ExprPath m
  as' <- traverse (resolveAttr OuterAttr) as
  pure (MacStmt m' s as' x)

instance (Typeable a, Monoid a) => Resolve (Stmt a) where resolveM = resolveStmt AnyStmt

-- | A block must a a series of terminated statements ended by one possibly unterminated one
resolveBlock :: (Typeable a, Monoid a) => Block a -> ResolveM (Block a)
resolveBlock b@(Block [] _ _) = pure b
resolveBlock b@(Block (s:ss) r x) = scope b $ do
  ss' <- traverse (resolveStmt TermStmt) (N.init (s :| ss))
  s' <- resolveStmt AnyStmt (N.last (s :| ss))
  pure (Block (ss' ++ [s']) r x)

instance (Typeable a, Monoid a) => Resolve (Block a) where resolveM = resolveBlock


-----------
-- Items --
-----------

-- Whether the item is a statement item, or a general item
data ItemType
  = StmtItem   -- ^ Corresponds to 'stmt_item' - basically limited visbility and no macros
  | ModItem    -- ^ General item

resolveVisibility' :: Typeable a => ItemType -> Visibility a -> ResolveM (Visibility a)
resolveVisibility' StmtItem PublicV = pure PublicV
resolveVisibility' StmtItem InheritedV = pure InheritedV
resolveVisibility' StmtItem v = scope v $ err v "statement items can only have public or inherited visibility"
resolveVisibility' ModItem v = pure v

-- | An item can be invalid if
--
--   * it is a macro but has 'StmtItem' restriction
--   * it has visibility other than public/inherited but has 'StmtItem' restriction
--   * an underlying component is invalid
--
resolveItem :: (Typeable a, Monoid a) => ItemType -> Item a -> ResolveM (Item a)
resolveItem t e@(ExternCrate as v i r x) = scope e $ do
  as' <- traverse (resolveAttr OuterAttr) as
  v' <- resolveVisibility' t v
  i' <- resolveIdent i
  r' <- traverse resolveIdent r
  pure (ExternCrate as' v' i' r' x)

resolveItem t u@(Use as v p x) = scope u $ do
  as' <- traverse (resolveAttr OuterAttr) as
  v' <- resolveVisibility' t v
  p' <- resolveUseTree p
  pure (Use as' v' p' x)

resolveItem t s@(Static as v i t' m e x) = scope s $ do
  as' <- traverse (resolveAttr OuterAttr) as
  v' <- resolveVisibility' t v
  i' <- resolveIdent i
  t'' <- resolveTy AnyType t'
  e' <- resolveExpr AnyExpr e
  pure (Static as' v' i' t'' m e' x)

resolveItem t c@(ConstItem as v i t' e x) = scope c $ do
  as' <- traverse (resolveAttr OuterAttr) as
  v' <- resolveVisibility' t v
  i' <- resolveIdent i
  t'' <- resolveTy AnyType t'
  e' <- resolveExpr AnyExpr e
  pure (ConstItem as' v' i' t'' e' x)

resolveItem t f@(Fn as v i d u c a g b x) = scope f $ do
  as' <- traverse (resolveAttr EitherAttr) as
  v' <- resolveVisibility' t v
  i' <- resolveIdent i
  d' <- resolveFnDecl NoSelf NamedArg d
  g' <- resolveGenerics g
  b' <- resolveBlock b
  pure (Fn as' v' i' d' u c a g' b' x)

resolveItem t m@(Mod as v i (Just is) x) = scope m $ do
  as' <- traverse (resolveAttr EitherAttr) as
  v' <- resolveVisibility' t v
  i' <- resolveIdent i
  is' <- traverse (resolveItem ModItem) is
  pure (Mod as' v' i' (Just is') x)

resolveItem t m@(Mod as v i Nothing x) = scope m $ do
  as' <- traverse (resolveAttr OuterAttr) as
  v' <- resolveVisibility' t v
  i' <- resolveIdent i
  pure (Mod as' v' i' Nothing x)

resolveItem t m@(ForeignMod as v a is x) = scope m $ do
  as' <- traverse (resolveAttr EitherAttr) as
  v' <- resolveVisibility' t v
  is' <- traverse (resolveForeignItem a) is
  pure (ForeignMod as' v' a is' x)

resolveItem t a@(TyAlias as v i t' g x) = scope a $ do
  as' <- traverse (resolveAttr OuterAttr) as
  v' <- resolveVisibility' t v
  i' <- resolveIdent i
  t'' <- resolveTy AnyType t'
  g' <- resolveGenerics g
  pure (TyAlias as' v' i' t'' g' x)

resolveItem t e@(Enum as v i vs g x) = scope e $ do
  as' <- traverse (resolveAttr OuterAttr) as
  v' <- resolveVisibility' t v
  i' <- resolveIdent i
  vs' <- traverse resolveVariant vs
  g' <- resolveGenerics g
  pure (Enum as' v' i' vs' g' x)
  
resolveItem t s@(StructItem as v i vd g x) = scope s $ do
  as' <- traverse (resolveAttr OuterAttr) as
  v' <- resolveVisibility' t v
  i' <- resolveIdent i
  vd' <- resolveVariantData vd
  g' <- resolveGenerics g
  pure (StructItem as' v' i' vd' g' x)

resolveItem t u@(Union as v i vd g x) = scope u $ do
  as' <- traverse (resolveAttr OuterAttr) as
  v' <- resolveVisibility' t v
  i' <- resolveIdent i
  vd' <- resolveVariantData vd
  g' <- resolveGenerics g
  pure (Union as' v' i' vd' g' x)

resolveItem t r@(Trait as v i a u g bd is x) = scope r $ do
  as' <- traverse (resolveAttr OuterAttr) as
  v' <- resolveVisibility' t v
  i' <- resolveIdent i
  g' <- resolveGenerics g
  bd' <- traverse (resolveTyParamBound NoneBound) bd
  is' <- traverse resolveTraitItem is
  pure (Trait as' v' i' a u g' bd' is' x)

resolveItem t r@(TraitAlias as v i g bd x) = scope r $ do
  as' <- traverse (resolveAttr OuterAttr) as
  v' <- resolveVisibility' t v
  i' <- resolveIdent i
  g' <- resolveGenerics g
  bd' <- traverse (resolveTyParamBound NoneBound) bd
  pure (TraitAlias as' v' i' g' bd' x)

resolveItem t i'@(Impl as v d u i g mt t' is x) = scope i' $ do
  as' <- traverse (resolveAttr EitherAttr) as
  v' <- resolveVisibility' t v
  g' <- resolveGenerics g
  mt' <- traverse resolveTraitRef mt
  t'' <- case mt of
           Nothing -> resolveTy PrimParenType t'
           Just _ -> resolveTy AnyType t'
  is' <- traverse resolveImplItem is
  pure (Impl as' v' d u i g' mt' t'' is' x)

resolveItem StmtItem m@MacItem{} = scope m (err m "macro items cannot be in statement items")
resolveItem _ a@(MacItem as i m x) = scope a $ do
  as' <- traverse (resolveAttr OuterAttr) as
  i' <- traverse resolveIdent i
  m' <- resolveMac ExprPath m
  pure (MacItem as' i' m' x)

resolveItem _ m@(MacroDef as i ts x) = scope m $ do
  as' <- traverse (resolveAttr OuterAttr) as
  i' <- resolveIdent i
  ts' <- resolveTokenStream ts
  pure (MacroDef as' i' ts' x)

instance (Typeable a, Monoid a) => Resolve (Item a) where resolveM = resolveItem ModItem

-- | A foreign item is invalid only if any of its underlying constituents are 
resolveForeignItem :: (Typeable a, Monoid a) => Abi -> ForeignItem a -> ResolveM (ForeignItem a)
resolveForeignItem a f@(ForeignFn as v i fn g x) = scope f $ do
  as' <- traverse (resolveAttr OuterAttr) as
  v' <- resolveVisibility v
  i' <- resolveIdent i
  fn' <- resolveFnDecl (case a of { C -> VarNoSelf; _ -> NoSelf }) NamedArg fn
  g' <- resolveGenerics g
  pure (ForeignFn as' v' i' fn' g' x)
resolveForeignItem _ f@(ForeignStatic as v i t m x) = scope f $ do
  as' <- traverse (resolveAttr OuterAttr) as
  v' <- resolveVisibility v
  i' <- resolveIdent i
  t' <- resolveTy AnyType t
  pure (ForeignStatic as' v' i' t' m x)
resolveForeignItem _ f@(ForeignTy as v i x) = scope f $ do
  as' <- traverse (resolveAttr OuterAttr) as
  v' <- resolveVisibility v
  i' <- resolveIdent i
  pure (ForeignTy as' v' i' x)

instance (Typeable a, Monoid a) => Resolve (ForeignItem a) where resolveM = resolveForeignItem C

-- | A where clause is valid only if the underlying predicates are
resolveWhereClause :: (Typeable a, Monoid a) => WhereClause a -> ResolveM (WhereClause a)
resolveWhereClause w@(WhereClause p x) = scope w (WhereClause <$> traverse resolveWherePredicate p <*> pure x)

instance (Typeable a, Monoid a) => Resolve (WhereClause a) where resolveM = resolveWhereClause

-- | Generics are only invalid if the underlying lifetimes or type parameters are
resolveGenerics :: (Typeable a, Monoid a) => Generics a -> ResolveM (Generics a)
resolveGenerics g@(Generics lts typ wc x) = scope g $ do
  lts' <- traverse resolveLifetimeDef lts
  typ' <- traverse resolveTyParam typ
  wc' <- resolveWhereClause wc
  pure (Generics lts' typ' wc' x)

instance (Typeable a, Monoid a) => Resolve (Generics a) where resolveM = resolveGenerics

-- | A type parameter is invalid only when the underlying components are
resolveTyParam :: (Typeable a, Monoid a) => TyParam a -> ResolveM (TyParam a)
resolveTyParam p@(TyParam as i bds t x) = scope p $ do
  as' <- traverse (resolveAttr OuterAttr) as
  i' <- resolveIdent i
  bds' <- traverse (resolveTyParamBound ModBound) bds
  t' <- traverse (resolveTy AnyType) t
  pure (TyParam as' i' bds' t' x)

instance (Typeable a, Monoid a) => Resolve (TyParam a) where resolveM = resolveTyParam

-- Invariants for struct fields
data StructFieldType
  = IdentStructField  -- ^ struct style field
  | BareStructField   -- ^ tuple-struct style field

-- | A variant is valid if the underlying components are
resolveVariant :: (Typeable a, Monoid a) => Variant a -> ResolveM (Variant a)
resolveVariant v@(Variant i as n e x) = scope v $ do
  i' <- resolveIdent i
  as' <- traverse (resolveAttr OuterAttr) as
  n' <- resolveVariantData n
  e' <- traverse (resolveExpr AnyExpr) e
  pure (Variant i' as' n' e' x)

instance (Typeable a, Monoid a) => Resolve (Variant a) where resolveM = resolveVariant

-- | A variant data is valid if the underlying components are
resolveVariantData :: (Typeable a, Monoid a) => VariantData a -> ResolveM (VariantData a)
resolveVariantData v@(StructD fs x) = scope v (StructD <$> traverse (resolveStructField IdentStructField) fs <*> pure x)
resolveVariantData v@(TupleD fs x) = scope v (TupleD <$> traverse (resolveStructField BareStructField) fs <*> pure x)
resolveVariantData   (UnitD x) = pure (UnitD x)

instance (Typeable a, Monoid a) => Resolve (VariantData a) where resolveM = resolveVariantData

-- | A struct field is invalid if
--
--   * it has the invariant that it needs an identifier, but it doesn't have one
--   * it has the invariant that should not have an identifier, but it doe have one
--   * any of the underlying components are invalid
--
resolveStructField :: (Typeable a, Monoid a) => StructFieldType -> StructField a -> ResolveM (StructField a)
resolveStructField IdentStructField s@(StructField Nothing _ _ _ _) = scope s $ err s "struct field needs an identifier"
resolveStructField IdentStructField s@(StructField (Just i) v t as x) = scope s $ do
  i' <- resolveIdent i
  v' <- resolveVisibility v
  t' <- resolveTy AnyType t
  as' <- traverse (resolveAttr OuterAttr) as
  pure (StructField (Just i') v' t' as' x)
resolveStructField BareStructField s@(StructField (Just _) _ _ _ _) = scope s $ err s "tuple-struct field cannot have an identifier"
resolveStructField BareStructField s@(StructField Nothing v t as x) = scope s $ do
  v' <- resolveVisibility v
  t' <- resolveTy AnyType t
  as' <- traverse (resolveAttr OuterAttr) as
  pure (StructField Nothing v' t' as' x)

instance (Typeable a, Monoid a) => Resolve (StructField a) where resolveM = resolveStructField IdentStructField

-- | A where predicate is invalid only if the underlying lifetimes are
resolveWherePredicate :: (Typeable a, Monoid a) => WherePredicate a -> ResolveM (WherePredicate a)
resolveWherePredicate p@(EqPredicate t1 t2 x) = scope p (EqPredicate <$> resolveTy NoForType t1 <*> resolveTy AnyType t2 <*> pure x)
resolveWherePredicate p@(RegionPredicate l ls x) = scope p $ do
  l' <- resolveLifetime l
  ls' <- traverse resolveLifetime ls
  pure (RegionPredicate l' ls' x)
resolveWherePredicate p@(BoundPredicate lts t bds x) = scope p $ do
  lts' <- traverse resolveLifetimeDef lts
  t' <- resolveTy NoForType t
  bds' <- traverse (resolveTyParamBound ModBound) bds
  pure (BoundPredicate lts' t' bds' x)

instance (Typeable a, Monoid a) => Resolve (WherePredicate a) where resolveM = resolveWherePredicate

-- | A trait item is valid if the underlying components are
resolveTraitItem :: (Typeable a, Monoid a) => TraitItem a -> ResolveM (TraitItem a)
resolveTraitItem n@(ConstT as i t e x) = scope n $ do
  as' <- traverse (resolveAttr OuterAttr) as
  i' <- resolveIdent i
  t' <- resolveTy AnyType t
  e' <- traverse (resolveExpr AnyExpr) e
  pure (ConstT as' i' t' e' x)
resolveTraitItem n@(MethodT as i g m b x) = scope n $ do
  as' <- traverse (resolveAttr OuterAttr) as
  i' <- resolveIdent i
  g' <- resolveGenerics g
  m' <- resolveMethodSig GeneralArg m 
  b' <- traverse resolveBlock b
  pure (MethodT as' i' g' m' b' x)
resolveTraitItem n@(TypeT as i bd t x) = scope n $ do
  as' <- traverse (resolveAttr OuterAttr) as
  i' <- resolveIdent i
  bd' <- traverse (resolveTyParamBound ModBound) bd
  t' <- traverse (resolveTy AnyType) t
  pure (TypeT as' i' bd' t' x)
resolveTraitItem n@(MacroT as m x) = scope n $ do
  as' <- traverse (resolveAttr OuterAttr) as
  m' <- resolveMac ModPath m
  pure (MacroT as' m' x)

instance (Typeable a, Monoid a) => Resolve (TraitItem a) where resolveM = resolveTraitItem

-- | An impl item is valid if the underlying components are
resolveImplItem :: (Typeable a, Monoid a) => ImplItem a -> ResolveM (ImplItem a)
resolveImplItem n@(ConstI as v d i t e x) = scope n $ do
  as' <- traverse (resolveAttr OuterAttr) as
  v' <- resolveVisibility v
  i' <- resolveIdent i
  t' <- resolveTy AnyType t
  e' <- resolveExpr AnyExpr e
  pure (ConstI as' v' d i' t' e' x)
resolveImplItem n@(MethodI as v d i g m b x) = scope n $ do
  as' <- traverse (resolveAttr EitherAttr) as
  v' <- resolveVisibility v
  i' <- resolveIdent i
  g' <- resolveGenerics g
  m' <- resolveMethodSig NamedArg m 
  b' <- resolveBlock b
  pure (MethodI as' v' d i' g' m' b' x)
resolveImplItem n@(TypeI as v d i t x) = scope n $ do
  as' <- traverse (resolveAttr OuterAttr) as
  v' <- resolveVisibility v
  i' <- resolveIdent i
  t' <- resolveTy AnyType t
  pure (TypeI as' v' d i' t' x)
resolveImplItem n@(MacroI as d m x) = scope n $ do
  as' <- traverse (resolveAttr OuterAttr) as
  m' <- resolveMac ModPath m
  pure (MacroI as' d m' x)

instance (Typeable a, Monoid a) => Resolve (ImplItem a) where resolveM = resolveImplItem

-- | The 'Monoid' constraint is theoretically not necessary - restricted visibility paths are mod paths,
-- so they should never have generics.
resolveVisibility :: (Typeable a, Monoid a) => Visibility a -> ResolveM (Visibility a)
resolveVisibility PublicV = pure PublicV
resolveVisibility InheritedV = pure InheritedV
resolveVisibility CrateV = pure CrateV
resolveVisibility v@(RestrictedV p) = scope v (RestrictedV <$> resolvePath ModPath p)

instance (Typeable a, Monoid a) => Resolve (Visibility a) where resolveM = resolveVisibility

-- | A method signature is valid if the underlying components are
resolveMethodSig :: (Typeable a, Monoid a) => ArgType -> MethodSig a -> ResolveM (MethodSig a)
resolveMethodSig at m@(MethodSig u c a f) = scope m (MethodSig u c a <$> resolveFnDecl AllowSelf at f)

instance (Typeable a, Monoid a) => Resolve (MethodSig a) where resolveM = resolveMethodSig NamedArg

-- | A view path is valid if the underlying components are
resolveUseTree :: (Typeable a, Monoid a) => UseTree a -> ResolveM (UseTree a)
resolveUseTree v@(UseTreeSimple p i x) = scope v $ do
  p' <- resolvePath ModPath p 
  i' <- traverse resolveIdent i
  pure (UseTreeSimple p' i' x)
resolveUseTree v@(UseTreeGlob p x) = scope v $ do
  p' <- resolvePath ModPath p 
  pure (UseTreeGlob p' x) 
resolveUseTree v@(UseTreeNested p ns x) = scope v $ do
  p' <- resolvePath ModPath p
  ns' <- traverse resolveUseTree ns
  pure (UseTreeNested p' ns' x)

instance (Typeable a, Monoid a) => Resolve (UseTree a) where resolveM = resolveUseTree


-------------------
-- Macro related --
-------------------

-- | A macro call is only invalid if any of the underlying components are
resolveMac :: (Typeable a, Monoid a) => PathType -> Mac a -> ResolveM (Mac a)
resolveMac t m@(Mac p ts x) = scope m (Mac <$> resolvePath t p <*> resolveTokenStream ts <*> pure x)

instance (Typeable a, Monoid a) => Resolve (Mac a) where
  resolveM m@(Mac p ts x) = scope m (Mac <$> resolveM p <*> resolveTokenStream ts <*> pure x) 

-- | A token tree is invalid when
--
--   * there is an open or close delim token (those should be balanced and in 'Delimited')
--   * the underlying token trees are invalid
--
resolveTt :: TokenTree -> ResolveM TokenTree
resolveTt t@(Token _ (OpenDelim _)) = scope t (err t "open delimiter is not allowed as a token in a token tree")
resolveTt t@(Token _ (CloseDelim _)) = scope t (err t "close delimiter is not allowed as a token in a token tree")
resolveTt t@Token{} = pure t
resolveTt t@(Delimited s d ts) = scope t (Delimited s d <$> resolveTokenStream ts)

instance Resolve TokenTree where resolveM = resolveTt

resolveTokenStream :: TokenStream -> ResolveM TokenStream
resolveTokenStream s@(Tree tt) = scope s (Tree <$> resolveTt tt)
resolveTokenStream s@(Stream ts) = scope s (Stream <$> mapM resolveTokenStream ts)

instance Resolve TokenStream where resolveM = resolveTokenStream

