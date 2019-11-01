{-# LANGUAGE OverloadedStrings, OverloadedLists, InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Diff where

import Data.Aeson
import Language.Rust.Data.Ident
import Language.Rust.Syntax
import Language.Rust.Parser

import Control.Monad (when, unless)

import Text.Read (readMaybe)
import Data.String (fromString)
import Data.Maybe (fromMaybe, isNothing)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.List.NonEmpty as N

import DiffUtils

-- TODO: Check spans too

instance Show a => Diffable (SourceFile a) where
  SourceFile _ as is === val = do
    as === (val ! "attrs")
    is === (val ! "module" ! "items")

instance Show a => Diffable (Item a) where
  item === val = do
    let n' = val ! "node"
        inheritedV = InheritedV :: Visibility ()
    case (n' ! "variant", item) of
      ("Fn", Fn as v i decl hdr gen bod _) -> do
        as   === (val ! "attrs")
        v    === (val ! "vis")
        i    === (val ! "ident")
        decl === (n' ! "fields" ! 0)
        hdr  === (n' ! "fields" ! 1)
        gen  === (n' ! "fields" ! 2)
        bod  === (n' ! "fields" ! 3)
      ("ExternCrate", ExternCrate as v i i' _) -> do
        as === (val ! "attrs")
        v === (val ! "vis")
        i === (val ! "ident")
        maybe (pure ()) (=== (n' ! "fields" ! 0)) i'
      ("Use", Use as v v' _) -> do
        as === (val ! "attrs")
        v === (val ! "vis")
        mkIdent "" === (val ! "ident")
        v' === (n' ! "fields" ! 0)
      ("Static", Static as v i t m e _) -> do
        as === (val ! "attrs")
        v === (val ! "vis")
        i === (val ! "ident")
        t === (n' ! "fields" ! 0)
        m === (n' ! "fields" ! 1)
        e === (n' ! "fields" ! 2)
      ("Const", ConstItem as v i t e _) -> do
        as === (val ! "attrs")
        v === (val ! "vis")
        i === (val ! "ident")
        t === (n' ! "fields" ! 0)
        e === (n' ! "fields" ! 1)
      ("Mod", Mod as v i is _) -> do
        as === (val ! "attrs")
        v === (val ! "vis")
        i === (val ! "ident")
        case is of
          Nothing -> pure () -- TODO: should look at external files?
          Just is' -> is' === (n' ! "fields" ! 0 ! "items")
      ("ForeignMod", ForeignMod as v a is _) -> do
        as === (val ! "attrs")
        v === (val ! "vis")
        mkIdent "" === (val ! "ident")
        a === (n' ! "fields" ! 0 ! "abi")
        is === (n' ! "fields" ! 0 ! "items")
      ("TyAlias", TyAlias as v i t g _) -> do
        as === (val ! "attrs")
        v === (val ! "vis")
        i === (val ! "ident")
        t === (n' ! "fields" ! 0)
        g === (n' ! "fields" ! 1)
      ("OpaqueTy", TyAlias as v i (ImplTrait b _) g _) -> do
        as === (val ! "attrs")
        v === (val ! "vis")
        i === (val ! "ident")
        b === (n' ! "fields" ! 0)
        g === (n' ! "fields" ! 1)
      ("Enum", Enum as v i vs g _) -> do
        as === (val ! "attrs")
        v === (val ! "vis")
        i === (val ! "ident")
        vs === (n' ! "fields" ! 0 ! "variants")
        g === (n' ! "fields" ! 1)
      ("Struct", StructItem as v i v' g _) ->  do
        as === (val ! "attrs")
        v === (val ! "vis")
        i === (val ! "ident")
        v' === (n' ! "fields" ! 0)
        g === (n' ! "fields" ! 1)
      ("Union", Union as v i v' g _) ->  do
        as === (val ! "attrs")
        v === (val ! "vis")
        i === (val ! "ident")
        v' === (n' ! "fields" ! 0)
        g === (n' ! "fields" ! 1)
      ("Trait", Trait as v i a u g bd is _) -> do
        as === (val ! "attrs")
        v === (val ! "vis")
        i === (val ! "ident")
        IsAuto a === (n' ! "fields" ! 0)
        u === (n' ! "fields" ! 1)
        g === (n' ! "fields" ! 2)
        bd === (n' ! "fields" ! 3)
        is === (n' ! "fields" ! 4)
      ("TraitAlias", TraitAlias as v i g bd _) -> do
        as === (val ! "attrs")
        v === (val ! "vis")
        i === (val ! "ident")
        g === (n' ! "fields" ! 0)
        bd === (n' ! "fields" ! 1)
      ("Impl", Impl as v d u p g mtr t is _) -> do
        as === (val ! "attrs")
        v === (val ! "vis")
        mkIdent "" === (val ! "ident")
        u === (n' ! "fields" ! 0)
        p === (n' ! "fields" ! 1)
        d === (n' ! "fields" ! 2)
        g === (n' ! "fields" ! 3)
        mtr === (n' ! "fields" ! 4)
        t === (n' ! "fields" ! 5)
        is === (n' ! "fields" ! 6)
      ("Mac", MacItem as m _) -> do
        as === (val ! "attrs")
        inheritedV === (val ! "vis")
        m === (n' ! "fields" ! 0)
      ("MacroDef", MacroDef as v i tt _) -> do
        as === (val ! "attrs")
        v === (val ! "vis")
        i === (val ! "ident")
        tt === (n' ! "fields" ! 0 ! "tokens")
      _ -> diff "different items" item val

newtype IsAuto = IsAuto Bool deriving (Show, Eq)

instance Diffable IsAuto where
  IsAuto True === "Yes" = pure ()
  IsAuto False === "No" = pure ()
  item === val = diff "different auto" item val

instance Diffable ImplPolarity where
  Positive === "Positive" = pure ()
  Negative === "Negative" = pure ()
  p        === val        = diff "different polarity" p val

instance Show a => Diffable (TraitItem a) where
  item === val = do
    let n' = val ! "node"
    case (n' ! "variant", item) of
      ("Const", ConstT as i t me _) -> do
        as === (val ! "attrs")
        i === (val ! "ident")
        t === (n' ! "fields" ! 0)
        me ===(n' ! "fields" ! 1)
      ("Method", MethodT as i g m mb _) -> do
        as === (val ! "attrs")
        i === (val ! "ident")
        g === (val ! "generics")
        m === (n' ! "fields" ! 0)
        mb === (n' ! "fields" ! 1)
      ("Type", TypeT as i bd mt _) -> do
        as === (val ! "attrs")
        i === (val ! "ident")
        bd === (n' ! "fields" ! 0)
        mt === (n' ! "fields" ! 1)
      ("Macro", MacroT as m _) -> do
        as === (val ! "attrs")
        m === (n' ! "fields" ! 0)
      _ -> diff "different trait item" item val

instance Show a => Diffable (ImplItem a) where
  item === val = do
    let n' = val ! "node"
    case (n' ! "variant", item) of
      ("Const", ConstI as v d i t e _) -> do
        as === (val ! "attrs")
        v === (val ! "vis")
        d === (val ! "defaultness")
        i === (val ! "ident")
        t === (n' ! "fields" ! 0)
        e ===(n' ! "fields" ! 1)
      ("Method", MethodI as v d i g m b _) -> do
        as === (val ! "attrs")
        v === (val ! "vis")
        d === (val ! "defaultness")
        i === (val ! "ident")
        g === (val ! "generics")
        m === (n' ! "fields" ! 0)
        b === (n' ! "fields" ! 1)
      ("TyAlias", TypeI as v d i t _) -> do
        as === (val ! "attrs")
        v === (val ! "vis")
        d === (val ! "defaultness")
        i === (val ! "ident")
        t === (n' ! "fields" ! 0)
      ("OpaqueTy", TypeI as v d i (ImplTrait b _) _) -> do
        as === (val ! "attrs")
        v === (val ! "vis")
        d === (val ! "defaultness")
        i === (val ! "ident")
        b === (n' ! "fields" ! 0)
      ("Macro", MacroI as d m _) -> do
        as === (val ! "attrs")
        d === (val ! "defaultness")
        m === (n' ! "fields" ! 0)
      _ -> diff "different impl item" item val

instance Show a => Diffable (MethodSig a) where
  MethodSig header decl === val = do
    header === (val ! "header")
    decl === (val ! "decl")

instance Show a => Diffable (FnHeader a) where
  FnHeader u s c a _ === val = do
    u === (val ! "unsafety")
    s === (val ! "asyncness" ! "node")
    a === (val ! "abi")
    c === (val ! "constness")


instance Diffable Defaultness where
  Final   === "Final" = pure ()
  Default === "Default" = pure ()
  d       === val = diff "different defaultness" d val

instance Show a => Diffable (Variant a) where
  Variant i as d e _ === val = do
    i === (val ! "ident")
    as === (val ! "attrs")
    d === (val ! "data")
    fmap AnonConst e === (val ! "disr_expr")

instance Show a => Diffable (VariantData a) where
  d === val =
    case (val ! "variant", d) of
      ("Tuple", TupleD sf _) -> sf === (val ! "fields" ! 0)
      ("Struct", StructD sf _) -> sf === (val ! "fields" ! 0)
      ("Unit", UnitD _) -> pure ()
      _ -> diff "different variants" d val

instance Show a => Diffable (StructField a) where
  StructField i v t as _ === val = do
    i === (val ! "ident")
    v === (val ! "vis")
    t === (val ! "ty")
    as === (val ! "attrs")

instance Show a => Diffable (ForeignItem a) where
  f === val = do
    case (val ! "node", f) of
      ("Ty", ForeignTy as v i _) -> do
        as === (val ! "attrs")
        v === (val ! "vis")
        i === (val ! "ident")
      (n', _) -> case (n' ! "variant", f) of
        ("Fn", ForeignFn as v i d g _) -> do
          as === (val ! "attrs")
          v === (val ! "vis")
          i === (val ! "ident")
          d === (n' ! "fields" ! 0)
          g === (n' ! "fields" ! 1)
        ("Static", ForeignStatic as v i t m _) -> do
          as === (val ! "attrs")
          v === (val ! "vis")
          i === (val ! "ident")
          t === (n' ! "fields" ! 0)
          m === (n' ! "fields" ! 1)
        ("Macro", ForeignMac as m _) -> do
          as === (val ! "attrs")
          m === (n' ! "fields" ! 0)
        _ -> diff "different foreign item" f val

instance Show a => Diffable (UseTree a) where
  v === val = do
    case (val ! "kind", v) of
      ("Glob", UseTreeGlob p _) -> p === (val ! "prefix")
      (n', _) -> case (n' ! "variant", v) of
        ("Simple", UseTreeSimple p i _) -> do
          p === (val ! "prefix")
          i === (n' ! "fields" ! 0)
        ("Nested", UseTreeNested p ns  _) -> do
          p === (val ! "prefix")
          map UseTreePair ns === (n' ! "fields" ! 0)
        _ -> diff "different view path" v val

newtype UseTreePair a = UseTreePair (UseTree a)

instance Show (UseTreePair a) where
  show _ = "Some use tree"

instance Show a => Diffable (UseTreePair a) where
  UseTreePair ut === val = ut === (val ! 0)

instance Show a => Diffable (FnDecl a) where
  FnDecl as out variadic _ === val = do
    -- Check inputs
    -- Rust tacks on an extra bogus argument representing the "...". We don't.
    let adjustedIns = case val ! "inputs" of
          Data.Aeson.Array arr | variadic -> Data.Aeson.Array (V.init arr)
          other -> other
    as === adjustedIns

    -- Check output
    let outTy = val ! "output" ! "variant"
    case (outTy, out) of
      ("Default", Nothing) -> pure ()
      ("Ty", Just t) -> t === (val ! "output" ! "fields" ! 0)
      _ -> diff "different output types" out outTy

    -- Check variadic
    variadic === (val ! "c_variadic")

instance Show a => Diffable (Arg a) where
  SelfRegion as _ _ x === val = do
    NullList as === (val ! "attrs" ! "_field0")
    IdentP (ByValue Immutable) "self" Nothing x === (val ! "pat")
  SelfValue as m x === val = do
    NullList as === (val ! "attrs" ! "_field0")
    IdentP (ByValue m) "self" Nothing x === (val ! "pat")
  SelfExplicit as t m x === val = do
    NullList as === (val ! "attrs" ! "_field0")
    IdentP (ByValue m) "self" Nothing x === (val ! "pat")
    t === (val ! "ty")
  Arg as p t _ === val = do
    NullList as === (val ! "attrs" ! "_field0")
    let p' = fromMaybe (IdentP (ByValue Immutable) invalidIdent Nothing undefined) p
    p' === (val ! "pat")
    t === (val ! "ty")

instance Diffable Unsafety where
  Unsafe === "Unsafe" = pure ()
  Unsafe === val | val ! "variant" == "Unsafe" = pure ()
  Normal === "Normal" = pure ()
  Normal === "Default" = pure ()
  unsfty === val = diff "different safety" unsfty val

instance Diffable Constness where
  c === val = case (val ! "node", c) of
                ("Const", Const) -> pure ()
                ("NotConst", NotConst) -> pure ()
                _ -> diff "different constness" c val

instance Diffable Abi where
  Cdecl             === "Cdecl"             = pure ()
  Stdcall           === "Stdcall"           = pure ()
  Fastcall          === "Fastcall"          = pure ()
  Thiscall          === "Thiscall"          = pure ()
  Vectorcall        === "Vectorcall"        = pure ()
  Aapcs             === "Aapcs"             = pure ()
  Win64             === "Win64"             = pure ()
  SysV64            === "SysV64"            = pure ()
  PtxKernel         === "PtxKernel"         = pure ()
  Msp430Interrupt   === "Msp430Interrupt"   = pure ()
  X86Interrupt      === "X86Interrupt"      = pure ()
  AmdGpuKernel      === "AmdGpuKernel"      = pure ()
  Rust              === "Rust"              = pure ()
  C                 === "C"                 = pure ()
  System            === "System"            = pure ()
  RustIntrinsic     === "RustIntrinsic"     = pure ()
  RustCall          === "RustCall"          = pure ()
  PlatformIntrinsic === "PlatformIntrinsic" = pure ()
  Unadjusted        === "Unadjusted"        = pure ()
  a                 === val                 = diff "different ABI" a val

instance Show a => Diffable (Generics a) where
  Generics params whr _ === val = do
    params === (val ! "params")
    whr === (val ! "where_clause")

instance Show a => Diffable (GenericParam a) where
  param === val =
    case (val ! "kind", param) of
      ("Lifetime", LifetimeParam as l bd _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        l === val
        bd === (val ! "bounds")
      (n, _) ->
        case (n ! "variant", param) of
          ("Type", TypeParam as i bd d _) -> do
            NullList as === (val ! "attrs" ! "_field0")
            i === (val ! "ident")
            bd === (val ! "bounds")
            d === (val ! "kind" ! "fields" ! 0)
          ("Const", ConstParam as i t _) -> do
            NullList as === (val ! "attrs" ! "_field0")
            i === (val ! "ident")
            t === (val ! "kind" ! "fields" ! 0)
          _ -> diff "different generic parameter" param val

instance Show a => Diffable (WhereClause a) where
  WhereClause preds _ === val = preds === (val ! "predicates")

instance Show a => Diffable (WherePredicate a) where
  w === val =
    case (val ! "variant", w) of
      ("BoundPredicate", BoundPredicate ls t bds _) -> do
        ls === (val ! "fields" ! 0 ! "bound_generic_params")
        t === (val ! "fields" ! 0 ! "bounded_ty")
        bds === (val ! "fields" ! 0 ! "bounds")
      ("RegionPredicate", RegionPredicate l bs _) -> do
        l === (val ! "fields" ! 0 ! "lifetime")
        map (\b@(Lifetime _ x) -> OutlivesBound b x) bs === (val ! "fields" ! 0 ! "bounds")
      ("EqPredicate", EqPredicate l r _) -> do
        l === (val ! "fields" ! 0 ! "lhs_ty")
        r === (val ! "fields" ! 0 ! "rhs_ty")
      _ -> diff "different where predicate" w val

instance Show a => Diffable (Block a) where
  Block ss ru _ === val = do
    massageStandaloneSemis ss === (val ! "stmts")
    ru === (val ! "rules")

massageStandaloneSemis :: [Stmt a] -> [Stmt a]
massageStandaloneSemis [] = []
massageStandaloneSemis (StandaloneSemi s : rest) = semiTup s : massageStandaloneSemis (dropWhile isStandalone rest)
  where
    isStandalone StandaloneSemi{} = True
    isStandalone _ = False
    semiTup x = Semi (TupExpr [] [] x) x
massageStandaloneSemis (s : rest) = s : massageStandaloneSemis rest

-- TODO: Rust parses out extra semicolons
instance Show a => Diffable (Stmt a) where
  stmt === val = do
    let n' = val ! "node"
    case (n' ! "variant", stmt) of
      ("Local", Local p t i as _) -> do
        p === (n' ! "fields" ! 0 ! "pat")
        t === (n' ! "fields" ! 0 ! "ty")
        i === (n' ! "fields" ! 0 ! "init")
        NullList as === (n' ! "fields" ! 0 ! "attrs" ! "_field0")
      ("Expr", NoSemi e _)   -> e === (n' ! "fields" ! 0)
      ("Semi", Semi e _)     -> e === (n' ! "fields" ! 0)
      ("Item", ItemStmt i _) -> i === (n' ! "fields" ! 0)
      ("Mac", MacStmt m s as _) -> do
        m === (n' ! "fields" ! 0 ! 0)
        s === (n' ! "fields" ! 0 ! 1)
        NullList as === (n' ! "fields" ! 0 ! 2 ! "_field0")
      _ -> diff "different statements" stmt val

instance Diffable MacStmtStyle where
  SemicolonMac === "Semicolon" = pure ()
  BracesMac    === "Braces"    = pure ()
  sty          === val         = diff "different styles" sty val

instance Show a => Diffable (Visibility a) where
  a === val =
    case (a, val ! "node") of
      (PublicV, "Public") -> pure ()
      (CrateV, val'@Object{}) -> mkIdent "Crate" === (val' ! "variant")
      (CrateV, "Crate") -> pure ()
      (RestrictedV p, val') -> do
        mkIdent "Restricted" === (val' ! "variant")
        p === (val' ! "fields" ! 0)
      (InheritedV, "Inherited") -> pure ()
      (_, val') -> diff "different visibilities" a val'

instance Show a => Diffable (Attribute a) where
  a === val =
    case (a, val ! "is_sugared_doc") of
      (SugaredDoc sty inl n _, Data.Aeson.Bool True) -> do
        case (val ! "style", sty) of
          ("Inner", Inner) -> pure ()
          ("Outer", Outer) -> pure ()
          _ -> diff "doc attributes have different styles" a val
        -- Content
        let toks = val ! "tokens"
        Token mempty Equal === (toks ! 0)
        let str = toks ! 1
        mkIdent "Literal" === (str ! "fields" ! 0 ! "kind" ! "variant")
        mkIdent "Str" ===  (str ! "fields" ! 0 ! "kind" ! "fields" ! 0 ! "kind")
        let Data.Aeson.String n' = str ! "fields" ! 0 ! "kind" ! "fields" ! 0 ! "symbol"
            Str n'' Cooked Unsuffixed () = translateLit (StrTok (T.unpack n')) Unsuffixed ()
        -- Style
        case (val ! "style", sty, inl) of
          ("Inner", Inner, False) | ("//!" <> n)         == n'' -> pure ()
          ("Inner", Inner, True)  | ("/*!" <> n <> "*/") == n'' -> pure ()
          ("Outer", Outer, False) | ("///" <> n)         == n'' -> pure ()
          ("Outer", Outer, True)  | ("/**" <> n <> "*/") == n'' -> pure ()
          _ -> diff "doc attributes are different" a val
      (Attribute sty p ts _, Data.Aeson.Bool False) -> do
        case (val ! "style", sty) of
          ("Inner", Inner) -> pure ()
          ("Outer", Outer) -> pure ()
          _ -> diff "attribute style is different" a val
        p === (val ! "path")
        ts === (val ! "tokens")
      _ -> diff "attribute is not sugared doc" a val

-- Temporary hack until Rust gets their AST in order...
newtype OrPatHack a = OrPatHack (Pat a) deriving Show

variants :: Pat a -> N.NonEmpty (Pat a)
variants (OrP ps _) = ps
variants notOrPat = [notOrPat]

instance Show a => Diffable (OrPatHack a) where
  OrPatHack p === val = variants p === val

instance Show a => Diffable (Pat a) where
  p' === val = do
    let val' = val ! "node"
    case (val', p') of
      ("Wild", WildP _) -> pure ()
      ("Rest", RestP _) -> pure ()
      (Data.Aeson.Object{}, _) ->
        case (val' ! "variant", p') of
          ("Box", BoxP p _) -> p === (val' ! "fields" ! 0)
          ("Paren", ParenP p _) -> p === (val' ! "fields" ! 0)
          ("Lit", LitP e _) -> e === (val' ! "fields" ! 0)
          ("Mac", MacP m _) -> m === (val' ! "fields" ! 0)
          ("Ident", IdentP bm i m _) -> do
            bm === (val' ! "fields" ! 0)
            i === (val' ! "fields" ! 1)
            m === (val' ! "fields" ! 2)
          ("Ref", RefP p m _) -> do
            p === (val' ! "fields" ! 0)
            m === (val' ! "fields" ! 1)
          ("Struct", StructP p fp d _) -> do
            p ===  (val' ! "fields" ! 0)
            fp === (val' ! "fields" ! 1)
            d === (val' ! "fields" ! 2)
          ("TupleStruct", TupleStructP p fp _) -> do
            p  === (val' ! "fields" ! 0)
            fp === (val' ! "fields" ! 1)
          ("Tuple", TupleP fp _) -> do
            fp === (val' ! "fields" ! 0)
          ("Or", OrP ps _) -> do
            ps === (val' ! "fields" ! 0)
          ("Range", RangeP e1 e2 rl _) -> do
            e1 === (val' ! "fields" ! 0)
            e2 === (val' ! "fields" ! 1)
            rl === (val' ! "fields" ! 2)
          ("Slice", SliceP a _) -> do
            a === (val' ! "fields" ! 0)
          ("Path", PathP q p _) -> do
            q === (val' ! "fields" ! 0)
            p === (val' ! "fields" ! 1)
          _ -> diff "differing patterns" p' val
      _ -> diff "differing patterns" p' val

instance Show a => Diffable (Mac a) where
  Mac p tt _ === val = do
    p === (val ! "path")
    tt === (val ! "tts")

instance Diffable TokenTree where
  tt === val =
    case (val ! "variant", tt) of
      ("Token", Token _ t) -> t === (val ! "fields" ! 0 ! "kind")
      ("Delimited", Delimited _ d tt') -> do
        d === (val ! "fields" ! 1)
        tt' === (val ! "fields" ! 2)
      _ -> diff "different token trees" tt val

instance Diffable TokenStream where
  ts === val = flatten ts === val
    where
    flatten (Tree t) = [t]
    flatten (Stream s) = concatMap flatten s

instance Diffable Delim where
  Paren   === "Paren"   = pure ()
  Bracket === "Bracket" = pure ()
  Brace   === "Brace"   = pure ()
  del     === val      = diff "different delimiters" del val

instance Diffable Token where
  Comma === "Comma" = pure ()
  Dot === "Dot" = pure ()
  Equal === "Eq" = pure ()
  NotEqual === "Ne" = pure ()
  Colon === "Colon" = pure ()
  ModSep === "ModSep" = pure ()
  Less === "Lt" = pure ()
  LessEqual === "Le" = pure ()
  Greater === "Gt" = pure ()
  GreaterEqual === "Ge" = pure ()
  Exclamation === "Not" = pure ()
  Dollar === "Dollar" = pure ()
  FatArrow === "FatArrow" = pure ()
  Semicolon === "Semi" = pure ()
  Tilde === "Tilde" = pure ()
  EqualEqual === "EqEq" = pure ()
  At === "At" = pure ()
  Pound === "Pound" = pure ()
  PipePipe === "OrOr" = pure ()
  DotDot === "DotDot" = pure ()
  DotDotDot === "DotDotDot" = pure ()
  DotDotEqual === "DotDotEq" = pure ()
  RArrow === "RArrow" = pure ()
  LArrow === "LArrow" = pure ()
  Question === "Question" = pure ()
  AmpersandAmpersand === "AndAnd" = pure ()
  t === val@Object{} =
    case (val ! "variant", t) of
      ("Ident", IdentTok i) -> i === (val ! "fields" ! 0)
      ("Lifetime", LifetimeTok l) -> ("'" <> l) === (val ! "fields" ! 0)
      ("DocComment", Doc s Inner False) -> ("//!" <> mkIdent s) === (val ! "fields" ! 0)
      ("DocComment", Doc s Outer False) -> ("///" <> mkIdent s) === (val ! "fields" ! 0)
      ("DocComment", Doc s Inner True) -> ("/*!" <> mkIdent s <> "*/") === (val ! "fields" ! 0)
      ("DocComment", Doc s Outer True) -> ("/**" <> mkIdent s <> "*/") === (val ! "fields" ! 0)
      ("Literal", LiteralTok l s) -> do
        l === (val ! "fields" ! 0)
        fmap mkIdent s === (val ! "fields" ! 0 ! "suffix")
      ("BinOp", _) ->
        case (val ! "fields" ! 0, t) of
          ("Star", Star) -> pure ()
          ("Plus", Plus) -> pure ()
          ("Minus", Minus) -> pure ()
          ("Slash", Slash) -> pure ()
          ("Percent", Percent) -> pure ()
          ("And", Ampersand) -> pure ()
          ("Caret", Caret) -> pure ()
          ("Or", Pipe) -> pure ()
          ("Shr", GreaterGreater) -> pure ()
          ("Shl", LessLess) -> pure ()
          _ -> diff "differing binary operator tokens" t val
      ("BinOpEq", _) ->
        case (val ! "fields" ! 0, t) of
          ("Shr", GreaterGreaterEqual) -> pure ()
          ("Shl", LessLessEqual) -> pure ()
          ("Gt", GreaterEqual) -> pure ()
          ("Lt", LessEqual) -> pure ()
          ("Minus", MinusEqual) -> pure ()
          ("And", AmpersandEqual) -> pure ()
          ("Or", PipeEqual) -> pure ()
          ("Plus", PlusEqual) -> pure ()
          ("Star", StarEqual) -> pure ()
          ("Slash", SlashEqual) -> pure ()
          ("Caret", CaretEqual) -> pure ()
          ("Percent", PercentEqual) -> pure ()
          _ -> diff "differing binary-equal operator tokens" t val
      _ -> diff "differing tokens" t val
  t === val = diff "differing tokens" t val

instance Diffable LitTok where
  l === val =
    case (val ! "kind", l) of
      ("Byte", ByteTok s)             | fromString s == (val ! "symbol") -> pure ()
      ("Char", CharTok s)             | fromString s == (val ! "symbol") -> pure ()
      ("Integer", IntegerTok s)       | fromString s == (val ! "symbol") -> pure ()
      ("Float", FloatTok s)           | fromString s == (val ! "symbol") -> pure ()
      ("Str", StrTok s)               | fromString s == clean (val ! "symbol") -> pure ()
      ("ByteStr", ByteStrTok s)       | fromString s == clean (val ! "symbol") -> pure ()
      (strRaw, StrRawTok s i)         | strRaw !? "variant" == Just "StrRaw"
                                      , fromString s == clean (val ! "symbol")
                                      -> i === (strRaw ! "fields" ! 0)
      (byteStrRaw, ByteStrRawTok s i) | byteStrRaw !? "variant" == Just "ByteStrRaw"
                                      , fromString s == clean (val ! "symbol")
                                      -> i === (byteStrRaw ! "fields" ! 0)
      _ -> diff "different literal token" l val

clean :: Value -> Value
clean x =
  case x of
    String s -> String (T.replace "\r\n" "\n" s)
    _        -> x

instance Show a => Diffable (FieldPat a) where
  f@(FieldPat mi p as _) === val = do
    -- Extract the identifier and whether the pattern is shorthand
    (i, s) <- case (mi,p) of
                (Nothing, BoxP (IdentP _ i' Nothing _) _) -> pure (i', True)
                (Nothing, IdentP _ i' Nothing _) -> pure (i', True)
                (Nothing, _) -> diff "shorthand field pattern needs to be identifier" f val
                (Just i', _) -> pure (i', False)

    i === (val ! "ident")
    s === (val ! "is_shorthand")
    p === (val ! "pat")
    NullList as === (val ! "attrs" ! "_field0")

instance Show a => Diffable (Field a) where
  Field i me as _ === val = do
    isNothing me === (val ! "is_shorthand")
    i === (val ! "ident")
    unless (isNothing me) $
      me === (val ! "expr")
    NullList as === (val ! "attrs" ! "_field0")

instance Diffable Ident where
  Ident i _ _ === String s | fromString i == s = pure ()
  Ident i _ _ === val | Just (String s) <- val !? "name"
                      , fromString i == s
                      = pure ()
  ident'      === val = diff "identifiers are different" ident' val

-- | The empty identifier is invalid
invalidIdent :: Ident
invalidIdent = mkIdent ""

instance Diffable BindingMode where
  bm === val =
    case (val ! "variant", bm) of
      ("ByValue", ByValue m) -> m === (val ! "fields" ! 0)
      ("ByRef", ByRef m) -> m === (val ! "fields" ! 0)
      _ -> diff "differing binding mode" bm val

instance Diffable Mutability where
  Mutable   === "Mutable" = pure ()
  Immutable === "Immutable" = pure ()
  m         === val = diff "differing mutability" m val

instance Show a => Diffable (Ty a) where
  t' === val = do
    let n = val ! "node"
    case (n, t') of
      ("Never", Never _) -> pure ()
      ("Infer", Infer _) -> pure ()
      (Data.Aeson.Object{}, _) ->
        case (n ! "variant", t') of
          ("Path", PathTy q p _) -> do
            q === (n ! "fields" ! 0)
            p === (n ! "fields" ! 1)
          ("Tup", TupTy t _) -> t === (n ! "fields" ! 0)
          ("Slice", Slice t _) -> t === (n ! "fields" ! 0)
          ("Array", Language.Rust.Syntax.Array t e _) -> do
            t === (n ! "fields" ! 0)
            AnonConst e === (n ! "fields" ! 1)
          ("Ptr", Ptr m t _) -> do
            m === (n ! "fields" ! 0 ! "mutbl")
            t === (n ! "fields" ! 0 ! "ty")
          ("Rptr", Rptr lt m t _) -> do
            lt === (n ! "fields" ! 0)
            m === (n ! "fields" ! 1 ! "mutbl")
            t === (n ! "fields" ! 1 ! "ty")
          ("BareFn", BareFn u a lts decl _) -> do
            u === (n ! "fields" ! 0 ! "unsafety")
            a === (n ! "fields" ! 0 ! "abi")
            lts === (n ! "fields" ! 0 ! "generic_params")
            decl === (n ! "fields" ! 0 ! "decl")
          ("TraitObject", TraitObject bds _) ->
            bds === (n ! "fields" ! 0)
          ("Paren", ParenTy t _) -> t === (n ! "fields" ! 0)
          ("Typeof", Typeof e _) -> AnonConst e === (n ! "fields" ! 0)
          ("Mac", MacTy m _) -> m === (n ! "fields" ! 0)
          ("ImplTrait", ImplTrait bds _) -> bds === (n ! "fields" ! 1)
          _ -> diff "differing types" t' val
      _ -> diff "differing types" t' val


instance Show a => Diffable (GenericBound a) where
  bd === val = case (val ! "variant", bd) of
                 ("Trait", TraitBound p m _) -> do
                   p === (val ! "fields" ! 0)
                   m === (val ! "fields" ! 1)
                 ("Outlives", OutlivesBound l _) ->
                   l === (val ! "fields" ! 0)
                 _ -> diff "different type parameter bounds" bd val

instance Show a => Diffable (PolyTraitRef a) where
  PolyTraitRef lts tr _ === val = do
    lts === (val ! "bound_generic_params")
    tr === (val ! "trait_ref")

instance Show a => Diffable (TraitRef a) where
  TraitRef p === val = p === (val ! "path")

instance Diffable TraitBoundModifier where
  None  === "None" = pure ()
  Maybe === "Maybe" = pure ()
  m     === val = diff "different trait bound modifier" m val

instance Show a => Diffable (Lifetime a) where
  l@(Lifetime n _) === val
    | fromString ("'" ++ n) == val ! "ident" ! "name" = pure () 
    | otherwise = diff "lifetime is different" l val

instance Show a => Diffable (QSelf a) where
  QSelf t p === val = do
    t === (val ! "ty")
    p === (val ! "position")

-- TODO: use gbl to determine crate root
instance Show a => Diffable (Path a) where
  Path _ segs _ === val = do
    let val' = case val ! "segments" of
                 j@(Data.Aeson.Array v)
                   | not (V.null v)
                   , j ! 0 ! "ident" ! "name" == "{{root}}"
                   -> Data.Aeson.Array (V.drop 1 v)
                 j -> j

    segs === val'

instance Show a => Diffable (PathSegment a) where
  PathSegment i pp _ === val = do
      i === (val ! "ident")
      pp === (val ! "args")

instance Show a => Diffable (GenericArgs a) where
  p === val =
    case (val ! "variant", p) of
      ("AngleBracketed", AngleBracketed args bds _) -> do
        args === (val ! "fields" ! 0 ! "args")
        bds === (val ! "fields" ! 0 ! "constraints")
      ("Parenthesized", Parenthesized is o _) -> do
        is === (val ! "fields" ! 0 ! "inputs")
        o  === (val ! "fields" ! 0 ! "output")
      _ -> diff "different path parameters" p val

instance Show a => Diffable (GenericArg a) where
  arg === val =
    case (val ! "variant", arg) of
      ("Lifetime", LifetimeArg l) -> l === (val ! "fields" ! 0)
      ("Type", TypeArg t) -> t === (val ! "fields" ! 0)
      ("Const", ConstArg e) -> AnonConst e === (val ! "fields" ! 0)
      _ -> diff "different generic argument" arg val

instance Show a => Diffable (AssocTyConstraint a) where
  con === val =
    case (val ! "kind" ! "variant", con) of
      ("Equality", EqualityConstraint i t _) -> do
        i === (val ! "ident")
        t === (val ! "kind" ! "fields" ! 0)
      ("Bound", BoundConstraint i b _) -> do
        i === (val ! "ident")
        b === (val ! "kind" ! "fields" ! 0)
      _ -> diff "different associated constraints" con val

newtype AnonConst a = AnonConst (Expr a) deriving (Show, Eq)

instance Show a => Diffable (AnonConst a) where
  AnonConst e === val = e === (val ! "value")

instance Show a => Diffable (Expr a) where
  ex === val =
    case (n ! "variant", ex) of
      ("Lit", Lit as l _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        l === (n ! "fields" ! 0)
      ("Tup", TupExpr as es _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        es === (n ! "fields" ! 0)
      ("Match", Match as e arms _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        e === (n ! "fields" ! 0)
        arms === (n ! "fields" ! 1)
      ("Box", Box as e _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        e === (n ! "fields" ! 0)
      ("Path", PathExpr as q p _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        q === (n ! "fields" ! 0)
        p === (n ! "fields" ! 1)
      ("Array", Vec as es _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        es === (n ! "fields" ! 0)
      ("Call", Call as f es _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        f === (n ! "fields" ! 0)
        es === (n ! "fields" ! 1)
      ("MethodCall", MethodCall as o i es _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        i === (n ! "fields" ! 0)
        (o : es) === (n ! "fields" ! 1)
      ("Binary", Binary as o e1 e2 _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        o === (n ! "fields" ! 0)
        e1 === (n ! "fields" ! 1)
        e2 === (n ! "fields" ! 2)
      ("Unary", Unary as o e _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        o === (n ! "fields" ! 0)
        e === (n ! "fields" ! 1)
      ("Cast", Cast as e t _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        e === (n ! "fields" ! 0)
        t === (n ! "fields" ! 1)
      ("Type", TypeAscription as e t _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        e === (n ! "fields" ! 0)
        t === (n ! "fields" ! 1)
      ("Block", BlockExpr as b l _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        b === (n ! "fields" ! 0)
        l === (n ! "fields" ! 1)
      ("If", If as e tb ee _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        e === (n ! "fields" ! 0)
        tb === (n ! "fields" ! 1)
        ee === (n ! "fields" ! 2)
      ("If", IfLet as p e tb ee _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        case n ! "fields" ! 0 ! "node" ! "variant" of
          "Let" -> pure ()
          _ -> diff "expected `Let` for condition of `IfLet`" ex val
        OrPatHack p === (n ! "fields" ! 0 ! "node" ! "fields" ! 0)
        e === (n ! "fields" ! 0 ! "node" ! "fields" ! 1)
        tb === (n ! "fields" ! 1)
        ee === (n ! "fields" ! 2)
      ("While", While as e b l _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        e === (n ! "fields" ! 0)
        b === (n ! "fields" ! 1)
        l === (n ! "fields" ! 2)
      ("While", WhileLet as p e b l _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        case n ! "fields" ! 0 ! "node" ! "variant" of
          "Let" -> pure ()
          _ -> diff "expected `Let` for condition of `WhileLet`" ex val
        OrPatHack p === (n ! "fields" ! 0 ! "node" ! "fields" ! 0)
        e === (n ! "fields" ! 0 ! "node" ! "fields" ! 1)
        b === (n ! "fields" ! 1)
        l === (n ! "fields" ! 2)
      ("Continue", Continue as l _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        l === (n ! "fields" ! 0)
      ("Break", Break as l e _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        l === (n ! "fields" ! 0)
        e === (n ! "fields" ! 1)
      ("ForLoop", ForLoop as p e b l _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        p === (n ! "fields" ! 0)
        e === (n ! "fields" ! 1)
        b === (n ! "fields" ! 2)
        l === (n ! "fields" ! 3)
      ("Loop", Loop as b l _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        b === (n ! "fields" ! 0)
        l === (n ! "fields" ! 1)
      ("Range", Range as l h rl _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        l === (n ! "fields" ! 0)
        h === (n ! "fields" ! 1)
        rl === (n ! "fields" ! 2)
      ("Closure", Closure as c a m decl e _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        c === (n ! "fields" ! 0)
        a === (n ! "fields" ! 1)
        m === (n ! "fields" ! 2)
        decl === (n ! "fields" ! 3)
        e === (n ! "fields" ! 4)
      ("Assign", Assign as l r _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        l === (n ! "fields" ! 0)
        r === (n ! "fields" ! 1)
      ("AssignOp", AssignOp as o l r _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        o === (n ! "fields" ! 0)
        l === (n ! "fields" ! 1)
        r === (n ! "fields" ! 2)
      ("Field", FieldAccess as e i _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        e === (n ! "fields" ! 0)
        i === (n ! "fields" ! 1)
      ("Field", TupField as e i _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        e === (n ! "fields" ! 0)
        mkIdent (show i) === (n ! "fields" ! 1)
      ("Index", Language.Rust.Syntax.Index as e1 e2 _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        e1 === (n ! "fields" ! 0)
        e2 === (n ! "fields" ! 1)
      ("AddrOf", AddrOf as m e _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        m === (n ! "fields" ! 0)
        e === (n ! "fields" ! 1)
      ("Ret", Ret as e _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        e === (n ! "fields" ! 0)
      ("Yield", Yield as e _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        e === (n ! "fields" ! 0)
      ("Mac", MacExpr as m _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        m === (n ! "fields" ! 0)
      ("Struct", Struct as p f e _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        p === (n ! "fields" ! 0)
        f === (n ! "fields" ! 1)
        e === (n ! "fields" ! 2)
      ("Repeat", Repeat as e1 e2 _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        e1 === (n ! "fields" ! 0)
        AnonConst e2 === (n ! "fields" ! 1)
      ("Paren", ParenExpr as e' _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        e' === (n ! "fields" ! 0)
      ("Try", Try as e _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        e === (n ! "fields" ! 0)
      ("TryBlock", TryBlock as e _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        e === (n ! "fields" ! 0)
      ("Async", Async as c e _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        c === (n ! "fields" ! 0)
        e === (n ! "fields" ! 2)
      ("Await", Await as e _) -> do
        NullList as === (val ! "attrs" ! "_field0")
        e === (n ! "fields" ! 0)
      _ -> diff "differing expressions:" ex val
    where
    n = val ! "node"

instance Show a => Diffable (Label a) where
  Label n _ === val = ("'" <> mkIdent n) === (val ! "ident")

instance Diffable CaptureBy where
  Value === "Value" = pure ()
  Ref   === "Ref" = pure ()
  c     === val = diff "different capture by" c val

instance Diffable Movability where
  Immovable === "Static" = pure ()
  Movable   === "Movable" = pure ()
  m         === val = diff "different movability" m val

instance Diffable IsAsync where
  IsAsync   === val | val !? "node" == Just "Async" = pure ()
  NotAsync  === val | val !? "node" == Just "NotAsync" = pure ()
  IsAsync   === val | val !? "variant" == Just "Async" = pure ()
  NotAsync  === val | val !? "variant" == Just "NotAsync" = pure ()
  IsAsync   === "Async" = pure ()
  NotAsync  === "NotAsync" = pure ()
  a         === val = diff "different async-ness" a val

instance Diffable RangeLimits where
  HalfOpen === val | Just "Excluded" <- val !? "node" = pure ()
  Closed   === val | Just v <- val !? "node"
                   , Just "Included" <- v !? "variant" = pure ()
  HalfOpen === "HalfOpen" = pure ()
  Closed   === "Closed" = pure ()
  rl       === val = diff "different range limits" rl val

instance Diffable BinOp where
  b === val =
    case (val ! "node", b) of
      ("Add",    AddOp   ) -> pure ()
      ("Sub",    SubOp   ) -> pure ()
      ("Mul",    MulOp   ) -> pure ()
      ("Div",    DivOp   ) -> pure ()
      ("Rem",    RemOp   ) -> pure ()
      ("And",    AndOp   ) -> pure ()
      ("Or",     OrOp    ) -> pure ()
      ("BitXor", BitXorOp) -> pure ()
      ("BitAnd", BitAndOp) -> pure ()
      ("BitOr",  BitOrOp ) -> pure ()
      ("Shl",    ShlOp   ) -> pure ()
      ("Shr",    ShrOp   ) -> pure ()
      ("Eq",     EqOp    ) -> pure ()
      ("Lt",     LtOp    ) -> pure ()
      ("Le",     LeOp    ) -> pure ()
      ("Ne",     NeOp    ) -> pure ()
      ("Ge",     GeOp    ) -> pure ()
      ("Gt",     GtOp    ) -> pure ()
      _ -> diff "different binary operation" b val

instance Diffable UnOp where
  Deref === "Deref" = pure ()
  Not   === "Not"   = pure ()
  Neg   === "Neg"   = pure ()
  u     === val     = diff "different unary operator" u val

instance Show a => Diffable (Arm a) where
  Arm as p g b _ === val = do
    as === (val ! "attrs")
    OrPatHack p === (val ! "pats")
    g  === (val ! "guard")
    b  === (val ! "body")

instance Show a => Diffable (Lit a) where
  l === val = do
    let n = val ! "node"
    case (n ! "variant", l) of
      ("Int", Int _ i suf _) -> do
        when (Number (fromInteger i) /= n ! "fields" ! 0) $
          diff "int literal has two different values" l val
        suf === (n ! "fields" ! 1)
      ("FloatUnsuffixed", Float f Unsuffixed _) -> do
        let String j = n ! "fields" ! 0
            str' = case T.unpack j of
                     str | last str == '.' -> str ++ "0"
                         | otherwise -> str
        case readMaybe str' of
          Nothing -> diff "float literal has un-readable value" l val
          Just s | s == f -> pure ()
          _ -> diff "float literal has two different values" l val
      ("Float", Float f suf _) -> do
        let String j = n ! "fields" ! 0
        case readMaybe (T.unpack j) of
          Nothing -> diff "float literal has un-readable value" l val
          Just s | s == f -> pure ()
          _ -> diff "float literal has two different values" l val
        suf === (n ! "fields" ! 1)
      ("Str", Str s sty Unsuffixed _) -> do
        when (String (fromString s) /= n ! "fields" ! 0) $
          diff "string literal has two different values" l val
        sty === (n ! "fields" ! 1)
      ("Char", Char c Unsuffixed _) ->
        when (String (fromString [c]) /= n ! "fields" ! 0) $
          diff "character literal has two different values" l val
      ("Byte", Byte b Unsuffixed _) ->
        b === (n ! "fields" ! 0)
      ("ByteStr", ByteStr s _ Unsuffixed _) ->
        s === (n ! "fields" ! 0)
      ("Bool", Language.Rust.Syntax.Bool b Unsuffixed _) ->
        when (Data.Aeson.Bool b /= n ! "fields" ! 0) $
          diff "boolean literal has two different values" l val
      _ -> diff "different literals" l val

instance Diffable StrStyle where
  Cooked === "Cooked" = pure ()
  (Raw n) === val | val ! "variant" == "Raw" = n === (val ! "fields" ! 0)
  sty === val = diff "different string style" sty val

instance Diffable Suffix where
  Unsuffixed === "Unsuffixed" = pure ()
  F32 === "F32" = pure ()
  F64 === "F64" = pure ()
  Is === val   | val ! "fields" == Data.Aeson.Array ["Isize"]   = pure ()
  I8 === val   | val ! "fields" == Data.Aeson.Array ["I8"]   = pure ()
  I16 === val  | val ! "fields" == Data.Aeson.Array ["I16"]  = pure ()
  I32 === val  | val ! "fields" == Data.Aeson.Array ["I32"]  = pure ()
  I64 === val  | val ! "fields" == Data.Aeson.Array ["I64"]  = pure ()
  I128 === val | val ! "fields" == Data.Aeson.Array ["I128"] = pure ()
  Us === val   | val ! "fields" == Data.Aeson.Array ["Usize"]   = pure ()
  U8 === val   | val ! "fields" == Data.Aeson.Array ["U8"]   = pure ()
  U16 === val  | val ! "fields" == Data.Aeson.Array ["U16"]  = pure ()
  U32 === val  | val ! "fields" == Data.Aeson.Array ["U32"]  = pure ()
  U64 === val  | val ! "fields" == Data.Aeson.Array ["U64"]  = pure ()
  U128 === val | val ! "fields" == Data.Aeson.Array ["U128"] = pure ()
  u === val = diff "different suffix" u val

