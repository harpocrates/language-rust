{-|
Module      : Language.Rust.Syntax.AST
Description : Non-token AST definitions
Copyright   : (c) Alec Theriault, 2017-2019
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC
-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.Rust.Syntax.AST (
  -- ** Top level
  SourceFile(..),

  -- ** General
  Mutability(..),
  Unsafety(..),
  Arg(..),
  FnDecl(..),
  FnHeader(..),

  -- ** Paths
  Path(..),
  PathParameters(..),
  PathSegment(..),
  QSelf(..),

  -- ** Attributes
  Attribute(..),
  AttrStyle(..),

  -- ** Literals
  Lit(..),
  byteStr,
  Suffix(..),
  suffix,
  IntRep(..),
  StrStyle(..),

  -- ** Expressions
  Expr(..),
  Abi(..),
  Arm(..),
  IsAsync(..),
  UnOp(..),
  BinOp(..),
  Label(..),
  CaptureBy(..),
  Movability(..),
  Field(..),
  RangeLimits(..),

  -- ** Types and lifetimes
  Ty(..),
  Generics(..),
  Lifetime(..),
  LifetimeDef(..),
  TyParam(..),
  GenericBound(..),
  partitionGenericBounds,
  WhereClause(..),
  whereClause,
  WherePredicate(..),
  PolyTraitRef(..),
  TraitRef(..),
  TraitBoundModifier(..),

  -- ** Patterns
  Pat(..),
  BindingMode(..),
  FieldPat(..),

  -- ** Statements
  Stmt(..),

  -- ** Items
  Item(..),
  ForeignItem(..),
  ImplItem(..),
  TraitItem(..),
  Defaultness(..),
  ImplPolarity(..),
  StructField(..),
  Variant(..),
  VariantData(..),
  UseTree(..),
  Visibility(..),
  Constness(..),
  MethodSig(..),

  -- ** Blocks
  Block(..),

  -- ** Token trees
  TokenTree(..),
  TokenStream(..),
  unconsTokenStream,
  Nonterminal(..),
  Mac(..),
  MacStmtStyle(..),
) where

import Language.Rust.Data.Ident                  ( Ident, Name )
import Language.Rust.Data.Position
import {-# SOURCE #-} Language.Rust.Syntax.Token ( Delim, Token )

import GHC.Generics                              ( Generic, Generic1 )

import Control.DeepSeq                           ( NFData )
import Data.Data                                 ( Data )
import Data.Typeable                             ( Typeable )

import Data.Char                                 ( ord )
import Data.List                                 ( partition )
import Data.List.NonEmpty                        ( NonEmpty(..) )
import Data.Semigroup as Sem                     ( Semigroup(..) )
import Data.Word                                 ( Word8 )

-- | ABIs support by Rust's foreign function interface (@syntax::abi::Abi@). Note that of these,
-- only 'Rust', 'C', 'System', 'RustIntrinsic', 'RustCall', and 'PlatformIntrinsic' and 'Unadjusted'
-- are cross-platform - all the rest are platform-specific.
--
-- Example: @\"C\"@ as in @extern \"C\" fn foo(x: i32);@
data Abi
  -- Platform-specific ABIs
  = Cdecl
  | Stdcall
  | Fastcall
  | Vectorcall
  | Aapcs
  | Win64
  | SysV64
  | PtxKernel
  | Msp430Interrupt
  | X86Interrupt
  -- Cross-platform ABIs
  | Rust
  | C
  | System
  | RustIntrinsic
  | RustCall
  | PlatformIntrinsic
  | Unadjusted
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data, Generic, NFData)

-- | An argument in a function header (@syntax::ast::Arg@, except with @SelfKind@ and @ExplicitSelf@
-- inlined).
--
-- Example: @x: usize@, @self@, @mut self@, @&self@, @&'lt mut self@, @mut self: Foo@ as in
--
-- @
-- trait Foo {
--   // Regular argument
--   fn new(x: usize) -> Foo;
--
--   // Self argument, by value
--   fn foo(self) -> i32;
--   fn bar(mut self);
--
--   // Self argument, by reference
--   fn baz(&self) -> Bar\<'lt\>;
--   fn qux(&'lt mut self) -> Bar\<'lt\>;
--
--   // Explicit self argument
--   fn quux(mut self: Foo);
-- }
-- @
data Arg a
  = Arg (Maybe (Pat a)) (Ty a) a                   -- ^ Regular argument
  | SelfValue Mutability a                         -- ^ Self argument, by value
  | SelfRegion (Maybe (Lifetime a)) Mutability a   -- ^ Self argument, by reference
  | SelfExplicit (Ty a) Mutability a               -- ^ Explicit self argument
  deriving (Eq, Ord, Show, Functor, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (Arg a) where
  spanOf (Arg _ _ s) = spanOf s
  spanOf (SelfValue _ s) = spanOf s
  spanOf (SelfRegion _ _ s) = spanOf s
  spanOf (SelfExplicit _ _ s) = spanOf s

-- | An arm of a 'Match' expression (@syntax::ast::Arm@). An arm has at least one patten, possibly a
-- guard expression, and a body expression.
--
-- Example: @n if n % 4 == 3 => { println!("{} % 4 = 3", n) }@ as in
--
-- @
-- match n {
--   n if n % 4 == 3 => { println!("{} % 4 = 3", n) }
--   n if n % 4 == 1 => { println!("{} % 4 = 1", n) }
--   _ => println!("{} % 2 = 0", n)
-- }
-- @
data Arm a = Arm [Attribute a] (Pat a) (Maybe (Expr a)) (Expr a) a
  deriving (Eq, Ord, Show, Functor, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (Arm a) where spanOf (Arm _ _ _ _ s) = spanOf s

-- | 'Attribute's are annotations for other AST nodes (@syntax::ast::Attribute@). Note that
-- doc-comments are promoted to attributes.
--
-- Example: @#[derive(Copy,Clone)]@ as in
--
-- @
-- #[derive(Clone, Copy)]
-- struct Complex { re: f32, im: f32 }
-- @
data Attribute a
  -- | Regular attributes of the form @#[...]@
  = Attribute AttrStyle (Path a) TokenStream a
  -- | Doc comment attributes. The 'Prelude.Bool' argument identifies if the comment is inline or
  -- not, and the 'Name' contains the actual doc comment content.
  | SugaredDoc AttrStyle Bool Name a
  deriving (Eq, Ord, Show, Functor, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (Attribute a) where
  spanOf (Attribute _ _ _ s) = spanOf s
  spanOf (SugaredDoc _ _ _ s) = spanOf s

-- | Distinguishes between attributes that are associated with the node that follows them and
-- attributes that are associated with the node that contains them (@syntax::ast::AttrStyle@).
-- These two cases need to be distinguished only for pretty printing - they are otherwise
-- fundamentally equivalent.
--
-- Example: @#[repr(C)]@ is an outer attribute while @#![feature(slice_patterns)]@ is an inner one
data AttrStyle = Outer | Inner deriving (Eq, Ord, Enum, Bounded, Show, Typeable, Data, Generic, NFData)

-- | Binary operators, used in the 'Binary' and 'AssignOp' constructors of 'Expr'
-- (@syntax::ast::BinOp@).
--
-- Example: @+@ as in @1 + 1@ or @1 += 1@
data BinOp
  = AddOp    -- ^ @+@ operator (addition)
  | SubOp    -- ^ @-@ operator (subtraction)
  | MulOp    -- ^ @*@ operator (multiplication)
  | DivOp    -- ^ @/@ operator (division)
  | RemOp    -- ^ @%@ operator (modulus)
  | AndOp    -- ^ @&&@ operator (logical and)
  | OrOp     -- ^ @||@ operator (logical or)
  | BitXorOp -- ^ @^@ operator (bitwise xor)
  | BitAndOp -- ^ @&@ operator (bitwise and)
  | BitOrOp  -- ^ @|@ operator (bitwise or)
  | ShlOp    -- ^ @<<@ operator (shift left)
  | ShrOp    -- ^ @>>@ operator (shift right)
  | EqOp     -- ^ @==@ operator (equality)
  | LtOp     -- ^ @< @operator (less than)
  | LeOp     -- ^ @<=@ operator (less than or equal to)
  | NeOp     -- ^ @!=@ operator (not equal to)
  | GeOp     -- ^ @>=@ operator (greater than or equal to)
  | GtOp     -- ^ @>@ operator (greater than)
  deriving (Eq, Ord, Enum, Bounded, Show, Typeable, Data, Generic, NFData)

-- | Describes how a value bound to an identifier in a pattern is going to be borrowed
-- (@syntax::ast::BindingMode@).
--
-- Example: @&mut@ in @|&mut x: i32| -> { x += 1 }@
data BindingMode
  = ByRef Mutability
  | ByValue Mutability
  deriving (Eq, Ord, Show, Typeable, Data, Generic, NFData)

-- | A curly brace delimited sequence of statements (@syntax::ast::Block@). The last statement in
-- the block can always be a 'NoSemi' expression.
--
-- Example: @{ let x = 1; return x + y }@ as in @fn foo() { let x = 1; return x + y }@
data Block a = Block [Stmt a] Unsafety a
  deriving (Eq, Ord, Show, Functor, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (Block a) where spanOf (Block _ _ s) = spanOf s

-- | Describes how a 'Closure' should close over its free variables (@syntax::ast::CaptureBy@).
data CaptureBy
  = Value -- ^ make copies of free variables closed over (@move@ closures)
  | Ref   -- ^ borrow free variables closed over
  deriving (Eq, Ord, Enum, Bounded, Show, Typeable, Data, Generic, NFData)

-- | Const annotation to specify if a function or method is allowed to be called in constants
-- context with constant arguments (@syntax::ast::Constness@). [Relevant
-- RFC](https://github.com/rust-lang/rfcs/blob/master/text/0911-const-fn.md)
--
-- Example: @const@ in @const fn inc(x: i32) -> i32 { x + 1 }@
data Constness = Const | NotConst deriving (Eq, Ord, Enum, Bounded, Show, Typeable, Data, Generic, NFData)

-- | An 'ImplItem' can be marked @default@ (@syntax::ast::Defaultness@).
data Defaultness = Default | Final deriving (Eq, Ord, Enum, Bounded, Show, Typeable, Data, Generic, NFData)

-- | Expression (@syntax::ast::Expr@). Note that Rust pushes into expressions an unusual number
-- of constructs including @if@, @while@, @match@, etc.
data Expr a
  -- | box expression (example:  @box x@)
  = Box [Attribute a] (Expr a) a
  -- | array literal (example: @[a, b, c, d]@)
  | Vec [Attribute a] [Expr a] a
  -- | function call where the first 'Expr' is the function itself, and the @['Expr']@ is the list of
  -- arguments (example: @foo(1,2,x)@)
  | Call [Attribute a] (Expr a) [Expr a] a
  -- | method call where the first 'Expr' is the receiver, 'Ident' is the method name, @Maybe ['Ty']@
  -- the list of type arguments and '[Expr]' the arguments to the method.
  -- (example: @x.foo::\<Bar, Baz\>(a, b, c, d)@
  | MethodCall [Attribute a] (Expr a) Ident (Maybe [Ty a]) [Expr a] a
  -- | tuple (example: @(a, b, c ,d)@)
  | TupExpr [Attribute a] [Expr a] a
  -- | binary operation (example: @a + b@, @a * b@)
  | Binary [Attribute a] BinOp (Expr a) (Expr a) a
  -- | unary operation (example: @!x@, @*x@)
  | Unary [Attribute a] UnOp (Expr a) a
  -- | literal (example: @1@, @"foo"@)
  | Lit [Attribute a] (Lit a) a
  -- | cast (example: @foo as f64@)
  | Cast [Attribute a] (Expr a) (Ty a) a
  -- | type annotation (example: @x: i32@)
  | TypeAscription [Attribute a] (Expr a) (Ty a) a
  -- | if expression, with an optional @else@ block. In the case the @else@ block is missing, the
  -- type of the @if@ is inferred to be @()@. (example: @if 1 == 2 { (1,1) } else { (2,2) }@
  | If [Attribute a] (Expr a) (Block a) (Maybe (Expr a)) a
  -- | if-let expression with an optional else block (example: @if let Some(x) = None { () }@)
  | IfLet [Attribute a] (Pat a) (Expr a) (Block a) (Maybe (Expr a)) a
  -- | while loop, with an optional label (example: @'lbl: while 1 == 1 { break 'lbl }@)
  | While [Attribute a] (Expr a) (Block a) (Maybe (Label a)) a
  -- | while-let loop, with an optional label (example: @while let Some(x) = None { x }@)
  | WhileLet [Attribute a] (Pat a) (Expr a) (Block a) (Maybe (Label a)) a
  -- | for loop, with an optional label (example: @for i in 1..10 { println!("{}",i) }@)
  | ForLoop [Attribute a] (Pat a) (Expr a) (Block a) (Maybe (Label a)) a
  -- | conditionless loop (can be exited with 'Break', 'Continue', or 'Ret')
  | Loop [Attribute a] (Block a) (Maybe (Label a)) a
  -- | match block
  | Match [Attribute a] (Expr a) [Arm a] a
  -- | closure (example: @move |a, b, c| { a + b + c }@)
  | Closure [Attribute a] CaptureBy IsAsync Movability (FnDecl a) (Expr a) a
  -- | (possibly unsafe) block (example: @unsafe { 1 }@)
  | BlockExpr [Attribute a] (Block a) a
  -- | a try block (example: @try { 1 }@)
  | TryBlock [Attribute a] (Block a) a
  -- | an async block (example: @async move { 1 }@)
  | Async [Attribute a] CaptureBy (Block a) a
  -- | an await expression (example: @foo(1,2,3).await@)
  | Await [Attribute a] (Expr a) a
  -- | assignment (example: @a = foo()@)
  | Assign [Attribute a] (Expr a) (Expr a) a
  -- | assignment with an operator (example: @a += 1@)
  | AssignOp [Attribute a] BinOp (Expr a) (Expr a) a
  -- | access of a named struct field (example: @obj.foo@)
  | FieldAccess [Attribute a] (Expr a) Ident a
  -- | access of an unnamed field of a struct or tuple-struct (example: @foo.0@)
  | TupField [Attribute a] (Expr a) Int a
  -- | indexing operation (example: @foo[2]@)
  | Index [Attribute a] (Expr a) (Expr a) a
  -- | range (examples: @1..2@, @1..@, @..2@, @1...2@, @1...@, @...2@)
  | Range [Attribute a] (Maybe (Expr a)) (Maybe (Expr a)) RangeLimits a
  -- | variable reference
  | PathExpr [Attribute a] (Maybe (QSelf a)) (Path a) a
  -- | referencing operation (example: @&a or &mut a@)
  | AddrOf [Attribute a] Mutability (Expr a) a
  -- | @break@ with an optional label and expression denoting what to break out of and what to
  -- return (example: @break 'lbl 1@)
  | Break [Attribute a] (Maybe (Label a)) (Maybe (Expr a)) a
  -- | @continue@ with an optional label (example: @continue@)
  | Continue [Attribute a] (Maybe (Label a)) a
  -- | @return@ with an optional value to be returned (example: @return 1@)
  | Ret [Attribute a] (Maybe (Expr a)) a
  -- | macro invocation before expansion
  | MacExpr [Attribute a] (Mac a) a
  -- | struct literal expression (examples: @Foo { x: 1, y: 2 }@ or @Foo { x: 1, ..base }@)
  | Struct [Attribute a] (Path a) [Field a] (Maybe (Expr a)) a
  -- | array literal constructed from one repeated element (example: @[1; 5]@)
  | Repeat [Attribute a] (Expr a) (Expr a) a
  -- | no-op: used solely so we can pretty print faithfully
  | ParenExpr [Attribute a] (Expr a) a
  -- | sugar for error handling with @Result@ (example: @parsed_result?@)
  | Try [Attribute a] (Expr a) a
  -- | @yield@ with an optional value to yield (example: @yield 1@)
  | Yield [Attribute a] (Maybe (Expr a)) a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (Expr a) where
  spanOf (Box _ _ s) = spanOf s
  spanOf (Vec _ _ s) = spanOf s
  spanOf (Call _ _ _ s) = spanOf s
  spanOf (MethodCall _ _ _ _ _ s) = spanOf s
  spanOf (TupExpr _ _ s) = spanOf s
  spanOf (Binary _ _ _ _ s) = spanOf s
  spanOf (Unary _ _ _ s) = spanOf s
  spanOf (Lit _ _ s) = spanOf s
  spanOf (Cast _ _ _ s) = spanOf s
  spanOf (TypeAscription _ _ _ s) = spanOf s
  spanOf (If _ _ _ _ s) = spanOf s
  spanOf (IfLet _ _ _ _ _ s) = spanOf s
  spanOf (While _ _ _ _ s) = spanOf s
  spanOf (WhileLet _ _ _ _ _ s) = spanOf s
  spanOf (ForLoop _ _ _ _ _ s) = spanOf s
  spanOf (Loop _ _ _ s) = spanOf s
  spanOf (Match _ _ _ s) = spanOf s
  spanOf (Closure _ _ _ _ _ _ s) = spanOf s
  spanOf (BlockExpr _ _ s) = spanOf s
  spanOf (TryBlock _ _ s) = spanOf s
  spanOf (Async _ _ _ s) = spanOf s
  spanOf (Await _ _ s) = spanOf s
  spanOf (Assign _ _ _ s) = spanOf s
  spanOf (AssignOp _ _ _ _ s) = spanOf s
  spanOf (FieldAccess _ _ _ s) = spanOf s
  spanOf (TupField _ _ _ s) = spanOf s
  spanOf (Index _ _ _ s) = spanOf s
  spanOf (Range _ _ _ _ s) = spanOf s
  spanOf (PathExpr _ _ _ s) = spanOf s
  spanOf (AddrOf _ _ _ s) = spanOf s
  spanOf (Break _ _ _ s) = spanOf s
  spanOf (Continue _ _ s) = spanOf s
  spanOf (Ret _ _ s) = spanOf s
  spanOf (MacExpr _ _ s) = spanOf s
  spanOf (Struct _ _ _ _ s) = spanOf s
  spanOf (Repeat _ _ _ s) = spanOf s
  spanOf (ParenExpr _ _ s) = spanOf s
  spanOf (Try _ _ s) = spanOf s
  spanOf (Yield _ _ s) = spanOf s

-- | Field in a struct literal expression (@syntax::ast::Field@).
--
-- Example: @x: 1@ in @Point { x: 1, y: 2 }@
data Field a = Field Ident (Maybe (Expr a)) a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (Field a) where spanOf (Field _ _ s) = spanOf s

-- | Field in a struct literal pattern (@syntax::ast::FieldPat@). The field name 'Ident' is optional
-- but, when it is 'Nothing', the pattern the field is destructured to must be 'IdentP'.
--
-- Example: @x@ in @Point { x, y }@
data FieldPat a = FieldPat (Maybe Ident) (Pat a) a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (FieldPat a) where spanOf (FieldPat _ _ s) = spanOf s

-- | Header (not the body) of a function declaration (@syntax::ast::FnDecl@). The 'Prelude.Bool'
-- argument indicates whether the header is variadic (so whether the argument list ends in @...@).
--
-- Example: @(bar: i32) -> i32@ as in
--
-- @
-- fn foo(bar: i32) -> i32 {
--   bar + 2
-- }
-- @
data FnDecl a = FnDecl [Arg a] (Maybe (Ty a)) Bool a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (FnDecl a) where spanOf (FnDecl _ _ _ s) = spanOf s

-- | A function header, as seen on a method or function. All of the information between the
-- visibility and the name of the funciton is included in this struct.
data FnHeader a = FnHeader Unsafety IsAsync Constness Abi a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (FnHeader a) where spanOf (FnHeader _ _ _ _ s) = spanOf s

-- | An item within an extern block (@syntax::ast::ForeignItem@ with @syntax::ast::ForeignItemKind@
-- inlined).
--
-- Example: @static ext: u8@ in @extern \"C\" { static ext: u8 }@
data ForeignItem a
  -- | Foreign function
  --
  -- Example: @fn foo(x: i32);@ in @extern \"C\" { fn foo(x: i32); }@
  = ForeignFn [Attribute a] (Visibility a) Ident (FnDecl a) (Generics a) a
  -- | Foreign static variable, optionally mutable
  --
  -- Example: @static mut bar: i32;@ in @extern \"C\" { static mut bar: i32; }@
  | ForeignStatic [Attribute a] (Visibility a) Ident (Ty a) Mutability a
  -- | Foreign type
  --
  -- Example: @type Boo;@
  | ForeignTy [Attribute a] (Visibility a) Ident a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (ForeignItem a) where
  spanOf (ForeignFn _ _ _ _ _ s) = spanOf s
  spanOf (ForeignStatic _ _ _ _ _ s) = spanOf s
  spanOf (ForeignTy _ _ _ s) = spanOf s

-- | Represents lifetimes and type parameters attached to a declaration of a functions, enums,
-- traits, etc. (@syntax::ast::Generics@). Note that lifetime definitions are always required to be
-- before the type parameters.
--
-- This one AST node is also a bit weird: it is the only node whose source representation is not
-- compact - the lifetimes and type parameters occur by themselves between @\<@ and @\>@ then a
-- bit further the where clause occurs after a @where@.
--
-- Example: @\<\'a, \'b: \'c, T: \'a\>@ and @where Option\<T\>: Copy@ as in
--
-- @
-- fn nonsense\<\'a, \'b: \'c, T: \'a\>(x: i32) -\> i32
-- where Option\<T\>: Copy {
--   1
-- }@.
data Generics a = Generics [LifetimeDef a] [TyParam a] (WhereClause a) a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

-- | Extract the where clause from a 'Generics'.
whereClause :: Generics a -> WhereClause a
whereClause (Generics _ _ wc _) = wc

instance Located a => Located (Generics a) where spanOf (Generics _ _ _ s) = spanOf s

instance Sem.Semigroup a => Sem.Semigroup (Generics a) where
  Generics lt1 tp1 wc1 x1 <> Generics lt2 tp2 wc2 x2 = Generics lts tps wcs xs
    where lts = lt1 ++ lt2
          tps = tp1 ++ tp2
          wcs = wc1 <> wc2
          xs  = x1 <> x2

instance (Sem.Semigroup a, Monoid a) => Monoid (Generics a) where
  mappend = (<>)
  mempty = Generics [] [] mempty mempty

{-
data GenericParam a
  = LifetimeParam [Attribute a] (Lifetime a) [GenericBounds a] a
  | TypeParam [Attribute a] Ident [GenericBounds a] (Maybe (Ty a)) a
  | ConstParam [Attribute a] Ident [GenericBounds a] (Ty a) a
-}

-- | An item within an impl (@syntax::ast::ImplItem@ with @syntax::ast::ImplItemKind@ inlined).
--
-- Examples:
--
-- @
-- impl MyTrait {
--   // Associated constant
--   const ID: i32 = 1;
--
--   // Method
--   fn area(&self) -> f64 { 1f64 }
--
--   // Associated type
--   type N = i32;
--
--   // Call to a macro
--   foo!{}
-- }
-- @
data ImplItem a
  -- | Associated constant
  = ConstI [Attribute a] (Visibility a) Defaultness Ident (Ty a) (Expr a) a
  -- | Method
  | MethodI [Attribute a] (Visibility a) Defaultness Ident (Generics a) (MethodSig a) (Block a) a
  -- | Associated type
  | TypeI [Attribute a] (Visibility a) Defaultness Ident (Ty a) a
  -- | Macro call
  | MacroI [Attribute a] Defaultness (Mac a) a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (ImplItem a) where
  spanOf (ConstI _ _ _ _ _ _ s) = spanOf s
  spanOf (MethodI _ _ _ _ _ _ _ s) = spanOf s
  spanOf (TypeI _ _ _ _ _ s) = spanOf s
  spanOf (MacroI _ _ _ s) = spanOf s

-- | For traits with a default impl, one can "opt out" of that impl with a negative impl, by adding
-- @!@ mark before the trait name. [RFC on builtin
-- traits](https://github.com/rust-lang/rfcs/blob/master/text/0019-opt-in-builtin-traits.md)
--
-- Example: @!@ as in @impl !Trait for Foo { }@
data ImplPolarity = Positive | Negative
  deriving (Eq, Ord, Enum, Bounded, Show, Typeable, Data, Generic, NFData)

-- | Distinguishes async from not async elements, eg. closures.
--
-- Example: @async@ in @async |x: i32| { ... }@
data IsAsync = IsAsync | NotAsync
  deriving (Eq, Ord, Enum, Bounded, Show, Typeable, Data, Generic, NFData)

-- | A top-level item, possibly in a 'Mod' or a 'ItemStmt' (@syntax::ast::Item@ with
-- @syntax::ast::ItemKind@ inlined).
--
-- Example: @fn main() { return; }@
data Item a
  -- | extern crate item, with optional original crate name.
  -- Examples: @extern crate foo@ or @extern crate foo_bar as foo@
  = ExternCrate [Attribute a] (Visibility a) Ident (Maybe Ident) a
  -- | use declaration (@use@ or @pub use@) item.
  -- Examples: @use foo;@, @use foo::bar;@, or @use foo::bar as FooBar;@
  | Use [Attribute a] (Visibility a) (UseTree a) a
  -- | static item (@static@ or @pub static@).
  -- Examples: @static FOO: i32 = 42;@ or @static FOO: &'static str = "bar";@
  | Static [Attribute a] (Visibility a) Ident (Ty a) Mutability (Expr a) a
  -- | constant item (@const@ or @pub const@).
  -- Example: @const FOO: i32 = 42;@
  | ConstItem [Attribute a] (Visibility a) Ident (Ty a) (Expr a) a
  -- | function declaration (@fn@ or @pub fn@).
  -- Example: @fn foo(bar: usize) -\> usize { .. }@
  | Fn [Attribute a] (Visibility a) Ident (FnDecl a) (FnHeader a) (Generics a) (Block a) a
  -- | module declaration (@mod@ or @pub mod@) (@syntax::ast::Mod@).
  -- Example: @mod foo;@ or @mod foo { .. }@
  | Mod [Attribute a] (Visibility a) Ident (Maybe [Item a]) a
  -- | external module (@extern@ or @pub extern@) (@syntax::ast::ForeignMod@).
  -- Example: @extern { .. }@ or @extern \"C\" { .. }@
  | ForeignMod [Attribute a] (Visibility a) Abi [ForeignItem a] a
  -- | type alias (@type@ or @pub type@).
  -- Example: @type Foo = Bar\<u8\>;@
  | TyAlias [Attribute a] (Visibility a) Ident (Ty a) (Generics a) a
  -- | enum definition (@enum@ or @pub enum@) (@syntax::ast::EnumDef@).
  -- Example: @enum Foo\<A, B\> { C(A), D(B) }@
  | Enum [Attribute a] (Visibility a) Ident [Variant a] (Generics a) a
  -- | struct definition (@struct@ or @pub struct@).
  -- Example: @struct Foo\<A\> { x: A }@
  | StructItem [Attribute a] (Visibility a) Ident (VariantData a) (Generics a) a
  -- | union definition (@union@ or @pub union@).
  -- Example: @union Foo\<A, B\> { x: A, y: B }@
  | Union [Attribute a] (Visibility a) Ident (VariantData a) (Generics a) a
  -- | trait declaration (@trait@ or @pub trait@).
  -- Example: @trait Foo { .. }@ or @trait Foo\<T\> { .. }@
  | Trait [Attribute a] (Visibility a) Ident Bool Unsafety (Generics a) [GenericBound a] [TraitItem a] a
  -- | trait alias
  -- Example: @trait Foo = Bar + Quux;@
  | TraitAlias [Attribute a] (Visibility a) Ident (Generics a) (NonEmpty (GenericBound a)) a
  -- | implementation
  -- Example: @impl\<A\> Foo\<A\> { .. }@ or @impl\<A\> Trait for Foo\<A\> { .. }@
  | Impl [Attribute a] (Visibility a) Defaultness Unsafety ImplPolarity (Generics a) (Maybe (TraitRef a)) (Ty a) [ImplItem a] a
  -- | generated from a call to a macro
  -- Example: @foo!{ .. }@
  | MacItem [Attribute a] (Maybe Ident) (Mac a) a
  -- | definition of a macro via @macro_rules@
  -- Example: @macro_rules! foo { .. }@
  | MacroDef [Attribute a] Ident TokenStream a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (Item a) where
  spanOf (ExternCrate _ _ _ _ s) = spanOf s
  spanOf (Use _ _ _ s) = spanOf s
  spanOf (Static _ _ _ _ _ _ s) = spanOf s
  spanOf (ConstItem _ _ _ _ _ s) = spanOf s
  spanOf (Fn _ _ _ _ _ _ _ s) = spanOf s
  spanOf (Mod _ _ _ _ s) = spanOf s
  spanOf (ForeignMod _ _ _ _ s) = spanOf s
  spanOf (TyAlias _ _ _ _ _ s) = spanOf s
  spanOf (Enum _ _ _ _ _ s) = spanOf s
  spanOf (StructItem _ _ _ _ _ s) = spanOf s
  spanOf (Union _ _ _ _ _ s) = spanOf s
  spanOf (Trait _ _ _ _ _ _ _ _ s) = spanOf s
  spanOf (TraitAlias _ _ _ _ _ s) = spanOf s
  spanOf (Impl _ _ _ _ _ _ _ _ _ s) = spanOf s
  spanOf (MacItem _ _ _ s) = spanOf s
  spanOf (MacroDef _ _ _ s) = spanOf s

-- | Used to annotate loops, breaks, continues, etc.
data Label a = Label Name a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (Label a) where spanOf (Label _ s) = spanOf s

-- | A lifetime is a name for a scope in a program (@syntax::ast::Lifetime@). One of the novel
-- features of Rust is that code can be parametrized over lifetimes. Syntactically, they are like
-- regular identifiers, but start with a tick @\'@ mark. The 'Name' argument is /not/ supposed to
-- include that tick.
--
-- Examples: @\'a@ or @\'static@
data Lifetime a = Lifetime Name a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (Lifetime a) where spanOf (Lifetime _ s) = spanOf s

-- | A lifetime definition, introducing a lifetime and the other lifetimes that bound it
-- (@syntax::ast::LifetimeDef@).
--
-- Example: @\'a: \'b + \'c + \'d@
data LifetimeDef a = LifetimeDef [Attribute a] (Lifetime a) [Lifetime a] a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (LifetimeDef a) where spanOf (LifetimeDef _ _ _ s) = spanOf s

-- | This is the fundamental unit of parsing - it represents the contents of one source file. It is
-- composed of an optional shebang line, inner attributes that follow, and then the module items.
--
-- Example:
--
-- @
-- #!\/usr\/bin/env rust
--
-- #![allow(dead_code)]
--
-- fn main() {
--   println!("Hello world")
-- }
-- @
data SourceFile a
  = SourceFile (Maybe Name) [Attribute a] [Item a]
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

-- | The suffix on a literal (unifies @syntax::ast::LitIntType@, @syntax::ast::IntTy@,
-- @syntax::ast::UintTy@, and @syntax::ast::FloatTy@). As of today, only numeric types can have
-- suffixes, but the possibility of adding more (possibly arbitrary) suffixes to literals in general
-- is being kept open intentionally. [RFC about future-proofing literal
-- suffixes](https://github.com/rust-lang/rfcs/blob/master/text/0463-future-proof-literal-suffixes.md)
--
-- Examples: @i32@, @isize@, and @f32@
data Suffix
  = Unsuffixed
  | Is | I8 | I16 | I32 | I64 | I128
  | Us | U8 | U16 | U32 | U64 | U128
  |                 F32 | F64
  deriving (Eq, Ord, Show, Enum, Bounded, Typeable, Data, Generic, NFData)

-- | Literals in Rust (@syntax::ast::Lit@). As discussed in 'Suffix', Rust AST is designed to parse
-- suffixes for all literals, even if they are currently only valid on integer and float literals.
data Lit a
  = Str String StrStyle Suffix a            -- ^ string (example: @"foo"@)
  | ByteStr [Word8] StrStyle Suffix a       -- ^ byte string (example: @b"foo"@)
  | Char Char Suffix a                      -- ^ character (example: @\'a\'@)
  | Byte Word8 Suffix a                     -- ^ byte (example: @b\'f\'@)
  | Int IntRep Integer Suffix a             -- ^ integer (example: @1i32@)
  | Float Double Suffix a                   -- ^ float (example: @1.12e4@)
  | Bool Bool Suffix a                      -- ^ boolean (example: @true@)
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (Lit a) where
  spanOf (Str _ _ _ s) = spanOf s
  spanOf (ByteStr _ _ _ s) = spanOf s
  spanOf (Char _ _ s) = spanOf s
  spanOf (Byte _ _ s) = spanOf s
  spanOf (Int _ _ _ s) = spanOf s
  spanOf (Float _ _ s) = spanOf s
  spanOf (Bool _ _ s) = spanOf s

-- | Smart constructor for 'ByteStr'.
byteStr :: String -> StrStyle -> Suffix -> a -> Lit a
byteStr s = ByteStr (map (fromIntegral . ord) s)

-- | Extract the suffix from a literal.
suffix :: Lit a -> Suffix
suffix (Str _ _ s _) = s
suffix (ByteStr _ _ s _) = s
suffix (Char _ s _) = s
suffix (Byte _ s _) = s
suffix (Int _ _ s _) = s
suffix (Float _ s _) = s
suffix (Bool _ s _) = s

-- | The base of the number in an @Int@ literal can be binary (e.g. @0b1100@), octal (e.g. @0o14@),
-- decimal (e.g. @12@), or hexadecimal (e.g. @0xc@).
data IntRep = Bin | Oct | Dec | Hex deriving (Eq, Ord, Show, Enum, Bounded, Typeable, Data, Generic, NFData)

-- | Represents a macro invocation (@syntax::ast::Mac@). The 'Path' indicates which macro is being
-- invoked, and the 'TokenStream' contains the source of the macro invocation.
data Mac a = Mac (Path a) TokenStream a deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (Mac a) where spanOf (Mac _ _ s) = spanOf s

-- | Style of the macro statement (@syntax::ast::MacStmtStyle@).
data MacStmtStyle
  = SemicolonMac -- ^ trailing semicolon (example: @foo! { ... };@, @ foo!(...);@, and @foo![...];@)
  | BracesMac    -- ^ braces (example: @foo! { ... }@)
  deriving (Eq, Ord, Enum, Bounded, Show, Typeable, Data, Generic, NFData)

-- | Represents a method's signature in a trait declaration, or in an implementation.
data MethodSig a = MethodSig (FnHeader a) (FnDecl a)
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

-- | The movability of a generator / closure literal (@syntax::ast::Movability@).
data Movability = Immovable | Movable deriving (Eq, Ord, Enum, Bounded, Show, Typeable, Data, Generic, NFData)

-- | Encodes whether something can be updated or changed (@syntax::ast::Mutability@).
data Mutability = Mutable | Immutable deriving (Eq, Ord, Enum, Bounded, Show, Typeable, Data, Generic, NFData)

-- | For interpolation during macro expansion (@syntax::ast::NonTerminal@).
data Nonterminal a
  = NtItem (Item a)
  | NtBlock (Block a)
  | NtStmt (Stmt a)
  | NtPat (Pat a)
  | NtExpr (Expr a)
  | NtTy (Ty a)
  | NtIdent Ident
  | NtPath (Path a)
  | NtTT TokenTree
  | NtArm (Arm a)
  | NtImplItem (ImplItem a)
  | NtTraitItem (TraitItem a)
  | NtGenerics (Generics a)
  | NtWhereClause (WhereClause a)
  | NtArg (Arg a)
  | NtLit (Lit a)
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

-- | Patterns (@syntax::ast::Pat@).
data Pat a
  -- | wildcard pattern: @_@
  = WildP a
  -- | identifier pattern - either a new bound variable or a unit (tuple) struct pattern, or a
  -- const pattern. Disambiguation cannot be done with parser alone, so it happens during name
  -- resolution. (example: @mut x@)
  | IdentP BindingMode Ident (Maybe (Pat a)) a
  -- | struct pattern. The 'Prelude.Bool' signals the presence of a @..@.
  -- (example: @Variant { x, y, .. }@)
  | StructP (Path a) [FieldPat a] Bool a
  -- | tuple struct pattern (example: @Variant(x, y, z)@)
  | TupleStructP (Path a) [Pat a] a
  -- | path pattern (example @A::B::C@)
  | PathP (Maybe (QSelf a)) (Path a) a
  -- | tuple pattern (example: @(a, b)@)
  | TupleP [Pat a] a
  -- | or pattern, must have more than one variant (example: @Some(0) | None@)
  | OrP (NonEmpty (Pat a)) a
  -- | box pattern (example: @box _@)
  | BoxP (Pat a) a
  -- | reference pattern (example: @&mut (a, b)@)
  | RefP (Pat a) Mutability a
  -- | literal (example: @1@)
  | LitP (Expr a) a
  -- | range pattern (example: @1...2@)
  | RangeP (Expr a) (Expr a) a
  -- | slice pattern (example: @[a, b, .., y, z]@)
  | SliceP [Pat a] a
  -- | A rest pattern @..@. Syntactically it is valid anywhere, but semantically it only has meaning
  -- immediately inside a 'SliceP', a 'TupleP', or a 'TupleStructP' (example @[a, .., b]@)
  | RestP a
  -- | A paren
  | ParenP (Pat a) a
  -- | generated from a call to a macro (example: @LinkedList!(1,2,3)@)
  | MacP (Mac a) a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (Pat a) where
  spanOf (WildP s) = spanOf s
  spanOf (IdentP _ _ _ s) = spanOf s
  spanOf (StructP _ _ _ s) = spanOf s
  spanOf (TupleStructP _ _ s) = spanOf s
  spanOf (PathP _ _ s) = spanOf s
  spanOf (TupleP _ s) = spanOf s
  spanOf (OrP _ s) = spanOf s
  spanOf (BoxP _ s) = spanOf s
  spanOf (RefP _ _ s) = spanOf s
  spanOf (LitP _ s) = spanOf s
  spanOf (RangeP _ _ s) = spanOf s
  spanOf (SliceP _ s) = spanOf s
  spanOf (RestP s) = spanOf s
  spanOf (ParenP _ s) = spanOf s
  spanOf (MacP _ s) = spanOf s

-- | Everything in Rust is namespaced using nested modules. A 'Path' represents a path into nested
-- modules, possibly instantiating type parameters along the way (@syntax::ast::Path@). Much like
-- file paths, these paths can be relative or absolute (global) with respect to the crate root.
--
-- Paths are used to identify expressions (see 'PathExpr'), types (see 'PathTy'), and modules
-- (indirectly through 'UseTree' and such).
--
-- The 'Prelude.Bool' argument identifies whether the path is relative or absolute.
--
-- Example: @std::cmp::PartialEq@
data Path a = Path Bool [PathSegment a] a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (Path a) where spanOf (Path _ _ s) = spanOf s

-- | Parameters on a path segment (@syntax::ast::PathParameters@).
data PathParameters a
  -- | Parameters in a chevron comma-delimited list (@syntax::ast::AngleBracketedParameterData@).
  -- Note that lifetimes must come before types, which must come before bindings. Bindings are
  -- equality constraints on associated types (example: @Foo\<A=Bar\>@)
  --
  -- Example: @\<\'a,A,B,C=i32\>@ in a path segment like @foo::\<'a,A,B,C=i32\>@
  = AngleBracketed [Lifetime a] [Ty a] [(Ident, Ty a)] a
  -- | Parameters in a parenthesized comma-delimited list, with an optional output type
  -- (@syntax::ast::ParenthesizedParameterData@).
  --
  -- Example: @(A,B) -\> C@ in a path segment @Foo(A,B) -\> C@
  | Parenthesized [Ty a] (Maybe (Ty a)) a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (PathParameters a) where
  spanOf (AngleBracketed _ _ _ s) = spanOf s
  spanOf (Parenthesized _ _ s) = spanOf s

-- | Segment of a path (@syntax::ast::PathSegment@).
data PathSegment a
  = PathSegment Ident (Maybe (PathParameters a)) a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (PathSegment a) where spanOf (PathSegment _ _ s) = spanOf s

-- | Trait ref parametrized over lifetimes introduced by a @for@ (@syntax::ast::PolyTraitRef@).
--
-- Example: @for\<\'a,'b\> Foo\<&\'a Bar\>@
data PolyTraitRef a = PolyTraitRef [LifetimeDef a] (TraitRef a) a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (PolyTraitRef a) where spanOf (PolyTraitRef _ _ s) = spanOf s

-- | The explicit @Self@ type in a "qualified path". The actual path, including the trait and the
-- associated item, is stored separately. The first argument is the type given to @Self@ and the
-- second is the index of the associated qualified with this @Self@ type
data QSelf a = QSelf (Ty a) Int
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

-- | Limit types of a 'Range'
data RangeLimits
  = HalfOpen -- ^ Inclusive at the beginning, exclusive at the end
  | Closed   -- ^ Inclusive at the beginning and end
  deriving (Eq, Ord, Enum, Bounded, Show, Typeable, Data, Generic, NFData)

-- | A statement (@syntax::ast::Stmt@). Rust has relatively few types of statements by turning both
-- expressions (sometimes with a required semicolon at the end) and items into statements.
data Stmt a
  -- | A local @let@ binding (@syntax::ast::Local@) (example: @let x: i32 = 1;@)
  = Local (Pat a) (Maybe (Ty a)) (Maybe (Expr a)) [Attribute a] a
  -- | Item definition (example: @fn foo(x: i32) { return x + 1 }@)
  | ItemStmt (Item a) a
  -- | Expression without a trailing semicolon (example: @x + 1@)
  | NoSemi (Expr a) a
  -- | Expression with a trailing semicolon (example: @x + 1;@)
  | Semi (Expr a) a
  -- | A macro call (example: @println!("hello world")@)
  | MacStmt (Mac a) MacStmtStyle [Attribute a] a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (Stmt a) where
  spanOf (Local _ _ _ _ s) = spanOf s
  spanOf (ItemStmt _ s) = spanOf s
  spanOf (NoSemi _ s) = spanOf s
  spanOf (Semi _ s) = spanOf s
  spanOf (MacStmt _ _ _ s) = spanOf s

-- | Style of a string literal (@syntax::ast::StrStyle@).
data StrStyle
  = Cooked     -- ^ regular strings (example: @\"foo\"@)
  | Raw Int    -- ^ raw strings, with the number of @#@ delimiters (example: @r##\"foo\"##@)
  deriving (Eq, Ord, Show, Typeable, Data, Generic, NFData)

-- | Field of a struct (@syntax::ast::StructField@) used in declarations
--
-- Example: @bar: usize@ as in @struct Foo { bar: usize }@
data StructField a = StructField (Maybe Ident) (Visibility a) (Ty a) [Attribute a] a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (StructField a) where spanOf (StructField _ _ _ _ s) = spanOf s

-- | An abstract sequence of tokens, organized into a sequence (e.g. stream) of 'TokenTree', each of
-- which is a single token or a delimited subsequence of tokens.
data TokenStream
  = Tree TokenTree              -- ^ a single token or a single set of delimited tokens
  | Stream [TokenStream]        -- ^ stream of streams of tokens
  deriving (Eq, Ord, Show, Typeable, Data, Generic, NFData)

-- | A 'TokenStream' is at its core just a stream of 'TokenTree'. This function lets you get at that
-- directly. For example, you can use 'Data.List.unfoldr unconsTokenStream' to convert between a
-- 'TokenStream' and '[TokenTree]'.
unconsTokenStream :: TokenStream -> Maybe (TokenTree, TokenStream)
unconsTokenStream (Tree t) = Just (t, Stream [])
unconsTokenStream (Stream []) = Nothing
unconsTokenStream (Stream (ts:tss)) = case unconsTokenStream ts of
                                        Nothing -> unconsTokenStream (Stream tss)
                                        Just (t,ts') -> Just (t, Stream (ts':tss))

-- | 'Span' is not stored on 'TokenStream' - it is computed
instance Located TokenStream where
  spanOf (Tree t) = spanOf t
  spanOf (Stream tt) = spanOf tt

-- | When the parser encounters a macro call, it parses what follows as a 'Delimited' token tree.
-- Basically, token trees let you store raw tokens inside of balanced parens, braces, or brackets.
-- This is a very loose structure, such that all sorts of different AST-fragments can be passed to
-- syntax extensions using a uniform type.
data TokenTree
  -- | A single token
  = Token Span Token
  -- | A delimited sequence of tokens (@syntax::tokenstream::Delimited@)
  -- Example: @{ [-\>+\<] }@ in @brainfuck!{ [-\>+\<] };@
  | Delimited
      { span :: Span
      , delim :: Delim             -- ^ type of delimiter
      , tts :: TokenStream         -- ^ delimited sequence of tokens
      }
  deriving (Eq, Ord, Show, Typeable, Data, Generic, NFData)

instance Located TokenTree where
  spanOf (Token s _) = s
  spanOf (Delimited s _ _) = s

-- | Modifier on a bound, currently this is only used for @?Sized@, where the modifier is @Maybe@.
-- Negative bounds should also be handled here.
data TraitBoundModifier = None | Maybe
  deriving (Eq, Ord, Enum, Bounded, Show, Typeable, Data, Generic, NFData)

-- | Item declaration within a trait declaration (@syntax::ast::TraitItem@ with
-- @syntax::ast::TraitItemKind@ inlined), possibly including a default implementation. A trait item
-- is either required (meaning it doesn't have an implementation, just a signature) or provided
-- (meaning it has a default implementation).
--
-- Examples:
--
-- @
-- trait MyTrait {
--   // Associated constant
--   const ID: i32 = 1;
--
--   // Method
--   fn area(&self) -> f64;
--
--   // Associated type
--   type N: fmt::Display;
--
--   // Call to a macro
--   foo!{}
-- }
-- @
data TraitItem a
  -- | Associated constants
  = ConstT [Attribute a] Ident (Ty a) (Maybe (Expr a)) a
  -- | Method with optional body
  | MethodT [Attribute a] Ident (Generics a) (MethodSig a) (Maybe (Block a)) a
  -- | Possibly abstract associated types
  | TypeT [Attribute a] Ident [GenericBound a] (Maybe (Ty a)) a
  -- | Call to a macro
  | MacroT [Attribute a] (Mac a) a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (TraitItem a) where
  spanOf (ConstT _ _ _ _ s) = spanOf s
  spanOf (MethodT _ _ _ _ _ s) = spanOf s
  spanOf (TypeT _ _ _ _ s) = spanOf s
  spanOf (MacroT _ _ s) = spanOf s

-- | A 'TraitRef' is a path which identifies a trait (@syntax::ast::TraitRef@).
newtype TraitRef a = TraitRef (Path a) deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (TraitRef a) where spanOf (TraitRef p) = spanOf p

-- | Types (@syntax::ast::Ty@).
data Ty a
  -- | variable length slice (example: @[T]@)
  = Slice (Ty a) a
  -- | fixed length array (example: @[T; n]@)
  | Array (Ty a) (Expr a) a
  -- | raw pointer (example: @*const T@ or @*mut T@)
  | Ptr Mutability (Ty a) a
  -- | reference (example: @&\'a T@ or @&\'a mut T@)
  | Rptr (Maybe (Lifetime a)) Mutability (Ty a) a
  -- | bare function (example: @fn(usize) -> bool@)
  | BareFn Unsafety Abi [LifetimeDef a] (FnDecl a) a
  -- | never type: @!@
  | Never a
  -- | tuple (example: @(i32, i32)@)
  | TupTy [Ty a] a
  -- | path type (examples: @std::math::pi@, @\<Vec\<T\> as SomeTrait\>::SomeType@).
  | PathTy (Maybe (QSelf a)) (Path a) a
  -- | trait object type (example: @Bound1 + Bound2 + Bound3@)
  | TraitObject (NonEmpty (GenericBound a)) a
  -- | impl trait type (example: @impl Bound1 + Bound2 + Bound3@).
  | ImplTrait (NonEmpty (GenericBound a)) a
  -- | no-op; kept solely so that we can pretty print faithfully
  | ParenTy (Ty a) a
  -- | typeof, currently unsupported in @rustc@ (example: @typeof(1)@)
  | Typeof (Expr a) a
  -- | inferred type: @_@
  | Infer a
  -- | generated from a call to a macro (example: @HList![i32,(),u8]@)
  | MacTy (Mac a) a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (Ty a) where
  spanOf (Slice _ s) = spanOf s
  spanOf (Array _ _ s) = spanOf s
  spanOf (Ptr _ _ s) = spanOf s
  spanOf (Rptr _ _ _ s) = spanOf s
  spanOf (BareFn _ _ _ _ s) = spanOf s
  spanOf (Never s) = spanOf s
  spanOf (TupTy _ s) = spanOf s
  spanOf (PathTy _ _ s) = spanOf s
  spanOf (TraitObject _ s) = spanOf s
  spanOf (ImplTrait _ s) = spanOf s
  spanOf (ParenTy _ s) = spanOf s
  spanOf (Typeof _ s) = spanOf s
  spanOf (Infer s) = spanOf s
  spanOf (MacTy _ s) = spanOf s

-- | Type parameter definition used in 'Generics' (@syntax::ast::TyParam@). Note that each
-- parameter can have any number of (lifetime or trait) bounds, as well as possibly a default type.
data TyParam a = TyParam [Attribute a] Ident [GenericBound a] (Maybe (Ty a)) a
   deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (TyParam a) where spanOf (TyParam _ _ _ _ s) = spanOf s

-- | Bounds that can be placed on types (@syntax::ast::GenericBound@). These can be either traits or
-- lifetimes.
data GenericBound a
  = TraitBound (PolyTraitRef a) TraitBoundModifier a -- ^ trait bound
  | OutlivesBound (Lifetime a) a                     -- ^ lifetime bound
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (GenericBound a) where
  spanOf (TraitBound _ _ s) = spanOf s
  spanOf (OutlivesBound _ s) = spanOf s

-- | Partition a list of 'GenericBound' into a tuple of the 'TraitBound' and
-- 'OutlivesBound' variants.
partitionGenericBounds :: [GenericBound a] -> ([GenericBound a], [GenericBound a])
partitionGenericBounds = partition isTraitBound
  where
    isTraitBound TraitBound{} = True
    isTraitBound OutlivesBound{} = False

-- | Unary operators, used in the 'Unary' constructor of 'Expr' (@syntax::ast::UnOp@).
--
-- Example: @!@ as in @!true@
data UnOp
  = Deref -- ^ @*@ operator (dereferencing)
  | Not   -- ^ @!@ operator (logical inversion)
  | Neg   -- ^ @-@ operator (negation)
  deriving (Eq, Ord, Enum, Bounded, Show, Typeable, Data, Generic, NFData)

-- | Qualifies whether something is using unsafe Rust or not (@syntax::ast::Unsafety@). Note that we
-- also use this to describe whether a 'Block' is unsafe or not, while Rust has a seperate structure
-- (@syntax::ast::BlockCheckMode@) for that. This is because they also need to keep track of whether
-- an unsafe block is compiler generated.
data Unsafety = Unsafe | Normal deriving (Eq, Ord, Enum, Bounded, Show, Typeable, Data, Generic, NFData)

-- | A variant in Rust is a constructor (either in a 'StructItem', 'Union', or
-- 'Language.Rust.Syntax.Enum') which groups together fields (@syntax::ast::Variant@). In the case
-- of a unit variant, there can also be an explicit discriminant expression.
data Variant a = Variant Ident [Attribute a] (VariantData a) (Maybe (Expr a)) a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (Variant a) where spanOf (Variant _ _ _ _ s) = spanOf s

-- | Main payload in a 'Variant' (@syntax::ast::VariantData@).
--
-- Examples:
--
-- @
-- enum Foo {
--   // Struct variant
--   Bar { x: f64 },
--
--   // Tuple variant
--   Baz(i32, i32),
--
--   // Unit variant
--   Qux,
-- }
-- @
data VariantData a
  = StructD [StructField a] a -- ^ Struct variant
  | TupleD [StructField a] a  -- ^ Tuple variant
  | UnitD a                   -- ^ Unit variant
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (VariantData a) where
  spanOf (StructD _ s) = spanOf s
  spanOf (TupleD _ s) = spanOf s
  spanOf (UnitD s) = spanOf s

-- | Paths used in 'Use' items (@ast::syntax::UseTree@).
--
-- Examples:
--
-- @
-- // Simple use paths
-- use foo::bar::baz as quux;
-- use foo::bar::baz;
--
-- // Glob use paths
-- use foo::bar::*;
--
-- // Nested use paths
-- use foo::bar::{a, b, c::quux as d}
-- @
data UseTree a
  -- | Simple path, optionally ending in an @as@
  = UseTreeSimple (Path a) (Maybe Ident) a
  -- | Path ending in a glob pattern
  | UseTreeGlob (Path a) a
  -- | Path ending in a list of more paths
  | UseTreeNested (Path a) [UseTree a] a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (UseTree a) where
  spanOf (UseTreeSimple _ _ s) = spanOf s
  spanOf (UseTreeGlob _ s) = spanOf s
  spanOf (UseTreeNested _ _ s) = spanOf s

-- | The visibility modifier dictates from where one can access something
-- (@ast::syntax::Visibility@). [RFC about adding restricted
-- visibility](https://github.com/rust-lang/rfcs/blob/master/text/1422-pub-restricted.md)
data Visibility a
  = PublicV               -- ^ @pub@ is accessible from everywhere
  | CrateV                -- ^ @pub(crate)@ is accessible from within the crate
  | RestrictedV (Path a)  -- ^ for some path @p@, @pub(p)@ is visible at that path
  | InheritedV            -- ^ if no visbility is specified, this is the default
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

-- | A @where@ clause in a definition, where one can apply a series of constraints to the types
-- introduced and used by a 'Generic' clause (@syntax::ast::WhereClause@). In many cases, @where@
-- is the /only/ way to express certain bounds (since those bounds may not be immediately on a type
-- defined in the generic, but on a type derived from types defined in the generic).
--
-- Note that while 'WhereClause' is a field of 'Generic', not all uses of generics are coupled with
-- a where clause. In those cases, we leave the list of predicates empty.
--
-- Example:  @where Option\<T\>: Debug@ as in
--
-- @
-- impl\<T\> PrintInOption for T where Option\<T\>: Debug { }
-- @
data WhereClause a = WhereClause [WherePredicate a] a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (WhereClause a) where spanOf (WhereClause _ s) = spanOf s

instance Sem.Semigroup a => Sem.Semigroup (WhereClause a) where
  WhereClause wp1 x1 <> WhereClause wp2 x2 = WhereClause (wp1 ++ wp2) (x1 <> x2)

instance (Sem.Semigroup a, Monoid a) => Monoid (WhereClause a) where
  mappend = (<>)
  mempty = WhereClause [] mempty

-- | An individual predicate in a 'WhereClause' (@syntax::ast::WherePredicate@).
data WherePredicate a
  -- | type bound (@syntax::ast::WhereBoundPredicate@) (example: @for\<\'c\> Foo: Send+Clone+\'c@)
  = BoundPredicate [LifetimeDef a] (Ty a) [GenericBound a] a
  -- | lifetime predicate (@syntax::ast::WhereRegionPredicate@) (example: @\'a: \'b+\'c@)
  | RegionPredicate (Lifetime a) [Lifetime a] a
  -- | equality predicate (@syntax::ast::WhereEqPredicate@) (example: @T=int@). Note that this is
  -- not currently supported.
  | EqPredicate (Ty a) (Ty a) a
  deriving (Eq, Ord, Functor, Show, Typeable, Data, Generic, Generic1, NFData)

instance Located a => Located (WherePredicate a) where
  spanOf (BoundPredicate _ _ _ s) = spanOf s
  spanOf (RegionPredicate _ _ s) = spanOf s
  spanOf (EqPredicate _ _ s) = spanOf s

