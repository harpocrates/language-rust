{-|
Module      : Language.Rust.Syntax.AST
Description : Non-token AST definitions
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE DuplicateRecordFields, DeriveFunctor, DeriveDataTypeable, DeriveGeneric, PatternSynonyms #-}

module Language.Rust.Syntax.AST (
  -- $overview
  -- ** Top level
  SourceFile(..),
  -- ** General
  Mutability(..), Unsafety(..), Arg(..), FnDecl(..),
  -- ** Paths
  Path(..), PathListItem(..), PathParameters(..), QSelf(..),
  -- ** Attributes
  Attribute(..), AttrStyle(..), MetaItem(..), NestedMetaItem(..),
  -- ** Literals
  Lit(..), byteStr, Suffix(..), suffix, IntRep(..), StrStyle(..),
  -- ** Expressions
  Expr(..), Abi(..), Arm(..), AsmDialect(..), UnOp(..), BinOp(..), CaptureBy(..), Field(..),
  InlineAsm(..), InlineAsmOutput(..), RangeLimits(..),
  -- ** Types and lifetimes
  Ty(..), Generics(..), pattern NoGenerics, Lifetime(..), LifetimeDef(..), TyParam(..),
  TyParamBound(..), partitionTyParamBounds, WhereClause(..), WherePredicate(..), PolyTraitRef(..),
  TraitRef(..), TraitBoundModifier(..),
  -- ** Patterns
  Pat(..), BindingMode(..), FieldPat(..),
  -- ** Statements
  Stmt(..),
  -- ** Items
  Item(..), ItemKind(..), ForeignItem(..), ForeignItemKind(..), ImplItem(..), ImplItemKind(..),
  Defaultness(..), ImplPolarity(..), StructField(..), TraitItem(..), TraitItemKind(..), Variant(..),
  VariantData(..), ViewPath(..), Visibility(..), Constness(..), MethodSig(..),
  -- ** Blocks
  Block(..),
  -- ** Token trees
  TokenTree(..), Nonterminal(..), KleeneOp(..), Mac(..), MacStmtStyle(..),
) where

import {-# SOURCE #-} Language.Rust.Syntax.Token (Token, Delim)
import Language.Rust.Syntax.Ident (Ident, Name)
import Language.Rust.Data.Position

import GHC.Generics (Generic)
import Data.Data (Data)
import Data.Typeable (Typeable)

import Data.Word (Word8)
import Data.Char (ord)
import Data.List.NonEmpty (NonEmpty(..))

import Text.Read (Read(..))
import Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadP (choice, string)

-- $overview
-- The abstract syntax tree(s) of the Rust language. Based on the definitions in the @syntax::ast@
-- crate of @rustc@ whenever possible. Unfortunately, since the internals of @rustc@ are not exposed
-- themselves, there are no official docs for them - the current working docs
-- are [here](https://manishearth.github.io/rust-internals-docs/syntax/ast/index.html).

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
  deriving (Eq, Enum, Bounded, Typeable, Data, Generic)

instance Show Abi where
  show Cdecl = "cdecl"
  show Stdcall = "stdcall"
  show Fastcall = "fastcall"
  show Vectorcall = "vectorcall"
  show Aapcs = "aapcs"
  show Win64 = "win64"
  show SysV64 = "sysv64"
  show PtxKernel = "ptx-kernel"
  show Msp430Interrupt = "msp430-interrupt"
  show X86Interrupt = "x86-interrupt"
  show Rust = "Rust"
  show C = "C"
  show System = "system"
  show RustIntrinsic = "rust-intrinsic"
  show RustCall = "rust-call"
  show PlatformIntrinsic = "platform-intrinsic"
  show Unadjusted = "unadjusted"

instance Read Abi where
  readPrec = lift $ choice
    [ string "cdecl" *> pure Cdecl
    , string "stdcall" *> pure Stdcall
    , string "fastcall" *> pure Fastcall
    , string "vectorcall" *> pure Vectorcall
    , string "aapcs" *> pure Aapcs
    , string "win64" *> pure Win64
    , string "sysv64" *> pure SysV64
    , string "ptx-kernel" *> pure PtxKernel
    , string "msp430-interrupt" *> pure Msp430Interrupt
    , string "x86-interrupt" *> pure X86Interrupt
    , string "Rust" *> pure Rust
    , string "C" *> pure C
    , string "system" *> pure System
    , string "rust-intrinsic" *> pure RustIntrinsic
    , string "rust-call" *> pure RustCall
    , string "platform-intrinsic" *> pure PlatformIntrinsic
    , string "unadjusted" *> pure Unadjusted
    ]

-- | An argument in a function header (@syntax::ast::Arg@, except with @SelfKind@ and @ExplicitSelf@
-- inlined).
--
-- Example: @bar: usize@ as in @fn foo(bar: usize)@
data Arg a
  = Arg (Maybe (Pat a)) (Ty a) a                   -- ^ @x: i32@
  | SelfValue Mutability a                         -- ^ @self@, @mut self@
  | SelfRegion (Maybe (Lifetime a)) Mutability a   -- ^ @&'lt self@, @&'lt mut self@
  | SelfExplicit (Ty a) Mutability a               -- ^ @self: i32@, @mut self: i32@
  deriving (Eq, Show, Functor, Typeable, Data, Generic)

instance Located a => Located (Arg a) where
  spanOf (Arg _ _ s) = spanOf s
  spanOf (SelfValue _ s) = spanOf s
  spanOf (SelfRegion _ _ s) = spanOf s
  spanOf (SelfExplicit _ _ s) = spanOf s

-- | An arm of a 'Match' expression (@syntax::ast::Arm@).
--
-- Example: @_ if 1 == 1 => { println!("match!") }@ as in @match n { _ if 1 == 1 => { println!("match!") } }@
data Arm a
  = Arm
      { attrs :: [Attribute a]
      , pats :: NonEmpty (Pat a)
      , guard :: Maybe (Expr a)
      , body :: Expr a
      , nodeInfo :: a
      } deriving (Eq, Show, Functor, Typeable, Data, Generic)

instance Located a => Located (Arm a) where spanOf (Arm _ _ _ _ s) = spanOf s

-- | Inline assembly dialect.
--
-- Example: @"intel"@ as in @asm!("mov eax, 2" : "={eax}"(result) : : : "intel")@
data AsmDialect = Att | Intel deriving (Eq, Enum, Bounded, Show, Typeable, Data, Generic)

-- | 'MetaItem's are annotations for other AST nodes (@syntax::ast::Attribute@). Note that
-- doc-comments are promoted to attributes that have @isSugaredDoc = True@.
--
-- Example: @#[repr(C)]@ in @#[derive(Clone, Copy)] struct Complex { re: f32, im: f32 }@
data Attribute a
  = Attribute
      { style :: AttrStyle   -- ^ whether attribute is inner or outer
      , value :: MetaItem a  -- ^ actual content of attribute
      , isSugaredDoc :: Bool -- ^ whether the attribute was initially a doc-comment
      , nodeInfo :: a
      } deriving (Eq, Show, Functor, Typeable, Data, Generic)

instance Located a => Located (Attribute a) where spanOf (Attribute _ _ _ s) = spanOf s

-- | Distinguishes between attributes that decorate what follows them and attributes that are
-- describe the node that contains them (@syntax::ast::AttrStyle@). These two cases need to be
-- distinguished only for pretty-printing - they are otherwise fundamentally equivalent.
--
-- Example: @#[repr(C)]@ is an outer attribute while @#![repr(C)]@ is an inner one
data AttrStyle = Outer | Inner deriving (Eq, Enum, Bounded, Show, Typeable, Data, Generic)

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
  deriving (Eq, Enum, Bounded, Show, Typeable, Data, Generic)

-- | Describes how a value bound to an identifier in a pattern is going to be borrowed
-- (@syntax::ast::BindingMode@). 
--
-- Example: @&mut@ in @|&mut x: i32| -> { x += 1 }@
data BindingMode
  = ByRef Mutability
  | ByValue Mutability
  deriving (Eq, Show, Typeable, Data, Generic)

-- | A curly brace delimited sequence of statements (@syntax::ast::Block@).
--
-- Example: @{ let x = 1; return x + y }@ as in @fn foo() { let x = 1; return x + y }@
data Block a
  = Block
      { stmts :: [Stmt a]  -- ^ Statements in the block. Note that the last statement in the block
                           -- can always be a 'NoSemi' expression.
      , rules :: Unsafety  -- ^ Distinguishes between regular (safe) blocks and unsafe blocks such
                           -- as  @unsafe { x += 1 }@.
      , nodeInfo :: a
      } deriving (Eq, Show, Functor, Typeable, Data, Generic)

instance Located a => Located (Block a) where spanOf (Block _ _ s) = spanOf s
 
-- | Describes how a 'Closure' should close over its free variables (@syntax::ast::CaptureBy@).
data CaptureBy
  = Value -- ^ make copies of free variables closed over (@move@ closures)
  | Ref   -- ^ borrow free variables closed over
  deriving (Eq, Enum, Bounded, Show, Typeable, Data, Generic)

-- | Const annotation to specify if a function or method is allowed to be called in constants
-- context with constant arguments (@syntax::ast::Constness@). [Relevant
-- RFC](https://github.com/rust-lang/rfcs/blob/master/text/0911-const-fn.md) 
--
-- Example: @const@ in @const fn inc(x: i32) -> i32 { x + 1 }@
data Constness = Const | NotConst deriving (Eq, Enum, Bounded, Show, Typeable, Data, Generic)

-- | 'ImplItem's can be marked @default@ (@syntax::ast::Defaultness@).  
data Defaultness = Default | Final deriving (Eq, Enum, Bounded, Show, Typeable, Data, Generic)

-- | Expression (@syntax::ast::Expr@). Note that Rust pushes into expressions an unusual number
-- of constructs including @if@, @while@, @match@, etc.
data Expr a
  -- | box expression (example:  @box x@)
  = Box [Attribute a] (Expr a) a
  -- | in-place expression - first 'Expr' is the place, second one is the value (example: @x <- y@)
  | InPlace [Attribute a] (Expr a) (Expr a) a
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
  | While [Attribute a] (Expr a) (Block a) (Maybe (Lifetime a)) a
  -- | while-let loop, with an optional label (example: @while let Some(x) = None { x }@)
  | WhileLet [Attribute a] (Pat a) (Expr a) (Block a) (Maybe (Lifetime a)) a
  -- | for loop, with an optional label (example: @for i in 1..10 { println!("{}",i) }@)
  | ForLoop [Attribute a] (Pat a) (Expr a) (Block a) (Maybe (Lifetime a)) a
  -- | conditionless loop (can be exited with 'Break', 'Continue', or 'Ret')
  | Loop [Attribute a] (Block a) (Maybe (Lifetime a)) a
  -- | match block
  | Match [Attribute a] (Expr a) [Arm a] a
  -- | closure (example: @move |a, b, c| { a + b + c }@)
  | Closure [Attribute a] CaptureBy (FnDecl a) (Expr a) a
  -- | (possibly unsafe) block (example: @unsafe { 1 }@)
  | BlockExpr [Attribute a] (Block a) a
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
  | Break [Attribute a] (Maybe (Lifetime a)) (Maybe (Expr a)) a
  -- | @continue@ with an optional label (example: @continue@)
  | Continue [Attribute a] (Maybe (Lifetime a)) a
  -- | @return@ with an optional value to be returned (example: @return 1@)
  | Ret [Attribute a] (Maybe (Expr a)) a
  -- | processed output of the @asm!()@ macro
  | InlineAsmExpr [Attribute a] (InlineAsm a) a
  -- | macro invocation before expansion
  | MacExpr [Attribute a] (Mac a) a
  -- | struct literal expression (examples: @Foo { x: 1, y: 2 }@ or @Foo { x: 1, ..base }@)
  | Struct [Attribute a] (Path a) [Field a] (Maybe (Expr a)) a
  -- | array literal constructed from one repeated element (example: @[1; 5]@)
  | Repeat [Attribute a] (Expr a) (Expr a) a
  -- | no-op: used solely so we can pretty-print faithfully
  | ParenExpr [Attribute a] (Expr a) a
  -- | sugar for error handling with @Result@ (example: @parsed_result?@)
  | Try [Attribute a] (Expr a) a
  deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (Expr a) where
  spanOf (Box _ _ s) = spanOf s
  spanOf (InPlace _ _ _ s) = spanOf s
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
  spanOf (Closure _ _ _ _ s) = spanOf s
  spanOf (BlockExpr _ _ s) = spanOf s
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
  spanOf (InlineAsmExpr _ _ s) = spanOf s
  spanOf (MacExpr _ _ s) = spanOf s
  spanOf (Struct _ _ _ _ s) = spanOf s
  spanOf (Repeat _ _ _ s) = spanOf s
  spanOf (ParenExpr _ _ s) = spanOf s
  spanOf (Try _ _ s) = spanOf s

-- | Field in a struct literal expression (@syntax::ast::Field@).
--
-- Example: @x: 1@ in @Point{ x: 1, y: 2 }@
data Field a
  = Field
      { ident :: Ident         -- ^ the field name
      , expr :: Maybe (Expr a) -- ^ value assigned to the field
      , nodeInfo :: a
      } deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (Field a) where spanOf (Field _ _ s) = spanOf s

-- | Field in a struct literal pattern (@syntax::ast::FieldPat@).
--
-- Example: @x@ in @Point{ x, y }@
data FieldPat a
  = FieldPat
      { ident :: Maybe Ident -- ^ the field name 
      , pat :: Pat a         -- ^ the pattern the field is destructured to - must be 'IdentP'
                             -- when the @ident@ field is 'Nothing'
      , nodeInfo :: a
      } deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (FieldPat a) where spanOf (FieldPat _ _ s) = spanOf s

-- | Header (not the body) of a function declaration (@syntax::ast::FnDecl@).
--
-- Example: @(bar: i32) -> ()@ in @fn foo(bar: i32) -> ()@
data FnDecl a
  = FnDecl
      { inputs :: [Arg a]      -- ^ argument list
      , output :: Maybe (Ty a) -- ^ return type (inlined from @syntax::ast::FunctionRetTy@). When
                               -- 'Nothing', functions default to @()@ and closures to inference.
      , variadic :: Bool       -- ^ whether the argument list ends in @...@
      , nodeInfo :: a
      } deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (FnDecl a) where spanOf (FnDecl _ _ _ s) = spanOf s

-- | An item within an extern block (@syntax::ast::ForeignItem@).
--
-- Example: @static ext: u8@ in @extern "C" { static ext: u8 }@
data ForeignItem a
  = ForeignItem
      { ident :: Ident            -- ^ name of the item
      , attrs :: [Attribute a]    -- ^ attributes attached to it
      , node :: ForeignItemKind a -- ^ actual item
      , vis :: Visibility a       -- ^ visibility
      , nodeInfo :: a
      } deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (ForeignItem a) where spanOf (ForeignItem _ _ _ _ s) = spanOf s

-- | The kind of things that go in a 'ForeignItem' (@syntax::ast::ForeignItemKind@).
data ForeignItemKind a
  = ForeignFn (FnDecl a) (Generics a) -- ^ foreign function
  | ForeignStatic (Ty a) Bool         -- ^ foreign static variable optionally mutable (as indicated by the 'Bool')
  deriving (Eq, Functor, Show, Typeable, Data, Generic)

-- | Represents lifetimes and type parameters attached to a declaration of a functions, enums,
-- traits, etc. (@syntax::ast::Generics@). Note that lifetime definitions are always required to be
-- before the type parameters.
--
-- This one AST node is also a bit weird: it is the only node that whose source representation is
-- not compact - the lifetimes and type parameters occur by themselves between @\<@ and @\>@ then a
-- bit further the where clause occurs after a @where@.
--
-- Example: @\<\'a, \'b: \'c, T: \'a\>@ and @where Option\<T\>: Copy@
-- in @fn nonsense\<\'a, \'b: \'c, T: \'a\>(x: i32) -\> i32 where Option\<T\>: Copy { 1 }@.
data Generics a
  = Generics
      { lifetimes :: [LifetimeDef a]  -- ^ lifetimes defined (possibly with bounds)
      , tyParams :: [TyParam a]       -- ^ type parameters defined (possibly with bounds)
      , whereClause :: WhereClause a  -- ^ constraints on the lifetimes and type parameters
      , nodeInfo :: a
      } deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (Generics a) where spanOf (Generics _ _ _ s) = spanOf s

-- | Most functions or paths have no generics at all, even if they /could/. To avoid having to write
-- out an empty generics @'Generics' [] [] ('WhereClause' [] x) y@, this pattern lets you write
-- the (slightly) abbreviated @'NoGenerics' x y@.
pattern NoGenerics :: a -> a -> Generics a
pattern NoGenerics x y = Generics [] [] (WhereClause [] x) y

-- | An item within an impl (@syntax::ast::ImplItem@).
--
-- Example: @const x: i32 = 1;@ in @impl MyTrait { const x: i32 = 1; }@
data ImplItem a
  = ImplItem
      { ident :: Ident             -- ^ name of the item
      , vis :: Visibility a        -- ^ visibility
      , defaultness :: Defaultness -- ^ whether it is marked @default@ or not
      , attrs :: [Attribute a]     -- ^ attributes attached to it
      , node :: ImplItemKind a     -- ^ actual item
      , nodeInfo :: a
      } deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (ImplItem a) where spanOf (ImplItem _ _ _ _ _ s) = spanOf s

-- | The kind of things that go in an 'ImplItem' (@syntax::ast::ImplItemKind@).
data ImplItemKind a
  = ConstI (Ty a) (Expr a)          -- ^ associated constants (example: @const ID: i32 = 1;@)
  | MethodI (MethodSig a) (Block a) -- ^ methods (example: @fn area(&self) -> f64 { 1f64 }@)
  | TypeI (Ty a)                    -- ^ associated types (example: @type N = i32@)
  | MacroI (Mac a)                  -- ^ call to a macro
  deriving (Eq, Functor, Show, Typeable, Data, Generic)

-- | For traits with a default impl, one can "opt out" of that impl with a negative impl, by adding
-- @!@ mark before the trait name. [RFC on builtin
-- traits](https://github.com/rust-lang/rfcs/blob/master/text/0019-opt-in-builtin-traits.md)
--
-- Example: @!@ as in @impl !Trait for Foo { }@
data ImplPolarity = Positive | Negative deriving (Eq, Enum, Bounded, Show, Typeable, Data, Generic)

-- | Expanded inline assembly macro (@syntax::ast::InlineAsm@).
--
-- Example: @asm!(\"NOP\")@
data InlineAsm a
  = InlineAsm
      { asm :: String
      , asmStrStyle :: StrStyle
      , outputs :: [InlineAsmOutput a]
      , inputs :: [(String, Expr a)]
      , clobbers :: [String]
      , volatile :: Bool
      , alignstack :: Bool
      , dialect :: AsmDialect
      , nodeInfo :: a
      } deriving (Eq, Functor, Show, Typeable, Data, Generic)

-- | Outputs for inline assembly (@syntax::ast::InlineAsmOutput@)
-- 
-- Example: @"={eax}"(result)@ as in @asm!("mov eax, 2" : "={eax}"(result) : : : "intel")@
data InlineAsmOutput a
  = InlineAsmOutput
      { constraint :: String
      , expr :: Expr a
      , isRw :: Bool
      , isIndirect :: Bool
      } deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (InlineAsm a) where spanOf (InlineAsm _ _ _ _ _ _ _ _ s) = spanOf s


-- | A top-level item, possibly in a 'Mod' or a 'ItemStmt' (@syntax::ast::Item@). Note that the name
-- may be a dummy name.
data Item a
  = Item
      { ident :: Ident         -- ^ name of the item
      , attrs :: [Attribute a] -- ^ attributes attached to it
      , node :: ItemKind a     -- ^ actual item
      , vis :: Visibility a    -- ^ visibility
      , nodeInfo :: a
      } deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (Item a) where spanOf (Item _ _ _ _ s) = spanOf s

-- | Kinds of things that can go into items (@syntax::ast::ItemKind@).
data ItemKind a
  -- | extern crate item, with optional original crate name.
  -- Examples: @extern crate foo@ or @extern crate foo_bar as foo@
  = ExternCrate (Maybe Ident)
  -- | use declaration (@use@ or @pub use@) item.
  -- Examples: @use foo;@, @use foo::bar;@, or @use foo::bar as FooBar;@
  | Use (ViewPath a)
  -- | static item (@static@ or @pub static@).
  -- Examples: @static FOO: i32 = 42;@ or @static FOO: &'static str = "bar";@
  | Static (Ty a) Mutability (Expr a)
  -- | constant item (@const@ or @pub const@).
  -- Example: @const FOO: i32 = 42;@
  | ConstItem (Ty a) (Expr a)
  -- | function declaration (@fn@ or @pub fn@).
  -- Example: @fn foo(bar: usize) -\> usize { .. }@
  | Fn (FnDecl a) Unsafety Constness Abi (Generics a) (Block a)
  -- | module declaration (@mod@ or @pub mod@) (@syntax::ast::Mod@).
  -- Example: @mod foo;@ or @mod foo { .. }@
  | Mod { items :: [Item a] }
  -- | external module (@extern@ or @pub extern@) (@syntax::ast::ForeignMod@).
  -- Example: @extern { .. }@ or @extern \"C\" { .. }@
  | ForeignMod { abi :: Abi, foreignItems :: [ForeignItem a] }
  -- | type alias (@type@ or @pub type@).
  -- Example: @type Foo = Bar\<u8\>;@
  | TyAlias (Ty a) (Generics a)
  -- | enum definition (@enum@ or @pub enum@) (@syntax::ast::EnumDef@).
  -- Example: @enum Foo\<A, B\> { C\<A\>, D\<B\> }@
  | Enum [Variant a] (Generics a)
  -- | struct definition (@struct@ or @pub struct@).
  -- Example: @struct Foo\<A\> { x: A }@
  | StructItem (VariantData a) (Generics a)
  -- | union definition (@union@ or @pub union@).
  -- Example: @union Foo\<A, B\> { x: A, y: B }@
  | Union (VariantData a) (Generics a)
  -- | trait declaration (@trait@ or @pub trait@).
  -- Example: @trait Foo { .. }@ or @trait Foo\<T\> { .. }@
  | Trait Unsafety (Generics a) [TyParamBound a] [TraitItem a]
  -- | default implementation
  -- [(RFC for impl
  -- specialization)](https://github.com/rust-lang/rfcs/blob/master/text/1210-impl-specialization.md)
  -- Examples: @impl Trait for .. {}@ or @impl\<T\> Trait\<T\> for .. {}@
  | DefaultImpl Unsafety (TraitRef a)
  -- | implementation
  -- Example: @impl\<A\> Foo\<A\> { .. }@ or @impl\<A\> Trait for Foo\<A\> { .. }@
  | Impl Unsafety ImplPolarity (Generics a) (Maybe (TraitRef a)) (Ty a) [ImplItem a]
  -- | generated from a call to a macro 
  -- Example: @macro_rules! foo { .. }@ or @foo!(..)@
  | MacItem (Mac a)
  deriving (Eq, Functor, Show, Typeable, Data, Generic)

-- | A Kleene-style repetition operator for token sequences (@syntax::ast::KleeneOp@). This refers
-- to the @*@ or @+@ suffix on a 'Sequence' token tree. 
--
-- Examples: @+@ as in @$($inits:expr),+@
data KleeneOp = ZeroOrMore | OneOrMore deriving (Eq, Enum, Bounded, Show, Typeable, Data, Generic)

-- | A lifetime is a name for a scope in a program (@syntax::ast::Lifetime@). One of the novel
-- features of Rust is that code can be parametrized over lifetimes. Syntactically, they are like
-- regular identifiers, but start with a tick @\'@ mark.
--
-- Examples: @\'a@ or @\'static@
data Lifetime a
  = Lifetime
      { name :: Name   -- ^ string that is the actual name part (so omits the tick @\'@)
      , nodeInfo :: a
      } deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (Lifetime a) where spanOf (Lifetime _ s) = spanOf s

-- | A lifetime definition, introducing a lifetime and the other lifetimes that bound it
-- (@syntax::ast::LifetimeDef@).
--
-- Example: @\'a: \'b+\'c+\'d@
data LifetimeDef a
  = LifetimeDef
      { attrs :: [Attribute a]   -- ^ attributes attached to the lifetime definition
      , lifetime :: Lifetime a   -- ^ lifetime being defined (@\'a@ in the example)
      , bounds :: [Lifetime a]   -- ^ other lifetimes which bound it (@\'b+\'c+\'d@ in the example)
      , nodeInfo :: a
      } deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (LifetimeDef a) where spanOf (LifetimeDef _ _ _ s) = spanOf s

-- | This is the fundamental unit of parsing - it represents the contents of one source file. It is
-- composed of an optional shebang line, inner attributes that follow, and then the mod items.
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
data SourceFile a = SourceFile (Maybe Name) [Attribute a] [Item a] deriving (Eq, Functor, Show, Typeable, Data, Generic)

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
   deriving (Eq, Enum, Bounded, Typeable, Data, Generic)

-- | renders suffixes faithfully to their source form 
instance Show Suffix where
  show Unsuffixed = ""
  show Is = "isize"
  show I8 = "i8"
  show I16 = "i16"
  show I32 = "i32"
  show I64 = "i64"
  show I128 = "i128"
  show Us = "usize"
  show U8 = "u8"
  show U16 = "u16"
  show U32 = "u32"
  show U64 = "u64"
  show U128 = "u128"
  show F32 = "f32"
  show F64 = "f64"

-- | Literals in Rust (@syntax::ast::Lit@). As discussed in 'Suffix', Rust AST is designed to parse
-- suffixes for all literals, even if they are currently only valid on 'Int' and 'Float' literals.
data Lit a
  = Str String StrStyle Suffix a            -- ^ string (example: @"foo"@)
  | ByteStr [Word8] StrStyle Suffix a       -- ^ byte string (example: @b"foo"@)
  | Char Char Suffix a                      -- ^ character (example: @\'a\'@)
  | Byte Word8 Suffix a                     -- ^ byte (example: @b\'f\'@)
  | Int IntRep Integer Suffix a             -- ^ integer (example: @1i32@)
  | Float Double Suffix a                   -- ^ float (example: @1.12e4@)
  | Bool Bool Suffix a                      -- ^ boolean (example: @true@)
  deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (Lit a) where
  spanOf (Str _ _ _ s) = spanOf s
  spanOf (ByteStr _ _ _ s) = spanOf s
  spanOf (Char _ _ s) = spanOf s
  spanOf (Byte _ _ s) = spanOf s
  spanOf (Int _ _ _ s) = spanOf s
  spanOf (Float _ _ s) = spanOf s
  spanOf (Bool _ _ s) = spanOf s

-- | Smart constructor for 'ByteStr'
byteStr :: String -> StrStyle -> Suffix -> a -> Lit a
byteStr s = ByteStr (map (fromIntegral . ord) s)

-- | Extract the suffix from a 'Lit'
suffix :: Lit a -> Suffix
suffix (Str _ _ s _) = s
suffix (ByteStr _ _ s _) = s
suffix (Char _ s _) = s 
suffix (Byte _ s _) = s 
suffix (Int _ _ s _) = s 
suffix (Float _ s _) = s
suffix (Bool _ s _) = s 

-- | The base of the number in an @Int@ literal can be binary (like @0b1100@), octal (like @0o14@),
-- decimal (like @12@), or hexadecimal (like @0xc@).
data IntRep = Bin | Oct | Dec | Hex deriving (Eq, Show, Enum, Bounded, Typeable, Data, Generic)

-- | Represents a macro invocation (@syntax::ast::Mac@). The 'Path' indicates which macro is being
-- invoked, and the 'TokenTree's contains the source of the macro invocation.
--
-- Quoted from the source "NB: the additional ident for a macro_rules-style macro is actually
-- stored in the enclosing item. Oog."
data Mac a = Mac (Path a) [TokenTree] a deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (Mac a) where spanOf (Mac _ _ s) = spanOf s

-- | Style of the macro statement (@syntax::ast::MacStmtStyle@).
data MacStmtStyle
  = SemicolonMac -- ^ trailing semicolon (example: @foo! { ... };@, @ foo!(...);@, and @foo![...];@)
  | BracesMac    -- ^ braces (example: @foo! { ... }@)
  deriving (Eq, Enum, Bounded, Show, Typeable, Data, Generic)

-- | Compile-time attribute item (@syntax::ast::MetaItem@)
data MetaItem a
  = Word Ident a                    -- ^ @test@ as in @#[test]@
  | List Ident [NestedMetaItem a] a -- ^ @derive(..)@ as in @#[derive(..)]@
  | NameValue Ident (Lit a) a       -- ^ @feature = "foo"@ as in @#[feature = "foo"]@
  deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (MetaItem a) where
  spanOf (Word _ s) = spanOf s
  spanOf (List _ _ s) = spanOf s 
  spanOf (NameValue _ _ s) = spanOf s

-- | Represents a method's signature in a trait declaration, or in an implementation.
data MethodSig a = MethodSig Unsafety Constness Abi (FnDecl a) (Generics a) deriving (Eq, Functor, Show, Typeable, Data, Generic)

-- | Encodes whether something can be updated or changed (@syntax::ast::Mutability@).
data Mutability = Mutable | Immutable deriving (Eq, Enum, Bounded, Show, Typeable, Data, Generic)

-- | Possible values inside of 'MetaItem's. This needs to be seperate from 'MetaItem' in order to
-- disallow literals in (top-level) 'MetaItem's. 
data NestedMetaItem a
  = MetaItem (MetaItem a) a -- ^ full 'MetaItem', for recursive meta items.
  | Literal (Lit a) a       -- ^ literal (example: @\"foo\"@, @64@, @true@)
  deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (NestedMetaItem a) where
  spanOf (MetaItem _ s) = spanOf s
  spanOf (Literal _ s) = spanOf s

-- | For interpolation during macro expansion (@syntax::ast::NonTerminal@).
data Nonterminal a
  = NtItem (Item a)
  | NtBlock (Block a)
  | NtStmt (Stmt a)
  | NtPat (Pat a)
  | NtExpr (Expr a)
  | NtTy (Ty a)
  | NtIdent Ident
  | NtMeta (MetaItem a)
  | NtPath (Path a)
  | NtTT TokenTree
  | NtArm (Arm a)
  | NtImplItem (ImplItem a)
  | NtTraitItem (TraitItem a)
  | NtGenerics (Generics a)
  | NtWhereClause (WhereClause a)
  | NtArg (Arg a)
  | NtLit (Lit a)
  deriving (Eq, Functor, Show, Typeable, Data, Generic)

-- | Patterns (@syntax::ast::Pat@).
data Pat a
  -- | wildcard pattern: @_@
  = WildP a
  -- | identifier pattern - either a new bound variable or a unit (tuple) struct pattern, or a
  -- const pattern. Disambiguation cannot be done with parser alone, so it happens during name
  -- resolution. (example: @mut x@)
  | IdentP BindingMode Ident (Maybe (Pat a)) a
  -- | struct pattern. The 'Bool' signals the presence of a @..@. (example: @Variant { x, y, .. }@)
  | StructP (Path a) [FieldPat a] Bool a
  -- | tuple struct pattern. If the @..@ pattern is present, the 'Maybe Int' denotes its position.
  -- (example: @Variant(x, y, .., z)@)
  | TupleStructP (Path a) [Pat a] (Maybe Int) a
  -- | path pattern (example @A::B::C@)
  | PathP (Maybe (QSelf a)) (Path a) a
  -- | tuple pattern. If the @..@ pattern is present, the 'Maybe Int' denotes its position.
  -- (example: @(a, b)@)
  | TupleP [Pat a] (Maybe Int) a
  -- | box pattern (example: @box _@)
  | BoxP (Pat a) a
  -- | reference pattern (example: @&mut (a, b)@)
  | RefP (Pat a) Mutability a
  -- | literal (example: @1@)
  | LitP (Expr a) a
  -- | range pattern (example: @1...2@)
  | RangeP (Expr a) (Expr a) a
  -- | slice pattern where, as per [this RFC](https://github.com/P1start/rfcs/blob/array-pattern-changes/text/0000-array-pattern-changes.md),
  -- the pattern is split into the patterns before/after the @..@ and the pattern at the @..@. (example: @[a, b, ..i, y, z]@)
  | SliceP [Pat a] (Maybe (Pat a)) [Pat a] a
  -- | generated from a call to a macro (example: @LinkedList!(1,2,3)@)
  | MacP (Mac a) a
  deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (Pat a) where
  spanOf (WildP s) = spanOf s
  spanOf (IdentP _ _ _ s) = spanOf s
  spanOf (StructP _ _ _ s) = spanOf s
  spanOf (TupleStructP _ _ _ s) = spanOf s
  spanOf (PathP _ _ s) = spanOf s
  spanOf (TupleP _ _ s) = spanOf s
  spanOf (BoxP _ s) = spanOf s
  spanOf (RefP _ _ s) = spanOf s
  spanOf (LitP _ s) = spanOf s
  spanOf (RangeP _ _ s) = spanOf s
  spanOf (SliceP _ _ _ s) = spanOf s 
  spanOf (MacP _ s) = spanOf s

-- | Everything in Rust is namespaced using nested modules. A 'Path' represents a path into nested
-- modules, possibly instantiating type parameters along the way (@syntax::ast::Path@). Much like
-- file paths, these paths can be relative or absolute (global) with respect to the crate root.
--
-- Paths are used to identify expressions (see 'PathExpr'), types (see 'PathTy'), and modules
-- (indirectly through 'ViewPath' and such).
--
-- Example: @std::cmp::PartialEq@
data Path a
  = Path
      { global :: Bool                                 -- ^ is the path relative or absolute (with respect to the crate root) 
      , segments :: NonEmpty (Ident, PathParameters a) -- ^ segments in the path (the things separated by @::@)
      , nodeInfo :: a
      } deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (Path a) where spanOf (Path _ _ s) = spanOf s

-- | Manage the terminal segments in (non-glob) 'ViewPath's (@syntax::ast::PathListItem@).
data PathListItem a
  = PathListItem
      { name :: Ident         -- ^ name of element (examples: @foo@)
      , rename :: Maybe Ident -- ^ optional renames (examples: @as bar@)
      , nodeInfo :: a
      } deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (PathListItem a) where spanOf (PathListItem _ _ s) = spanOf s

-- | Parameters on a path segment (@syntax::ast::PathParameters@).
data PathParameters a
  -- | Parameters in a chevron comma-delimited list (@syntax::ast::AngleBracketedParameterData@).
  -- Note that lifetimes must come before types, which must come before bindings. 
  --
  -- Example: @\<\'a,A,B,C=i32\>@ in a path segment like @foo::\<'a,A,B,C=i32\>@
  = AngleBracketed
      { lifetimes :: [Lifetime a]   -- ^ lifetime parameters
      , types :: [Ty a]             -- ^ type parameters
      , bindings :: [(Ident, Ty a)] -- ^ bindings (equality constraints) on associated types (example: @Foo\<A=Bar\>@)
      , nodeInfo :: a
      }
  -- | Parameters in a parenthesized comma-delimited list, with an optional output type
  -- (@syntax::ast::ParenthesizedParameterData@).
  --
  -- Example: @(A,B) -\> C@ in a path segment @Foo(A,B) -\> C@
  | Parenthesized
      { inputs :: [Ty a]            -- ^ input type parameters (@A@ and @B@ in the example)
      , output :: Maybe (Ty a)      -- ^ output type parameter (@C@ in the example)
      , nodeInfo :: a
      }
  -- | No parameters. Note that in @syntax::ast::PathParameters@, this variant does not exist - it
  -- is considered a subcase of 'AngleBracketed'. However, I want to be able to distinguish between
  -- @foo<>@ and @foo@ for faithful pretty-printing purposes.
  | NoParameters a
  deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (PathParameters a) where
  spanOf (AngleBracketed _ _ _ s) = spanOf s
  spanOf (Parenthesized _ _ s) = spanOf s
  spanOf (NoParameters s) = spanOf s

-- | Trait ref parametrized over lifetimes introduced by a @for@ (@syntax::ast::PolyTraitRef@).
--
-- Example: @for\<\'a\> Foo\<&\'a Bar\>@ 
data PolyTraitRef a
  = PolyTraitRef
      { boundLifetimes :: [LifetimeDef a] -- ^ lifetime introduced (@\'a'@ in the example)
      , traitRef :: TraitRef a            -- ^ trait ref using those lifetimes (@Foo\<&\'a T\>@ in the example)
      , nodeInfo :: a
      } deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (PolyTraitRef a) where spanOf (PolyTraitRef _ _ s) = spanOf s 

-- | The explicit @Self@ type in a "qualified path". The actual path, including the trait and the
-- associated item, is stored separately.
data QSelf a
  = QSelf
      { ty :: Ty a       -- ^ type given to @Self@
      , position :: Int  -- ^ index of the associated qualified with this @Self@ type
      } deriving (Eq, Functor, Show, Typeable, Data, Generic)

-- | Limit types of a 'Range'
data RangeLimits
  = HalfOpen -- ^ Inclusive at the beginning, exclusive at the end
  | Closed   -- ^ Inclusive at the beginning and end
  deriving (Eq, Enum, Bounded, Show, Typeable, Data, Generic)

-- | A statement (@syntax::ast::Stmt@). Rust has relatively few types of statements by turning both
-- expressions (sometimes with a required semicolon at the end) and items into statements.
data Stmt a
  -- | A local @let@ binding (@syntax::ast::Local@) (example: @let x: i32 = 1;@)
  = Local
      { pat :: Pat a                    -- ^ variable(s) being bound by the binding
      , ty :: Maybe (Ty a)              -- ^ optional type ascription
      , init :: Maybe (Expr a)          -- ^ optional initializer expression
      , attrs :: [Attribute a]          -- ^ attributes on the binding 
      , nodeInfo :: a
      }
  -- | Item definition (example: @fn foo(x: i32) { return x + 1 }@)
  | ItemStmt (Item a) a
  -- | Expression without a trailing semicolon (example: @x + 1@)
  | NoSemi (Expr a) a
  -- | Expression with a trailing semicolon (example: @x + 1;@)
  | Semi (Expr a) a
  -- | A macro call (example: @println!("hello world")@)
  | MacStmt (Mac a) MacStmtStyle [Attribute a] a
  deriving (Eq, Functor, Show, Typeable, Data, Generic)

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
  deriving (Eq, Show, Typeable, Data, Generic)

-- | Field of a struct (@syntax::ast::StructField@) used in declarations
--
-- Example: @bar: usize@ as in @struct Foo { bar: usize }@
data StructField a
  = StructField
      { ident :: Maybe Ident    -- ^ name of field
      , vis :: Visibility a     -- ^ visibility of the field
      , ty :: Ty a              -- ^ type of the field
      , attrs :: [Attribute a]  -- ^ attributes on the field
      , nodeInfo :: a
      } deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (StructField a) where spanOf (StructField _ _ _ _ s) = spanOf s

-- | When the parser encounters a macro call, it parses what follows as a 'Delimited' token tree.
-- Basically, token trees let you store raw tokens or 'Sequence' forms inside of balanced
-- parens or braces or brackets. This is a very loose structure, such that all sorts of different
-- AST-fragments can be passed to syntax extensions using a uniform type.
data TokenTree
  -- | A single token
  = Token Span Token
  -- | A delimited sequence of token trees (@syntax::tokenstream::Delimited@)
  -- Example: @{ [-\>+\<] }@ in @brainfuck!{ [-\>+\<] };@
  | Delimited
      { span :: Span 
      , delim :: Delim             -- ^ type of delimiter
      , openSpan :: Span           -- ^ span covering the opening delimiter
      , tts :: [TokenTree]         -- ^ delimited sequence of token trees
      , closeSpan :: Span          -- ^ span covering the closing delimiter
      }
  deriving (Eq, Show, Typeable, Data, Generic)

instance Located TokenTree where
  spanOf (Token s _) = s
  spanOf (Delimited s _ _ _ _) = s

-- | modifier on a bound, currently this is only used for @?Sized@, where the modifier is @Maybe@. Negative
-- bounds should also be handled here.
data TraitBoundModifier = None | Maybe deriving (Eq, Enum, Bounded, Show, Typeable, Data, Generic)

-- | item declaration within a trait declaration (@syntax::ast::TraitItem@), possibly including a default
-- implementation. A trait item is either required (meaning it doesn't have an implementation, just a
-- signature) or provided (meaning it has a default implementation).
data TraitItem a
  = TraitItem
      { ident :: Ident             -- ^ name of the item
      , attrs :: [Attribute a]     -- ^ attributes attached to it
      , node :: TraitItemKind a    -- ^ actual item
      , nodeInfo :: a
      } deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (TraitItem a) where spanOf (TraitItem _ _ _ s) = spanOf s

-- | Kinds of items that can go into traits (@syntax::ast::TraitItemKind@).
data TraitItemKind a
  = ConstT (Ty a) (Maybe (Expr a))           -- ^ associated constants (example: @const ID: i32 = 1;@)
  | MethodT (MethodSig a) (Maybe (Block a))  -- ^ method with optional body (example: @fn area(&self) -> f64;@)
  | TypeT [TyParamBound a] (Maybe (Ty a))    -- ^ associated types (example: @type N: fmt::Display;@)
  | MacroT (Mac a)                           -- ^ call to a macro
  deriving (Eq, Functor, Show, Typeable, Data, Generic)

-- | A 'TraitRef' is a path which identifies a trait (@syntax::ast::TraitRef@).
newtype TraitRef a = TraitRef (Path a) deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (TraitRef a) where spanOf (TraitRef p) = spanOf p

-- | Types (@syntax::ast::Ty@).
data Ty a
  -- | variable-length slice (example: @[T]@)
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
  | TraitObject (NonEmpty (TyParamBound a)) a
  -- | impl trait type (see the
  -- [RFC](https://github.com/rust-lang/rfcs/blob/master/text/1522-conservative-impl-trait.md))
  -- (example: @impl Bound1 + Bound2 + Bound3@).
  | ImplTrait (NonEmpty (TyParamBound a)) a
  -- | no-op; kept solely so that we can pretty-print faithfully
  | ParenTy (Ty a) a
  -- | typeof, currently unsupported in @rustc@ (example: @typeof(1)@)
  | Typeof (Expr a) a
  -- | inferred type: @_@
  | Infer a
  -- | generated from a call to a macro (example: @HList![i32,(),u8]@)
  | MacTy (Mac a) a
  deriving (Eq, Functor, Show, Typeable, Data, Generic)

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

-- | type parameter definition used in 'Generics' (@syntax::ast::TyParam@).
data TyParam a
  = TyParam
      { attrs :: [Attribute a]
      , ident :: Ident             -- ^ name of the type parameter
      , bounds :: [TyParamBound a] -- ^ lifetime or trait bounds on the parameter
      , default_ :: Maybe (Ty a)   -- ^ a possible default type
      , nodeInfo :: a
      } deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (TyParam a) where spanOf (TyParam _ _ _ _ s) = spanOf s

-- | Bounds that can be placed on types (@syntax::ast::TyParamBound@). These can be either traits or
-- lifetimes.
data TyParamBound a
  = TraitTyParamBound (PolyTraitRef a) TraitBoundModifier a -- ^ trait bound
  | RegionTyParamBound (Lifetime a) a                       -- ^ lifetime bound
  deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (TyParamBound a) where
  spanOf (TraitTyParamBound _ _ s) = spanOf s
  spanOf (RegionTyParamBound _ s) = spanOf s

-- | Partion a list of 'TyParamBound' into a tuple of the 'TraitTyParamBound' and 'RegionTyParamBound' variants.
partitionTyParamBounds :: [TyParamBound a] -> ([TyParamBound a], [TyParamBound a])
partitionTyParamBounds [] = ([],[])
partitionTyParamBounds (tpb@TraitTyParamBound{} : ts) = let ~(tpbs,rpbs) = partitionTyParamBounds ts in (tpb:tpbs,rpbs)
partitionTyParamBounds (rpb@RegionTyParamBound{} : ts) = let ~(tpbs,rpbs) = partitionTyParamBounds ts in (tpbs,rpb:rpbs)

-- | Unary operators, used in the 'Unary' constructor of 'Expr' (@syntax::ast::UnOp@).
--
-- Example: @!@ as in @!true@
data UnOp 
  = Deref -- ^ @*@ operator (dereferencing)
  | Not   -- ^ @!@ operator (logical inversion)
  | Neg   -- ^ @-@ operator (negation)
  deriving (Eq, Enum, Bounded, Show, Typeable, Data, Generic)

-- | Qualifies whether something is using unsafe Rust or not (@syntax::ast::Unsafety@). Note that we
-- also use this to describe whether a 'Block' is unsafe or not, while Rust has a seperate structure
-- (@syntax::ast::BlockCheckMode@) for that. This is because they also need to keep track of whether
-- an unsafe block is compiler generated.
data Unsafety = Unsafe | Normal deriving (Eq, Enum, Bounded, Show, Typeable, Data, Generic)

-- | A variant in Rust is a constructor (either in a 'StructItem', 'Union', or 'Enum') which groups
-- together fields (@syntax::ast::Variant@). In the case of a unit variant, there can also be an
-- explicit discriminant expression.
data Variant a
  = Variant
      { name :: Ident              -- ^ name of the constructor
      , attrs :: [Attribute a]     -- ^ attributes attached to it
      , data_ :: VariantData a     -- ^ fields and form of the constructor
      , disrExpr :: Maybe (Expr a) -- ^ explicit discriminant, e.g. Foo = 1
      , nodeInfo :: a
      } deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (Variant a) where spanOf (Variant _ _ _ _ s) = spanOf s

-- | Main payload in a 'Variant' (@syntax::ast::VariantData@).
data VariantData a
  = StructD [StructField a] a -- ^ struct variant (example: @Bar { .. }@ as in @enum Foo { Bar { .. } }@)
  | TupleD [StructField a] a  -- ^ tuple variant (exmaple: @Bar(..)@ as in enum Foo { Bar(..) }@)
  | UnitD a                   -- ^ unit variant (example @Bar@ as in @enum Foo { Bar = .. }@)
  deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (VariantData a) where
  spanOf (StructD _ s) = spanOf s
  spanOf (TupleD _ s) = spanOf s
  spanOf (UnitD s) = spanOf s

-- | Paths used in 'Use' items (@ast::syntax::ViewPath@).
data ViewPath a
  -- | A regular mod path, or a mod path ending in an @as@.
  --
  -- Examples: @foo::bar::baz as quux@ or just @foo::bar::baz@
  = ViewPathSimple Bool [Ident] (PathListItem a) a
  -- | A regular mod path ending in a glob pattern
  --
  -- Example: @foo::bar::*@
  | ViewPathGlob Bool [Ident] a
  -- | A regular mod path ending in a list of identifiers or renamed identifiers.
  --
  -- Example: @foo::bar::{a,b,c as d}@
  | ViewPathList Bool [Ident] [PathListItem a] a
  deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (ViewPath a) where
  spanOf (ViewPathSimple _ _ _ s) = spanOf s
  spanOf (ViewPathGlob _ _ s) = spanOf s
  spanOf (ViewPathList _ _ _ s) = spanOf s

-- | The visibility modifier dictates from where one can access something
-- (@ast::syntax::Visibility@). [RFC about adding restricted
-- visibility](https://github.com/rust-lang/rfcs/blob/master/text/1422-pub-restricted.md)
data Visibility a
  = PublicV               -- ^ @pub@ is accessible from everywhere 
  | CrateV                -- ^ @pub(crate)@ is accessible from within the crate
  | RestrictedV (Path a)  -- ^ for some path @p@, @pub(p)@ is visible at that path
  | InheritedV            -- ^ if no visbility is specified, this is the default
  deriving (Eq, Functor, Show, Typeable, Data, Generic)

-- | A @where@ clause in a definition, where one can apply a series of constraints to the types
-- introduced and used by a 'Generic' clause (@syntax::ast::WhereClause@). In many cases, @where@ 
-- is the /only/ way to express certain bounds (since those bounds may not be immediately on a type
-- defined in the generic, but on a type derived from types defined in the generic).
--
-- Note that while 'WhereClause' is a field of 'Generic', not all uses of generics are coupled with
-- a where clause. In those cases, we leave the list of predicates empty.
--
-- Example:  @where Option\<T\>: Debug@ in @impl\<T\> PrintInOption for T where Option\<T\>: Debug@
data WhereClause a
  = WhereClause
      { predicates :: [WherePredicate a]  -- ^ predicates enforced by the clause
      , nodeInfo :: a
      } deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (WhereClause a) where spanOf (WhereClause _ s) = spanOf s

-- | An individual predicate in a 'WhereClause' (@syntax::ast::WherePredicate@).
data WherePredicate a
  -- | type bound (@syntax::ast::WhereBoundPredicate@) (example: @for\<\'c\> Foo: Send+Clone+\'c@)
  = BoundPredicate
      { boundLifetimes :: [LifetimeDef a]       -- ^ any lifetimes from an optional @for@ binding
                                                -- (@\'c@ in the example)
      , boundedTy :: Ty a                       -- ^ type being bounded (@Foo@ in the example)
      , traitLifetimeBounds :: [TyParamBound a] -- ^ trait and lifetime bounds (@Clone+Send+'c@ in
                                                -- the example)
      , nodeInfo :: a
      }
  -- | lifetime predicate (@syntax::ast::WhereRegionPredicate@) (example: @\'a: \'b+\'c@)
  | RegionPredicate
      { lifetime :: Lifetime a                  -- ^ lifetime being bounded (@\'a@ in the example)
      , lifetimeBounds :: [Lifetime a]          -- ^ lifetime bounds (@\'b+\'c@) in the example)
      , nodeInfo :: a
      }
  -- | equality predicate (@syntax::ast::WhereEqPredicate@) (example: @T=int@). Note that this is
  -- not currently supported.
  | EqPredicate
      { lhs :: Ty a                             -- ^ LHS of the equality predicate (@T@ in the example)
      , rhs :: Ty a                             -- ^ RHS of the equality predicate (@int@ in the example)
      , nodeInfo :: a
      }
  deriving (Eq, Functor, Show, Typeable, Data, Generic)

instance Located a => Located (WherePredicate a) where
  spanOf (BoundPredicate _ _ _ s) = spanOf s
  spanOf (RegionPredicate _ _ s) = spanOf s
  spanOf (EqPredicate _ _ s) = spanOf s

