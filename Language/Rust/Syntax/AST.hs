{-# LANGUAGE DuplicateRecordFields #-}

module Language.Rust.Syntax.AST where

import qualified Language.Rust.Syntax.Token as Token
import Language.Rust.Syntax.Ident
import Language.Rust.Data.Position

import Data.Word

-- https://docs.serde.rs/syntex_syntax/abi/enum.Abi.html
data Abi
  = Cdecl
  | Stdcall
  | Fastcall
  | Vectorcall
  | Aapcs
  | Win64
  | SysV64
  | Rust
  | C
  | System
  | RustIntrinsic
  | RustCall
  | PlatformIntrinsic
  deriving (Eq, Enum, Bounded)

-- | An argument in a function header like `bar: usize` as in `fn foo(bar: usize)`
-- https://docs.serde.rs/syntex_syntax/ast/struct.Arg.html
data Arg a
  = Arg {
      ty :: [Ty a],
      pat :: [Pat a],
      nodeInfo :: a
    }

-- | An arm of a 'match'. E.g. `0...10 => { println!("match!") }` as in `match n { 0...10 => { println!("match!") }, /* .. */ }
-- https://docs.serde.rs/syntex_syntax/ast/struct.Arm.html
data Arm a
  = Arm {
      attrs :: [Attribute a],
      pats :: [Pat a],
      guard :: Maybe (Expr a),
      body :: (Expr a),
      nodeInfo :: a
    }

-- | Inline assembly dialect.
-- E.g. "intel" as in asm!("mov eax, 2" : "={eax}"(result) : : : "intel")
data AsmDialect = Att | Intel deriving (Eq, Enum, Bounded)

-- | Doc-comments are promoted to attributes that have isSugaredDoc = true
-- https://docs.serde.rs/syntex_syntax/ast/struct.Attribute_.html
data Attribute a
  = Attribute {
      style :: AttrStyle,
      value :: MetaItem a,
      isSugaredDoc :: Bool,
      nodeInfo :: a
    }

-- | Distinguishes between Attributes that decorate items and Attributes that are contained as statements
-- within items. These two cases need to be distinguished for pretty-printing.
-- https://docs.serde.rs/syntex_syntax/ast/enum.AttrStyle.html
data AttrStyle = Outer | Inner deriving (Eq, Enum, Bounded)

-- https://docs.serde.rs/syntex_syntax/ast/enum.BinOpKind.html
data BinOp
  = AddOp    -- ^ The + operator (addition)
  | SubOp    -- ^ The - operator (subtraction)
  | MulOp    -- ^ The * operator (multiplication)
  | DivOp    -- ^ The / operator (division)
  | RemOp    -- ^ The % operator (modulus)
  | AndOp    -- ^ The && operator (logical and)
  | OrOp     -- ^ The || operator (logical or)
  | BitXorOp -- ^ The ^ operator (bitwise xor)
  | BitAndOp -- ^ The & operator (bitwise and)
  | BitOrOp  -- ^ The | operator (bitwise or)
  | ShlOp    -- ^ The << operator (shift left)
  | ShrOp    -- ^ The >> operator (shift right)
  | EqOp     -- ^ The == operator (equality)
  | LtOp     -- ^ The < operator (less than)
  | LeOp     -- ^ The <= operator (less than or equal to)
  | NeOp     -- ^ The != operator (not equal to)
  | GeOp     -- ^ The >= operator (greater than or equal to)
  | GtOp     -- ^ The > operator (greater than)
  deriving (Eq, Enum, Bounded)

-- https://docs.serde.rs/syntex_syntax/ast/enum.BindingMode.html
data BindingMode
  = ByRef Mutability
  | ByValue Mutability

-- | A Block ({ .. }). E.g. `{ .. }` as in `fn foo() { .. }`
-- https://docs.serde.rs/syntex_syntax/ast/struct.Block.html
data Block a
  = Block {
      stmts :: [Stmt a],       -- ^ Statements in a block
      rules :: BlockCheckMode, -- ^ Distinguishes between `unsafe { ... }` and `{ ... }`
      nodeInfo :: a
    }

-- https://docs.serde.rs/syntex_syntax/ast/enum.BlockCheckMode.html
-- Inlined [UnsafeSource](-- https://docs.serde.rs/syntex_syntax/ast/enum.UnsafeSource.html)
data BlockCheckMode = DefaultBlock | UnsafeBlock { compilerGenerated :: Bool } deriving (Eq)

-- | A capture clause
-- https://docs.serde.rs/syntex_syntax/ast/enum.CaptureBy.html
data CaptureBy = Value | Ref deriving (Eq, Enum, Bounded)

-- https://docs.serde.rs/syntex_syntax/ast/enum.Constness.html
data Constness = Const | NotConst deriving (Eq, Enum, Bounded)

-- https://docs.serde.rs/syntex_syntax/ast/struct.Crate.html
data Crate a
  = Crate {
      module_ :: [Item a],
      attrs :: [Attribute a],
      config :: MetaItem a,
      exportedMacros :: [MacroDef a],
      nodeInfo :: a
    }

-- | The set of MetaItems that define the compilation environment of the crate, used to drive conditional compilation
-- https://docs.serde.rs/syntex_syntax/ast/type.CrateConfig.html
type CrateConfig a = [MetaItem a]

-- https://docs.serde.rs/syntex_syntax/ast/enum.Defaultness.html
data Defaultness = Default | Final deriving (Eq, Enum, Bounded)

-- | An expression
-- https://docs.serde.rs/syntex_syntax/ast/struct.Expr.html
-- Inlined [ExprKind](https://docs.serde.rs/syntex_syntax/ast/enum.ExprKind.html)
data Expr a
  -- | A `box x` expression.
  = Box [Attribute a] (Expr a) a
  -- |  First expr is the place; second expr is the value.
  | InPlace [Attribute a] (Expr a) (Expr a) a
  -- |  An array (`[a, b, c, d]`)
  | Vec [Attribute a] [Expr a] a
  -- | A function call. The first field resolves to the function itself, and the second field is the list of arguments
  | Call [Attribute a] (Expr a) [Expr a] a
  -- | A method call (x.foo::<Bar, Baz>(a, b, c, d)).
  -- The (Ident a) is the identifier for the method name. The vector of Tys are the ascripted
  -- type parameters for the method (within the angle brackets).

  -- The first element of the vector of Exprs is the expression that evaluates to the object on
  -- which the method is being called on (the receiver), and the remaining elements are the rest
  -- of the arguments.

  -- Thus, `x.foo::<Bar, Baz>(a, b, c, d)` is represented as
  -- `ExprKind::MethodCall(foo, [Bar, Baz], [x, a, b, c, d])`. -}
  | MethodCall [Attribute a] (Ident a) [Ty a] [Expr a] a
  -- |  A tuple (`(a, b, c ,d)`)
  | TupExpr [Attribute a] [Expr a] a
  -- | A binary operation (For example: `a + b`, `a * b`)
  | Binary [Attribute a] BinOp (Expr a) (Expr a) a
  -- | A unary operation (For example: `!x`, `*x`)
  | Unary [Attribute a] UnOp (Expr a) a
  -- | A literal (For example: `1`, `"foo"`)
  | Lit [Attribute a] (Lit a) a
  -- | A cast (`foo as f64`)
  | Cast [Attribute a] (Expr a) (Ty a) a
  -- | Experimental annotation
  | TypeAscription [Attribute a] (Expr a) (Ty a) a
  -- | An if block, with an optional else block `if expr { block } else { expr }`
  | If [Attribute a] (Expr a) (Block a) (Maybe (Expr a)) a
  -- | An `if let` expression with an optional else block
  -- `if let pat = expr { block } else { expr }`
  -- This is desugared to a match expression.
  | IfLet [Attribute a] (Pat a) (Expr a) (Block a) (Maybe (Expr a)) a
  -- | A while loop, with an optional label `'label: while expr { block }`
  | While [Attribute a] (Expr a) (Block a) (Maybe (Ident a)) a
  -- | A while-let loop, with an optional label
  -- 'label: while let pat = expr { block }
  -- This is desugared to a combination of loop and match expressions.
  | WhileLet [Attribute a] (Pat a) (Expr a) (Block a) (Maybe (Ident a)) a
  -- | A for loop, with an optional label
  -- 'label: for pat in expr { block }
  -- This is desugared to a combination of loop and match expressions.
  | ForLoop [Attribute a] (Pat a) (Expr a) (Block a) (Maybe (Ident a)) a
  -- | Conditionless loop (can be exited with break, continue, or return) 'label: loop { block }
  | Loop [Attribute a] (Block a) (Maybe (Ident a)) a
  -- | A match block.
  | Match [Attribute a] (Expr a) [Arm a] a
  -- | A closure (for example, `move |a, b, c| {a + b + c}`).
  | Closure [Attribute a] CaptureBy (FnDecl a) (Block a) a
  -- | A block (`{ ... }`)
  | BlockExpr [Attribute a] (Block a) a
  -- | An assignment (a = foo())
  | Assign [Attribute a] (Expr a) (Expr a) a
  -- | An assignment with an operator. For example, a += 1.
  | AssignOp [Attribute a] BinOp (Expr a) (Expr a) a
  -- | Access of a named struct field (obj.foo)
  | FieldAccess [Attribute a] (Expr a) (Ident a) a
  -- | Access of an unnamed field of a struct or tuple-struct. For example, foo.0.
  | TupField [Attribute a] (Expr a) Int a
  -- | An indexing operation (foo[2])
  | Index [Attribute a] (Expr a) (Expr a) a
  -- | A range (1..2, 1.., ..2, 1...2, 1..., ...2)
  | Range [Attribute a] (Maybe (Expr a)) (Maybe (Expr a)) RangeLimits a
  -- | Variable reference, possibly containing :: and/or type parameters, e.g. foo::bar::.
  -- Optionally "qualified", E.g. <Vec<T> as SomeTrait>::SomeType.
  | PathExpr [Attribute a] (Maybe (QSelf a)) (Path a) a
  -- | A referencing operation (&a or &mut a)
  | AddrOf [Attribute a] Mutability (Expr a) a
  -- | A break, with an optional label to break
  | Break [Attribute a] (Maybe (Ident a)) a
  -- | A continue, with an optional label
  | Continue [Attribute a] (Maybe (Ident a)) a
  -- | A return, with an optional value to be returned
  | Ret [Attribute a] (Maybe (Expr a)) a
  -- | Output of the asm!() macro
  | InlineAsmExpr [Attribute a] (InlineAsm a) a
  -- | A macro invocation; pre-expansion
  | MacExpr [Attribute a] (Mac a) a
  -- | A struct literal expression. For example, Foo {x: 1, y: 2}, or Foo {x: 1, .. base}, where
  -- base is the (Maybe Expr).
  | Struct [Attribute a] (Path a) [Field a] (Maybe (Expr a)) a
  -- | An array literal constructed from one repeated element. For example, [1; 5]. The first
  -- expression is the element to be repeated; the second is the number of times to repeat it.
  | Repeat [Attribute a] (Expr a) (Expr a) a
  -- | No-op: used solely so we can pretty-print faithfully
  | ParenExpr [Attribute a] (Expr a) a
  -- | `expr?`
  | Try [Attribute a] (Expr a) a

-- https://docs.serde.rs/syntex_syntax/ast/struct.Field.html
data Field a
  = Field {
      ident :: Ident a,
      expr :: Expr a,
      nodeInfo :: a
    }

-- A single field in a struct pattern
-- Patterns like the fields of `Foo { x, ref y, ref mut z }` are treated the same as
-- `x: x, y: ref y, z: ref mut z`, except `isShorthand` is `true`
-- https://docs.serde.rs/syntex_syntax/ast/struct.FieldPat.html
data FieldPat a
  = FieldPat {
      ident :: Ident a,     -- ^ The identifier for the field
      pat :: Pat a,         -- ^ The pattern the field is destructured to
      isShorthand :: Bool,
      nodeInfo :: a
    }

-- https://docs.serde.rs/syntex_syntax/ast/enum.FloatTy.html
data FloatTy = F32 | F64 deriving (Eq, Enum, Bounded)

-- | Header (not the body) of a function declaration.
-- E.g. `fn foo(bar: baz)`
-- https://docs.serde.rs/syntex_syntax/ast/struct.FnDecl.html
data FnDecl a
  = FnDecl {
      inputs :: [Arg a],
      -- | Inlined from [FunctionRetTy](https://docs.serde.rs/syntex_syntax/ast/enum.FunctionRetTy.html)
      --    * Nothing -> Return type is not specified. Functions default to () and closures default to inference. Span points to where return type would be inserted.
      --    * Right Ty -> Everything else
      output :: Maybe (Ty a),
      variadic :: Bool,
      nodeInfo :: a
    }

-- https://docs.serde.rs/syntex_syntax/ast/struct.ForeignItem.html
data ForeignItem a
  = ForeignItem {
      ident :: Ident a,
      attrs :: [Attribute a],
      node :: ForeignItemKind a,
      vis :: Visibility a,
      nodeInfo :: a
    }

-- | An item within an extern block
-- https://docs.serde.rs/syntex_syntax/ast/enum.ForeignItemKind.html
data ForeignItemKind a
  = ForeignFn (FnDecl a) (Generics a) -- ^ A foreign function
  | ForeignStatic (Ty a) Bool         -- ^ A foreign static item (static ext: u8), with optional mutability (the boolean is true when mutable)

-- | Represents lifetimes and type parameters attached to a declaration of a function, enum, trait, etc.
-- https://docs.serde.rs/syntex_syntax/ast/struct.Generics.html
data Generics a
  = Generics {
      lifetimes :: [LifetimeDef a],
      tyParams :: [TyParam a],
      whereClause :: WhereClause a,
      nodeInfo :: a
    }

-- https://docs.serde.rs/syntex_syntax/ast/struct.ImplItem.html
data ImplItem a
  = ImplItem {
      ident :: Ident a,
      vis :: Visibility a,
      defaultness :: Defaultness,
      attrs :: [Attribute a],
      node :: ImplItemKind a,
      nodeInfo :: a
    }

-- https://docs.serde.rs/syntex_syntax/ast/enum.ImplItemKind.html
data ImplItemKind a
  = ConstI (Ty a) (Expr a)
  | MethodI (MethodSig a) (Block a)
  | TypeI (Ty a)
  | MacroI (Mac a)

-- https://docs.serde.rs/syntex_syntax/ast/enum.ImplPolarity.html
data ImplPolarity
  = Positive -- ^ `impl Trait for Type`
  | Negative -- ^ `impl !Trait for Type`
  deriving (Eq, Enum, Bounded)

-- Inline assembly. E.g. `asm!("NOP")`;
-- https://docs.serde.rs/syntex_syntax/ast/struct.InlineAsm.html
data InlineAsm a
  = InlineAsm {
      asm :: InternedString,
      asmStrStyle :: StrStyle,
      outputs :: [InlineAsmOutput a],
      inputs :: [(InternedString, Expr a)],
      clobbers :: [InternedString],
      volatile :: Bool,
      alignstack :: Bool,
      dialect :: AsmDialect,
      nodeInfo :: a
}

-- | Inline assembly.
-- E.g. "={eax}"(result) as in asm!("mov eax, 2" : "={eax}"(result) : : : "intel")`
-- https://docs.serde.rs/syntex_syntax/ast/struct.InlineAsmOutput.html
data InlineAsmOutput a
  = InlineAsmOutput {
      constraint :: InternedString,
      expr :: Expr a,
      isRw :: Bool,
      isIndirect :: Bool
    }

-- https://docs.serde.rs/syntex_syntax/ast/enum.IntTy.html
data IntTy = Is | I8 | I16 | I32 | I64 deriving (Eq, Enum, Bounded)

-- | An item
-- The name might be a dummy name in case of anonymous items
-- https://docs.serde.rs/syntex_syntax/ast/struct.Item.html
data Item a
  = Item {
      ident :: Ident a,
      attrs :: [Attribute a],
      node :: ItemKind a,
      vis :: Visibility a,
      nodeInfo :: a
    }

data ItemKind a
  -- | An extern crate item, with optional original crate name.
  -- E.g. extern crate foo or extern crate foo_bar as foo
  = ExternCrate (Maybe Name)
  -- | A use declaration (use or pub use) item.
  -- E.g. use foo;, use foo::bar; or use foo::bar as FooBar;
  | Use (ViewPath a)
  -- | A static item (static or pub static).
  -- E.g. static FOO: i32 = 42; or static FOO: &'static str = "bar";
  | Static (Ty a) Mutability (Expr a)
  -- | A constant item (const or pub const).
  -- E.g. const FOO: i32 = 42;
  | ConstItem (Ty a) (Expr a)
  -- | A function declaration (fn or pub fn).
  -- E.g. fn foo(bar: usize) -> usize { .. }
  | Fn (FnDecl a) Unsafety Constness Abi (Generics a) (Block a)
  -- | A module declaration (mod or pub mod).
  -- E.g. mod foo; or mod foo { .. }
  -- Inlined [Mod](https://docs.serde.rs/syntex_syntax/ast/struct.Mod.html)
  | Mod {
      inner :: Span,   -- ^ A span from the first token past { to the last token until }. For mod foo;, the inner span ranges from the first token to the last token in the external file.
      items :: [Item a]
  }
  -- | An external module (extern or pub extern).
  -- E.g. extern { .. } or extern C { .. }
  -- Inlined [ForeignMod](https://docs.serde.rs/syntex_syntax/ast/struct.ForeignMod.html)
  | ForeignMod { abi :: Abi, foreignItems :: [ForeignItem a] }
  -- | A type alias (type or pub type).
  -- E.g. type Foo = Bar<u8>;
  | TyAlias (Ty a) (Generics a)
  -- | An enum definition (enum or pub enum).
  -- E.g. enum Foo<A, B> { C<A>, D<B> }
  -- Inlined [EnumDef](https://docs.serde.rs/syntex_syntax/ast/struct.EnumDef.html)
  | Enum [Variant a] (Generics a)
  -- | A struct definition (struct or pub struct).
  -- E.g. struct Foo<A> { x: A }
  | StructItem (VariantData a) (Generics a)
  -- | A union definition (union or pub union).
  -- E.g. union Foo<A, B> { x: A, y: B }
  | Union (VariantData a) (Generics a)
  -- | A Trait declaration (trait or pub trait).
  -- E.g. trait Foo { .. } or trait Foo<T> { .. }
  | Trait Unsafety (Generics a) [TyParamBound a] [TraitItem a]
  -- | E.g. impl Trait for .. {} or impl<T> Trait<T> for .. {}
  | DefaultImpl Unsafety (TraitRef a)
  -- | An implementation.
  -- E.g. impl<A> Foo<A> { .. } or impl<A> Trait for Foo<A> { .. }
  | Impl Unsafety ImplPolarity (Generics a) (Maybe (TraitRef a)) (Ty a) [ImplItem a]
  -- | A macro invocation (which includes macro definition).
  -- E.g. macro_rules! foo { .. } or foo!(..)
  | MacItem (Mac a)

-- | A Kleene-style repetition operator for token sequences.
-- https://docs.serde.rs/syntex_syntax/tokenstream/enum.KleeneOp.html
data KleeneOp = ZeroOrMore | OneOrMore deriving (Eq, Enum, Bounded)

-- | A lifetime
-- https://docs.serde.rs/syntex_syntax/ast/struct.Lifetime.html
data Lifetime a
  = Lifetime {
      name :: Name,
      nodeInfo :: a
    }

-- | A lifetime definition, e.g. 'a: 'b+'c+'d
-- https://docs.serde.rs/syntex_syntax/ast/struct.LifetimeDef.html
data LifetimeDef a
  = LifetimeDef {
      attrs :: [Attribute a],
      lifetime :: Lifetime a,
      bounds :: [Lifetime a],
      nodeInfo :: a
    }

-- https://docs.serde.rs/syntex_syntax/ast/enum.LitIntType.html
data LitIntType = Signed IntTy | Unsigned UintTy | Unsuffixed

-- | Literal kind.
-- E.g. "foo", 42, 12.34 or bool
-- https://docs.serde.rs/syntex_syntax/ast/enum.LitKind.html
data Lit a
  = Str InternedString StrStyle a    -- ^ A string literal ("foo")
  | ByteStr [Word8] a                -- ^ A byte string (b"foo")    TODO: maybe ByteString?
  | Byte Word8 a                     -- ^ A byte char (b'f')
  | Char Char a                      -- ^ A character literal ('a')
  | Int Word64 LitIntType a          -- ^ An integer literal (1)
  | Float InternedString FloatTy a   -- ^ A float literal (1f64 or 1E10f64)
  | FloatUnsuffixed InternedString a -- ^ A float literal without a suffix (1.0 or 1.0E10)
  | Bool Bool a                      -- ^ A boolean literal

-- | Represents a macro invocation. The Path indicates which macro is being invoked, and the vector of
-- token-trees contains the source of the macro invocation.
--
-- NB: the additional ident for a macro_rules-style macro is actually stored in the enclosing item. Oog.
-- https://docs.serde.rs/syntex_syntax/ast/type.Mac.html
-- https://docs.serde.rs/syntex_syntax/ast/struct.Mac_.html
data Mac a
  = Mac {
      path :: Path a,
      tts :: [TokenTree],
      nodeInfo :: a
    }

-- https://docs.serde.rs/syntex_syntax/ast/enum.MacStmtStyle.html
data MacStmtStyle
  = Semicolon -- ^ The macro statement had a trailing semicolon, e.g. foo! { ... }; foo!(...);, foo![...];
  | Braces    -- ^ The macro statement had braces; e.g. foo! { ... }
  | NoBraces  -- ^ The macro statement had parentheses or brackets and no semicolon; e.g. foo!(...). All of these will end up being converted into macro expressions.

-- | A macro definition, in this crate or imported from another.
-- Not parsed directly, but created on macro import or macro_rules! expansion.
-- https://docs.serde.rs/syntex_syntax/ast/struct.MacroDef.html
data MacroDef a
  = MacroDef {
      ident :: Ident a,
      attrs :: [Attribute a],
      importedFrom :: Maybe (Ident a),
      export :: Bool,
      useLocally :: Bool,
      allowInternalUnstable :: Bool,
      body :: [TokenTree],
      nodeInfo :: a
}

-- | A compile-time attribute item.
-- E.g. #[test], #[derive(..)] or #[feature = "foo"]
-- https://docs.serde.rs/syntex_syntax/ast/type.MetaItem.html
-- https://docs.serde.rs/syntex_syntax/ast/enum.MetaItemKind.html
-- https://docs.serde.rs/syntex_syntax/ast/enum.NestedMetaItemKind.html
data MetaItem a
  = Word InternedString a                    -- ^ Word meta item.        E.g. test as in #[test]
  | List InternedString [NestedMetaItem a] a -- ^ List meta item.        E.g. derive(..) as in #[derive(..)]
  | NameValue InternedString (Lit a) a       -- ^ Name value meta item.  E.g. feature = "foo" as in #[feature = "foo"]

-- | Represents a method's signature in a trait declaration, or in an implementation.
-- https://docs.serde.rs/syntex_syntax/ast/struct.MethodSig.html
data MethodSig a
  = MethodSig {
      unsafety :: Unsafety,
      constness :: Constness,
      abi :: Abi,
      decl :: FnDecl a,
      generics :: Generics a
    }

-- https://docs.serde.rs/syntex_syntax/ast/enum.Mutability.html
data Mutability = Mutable | Immutable deriving (Eq, Enum, Bounded)

-- https://docs.serde.rs/syntex_syntax/ast/struct.MutTy.html
data MutTy a
  = MutTy {
      ty :: Ty a,
      mutbl :: Mutability
    }

-- | Possible values inside of compile-time attribute lists.
-- E.g. the '..' in #[name(..)].
-- https://docs.serde.rs/syntex_syntax/ast/enum.NestedMetaItemKind.html
data NestedMetaItem a
  = MetaItem (MetaItem a) a -- ^ A full MetaItem, for recursive meta items.
  | Literal (Lit a) a      -- ^ A literal. E.g. "foo", 64, true

-- | For interpolation during macro expansion.
-- https://docs.serde.rs/syntex_syntax/parse/token/enum.Nonterminal.html
data Nonterminal a
  = NtItem (Item a)
  | NtBlock (Block a)
  | NtStmt (Stmt a)
  | NtPat (Pat a)
  | NtExpr (Expr a)
  | NtTy (Ty a)
  | NtIdent (Ident a)
  | NtMeta (MetaItem a)
  | NtPath (Path a)
  | NtTT TokenTree
  | NtArm (Arm a)
  | NtImplItem (ImplItem a)
  | NtTraitItem (TraitItem a)
  | NtGenerics (Generics a)
  | NtWhereClause (WhereClause a)
  | NtArg (Arg a)

-- | A "Path" is essentially Rust's notion of a name.
-- It's represented as a sequence of identifiers, along with a bunch of supporting information.
-- E.g. `std::cmp::PartialEq`
-- https://docs.serde.rs/syntex_syntax/ast/struct.Path.html
-- Inlined [PathSegment](https://docs.serde.rs/syntex_syntax/ast/struct.PathSegment.html)
data Path a
  = Path {
      -- | A ::foo path, is relative to the crate root rather than current module (like paths in an import).
      global :: Bool,
      -- | The segments in the path: the things separated by ::.
      -- Each segment consists of an identifier, an optional lifetime, and a set of types. E.g. std, String or Box<T>
      segments :: [(Ident a, PathParameters a)],
      nodeInfo :: a
    }

-- https://docs.serde.rs/syntex_syntax/ast/struct.Pat.html
-- Inlined [PatKind][https://docs.serde.rs/syntex_syntax/ast/enum.PatKind.html]
data Pat a
  -- | Represents a wildcard pattern (_)
  = WildP a
  -- | A PatKind::Ident may either be a new bound variable (ref mut binding @ OPT_SUBPATTERN), or a unit
  -- struct/variant pattern, or a const pattern (in the last two cases the third field must be None). Disambiguation
  -- cannot be done with parser alone, so it happens during name resolution.
  | IdentP BindingMode (Ident a) (Maybe (Pat a)) a
  -- | A struct or struct variant pattern, e.g. Variant {x, y, ..}. The bool is true in the presence of a ...
  | StructP (Path a) [FieldPat a] Bool a
  -- | A tuple struct/variant pattern Variant(x, y, .., z). If the .. pattern fragment is present, then
  -- (Maybe usize) denotes its position. 0 <= position <= subpats.len()
  | TupleStructP (Path a) [Pat a] (Maybe Word64) a
  -- | A possibly qualified path pattern. Unquailfied path patterns A::B::C can legally refer to variants, structs,
  -- constants or associated constants. Quailfied path patterns <A>::B::C/<A as Trait>::B::C can only legally refer to
  -- associated constants.
  | PathP (Maybe (QSelf a)) (Path a) a
  -- | A tuple pattern (a, b). If the .. pattern fragment is present, then (Maybe usize) denotes its position.
  -- 0 <= position <= subpats.len()
  | TupleP [Pat a] (Maybe Word64) a
  -- | A box pattern
  | BoxP (Pat a) a
  -- | A reference pattern, e.g. &mut (a, b)
  | RefP (Pat a) Mutability a
  -- | A literal
  | LitP (Expr a) a
  -- | A range pattern, e.g. 1...2
  | RangeP (Expr a) (Expr a) a
  -- | [a, b, ..i, y, z] is represented as: PatKind::Slice(box [a, b], Some(i), box [y, z])
  | SliceP [Pat a] (Maybe (Pat a)) [Pat a] a
  -- | A macro pattern; pre-expansion
  | MacP (Mac a) a

-- https://docs.serde.rs/syntex_syntax/ast/type.PathListItem.html
-- https://docs.serde.rs/syntex_syntax/ast/struct.PathListItem_.html
data PathListItem a
  = PathListItem {
      name :: Ident a,
      rename :: Maybe (Ident a), -- ^ renamed in list, e.g. `use foo::{bar as baz};`
      nodeInfo :: a
    }

-- | Parameters of a path segment.
-- E.g. <A, B> as in Foo<A, B> or (A, B) as in Foo(A, B)
-- https://docs.serde.rs/syntex_syntax/ast/enum.PathParameters.html
data PathParameters a
  -- | The <'a, A,B,C> in foo::bar::baz::<'a, A,B,C> - A path like `Foo<'a, T>`
  -- Inlined [AngleBracketedParameterData](https://docs.serde.rs/syntex_syntax/ast/struct.AngleBracketedParameterData.html)
  -- Inlinde [TypeBinding](https://docs.serde.rs/syntex_syntax/ast/struct.TypeBinding.html)
  = AngleBracketed {
      lifetimes :: [Lifetime a],     -- ^ The lifetime parameters for this path segment.
      types :: [Ty a],               -- ^ The type parameters for this path segment, if present.
      bindings :: [(Ident a, Ty a)], -- ^ Bindings (equality constraints) on associated types, if present. E.g., `Foo<A=Bar>`.
      nodeInfo :: a
    }
  -- | A path like `Foo(A,B) -> C`
  -- Inlined [ParenthesizedParameterData](https://docs.serde.rs/syntex_syntax/ast/struct.ParenthesizedParameterData.html)
  | Parenthesized {
      inputs :: [Ty a],       -- ^ `(A,B)`
      output :: Maybe (Ty a), -- ^ `C`
      nodeInfo :: a
    }

-- https://docs.serde.rs/syntex_syntax/ast/struct.PolyTraitRef.html
data PolyTraitRef a
  = PolyTraitRef {
      boundLifetimes :: [LifetimeDef a], -- ^ The `'a` in `<'a> Foo<&'a T>`
      traitRef :: TraitRef a,            -- ^ The `Foo<&'a T>` in `<'a> Foo<&'a T>`
      nodeInfo :: a
    }

-- |The explicit Self type in a "qualified path". The actual path, including the trait and the associated item, is stored
-- separately. position represents the index of the associated item qualified with this Self type.

-- <Vec<T> as a::b::Trait>::AssociatedItem
--  ^~~~~     ~~~~~~~~~~~~~~^
--  ty        position = 3

-- <Vec<T>>::AssociatedItem
--  ^~~~~    ^
--  ty       position = 0 
-- https://docs.serde.rs/syntex_syntax/ast/struct.QSelf.html
data QSelf a
  = QSelf {
      ty :: Ty a,
      position :: Int
    }

-- | Limit types of a range (inclusive or exclusive)
-- https://docs.serde.rs/syntex_syntax/ast/enum.RangeLimits.html
data RangeLimits
  = HalfOpen -- ^ Inclusive at the beginning, exclusive at the end
  | Closed   -- ^ Inclusive at the beginning and end
  deriving (Eq, Enum, Bounded)

-- | Alternative representation for Args describing self parameter of methods.
-- E.g. &mut self as in fn foo(&mut self)
-- https://docs.serde.rs/syntex_syntax/ast/enum.SelfKind.html
data SelfKind a
  = ValueSelf Mutability                    -- ^ self, mut self
  | Region (Maybe (Lifetime a)) Mutability  -- ^ &'lt self, &'lt mut self
  | Explicit (Ty a) Mutability              -- ^ self: TYPE, mut self: TYPE

-- | A statement.
-- https://docs.serde.rs/syntex_syntax/ast/struct.Stmt.html
-- Inlined [StmtKind](https://docs.serde.rs/syntex_syntax/ast/enum.StmtKind.html)
data Stmt a
  -- | A local (let) binding, e.g., let <pat>:<ty> = <expr>;
  -- Inlined [Local](https://docs.serde.rs/syntex_syntax/ast/struct.Local.html)
  = Local {
      pat :: Pat a,
      ty :: Maybe (Ty a),
      init :: Maybe (Expr a),          -- ^ Initializer expression to set the value, if any
      attrs :: [Attribute a],
      nodeInfo :: a
    }
  | ItemStmt (Item a) a  -- ^ An item definition.
  | NoSemi (Expr a) a    -- ^ Expr without trailing semi-colon.
  | Semi (Expr a) a
  | MacStmt (Mac a) MacStmtStyle [Attribute a] a

-- https://docs.serde.rs/syntex_syntax/ast/enum.StrStyle.html
data StrStyle
  = Cooked     -- ^ A regular string, like "foo"
  | Raw Word64 -- ^ A raw string, like r##"foo"##. The uint is the number of # symbols used

-- | Field of a struct. E.g. bar: usize as in struct Foo { bar: usize }
-- https://docs.serde.rs/syntex_syntax/ast/struct.StructField.html
data StructField a
  = StructField {
      ident :: Maybe (Ident a),
      vis :: Visibility a,
      ty :: Ty a,
      attrs :: [Attribute a],
      nodeInfo :: a
   }

-- https://docs.serde.rs/syntex_syntax/parse/token/enum.Token.html
data Token
  = Eq | Lt | Le | EqEq | Ne | Ge | Gt | AndAnd | OrOr | NotToken | Tilde | BinOp Token.BinOpToken
  | BinOpEq Token.BinOpToken | At | Dot | DotDot | DotDotDot | Comma | SemiToken | Colon | ModSep
  | RArrow | LArrow | FatArrow | Pound | Dollar | Question
  | OpenDelim Token.DelimToken    -- ^ An opening delimiter, eg. {
  | CloseDelim Token.DelimToken   -- ^ A closing delimiter, eg. }
  | LiteralToken (Lit ()) (Maybe Name) | IdentToken (Ident ()) | Underscore | LifetimeToken (Ident ()) | Interpolated (Nonterminal ())
  | DocComment Name               -- ^ Doc comment
  | MatchNt (Ident ()) (Ident ()) -- ^ Parse a nonterminal (name to bind, name of NT)
  | SubstNt (Ident ())            -- ^ A syntactic variable that will be filled in by macro expansion.
  | SpecialMacroVar               -- ^ A macro variable with special meaning.
  | Whitespace                    -- ^ Whitespace
  | Comment                       -- ^ Comment
  | Shebang Name | Eof

-- | When the main rust parser encounters a syntax-extension invocation, it parses the arguments to the invocation
-- as a token-tree. This is a very loose structure, such that all sorts of different AST-fragments can be passed to
-- syntax extensions using a uniform type.
--
-- If the syntax extension is an MBE macro, it will attempt to match its LHS token tree against the provided token
-- tree, and if it finds a match, will transcribe the RHS token tree, splicing in any captured
-- macro_parser::matched_nonterminals into the SubstNts it finds.
--
-- The RHS of an MBE macro is the only place SubstNts are substituted. Nothing special happens to misnamed or
-- misplaced SubstNts.
--
-- https://docs.serde.rs/syntex_syntax/tokenstream/enum.TokenTree.html
data TokenTree
  -- | A single token
  = Token Span Token
  -- | A delimited sequence of token trees
  -- Inlined [Delimited](https://docs.serde.rs/syntex_syntax/tokenstream/struct.Delimited.html)
  | Delimited {
      span :: Span,
      delim :: Token.DelimToken, -- ^ The type of delimiter
      open_span :: Span,         -- ^ The span covering the opening delimiter
      tts :: [TokenTree],        -- ^ The delimited sequence of token trees
      close_span :: Span         -- ^ The span covering the closing delimiter
    }
  -- | A kleene-style repetition sequence of token trees with a span
  -- Inlined [SequenceRepetition](https://docs.serde.rs/syntex_syntax/tokenstream/struct.SequenceRepetition.html)
  | Sequence {
      span :: Span,
      tts :: [TokenTree],       -- ^ The sequence of token trees
      separator :: Maybe Token, -- ^ The optional separator
      op :: KleeneOp,           -- ^ Whether the sequence can be repeated zero (*), or one or more times (+)
      num_captures :: Word64    -- ^ The number of MatchNts that appear in the sequence (and subsequences)
    }

-- | A modifier on a bound, currently this is only used for ?Sized, where the modifier is Maybe. Negative
-- bounds should also be handled here.
data TraitBoundModifier = None | Maybe

-- | Represents an item declaration within a trait declaration, possibly including a default implementation.
-- A trait item is either required (meaning it doesn't have an implementation, just a signature) or provided
-- (meaning it has a default implementation).
-- https://docs.serde.rs/syntex_syntax/ast/struct.TraitItem.html
data TraitItem a
  = TraitItem {
      ident :: Ident a,
      attrs :: [Attribute a],
      node :: TraitItemKind a,
      nodeInfo :: a
    }

-- https://docs.serde.rs/syntex_syntax/ast/enum.TraitItemKind.html
data TraitItemKind a
  = ConstT (Ty a) (Maybe (Expr a))
  | MethodT (MethodSig a) (Maybe (Block a))
  | TypeT [TyParamBound a] (Maybe (Ty a))
  | MacroT (Mac a)

-- | TraitRef's appear in impls.
-- resolve maps each TraitRef's ref_id to its defining trait; that's all that the ref_id is for.
-- The impl_id maps to the "self type" of this impl. If this impl is an ItemKind::Impl, the impl_id
-- is redundant (it could be the same as the impl's node id).
-- https://docs.serde.rs/syntex_syntax/ast/struct.TraitRef.html
data TraitRef a
  = TraitRef {
      path :: Path a,
      nodeInfo :: a
    }

-- | The different kinds of types recognized by the compiler
-- Inlined [TyKind](https://docs.serde.rs/syntex_syntax/ast/enum.TyKind.html)
-- https://docs.serde.rs/syntex_syntax/ast/struct.Ty.html
data Ty a
  -- | A variable-length slice ([T])
  = Slice (Ty a) a
  -- | A fixed length array ([T; n])
  | Array (Ty a) (Expr a) a
  -- | A raw pointer (*const T or *mut T)
  | Ptr (MutTy a) a
  -- | A reference (&'a T or &'a mut T)
  | Rptr (Maybe (Lifetime a)) (MutTy a) a
  -- | A bare function (e.g. fn(usize) -> bool)
  -- Inlined [BareFnTy](-- https://docs.serde.rs/syntex_syntax/ast/struct.BareFnTy.html)
  | BareFn Unsafety Abi [LifetimeDef a] (FnDecl a) a
  -- | The never type (!)
  | Never a
  -- | A tuple ((A, B, C, D,...))
  | TupTy [Ty a] a
  -- | A path (module::module::...::Type), optionally "qualified", e.g. <Vec<T> as SomeTrait>::SomeType.
  -- Type parameters are stored in the Path itself
  | PathTy (Maybe (QSelf a)) (Path a) a
  -- | Something like A+B. Note that B must always be a path.
  | ObjectSum (Ty a) [TyParamBound a] a
  -- | A type like for<'a> Foo<&'a Bar>
  | PolyTraitRefTy [TyParamBound a] a
  -- | An impl TraitA+TraitB type.
  | ImplTrait [TyParamBound a] a
  -- | No-op; kept solely so that we can pretty-print faithfully
  | ParenTy (Ty a) a
  -- | Unused for now
  | Typeof (Expr a) a
  -- | TyKind::Infer means the type should be inferred instead of it having been specified. This can appear anywhere in a type.
  | Infer a
  -- | Inferred type of a self or &self argument in a method.
  | ImplicitSelf a
  | MacTy (Mac a) a

-- https://docs.serde.rs/syntex_syntax/ast/struct.TyParam.html
data TyParam a
  = TyParam {
      attrs :: [Attribute a],
      ident :: Ident a,
      bounds :: [TyParamBound a],
      default_ :: Maybe (Ty a),
      nodeInfo :: a
    }

-- | The AST represents all type param bounds as types. typeck::collect::compute_bounds matches these against the
-- "special" built-in traits (see middle::lang_items) and detects Copy, Send and Sync.
-- https://docs.serde.rs/syntex_syntax/ast/enum.TyParamBound.html
data TyParamBound a
  = TraitTyParamBound (PolyTraitRef a) TraitBoundModifier
  | RegionTyParamBound (Lifetime a)

-- https://docs.serde.rs/syntex_syntax/ast/enum.UintTy.html
data UintTy = Us | U8 | U16 | U32 | U64 deriving (Eq, Enum, Bounded)

-- https://docs.serde.rs/syntex_syntax/ast/enum.UnOp.html
data UnOp 
  = Deref -- ^ The * operator for dereferencing
  | Not   -- ^ the ! operator for logical inversion
  | Neg   -- ^ the - operator for negation
  deriving (Eq, Enum, Bounded)

-- https://docs.serde.rs/syntex_syntax/ast/enum.Unsafety.html
data Unsafety = Unsafe | Normal deriving (Eq, Enum, Bounded)

-- https://docs.serde.rs/syntex_syntax/ast/type.Variant.html
-- https://docs.serde.rs/syntex_syntax/ast/struct.Variant_.html
data Variant a
  = Variant {
      name :: Ident a,
      attrs :: [Attribute a],
      data_ :: VariantData a,
      disrExpr :: Maybe (Expr a), -- ^ Explicit discriminant, e.g. Foo = 1
      nodeInfo :: a
    }

-- | Fields and Ids of enum variants and structs
-- 
-- https://docs.serde.rs/syntex_syntax/ast/enum.VariantData.html
data VariantData a
  = StructD [StructField a] a -- ^ Struct variant. E.g. Bar { .. } as in enum Foo { Bar { .. } }
  | TupleD [StructField a] a  -- ^ Tuple variant. E.g. Bar(..) as in enum Foo { Bar(..) }
  | UnitD a                   -- ^ Unit variant. E.g. Bar = .. as in enum Foo { Bar = .. }

-- https://docs.serde.rs/syntex_syntax/ast/type.ViewPath.html
-- https://docs.serde.rs/syntex_syntax/ast/enum.ViewPath_.html
data ViewPath a
  -- | `foo::bar::baz as quux` or just `foo::bar::baz` (with `as baz` implicitly on the right)
  = ViewPathSimple (Ident a) (Path a) a
  -- | foo::bar::*
  | ViewPathGlob (Path a) a
  -- | foo::bar::{a,b,c}
  | ViewPathList (Path a) [PathListItem a] a

-- https://docs.serde.rs/syntex_syntax/ast/enum.Visibility.html
data Visibility a
  = PublicV a
  | CrateV a
  | RestrictedV { path :: Path a, nodeInfo :: a }
  | InheritedV a

-- | A `where` clause in a definition
-- https://docs.serde.rs/syntex_syntax/ast/struct.WhereClause.html
data WhereClause a
  = WhereClause {
      predicates :: [WherePredicate a],
      nodeInfo :: a
    }

-- | A single predicate in a where clause
-- https://docs.serde.rs/syntex_syntax/ast/enum.WherePredicate.html
data WherePredicate a
  -- | A type bound. E.g. `for<'c> Foo: Send+Clone+'c`
  -- Inlined [WhereBoundPredicate](https://docs.serde.rs/syntex_syntax/ast/struct.WhereBoundPredicate.html)
  = BoundPredicate {
      boundLifetimes :: [LifetimeDef a],       -- ^ Any lifetimes from a for binding
      boundedTy :: Ty a,                       -- ^ The type being bounded
      traitLifetimeBounds :: [TyParamBound a], -- ^ Trait and lifetime bounds (Clone+Send+'static)
      nodeInfo :: a
    }
  -- | A lifetime predicate, e.g. 'a: 'b+'c
  -- Inlined [WhereRegionPredicate](https://docs.serde.rs/syntex_syntax/ast/struct.WhereRegionPredicate.html)
  | RegionPredicate {
      lifetime :: Lifetime a,
      lifetimeBounds :: [Lifetime a],
      nodeInfo :: a
    }
  -- | An equality predicate (unsupported), e.g. T=int
  -- Inlined [WereEqPredicate](https://docs.serde.rs/syntex_syntax/ast/struct.WhereEqPredicate.html)
  | EqPredicate  {
      path :: Path a,
      ty :: Ty a,
      nodeInfo :: a
    }         
