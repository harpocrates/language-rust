{
{-|
Module      : Language.Rust.Parser.Lexer
Description : Rust lexer
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : portable

As much as possible, this follows Rust's choices for tokenization, including punting some things to
the parser. For instance, the last two @>@ in @Vec\<Option\<i32\>\>@ are lexed as a single
'GreaterGreater' token while the last two tokens of @Vec\<Option\<Option\<i32\>\>\>@ are
'GreaterGreater' and 'Greater'.

Yet weirder (but very useful in parsing for dealing with conflicts and precedences of logical and,
bitwise and, and unary reference), @&&&x&&&y@ lexes into 'AmpersandAmpersand', 'Ampersand',
@'IdentTok' "x"@, 'AmpersandAmpersand', 'Ampersand', @'IdentTok' "y"@. Although the parser sometimes
needs to "break apart" tokens, it never has to think about putting them together. That means it can
easily figure out that @&&&x&&&y@ parses as @&(&(&x)) && (&y)@ and not @&(&(&x)) & (&(&y))@ even if
bitwise conjunctions binds more tightly that logical conjunctions. 

This sort of amguity where one tokens need to be broken up occurs for

   * @&&@ in patterns like @&&mut x@
   * @||@ in closures with no arguments like @|| x@
   * @<<@ in qualified type paths like @FromIterator\<\<A as IntoIterator\>::Item\>@
   * @>>@ in qualified paths like @\<Self as Foo\<T\>\>::Bar@
   * @>=@ possibly in equality predicates like @F\<A\>=i32@ (not yet in Rust)
   * @>>=@ possibly in equality predicates?
-}

module Language.Rust.Parser.Lexer (
  -- * Lexing
  lexToken, lexNonSpace, lexTokens, lexShebangLine,
  -- * Tokens
  Token(..),
  -- * Error reporting
  lexicalError,
) where

import Language.Rust.Data.InputStream
import Language.Rust.Data.Position
import Language.Rust.Parser.ParseMonad
import Language.Rust.Syntax.Token 
import Language.Rust.Syntax.Ident (mkIdent, Ident(..))

import Data.Word (Word8)
import Data.Char (chr)
import Data.Functor (($>))

-- Things to review:
--   * improved error messages

-- Based heavily on:
--  * <https://github.com/rust-lang/rust/blob/master/src/grammar/RustLexer.g4>
--  * <https://github.com/rust-lang/rust/blob/master/src/grammar/xidstart.g4>
--  * <https://github.com/rust-lang/rust/blob/master/src/grammar/xidcontinue.g4>

}

-- XID_START unicode character class
@xid_start
  = [\x0041-\x005a]
  | "_"
  | [\x0061-\x007a]
  | \x00aa
  | \x00b5
  | \x00ba
  | [\x00c0-\x00d6]
  | [\x00d8-\x00f6]
  | [\x00f8-\x0236]
  | [\x0250-\x02c1]
  | [\x02c6-\x02d1]
  | [\x02e0-\x02e4]
  | \x02ee
  | \x0386
  | [\x0388-\x038a]
  | \x038c
  | [\x038e-\x03a1]
  | [\x03a3-\x03ce]
  | [\x03d0-\x03f5]
  | [\x03f7-\x03fb]
  | [\x0400-\x0481]
  | [\x048a-\x04ce]
  | [\x04d0-\x04f5]
  | [\x04f8-\x04f9]
  | [\x0500-\x050f]
  | [\x0531-\x0556]
  | \x0559
  | [\x0561-\x0587]
  | [\x05d0-\x05ea]
  | [\x05f0-\x05f2]
  | [\x0621-\x063a]
  | [\x0640-\x064a]
  | [\x066e-\x066f]
  | [\x0671-\x06d3]
  | \x06d5
  | [\x06e5-\x06e6]
  | [\x06ee-\x06ef]
  | [\x06fa-\x06fc]
  | \x06ff
  | \x0710
  | [\x0712-\x072f]
  | [\x074d-\x074f]
  | [\x0780-\x07a5]
  | \x07b1
  | [\x0904-\x0939]
  | \x093d
  | \x0950
  | [\x0958-\x0961]
  | [\x0985-\x098c]
  | [\x098f-\x0990]
  | [\x0993-\x09a8]
  | [\x09aa-\x09b0]
  | \x09b2
  | [\x09b6-\x09b9]
  | \x09bd
  | [\x09dc-\x09dd]
  | [\x09df-\x09e1]
  | [\x09f0-\x09f1]
  | [\x0a05-\x0a0a]
  | [\x0a0f-\x0a10]
  | [\x0a13-\x0a28]
  | [\x0a2a-\x0a30]
  | [\x0a32-\x0a33]
  | [\x0a35-\x0a36]
  | [\x0a38-\x0a39]
  | [\x0a59-\x0a5c]
  | \x0a5e
  | [\x0a72-\x0a74]
  | [\x0a85-\x0a8d]
  | [\x0a8f-\x0a91]
  | [\x0a93-\x0aa8]
  | [\x0aaa-\x0ab0]
  | [\x0ab2-\x0ab3]
  | [\x0ab5-\x0ab9]
  | \x0abd
  | \x0ad0
  | [\x0ae0-\x0ae1]
  | [\x0b05-\x0b0c]
  | [\x0b0f-\x0b10]
  | [\x0b13-\x0b28]
  | [\x0b2a-\x0b30]
  | [\x0b32-\x0b33]
  | [\x0b35-\x0b39]
  | \x0b3d
  | [\x0b5c-\x0b5d]
  | [\x0b5f-\x0b61]
  | \x0b71
  | \x0b83
  | [\x0b85-\x0b8a]
  | [\x0b8e-\x0b90]
  | [\x0b92-\x0b95]
  | [\x0b99-\x0b9a]
  | \x0b9c
  | [\x0b9e-\x0b9f]
  | [\x0ba3-\x0ba4]
  | [\x0ba8-\x0baa]
  | [\x0bae-\x0bb5]
  | [\x0bb7-\x0bb9]
  | [\x0c05-\x0c0c]
  | [\x0c0e-\x0c10]
  | [\x0c12-\x0c28]
  | [\x0c2a-\x0c33]
  | [\x0c35-\x0c39]
  | [\x0c60-\x0c61]
  | [\x0c85-\x0c8c]
  | [\x0c8e-\x0c90]
  | [\x0c92-\x0ca8]
  | [\x0caa-\x0cb3]
  | [\x0cb5-\x0cb9]
  | \x0cbd
  | \x0cde
  | [\x0ce0-\x0ce1]
  | [\x0d05-\x0d0c]
  | [\x0d0e-\x0d10]
  | [\x0d12-\x0d28]
  | [\x0d2a-\x0d39]
  | [\x0d60-\x0d61]
  | [\x0d85-\x0d96]
  | [\x0d9a-\x0db1]
  | [\x0db3-\x0dbb]
  | \x0dbd
  | [\x0dc0-\x0dc6]
  | [\x0e01-\x0e30]
  | \x0e32
  | [\x0e40-\x0e46]
  | [\x0e81-\x0e82]
  | \x0e84
  | [\x0e87-\x0e88]
  | \x0e8a
  | \x0e8d
  | [\x0e94-\x0e97]
  | [\x0e99-\x0e9f]
  | [\x0ea1-\x0ea3]
  | \x0ea5
  | \x0ea7
  | [\x0eaa-\x0eab]
  | [\x0ead-\x0eb0]
  | \x0eb2
  | \x0ebd
  | [\x0ec0-\x0ec4]
  | \x0ec6
  | [\x0edc-\x0edd]
  | \x0f00
  | [\x0f40-\x0f47]
  | [\x0f49-\x0f6a]
  | [\x0f88-\x0f8b]
  | [\x1000-\x1021]
  | [\x1023-\x1027]
  | [\x1029-\x102a]
  | [\x1050-\x1055]
  | [\x10a0-\x10c5]
  | [\x10d0-\x10f8]
  | [\x1100-\x1159]
  | [\x115f-\x11a2]
  | [\x11a8-\x11f9]
  | [\x1200-\x1206]
  | [\x1208-\x1246]
  | \x1248
  | [\x124a-\x124d]
  | [\x1250-\x1256]
  | \x1258
  | [\x125a-\x125d]
  | [\x1260-\x1286]
  | \x1288
  | [\x128a-\x128d]
  | [\x1290-\x12ae]
  | \x12b0
  | [\x12b2-\x12b5]
  | [\x12b8-\x12be]
  | \x12c0
  | [\x12c2-\x12c5]
  | [\x12c8-\x12ce]
  | [\x12d0-\x12d6]
  | [\x12d8-\x12ee]
  | [\x12f0-\x130e]
  | \x1310
  | [\x1312-\x1315]
  | [\x1318-\x131e]
  | [\x1320-\x1346]
  | [\x1348-\x135a]
  | [\x13a0-\x13f4]
  | [\x1401-\x166c]
  | [\x166f-\x1676]
  | [\x1681-\x169a]
  | [\x16a0-\x16ea]
  | [\x16ee-\x16f0]
  | [\x1700-\x170c]
  | [\x170e-\x1711]
  | [\x1720-\x1731]
  | [\x1740-\x1751]
  | [\x1760-\x176c]
  | [\x176e-\x1770]
  | [\x1780-\x17b3]
  | \x17d7
  | \x17dc
  | [\x1820-\x1877]
  | [\x1880-\x18a8]
  | [\x1900-\x191c]
  | [\x1950-\x196d]
  | [\x1970-\x1974]
  | [\x1d00-\x1d6b]
  | [\x1e00-\x1e9b]
  | [\x1ea0-\x1ef9]
  | [\x1f00-\x1f15]
  | [\x1f18-\x1f1d]
  | [\x1f20-\x1f45]
  | [\x1f48-\x1f4d]
  | [\x1f50-\x1f57]
  | \x1f59
  | \x1f5b
  | \x1f5d
  | [\x1f5f-\x1f7d]
  | [\x1f80-\x1fb4]
  | [\x1fb6-\x1fbc]
  | \x1fbe
  | [\x1fc2-\x1fc4]
  | [\x1fc6-\x1fcc]
  | [\x1fd0-\x1fd3]
  | [\x1fd6-\x1fdb]
  | [\x1fe0-\x1fec]
  | [\x1ff2-\x1ff4]
  | [\x1ff6-\x1ffc]
  | \x2071
  | \x207f
  | \x2102
  | \x2107
  | [\x210a-\x2113]
  | \x2115
  | [\x2118-\x211d]
  | \x2124
  | \x2126
  | \x2128
  | [\x212a-\x2131]
  | [\x2133-\x2139]
  | [\x213d-\x213f]
  | [\x2145-\x2149]
  | [\x2160-\x2183]
  | [\x3005-\x3007]
  | [\x3021-\x3029]
  | [\x3031-\x3035]
  | [\x3038-\x303c]
  | [\x3041-\x3096]
  | [\x309d-\x309f]
  | [\x30a1-\x30fa]
  | [\x30fc-\x30ff]
  | [\x3105-\x312c]
  | [\x3131-\x318e]
  | [\x31a0-\x31b7]
  | [\x31f0-\x31ff]
  | [\x3400-\x4db5]
  | [\x4e00-\x9fa5]
  | [\xa000-\xa48c]
  | [\xac00-\xd7a3]
  | [\xf900-\xfa2d]
  | [\xfa30-\xfa6a]
  | [\xfb00-\xfb06]
  | [\xfb13-\xfb17]
  | \xfb1d
  | [\xfb1f-\xfb28]
  | [\xfb2a-\xfb36]
  | [\xfb38-\xfb3c]
  | \xfb3e
  | [\xfb40-\xfb41]
  | [\xfb43-\xfb44]
  | [\xfb46-\xfbb1]
  | [\xfbd3-\xfc5d]
  | [\xfc64-\xfd3d]
  | [\xfd50-\xfd8f]
  | [\xfd92-\xfdc7]
  | [\xfdf0-\xfdf9]
  | \xfe71
  | \xfe73
  | \xfe77
  | \xfe79
  | \xfe7b
  | \xfe7d
  | [\xfe7f-\xfefc]
  | [\xff21-\xff3a]
  | [\xff41-\xff5a]
  | [\xff66-\xff9d]
  | [\xffa0-\xffbe]
  | [\xffc2-\xffc7]
  | [\xffca-\xffcf]
  | [\xffd2-\xffd7]
  | [\xffda-\xffdc]
  | \xd800 [\xdc00-\xdc0a]
  | \xd800 [\xdc0d-\xdc25]
  | \xd800 [\xdc28-\xdc39]
  | \xd800 [\xdc3c-\xdc3c]
  | \xd800 [\xdc3f-\xdc4c]
  | \xd800 [\xdc50-\xdc5c]
  | \xd800 [\xdc80-\xdcf9]
  | \xd800 [\xdf00-\xdf1d]
  | \xd800 [\xdf30-\xdf49]
  | \xd800 [\xdf80-\xdf9c]
  | \xd801 [\xe000-\xe09c]
  | \xd802 [\xe400-\xe404]
  | \xd802 \x0808
  | \xd802 [\xe40a-\xe434]
  | \xd802 [\xe437-\xe437]
  | \xd802 \x083c
  | \xd802 \x083f
  | \xd835 [\xb000-\xb053]
  | \xd835 [\xb056-\xb09b]
  | \xd835 [\xb09e-\xb09e]
  | \xd835 \xd4a2
  | \xd835 [\xb0a5-\xb0a5]
  | \xd835 [\xb0a9-\xb0ab]
  | \xd835 [\xb0ae-\xb0b8]
  | \xd835 \xd4bb
  | \xd835 [\xb0bd-\xb0c2]
  | \xd835 [\xb0c5-\xb104]
  | \xd835 [\xb107-\xb109]
  | \xd835 [\xb10d-\xb113]
  | \xd835 [\xb116-\xb11b]
  | \xd835 [\xb11e-\xb138]
  | \xd835 [\xb13b-\xb13d]
  | \xd835 [\xb140-\xb143]
  | \xd835 \xd546
  | \xd835 [\xb14a-\xb14f]
  | \xd835 [\xb152-\xb2a2]
  | \xd835 [\xb2a8-\xb2bf]
  | \xd835 [\xb2c2-\xb2d9]
  | \xd835 [\xb2dc-\xb2f9]
  | \xd835 [\xb2fc-\xb313]
  | \xd835 [\xb316-\xb333]
  | \xd835 [\xb336-\xb34d]
  | \xd835 [\xb350-\xb36d]
  | \xd835 [\xb370-\xb387]
  | \xd835 [\xb38a-\xb3a7]
  | \xd835 [\xb3aa-\xb3c1]
  | \xd835 [\xb3c4-\xb3c8]
  | \xd840 [\xdc00-\xdffe]
  | \xd841 [\xe000-\xe3fe]
  | \xd842 [\xe400-\xe7fe]
  | \xd843 [\xe800-\xebfe]
  | \xd844 [\xec00-\xeffe]
  | \xd845 [\xf000-\xf3fe]
  | \xd846 [\xf400-\xf7fe]
  | \xd847 [\xf800-\xfbfe]
  | \xd848 [\xfc00-\xfffe]
  | \xd849 [\x0000-\x03fe]
  | \xd84a [\x0400-\x07fe]
  | \xd84b [\x0800-\x0bfe]
  | \xd84c [\x0c00-\x0ffe]
  | \xd84d [\x1000-\x13fe]
  | \xd84e [\x1400-\x17fe]
  | \xd84f [\x1800-\x1bfe]
  | \xd850 [\x1c00-\x1ffe]
  | \xd851 [\x2000-\x23fe]
  | \xd852 [\x2400-\x27fe]
  | \xd853 [\x2800-\x2bfe]
  | \xd854 [\x2c00-\x2ffe]
  | \xd855 [\x3000-\x33fe]
  | \xd856 [\x3400-\x37fe]
  | \xd857 [\x3800-\x3bfe]
  | \xd858 [\x3c00-\x3ffe]
  | \xd859 [\x4000-\x43fe]
  | \xd85a [\x4400-\x47fe]
  | \xd85b [\x4800-\x4bfe]
  | \xd85c [\x4c00-\x4ffe]
  | \xd85d [\x5000-\x53fe]
  | \xd85e [\x5400-\x57fe]
  | \xd85f [\x5800-\x5bfe]
  | \xd860 [\x5c00-\x5ffe]
  | \xd861 [\x6000-\x63fe]
  | \xd862 [\x6400-\x67fe]
  | \xd863 [\x6800-\x6bfe]
  | \xd864 [\x6c00-\x6ffe]
  | \xd865 [\x7000-\x73fe]
  | \xd866 [\x7400-\x77fe]
  | \xd867 [\x7800-\x7bfe]
  | \xd868 [\x7c00-\x7ffe]
  | \xd869 [\x8000-\x82d5]
  | \xd87e [\xd400-\xd61c]

-- XID_CONTINUE unicode character class
@xid_continue
  = [\x0030-\x0039]
  | [\x0041-\x005a]
  | \x005f
  | [\x0061-\x007a]
  | \x00aa
  | \x00b5
  | \x00b7
  | \x00ba
  | [\x00c0-\x00d6]
  | [\x00d8-\x00f6]
  | [\x00f8-\x0236]
  | [\x0250-\x02c1]
  | [\x02c6-\x02d1]
  | [\x02e0-\x02e4]
  | \x02ee
  | [\x0300-\x0357]
  | [\x035d-\x036f]
  | \x0386
  | [\x0388-\x038a]
  | \x038c
  | [\x038e-\x03a1]
  | [\x03a3-\x03ce]
  | [\x03d0-\x03f5]
  | [\x03f7-\x03fb]
  | [\x0400-\x0481]
  | [\x0483-\x0486]
  | [\x048a-\x04ce]
  | [\x04d0-\x04f5]
  | [\x04f8-\x04f9]
  | [\x0500-\x050f]
  | [\x0531-\x0556]
  | \x0559
  | [\x0561-\x0587]
  | [\x0591-\x05a1]
  | [\x05a3-\x05b9]
  | [\x05bb-\x05bd]
  | \x05bf
  | [\x05c1-\x05c2]
  | \x05c4
  | [\x05d0-\x05ea]
  | [\x05f0-\x05f2]
  | [\x0610-\x0615]
  | [\x0621-\x063a]
  | [\x0640-\x0658]
  | [\x0660-\x0669]
  | [\x066e-\x06d3]
  | [\x06d5-\x06dc]
  | [\x06df-\x06e8]
  | [\x06ea-\x06fc]
  | \x06ff
  | [\x0710-\x074a]
  | [\x074d-\x074f]
  | [\x0780-\x07b1]
  | [\x0901-\x0939]
  | [\x093c-\x094d]
  | [\x0950-\x0954]
  | [\x0958-\x0963]
  | [\x0966-\x096f]
  | [\x0981-\x0983]
  | [\x0985-\x098c]
  | [\x098f-\x0990]
  | [\x0993-\x09a8]
  | [\x09aa-\x09b0]
  | \x09b2
  | [\x09b6-\x09b9]
  | [\x09bc-\x09c4]
  | [\x09c7-\x09c8]
  | [\x09cb-\x09cd]
  | \x09d7
  | [\x09dc-\x09dd]
  | [\x09df-\x09e3]
  | [\x09e6-\x09f1]
  | [\x0a01-\x0a03]
  | [\x0a05-\x0a0a]
  | [\x0a0f-\x0a10]
  | [\x0a13-\x0a28]
  | [\x0a2a-\x0a30]
  | [\x0a32-\x0a33]
  | [\x0a35-\x0a36]
  | [\x0a38-\x0a39]
  | \x0a3c
  | [\x0a3e-\x0a42]
  | [\x0a47-\x0a48]
  | [\x0a4b-\x0a4d]
  | [\x0a59-\x0a5c]
  | \x0a5e
  | [\x0a66-\x0a74]
  | [\x0a81-\x0a83]
  | [\x0a85-\x0a8d]
  | [\x0a8f-\x0a91]
  | [\x0a93-\x0aa8]
  | [\x0aaa-\x0ab0]
  | [\x0ab2-\x0ab3]
  | [\x0ab5-\x0ab9]
  | [\x0abc-\x0ac5]
  | [\x0ac7-\x0ac9]
  | [\x0acb-\x0acd]
  | \x0ad0
  | [\x0ae0-\x0ae3]
  | [\x0ae6-\x0aef]
  | [\x0b01-\x0b03]
  | [\x0b05-\x0b0c]
  | [\x0b0f-\x0b10]
  | [\x0b13-\x0b28]
  | [\x0b2a-\x0b30]
  | [\x0b32-\x0b33]
  | [\x0b35-\x0b39]
  | [\x0b3c-\x0b43]
  | [\x0b47-\x0b48]
  | [\x0b4b-\x0b4d]
  | [\x0b56-\x0b57]
  | [\x0b5c-\x0b5d]
  | [\x0b5f-\x0b61]
  | [\x0b66-\x0b6f]
  | \x0b71
  | [\x0b82-\x0b83]
  | [\x0b85-\x0b8a]
  | [\x0b8e-\x0b90]
  | [\x0b92-\x0b95]
  | [\x0b99-\x0b9a]
  | \x0b9c
  | [\x0b9e-\x0b9f]
  | [\x0ba3-\x0ba4]
  | [\x0ba8-\x0baa]
  | [\x0bae-\x0bb5]
  | [\x0bb7-\x0bb9]
  | [\x0bbe-\x0bc2]
  | [\x0bc6-\x0bc8]
  | [\x0bca-\x0bcd]
  | \x0bd7
  | [\x0be7-\x0bef]
  | [\x0c01-\x0c03]
  | [\x0c05-\x0c0c]
  | [\x0c0e-\x0c10]
  | [\x0c12-\x0c28]
  | [\x0c2a-\x0c33]
  | [\x0c35-\x0c39]
  | [\x0c3e-\x0c44]
  | [\x0c46-\x0c48]
  | [\x0c4a-\x0c4d]
  | [\x0c55-\x0c56]
  | [\x0c60-\x0c61]
  | [\x0c66-\x0c6f]
  | [\x0c82-\x0c83]
  | [\x0c85-\x0c8c]
  | [\x0c8e-\x0c90]
  | [\x0c92-\x0ca8]
  | [\x0caa-\x0cb3]
  | [\x0cb5-\x0cb9]
  | [\x0cbc-\x0cc4]
  | [\x0cc6-\x0cc8]
  | [\x0cca-\x0ccd]
  | [\x0cd5-\x0cd6]
  | \x0cde
  | [\x0ce0-\x0ce1]
  | [\x0ce6-\x0cef]
  | [\x0d02-\x0d03]
  | [\x0d05-\x0d0c]
  | [\x0d0e-\x0d10]
  | [\x0d12-\x0d28]
  | [\x0d2a-\x0d39]
  | [\x0d3e-\x0d43]
  | [\x0d46-\x0d48]
  | [\x0d4a-\x0d4d]
  | \x0d57
  | [\x0d60-\x0d61]
  | [\x0d66-\x0d6f]
  | [\x0d82-\x0d83]
  | [\x0d85-\x0d96]
  | [\x0d9a-\x0db1]
  | [\x0db3-\x0dbb]
  | \x0dbd
  | [\x0dc0-\x0dc6]
  | \x0dca
  | [\x0dcf-\x0dd4]
  | \x0dd6
  | [\x0dd8-\x0ddf]
  | [\x0df2-\x0df3]
  | [\x0e01-\x0e3a]
  | [\x0e40-\x0e4e]
  | [\x0e50-\x0e59]
  | [\x0e81-\x0e82]
  | \x0e84
  | [\x0e87-\x0e88]
  | \x0e8a
  | \x0e8d
  | [\x0e94-\x0e97]
  | [\x0e99-\x0e9f]
  | [\x0ea1-\x0ea3]
  | \x0ea5
  | \x0ea7
  | [\x0eaa-\x0eab]
  | [\x0ead-\x0eb9]
  | [\x0ebb-\x0ebd]
  | [\x0ec0-\x0ec4]
  | \x0ec6
  | [\x0ec8-\x0ecd]
  | [\x0ed0-\x0ed9]
  | [\x0edc-\x0edd]
  | \x0f00
  | [\x0f18-\x0f19]
  | [\x0f20-\x0f29]
  | \x0f35
  | \x0f37
  | \x0f39
  | [\x0f3e-\x0f47]
  | [\x0f49-\x0f6a]
  | [\x0f71-\x0f84]
  | [\x0f86-\x0f8b]
  | [\x0f90-\x0f97]
  | [\x0f99-\x0fbc]
  | \x0fc6
  | [\x1000-\x1021]
  | [\x1023-\x1027]
  | [\x1029-\x102a]
  | [\x102c-\x1032]
  | [\x1036-\x1039]
  | [\x1040-\x1049]
  | [\x1050-\x1059]
  | [\x10a0-\x10c5]
  | [\x10d0-\x10f8]
  | [\x1100-\x1159]
  | [\x115f-\x11a2]
  | [\x11a8-\x11f9]
  | [\x1200-\x1206]
  | [\x1208-\x1246]
  | \x1248
  | [\x124a-\x124d]
  | [\x1250-\x1256]
  | \x1258
  | [\x125a-\x125d]
  | [\x1260-\x1286]
  | \x1288
  | [\x128a-\x128d]
  | [\x1290-\x12ae]
  | \x12b0
  | [\x12b2-\x12b5]
  | [\x12b8-\x12be]
  | \x12c0
  | [\x12c2-\x12c5]
  | [\x12c8-\x12ce]
  | [\x12d0-\x12d6]
  | [\x12d8-\x12ee]
  | [\x12f0-\x130e]
  | \x1310
  | [\x1312-\x1315]
  | [\x1318-\x131e]
  | [\x1320-\x1346]
  | [\x1348-\x135a]
  | [\x1369-\x1371]
  | [\x13a0-\x13f4]
  | [\x1401-\x166c]
  | [\x166f-\x1676]
  | [\x1681-\x169a]
  | [\x16a0-\x16ea]
  | [\x16ee-\x16f0]
  | [\x1700-\x170c]
  | [\x170e-\x1714]
  | [\x1720-\x1734]
  | [\x1740-\x1753]
  | [\x1760-\x176c]
  | [\x176e-\x1770]
  | [\x1772-\x1773]
  | [\x1780-\x17b3]
  | [\x17b6-\x17d3]
  | \x17d7
  | [\x17dc-\x17dd]
  | [\x17e0-\x17e9]
  | [\x180b-\x180d]
  | [\x1810-\x1819]
  | [\x1820-\x1877]
  | [\x1880-\x18a9]
  | [\x1900-\x191c]
  | [\x1920-\x192b]
  | [\x1930-\x193b]
  | [\x1946-\x196d]
  | [\x1970-\x1974]
  | [\x1d00-\x1d6b]
  | [\x1e00-\x1e9b]
  | [\x1ea0-\x1ef9]
  | [\x1f00-\x1f15]
  | [\x1f18-\x1f1d]
  | [\x1f20-\x1f45]
  | [\x1f48-\x1f4d]
  | [\x1f50-\x1f57]
  | \x1f59
  | \x1f5b
  | \x1f5d
  | [\x1f5f-\x1f7d]
  | [\x1f80-\x1fb4]
  | [\x1fb6-\x1fbc]
  | \x1fbe
  | [\x1fc2-\x1fc4]
  | [\x1fc6-\x1fcc]
  | [\x1fd0-\x1fd3]
  | [\x1fd6-\x1fdb]
  | [\x1fe0-\x1fec]
  | [\x1ff2-\x1ff4]
  | [\x1ff6-\x1ffc]
  | [\x203f-\x2040]
  | \x2054
  | \x2071
  | \x207f
  | [\x20d0-\x20dc]
  | \x20e1
  | [\x20e5-\x20ea]
  | \x2102
  | \x2107
  | [\x210a-\x2113]
  | \x2115
  | [\x2118-\x211d]
  | \x2124
  | \x2126
  | \x2128
  | [\x212a-\x2131]
  | [\x2133-\x2139]
  | [\x213d-\x213f]
  | [\x2145-\x2149]
  | [\x2160-\x2183]
  | [\x3005-\x3007]
  | [\x3021-\x302f]
  | [\x3031-\x3035]
  | [\x3038-\x303c]
  | [\x3041-\x3096]
  | [\x3099-\x309a]
  | [\x309d-\x309f]
  | [\x30a1-\x30ff]
  | [\x3105-\x312c]
  | [\x3131-\x318e]
  | [\x31a0-\x31b7]
  | [\x31f0-\x31ff]
  | [\x3400-\x4db5]
  | [\x4e00-\x9fa5]
  | [\xa000-\xa48c]
  | [\xac00-\xd7a3]
  | [\xf900-\xfa2d]
  | [\xfa30-\xfa6a]
  | [\xfb00-\xfb06]
  | [\xfb13-\xfb17]
  | [\xfb1d-\xfb28]
  | [\xfb2a-\xfb36]
  | [\xfb38-\xfb3c]
  | \xfb3e
  | [\xfb40-\xfb41]
  | [\xfb43-\xfb44]
  | [\xfb46-\xfbb1]
  | [\xfbd3-\xfc5d]
  | [\xfc64-\xfd3d]
  | [\xfd50-\xfd8f]
  | [\xfd92-\xfdc7]
  | [\xfdf0-\xfdf9]
  | [\xfe00-\xfe0f]
  | [\xfe20-\xfe23]
  | [\xfe33-\xfe34]
  | [\xfe4d-\xfe4f]
  | \xfe71
  | \xfe73
  | \xfe77
  | \xfe79
  | \xfe7b
  | \xfe7d
  | [\xfe7f-\xfefc]
  | [\xff10-\xff19]
  | [\xff21-\xff3a]
  | \xff3f
  | [\xff41-\xff5a]
  | [\xff65-\xffbe]
  | [\xffc2-\xffc7]
  | [\xffca-\xffcf]
  | [\xffd2-\xffd7]
  | [\xffda-\xffdc]
  | \xd800 [\xdc00-\xdc0a]
  | \xd800 [\xdc0d-\xdc25]
  | \xd800 [\xdc28-\xdc39]
  | \xd800 [\xdc3c-\xdc3c]
  | \xd800 [\xdc3f-\xdc4c]
  | \xd800 [\xdc50-\xdc5c]
  | \xd800 [\xdc80-\xdcf9]
  | \xd800 [\xdf00-\xdf1d]
  | \xd800 [\xdf30-\xdf49]
  | \xd800 [\xdf80-\xdf9c]
  | \xd801 [\xe000-\xe09c]
  | \xd801 [\xe0a0-\xe0a8]
  | \xd802 [\xe400-\xe404]
  | \xd802 \x0808
  | \xd802 [\xe40a-\xe434]
  | \xd802 [\xe437-\xe437]
  | \xd802 \x083c
  | \xd802 \x083f
  | \xd834 [\xad65-\xad68]
  | \xd834 [\xad6d-\xad71]
  | \xd834 [\xad7b-\xad81]
  | \xd834 [\xad85-\xad8a]
  | \xd834 [\xadaa-\xadac]
  | \xd835 [\xb000-\xb053]
  | \xd835 [\xb056-\xb09b]
  | \xd835 [\xb09e-\xb09e]
  | \xd835 \xd4a2
  | \xd835 [\xb0a5-\xb0a5]
  | \xd835 [\xb0a9-\xb0ab]
  | \xd835 [\xb0ae-\xb0b8]
  | \xd835 \xd4bb
  | \xd835 [\xb0bd-\xb0c2]
  | \xd835 [\xb0c5-\xb104]
  | \xd835 [\xb107-\xb109]
  | \xd835 [\xb10d-\xb113]
  | \xd835 [\xb116-\xb11b]
  | \xd835 [\xb11e-\xb138]
  | \xd835 [\xb13b-\xb13d]
  | \xd835 [\xb140-\xb143]
  | \xd835 \xd546
  | \xd835 [\xb14a-\xb14f]
  | \xd835 [\xb152-\xb2a2]
  | \xd835 [\xb2a8-\xb2bf]
  | \xd835 [\xb2c2-\xb2d9]
  | \xd835 [\xb2dc-\xb2f9]
  | \xd835 [\xb2fc-\xb313]
  | \xd835 [\xb316-\xb333]
  | \xd835 [\xb336-\xb34d]
  | \xd835 [\xb350-\xb36d]
  | \xd835 [\xb370-\xb387]
  | \xd835 [\xb38a-\xb3a7]
  | \xd835 [\xb3aa-\xb3c1]
  | \xd835 [\xb3c4-\xb3c8]
  | \xd835 [\xb3ce-\xb3fe]
  | \xd840 [\xdc00-\xdffe]
  | \xd841 [\xe000-\xe3fe]
  | \xd842 [\xe400-\xe7fe]
  | \xd843 [\xe800-\xebfe]
  | \xd844 [\xec00-\xeffe]
  | \xd845 [\xf000-\xf3fe]
  | \xd846 [\xf400-\xf7fe]
  | \xd847 [\xf800-\xfbfe]
  | \xd848 [\xfc00-\xfffe]
  | \xd849 [\x0000-\x03fe]
  | \xd84a [\x0400-\x07fe]
  | \xd84b [\x0800-\x0bfe]
  | \xd84c [\x0c00-\x0ffe]
  | \xd84d [\x1000-\x13fe]
  | \xd84e [\x1400-\x17fe]
  | \xd84f [\x1800-\x1bfe]
  | \xd850 [\x1c00-\x1ffe]
  | \xd851 [\x2000-\x23fe]
  | \xd852 [\x2400-\x27fe]
  | \xd853 [\x2800-\x2bfe]
  | \xd854 [\x2c00-\x2ffe]
  | \xd855 [\x3000-\x33fe]
  | \xd856 [\x3400-\x37fe]
  | \xd857 [\x3800-\x3bfe]
  | \xd858 [\x3c00-\x3ffe]
  | \xd859 [\x4000-\x43fe]
  | \xd85a [\x4400-\x47fe]
  | \xd85b [\x4800-\x4bfe]
  | \xd85c [\x4c00-\x4ffe]
  | \xd85d [\x5000-\x53fe]
  | \xd85e [\x5400-\x57fe]
  | \xd85f [\x5800-\x5bfe]
  | \xd860 [\x5c00-\x5ffe]
  | \xd861 [\x6000-\x63fe]
  | \xd862 [\x6400-\x67fe]
  | \xd863 [\x6800-\x6bfe]
  | \xd864 [\x6c00-\x6ffe]
  | \xd865 [\x7000-\x73fe]
  | \xd866 [\x7400-\x77fe]
  | \xd867 [\x7800-\x7bfe]
  | \xd868 [\x7c00-\x7ffe]
  | \xd869 [\x8000-\x82d5]
  | \xd87e [\xd400-\xd61c]
  | \xdb40 [\xdd00-\xddee]

@ident             = @xid_start @xid_continue*

@lifetime          = \' @ident

$hexit             = [0-9a-fA-F]

@char_escape
  = [nrt\\'"0]
  | [xX] $hexit $hexit
  | u $hexit $hexit $hexit $hexit
  | U $hexit $hexit $hexit $hexit $hexit $hexit $hexit $hexit
  | u\{ $hexit \}
  | u\{ $hexit $hexit \}
  | u\{ $hexit $hexit $hexit \}
  | u\{ $hexit $hexit $hexit $hexit \}
  | u\{ $hexit $hexit $hexit $hexit $hexit \}
  | u\{ $hexit $hexit $hexit $hexit $hexit $hexit \}


-- literals

@lit_char
  = \' ( \\ @char_escape
       | [^\\'\n\t\r]
       | [ \ud800-\udbff \udc00-\udfff ]
       )
    \'

@lit_byte
  = b\' ( \\ ( [xX] $hexit $hexit
             | [nrt\\'"0] )
        | [^\\'\n\t\r] [ \udc00-\udfff ]?
        )
    \'

@lit_integer
  = [0-9][0-9_]*
  | 0b [01_]+
  | 0o [0-8_]+
  | 0x [0-9a-fA-F_]+

@lit_float         = [0-9][0-9_]* (\. [0-9][0-9_]*)? ([eE] [\-\+]? [0-9][0-9_]*)?
@lit_float2        = [0-9][0-9_]* \.

@lit_str           = \" (\\\n | \\\r\n | \\ @char_escape | [^\"])* \"
@lit_byte_str      = b @lit_str

@lit_raw_str       = r \#* \"
@lit_raw_bstr      = br \#* \"


-- Comments

@outer_doc_line    = "///" [^\r\n]*
@outer_doc_inline  = "/**"

@inner_doc_line    = "//!" [^\r\n]*
@inner_doc_inline  = "/*!"

@line_comment      = "//" ( [^\n\/]* [^\n]* )?
@inline_comment    = "/*"

-- Macro related

@subst_nt          = "$" @ident
@match_nt          = @subst_nt ":" @ident

tokens :-

$white+         { \s -> pure (Space Whitespace s)  }

"="             { token Equal }
"<"             { token Less }
">"             { token Greater }
"&"             { token Ampersand }
"|"             { token Pipe }
"!"             { token Exclamation }
"~"             { token Tilde }
"+"             { token Plus }
"-"             { token Minus }
"*"             { token Star }
"/"             { token Slash }
"%"             { token Percent }
"^"             { token Caret }

"||"            { token PipePipe }
"&&"            { token AmpersandAmpersand }
">="            { token GreaterEqual }
">>="           { token GreaterGreaterEqual }
"<<"            { token LessLess }
">>"            { token GreaterGreater }

"=="            { token EqualEqual }
"!="            { token NotEqual }
"<="            { token LessEqual }
"<<="           { token LessLessEqual }
"-="            { token MinusEqual }
"&="            { token AmpersandEqual }
"|="            { token PipeEqual }
"+="            { token PlusEqual }
"*="            { token StarEqual }
"/="            { token SlashEqual }
"^="            { token CaretEqual }
"%="            { token PercentEqual }
 

"@"             { token At }          
"."             { token Dot }        
".."            { token DotDot }     
"..."           { token DotDotDot } 
","             { token Comma } 
";"             { token Semicolon }     
":"             { token Colon }
"::"            { token ModSep }
"->"            { token RArrow }
"<-"            { token LArrow }
"=>"            { token FatArrow }
"("             { token (OpenDelim Paren) }      
")"             { token (CloseDelim Paren) }  
"["             { token (OpenDelim Bracket) }
"]"             { token (CloseDelim Bracket) }
"{"             { token (OpenDelim Brace) }      
"}"             { token (CloseDelim Brace) }      
"#"             { token Pound }     
"$"             { token Dollar }     
"_"             { token Underscore }

@lit_integer    { \i -> literal (IntegerTok i) }
@lit_float      { \f -> literal (FloatTok   f) }
@lit_float / [^\._a-zA-Z]
                { \f -> literal (FloatTok   f) }
@lit_float2 / [^\.]
                { \f -> literal (FloatTok   f) }

@lit_byte       { \c -> literal (ByteTok    (drop 2 (init c))) }
@lit_char       { \c -> literal (CharTok    (drop 1 (init c))) }
@lit_str        { \s -> literal (StrTok     (drop 1 (init s))) }
@lit_byte_str   { \s -> literal (ByteStrTok (drop 2 (init s))) }

@lit_raw_str    { \s -> let n = length s - 2
                        in do
                            str <- rawString n
                            literal (StrRawTok str (fromIntegral n))
                }
@lit_raw_bstr   { \s -> let n = length s - 3
                        in do
                            str <- rawString n
                            literal (ByteStrRawTok str (fromIntegral n))
                }

<lits> ""       ;
<lits> @ident   { \s -> pure (IdentTok (mkIdent s)) }

\?              { token Question }
@ident          { \s -> pure (IdentTok (mkIdent s)) } 
@lifetime       { \s -> (pure (LifetimeTok (mkIdent (tail s))) :: P Token) }


@outer_doc_line   { \c -> pure (Doc (drop 3 c) OuterDoc) } 
@outer_doc_inline { \_ -> Doc <$> nestedComment <*> pure OuterDoc }

@inner_doc_line   { \c -> pure (Doc (drop 3 c) InnerDoc) }
@inner_doc_inline { \_ -> Doc <$> nestedComment <*> pure InnerDoc }

@line_comment     { \c -> pure (Space Comment (drop 2 c)) }
@inline_comment   { \_ -> Space Comment <$> nestedComment }

"#!"              { token Shebang } 

@subst_nt         { \(_:i) -> pure (SubstNt (mkIdent i)) }
@match_nt         { \(_:s) -> let (i,':':n) = Prelude.span (/= ':') s
                              in pure (MatchNt (mkIdent i) (mkIdent n))
                  }

{

-- | Make a token.
token :: Token -> String -> P Token
token t _ = pure t

-- | Given the first part of a literal, try to parse also a suffix. Even if
-- the allowed suffixes are very well defined and only valid on integer and
-- float literals, we need to put in the same token whatever suffix follows.
-- This is for backwards compatibility if Rust decides to ever add suffixes. 
literal :: LitTok -> P Token 
literal lit = do
  pos <- getPosition
  inp <- getInput
  case alexScan (pos,inp) lits of
    AlexToken (pos',inp') len action -> do
        tok <- action (takeChars len inp)
        case tok of
          IdentTok (Ident suf _) -> setPosition pos' *> setInput inp' $> LiteralTok lit (Just suf)
          _ -> pure (LiteralTok lit Nothing)
    _ -> pure (LiteralTok lit Nothing)

-- | Parses a raw string, the closing quotation, and the appropriate number of
-- '#' characters. Note that there can be more closing '#' characters than
-- opening ones (this is as per Rust's standard).
rawString :: Int -> P String
rawString n = do
  c_m <- nextChar
  case c_m of
    -- The string was never closed
    Nothing -> fail "Invalid raw (byte)string"
    
    -- The string has a chance of being closed
    Just '"' -> do
      n' <- greedyChar '#'
      if n' >= n
        then pure ""
        else (('"' : replicate n' '#') ++) <$> rawString n 

    -- Just another character...
    Just c -> ([c] ++) <$> rawString n 

-- | Consume a full inline comment (which may be nested).
nestedComment :: P String
nestedComment = go 1 ""
  where
    go :: Int -> String -> P String
    go 0 s = pure (reverse (drop 2 s))
    go n s = do
      c <- nextChar
      case c of
        Nothing -> fail "Unclosed comment"
        Just '*' -> do
          c' <- peekChar
          case c' of 
            Nothing -> fail "Unclosed comment"
            Just '/' -> nextChar *> go (n-1) ('/':'*':s)
            Just _ -> go n ('*':s)
        Just '/' -> do
          c' <- peekChar
          case c' of 
            Nothing -> fail "Unclosed comment"
            Just '*' -> nextChar *> go (n+1) ('*':'/':s) 
            Just _ -> go n ('/':s)
        Just c' -> go n (c':s)


-- Monadic functions

-- | Retrieve the next character (if there is one), updating the parser state accordingly.
nextChar :: P (Maybe Char)
nextChar = do
  pos <- getPosition
  inp <- getInput
  if inputStreamEmpty inp 
    then pure Nothing
    else let (c,inp') = takeChar inp
             pos' = alexMove pos c
         in pos' `seq` (setPosition pos' *> setInput inp' $> Just c)

-- | Retrieve the next character (if there is one), without updating the
-- parser state.
peekChar :: P (Maybe Char)
peekChar = do
  inp <- getInput
  if inputStreamEmpty inp 
    then pure Nothing
    else let (c,_) = takeChar inp
         in pure (Just c)

-- | Greedily try to eat as many of a given character as possible (and return
-- how many characters were eaten).
greedyChar :: Char -> P Int
greedyChar c = do
  c_m <- peekChar
  case c_m of
    Just c' | c == c' -> do { _ <- nextChar; n <- greedyChar c; pure (n+1) }
    _ -> pure 0

-- | Signal a lexical error.
lexicalError :: P a
lexicalError = do
  c <- peekChar
  fail ("Lexical error: the character " ++ show c ++ " does not fit here")


-- Functions required by Alex 

-- | type passed around by Alex functions (required by Alex)
type AlexInput = (Position,    -- current position,
                  InputStream) -- current input string

-- | get previous character (required by Alex). Since this is never used, the
-- implementation just raises an error.
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _ = error "alexInputPrevChar not used"

-- | get the next byte and new input from the current input (required by Alex
-- 3.0)
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (pos,inp)
  | inputStreamEmpty inp = Nothing
  | otherwise = let (b,inp') = takeByte inp
                    -- this is safe for latin-1, but ugly
                    pos' = alexMove pos (chr (fromIntegral b))
                in pos' `seq` Just (b, (pos', inp'))

-- | find the new position given the next character
alexMove :: Position -> Char -> Position
alexMove pos ' '  = incPos pos 1
alexMove pos '\n' = retPos pos
alexMove pos '\r' = incOffset pos 1
alexMove pos _    = incPos pos 1

-- | Lexer for one 'Token'. The only token this cannot produce is 'Interpolated'. 
lexToken :: P (Spanned Token)
lexToken = do
  tok_maybe <- popToken
  case tok_maybe of
    Just tok -> pure tok
    Nothing -> do
      pos <- getPosition
      inp <- getInput
      case alexScan (pos, inp) 0 of
        AlexEOF                 -> pure (Spanned Eof (Span pos pos))
        AlexError _             -> fail "lexical error"
        AlexSkip  (pos',inp') _ -> setPosition pos' *> setInput inp' *> lexToken
        AlexToken (pos',inp') len action -> do
          setPosition pos'
          setInput inp'
          tok <- action (takeChars len inp)
          tok' <- swapToken tok
          pos'' <- getPosition
          return (Spanned tok' (Span pos pos''))

-- | Lexer for one non-whitespace 'Token'. The only tokens this cannot produce are 'Interpolated'
-- and 'Space' (which includes comments that aren't doc comments).
lexNonSpace :: P (Spanned Token)
lexNonSpace = do
  tok <- lexToken
  case tok of
    Spanned Space{} _ -> lexNonSpace
    _ -> pure tok

-- | Apply the given lexer repeatedly until (but not including) the 'Eof' token. Meant for debugging
-- purposes - in general this defeats the point of a threaded lexer.
lexTokens :: P (Spanned Token) -> P [Spanned Token]
lexTokens lexer = do
  tok <- lexer
  case tok of
    Spanned Eof _ -> pure []
    _ -> (tok :) <$> lexTokens lexer

-- | Lex the first line, if it immediately starts with @#!@ (but not @#![@ - that should be an
-- inner attribute). If this fails to find a shebang line, it consumes no input (in reality it does
-- consume one token, but it pushed it back).
lexShebangLine :: P (Maybe String)
lexShebangLine = do
  tok <- lexNonSpace
  case unspan tok of
    Shebang -> do
      c <- peekChar
      case c of
        Just '[' -> pushToken tok *> pure Nothing
        _ -> Just <$> toNewline
    _ -> pushToken tok *> pure Nothing 

  where
  -- Lexes a string until a newline
  toNewline :: P String
  toNewline = do
    c <- peekChar
    case c of
      Nothing -> pure ""
      Just '\n' -> pure ""
      Just c' -> do
        _ <- nextChar
        (c' :) <$> toNewline

    
}
