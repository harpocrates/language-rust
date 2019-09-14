// See <https://github.com/rust-lang/rust/blob/master/src/libsyntax/util/parser.rs>

fn main() {
  a /= - b ? ^= * - c >= - * d [0] ? + box & ! e ? [0] .0 ? [0] .0 ?;
  - a == box b [0] [0] [0] .0 || c [0] ^= d % e = f != g;
  & a *= ! b ^= c * d | e [0];
  a ^ b < * c ? || box d ? %= e [0] = - f < box g % & h &= i ? % & & & j ?;
  a &= - box b ? >> c [0] <= d || & - e ^= f = & g .0 ? [0] - h .0 .0 += i [0] << j ?;
  a >> & b && - box c >= d .. e .0 [0] + ! f * g >= h && & - i [0] * j;
  * a > b *= c * d && e ^ - & f ? *= g %= h .0 [0] ? & i -= j ?;
  * a >> b %= c .0 != * - d [0] || e <= f << g .0 | ! h += - & i >= j .0;
  a && ! * box b [0] >> - c || * d ^ * e + f >= g /= ! h = i [0] ..= j;
  .. a + box b ? [0] * c ? .0 ^ d +  & f <= g [0] * h && i + j [0] ?;
  a * b &= c & * d ? |= & ! e [0] != & f .0 = g >> ! ! * - h ? ^ i ? %= - j;
  & a != b [0] % c - & & ! - box - d & e [0] += g ? << h /= i <<= .. .. j;
  a [0] >>= b | & ! c / ! & d [0] ? / box e .. - f * * & * - g < h && j;
  a & b - c *= &&*d? << *e.0?.0 ^= !f?[0].0 >> -g - h .. i??[0] | j;
  -a[0] - b[0][0]??.0 != c -= d || e[0][0] *= &f = !!g[0] / h ^ i | box!j[0];
  a != -b.0 - box!c.0?.0 *= -e? <= *-f.0 ^= g >> box box h + i + j;
  !&&-box a??.0 >> b |= c[0]? ^ d * e | &*f[0] <= box -g? ..= &h * &i - j[0][0];
  a & b &= c[0] <= d + -!-box*e[0]?[0] % f.0? + -g >>= h[0] /= i = j;
  a[0][0] >>= *b.0 .0? / c ^ d >>= !e >= box f ..= g >= h + i? || j;
  !&a? |= -b >= c / *-d? *= e % f += !box g.0? != &-*h? + i.0 / j;
  a? - b? | c * *-box box box*box d? .0 .0 %= !!!e? &= -box!f.0 += g - h /= i && j;
  *box box!a -= box-b.0 % c &= d % e? ^= !&f[0]? != h & i /= j;
  box---!a.0 ^= b = -&box c? > d << e.0 + f[0] + g -= h >>= i * j;
  a .0 .0 >>= -b >= -box*c || d? &= box&-e | f << g * h[0][0] |= i * j;
  box-a / b != c -= d == &e.0 >>= f - *g[0] %= h & i << &-j??;
  !&-a.0 == b *= c.0 <<= --d && e? ^= !f <<= g[0] > h += -i >>= j;
  return box break 'lt 1? + 2?;
  break 'lt box return 1? + 2?;
}

struct Point { x: i32 }
fn range_expressions() {
 
  // general expression
  let _ = ..Point { x: 1 };
  let _ = ..{};
  let _ = |x| { 1 };
  
  // non-block expression 
  ..Point { x: 1 };
  ..{};
  - { 1 };
  return |x: i32| x + 1;
  box 1 + 2;
  |x| { 1 };
  x || y();
  x && y();

  // block expression / starting with block statement
  { 1 }?.0 + 1;
  if true { 1 } else { 2 }.toString();
  if true { 1 }.toString();
  { x }[2]?.foo * { 3 };

  // no struct expression
  for x in 1.. { }
  for x in ..1 { }
  for x in ..Point{} { }
  for x in .. { }
  for x in {}.. { }
  for x in |x| { 1 } { }

  // no struct/block expression (to the right of '..')
  for x in ..1 { }
  for x in ..1 + 1 { }
  for x in ..1 + { 2 } { }
  for x in ..|x| { 1 } { }

  // precedences of ranges in general
  fn general_ranges() {
    let _ = ..;
    let _ = a - c .. 3;
    let _ = a - c == 3;
  
    let _ = 1 + 2..;
    let _ = 1 == 2..;
    let _ = 1 == 2..3;
    let _ = 2..3 == 1;
    let _ = ..1 + 2;
    let _ = ..1 == 2;
    let _ = 1 == ..2;
    // let _ = 1.. == 2; Shouldn't parse according to rustc
  
    let _ = .. - 1;
    let _ = - 1 .. 2;
    let _ = .. box 1;
    let _ = .. .. 1;
    let _ = 1.. ..;
    let _ = 2 & ..2 + ..3;
  }

}


