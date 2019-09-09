fn foo() { }
pub fn foo() { }
pub(crate) fn foo() { }
pub(in bar::baz) fn foo() { }
pub(super) fn foo() { }
pub(self) fn foo() { }

fn as_stmt() {
  fn foo() { }
  pub fn foo() { }
  pub(crate) fn foo() { }
  pub(in bar::baz) fn foo() { }
  pub(super) fn foo() { }
  pub(self) fn foo() { }
}
