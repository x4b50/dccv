use dccv::prelude::*;

#[test]
fn dev_testing() {
    assert_ne!(parser::parse_config("tests/dev_testing.input"), Err(()));
}
