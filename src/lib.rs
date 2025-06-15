mod validator;

pub mod prelude {
    pub use crate::validator::*;
    pub use crate::validator::parser::parse_config;
    pub use crate::validator::parser::InputType;
}
