use std::collections::HashMap;
use crate::Error;
use crate::ast::*;

// =================================================================
// Error
// =================================================================

pub type Result<T> = std::result::Result<T, Error>;

// =================================================================
// Type Checker
// =================================================================

pub type Env = HashMap<String, Type>;

pub struct TypeChecker {
    globals: Env
}

impl TypeChecker {
    pub fn new() -> Self {
        let globals : Env = HashMap::new();
	TypeChecker{globals}
    }
}
