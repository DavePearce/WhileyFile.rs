use std::cmp;
use crate::ast::{AbstractSyntaxTree,Types,Type};
use crate::{Result};
use syntactic_heap::SyntacticHeap;

// ===================================================================
// Constraint
// ===================================================================

/// Represents a type constraint on a given variable
#[derive(Clone,Copy,Debug)]
pub enum Constraint {
    LowerBound(usize,Type),
    UpperBound(Type,usize),
    Subtype(usize,usize),
    Equal(usize,usize)
}

impl Constraint {
    pub fn max_var(&self) -> usize {
        match self {
            Constraint::LowerBound(v,_) => *v,
            Constraint::UpperBound(_,v) => *v,
            Constraint::Subtype(lhs,rhs) => cmp::max(*lhs,*rhs),
            _ => {
                todo!("Support more constraints constraint");
            }
        }
    }

    pub fn apply(&self, types: &mut [Type]) {
        match self {
            Constraint::LowerBound(v,t) => {
                todo!("Found upper bound constraint");
            }
            Constraint::UpperBound(t,v) => {
                todo!("Found upper bound constraint");
            }
            Constraint::Subtype(lhs,rhs) => {
                todo!("Found subtype constraint");
            }
            _ => {
                todo!("Support more constraints constraint");
            }
        }
    }
}

// ===================================================================
// Typing
// ===================================================================

/// Represents a typing of all variables.
#[derive(Debug)]
pub struct TypeConstraints {
    /// Syntactic heap of types
    heap: SyntacticHeap<Types>,
    /// The set of constraints on each variable
    constraints: Vec<Constraint>,
    /// Current variable typings
    types: Vec<Type>
}

impl TypeConstraints {
    const BOTTOM : Type = Type(0);

    pub fn new() -> Self {
        let heap = SyntacticHeap::new();
        let constraints = Vec::new();
        let types = Vec::new();
        TypeConstraints{heap, constraints, types}
    }

    /// Copy a type from the abstract syntax tree into this typing so
    /// that it can be used later.
    pub fn copy_into(&mut self, t: Type, ast: &AbstractSyntaxTree) -> Type {
        todo!("implement me");
    }

    pub fn add(&mut self, c: Constraint) {
        // Ensure sufficient types
        self.expand(c.max_var());
        // Record constraint
        self.constraints.push(c);
        // Apply constraint
        c.apply(&mut self.types);
    }

    /// Obtain a type of the given kind.  This may require allocating
    /// such a type on the heap, or it may reuse an existing (and
    /// matching) type.
    pub fn type_of<T:Into<Types>>(&mut self, t: T) -> Type {
	// FIXME: this is where we want to manage the creation of
	// types carefully, such that we don't create any duplicate
	// types.  In particular, ideally, physical equality implies
	// semantic equality.
	//
        // Create new node
        let index = self.heap.push(t.into()).raw_index();
        // Done
        Type(index)
    }

    // ===============================================================
    // Helpers
    // ===============================================================

    /// Make sure that `self.types` has sufficient space for the given variable.
    fn expand(&mut self, var: usize) {
        if var >= self.types.len() {
            // Yes, expansion is required
            self.types.resize(var+1, Self::BOTTOM);
        }
    }

    // pub fn check_type_array(&mut self, env: Env, t: &types::Array) -> Result<Env> {
    //     let (nenv,elem) = self.check(env, t.0)?;
    //     Ok((nenv,self.type_of(types::Array(elem))))
    // }

    // pub fn check_type_bool(&mut self, env: Env, _t: &types::Bool) -> Result<Env> {
    //     Ok((env,self.type_of(types::Bool())))
    // }

    // pub fn check_type_int(&mut self, env: Env, t: &types::Int) -> Result<Env> {
    //     Ok((env,self.type_of(types::Int(t.0,t.1))))
    // }

    // pub fn check_type_null(&mut self, env: Env, _t: &types::Null) -> Result<Env> {
    //     Ok((env,self.type_of(types::Null())))
    // }

    // pub fn check_type_record(&mut self, mut env: Env, t: &types::Record) -> Result<Env> {
    //     let mut fields = Vec::new();
    //     //
    //     for &(t,n) in &t.0 {
    //         let ith : Type;
    //         (env,ith) = self.check(env,t)?;
    //         // FIXME: there is a major bug here, since n is not
    //         // allocated within the types heap.
    //         fields.push((ith,n));
    //     }
    //     Ok((env,self.type_of(types::Record(fields))))
    // }

    // pub fn check_type_reference(&mut self, env: Env, t: &types::Reference) -> Result<Env> {
    //     let (nenv,elem) = self.check(env, t.0)?;
    //     Ok((nenv,self.type_of(types::Reference(elem))))
    // }

}
