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
        todo!("got here");
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
        self.constraints.push(c);
    }

    pub fn solve(&mut self) -> Result<Vec<Type>> {
        let changed = true;
        let mut types = Vec::new();
        // Keep iterating until no change
        while changed {
            for i in 0..self.constraints.len() {
                let ith = self.constraints[i];
                self.apply_constraint(&ith)?;
            }
        }
        // Done
        Ok(types)
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

    pub fn apply_constraint(&mut self, c: &Constraint) -> Result<()> {
        match c {
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

    /// Determine largest variable contained within constraints.
    fn max_var(&self) -> usize {
        let mut max_var = 0;
        for c in &self.constraints {
            max_var = cmp::max(max_var,c.max_var());
        }
        max_var
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
