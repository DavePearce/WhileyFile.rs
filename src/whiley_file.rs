use crate::ast::AbstractSyntaxTree;

struct WhileyFile {
    ast : Box<AbstractSyntaxTree>
}

impl WhileyFile {
    pub fn from_str(input: &str) {
	let mut ast = AbstractSyntaxTree::new();
	let mut parser = Parser::new(input,&mut ast, source_mapper);
	// Parse the file
	
	// Done
	WhileyFile{ast:Box::new(ast)}
    }
}
