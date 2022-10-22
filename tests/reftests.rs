use std::fmt;
use std::fs;
use std::path::PathBuf;
use std::collections::HashMap;
use whiley_file::WhileyFile;
use whiley_test_file::{Action,WhileyTestFile};

pub static REFTESTS_DIR: &str = "reference-tests/tests";

// Include the programmatically generated test file.
include!(concat!(env!("OUT_DIR"), "/reftests.rs"));

/// Run a specific test by loading the file out of the reference tests
/// repository and attempting to parse it.  All reference tests should
/// parse correctly.
fn check(test: &str) {
    // Construct filename
    let mut path = PathBuf::from(REFTESTS_DIR);
    path.push(test);
    let filename = path.as_path().to_str().unwrap();
    // Read the test file
    let input = fs::read_to_string(filename).unwrap();
    // Parse test file
    let test_file = WhileyTestFile::new(&input).unwrap();
    // Construct initial (empty) snapshot
    let mut snapshot = SnapShot::new();
    // Process each frame, one by one.
    for fr in test_file.iter() {
        // Apply actions
        for a in &fr.actions {
            snapshot.apply(a);
        }
        // Compile snapshot
        build(&snapshot);
        // Check against expected errors!
    }
}

/// Attempt to build a given snapshot, producing zero or more errors.
fn build(snapshot: &SnapShot) {
    for (_,srcfile) in snapshot.files.iter() {
        // Attempt to parse source file
        match WhileyFile::from_str(&srcfile.to_str()) {
            Err(e) => {
                srcfile.print_error(e);
                panic!("parse error");
            }
            Ok(_) => {}
        }
    }
}

// ===========================================================================
// SnapShot
// ===========================================================================

/// Represents a snapshot of the "filesystem" at a given point in the
/// test.  As such, it provides access to the contents of each file at
/// that point.
struct SnapShot<'a> {
    files: HashMap<&'a str,SourceFile<'a>>
}

impl<'a> SnapShot<'a> {
    pub fn new() -> Self {
        SnapShot{files: HashMap::new()}
    }

    fn apply(&mut self, action: &Action<'a>) {
        match action {
            Action::CREATE(name,lines) => {
                let sf = SourceFile::new(lines);
                self.files.insert(name,sf);
            }
            Action::INSERT(name,range,lines) => {
                let sf = self.files.get_mut(name).unwrap();
                sf.replace(range.0,range.1, lines);
            }
            Action::REMOVE(n) => {
                self.files.remove(n);
            }
        }
    }
}

// ============================================================================
// To be deprecated (by merging into WhileyFile)
// ============================================================================

#[derive(Debug)]
pub struct SourceFile<'a> {
    lines: Vec<&'a str>
}

impl<'a> SourceFile<'a> {
    pub fn new(lines: &[&'a str]) -> Self {
        SourceFile{lines: lines.to_vec()}
    }

    pub fn replace(&mut self, start: usize, end: usize, lines: &[&'a str]) {
        self.lines.splice(start..end,lines.iter().cloned());
    }

    pub fn to_str(&self) -> String {
        self.lines.join("\n")
    }

    pub fn print_error(&self, err: whiley_file::Error) {
        let span = err.span;
        //
        println!("error: {:?}",err.code);
        // Print line itself (if possible)
        match self.enclosing(span.start()) {
            Some((l,s,_)) => {
                println!("{}",&self.lines[l]);
                // Determine start of highlight
                let hs = span.start() - s;
                let he = span.end() - s;
		let padding = " ".repeat(hs);
		let highlight = "^".repeat(he-hs);
		println!("{}{}",padding,highlight);
            }
            _ => {}
        }
    }

    /// Determine the enclosing line for a given offset in the file
    /// (or none if there is no match).
    pub fn enclosing(&self, offset: usize) -> Option<(usize,usize,usize)> {
        let mut start = 0;
        //
        for i in 0..self.lines.len() {
            // Shift offset
            let end = start + self.lines[i].len();
            // check for inclusion
            if offset <= end {
                return Some((i,start,end));
            }
            //
            start = end + 1;
        }
        //
        None
    }
}

impl<'a> fmt::Display for SourceFile<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), std::fmt::Error> {
        for line in &self.lines {
            writeln!(f,"{}",line)?;
        }
        Ok(())
    }
}
