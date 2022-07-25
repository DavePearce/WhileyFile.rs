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
    for i in 0..test_file.size() {
        let ith = test_file.frame(i);
        // Apply actions
        for j in 0..ith.actions.len() {
            snapshot.apply(&ith.actions[j]);
        }
        // Compile snapshot
        build(&snapshot);
        // Check against expected errors!
    }
}

/// Attempt to build a given snapshot, producing zero or more errors.
fn build(snapshot: &SnapShot) {
    for (k,v) in snapshot.files.iter() {
        // Attempt to parse source file
        let wf = WhileyFile::from_str(&v.to_str()).unwrap();
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
    contents: Vec<&'a str>
}

impl<'a> SourceFile<'a> {
    pub fn new(contents: &[&'a str]) -> Self {
        SourceFile{contents: contents.to_vec()}
    }

    pub fn replace(&mut self, start: usize, end: usize, lines: &[&'a str]) {
        self.contents.splice(start..end,lines.iter().cloned());
    }

    pub fn to_str(&self) -> String {
        self.contents.join("\n")
    }
}

impl<'a> fmt::Display for SourceFile<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), std::fmt::Error> {
        for line in &self.contents {
            writeln!(f,"{}",line)?;
        }
        Ok(())
    }
}
