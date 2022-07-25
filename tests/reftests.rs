use std::fs;
use std::path::PathBuf;
use std::str::FromStr;
use whiley_test_file::WhileyTestFile;

pub static REFTESTS_DIR: &str = "reference-tests/tests";

// Include the programmatically generated test file.
include!(concat!(env!("OUT_DIR"), "/reftests.rs"));

/// Run a specific test by loading the file out of the reference tests
/// repository and attempting to parse it.  All reference tests should
/// parse correctly.
fn check(test: &str) {
    let _wtf = read_test_file(test);
    // FIXME: do more here!!
}

/// Read the Whiley test file for the given test name.
fn read_test_file(test: &str) -> WhileyTestFile {
    // Construct filename
    let mut path = PathBuf::from(REFTESTS_DIR);
    path.push(test);
    let filename = path.as_path().to_str().unwrap();
    // Read the test file
    let input = fs::read_to_string(filename).unwrap();
    // Parser test file
    WhileyTestFile::from_str(&input).unwrap()
}
