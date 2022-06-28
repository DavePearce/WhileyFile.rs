# Overview

A Rust library for reading Whiley files.

```Rust
    use whiley_file::WhileyFile;
    //
    let input ="function f(u8 x) -> (u8 r):\n skip";
    let mut r = WhileyFile::from_str(input);
    //
    match r {
      Ok(WhileyFile) => {
          println!("Parsing succeeded");
      }
      Err(e) => {
          println!("Parsing failed --- {}",e.message);
      }
    }
```
