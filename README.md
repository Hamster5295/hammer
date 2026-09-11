# Hammer

This library provides utilities for [Chisel](https://www.chisel-lang.org/)  
Might be able to save time and hair of hardware devs

## Quick Start

Add the following dependency line to your `build.mill`

```scala
override def mvnDeps = Seq(
    // There might have been other dependencies
    mvn"io.github:hamster5295::hammer:1.0.0",
)
```

The line above works at mill `1.0.0` and above.

You'll need to transform according to your own build tool, i.e. mill `0.X` or sbt


## Documention

**TODO**

Currently there're only a few simple tools separated in source files.  
You can get a brief knowledge about what `hammer` provides through reading the file names.  

File Organization:
* `chisel` - **Compile Time** utils that helps creating hardware faster
* `model` - Golden model for various data structures for testbench purposes
* `test` - Utils for writing better and faster tests
