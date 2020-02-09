# NRap toy language

NRap was created to be used as a 1-to-1 representation of Raptor Flowchart Interpreter programs.

NRap is the format used for files saved by a flowchart interpreter. 

## Why?
Raptor Flowchart was used at my university but had a hard time working with it because it's officialy only supported on windows.
I decided to create my own cross-platform flowchart interpreter and NRap was the first step. Having a text representation of the flowchart allows you to edit the program without having to use a GUI.

NRap is still in early development.

## Building
debug: `cargo build`
release: `cargo build --release`

## Usage
`./nrap FILE.nrap`

## Run examples
`cargo run --release ./nrap_examples/fib.nrap`
