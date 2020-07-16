# NRap language

NRap was created to be used as a 1-to-1 representation of Raptor Flowchart Interpreter programs.

## Overview

```
procedure main() {
    x = "Hello, world"

    // The second parameter indicates if a newline should be printed
    output(x, true)

    y = 1+2

    if y > 0 {
        // my_x_negated is created automatically if it's an «out» argument
        neg(x, my_x_negated)

        output(my_x_negated, true)
    }

    // Store input in variable «n»
    input("Insert n for fib(n): ", n);
    fib(n, result);
    output("Result: ", false);
    output(result, true);
}

// «out» parameters are used as return values
procedure neg(x, out neg_x) {
    neg_x = -x
}

procedure fib(n, out result) {
    if n < 2 {
        result = n;
    } else {
        fib(n-1, result1);
        fib(n-2, result2);
        result = result1 + result2;
    }
}
```

## Building
debug: `cargo build`

release: `cargo build --release`

The resulting binary is in `nrap/target/[debug | release]`

## Usage
`./nrap <file>`

## Run examples
`cargo run --release ./nrap_examples/fib.nrap`
