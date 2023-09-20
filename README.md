# NRap language

NRap was created to be used as a 1-to-1 representation of Raptor Flowchart Interpreter programs.

## Overview

```
funcion principal() {
    fib(12, resultadoFib);
    fact(15, resultadoFact);

    imprimir("Resultado fib(12): ", falso);
    imprimir(resultadoFib, verdadero);

    imprimir("Resultado 15!: ", falso);
    imprimir(resultadoFact, verdadero);
}

funcion fib(n, salida resultado) {
    si n < 2 {
        resultado = n;
    } sino {
        fib(n-1, resultado1);
        fib(n-2, resultado2);
        resultado = resultado1 + resultado2;
    }
}

funcion fact(n, salida resultado) {
    resultado = 1;
    ciclo {
        resultado = resultado * n;
        n = n - 1;
        romper si n = 2;
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
