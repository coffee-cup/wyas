# write you a scheme

A very basic Scheme implementation in Haskell. Heavily based on [Part 1](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) and [Part 2](https://wespiser.com/writings/wyas/home.html)

## Compiling

```sh
> stack build
```

## Running

```sh
stack exec wyas -- -h
Executable binary for Write You A Scheme v2.0

Usage: wyas ((-s|--script SCRIPT) | (-r|--repl))
  contains an entry point for both running scripts and repl

Available options:
  -h,--help                Show this help text
  -s,--script SCRIPT       File containing the script you want to run
  -r,--repl                Run as interavtive read/evaluate/print/loop
```

### Repl

```sh
> stack exec wyas -- -r
wyas> (+ 1 2)
3
```

### Scripts

```lisp
;; test.scm
(define hello (lambda ()
                "world"))

(hello)
```

```sh
> stack exec wyas -- -s test.scm
"world"
```
