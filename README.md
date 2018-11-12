# elm-compiler-fuzz

![Flowchart](flowchart.png)

## The bugs in question

- https://github.com/elm/compiler/issues/1817
- https://github.com/elm/compiler/issues/1837

## Installation and usage

```
$ stack build
$ stack exec -- elm-compiler-fuzz
```

## TODO:

- [ ] actually trigger a crash :)
- [ ] when that happens, implement shrinking
- [ ] if that doesn't happen, rethink code generation (modules probably need to successfully compile?) and mutations (changes in code seem like a promising target)
