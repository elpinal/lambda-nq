# Lambda NQ

Lambda NQ with call-by-value, left-to-right evaluation.

## Build

Prerequisites: MLKit or MLton, and [cmyacc](http://www.cs.cmu.edu/~crary/cmtool/).

```bash
$ make mlkit
```

Or,

```bash
$ make mlton
```

## Usage

```bash
$ ./lambda-nq [filename]
```

## Examples

```
catch X : unit =>
  throw X () : bottom
```

```
catch X : unit =>
catch Y : bottom =>
  throw Y (throw X () : bottom) : bottom
```

The following is ill-typed because `X` is inaccessible inside tha lambda abstraction:

```
catch X : unit -> unit =>
  fn x : unit =>
    throw X (fn y : unit => y) : unit
```

## Reference

Yosuke Fukuda and Ryosuke Igarashi.
A Reconstruction of Ex Falso Quodlibet via Quasi-Multiple-Conclusion Natural Deduction.
Logic, Rationality, and Interaction, 2017.
[DOI](https://doi.org/10.1007/978-3-662-55665-8_38).
