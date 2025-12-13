# ppx_defer

Go-like defer, runs at the end of the current lexical scope, even if a exception is thrown.

```ocaml
let f () =
  print_endline "A"
  [%defer print_endline "B"];
  [%defer.if (true), print_endline "C"];
  [%defer print_endline "D"];
  raise (Failure "boom");
  print_endline "Will not reach."
(* =>
    A
    D
    C
    B
    Exception: Failure "boom" *)
```
This does occur some overhead, since we have to allocate closures on a stack (LIFO). The performance hit is in the order of double digit nanoseconds. Roughly 50 nanos to introduce defers to a scope and subsequent defers are roughly 10 nanos. See the [test directory](./test) for benchmarks.

[ppx_defer](.) and [ppx_assert](../ppx_assert) is heavily inspired by [Tigerstyle](https://tigerstyle.dev/)