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
This does occur some overhead, roughly ~20-50 on consumer hardware, due to extra closures being allocated.