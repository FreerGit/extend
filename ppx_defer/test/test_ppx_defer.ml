open Ppx_defer_runtime

let%expect_test "withdefer with exception" =
  let f () =
    with_defer (fun push ->
      push (fun () -> Stdio.print_endline "cleanup 1");
      push (fun () -> Stdio.print_endline "cleanup 2");
      Stdio.print_endline "about to fail";
      failwith "boom")
  in
  (try f () with
   | Failure msg -> Stdio.printf "caught: %s\n" msg);
  [%expect
    {|
    about to fail
    cleanup 2
    cleanup 1
    caught: boom |}]
;;

let%expect_test "multiple defers in order" =
  with_defer (fun push ->
    push (fun () -> Stdio.print_endline "third");
    push (fun () -> Stdio.print_endline "second");
    push (fun () -> Stdio.print_endline "first");
    Stdio.print_endline "body");
  [%expect
    {|
    body
    first
    second
    third |}]
;;

let%expect_test "basic defer" =
  let f =
    fun () ->
    [%defer print_endline "3"];
    [%defer print_endline "2"];
    [%defer print_endline "1"];
    print_endline "0";
    "return"
  in
  print_endline @@ f ();
  [%expect
    {|
    0
    1
    2
    3
    return
    |}]
;;

(* Benches  *)

let%bench "with_defer overhead - no defers" = with_defer (fun _push -> 42)

let%bench "with_defer overhead - w/ closure " =
  with_defer (fun _push ->
    let f = fun () -> () in
    f ())
;;

let%bench "with_defer overhead - 1 defer" =
  with_defer (fun push ->
    push (fun () -> ());
    42)
;;

let%bench "ppx_defer ovearhead assert" =
  [%defer assert true];
  [%defer assert true]
;;

let%bench "ppx_defer.if ovearhead assert" = [%defer.if true, assert true]
let%bench "baseline - ppx_defer assert" = assert true
let%bench "baseline - no defer mechanism" = 42
