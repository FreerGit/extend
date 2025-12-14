open Base

module Make () : Queue_intf.S = struct
  type 'a node =
    { mutable next : 'a node option
    ; value : 'a option
    }

  type 'a t =
    { head : 'a node Atomic.t
    ; tail : 'a node Atomic.t
    }

  let make () =
    let dummy = { next = None; value = None } in
    { head = Atomic.make dummy; tail = Atomic.make dummy }
  ;;

  let push t v =
    let node = { next = None; value = Some v } in
    let tail = Atomic.get t.tail in
    tail.next <- Some node;
    Atomic.set t.tail node
  ;;

  let pop t =
    let head = Atomic.get t.head in
    match head.next with
    | None -> None
    | Some node ->
      Atomic.set t.head node;
      node.value
  ;;

  let is_empty t =
    let head = Atomic.get t.head in
    phys_equal head.next None
  ;;
end

(* Tests *)

let%expect_test "basic push/pop" =
  let module Q = Make () in
  let q = Q.make () in
  assert (Q.is_empty q);
  Q.push q 1;
  Q.push q 2;
  assert (Option.equal Int.equal (Q.pop q) (Some 1));
  assert (Option.equal Int.equal (Q.pop q) (Some 2));
  assert (Option.equal Int.equal (Q.pop q) None);
  [%expect {| |}]
;;

let%expect_test "spsc unbounded correctness" =
  let module Q = Make () in
  let q = Q.make () in
  (* Many more pushes than pops*)
  let n = 1024 * 10 in
  let producer =
    Domain.spawn (fun () ->
      for i = 1 to n do
        Q.push q i
      done)
  in
  let cnt = ref 0 in
  let consumer =
    Domain.spawn (fun () ->
      while not (!cnt = n) do
        match Q.pop q with
        | None -> ()
        | Some _ -> Int.incr cnt
      done)
  in
  Domain.join producer;
  Domain.join consumer;
  Stdlib.print_endline @@ Int.to_string !cnt;
  [%expect {| 10240 |}]
;;

(* Benches *)

let%bench_fun "push/pop pair" =
  let module Q = Make () in
  let q = Q.make () in
  fun () ->
    Q.push q 1;
    Q.pop q |> ignore
;;
