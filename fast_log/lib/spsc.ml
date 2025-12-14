open Base

module Make (Config : sig
    val size : int (* size - 1 *)
  end) : Queue_intf.S = struct
  type 'a t =
    { buf : 'a option array
    ; mask : int
    ; head : int Atomic.t
    ; tail : int Atomic.t
    }

  let make () =
    let size = Config.size in
    { buf = Array.create ~len:size None
    ; mask = size - 1
    ; head = Atomic.make 0
    ; tail = Atomic.make 0
    }
  ;;

  let[@inline] push t v =
    let head = Atomic.get t.head in
    let next = (head + 1) land t.mask in
    let tail = Atomic.get t.tail in
    if next = tail
    then ()
    else (
      t.buf.(head) <- Some v;
      Atomic.set t.head next;
      ())
  ;;

  let[@inline] pop t =
    let tail = Atomic.get t.tail in
    let head = Atomic.get t.head in
    if tail = head
    then None
    else (
      let v = t.buf.(tail) in
      t.buf.(tail) <- None;
      Atomic.set t.tail ((tail + 1) land t.mask);
      v)
  ;;

  let[@inline] is_empty t = Atomic.get t.head = Atomic.get t.tail
end

(* Tests *)

let%expect_test "basic push/pop" =
  let module Q =
    Make (struct
      let size = 1024
    end)
  in
  let q = Q.make () in
  assert (Q.is_empty q);
  Q.push q 1;
  Q.push q 2;
  assert (phys_equal (Q.pop q) (Some 1));
  assert (phys_equal (Q.pop q) (Some 2));
  assert (phys_equal (Q.pop q) None);
  [%expect {|  |}]
;;

let%expect_test "spsc correctness" =
  let module Q =
    Make (struct
      let size = 1024
    end)
  in
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
      for i = 1 to n do
        match Q.pop q with
        | None -> ()
        | Some v ->
          assert (phys_equal v i);
          Int.incr cnt
      done)
  in
  Domain.join producer;
  Domain.join consumer;
  Stdlib.print_endline @@ Int.to_string !cnt;
  [%expect {| 1023 |}]
;;

(* Benches *)

let%bench_fun "push/pop pair" =
  let module Q =
    Make (struct
      let size = 1024
    end)
  in
  let q = Q.make () in
  fun () ->
    Q.push q 1;
    Q.pop q |> ignore
;;
