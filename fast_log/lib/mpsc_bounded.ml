open Base

module Make (Config : sig
    val size_exponent : int (* power of 2 *)
  end) : Queue_intf.S = struct
  type 'a t =
    { buf : 'a option array
    ; mask : int
    ; head : int Atomic.t (* producers increment *)
    ; tail : int Atomic.t (* consumer increments *)
    }

  let make () =
    let size = 1 lsl Config.size_exponent in
    { buf = Array.create ~len:size None
    ; mask = size - 1
    ; head = Atomic.make 0
    ; tail = Atomic.make 0
    }
  ;;

  let push t v =
    let rec loop () =
      let head = Atomic.get t.head in
      let next = (head + 1) land t.mask in
      let tail = Atomic.get t.tail in
      (* full *)
      if Int.equal next tail
      then ()
      else if Atomic.compare_and_set t.head head next
      then t.buf.(head) <- Some v
      else loop ()
    in
    loop ()
  ;;

  let pop t =
    let tail = Atomic.get t.tail in
    let head = Atomic.get t.head in
    if Int.equal tail head
    then None
    else (
      let v = t.buf.(tail) in
      t.buf.(tail) <- None;
      Atomic.set t.tail ((tail + 1) land t.mask);
      v)
  ;;

  let is_empty t = Atomic.get t.head = Atomic.get t.tail
end

let%expect_test "basic push/pop" =
  let module Q =
    Make (struct
      let size_exponent = 10
    end)
  in
  let q = Q.make () in
  assert (Q.is_empty q);
  Q.push q 1;
  Q.push q 2;
  assert (Option.equal Int.equal (Q.pop q) (Some 1));
  assert (Option.equal Int.equal (Q.pop q) (Some 2));
  assert (Option.equal Int.equal (Q.pop q) None);
  [%expect {|  |}]
;;

let%expect_test "bounded mpsc correctness" =
  let module Q =
    Make (struct
      let size_exponent = 10
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
      for _ = 1 to n do
        match Q.pop q with
        | None -> ()
        | Some _ -> Int.incr cnt
      done)
  in
  Domain.join producer;
  Domain.join consumer;
  Stdlib.print_endline @@ Int.to_string !cnt;
  [%expect {| 1023 |}]
;;
