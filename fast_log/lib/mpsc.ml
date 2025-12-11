open Base

type 'a node =
  { mutable value : 'a option
  ; mutable next : 'a node
  }

type 'a t =
  { head : 'a node Atomic.t
  ; mutable tail : 'a node
  }

let make () =
  let rec stub = { value = None; next = stub } in
  { head = Atomic.make stub; tail = stub }
;;

let[@inline] push t v =
  let rec n = { value = Some v; next = n } in
  let prev = Atomic.exchange t.head n in
  prev.next <- n
;;

let[@inline] pop t =
  let next_node = t.tail.next in
  if not (phys_equal next_node t.tail)
  then (
    match next_node.value with
    | None -> None
    | Some v ->
      next_node.value <- None;
      t.tail <- next_node;
      Some v)
  else None
;;

let[@inline] is_empty t = phys_equal t.tail.next t.tail
