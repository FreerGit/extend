open Base

module Make (Config : sig
    val size : int
  end) : Queue_intf.S = struct
  type 'a t =
    { buf : 'a option array
    ; mask : int
    ; mutable head : int
    ; mutable tail : int
    }

  let make () =
    let size = 1 lsl Config.size in
    { buf = Array.create ~len:size None; mask = size - 1; head = 0; tail = 0 }
  ;;

  let[@inline] push t v =
    let next = (t.head + 1) land t.mask in
    if next = t.tail
    then ()
    else (
      t.buf.(t.head) <- Some v;
      t.head <- next)
  ;;

  let[@inline] pop t =
    if t.tail = t.head
    then None
    else (
      let v = t.buf.(t.tail) in
      t.buf.(t.tail) <- None;
      t.tail <- (t.tail + 1) land t.mask;
      v)
  ;;

  let[@inline] is_empty t = t.head = t.tail
end
