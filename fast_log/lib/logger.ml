open Base
open Output

module Make (Q : Queue_intf.S) : Logger_intf.S = struct
  type _ ty =
    | TInt : int ty
    | TFloat : float ty
    | TBool : bool ty
    | TString : string ty
    | TChar : char ty
    | TCustom : (Buffer.t -> 'a -> unit) -> 'a ty

  type arg = Arg : 'a ty * 'a -> arg

  type entry =
    { format : string
    ; args : arg array
    }

  let queue : entry Q.t = Q.make ()
  let running = ref false
  let consumer_thread : unit Domain.t option ref = ref None
  let pool_max_size = 128
  let format_buffer = Buffer.create 256
  let to_output = ref [||]

  let pools : arg array Queue.t array =
    Array.init (pool_max_size + 1) ~f:(fun _ -> Queue.create ())
  ;;

  let locks = Array.init (pool_max_size + 1) ~f:(fun _ -> Stdlib.Mutex.create ())

  let[@inline] get_args_array size =
    if size <= pool_max_size
    then (
      Stdlib.Mutex.lock locks.(size);
      let result =
        match Queue.dequeue pools.(size) with
        | Some arr -> arr
        | None -> Array.create ~len:size (Arg (TInt, 0))
      in
      Stdlib.Mutex.unlock locks.(size);
      result)
    else Array.create ~len:size (Arg (TInt, 0))
  ;;

  let[@inline] return_args_array arr =
    let size = Array.length arr in
    if size <= pool_max_size
    then (
      let lock = locks.(size) in
      Stdlib.Mutex.lock lock;
      if Queue.length pools.(size) < 100 then Queue.enqueue pools.(size) arr;
      Stdlib.Mutex.unlock lock)
  ;;

  let[@inline] print_arg (buf : Buffer.t) (Arg (ty, v) : arg) : unit =
    match ty with
    | TInt -> Buffer.add_string buf (Int.to_string v)
    | TFloat -> Buffer.add_string buf (Float.to_string v)
    | TBool -> Buffer.add_string buf (Bool.to_string v)
    | TString -> Buffer.add_string buf v
    | TChar -> Buffer.add_char buf v
    | TCustom f -> f buf v
  ;;

  let[@inline] format_entry (e : entry) =
    Buffer.clear format_buffer;
    let fmt = e.format in
    let len = String.length fmt in
    let i = ref 0 in
    let arg_idx = ref 0 in
    while !i < len do
      if Char.equal fmt.[!i] '%' && !i + 1 < len
      then
        if Char.equal fmt.[!i + 1] '%'
        then (
          Buffer.add_char format_buffer '%';
          i := !i + 2)
        else (
          while !i < len && not (Char.is_whitespace fmt.[!i]) do
            Int.incr i
          done;
          if !arg_idx < Array.length e.args
          then (
            print_arg format_buffer e.args.(!arg_idx);
            Int.incr arg_idx))
      else (
        Buffer.add_char format_buffer fmt.[!i];
        Int.incr i)
    done;
    Buffer.contents format_buffer
  ;;

  let[@inline] log format args = Q.push queue { format; args }

  let rec consumer_loop ~sleep_us spins =
    if !running
    then (
      match Q.pop queue with
      | Some entry ->
        let formatted = format_entry entry in
        Array.iter !to_output ~f:(fun output ->
          output.write formatted;
          output.flush ());
        (* Stdlib.print_endline (format_entry entry); *)
        return_args_array entry.args;
        consumer_loop ~sleep_us:(max 10 (sleep_us / 2)) 128
      | None ->
        if spins > 0
        then consumer_loop ~sleep_us (spins - 1)
        else (
          Unix.sleepf (Float.of_int sleep_us /. 1_000_000.);
          consumer_loop ~sleep_us:(min 500 (sleep_us * 3 / 2)) 128))
  ;;

  let init ~outputs () =
    to_output := Array.of_list outputs;
    running := true;
    consumer_thread := Some (Domain.spawn (fun _ -> consumer_loop ~sleep_us:1 128))
  ;;

  let stop () =
    running := false;
    Option.iter !consumer_thread ~f:Domain.join;
    consumer_thread := None
  ;;

  (** Private parts of the Logger *)
  module Logger_Interal = struct
    let[@inline] log0 format = Q.push queue { format; args = [||] }

    let[@inline] log1 format a1 =
      let args = get_args_array 1 in
      args.(0) <- a1;
      Q.push queue { format; args }
    ;;

    let[@inline] log2 format a1 a2 =
      let args = get_args_array 2 in
      args.(0) <- a1;
      args.(1) <- a2;
      Q.push queue { format; args }
    ;;

    let[@inline] log3 format a1 a2 a3 =
      let args = get_args_array 3 in
      args.(0) <- a1;
      args.(1) <- a2;
      args.(2) <- a3;
      Q.push queue { format; args }
    ;;

    let[@inline] log4 format a1 a2 a3 a4 =
      let args = get_args_array 4 in
      args.(0) <- a1;
      args.(1) <- a2;
      args.(2) <- a3;
      args.(3) <- a4;
      Q.push queue { format; args }
    ;;

    let[@inline] log5 format a1 a2 a3 a4 a5 =
      let args = get_args_array 5 in
      args.(0) <- a1;
      args.(1) <- a2;
      args.(2) <- a3;
      args.(3) <- a4;
      args.(4) <- a5;
      Q.push queue { format; args }
    ;;

    let[@inline] log6 format a1 a2 a3 a4 a5 a6 =
      let args = get_args_array 6 in
      args.(0) <- a1;
      args.(1) <- a2;
      args.(2) <- a3;
      args.(3) <- a4;
      args.(4) <- a5;
      args.(5) <- a6;
      Q.push queue { format; args }
    ;;
  end
end
