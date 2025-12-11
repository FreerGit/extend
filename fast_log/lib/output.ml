open Base

type t =
  { write : string -> unit
  ; flush : unit -> unit
  }

let dev_null () = { write = (fun _ -> ()); flush = (fun () -> ()) }

let stdout () =
  { write = Stdlib.output_string Stdlib.stdout
  ; flush = (fun () -> Stdlib.flush Stdlib.stdout)
  }
;;

let stderr () =
  { write = Stdlib.output_string Stdlib.stderr
  ; flush = (fun () -> Stdlib.flush Stdlib.stderr)
  }
;;

let file path =
  let oc = Stdlib.open_out path in
  { write = Stdlib.output_string oc; flush = (fun () -> Stdlib.flush oc) }
;;
