open! Base
open Core_bench
open Fast_log

module User = struct
  type t =
    { name : string
    ; age : int
    }
  [@@deriving log_printer]
end

type request =
  { url : string
  ; method_ : string
  }
[@@deriving log_printer]

type both =
  { user : User.t
  ; request : request
  }
[@@deriving log_printer]

module Config = struct
  let size = 1028
end

module Logger_SPSC = Logger.Make (Spsc.Make (Config))
module Logger_MPSC = Logger.Make (Mpsc)
module Logger = Logger_SPSC

let alice : User.t = { name = "Alice"; age = 30 }
let req1 = { url = "/login"; method_ = "POST" }
let both = { user = alice; request = req1 }
let writes = ref 0
let pushes = ref 0

let t ~name =
  Int.incr pushes;
  Bench.Test.create ~name (fun _ ->
    [%log
      "%s %f %a %a %a %a\n"
        name
        55.5
        (alice : User.t)
        (req1 : request)
        (both.user : User.t)
        (both : both)];
    ())
;;

0;;

let incr_output () : Output.t =
  { write = (fun _ -> Int.incr writes); flush = (fun () -> ()) }
;;

let () =
  Logger_SPSC.init ~outputs:[ incr_output () ] ();
  Command_unix.run (Bench.make_command [ t ~name:"SPSC" ]);
  Logger_SPSC.stop ();
  assert (!writes = !pushes)
;;
(* Logger_MPSC.init ~outputs:[ Output.stdout () ] ();
  Command_unix.run (Bench.make_command [ t ~name:"MPSC" ]);
  Logger_MPSC.stop () *)
