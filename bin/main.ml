open Base
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

module Logger = Logger.Make (Spsc.Make (Config))

let () =
  Logger.init ();
  (* Logger.log0 ""; *)
  let alice : User.t = { name = "Alice"; age = 30 } in
  let req1 = { url = "/login"; method_ = "POST" } in
  let both = { user = alice; request = req1 } in
  let start = Unix.gettimeofday () in
  for _ = 0 to 100 do
    [%log
      "%f %a %a %a %a"
        55.5
        (alice : User.t)
        (req1 : request)
        (both.user : User.t)
        (both : both)]
  done;
  let elapsed = Unix.gettimeofday () -. start in
  Unix.sleep 1;
  Stdlib.Printf.printf "%.2fns per log\n" (elapsed *. 1e9 /. 100.);
  Logger.stop ()
;;
