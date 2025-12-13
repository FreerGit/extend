open Base

let with_defer f =
  let stack = Stack.create () in
  let push thunk = Stack.push stack thunk in
  match f push with
  | result ->
    Stack.iter stack ~f:(fun thunk -> thunk ());
    result
  | exception exn ->
    Stack.iter stack ~f:(fun thunk -> thunk ());
    raise exn
;;
