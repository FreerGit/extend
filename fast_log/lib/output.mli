type t =
  { write : string -> unit
  ; flush : unit -> unit
  }

val stdout : unit -> t
val stderr : unit -> t
val dev_null : unit -> t
val file : string -> t
