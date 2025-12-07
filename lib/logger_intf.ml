module type S = sig
  type _ ty =
    | TInt : int ty
    | TFloat : float ty
    | TBool : bool ty
    | TString : string ty
    | TChar : char ty
    | TCustom : (Buffer.t -> 'a -> unit) -> 'a ty

  (** A single log argument, existentially quantified.
    
    The type witness ['a ty] proves what type the value has,
    enabling type-safe printing on the consumer thread. *)
  type arg = Arg : 'a ty * 'a -> arg

  (** [log format args] enqueues a log entry.
        
    The [format] string contains placeholders like [%d], [%f], [%a], etc.
    The [args] array must match the placeholders in order.
    
    Typically called via the [[%log ...]] PPX extension:
    
    {[
      type user = { name : string; id : int } [@@deriving log_printer]
      
      let alice = { name = "Alice"; id = 42 } in
      [%log "User %a logged in at %f" (alice : user) (Unix.gettimeofday ())]
    ]} *)
  val log : string -> arg array -> unit

  module Logger_Interal : sig
    val log0 : string -> unit
    val log1 : string -> arg -> unit
    val log2 : string -> arg -> arg -> unit
    val log3 : string -> arg -> arg -> arg -> unit
    val log4 : string -> arg -> arg -> arg -> arg -> unit
    val log5 : string -> arg -> arg -> arg -> arg -> arg -> unit
    val log6 : string -> arg -> arg -> arg -> arg -> arg -> arg -> unit
  end

  (** [init ()] starts the logger.
    
    - Spawns the consumer domain
    - Begins processing log entries asynchronously
    
    Must be called before any logging occurs. *)
  val init : unit -> unit

  (** [stop ()] gracefully shuts down the logger.
    
    - Signals the consumer to stop
    - Waits for it to finish processing remaining entries
    - Joins the consumer domain
    
    After calling [stop], no more logs will be processed. *)
  val stop : unit -> unit
end
