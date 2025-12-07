(** Interface for the Backing Queue, MPSC or SPSPC. *)
module type S = sig
  (** The type of MPSC queues containing elements of type ['a]. *)
  type 'a t

  (** [make ()] creates a new empty queue. *)
  val make : unit -> 'a t

  (** [push t v] enqueues value [v] into queue [t]. *)
  val push : 'a t -> 'a -> unit

  (** [pop t] removes and returns the next element from queue [t].
    
    Returns [Some v] if an element is available, [None] if the queue is empty.
    
    This operation must only be called from a single consumer thread.
    Calling from multiple threads will result in undefined behavior.
    
    Non-blocking: returns immediately even when empty. *)
  val pop : 'a t -> 'a option

  (** [is_empty t] checks if the queue is currently empty.
    
    This is a snapshot check - the queue may become non-empty immediately
    after this returns [true] if producers are active.
    
    Only safe to call from the consumer thread. *)
  val is_empty : 'a t -> bool
end
