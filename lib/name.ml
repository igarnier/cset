module type S = sig
  type t

  val compare : t -> t -> int

  val make : unit -> t
end

module Make () = struct
  type t = int

  let compare = compare

  let make =
    let c = ref 0 in
    fun () ->
      incr c ;
      !c
end
