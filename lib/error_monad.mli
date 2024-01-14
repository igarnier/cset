type error = ..

type 'a t = ('a, error list) result

val register : (error -> 'a option) -> (Format.formatter -> 'a -> unit) -> unit

val pp_trace : Format.formatter -> error list -> unit

val ( let* ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result

val ( let+ ) : ('a, 'b) result -> ('a -> 'c) -> ('c, 'b) result

val map : ('a -> 'b) -> ('a, 'c) result -> ('b, 'c) result

val return_unit : (unit, 'a) result

val return : 'a -> ('a, 'b) result

val return_some : 'a -> ('a option, 'b) result

val return_none : ('a option, 'b) result

val map_error : ('a -> 'b) -> ('c, 'a) result -> ('c, 'b) result

val lift_option : ('a, 'b) result -> ('a option, 'b) result

val fail : error -> 'a t

val trace : (unit -> error) -> (unit -> 'b t) -> 'b t

val run : 'a t -> 'a
