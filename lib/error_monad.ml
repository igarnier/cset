type error = ..

type info =
  | Info :
      { decode : error -> 'a option; pp : Format.formatter -> 'a -> unit }
      -> info

type 'a t = ('a, error list) result

exception Trace of error list

let table = ref []

let register decode pp = table := Info { decode; pp } :: !table

let ( let* ) = Result.bind

let ( let+ ) m f = Result.map f m

let map = Result.map

let return_unit = Result.ok ()

let return = Result.ok

let return_some x = Result.ok (Some x)

let return_none = Result.ok None

let map_error = Result.map_error

let lift_option x = Result.map Option.some x

let fail e : 'a t = Error [e]

let trace err m =
  match m () with
  | Ok _ as res -> res
  | Error tr -> Error (err () :: tr)
  | exception Trace e -> raise (Trace (err () :: e))

let rec lookup_and_print fmt err infos =
  match infos with
  | [] -> Format.fprintf fmt "Unregistered error\n"
  | Info { decode; pp } :: rest -> (
      match decode err with
      | None -> lookup_and_print fmt err rest
      | Some decoded -> Format.fprintf fmt "%a\n" pp decoded)

let pp_trace fmtr tr =
  let error_buff = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer error_buff in
  List.iter (fun err -> lookup_and_print fmt err !table) tr ;
  Format.fprintf fmt "@?" ;
  let result = Buffer.contents error_buff in
  Format.fprintf fmtr "%s" result

let () =
  Printexc.register_printer (function
      | Trace tr -> Some (Format.asprintf "%a" pp_trace tr)
      | _ -> None)

let run res = match res with Ok v -> v | Error tr -> raise (Trace tr)
