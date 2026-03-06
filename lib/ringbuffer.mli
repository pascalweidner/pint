type t = {
    capacity: int;
    buffer: string array;
    mutable head: int;
    mutable total_written: int;
    new_data: unit Lwt_condition.t;
}

val create: int -> t

val push: t -> string -> unit
