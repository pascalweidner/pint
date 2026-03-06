type t = {
    capacity: int;
    buffer: string array;
    mutable head: int;
    mutable total_written: int;
    new_data: unit Lwt_condition.t;
}

let create capacity = {
    capacity;
    buffer = Array.make capacity "";
    head = 0;
    total_written = 0;
    new_data = Lwt_condition.create ();
}

let push t item =
    t.buffer.(t.head) <- item;
    t.head <- (t.head + 1) mod t.capacity;
    t.total_written <- t.total_written + 1;

    Lwt_condition.broadcast t.new_data () 

