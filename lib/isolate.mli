type clone_flag = 
    | NEWNS (** Mount namespace *)
    | NEWUTS (** Hostname and NIS domain name *)
    | NEWIPC (** System V IPC, POSIX message queues*)
    | NEWUSER (** User and group IDs *)
    | NEWPID (** Process IDs *)
    | NEWNET (** Network stack *)
    | NEWCGROUP (** cGroup *)

(* [namespaces flags] isolates the current process using the provided list
    of namespace types.
    @raise Unix.Unix_error if the system call fails. *)
val namespaces : clone_flag list -> unit