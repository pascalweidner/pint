val destroy_cgroup: int -> unit
val setup_cgroup: int -> unit
val add_limits: Parse.resource_config option -> int -> unit