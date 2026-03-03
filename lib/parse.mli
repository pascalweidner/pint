type resource_config = {
    memory_mb: int option;
    pids_limit: int option;
    cpus: float option;
}

type container_config = {
    container_name: string;
    command: string array;
    resources: resource_config option;
    port_mappings: (int * int) list option;
    mounts: (string * string) list option
}

val load_config: string -> container_config