open Yojson.Basic.Util

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

let parse_resources json =
    {
        memory_mb = json |> member "memory_mb" |> to_int_option;
        pids_limit = json |> member "pids_limit" |> to_int_option;
        cpus = json |> member "cpus" |> to_number_option;
    }

let parse_port json =
    let h = json |> member "host" |> to_int in
    let c = json |> member "container" |> to_int in
    (h, c)

let parse_mount json =
    let h = json |> member "host" |> to_string in
    let c = json |> member "container" |> to_string in
    (h, c)

let load_config filename =
    try
        let json = Yojson.Basic.from_file filename in

        let parse_list field parser =
            match json |> member field with
            | `Null -> None
            | list_json -> Some (list_json |> to_list |> List.map parser)
        in

        {
            container_name = json |> member "container_name" |> to_string;
            command = json |> member "command" |> to_list |> List.map to_string |> Array.of_list;

            resources = json |> member "resources" |> to_option parse_resources;

            port_mappings = parse_list "port_mappings" parse_port;
            mounts = parse_list "mounts" parse_mount;
        }
    with 
    | Yojson.Json_error msg ->
        failwith (Printf.sprintf "Invalid Json format in %s: %s" filename msg)
    | Type_error (msg, _) ->
        failwith (Printf.sprintf "Json Type mismatch in %s: %s" filename msg)
    | Sys_error msg ->
        failwith (Printf.sprintf "Could not open config file: %s" msg)