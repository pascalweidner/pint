# Pint: A lightweight container runtime

Pint is a lightweight conatiner runtime

# Features
- Can create and run a container
    - Generates unique id
    - Creates a folder for each container in /var/lib/pint/containers/<container-id>
    - Creates a rootfs folder in the container path where the image of alpinefx is copied to
    - Copies the json config too the folder
    - Downloads alpine only if it is not already downloaded, but still copies it everytime = Next improvement would be overlay filesystem
    - Also creates a run folder for temporary stuff, so if the container crashes we don't have to clean it up, as a restart of the computer does this for us
    - We start the shim with execv (so that we can use LWT and don't get problems with fork)
    - if we started the container as a deamon it starts in the background and the cli detaches
    - if we started with interactive in the foreground, we first try to connect with the stdin socket to send data to the shim over a file in the run folder of the container. Then we set the terminal to raw mode and register a callback that the terminal is reset after the cli-tool exits.
    - And then we read the input, if we get a a Ctrl+P + Ctrl+Q we detach
    - Then we also connect to a log_socket (which sends all the logs from the backend to us and a cmd_socket to stop or controll the shim)


## Examples
A simple example:
```

```