load("@minimal_racket//:racket.bzl", "racket_binary", "racket_library")

racket_library(
    name = "echo-server",
    srcs = ["echo-server.rkt"],
    deps = [
        "//:server"
    ]
)
        
racket_binary(
    name = "echo-server-bin",
    main_module = "echo-server.rkt",
    deps = [
        ":echo-server"
    ]
)

racket_library(
    name = "hello-world-client",
    srcs = ["hello-world-client.rkt"],
    deps = [
        "//:client-call",
        "//ffi:channel",
        "//ffi:completion-queue",
    ]
)

racket_binary(
    name = "hello-world-client-bin",
    main_module = "hello-world-client.rkt",
    deps = [
        ":hello-world-client"
    ]
)
