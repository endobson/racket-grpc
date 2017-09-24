load(
    "@minimal_racket//:racket.bzl",
    "racket_binary",
    "racket_library",
    "racket_collection",
)

package(
  default_visibility = ["//visibility:public"]
)

racket_library(
    name = "buffer-reader",
    srcs = ["buffer-reader.rkt"],
    deps = [
        "//ffi:lib",
    ],
)

racket_library(
    name = "client-call",
    srcs = ["client-call.rkt"],
    deps = [
        "//ffi:lib",
        "//ffi:call",
        ":grpc-op-batch",
        ":malloc-util",
        ":buffer-reader",
    ],
)

racket_library(
    name = "client",
    srcs = ["client.rkt"],
    deps = [
        "//ffi:lib",
        "//ffi:call",
        ":grpc-op-batch",
        ":malloc-util",
        ":buffer-reader",
    ],
)

racket_library(
    name = "continuous-client",
    srcs = ["continuous-client.rkt"],
    deps = [
        "//ffi:lib",
        "//ffi:channel",
        ":client",
    ],
)

racket_library(
    name = "grpc-op-batch",
    srcs = ["grpc-op-batch.rkt"],
    deps = [
        "//ffi:lib",
    ],
)

racket_library(
    name = "malloc-util",
    srcs = ["malloc-util.rkt"],
    deps = [
        "//ffi:lib",
        "//ffi:slice",
    ],
)

racket_library(
    name = "return-box",
    srcs = ["return-box.rkt"],
    deps = [
        "//ffi:lib",
    ],
)

racket_library(
    name = "serial-client",
    srcs = ["serial-client.rkt"],
    deps = [
        ":client",
        "//ffi:lib",
        "//ffi:channel",
    ],
)

racket_library(
    name = "server-call",
    srcs = ["server-call.rkt"],
    deps = [
        ":grpc-op-batch",
        ":buffer-reader",
        ":timestamp",
        ":return-box",
        ":status",
        ":malloc-util",
        "//ffi:lib",
        "//ffi:call",
    ],
)

racket_library(
    name = "server-main",
    srcs = ["server-main.rkt"],
    deps = [
        ":server",
    ],
)

racket_library(
    name = "server",
    srcs = ["server.rkt"],
    deps = [
        "//ffi:lib",
        ":server-call",
        ":timestamp",
        ":grpc-op-batch",
        ":buffer-reader",
        ":status",
    ],
)

racket_library(
    name = "status",
    srcs = ["status.rkt"],
)

racket_library(
    name = "timestamp",
    srcs = ["timestamp.rkt"],
)

