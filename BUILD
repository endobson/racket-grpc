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
    name = "client-call",
    srcs = ["client-call.rkt"],
    deps = [
        "//ffi:lib",
        "//ffi:call",
        "//ffi:immobile-pointers",
        ":grpc-op-batch",
        ":malloc-util",
    ],
)

racket_library(
    name = "grpc-op-batch",
    srcs = ["grpc-op-batch.rkt"],
    deps = [
        "//ffi:call",
        "//ffi:lib",
    ],
)

racket_library(
    name = "malloc-util",
    srcs = ["malloc-util.rkt"],
    deps = [
        "//ffi:byte-buffer",
        "//ffi:call",
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
    name = "server-call",
    srcs = ["server-call.rkt"],
    deps = [
        ":grpc-op-batch",
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

