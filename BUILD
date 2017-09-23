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
        ":lib",
    ],
)

racket_library(
    name = "client-call",
    srcs = ["client-call.rkt"],
    deps = [
        ":lib",
        ":grpc-op-batch",
        ":malloc-util",
        ":buffer-reader",
    ],
)

racket_library(
    name = "client",
    srcs = ["client.rkt"],
    deps = [
        ":lib",
        ":grpc-op-batch",
        ":malloc-util",
        ":buffer-reader",
    ],
)

racket_library(
    name = "continuous-client",
    srcs = ["continuous-client.rkt"],
    deps = [
        ":lib",
        ":client",
        ":place",
    ],
)

racket_library(
    name = "grpc-op-batch",
    srcs = ["grpc-op-batch.rkt"],
    deps = [
        ":lib",
    ],
)

racket_library(
    name = "lib",
    srcs = ["lib.rkt"],
    data = [
        "//:libgrpc_unsecure.so",
    ]
)

racket_library(
    name = "malloc-util",
    srcs = ["malloc-util.rkt"],
    deps = [
        ":lib",
    ],
)

racket_library(
    name = "place",
    srcs = ["place.rkt"],
    deps = [
        ":lib",
    ],
)

racket_library(
    name = "return-box",
    srcs = ["return-box.rkt"],
    deps = [
        ":lib",
    ],
)

racket_library(
    name = "serial-client",
    srcs = ["serial-client.rkt"],
    deps = [
        ":client",
        ":lib",
        ":place",
    ],
)

racket_library(
    name = "server-call",
    srcs = ["server-call.rkt"],
    deps = [
        ":grpc-op-batch",
        ":lib",
        ":buffer-reader",
        ":timestamp",
        ":return-box",
        ":status",
        ":malloc-util",
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
        ":lib",
        ":place",
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

cc_binary(
  name = "libgrpc_unsecure.so",
  deps = ["@grpc//:grpc_unsecure"],
  linkshared = 1
)
