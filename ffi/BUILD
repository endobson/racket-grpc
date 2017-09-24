load("@minimal_racket//:racket.bzl", "racket_library")

package(
  default_visibility = ["//visibility:public"]
)

cc_binary(
  name = "libgrpc_unsecure.so",
  deps = ["@grpc//:grpc_unsecure"],
  linkshared = 1
)


racket_library(
    name = "base-lib",
    srcs = ["base-lib.rkt"],
    data = [
        ":libgrpc_unsecure.so",
    ]
)

racket_library(
    name = "completion-queue",
    srcs = ["completion-queue.rkt"],
    deps = [
        ":base-lib",
        ":timespec",
    ]
)

racket_library(
    name = "lib",
    srcs = ["lib.rkt"],
    deps = [
        ":base-lib",
        ":completion-queue",
        ":timespec",
    ]
)

racket_library(
    name = "timespec",
    srcs = ["timespec.rkt"],
    deps = [
        ":base-lib",
    ]
)
