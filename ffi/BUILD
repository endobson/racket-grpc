load("@minimal_racket//:racket.bzl",  "racket_test", "racket_library")

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
    name = "completion-queue-test",
    srcs = ["completion-queue-test.rkt"],
    deps = [
        ":completion-queue",
    ]
)

racket_test(
    name = "completion-queue_test",
    main_module = "completion-queue-test.rkt",
    deps = [
        ":completion-queue-test",
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
