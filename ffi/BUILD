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
    name = "alarm",
    srcs = ["alarm.rkt"],
    deps = [
       ":completion-queue",
    ]
)

racket_library(
    name = "base-lib",
    srcs = ["base-lib.rkt"],
    data = [
        ":libgrpc_unsecure.so",
    ]
)

racket_library(
    name = "byte-buffer",
    srcs = ["byte-buffer.rkt"],
    deps = [
        ":base-lib",
        ":slice",
    ]
)

racket_library(
    name = "call",
    srcs = ["call.rkt"],
    deps = [
        ":base-lib",
        ":byte-buffer",
        ":completion-queue",
        ":channel",
        ":immobile-pointers",
        ":metadata-array",
        ":slice",
    ]
)

racket_library(
    name = "channel",
    srcs = ["channel.rkt"],
    deps = [
        ":base-lib",
        ":completion-queue",
    ]
)

racket_library(
    name = "channel-test",
    srcs = ["channel-test.rkt"],
    deps = [
        ":channel",
        ":completion-queue",
        ":timespec",
    ]
)

racket_test(
    name = "channel_test",
    main_module = "channel-test.rkt",
    deps = [
        ":channel-test",
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
        ":alarm",
        ":completion-queue",
        "//time:time",
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
    name = "immobile-pointers",
    srcs = ["immobile-pointers.rkt"],
)

racket_library(
    name = "metadata-array",
    srcs = ["metadata-array.rkt"],
    deps = [
        ":base-lib",
    ]
)

racket_library(
    name = "server",
    srcs = ["server.rkt"],
    deps = [
        ":base-lib",
        ":completion-queue",
        ":timespec",
        ":metadata-array",
    ]
)

racket_library(
    name = "slice",
    srcs = ["slice.rkt"],
    deps = [
        ":base-lib",
    ]
)

racket_library(
    name = "timespec",
    srcs = ["timespec.rkt"],
    deps = [
        ":base-lib",
        "//time:time",
    ]
)
