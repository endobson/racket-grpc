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
        "//ffi:call",
        "//ffi:immobile-pointers",
        ":malloc-util",
    ],
)

racket_library(
    name = "malloc-util",
    srcs = ["malloc-util.rkt"],
    deps = [
        "//ffi:byte-buffer",
        "//ffi:call",
        "//ffi:slice",
    ],
)

racket_library(
    name = "return-box",
    srcs = ["return-box.rkt"],
)

racket_library(
    name = "server-call",
    srcs = ["server-call.rkt"],
    deps = [
        ":timestamp",
        ":return-box",
        ":status",
        ":malloc-util",
        "//ffi:call",
        "//ffi:server",
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
        ":server-call",
        ":timestamp",
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

