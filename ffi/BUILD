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
    name = "lib",
    srcs = ["lib.rkt"],
    data = [
        ":libgrpc_unsecure.so",
    ]
)
