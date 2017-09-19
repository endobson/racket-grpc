workspace(name = "racket_protobuf")

http_archive(
  name = "minimal_racket",
  sha256 = "4d431edf7784e4c869cb03f1dc371c9cc4b714dcfc23734b23dee0d6c21b7a51",
  strip_prefix = "minimal-racket-b05cd33865d17977ac75f3cc635f6a0b075ff054",
  urls = ["https://github.com/endobson/minimal-racket/archive/b05cd33865d17977ac75f3cc635f6a0b075ff054.tar.gz"]
)

load("@minimal_racket//:releases.bzl", "racket_releases")
racket_releases()

