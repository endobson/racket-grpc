workspace(name = "racket_grpc")

http_archive(
  name = "minimal_racket",
  sha256 = "4d431edf7784e4c869cb03f1dc371c9cc4b714dcfc23734b23dee0d6c21b7a51",
  strip_prefix = "minimal-racket-b05cd33865d17977ac75f3cc635f6a0b075ff054",
  urls = ["https://github.com/endobson/minimal-racket/archive/b05cd33865d17977ac75f3cc635f6a0b075ff054.tar.gz"]
)

load("@minimal_racket//:releases.bzl", "racket_releases")
racket_releases()

http_archive(
  name = "com_google_protobuf",
  sha256 = "8b3a82704fbf5202c3bcfbbe6b2eb4d07d85bcb507876aaf60edff751c821854",
  strip_prefix = "protobuf-hack-wkt",
  urls = ["https://github.com/endobson/protobuf/archive/hack-wkt.tar.gz"]
)

http_archive(
  name = "com_google_protobuf_cc",
  sha256 = "8b3a82704fbf5202c3bcfbbe6b2eb4d07d85bcb507876aaf60edff751c821854",
  strip_prefix = "protobuf-hack-wkt",
  urls = ["https://github.com/endobson/protobuf/archive/hack-wkt.tar.gz"]
)


# Bind rules for grpc
bind(
    name = "protobuf",
    actual = "@com_google_protobuf//:protobuf",
)

bind(
    name = "protobuf_clib",
    actual = "@com_google_protobuf//:protoc_lib",
)

bind(
    name = "libssl",
    actual = "@boringssl//:ssl",
)

bind(
    name = "zlib",
    actual = "@zlib_repo//:z",
)

bind(
    name = "nanopb",
    actual = "@grpc//third_party/nanopb",
)

bind(
    name = "cares",
    actual = "@cares_repo//:ares",
)


http_archive(
  name = "grpc",
  sha256 = "f0143c99942f47986713a92fca43b2fe8441e46f30caea32c9430f31600a9808",
  strip_prefix = "grpc-1.6.0",
  urls = ["https://github.com/grpc/grpc/archive/v1.6.0.tar.gz"]
)

http_archive(
  name = "boringssl",
  sha256 = "ddac4087c6cde221731290f884565f8457f0d00e25079785ba24282a999160f3",
  strip_prefix = "boringssl-74ffd81aa7ec3d0aa3d3d820dbeda934958ca81a",
  urls = ["https://github.com/google/boringssl/archive/74ffd81aa7ec3d0aa3d3d820dbeda934958ca81a.tar.gz"]
)

new_http_archive(
  name = "zlib_repo",
  sha256 = "629380c90a77b964d896ed37163f5c3a34f6e6d897311f1df2a7016355c45eff",
  strip_prefix = "zlib-1.2.11",
  urls = ["https://github.com/madler/zlib/archive/v1.2.11.tar.gz"],
  build_file = "@grpc//:third_party/zlib.BUILD"
)

http_archive(
  name = "cares_repo",
  sha256 = "4fb5938bcf490a12372a903dda5bcb20c81c84ba2ddbe29b9371e074bd0c4af8",
  strip_prefix = "c-ares-extra-dir-1.12.0",
  urls = ["https://github.com/endobson/c-ares/archive/extra-dir-1.12.0.tar.gz"],
)
