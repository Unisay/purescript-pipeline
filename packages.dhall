let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221026/packages.dhall
        sha256:8dc0b394f5861bb0136f652f3f826a88eaffb2bc0ecf0251468ed668102f5d0c

let overrides = {=}

let additions =
      { custom-prelude =
        { dependencies = [ "debug", "either", "maybe", "prelude", "strings" ]
        , repo = "https://github.com/Unisay/purescript-custom-prelude.git"
        , version = "v1.2.0"
        }
      }

in  upstream // overrides // additions
