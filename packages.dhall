let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221111/packages.dhall
        sha256:57b859ee75d213c899af3fe38667a96ae445b92216793a76a92836fc833de967

let overrides = {=}

let additions =
      { custom-prelude =
        { dependencies = [ "debug", "either", "maybe", "prelude", "strings" ]
        , repo = "https://github.com/Unisay/purescript-custom-prelude.git"
        , version = "v1.2.0"
        }
      }

in  upstream // overrides // additions
