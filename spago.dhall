{ name = "my-project"
, dependencies =
  [ "aff", "console", "custom-prelude", "effect", "prelude", "spec" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
