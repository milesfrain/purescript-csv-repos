{ name = "my-project"
, dependencies = [ "affjax", "argonaut", "console", "effect", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
