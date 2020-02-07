{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "profligate"
, dependencies =
    [ "aff"
    , "console"
    , "datetime"
    , "effect"
    , "foreign"
    , "halogen"
    , "node-fs-aff"
    , "numbers"
    , "prelude"
    , "psci-support"
    , "spec"
    , "string-parsers"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
