{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "data-default"
  , "effect"
  , "indexed-array"
  , "psci-support"
  , "quickcheck-utf8"
  , "spec"
  , "timeline-identifiers"
  , "timeline-time"
  , "unique-array"
  , "uuid"
  , "web-html"
  , "zeta"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
