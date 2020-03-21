{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "aff-promise"
    , "console"
    , "debug"
    , "effect"
    , "graphql"
    , "psci-support"
    , "react-basic-hooks"
    , "tuples-native"
    , "typelevel"
    ]
, license =
    "Apache-2.0"
, repository =
    "https://github.com/frasermince/purescript-apollo"
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
