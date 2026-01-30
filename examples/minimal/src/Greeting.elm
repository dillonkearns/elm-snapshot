module Greeting exposing (greet)

{-| A simple greeting module.
-}


{-| Create a greeting for a name.

    greet "World" == "Hello, World!"

-}
greet : String -> String
greet name =
    "Hello, " ++ name ++ "!"
