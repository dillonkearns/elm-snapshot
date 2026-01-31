module Snapshots exposing (run)

{-| Snapshot tests demonstrating the Elm printer with realistic use cases.

The `Printer.elm` pretty-printer is useful when you want to snapshot
complex Elm data structures in a readable, diffable format. Unlike
just printing raw Elm types, these examples show practical scenarios
where snapshot testing provides real value.

-}

import Pages.Script exposing (Script)
import SearchQuery exposing (Field(..), Operator(..), SortDirection(..))
import ShoppingCart exposing (Cart, DiscountType(..))
import Snapshot
import Snapshot.Printer as Printer


run : Script
run =
    Snapshot.run "Snapshots"
        [ Snapshot.describe "Query Builder"
            [ Snapshot.custom (Printer.elm Debug.toString) "simple filter" <|
                \() ->
                    SearchQuery.empty
                        |> SearchQuery.where_ Status Equals "active"
                        |> SearchQuery.limit 10
            , Snapshot.custom (Printer.elm Debug.toString) "complex search with sorting" <|
                \() ->
                    SearchQuery.empty
                        |> SearchQuery.select [ Name, Email, CreatedAt ]
                        |> SearchQuery.where_ Status Equals "active"
                        |> SearchQuery.where_ Age GreaterThan "18"
                        |> SearchQuery.sortBy CreatedAt Descending
                        |> SearchQuery.sortBy Name Ascending
                        |> SearchQuery.limit 25
                        |> SearchQuery.offset 50
            , Snapshot.custom (Printer.elm Debug.toString) "OR conditions" <|
                \() ->
                    SearchQuery.empty
                        |> SearchQuery.or
                            [ SearchQuery.where_ Status Equals "active"
                            , SearchQuery.where_ Status Equals "pending"
                            ]
                        |> SearchQuery.sortBy CreatedAt Descending
            , Snapshot.custom (Printer.elm Debug.toString) "nested AND/OR conditions" <|
                \() ->
                    SearchQuery.empty
                        |> SearchQuery.and
                            [ SearchQuery.where_ Age GreaterThan "21"
                            , SearchQuery.or
                                [ SearchQuery.where_ Status Equals "premium"
                                , SearchQuery.where_ Status Equals "vip"
                                ]
                            ]
                        |> SearchQuery.limit 100
            ]
        , Snapshot.describe "Shopping Cart State Machine"
            [ Snapshot.custom (Printer.elm Debug.toString) "add items to cart" <|
                \() ->
                    ShoppingCart.empty
                        |> ShoppingCart.addItem
                            { productId = "WIDGET-001"
                            , name = "Blue Widget"
                            , price = 1999
                            , quantity = 2
                            }
                        |> ShoppingCart.addItem
                            { productId = "GADGET-042"
                            , name = "Super Gadget"
                            , price = 4999
                            , quantity = 1
                            }
            , Snapshot.custom (Printer.elm Debug.toString) "update quantity" <|
                \() ->
                    ShoppingCart.empty
                        |> ShoppingCart.addItem
                            { productId = "WIDGET-001"
                            , name = "Blue Widget"
                            , price = 1999
                            , quantity = 1
                            }
                        |> ShoppingCart.updateQuantity "WIDGET-001" 5
            , Snapshot.custom (Printer.elm Debug.toString) "apply percentage discount and checkout" <|
                \() ->
                    ShoppingCart.empty
                        |> ShoppingCart.addItem
                            { productId = "WIDGET-001"
                            , name = "Blue Widget"
                            , price = 2000
                            , quantity = 5
                            }
                        |> ShoppingCart.applyDiscount "SAVE20" (Percentage 20)
                        |> ShoppingCart.checkout
            , Snapshot.custom (Printer.elm Debug.toString) "apply fixed discount and checkout" <|
                \() ->
                    ShoppingCart.empty
                        |> ShoppingCart.addItem
                            { productId = "GADGET-042"
                            , name = "Super Gadget"
                            , price = 4999
                            , quantity = 2
                            }
                        |> ShoppingCart.addItem
                            { productId = "ACCESSORY-007"
                            , name = "Gadget Case"
                            , price = 1500
                            , quantity = 1
                            }
                        |> ShoppingCart.applyDiscount "FLAT10" (FixedAmount 1000)
                        |> ShoppingCart.checkout
            , Snapshot.custom (Printer.elm Debug.toString) "remove item then checkout" <|
                \() ->
                    ShoppingCart.empty
                        |> ShoppingCart.addItem
                            { productId = "A"
                            , name = "Item A"
                            , price = 1000
                            , quantity = 1
                            }
                        |> ShoppingCart.addItem
                            { productId = "B"
                            , name = "Item B (will be removed)"
                            , price = 500
                            , quantity = 3
                            }
                        |> ShoppingCart.addItem
                            { productId = "C"
                            , name = "Item C"
                            , price = 2500
                            , quantity = 1
                            }
                        |> ShoppingCart.removeItem "B"
                        |> ShoppingCart.checkout
            ]
        ]
