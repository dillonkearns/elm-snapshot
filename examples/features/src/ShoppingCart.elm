module ShoppingCart exposing
    ( Cart
    , CartItem
    , DiscountType(..)
    , empty
    , addItem
    , removeItem
    , updateQuantity
    , applyDiscount
    , clearDiscount
    , checkout
    )

{-| A shopping cart state machine.

This demonstrates snapshot testing of stateful transformations -
verify the cart state after a series of operations.

-}


type Cart
    = Cart
        { items : List CartItem
        , discount : Maybe Discount
        , status : CartStatus
        }


type alias CartItem =
    { productId : String
    , name : String
    , price : Int -- cents
    , quantity : Int
    }


type alias Discount =
    { code : String
    , discountType : DiscountType
    }


type DiscountType
    = Percentage Int
    | FixedAmount Int


type CartStatus
    = Shopping
    | CheckedOut { subtotal : Int, discountAmount : Int, total : Int }


{-| Create an empty cart.
-}
empty : Cart
empty =
    Cart
        { items = []
        , discount = Nothing
        , status = Shopping
        }


{-| Add an item to the cart.
-}
addItem : CartItem -> Cart -> Cart
addItem item (Cart cart) =
    case cart.status of
        Shopping ->
            let
                existingItem =
                    List.filter (\i -> i.productId == item.productId) cart.items
                        |> List.head

                updatedItems =
                    case existingItem of
                        Just existing ->
                            List.map
                                (\i ->
                                    if i.productId == item.productId then
                                        { i | quantity = i.quantity + item.quantity }

                                    else
                                        i
                                )
                                cart.items

                        Nothing ->
                            cart.items ++ [ item ]
            in
            Cart { cart | items = updatedItems }

        CheckedOut _ ->
            Cart cart


{-| Remove an item from the cart.
-}
removeItem : String -> Cart -> Cart
removeItem productId (Cart cart) =
    case cart.status of
        Shopping ->
            Cart
                { cart
                    | items = List.filter (\i -> i.productId /= productId) cart.items
                }

        CheckedOut _ ->
            Cart cart


{-| Update the quantity of an item.
-}
updateQuantity : String -> Int -> Cart -> Cart
updateQuantity productId newQuantity (Cart cart) =
    case cart.status of
        Shopping ->
            if newQuantity <= 0 then
                removeItem productId (Cart cart)

            else
                Cart
                    { cart
                        | items =
                            List.map
                                (\i ->
                                    if i.productId == productId then
                                        { i | quantity = newQuantity }

                                    else
                                        i
                                )
                                cart.items
                    }

        CheckedOut _ ->
            Cart cart


{-| Apply a discount code.
-}
applyDiscount : String -> DiscountType -> Cart -> Cart
applyDiscount code discountType (Cart cart) =
    case cart.status of
        Shopping ->
            Cart { cart | discount = Just { code = code, discountType = discountType } }

        CheckedOut _ ->
            Cart cart


{-| Clear any applied discount.
-}
clearDiscount : Cart -> Cart
clearDiscount (Cart cart) =
    case cart.status of
        Shopping ->
            Cart { cart | discount = Nothing }

        CheckedOut _ ->
            Cart cart


{-| Checkout the cart, calculating totals.
-}
checkout : Cart -> Cart
checkout (Cart cart) =
    case cart.status of
        Shopping ->
            let
                subtotal =
                    List.foldl
                        (\item acc -> acc + (item.price * item.quantity))
                        0
                        cart.items

                discountAmount =
                    case cart.discount of
                        Just { discountType } ->
                            case discountType of
                                Percentage pct ->
                                    (subtotal * pct) // 100

                                FixedAmount amount ->
                                    min amount subtotal

                        Nothing ->
                            0

                total =
                    subtotal - discountAmount
            in
            Cart
                { cart
                    | status =
                        CheckedOut
                            { subtotal = subtotal
                            , discountAmount = discountAmount
                            , total = total
                            }
                }

        CheckedOut _ ->
            Cart cart
