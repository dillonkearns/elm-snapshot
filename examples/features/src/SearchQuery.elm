module SearchQuery exposing
    ( Query
    , Field(..)
    , Operator(..)
    , SortDirection(..)
    , empty
    , where_
    , and
    , or
    , sortBy
    , limit
    , offset
    , select
    )

{-| A fluent query builder for constructing search queries.

This demonstrates a common pattern where snapshot testing is valuable:
testing that a builder API produces the expected data structure.

-}


type Query
    = Query
        { conditions : List Condition
        , sorting : List ( Field, SortDirection )
        , limitValue : Maybe Int
        , offsetValue : Maybe Int
        , selectedFields : List Field
        }


type Condition
    = And (List Condition)
    | Or (List Condition)
    | Compare Field Operator Value


type Field
    = Name
    | Email
    | Age
    | CreatedAt
    | Status
    | Price


type Operator
    = Equals
    | NotEquals
    | GreaterThan
    | LessThan
    | Contains
    | StartsWith


type Value
    = StringValue String
    | IntValue Int
    | BoolValue Bool


type SortDirection
    = Ascending
    | Descending


{-| Create an empty query.
-}
empty : Query
empty =
    Query
        { conditions = []
        , sorting = []
        , limitValue = Nothing
        , offsetValue = Nothing
        , selectedFields = []
        }


{-| Add a where condition.
-}
where_ : Field -> Operator -> String -> Query -> Query
where_ field op value (Query q) =
    Query
        { q
            | conditions =
                q.conditions ++ [ Compare field op (StringValue value) ]
        }


{-| Combine conditions with AND.
-}
and : List (Query -> Query) -> Query -> Query
and builders (Query q) =
    let
        extractedConditions =
            List.concatMap
                (\builder ->
                    case builder empty of
                        Query inner ->
                            inner.conditions
                )
                builders
    in
    Query { q | conditions = q.conditions ++ [ And extractedConditions ] }


{-| Combine conditions with OR.
-}
or : List (Query -> Query) -> Query -> Query
or builders (Query q) =
    let
        extractedConditions =
            List.concatMap
                (\builder ->
                    case builder empty of
                        Query inner ->
                            inner.conditions
                )
                builders
    in
    Query { q | conditions = q.conditions ++ [ Or extractedConditions ] }


{-| Add sorting.
-}
sortBy : Field -> SortDirection -> Query -> Query
sortBy field direction (Query q) =
    Query { q | sorting = q.sorting ++ [ ( field, direction ) ] }


{-| Set the limit.
-}
limit : Int -> Query -> Query
limit n (Query q) =
    Query { q | limitValue = Just n }


{-| Set the offset.
-}
offset : Int -> Query -> Query
offset n (Query q) =
    Query { q | offsetValue = Just n }


{-| Select specific fields.
-}
select : List Field -> Query -> Query
select fields (Query q) =
    Query { q | selectedFields = fields }
