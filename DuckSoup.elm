module DuckSoup
    exposing
        ( Msg(Answer, Hint, NoMsg, Next, Tick)
        , Prop
        , Scene
        , TimeSpan
        , Modifier(Px, Static)
        , addModifier
        , addStatics
        , button
        , emptyProp
        , label
        , px
        , setText
        , setTextOfProp
        , timeSpan
        , updateScene
        , viewScene
        )

import Dict exposing (Dict, fromList, get, insert, toList, union)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (..)
import List exposing (map)
import Time exposing (Time)


{-| icky, This should be at the app level
-}
type Msg
    = Answer
    | Hint
    | NoMsg
    | Next
    | Tick Time


{-| A Prop is an animatable object
-}
type alias Prop =
    { message : Msg
    , modifiers : List Modifier
    , text : String
    , view : Time -> String -> List Modifier -> Msg -> Html Msg
    }


{-| A Modifier is the modifier of a List Modifier, either static or changing
-   from a begining modifier to an end modifier over a period of time
-}
type Modifier
    = Px String TimeSpan Int Int Int
    | Static String String


{-| A TimeSpan is the starting time and duration of a change
-   of a List Modifier's Modifier.
-}
type alias TimeSpan =
    { start : Time
    , duration : Time
    }


{-| A Scene is a collection of named Props
-}
type alias Scene =
    { props : Dict String Prop
    }


(??) : Maybe a -> a -> a
(??) maybe default =
    case maybe of
        Nothing ->
            default

        Just x ->
            x


{-| Set a trait of a Prop in a Scene, identified by its name
-}
addModifier : Modifier -> Prop -> Prop
addModifier mod prop =
    { prop | modifiers = addUnique mod prop.modifiers }


{-| Add 0 or more modifiers to a Prop.
-}
addStatics : List ( String, String ) -> Prop -> Prop
addStatics pairs prop =
    { prop
        | modifiers = (map (\( k, v ) -> Static k v) pairs) ++ prop.modifiers
    }


{-| Add 0 or more modifiers to a Prop.
-}
addModifiers : Prop -> List Modifier -> Prop
addModifiers prop modNames =
    { prop
        | modifiers = modNames ++ prop.modifiers
    }


addUnique : a -> List a -> List a
addUnique x xs =
    if List.member x xs then
        xs
    else
        x :: xs


{-| Return a Prop that displays text. Position defaults to absolute.
-}
button : ( Int, Int ) -> String -> Msg -> Prop
button ( left, top ) txt msg =
    { message = msg
    , modifiers =
        [ Static "position" "absolute"
        , Static "font-size" "200%"
        , Static "top" (px top)
        , Static "left" (px left)
        , Static "width" (px 300)
        , Static "height" (px 300)
        ]
    , text = txt
    , view = viewButton
    }


{-| Return a bare Prop
-}
emptyProp : Prop
emptyProp =
    { message = NoMsg
    , modifiers = []
    , text = "empty prop"
    , view = viewLabel
    }


{-| Return a Prop that displays text. Position defaults to absolute.
-}
label : ( Int, Int ) -> String -> Prop
label ( left, top ) txt =
    { message = Hint
    , modifiers =
        [ Static "position" "absolute"
        , Static "font-size" "200%"
        , Static "white-space" "nowrap"
        , Static "overflow" "hidden"
        , Static "top" (px top)
        , Static "left" (px left)
        ]
    , text = txt
    , view = viewLabel
    }


px : Int -> String
px number =
    toString number ++ "px"


{-| Set the text of a Prop in a Scene, identified by its name
-}
setText : String -> Prop -> Prop
setText txt prop =
    { prop | text = txt }


{-| Set the text of a Prop in a Scene, identified by its name
-}
setTextOfProp : String -> String -> Scene -> Scene
setTextOfProp propName txt scene =
    let
        prop =
            (get propName scene.props) ?? label ( 10, 10 ) ("Prop " ++ propName ++ " not found!")

        props' =
            insert propName { prop | text = txt } scene.props
    in
        { scene | props = props' }


timeSpan : Time -> Time -> TimeSpan
timeSpan start duration =
    { start = start
    , duration = duration
    }


updateMod : Time -> Modifier -> Modifier
updateMod now v =
    case v of
        Px attr ts start end current ->
            let
                ratio =
                    min 1.0 ((now - ts.start) / ts.duration)

                fStart =
                    toFloat start

                fEnd =
                    toFloat end

                modifier =
                    Debug.log (px current) (Px attr ts start end (truncate (fStart + ((fEnd - fStart)) * ratio)))
            in
                modifier

        _ ->
            v


updateProp : Time -> comparable -> Prop -> Prop
updateProp now k prop =
    { prop | modifiers = List.map (updateMod now) prop.modifiers }


updateScene : Time -> Scene -> Scene
updateScene now scene =
    { scene | props = Dict.map (updateProp now) scene.props }


viewButton : Time -> String -> List Modifier -> Msg -> Html Msg
viewButton now txt modifiers msg =
    Html.button [ onClick msg, disabled False ] [ text txt ]


viewModifier : Modifier -> ( String, String )
viewModifier v =
    case v of
        Px attr ts start end current ->
            ( attr, px current )

        Static k v ->
            ( k, v )


viewModifiers : Time -> List Modifier -> List ( String, String )
viewModifiers now mods =
    List.map viewModifier mods


viewLabel : Time -> String -> List Modifier -> Msg -> Html Msg
viewLabel now txt mods msg =
    div
        [ style (viewModifiers now mods)
        ]
        [ text txt ]


{-| Render the scene to Html
-}
viewScene : Time -> Scene -> List (Html Msg)
viewScene now scene =
    let
        f ( k, prop ) =
            prop.view now prop.text prop.modifiers prop.message
    in
        List.map f (toList scene.props)
