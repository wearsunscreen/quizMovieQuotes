module Main exposing (main)

import Browser exposing (Document, document)
import Dict exposing (Dict, fromList, get, insert, toList, union)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (..)
import List exposing (length, map)
import List.Extra exposing (unique, zip)
import Maybe exposing (withDefault)
import Random exposing (Seed, initialSeed)
import String exposing (fromInt)
import Time exposing (Posix, every, millisToPosix, posixToMillis)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- ============================================================================
-- MODEL


type alias QuoteData =
    { rank : Int
    , year : Int
    , actor : String
    , character : String
    , movie : String
    , line : String
    }


type alias Model =
    { index : Int
    , hint1 : Bool
    , hint2 : Bool
    , answer : Bool
    , now : Posix
    , seed : Seed
    , scene : Scene
    }


propNames =
    [ "Rank", "Year", "Actor", "Role", "Movie", "Quote", "button" ]


init : () -> ( Model, Cmd msg )
init _ =
    ( nextQuote 1 emptyModel, Cmd.none )


initProps : Dict String Prop
initProps =
    Dict.fromList <|
        zip propNames <|
            List.map (initQuizLine 1) (List.range 1 7)
                ++ [ ds_button ( 10, 20 ) "HINT" Hint ]


emptyModel : Model
emptyModel =
    Model 1
        False
        False
        False
        (millisToPosix 0)
        (initialSeed 0)
        { props = initProps
        }


nextQuote : Int -> Model -> Model
nextQuote idx m =
    let
        q =
            getMovieQuoteData idx

        s =
            if m.seed == initialSeed 0 then
                initialSeed (posixToMillis m.now)

            else
                m.seed
    in
    { m
        | index = idx
        , hint1 = False
        , hint2 = False
        , answer = False
        , seed = s
        , scene =
            setTextOfProp "Title" "Guess the Movie Quote" m.scene
                |> setTextOfProp "Rank" ("#" ++ fromInt q.rank ++ " in the AFI list of greatest movie quotes. ")
                |> setTextOfProp "Year" ("Released in " ++ fromInt q.year)
                |> setTextOfProp "Actor" ("spoken by " ++ q.actor)
                |> shrinkLine "Role" " portraying - "
                |> shrinkLine "Movie" "in the movie - "
                |> shrinkLine "Quote" " ? "
    }


initQuizLine : Int -> Int -> Prop
initQuizLine dataIndex lineNumber =
    let
        topOfLine line =
            line * 50 + 30
    in
    addStatics
        [ ( "border-radius", "4px" )
        , ( "align-items", "center" )
        , ( "justify-content", "center" )
        , ( "width", "80" )
        , ( "font-size", "200%" )
        , ( "background-color"
          , if modBy 2 lineNumber == 0 then
                "Bisque"

            else
                "Khaki"
          )
        ]
        (label ( 100, topOfLine lineNumber ) "no text")


shrinkLine : String -> String -> Scene -> Scene
shrinkLine propName displayString scene =
    let
        prop =
            withDefault emptyProp (get propName scene.props)
                |> setText displayString
                |> addStatics [ ( "width", "200px" ), ( "font-size", "50%" ) ]
    in
    { scene | props = insert propName prop scene.props }


growLine : Posix -> String -> String -> Scene -> Scene
growLine t propName displayString scene =
    let
        prop =
            withDefault emptyProp (get propName scene.props)
                |> setText displayString
                |> addStatics [ ( "font-size", "200%" ) ]
                |> addModifier (Px "width" (timeSpan (posixToMillis t) 35000) 20 800 200)
    in
    { scene | props = insert propName prop scene.props }



-- ============================================================================
-- UPDATE


getMovieQuoteData : Int -> QuoteData
getMovieQuoteData n =
    elementAt n quotes
        |> withDefault { rank = 85, year = 2002, actor = "Andy Serkis", character = "Sméagol", movie = "The Lord of the Rings: The Two Towers", line = "My precious." }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        q =
            getMovieQuoteData model.index
    in
    case msg of
        Answer ->
            ( { model
                | answer = True
                , scene = growLine model.now "Quote" q.line model.scene
              }
            , Cmd.none
            )

        Hint ->
            if model.hint1 then
                ( { model
                    | hint2 = True
                    , scene = growLine model.now "Movie" ("in " ++ q.movie) model.scene
                  }
                , Cmd.none
                )

            else
                ( { model
                    | hint1 = True
                    , scene = growLine model.now "Role" ("portraying " ++ q.character) model.scene
                  }
                , Cmd.none
                )

        NoMsg ->
            ( model, Cmd.none )

        Next ->
            let
                next =
                    modBy (length quotes + 1) (model.index + 1)

                qq =
                    getMovieQuoteData next
            in
            ( nextQuote next model, Cmd.none )

        Tick t ->
            ( { model
                | now = t
                , scene = updateScene t model.scene
              }
            , Cmd.none
            )



-- ============================================================================
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- ============================================================================
-- VIEW


view : Model -> Document Msg
view model =
    { title = "Movie Quotes Quiz"
    , body = [ view_ model ]
    }


view_ : Model -> Html Msg
view_ model =
    div []
        ([ button [ onClick Hint, disabled model.hint2 ] [ text "Hint" ]
         , button [ onClick Answer, disabled (not model.hint2 || model.answer) ] [ text "Answer" ]
         , button [ onClick Next, disabled (not model.answer) ] [ text "Next" ]
         ]
            ++ viewScene model.now model.scene
        )


{-| icky, This should be at the app level
-}
type Msg
    = Answer
    | Hint
    | NoMsg
    | Next
    | Tick Posix


{-| A Prop is an animatable object
-}
type alias Prop =
    { message : Msg
    , modifiers : List Modifier
    , text : String
    , view : Posix -> String -> List Modifier -> Msg -> Html Msg
    }


{-| A Modifier is the modifier of a List Modifier, either static or changing

  - from a begining modifier to an end modifier over a period of time

-}
type Modifier
    = Px String TimeSpan Int Int Int
    | Static String String


{-| A TimeSpan is the starting time and duration of a change

  - of a List Modifier's Modifier.

-}
type alias TimeSpan =
    { start : Int
    , duration : Int
    }


{-| A Scene is a collection of named Props
-}
type alias Scene =
    { props : Dict String Prop
    }


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
        | modifiers = map (\( k, v ) -> Static k v) pairs ++ prop.modifiers
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
ds_button : ( Int, Int ) -> String -> Msg -> Prop
ds_button ( left, top ) txt msg =
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
    fromInt number ++ "px"


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
            get propName scene.props |> withDefault (label ( 10, 10 ) ("Prop " ++ propName ++ " not found!"))

        props_ =
            insert propName { prop | text = txt } scene.props
    in
    { scene | props = props_ }


timeSpan : Int -> Int -> TimeSpan
timeSpan start duration =
    { start = start
    , duration = duration
    }


updateMod : Posix -> Modifier -> Modifier
updateMod now v =
    case v of
        Px attr ts start end current ->
            let
                ratio =
                    min 1.0 (toFloat (posixToMillis now - ts.start) / toFloat ts.duration)

                fStart =
                    toFloat start

                fEnd =
                    toFloat end

                modifier =
                    Debug.log (px current) (Px attr ts start end (truncate (fStart + (fEnd - fStart) * ratio)))
            in
            modifier

        _ ->
            v


updateProp : Posix -> comparable -> Prop -> Prop
updateProp now k prop =
    { prop | modifiers = List.map (updateMod now) prop.modifiers }


updateScene : Posix -> Scene -> Scene
updateScene now scene =
    { scene | props = Dict.map (updateProp now) scene.props }


viewButton : Posix -> String -> List Modifier -> Msg -> Html Msg
viewButton now txt modifiers msg =
    Html.button [ onClick msg, disabled False ] [ text txt ]


viewModifier : Modifier -> Html.Attribute Msg
viewModifier v =
    case v of
        Px attr ts start end current ->
            style attr <| px current

        Static k vv ->
            style k vv


viewModifiers : Posix -> List Modifier -> List (Html.Attribute Msg)
viewModifiers now mods =
    List.map viewModifier mods


viewLabel : Posix -> String -> List Modifier -> Msg -> Html Msg
viewLabel now txt mods msg =
    div
        (viewModifiers now mods)
        [ text txt ]


{-| Render the scene to Html
-}
viewScene : Posix -> Scene -> List (Html Msg)
viewScene now scene =
    let
        f ( k, prop ) =
            prop.view now prop.text prop.modifiers prop.message
    in
    List.map f (toList scene.props)



-- ============================================================================
-- DATA


elementAt : Int -> List a -> Maybe a
elementAt n xs =
    List.head (List.drop (n - 1) xs)


quotes : List QuoteData
quotes =
    [ { rank = 1, year = 1939, actor = "Clark Gable", character = "Rhett Butler", movie = "Gone with the Wind", line = "Frankly, my dear, I don't give a damn." }
    , { rank = 2, year = 1972, actor = "Marlon Brando", character = "Don Vito Corleone", movie = "The Godfather", line = "I'm going to make him an offer he can't refuse." }
    , { rank = 3, year = 1954, actor = "Marlon Brando", character = "Terry Malloy", movie = "On the Waterfront", line = "You don't understand! I coulda had class. I coulda been a contender. I could've been somebody, instead of a bum, which is what I am." }
    , { rank = 4, year = 1939, actor = "Judy Garland", character = "Dorothy Gale", movie = "The Wizard of Oz", line = "Toto, I've a feeling we're not in Kansas anymore." }
    , { rank = 5, year = 1942, actor = "Humphrey Bogart", character = "Rick Blaine", movie = "Casablanca", line = "Here's looking at you, kid." }
    , { rank = 6, year = 1983, actor = "Clint Eastwood", character = "Harry Callahan", movie = "Sudden Impact", line = "Go ahead, make my day." }
    , { rank = 7, year = 1950, actor = "Gloria Swanson", character = "Norma Desmond", movie = "Sunset Boulevard", line = "All right, Mr. DeMille, I'm ready for my close-up." }
    , { rank = 8, year = 1977, actor = "Several actors", character = "Several characters", movie = "Star Wars, et al", line = "May the Force be with you." }
    , { rank = 9, year = 1950, actor = "Bette Davis", character = "Margo Channing", movie = "All About Eve", line = "Fasten your seatbelts. It's going to be a bumpy night." }
    , { rank = 10, year = 1976, actor = "Robert De Niro", character = "Travis Bickle", movie = "Taxi Driver", line = "You talkin' to me?" }
    , { rank = 11, year = 1967, actor = "Strother Martin", character = "Captain", movie = "Cool Hand Luke", line = "What we've got here is failure to communicate." }
    , { rank = 12, year = 1979, actor = "Robert Duvall", character = "Lt. Col. Bill Kilgore", movie = "Apocalypse Now", line = "I love the smell of napalm in the morning." }
    , { rank = 13, year = 1970, actor = "Ali MacGraw", character = "Jennifer Cavilleri Barrett", movie = "Love Story", line = "Love means never having to say you're sorry." }
    , { rank = 14, year = 1941, actor = "Humphrey Bogart", character = "Sam Spade", movie = "The Maltese Falcon", line = "The stuff that dreams are made of." }
    , { rank = 15, year = 1982, actor = "Pat Welsh", character = "E.T.", movie = "E.T. the Extra-Terrestrial", line = "E.T. phone home." }
    , { rank = 16, year = 1967, actor = "Sidney Poitier", character = "Virgil Tibbs", movie = "In the Heat of the Night", line = "They call me Mister Tibbs!" }
    , { rank = 17, year = 1941, actor = "Orson Welles", character = "Charles Foster Kane", movie = "Citizen Kane", line = "Rosebud." }
    , { rank = 18, year = 1949, actor = "James Cagney", character = "Arthur 'Cody' Jarrett", movie = "White Heat", line = "Made it, Ma! Top of the world!" }
    , { rank = 19, year = 1976, actor = "Peter Finch", character = "Howard Beale", movie = "Network", line = "I'm as mad as hell, and I'm not going to take this anymore!" }
    , { rank = 20, year = 1942, actor = "Humphrey Bogart", character = "Rick Blaine", movie = "Casablanca", line = "Louis, I think this is the beginning of a beautiful friendship." }
    , { rank = 21, year = 1991, actor = "Anthony Hopkins", character = "Hannibal Lecter", movie = "The Silence of the Lambs", line = "A census taker once tried to test me. I ate his liver with some fava beans and a nice Chianti." }
    , { rank = 22, year = 1962, actor = "Sean Connery (and six other actors)", character = "James Bond", movie = "Dr. No (and 19 other movies)", line = "Bond. James Bond." }
    , { rank = 23, year = 1939, actor = "Judy Garland", character = "Dorothy Gale", movie = "The Wizard of Oz", line = "There's no place like home." }
    , { rank = 24, year = 1950, actor = "Gloria Swanson", character = "Norma Desmond", movie = "Sunset Boulevard", line = "I am big! It's the pictures that got small." }
    , { rank = 25, year = 1996, actor = "Cuba Gooding, Jr. and Tom Cruise", character = "Rod Tidwell and Jerry Maguire", movie = "Jerry Maguire", line = "Show me the money!" }
    , { rank = 26, year = 1933, actor = "Mae West", character = "Lady Lou", movie = "She Done Him Wrong", line = "Why don't you come up sometime and see me?" }
    , { rank = 27, year = 1969, actor = "Dustin Hoffman", character = "Ratso Rizzo", movie = "Midnight Cowboy", line = "I'm walking here! I'm walking here!" }
    , { rank = 28, year = 1942, actor = "Ingrid Bergman", character = "Ilsa Lund", movie = "Casablanca", line = "Play it, Sam. Play 'As Time Goes By.'" }
    , { rank = 29, year = 1992, actor = "Jack Nicholson", character = "Col. Nathan R. Jessep", movie = "A Few Good Men", line = "You can't handle the truth!" }
    , { rank = 30, year = 1932, actor = "Greta Garbo", character = "Grusinskaya", movie = "Grand Hotel", line = "I want to be alone." }
    , { rank = 31, year = 1939, actor = "Vivien Leigh", character = "Scarlett O'Hara", movie = "Gone with the Wind", line = "After all, tomorrow is another day!" }
    , { rank = 32, year = 1942, actor = "Claude Rains", character = "Capt. Louis Renault", movie = "Casablanca", line = "Round up the usual suspects." }
    , { rank = 33, year = 1989, actor = "Estelle Reiner", character = "Customer", movie = "When Harry Met Sally...", line = "I'll have what she's having." }
    , { rank = 34, year = 1944, actor = "Lauren Bacall", character = "Marie 'Slim' Browning", movie = "To Have and Have Not", line = "You know how to whistle, don't you, Steve? You just put your lips together and blow." }
    , { rank = 35, year = 1975, actor = "Roy Scheider", character = "Martin Brody", movie = "Jaws", line = "You're gonna need a bigger boat." }
    , { rank = 36, year = 1948, actor = "Alfonso Bedoya", character = "Gold Hat", movie = "The Treasure of the Sierra Madre", line = "Badges? We ain't got no badges! We don't need no badges! I don't have to show you any stinking badges!" }
    , { rank = 37, year = 1984, actor = "Arnold Schwarzenegger", character = "The Terminator", movie = "The Terminator", line = "I'll be back." }
    , { rank = 38, year = 1942, actor = "Gary Cooper", character = "Lou Gehrig", movie = "The Pride of the Yankees", line = "Today, I consider myself the luckiest man on the face of the earth." }
    , { rank = 39, year = 1989, actor = "Ray Liotta (voice)", character = "Shoeless Joe Jackson", movie = "Field of Dreams", line = "If you build it, he will come." }
    , { rank = 40, year = 1994, actor = "Tom Hanks", character = "Forrest Gump", movie = "Forrest Gump", line = "Mama always said life was like a box of chocolates. You never know what you're gonna get." }
    , { rank = 41, year = 1967, actor = "Warren Beatty", character = "Clyde Barrow", movie = "Bonnie and Clyde", line = "We rob banks." }
    , { rank = 42, year = 1967, actor = "Walter Brooke", character = "Mr. Maguire", movie = "The Graduate", line = "Plastics." }
    , { rank = 43, year = 1942, actor = "Humphrey Bogart", character = "Rick Blaine", movie = "Casablanca", line = "We'll always have Paris." }
    , { rank = 44, year = 1999, actor = "Haley Joel Osment", character = "Cole Sear", movie = "The Sixth Sense", line = "I see dead people." }
    , { rank = 45, year = 1951, actor = "Marlon Brando", character = "Stanley Kowalski", movie = "A Streetcar Named Desire", line = "Stella! Hey, Stella!" }
    , { rank = 46, year = 1942, actor = "Bette Davis", character = "Charlotte Vale", movie = "Now, Voyager", line = "Oh, Jerry, don't let's ask for the moon. We have the stars." }
    , { rank = 47, year = 1953, actor = "Brandon De Wilde", character = "Joey Starrett", movie = "Shane", line = "Shane. Shane. Come back!" }
    , { rank = 48, year = 1959, actor = "Joe E. Brown", character = "Osgood Fielding III", movie = "Some Like It Hot", line = "Well, nobody's perfect." }
    , { rank = 49, year = 1931, actor = "Colin Clive", character = "Henry Frankenstein", movie = "Frankenstein", line = "It's alive! It's alive!" }
    , { rank = 50, year = 1995, actor = "Tom Hanks", character = "Jim Lovell", movie = "Apollo 13", line = "Houston, we have a problem." }
    , { rank = 51, year = 1971, actor = "Clint Eastwood", character = "Harry Callahan", movie = "Dirty Harry", line = "You've got to ask yourself one question: 'Do I feel lucky?' Well, do ya, punk?" }
    , { rank = 52, year = 1996, actor = "Renée Zellweger", character = "Dorothy Boyd", movie = "Jerry Maguire", line = "You had me at 'hello.'" }
    , { rank = 53, year = 1930, actor = "Groucho Marx", character = "Capt. Geoffrey T. Spaulding", movie = "Animal Crackers", line = "One morning I shot an elephant in my pajamas. How he got in my pajamas, I don't know." }
    , { rank = 54, year = 1992, actor = "Tom Hanks", character = "Jimmy Dugan", movie = "A League of Their Own", line = "There's no crying in baseball!" }
    , { rank = 55, year = 1977, actor = "Diane Keaton", character = "Annie Hall", movie = "Annie Hall", line = "La-dee-da, la-dee-da." }
    , { rank = 56, year = 1960, actor = "Anthony Perkins", character = "Norman Bates", movie = "Psycho", line = "A boy's best friend is his mother." }
    , { rank = 57, year = 1987, actor = "Michael Douglas", character = "Gordon Gekko", movie = "Wall Street", line = "Greed, for lack of a better word, is good." }
    , { rank = 58, year = 1974, actor = "Al Pacino", character = "Michael Corleone", movie = "The Godfather Part II", line = "Keep your friends close, but your enemies closer." }
    , { rank = 59, year = 1939, actor = "Vivien Leigh", character = "Scarlett O'Hara", movie = "Gone with the Wind", line = "As God is my witness, I'll never be hungry again." }
    , { rank = 60, year = 1933, actor = "Oliver Hardy", character = "Oliver", movie = "Sons of the Desert", line = "Well, here's another nice mess you've gotten me into!" }
    , { rank = 61, year = 1983, actor = "Al Pacino", character = "Tony Montana", movie = "Scarface", line = "Say 'hello' to my little friend!" }
    , { rank = 62, year = 1949, actor = "Bette Davis", character = "Rosa Moline", movie = "Beyond the Forest", line = "What a dump." }
    , { rank = 63, year = 1967, actor = "Dustin Hoffman", character = "Benjamin Braddock", movie = "The Graduate", line = "Mrs. Robinson, you're trying to seduce me. Aren't you?" }
    , { rank = 64, year = 1964, actor = "Peter Sellers", character = "President Merkin Muffley", movie = "Dr. Strangelove", line = "Gentlemen, you can't fight in here! This is the War Room!" }
    , { rank = 65, year = 1939, actor = "Basil Rathbone", character = "Sherlock Holmes", movie = "The Adventures of Sherlock Holmes", line = "Elementary, my dear Watson." }
    , { rank = 66, year = 1968, actor = "Charlton Heston", character = "George Taylor", movie = "Planet of the Apes", line = "Take your stinking paws off me, you damn dirty ape!" }
    , { rank = 67, year = 1942, actor = "Humphrey Bogart", character = "Rick Blaine", movie = "Casablanca", line = "Of all the gin joints in all the towns in all the world, she walks into mine." }
    , { rank = 68, year = 1980, actor = "Jack Nicholson", character = "Jack Torrance", movie = "The Shining", line = "Here's Johnny!" }
    , { rank = 69, year = 1982, actor = "Heather O'Rourke", character = "Carol Anne Freeling", movie = "Poltergeist", line = "They're here!" }
    , { rank = 70, year = 1976, actor = "Laurence Olivier", character = "Dr. Christian Szell", movie = "Marathon Man", line = "Is it safe?" }
    , { rank = 71, year = 1927, actor = "Al Jolson", character = "Jakie Rabinowitz/Jack Robin", movie = "The Jazz Singer", line = "Wait a minute, wait a minute. You ain't heard nothin' yet!" }
    , { rank = 72, year = 1981, actor = "Faye Dunaway", character = "Joan Crawford", movie = "Mommie Dearest", line = "No wire hangers, ever!" }
    , { rank = 73, year = 1930, actor = "Edward G. Robinson", character = "Cesare Enrico 'Rico' Bandello", movie = "Little Caesar", line = "Mother of mercy, is this the end of Rico?" }
    , { rank = 74, year = 1974, actor = "Joe Mantell", character = "Lawrence Walsh", movie = "Chinatown", line = "Forget it, Jake, it's Chinatown." }
    , { rank = 75, year = 1951, actor = "Vivien Leigh", character = "Blanche DuBois", movie = "A Streetcar Named Desire", line = "I have always depended on the kindness of strangers." }
    , { rank = 76, year = 1991, actor = "Arnold Schwarzenegger", character = "The Terminator", movie = "Terminator 2: Judgment Day", line = "Hasta la vista, baby." }
    , { rank = 77, year = 1973, actor = "Charlton Heston", character = "Det. Robert Thorn", movie = "Soylent Green", line = "Soylent Green is people!" }
    , { rank = 78, year = 1968, actor = "Keir Dullea", character = "Dave Bowman", movie = "2001: A Space Odyssey", line = "Open the pod bay doors, HAL." }
    , { rank = 79, year = 1980, actor = "Robert Hays and Leslie Nielsen", character = "Ted Striker and Dr. Rumack", movie = "Airplane!", line = "Striker: 'Surely you can't be serious.' Rumack: 'I am serious...and don't call me Shirley.'" }
    , { rank = 80, year = 1976, actor = "Sylvester Stallone", character = "Rocky Balboa", movie = "Rocky", line = "Yo, Adrian!" }
    , { rank = 81, year = 1968, actor = "Barbra Streisand", character = "Fanny Brice", movie = "Funny Girl", line = "Hello, gorgeous." }
    , { rank = 82, year = 1978, actor = "John Belushi", character = "John 'Bluto' Blutarsky", movie = "National Lampoon's Animal House", line = "Toga! Toga!" }
    , { rank = 83, year = 1931, actor = "Bela Lugosi", character = "Count Dracula", movie = "Dracula", line = "Listen to them. Children of the night. What music they make." }
    , { rank = 84, year = 1933, actor = "Robert Armstrong", character = "Carl Denham", movie = "King Kong", line = "Oh, no, it wasn't the airplanes. It was Beauty killed the Beast." }
    , { rank = 85, year = 2002, actor = "Andy Serkis", character = "Sméagol", movie = "The Lord of the Rings: The Two Towers", line = "My precious." }
    , { rank = 86, year = 1975, actor = "Al Pacino", character = "Sonny Wortzik", movie = "Dog Day Afternoon", line = "Attica! Attica!" }
    , { rank = 87, year = 1933, actor = "Warner Baxter", character = "Julian Marsh", movie = "42nd Street", line = "Sawyer, you're going out a youngster, but you've got to come back a star!" }
    , { rank = 88, year = 1981, actor = "Katharine Hepburn", character = "Ethel Thayer", movie = "On Golden Pond", line = "Listen to me, mister. You're my knight in shining armor. Don't you forget it. You're going to get back on that horse, and I'm going to be right behind you, holding on tight, and away we're gonna go, go, go!" }
    , { rank = 89, year = 1940, actor = "Pat O'Brien", character = "Knute Rockne", movie = "Knute Rockne, All American", line = "Tell 'em to go out there with all they got and win just one for the Gipper." }
    , { rank = 90, year = 1964, actor = "Sean Connery", character = "James Bond", movie = "Goldfinger", line = "A martini. Shaken, not stirred." }
    , { rank = 91, year = 1945, actor = "Bud Abbott", character = "Dexter", movie = "The Naughty Nineties", line = "Who's on first." }
    , { rank = 92, year = 1980, actor = "Bill Murray", character = "Carl Spackler", movie = "Caddyshack", line = "Cinderella story. Outta nowhere. A former greenskeeper, now, about to become the Masters champion. It looks like a mirac...It's in the hole! It's in the hole! It's in the hole!" }
    , { rank = 93, year = 1958, actor = "Rosalind Russell", character = "Mame Dennis", movie = "Auntie Mame", line = "Life is a banquet, and most poor suckers are starving to death!" }
    , { rank = 94, year = 1986, actor = "Tom Cruise and Anthony Edwards", character = "Lt. Pete 'Maverick' Mitchell and Lt. Nick 'Goose' Bradshaw", movie = "Top Gun", line = "I feel the need—the need for speed!" }
    , { rank = 95, year = 1989, actor = "Robin Williams", character = "John Keating", movie = "Dead Poets Society", line = "Carpe diem. Seize the day, boys. Make your lives extraordinary." }
    , { rank = 96, year = 1987, actor = "Cher", character = "Loretta Castorini", movie = "Moonstruck", line = "Snap out of it!" }
    , { rank = 97, year = 1942, actor = "James Cagney", character = "George M. Cohan", movie = "Yankee Doodle Dandy", line = "My mother thanks you. My father thanks you. My sister thanks you. And I thank you." }
    , { rank = 98, year = 1987, actor = "Patrick Swayze", character = "Johnny Castle", movie = "Dirty Dancing", line = "Nobody puts Baby in a corner." }
    , { rank = 99, year = 1939, actor = "Margaret Hamilton", character = "Wicked Witch of the West", movie = "The Wizard of Oz", line = "I'll get you, my pretty, and your little dog too!" }
    , { rank = 100, year = 1997, actor = "Leonardo DiCaprio", character = "Jack Dawson", movie = "Titanic", line = "I'm king of the world!" }
    ]
