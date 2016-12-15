module Main exposing (..)

import Dict exposing (Dict, get, insert)
import DuckSoup
    exposing
        ( Msg(Answer, Hint, NoMsg, Next, Tick)
        , Prop
        , Scene
        , TimeSpan
        , Modifier(Px, Static)
        , addModifier
        , addStatics
        , emptyProp
        , label
        , px
        , setText
        , setTextOfProp
        , timeSpan
        , viewScene
        , updateScene
        )
import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (..)
import List exposing (length)
import List.Extra exposing (unique, zip)
import Maybe
import Random exposing (Seed, initialSeed)
import Time exposing (Time, every, second)


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- ============================================================================
-- MODEL


type alias QuoteData =
    ( Int, Int, String, String, String, String )


type alias Model =
    { index : Int
    , hint1 : Bool
    , hint2 : Bool
    , answer : Bool
    , now : Time
    , seed : Seed
    , scene : Scene
    }


propNames =
    [ "Title", "Rank", "Year", "Actor", "Role", "Movie", "Quote", "button" ]


init : ( Model, Cmd msg )
init =
    ( nextQuote 1 emptyModel, Cmd.none )


initProps : Dict String Prop
initProps =
    Dict.fromList
        <| zip propNames
        <| (List.map (initQuizLine 1) [1..7])
        ++ [ DuckSoup.button ( 10, 20 ) "HINT" Hint ]


emptyModel : Model
emptyModel =
    Model 1
        False
        False
        False
        0
        (initialSeed 0)
        { props = initProps
        }


nextQuote : Int -> Model -> Model
nextQuote idx m =
    let
        ( rank, year, actor, role, movie, quote ) =
            getMovieQuoteData idx

        s =
            if m.seed == initialSeed 0 then
                initialSeed (truncate m.now)
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
                    |> setTextOfProp "Rank" ("#" ++ (toString rank) ++ " in the AFI list of greatest movie quotes. ")
                    |> setTextOfProp "Year" ("Released in " ++ (toString year))
                    |> setTextOfProp "Actor" ("spoken by " ++ actor)
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
            [ "border-radius" => "4px"
            , "align-items" => "center"
            , "justify-content" => "center"
            , "width" => "80"
            , "font-size" => "200%"
            , "background-color"
                => if lineNumber % 2 == 0 then
                    "Bisque"
                   else
                    "Khaki"
            ]
            (label ( 100, (topOfLine lineNumber) ) "no text")


shrinkLine : String -> String -> Scene -> Scene
shrinkLine propName displayString scene =
    let
        prop =
            (get propName scene.props ?? emptyProp)
                |> setText displayString
                |> addStatics [ ( "width", "200px" ), ( "font-size", "50%" ) ]
    in
        { scene | props = insert propName prop scene.props }


growLine : Time -> String -> String -> Scene -> Scene
growLine t propName displayString scene =
    let
        prop =
            (get propName scene.props ?? emptyProp)
                |> setText displayString
                |> addStatics [ ( "font-size", "200%" ) ]
                |> addModifier (Px "width" (timeSpan t 35000) 20 800 200)
    in
        { scene | props = insert propName prop scene.props }


(=>) x y =
    ( x, y )



-- ============================================================================
-- UPDATE


getMovieQuoteData : Int -> ( Int, Int, String, String, String, String )
getMovieQuoteData n =
    (elementAt n quotes)
        ?? ( 85, 2002, " Andy Serkis ", " Gollum ", " The Lord of the Rings: The Two Towers ", "My precious." )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( rank, year, actor, role, movie, quote ) =
            getMovieQuoteData model.index
    in
        case msg of
            Answer ->
                ( { model
                    | answer = True
                    , scene = growLine model.now "Quote" quote model.scene
                  }
                , Cmd.none
                )

            Hint ->
                if model.hint1 then
                    ( { model
                        | hint2 = True
                        , scene = growLine model.now "Movie" ("in " ++ movie) model.scene
                      }
                    , Cmd.none
                    )
                else
                    ( { model
                        | hint1 = True
                        , scene = growLine model.now "Role" ("portraying " ++ role) model.scene
                      }
                    , Cmd.none
                    )

            NoMsg ->
                ( model, Cmd.none )

            Next ->
                let
                    next =
                        ((model.index + 1) % ((length quotes) + 1))

                    ( rank, year, actor, role, movie, quote ) =
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
    Time.every (second) Tick



-- ============================================================================
-- VIEW


view : Model -> Html Msg
view model =
    div []
        ([ button [ onClick Hint, disabled (model.hint2) ] [ text "Hint" ]
         , button [ onClick Answer, disabled ((not model.hint2) || model.answer) ] [ text "Answer" ]
         , button [ onClick Next, disabled (not model.answer) ] [ text "Next" ]
         ]
            ++ viewScene model.now model.scene
        )


(??) : Maybe a -> a -> a
(??) maybe default =
    case maybe of
        Nothing ->
            default

        Just x ->
            x



-- ============================================================================
-- DATA


elementAt : Int -> List a -> Maybe a
elementAt n xs =
    List.head (List.drop (n - 1) xs)


quotes : List QuoteData
quotes =
    [ ( 85, 2002, " Andy Serkis ", " Gollum ", " The Lord of the Rings: The Two Towers ", "My precious." )
    , ( 44, 1999, " Haley Joel Osment ", " Cole Sear ", " The Sixth Sense ", "I see dead people." )
    , ( 100, 1997, " Leonardo DiCaprio ", " Jack Dawson ", " Titanic ", "I'm the king of the world!" )
    , ( 25, 1996, " Cuba Gooding, Jr. ", " Rod Tidwell ", " Jerry Maguire ", "Show me the money!" )
    , ( 52, 1996, " Renée Zellweger ", " Dorothy Boyd ", " Jerry Maguire ", "You had me at 'hello.'" )
    , ( 50, 1995, " Tom Hanks ", " Jim Lovell ", " Apollo 13 ", "Houston, we have a problem." )
    , ( 40, 1994, " Tom Hanks ", " Forrest Gump ", " Forrest Gump ", "Mama always said life was like a box of chocolates. You never know what you're gonna get." )
    , ( 29, 1992, " Jack Nicholson ", " Col. Nathan R. Jessup ", " A Few Good Men ", "You can't handle the truth!" )
    , ( 54, 1992, " Tom Hanks ", " Jimmy Dugan ", " A League of Their Own ", "There's no crying in baseball!" )
    , ( 21, 1991, " Anthony Hopkins ", " Dr. Hannibal Lecter ", " The Silence of the Lambs ", "A census taker once tried to test me. I ate his liver with some fava beans and a nice Chianti." )
    , ( 76, 1991, " Arnold Schwarzenegger ", " The Terminator ", " Terminator 2: Judgment Day ", "Hasta la vista, baby." )
    , ( 33, 1989, " Estelle Reiner ", " Customer ", " When Harry Met Sally... ", "I'll have what she's having." )
    , ( 39, 1989, " Ray Liotta ", " Shoeless Joe Jackson ", " Field of Dreams ", "If you build it, he will come." )
    , ( 95, 1989, " Robin Williams ", " John Keating ", " Dead Poets Society ", "Carpe diem. Seize the day, boys. Make your lives extraordinary." )
    , ( 57, 1987, " Michael Douglas ", " Gordon Gekko ", " Wall Street ", "Greed, for lack of a better word, is good." )
    , ( 96, 1987, " Cher ", " Loretta Castorini ", " Moonstruck ", "Snap out of it!" )
    , ( 98, 1987, " Patrick Swayze ", " Johnny Castle ", " Dirty Dancing ", "Nobody puts Baby in a corner." )
    , ( 94, 1986, " Tom Cruise and Anthony Edwards ", " Lt. Pete \"Maverick\" Mitchell and Lt.jg Nick \"Goose\" Bradshaw ", " Top Gun ", "I feel the need—the need for speed!" )
    , ( 37, 1984, " Arnold Schwarzenegger ", " The Terminator ", " The Terminator ", "I'll be back." )
    , ( 6, 1983, " Clint Eastwood ", " Harry Callahan ", " Sudden Impact ", "Go ahead, make my day." )
    , ( 61, 1983, " Al Pacino ", " Tony Montana ", " Scarface ", "Say 'hello' to my little friend!" )
    , ( 15, 1982, " Pat Welsh ", " E.T. ", " E.T. the Extra-Terrestrial ", "E.T. phone home." )
    , ( 69, 1982, " Heather O'Rourke ", " Carol Anne Freeling ", " Poltergeist ", "They're here!" )
    , ( 72, 1981, " Faye Dunaway ", " Joan Crawford ", " Mommie Dearest ", "No wire hangers, ever!" )
    , ( 88, 1981, " Katharine Hepburn ", " Ethel Thayer ", " On Golden Pond ", "Listen to me, mister. You're my knight in shining armor. Don't you forget it. You're going to get back on that horse, and I'm going to be right behind you, holding on tight, and away we're gonna go, go, go!" )
    , ( 68, 1980, " Jack Nicholson ", " Jack Torrance ", " The Shining ", "Here's Johnny!" )
    , ( 79, 1980, " Robert Haysand Leslie Nielsen ", " Ted Striker and Dr. Rumack ", " Airplane! ", "Striker: \"Surely you can't be serious.\" Rumack: \"I am serious...and don't call me Shirley." )
    , ( 92, 1980, " Bill Murray ", " Carl Spackler ", " Caddyshack ", "Cinderella story. Outta nowhere. A former greenskeeper, now, about to become the Masters champion. It looks like a mirac...It's in the hole! It's in the hole! It's in the hole!" )
    , ( 12, 1979, " Robert Duvall ", " Lt. Col. Bill Kilgore ", " Apocalypse Now ", "I love the smell of napalm in the morning. It smells like ... victory" )
    , ( 82, 1978, " John Belushi ", " John \"Bluto\" Blutarsky ", " National Lampoon's Animal House ", "Toga! Toga!" )
    , ( 8, 1977, " Harrison Ford ", " Han Solo ", " Star Wars ", "May the Force be with you." )
    , ( 55, 1977, " Diane Keaton ", " Annie Hall ", " Annie Hall ", "La-dee-da, la-dee-da." )
    , ( 10, 1976, " Robert De Niro ", " Travis Bickle ", " Taxi Driver ", "You talkin' to me?" )
    , ( 19, 1976, " Peter Finch ", " Howard Beale ", " Network ", "I'm as mad as hell, and I'm not going to take this anymore!" )
    , ( 70, 1976, " Laurence Olivier ", " Dr. Christian Szell ", " Marathon Man ", "Is it safe?" )
    , ( 80, 1976, " Sylvester Stallone ", " Rocky Balboa ", " Rocky ", "Yo, Adrian!" )
    , ( 35, 1975, " Roy Scheider ", " Martin Brody ", " Jaws ", "You're gonna need a bigger boat." )
    , ( 86, 1975, " Al Pacino ", " Sonny Wortzik ", " Dog Day Afternoon ", "Attica! Attica!" )
    , ( 58, 1974, " Al Pacino ", " Michael Corleone ", " The Godfather Part II ", "Keep your friends close, but your enemies closer." )
    , ( 74, 1974, " Joe Mantell ", " Lawrence Walsh ", " Chinatown ", "Forget it, Jake, it's Chinatown." )
    , ( 77, 1973, " Charlton Heston ", " Det. Robert Thorn ", " Soylent Green ", "Soylent Green is people!" )
    , ( 2, 1972, " Marlon Brando ", " Vito Corleone ", " The Godfather ", "I'm gonna make him an offer he can't refuse." )
    , ( 51, 1971, " Clint Eastwood ", " Harry Callahan ", " Dirty Harry ", "You've got to ask yourself one question: 'Do I feel lucky?' Well, do ya, punk?" )
    , ( 13, 1970, " Ali MacGraw ", " Jennifer Cavilleri Barrett ", " Love Story ", "Love means never having to say you're sorry." )
    , ( 27, 1969, " Dustin Hoffman ", " \"Ratso\" Rizzo ", " Midnight Cowboy ", "I'm walking here! I'm walking here!" )
    , ( 66, 1968, " Charlton Heston ", " George Taylor ", " Planet of the Apes ", "Get your stinking paws off me, you damned dirty ape." )
    , ( 78, 1968, " Keir Dullea ", " Dave Bowman ", " 2001: A Space Odyssey ", "Open the pod bay doors, HAL." )
    , ( 81, 1968, " Barbra Streisand ", " Fanny Brice ", " Funny Girl ", "Hello, gorgeous." )
    , ( 11, 1967, " Strother Martin ", " Captain ", " Cool Hand Luke ", "What we've got here is failure to communicate." )
    , ( 16, 1967, " Sidney Poitier ", " Virgil Tibbs ", " In the Heat of the Night ", "They call me Mister Tibbs!" )
    , ( 41, 1967, " Warren Beatty ", " Clyde Barrow ", " Bonnie and Clyde ", "We rob banks." )
    , ( 42, 1967, " Walter Brooke ", " Mr. Maguire ", " The Graduate ", "Plastics." )
    , ( 63, 1967, " Dustin Hoffman ", " Benjamin Braddock ", " The Graduate ", "Mrs. Robinson, you're trying to seduce me. Aren't you?" )
    , ( 64, 1964, " Peter Sellers ", " President Merkin Muffley ", " Dr. Strangelove or: How I Learned to Stop Worrying and Love the Bomb ", "Gentlemen, you can't fight in here! This is the War Room!" )
    , ( 90, 1964, " Sean Connery", " James Bond ", " Goldfinger", "A martini. Shaken, not stirred." )
    , ( 22, 1962, " Sean Connery", " James Bond ", " Dr. No", "Bond. James Bond." )
    , ( 56, 1960, " Anthony Perkins ", " Norman Bates ", " Psycho ", "A boy's best friend is his mother." )
    , ( 48, 1959, " Joe E. Brown ", " Osgood Fielding III ", " Some Like It Hot ", "Well, nobody's perfect." )
    , ( 93, 1958, " Rosalind Russell ", " Mame Dennis ", "Auntie Mame ", "Life is a banquet, and most poor suckers are starving to death!" )
    , ( 3, 1954, " Marlon Brando ", " Terry Malloy ", " On the Waterfront ", "You don't understand! I coulda had class. I coulda been a     contender. I could've been somebody, instead of a bum, which is what I am." )
    , ( 47, 1953, " Brandon De Wilde ", " Joey Starrett ", " Shane ", "Shane. Shane. Come back!" )
    , ( 45, 1951, " Marlon Brando ", " Stanley Kowalski ", " A Streetcar Named Desire ", "Stella! Hey, Stella!" )
    , ( 75, 1951, " Vivien Leigh ", " Blanche DuBois ", " A Streetcar Named Desire ", "I have always depended on the kindness of strangers." )
    , ( 7, 1950, " Gloria Swanson ", " Norma Desmond ", " Sunset Boulevard ", "All right, Mr. DeMille, I'm ready for my close-up." )
    , ( 9, 1950, " Bette Davis ", " Margo Channing ", " All About Eve ", "Fasten your seatbelts. It's going to be a bumpy night." )
    , ( 24, 1950, " Gloria Swanson ", " Norma Desmond ", " Sunset Boulevard ", "I am big! It's the pictures that got small." )
    , ( 18, 1949, " James Cagney ", " Arthur \"Cody\" Jarrett ", " White Heat ", "Made it, Ma! Top of the world!" )
    , ( 62, 1949, " Bette Davis ", " Rosa Moline ", " Beyond the Forest ", "What a dump." )
    , ( 36, 1948, " Alfonso Bedoya ", " \"Gold Hat\" ", " The Treasure of the Sierra Madre ", "Badges? We ain't got no badges! We don't need no badges! I don't have to show you any stinking badges!" )
    , ( 91, 1945, " Bud Abbott ", " Dexter ", " The Naughty Nineties ", "Who's on first?" )
    , ( 34, 1944, " Lauren Bacall ", " Marie \"Slim\" Browning ", " To Have and Have Not ", "You know how to whistle, don't you, Steve? You just put your lips together and blow." )
    , ( 5
      , 1942
      , " Humphrey Bogart "
      , " Rick Blaine "
      , " Casablanca "
      , """"
        "Here's looking at you, kid."
        and (#20) "Louis, I think this is the beginning of a beautiful friendship."
        and (#43) "We'll always have Paris."
        and (#67) "Of all the gin joints in all the towns in all the world, she walks into mine."

        """
      )
    , ( 20, 1942, "Humphrey Bogart", "Rick Blaine", "Casablanca ", "" )
    , ( 28, 1942, " Ingrid Bergman ", " Ilsa Lund ", " Casablanca ", "Play it, Sam. Play 'As Time Goes By.'" )
    , ( 32, 1942, " Claude Rains ", " Capt. Louis Renault ", " Casablanca ", "Round up the usual suspects." )
    , ( 46, 1942, " Bette Davis ", " Charlotte Vale ", " Now, Voyager ", "Oh, Jerry, don't let's ask for the moon. We have the stars." )
    , ( 38, 1942, " Gary Cooper ", " Lou Gehrig ", " The Pride of the Yankees ", "Today, I consider myself the luckiest man on the face of the Earth." )
    , ( 97, 1942, " James Cagney ", " George M. Cohan ", " Yankee Doodle Dandy ", "My mother thanks you. My father thanks you. My sister thanks you. And I thank you." )
    , ( 14, 1941, " Humphrey Bogart ", " Sam Spade ", " The Maltese Falcon ", "The stuff that dreams are made of." )
    , ( 17, 1941, " Orson Welles ", " Charles Foster Kane ", " Citizen Kane ", "Rosebud." )
    , ( 89, 1940, " Pat O'Brien ", " Knute Rockne", " Knute Rockne, All American ", "Tell 'em to go out there with all they got and win just one for the Gipper." )
    , ( 1, 1939, " Clark Gable ", " Rhett Butler ", " Gone with the Wind ", "Frankly, my dear, I don't give a damn." )
    , ( 4
      , 1939
      , " Judy Garland "
      , " Dorothy Gale "
      , " The Wizard of Oz "
      , """
        "Toto, I've got a feeling we're not in Kansas anymore."
        and (#23) "There's no place like home."
        """
      )
    , ( 31
      , 1939
      , " Vivien Leigh "
      , " Scarlett O'Hara "
      , " Gone with the Wind "
      , """
        "After all, tomorrow is another day!"
        and (#59) "As God is my witness, I'll never be hungry again."
        """
      )
    , ( 65, 1939, " Basil Rathbone ", " Sherlock Holmes ", " The Adventures of Sherlock Holmes ", "Elementary, my dear Watson." )
    , ( 99, 1939, " Margaret Hamilton ", " Wicked Witch of the West ", " The Wizard of Oz ", "I'll get you, my pretty, and your little dog too!" )
    , ( 26, 1933, " Mae West ", " Lady Lou ", " She Done Him Wrong ", "Why don't you come up sometime and see me?" )
    , ( 60, 1933, " Oliver Hardy ", " Oliver ", " Sons of the Desert ", "Well, here's another nice mess you've gotten me into!" )
    , ( 84, 1933, " Robert Armstrong ", " Carl Denham ", " King Kong ", "Oh, no, it wasn't the airplanes. It was Beauty killed the Beast." )
    , ( 87, 1933, " Warner Baxter ", " Julian Marsh ", " 42nd Street ", "Sawyer, you're going out a youngster, but you've got to come back a star!" )
    , ( 30, 1932, " Greta Garbo ", " Grusinskaya ", " Grand Hotel ", "I want to be alone." )
    , ( 49, 1931, " Colin Clive ", " Henry Frankenstein ", " Frankenstein ", "It's alive! It's alive!" )
    , ( 73, 1931, " Edward G. Robinson ", " Cesare Enrico \"Rico\" Bandello ", " Little Caesar ", "Mother of mercy, is this the end of Rico?" )
    , ( 83, 1931, " Bela Lugosi ", " Count Dracula ", " Dracula ", "Listen to them. Children of the night. What music they make." )
    , ( 53, 1930, " Groucho Marx ", " Capt. Geoffrey T. Spaulding ", " Animal Crackers ", "One morning I shot an elephant in my pajamas. How he got in my pajamas, I don't know." )
    , ( 71, 1927, " Al Jolson ", " Jakie Rabinowitz/Jack Robin ", " The Jazz Singer ", "Wait a minute, wait a minute. You ain't heard nothin' yet!" )
    ]
