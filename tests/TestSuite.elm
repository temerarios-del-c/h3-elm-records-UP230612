module TestSuite exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, floatRange, int, list, string)
import Helper exposing (..)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (containing, exactText, tag, text)


add2Test : Test
add2Test =
    describe "Testing add2 function"
        [ test "add2 2 3 should reduce to 2+3" <|
            \_ ->
                add2 2 3
                    |> Expect.equal 5
        , fuzz2 int int "add2 x y should reduce to x + y" <|
            \x y ->
                add2 x y
                    |> Expect.equal (x + y)
        ]


add3Test : Test
add3Test =
    describe "Testing add3 function"
        [ test "add3 2 3 -10 should reduce to -5" <|
            \_ ->
                add3 2 3 -10
                    |> Expect.equal -5
        , fuzz3 (floatRange -1.0e12 1.0e12) (floatRange -1.0e12 1.0e12) (floatRange -1.0e12 1.0e12) "add3 x y z should reduce to x + y + z" <|
            \x y z ->
                add3 x y z
                    |> Expect.within (Expect.Absolute 0.000000000001) (x + y + z)
        ]


calcTest : Test
calcTest =
    describe "Testing calc function"
        [ test "calc 2 3 (+) should reduce to 5" <|
            \_ ->
                calc 2 3 (+)
                    |> Expect.equal 5
        , test "calc 2 3 (-) should reduce to -1" <|
            \_ ->
                calc 2 3 (-)
                    |> Expect.equal -1
        , fuzz2 int int "calc x y (+) should reduce to x + y" <|
            \x y ->
                calc x y (+)
                    |> Expect.equal (x + y)
        , fuzz2 int int "calc x y (-) should reduce to x - y" <|
            \x y ->
                calc x y (-)
                    |> Expect.equal (x - y)
        , fuzz2 int int "calc x y (//) should reduce to x // y" <|
            \x y ->
                calc x y (//)
                    |> Expect.equal (x // y)
        ]


languageFuzzer :
    Fuzzer
        { name : String
        , releaseYear : Int
        , currentVersion : String
        }
languageFuzzer =
    Fuzz.map3
        (\name releaseYear currentVersion ->
            { name = name
            , releaseYear = releaseYear
            , currentVersion = currentVersion
            }
        )
        Fuzz.string
        Fuzz.int
        Fuzz.string


languageNamesTest : Test
languageNamesTest =
    describe "Testing languageNames function"
        [ test "languageNames []" <|
            \_ ->
                languageNames []
                    |> Expect.equal []
        , test "languageNames [{name=\"elm\", releaseYear= 2012, currentVersion=\"0.19.1\"},{name=\"javascript\", releaseYear= 1995, currentVersion=\"ECMAScript 2025\"}]" <|
            \_ ->
                languageNames [ { name = "elm", releaseYear = 2012, currentVersion = "0.19.1" }, { name = "javascript", releaseYear = 1995, currentVersion = "ECMAScript 2025" } ]
                    |> Expect.equal [ "elm", "javascript" ]
        , fuzz (list languageFuzzer) "languageNames should return a list of strings" <|
            \fuzzList ->
                languageNames fuzzList
                    |> Expect.equal (List.map .name fuzzList)
        ]


uTypeFuzzer : Fuzzer String
uTypeFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant "Student"
        , Fuzz.constant "Professor"
        ]


userFuzzer :
    Fuzzer
        { name : String
        , uType : String
        }
userFuzzer =
    Fuzz.map2
        (\name uType ->
            { name = name
            , uType = uType
            }
        )
        Fuzz.string
        uTypeFuzzer


onlyStudentsTest : Test
onlyStudentsTest =
    describe "Testing onlyStudents function"
        [ test "onlyStudents []" <|
            \_ ->
                onlyStudents []
                    |> Expect.equal []
        , test "onlyStudents [{name=\"Roberto\", uType= \"Student\"}, {name=\"Mitsiu\", uType=\"Professor\"}]" <|
            \_ ->
                onlyStudents [ { name = "Roberto", uType = "Student" }, { name = "Mitsiu", uType = "Professor" } ]
                    |> Expect.equal [ "Roberto", "" ]
        , fuzz (list userFuzzer) "onlyStudents should return only the name of Student type" <|
            \fuzzList ->
                onlyStudents fuzzList
                    |> Expect.equal
                        (List.map
                            (\u ->
                                case .uType u of
                                    "Student" ->
                                        .name u

                                    _ ->
                                        ""
                            )
                            fuzzList
                        )
        ]


videogameAliasedFuzzer : Fuzzer Videogame
videogameAliasedFuzzer =
    Fuzz.map5 Videogame
        Fuzz.string
        Fuzz.int
        Fuzz.bool
        Fuzz.int
        (Fuzz.list Fuzz.string)


getVideogameGenresTest : Test
getVideogameGenresTest =
    describe "Testing getVideogameGenres function"
        [ test "getVideogameGenres []" <|
            \_ ->
                getVideogameGenres []
                    |> Expect.equal []
        , test "getVideogameGenres [{title=\"Control\", releaseYear=2019, available=True, downloads=1234567, genres=[\"Action\", \"Shooter\"]}, {title=\"Ocarina of time\", releaseyear=1998, available=False, downloads=12345, genres=[\"Action\", \"Adventure\"]}]" <|
            \_ ->
                getVideogameGenres [ { title = "Control", releaseYear = 2019, available = True, downloads = 1234567, genres = [ "Action", "Shooter" ] }, { title = "Ocarina of time", releaseYear = 1998, available = False, downloads = 12345, genres = [ "Action", "Adventure" ] } ]
                    |> Expect.equal [ [ "Action", "Shooter" ], [ "Action", "Adventure" ] ]
        , fuzz (list videogameAliasedFuzzer) "getVideogameGenres should return a List List String" <|
            \fuzzList ->
                getVideogameGenres fuzzList
                    |> Expect.equal (List.map (\vg -> .genres vg) fuzzList)
        ]


htmlTest : Test
htmlTest =
    describe "Testing html generated"
        [ test "main should have a <div><h1>My computer</h1></div>" <|
            \() ->
                main
                    |> Query.fromHtml
                    |> Query.has
                        [ tag "div"
                        , containing
                            [ tag "h1" ]
                        , containing
                            [ exactText "My laptop" ]
                        ]
        , test ("main should have a \n<div>\n  <div>\n    <ul>\n      <li></li>\n    </ul>\n  </div>\n</div>\nwith content \"Ram: " ++ .ram myLaptop ++ "\"") <|
            \() ->
                main
                    |> Query.fromHtml
                    |> Query.has
                        [ tag "div"
                        , containing
                            [ tag "div" ]
                        , containing
                            [ tag "ul" ]
                        , containing
                            [ tag "li" ]
                        , containing
                            [ exactText ("Ram: " ++ .ram myLaptop) ]
                        ]
        , test ("main should have a \n<div>\n  <div>\n    <ul>\n      <li></li>\n    </ul>\n  </div>\n</div>\nwith content \"Modelo: " ++ .model myLaptop ++ "\"") <|
            \_ ->
                main
                    |> Query.fromHtml
                    |> Query.has
                        [ tag "div"
                        , containing
                            [ tag "div" ]
                        , containing
                            [ tag "ul" ]
                        , containing
                            [ tag "li" ]
                        , containing
                            [ exactText ("Modelo: " ++ .model myLaptop) ]
                        ]
        , test ("main should have a \n<div>\n  <div>\n    <ul>\n      <li></li>\n    </ul>\n  </div>\n</div>\nwith content \"Marca: " ++ .brand myLaptop ++ "\"") <|
            \_ ->
                main
                    |> Query.fromHtml
                    |> Query.has
                        [ tag "div"
                        , containing
                            [ tag "div" ]
                        , containing
                            [ tag "ul" ]
                        , containing
                            [ tag "li" ]
                        , containing
                            [ exactText ("Marca: " ++ .brand myLaptop) ]
                        ]
        , test
            ("main should have a \n<div>\n  <div>\n    <ul>\n      <li></li>\n    </ul>\n  </div>\n</div>\nwith content \"Pulgadas: " ++ .screenSize myLaptop ++ "\"")
          <|
            \_ ->
                main
                    |> Query.fromHtml
                    |> Query.has
                        [ tag "div"
                        , containing
                            [ tag "div" ]
                        , containing
                            [ tag "ul" ]
                        , containing
                            [ tag "li" ]
                        , containing
                            [ exactText ("Pulgadas: " ++ .screenSize myLaptop) ]
                        ]
        ]
