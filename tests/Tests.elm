module Tests exposing (..)

import Test exposing (..)
import Expect
--import String
import ArgSpec exposing (..)
import Set exposing (Set)
import Dict exposing (Dict)

controlSpec : ArgSpec
controlSpec = Command "polling" &&& Argument "percent" &&& Argument "interval"
              ||| Command "reset"
              ||| Command "once" &&& Argument "timeout"
--
type Control = Polling Float Int
             | Reset
             | Once Int

--- OPTIONAL
initialSizeOption : ArgSpec
initialSizeOption = Option { short = Nothing
                           , long = Just "initial-size"
                           , name = "initialSize"
                           , description = Just "sets initial size of window"
                           , arguments = Just <| Argument "sizeX" &&& Argument "sizeY" }
--
optionalSpec : ArgSpec
optionalSpec = Command "demo"
             &&& Optional [ Command "later"
                          , Command "dust" &&& Argument "dustMass"
                          , initialSizeOption ]
             &&& Command "finished" &&& Argument "laserColor"
--
--OPTIONS
--
optionA : ArgSpec
optionA = Option { short = Just 'a'
                 , long = Nothing
                 , name = "Option-A"
                 , description = Just "enables option number 1"
                 , arguments = Nothing }
--
optionB : ArgSpec
optionB = Option { short = Just 'b'
                 , long = Just "option-b"
                 , name = "Option-B"
                 , description = Just "enables option number b"
                 , arguments = Just <| Argument "bx" &&& Argument "by" }
--
optionC : ArgSpec
optionC = Option { short = Just 'c'
                 , long = Nothing
                 , name = "Option-C"
                 , description = Just "enable option C"
                 , arguments = Nothing }
--
optionD : ArgSpec
optionD = Option { short = Just 'd'
                 , long = Just "option-d"
                 , name = "Option-D"
                 , description = Just "enables option d"
                 , arguments = Just <| Command "get" &&& Argument "getCount"
                                   ||| Command "end" }
--
options : List ArgSpec
options = [optionA, optionB, optionC, optionD]
--
optionsSpec : ArgSpec
optionsSpec = Optional options
--
-- CONSTRUCTING
--
mControl : ArgScan -> Maybe Control
mControl rscan = construct Polling (getCommand "polling" rscan)
                     `withFloatArg` getArgument "percent" rscan
                     `withIntArg` getArgument "interval" rscan
                 <|>
                 construct Reset (getCommand "reset" rscan)
                 <|>
                 construct Once (getCommand "once" rscan)
                     `withIntArg` getArgument "timeout" rscan
--
type BTest = BTest String Bool
--
bControl : ArgScan -> Maybe BTest
bControl rscan = construct BTest (getCommand "btest" rscan)
                   `withStringArg` getArgument "blast" rscan
                   `withBoolArg` getArgument "onoff" rscan
--
withBoolArg : WithXArg Bool a
withBoolArg = withXArg (\ s -> if s == "true" then
                                   Just True
                               else
                                   if s == "false" then
                                       Just False
                                   else
                                       Nothing )
--
all : Test
all =
    describe "ArgSpec"
        [ describe "scan controlSpec 'once 100'"
            [ test "" <|
                  (\ () ->
                       Expect.equal
                       (scan controlSpec ["once", "100"])
                       ( Just { commands = Set.fromList ["once"]
                              , arguments = Dict.fromList [("timeout","100")]
                              , options = Set.empty
                              , spec = Argument "timeout"
                              , args = []} ))
            , test "scan controleSpec 'polling 100 300'" <|
                (\ () ->
                     Expect.equal
                     (scan controlSpec ["polling", "100", "300"])
                     ( Just { commands = Set.fromList ["polling"]
                            , arguments = Dict.fromList [("percent","100"), ("interval", "300")]
                            , options = Set.empty
                            , spec = Argument "interval"
                            , args = []} ))

            , test "scan controleSpec 'polling 100 300 hey'" <|
                (\ () ->
                     Expect.equal
                     (scan controlSpec ["polling", "100", "300", "hey"])
                     Nothing)

            , test "scan optionalSpec 'demo finished red'" <|
                (\ () ->
                     Expect.equal
                     (scan optionalSpec ["demo", "finished", "red"])
                     ( Just { commands = Set.fromList ["demo", "finished"]
                            , arguments = Dict.fromList [("laserColor","red")]
                            , options = Set.empty
                            , spec = Argument "laserColor"
                            , args = []} ))
            , test "scan optionalSpec 'demo dust 3.0 later finished green'" <|
                (\ () ->
                     Expect.equal
                     (scan optionalSpec ["demo", "dust", "3.0", "later", "finished", "green"])
                     ( Just { commands = Set.fromList ["demo", "dust", "finished", "later"]
                            , arguments = Dict.fromList [("laserColor","green"), ("dustMass", "3.0")]
                            , options = Set.empty
                            , spec = Argument "laserColor"
                            , args = []} ))

            , test "scan optionalSpec 'demo --initial-size 240 320 later finished blue'" <|
                (\ () ->
                     Expect.equal
                     (scan optionalSpec ["demo"
                                        , "--initial-size", "240", "320"
                                        , "later", "finished", "blue"])
                     ( Just { commands = Set.fromList ["demo", "finished", "later"]
                            , arguments = Dict.fromList [ ("laserColor","blue")
                                                        , ("sizeX","240")
                                                        , ("sizeY","320")]
                            , options = Set.fromList ["initialSize"]
                            , spec = Argument "laserColor"
                            , args = []} ))

            , test "scan optionsSpec '-ac --option-b 38 44'" <|
                (\ () ->
                     Expect.equal
                     (scan optionsSpec ["-ac"
                                       , "--option-b", "38", "44"])
                     ( Just { commands = Set.empty
                            , arguments = Dict.fromList [ ("bx","38")
                                                        , ("by","44") ]
                            , options = Set.fromList ["Option-A", "Option-B", "Option-C"]
                            , spec = Argument "by"
                            , args = []} ))

            , test "scan optionsSpec '-adb'" <|
                (\ () ->
                     Expect.equal
                     (scan optionsSpec ["-adb"])
                     Nothing)
            ]

        , describe "construct"
            [ test "construct1" <|
                  (\ () ->
                       Expect.equal
                       ( mControl { commands = Set.fromList ["once"]
                                  , arguments = Dict.fromList [("timeout","100")]
                                  , options = Set.empty
                                  , spec = Argument "timeout"
                                  , args = []} )
                       ( Just <| Once 100 ))
            , test "construct2" <|
                  (\ () ->
                       Expect.equal
                       ( mControl { commands = Set.fromList ["polling"]
                                  , arguments = Dict.fromList [("percent","100"), ("interval", "300")]
                                  , options = Set.empty
                                  , spec = Argument "interval"
                                  , args = [] } )
                       ( Just <| Polling 100.0 300 ))
            , test "construct3" <|
                (\ () ->
                     Expect.equal
                     ( mControl { commands = Set.fromList ["polling"]
                                , arguments = Dict.fromList [("percento","100"), ("interval", "300")]
                                , options = Set.empty
                                , spec = Argument "interval"
                                , args = [] } )
                     ( Nothing ) )

            , test "construct4" <|
                (\ () ->
                     Expect.equal
                     (scan controlSpec ["polling", "100", "300", "hey"])
                     Nothing)

            , test "construct5" <|
                (\ () ->
                     Expect.equal
                     ( bControl { commands = Set.fromList ["btest"]
                                , arguments = Dict.fromList [("blast","big"), ("onoff", "true")]
                                , options = Set.empty
                                , spec = Argument "interval"
                                , args = [] } )
                     ( Just <| BTest "big" True ) )
            ]
        ]
