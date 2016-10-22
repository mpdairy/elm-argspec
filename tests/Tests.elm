module Tests exposing (..)

import Test exposing (..)
import Expect
import String
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
        [ test "Addition" <|
            \ () ->
                Expect.equal (3 + 7) 10
        , test "String.left" <|
            \ () ->
                Expect.equal "a" (String.left 1 "abcdefg")
        , describe "scan"
            [ test "scan controleSpec1" <|
                  (\ () ->
                       Expect.equal
                       (scan controlSpec ["once", "100"])
                       ( Just { commands = Set.fromList ["once"]
                              , arguments = Dict.fromList [("timeout","100")]
                              , options = Set.empty
                              , spec = Argument "timeout"
                              , args = []} ))
            , test "scan controleSpec2" <|
                (\ () ->
                     Expect.equal
                     (scan controlSpec ["polling", "100", "300"])
                     ( Just { commands = Set.fromList ["polling"]
                            , arguments = Dict.fromList [("percent","100"), ("interval", "300")]
                            , options = Set.empty
                            , spec = Argument "interval"
                            , args = []} ))

            , test "scan controleSpec3" <|
                (\ () ->
                     Expect.equal
                     (scan controlSpec ["polling", "100", "300", "hey"])
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
