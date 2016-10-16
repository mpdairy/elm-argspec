module ArgSpec exposing (ArgSpec (Command, Argument, Option, Optional, And, Or)
                        , OptionInfo, (&&&), (|||), ArgScan, scan
                        , getCommand, getArgument, getOption
                        , construct, withArg, withArgR, maybeOr, (<|>)
                        , withArgString, withArgFloat, withArgInt)

{-| Library for parsing command line arguments. You can form parsers that are similar
    to [http://docopt.org/](DocOpt) with positional commands, arguments, and options.
    Useful for sending commands to Elm through JavaScript or when using Node.js and
    [https://github.com/lukewestby/worker](Worker)

# Argument Spec
@docs ArgSpec, (&&&), (|||), OptionInfo

# Scanning
@docs scan, ArgScan, getCommand, getOption, getArgument

@docs construct, withArgString, withArgFloat, withArgInt, withArg, withArgR, maybeOr, (<|>)

-}

import Dict exposing (Dict)
import Set exposing (Set)
import Maybe exposing (andThen, withDefault)
import Regex exposing (regex)
import String


type alias Name = String
--
{-| Record type for declaring an Option. Options can be short, long, or both.
    Long option names are full string names, and are called with a "--" double dash.
    For instance, if `long = Just "option-one"` then its arg would be `--option-one`.

    Short option names are just a single alphabet character, upper or lower.
    They can be called with a single dash, like `-a`. If the options do not have any
    arguments, you can group them in one dash, like `-xvfz`.

    Arguments can be any ArgSpec after the option. This allows you to have complex
    commands after an option. For instance, if your option is:
```
{ short = Nothing
, long = Just "start-position"
, name = "startPosition"
, description = Just "sets the start position for object"
, arguments = Just <| Argument "start-x" &&& Argument "start-y"`}
```
Then the command line `--start-position 32 88` would activate it and
set "start-x" and "start-y" arguments to 32 and 88, respectively..
-}
type alias OptionInfo = { short : Maybe Char
                        , long : Maybe String
                        , name : String
                        , description : Maybe String
                        , arguments : Maybe ArgSpec}

{-| Form a spec for parsing incoming args. -}
type ArgSpec = Command Name
             | Argument Name
             | Option OptionInfo
             | Optional (List ArgSpec)
             | And ArgSpec ArgSpec
             | Or ArgSpec ArgSpec
--
type alias ArgVal = String
--
{-| Results from a successful scan. -}
type alias ArgScan = { commands : Set Name
                     , arguments : Dict Name ArgVal
                     , options : Set Name
                     , spec : ArgSpec
                     , args : List String }
--
{-| Combines two ArgSpecs together. Fails if either fails. -}
(&&&) : ArgSpec -> ArgSpec -> ArgSpec
(&&&) = And
infixl 3 &&&
--
{-| Returns first ArgSpec that succeeds or fails if both fails. -}
(|||) : ArgSpec -> ArgSpec -> ArgSpec
(|||) = Or
infixl 2 |||

{-| Checks if a particular command was scanned -}
getCommand : Name -> ArgScan -> Bool
getCommand name argscan = Set.member name (argscan.commands)

{-| Checks if a particular option was scanned -}
getOption : Name -> ArgScan -> Bool
getOption name argscan = Set.member name (argscan.options)

{-| Returns Just an argument's value (String) or Nothing if the arg was not scanned.  -}
getArgument : Name -> ArgScan -> Maybe ArgVal
getArgument name argscan = Dict.get name (argscan.arguments)

{-| Scans argument list and tries to match it to the ArgSpec. If it cannot find a match,
    it returns Nothing. Otherwise, it returns an ArgScan. -}
scan : ArgSpec -> List String -> Maybe ArgScan
scan spec args =
    scan' { commands = Set.empty
          , arguments = Dict.empty
          , options = Set.empty
          , spec = spec
          , args = args }
    `andThen`
    (\ rs -> if List.isEmpty rs.args then
                 Just rs
             else
                 Nothing )
--
scan' : ArgScan -> Maybe ArgScan
scan' s =
    case s.spec of
        Or s1 s2 ->
            scan' { s | spec = s1 } `maybeOr` scan' { s | spec = s2 }

        And s1 s2 ->
            scan' { s | spec = s1 }
                     `andThen` (\ rs -> scan' { rs | spec = s2 } )

        Optional sx -> Just <| handleOptional sx s

        Command c -> List.head s.args `andThen`
                     (\ arg -> if c == arg then
                                   Just { s | commands = Set.insert c s.commands
                                        , args = List.drop 1 s.args }
                               else Nothing)

        Argument a -> List.head s.args `andThen`
                      (\ v -> Just { s | arguments = Dict.insert a v s.arguments
                                   , args = List.drop 1 s.args } )

        Option o ->
            List.head s.args `andThen`
                (\ op ->
                     --LONG options
                     Maybe.map (\ long -> matchLongOption long op) o.long
                     `andThen`
                     (\ isMatchingLong ->
                          if isMatchingLong then
                              let sWithOpt = { s | options = Set.insert o.name s.options
                                             , args = List.drop 1 s.args }
                              in
                                  case o.arguments of
                                      Nothing ->
                                          Just sWithOpt

                                      Just arguments ->
                                          scan' { sWithOpt | spec = arguments }

                          else
                              Nothing)

                     `maybeOr`

                     --SHORT option
                     (Maybe.map (\ short -> ( matchShortOption short op
                                            , soloShortOption short op
                                            , short))
                          o.short
                     `andThen`
                         (\ (isMatch, isSolo, short) ->
                              if isMatch then
                                  let nextArgs = if isSolo then
                                                     List.drop 1 s.args
                                                 else
                                                     (dropShortOption short op)
                                                     :: (List.drop 1 s.args)
                                      sWithOpt = { s | options = Set.insert o.name s.options
                                                  , args = nextArgs }
                                  in
                                      case o.arguments of
                                          Nothing ->
                                              Just sWithOpt
                                          Just arguments ->
                                              if isSolo then
                                                  scan' { sWithOpt | spec = arguments }
                                              else
                                                  --non-solo shorts
                                                  --can't have args
                                                  Nothing
                              else
                                  Nothing))

                )

--        _ -> Nothing

--
handleOptional : List ArgSpec -> ArgScan -> ArgScan
handleOptional optionals s =
    case tryOneOf (\ spec -> scan' { s | spec = spec } ) optionals of
    Just rs -> handleOptional optionals rs
    Nothing -> s
--
matchLongOption : String -> String -> Bool
matchLongOption optionName = Regex.contains
                          (regex ("^--" ++ optionName ++ "$"))
--
matchShortOption : Char -> String -> Bool
matchShortOption optionChar = Regex.contains
                              (regex ("^-[A-Z,a-z]*"
                                          ++ (String.fromChar optionChar)
                                          ++ "[A-Z,a-z]*$"))
--
soloShortOption : Char -> String -> Bool
soloShortOption optionChar = Regex.contains (regex ("^-" ++ (String.fromChar optionChar) ++ "$"))
--
dropShortOption : Char -> String -> String
dropShortOption c = Regex.replace Regex.All (regex (String.fromChar c)) (always "")
--
isShortOption : String -> Bool
isShortOption = Regex.contains (regex "^-\\w+$")
--
tryOneOf : (a -> Maybe b) -> List a -> Maybe b
tryOneOf f ls = case ls of
                    [] -> Nothing
                    (x::xs) -> f x `maybeOr` tryOneOf f xs
--
{-| ok -}
maybeOr : Maybe a -> Maybe a -> Maybe a
maybeOr m1 m2 = case m1 of
                Nothing -> m2
                Just _ -> m1
--

-- Maybe functor
(<$>) : (a -> b) -> Maybe a -> Maybe b
(<$>) = Maybe.map
infixl 3 <$>

-- Maybe applicative
(<*>) : Maybe (a -> b) -> Maybe a -> Maybe b
(<*>) mab ma = case (mab, ma) of
                   (Nothing, _) -> Nothing
                   (_, Nothing) -> Nothing
                   (Just fab, Just a) -> Just <| fab a
infixl 3 <*>

-- Maybe alternative
{-| ok -}
(<|>) : Maybe a -> Maybe a -> Maybe a
(<|>) = maybeOr
infixl 2 <|>
--

{-| ok -}
construct : a -> Bool -> Maybe a
construct constructor bool = if bool then Just constructor else Nothing

{-| ok -}
withArg : Maybe (a -> b) -> (Maybe String, String -> Maybe a) -> Maybe b
withArg mc (argResult, typeConv) =
    mc <*> (argResult `andThen` typeConv)

{-| ok -}
withArgR : Maybe (a -> b) -> (Maybe String, String -> Result c a) -> Maybe b
withArgR mc (argResult, typeConv) = withArg mc (argResult, Result.toMaybe << typeConv)

{-| ok -}
withArgString : Maybe (String -> b) -> Maybe String -> Maybe b
withArgString mc s = withArg mc (s, (\ str -> Just str ))

{-| ok -}
withArgInt : Maybe (Int -> b) -> Maybe String -> Maybe b
withArgInt mc s = withArgR mc (s, String.toInt)

{-| ok -}
withArgFloat : Maybe (Float -> b) -> Maybe String -> Maybe b
withArgFloat mc s = withArgR mc (s, String.toFloat)


