[![Build Status](https://travis-ci.org/mpdairy/elm-argspec.svg?branch=master)](https://travis-ci.org/mpdairy/elm-argspec)

ArgSpec is a library for Elm that allows you to specify a spec for a
list of strings that represent arguments, options, commands, etc,
useful for parsing interop from Javascript or from the commandline
using Node.js and libraries like
[Worker](http://package.elm-lang.org/packages/lukewestby/worker/latest).

###Example

Let's say your program named `runApp` can take the following commands/args:

```
./runApp polling <percent> <interval>
./runApp reset
./runApp once <timeout>
```

This can be very easily represented with an ArgSpec:

```elm
controlSpec : ArgSpec
controlSpec = Command "polling" &&& Argument "percent" &&& Argument "interval"
          ||| Command "reset"
          ||| Command "once" &&& Argument "timeout"
```

Then you can use `scan` with a list of strings:

```elm
rscan : Maybe ArgScan
rscan = scan controlSpec ["once", "100"]
```

If `scan` can't find a match, it will return `Nothing`. Otherwise, it
returns Just an `ArgScan`.

## Using the ArgScan Result

An `ArgScan` stores commands in a `Set`, options in another `Set`, and arguments in
a `Dict`. To check if individual commands or options have been scanned, you can use
`getCommand` and `getOption`. To get the (Maybe value) for an argument that
has been scanned, use `getArgument`. Otherwise, you can directly 
access the `Set`s and `Dict` in the `ArgScan` record with the fields
`.commands`, `.options`, and `.arguments`.

### Constructing Union Types

Often times you'll want to convert what you've scanned, especially
positional commands and arguments, into Elm data types. For
the example spec used above, named `controlSpec`, we might want to make a `Control` type that
looks like this:

```elm
type Control = Polling Float Int
             | Reset
             | Once Int
```

ArgSpec provides some convenient applicative-style functions to
help you do this::

```elm
mControl : ArgScan -> Maybe Control
mControl rscan = construct Polling (getCommand "polling" rscan)
                   `withFloatArg` getArgument "percent" rscan
                   `withIntArg` getArgument "interval" rscan
                 <|>
                 construct Reset (getCommand "reset" rscan)
                 <|>
                 construct Once (getCommand "once" rscan)
                    `withIntArg` getArgument "timeout" rscan
```

There is also `withArgString`. `<|>` means "alternative" and will try
the next construct if the previous fails (You can use also use
`maybeOr` inline if you don't like fancy inline symbols, but you'll have
to use parens around its arguments).

You can make your own `with____Arg`
functions using `withXArg`. All you have to do is supply a function
that converts a `String` to a `Maybe` of your desired type.
For instance, you might want to make a `withBoolArg` function:

```elm
withBoolArg : WithXArg Bool a
withBoolArg = withXArg (\ s -> if s == "true" then
                                   Just True
                               else
                                   if s == "false" then
                                       Just False
                                   else
                                       Nothing )
```

Now you can use `withBoolArg` just like `withIntArg` and the others.

There is also `withConstruct` which allows you to nest constructs.

```elm
serverSpec = ArgSpec
serverSpec = Command "start" &&& Argument "serverName" &&& controlSpec
             ||| Command "stop" &&& Argument "goodbyeMessage"

data Server = Start String Control
            | Stop String

mServer : ArgScan -> Maybe Server
mServer rscan = construct Start (getCommand "start" rscan)
                   `withStringArg` getArgument "serverName" rscan
                   `withConstruct` mControl rscan
                <|>
                construct Stop (getCommand "stop" rscan)
                   `withStringArg` getArgument "goodbyeMessage" rscan
```

## Options

Options can be short (`-a`) or long (`--allude`) or both. Short
options can be grouped together in the actual argument list, like
`-xvfz`. Options can also require an ArgSpec to be specified after.

For example, here is an option named "initialSize" that would match
`"--initial-size 640 480"`, saving `"640"` and `"480"` as `"sizeX"`
and `"sizeY"`:

```elm
initialSizeOption : ArgSpec
initialSizeOption = Option { short = Nothing
                           , long = Just "initial-size"
                           , name = "initialSize"
                           , description = Just "sets initial size of window"
                           , arguments = Just <| Argument "sizeX" &&& Argument "sizeY" }
```

The arguments after an option are just stored in a flat `Dict` along with all the other
arguments, so be sure to use unique argument names.

### Optional

`Optional` takes a list of optional `ArgSpec`s, and is a good place to
put your options. It matches as many as it can, which could be none,
then continues to scan.

```elm
sampleSpec = Command "demo"
             &&& Optional [ Command "later"
                          , Command "dust" &&& Argument "dustMass"
                          , initialSizeOption ]
             &&& Command "finished" &&& Argument "laserColor"
```
This spec requires the command named "demo" at the beginning and
at the end the command "finished" and a laser color, and it will match
any of the optional stuff if it's there. The following would match:
```
demo finished red
demo dust 3.0 later finished green
demo --initial-size 240 320 later finished blue
```
It's a good idea to store all your `Option`s in a list and then you
can just plunk it inside an `Optional` to easily add options in
multiple places.

#### Abiguity with Optional Arguments

There might be some ambiguity with optional items which could cause
the `scan` to succeed when it should actually fail. For instance, both
of the following will succeed:

```
demo --initial-size 240 320 later finished blue
demo --initial-size 240 later finished blue
```
The first case is correct, but the second case incorrectly consumes `later` as
the second argument to `initial-size`. I am not sure how to remove this ambiguity.

### Using Options

Using options that have no arguments is easy with `getOption`, which
just returns `True` if the option has been set, or `False` otherwise:

```elm
settings = { headlessMode = getOption "headless" rscan
           , trampoline = getOption "trampoline" rscan }
```

For options that require arguments, you can construct types as you
would with commands, and use `withDefault` from the `Maybe` module to set the default:

```elm
settings = { windowSize = withDefault (Vector 1027 768) <|
                          construct Vector (getOption "initialSize" rscan)
                                   `withIntArg` getArgument "sizeX" rscan
                                   `withIntArg` getArgument "sizeY" rscan }
```
If you just have an option with a single argument, like a `Float`, you
could do:

```elm
{ brightness = withDefault 1.0 <|
               construct identity (getOption "brightness" rscan)
                        `withFloatArg` getArgument "brightness" rscan }
```

If multiple short options that also require arguments are grouped
(`-abc`), the scan will fail.

## (&&&) and (|||)

`&&&` is the `And` infix operator that specifies that ArgSpecs must be matched
in order. `|||` is the `Or` infix operator and will just try the next
pattern if the first fails. `&&&` has higher precedence than `|||` so
you can mostly use them without parens.

See
[Example.elm](https://github.com/mpdairy/elm-argspec/blob/master/src/Example.elm)
for a complicated arg spec example.

## Pretty Auto-Printing

I'm planning to implement a `printHelp` function that prints out a
pretty, instructional version of any `ArgSpec`, like you would see on
a command line. Any options in the printed spec will be displayed in
their shortest version in the structure, grouped if possible, then
listed alphabetically, with descriptions, below the entire argument
structure.

## Instructive Errors

Rewriting `scan` to use `Result` instead of `Maybe` would allow for
instructive errors, which would be nice for commandline usage.
