
ArgSpec is a library for Elm that allows you to specify a spec for a
list of strings that represent arguments, options, commands, etc,
useful for parsing interop from Javascript or from the commandline
using Node.js and libraries like
[Worker](http://package.elm-lang.org/packages/lukewestby/worker/latest).

###Example

Let's say your program named `runApp` can take the following commands/args:

```
runApp polling <interval> <timeout>
runApp reset
runApp once <timeout>
```

This can be very easily represented with an ArgSpec:

```elm
appSpec : ArgSpec
appSpec = Command "polling" &&& Argument "percent" &&& Argument "timeout"
       ||| Command "reset"
       ||| Command "once" &&& Argument "timeout"
```

Then you can use `scan` with a list of strings:

```elm
rscan : Maybe ArgScan
rscan = scan appSpec ["once", "100"]
```

If `scan` can't find a match, it will return `Nothing`. Otherwise, it
returns Just an `ArgScan`.

See
[Example.elm](https://github.com/mpdairy/elm-argspec/blob/master/src/Example.elm)
for a more complicated Spec example.

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
the example spec named `appSpec`, we might want to get a `Control` type that
looks like this:

```elm
type Control = Polling Float Int
             | Reset
             | Once Int
```

ArgSpec has provided some convenient applicative-style functions to
help you do this::

```elm
mControl : ArgScan -> Maybe Control
mControl rscan = construct Polling (getCommand "polling" rscan)
                   `withArgFloat` getArgument "percent" rscan
                   `withArgInt` getArgument "timeout" rscan
                 <|>
                 construct Reset (getCommand "reset") rscan
                 <|>
                 construct Once (getCommand "once" rscan)
                   `withArgInt` getArgument "timeout") rscan
```

There is also `withArgString`. The `withArg` function allows you to
specify a custom function for converting the string value of the
argument, so you could even construct nested constructed types.


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

Short options that require arguments should not be grouped.

## Optional

`Optional` takes a list of optional `ArgSpec`s. It matches as many as
it can, which could be zero, and the scan continues. It's best to put
your `Option`s in `Optional`, but you can also do other commands.

```elm
sampleSpec = Command "optional"
             &&& Optional [ Command "later"
                          , Command "dust" &&& Argument "dustMass"
                          , initialSizeOption ]
             &&& Command "finished" &&& Argument "laserColor"
```
This spec requires the command named "optional" at the beginning and
at the end the command "finished" and a laser color, and it will match
any of the optional stuff if it's there. The following would match:
```
optional finished red
optional dust 3.0 later finished green
optional --initial-size 240 320 later finished blue
```

## (&&&) and (|||)

`&&&` is the `And` infix operator that specifies that ArgSpecs must be matched
in order. `|||` is the `Or` infix operator and will just try the next
pattern if the first fails. `&&&` has higher precedence than `|||` so
you can mostly use them without parens.

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
