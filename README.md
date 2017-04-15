[![Build Status](https://travis-ci.org/nmattia/mask.svg?branch=master)](https://travis-ci.org/nmattia/mask)

# Haskell Makefile Parser and Generator

Simple Haskell Makefile parser and generator. The project is available on
hackage [(latest)](http://hackage.haskell.org/package/makefile) and
documentation for `master` can be found [here](http://docs.nmattia.com/mask/).

# Example

## Parsing

``` Makefile

# Define compiler
CC:=gcc

# Define lazy compiler
LC=ghc

all: hello

hello: foo bar
	$(CC) baz.o
```

Running `parseMakefile :: IO (Either String Makefile)` (pretty-printed for
convenience):

``` haskell
Right
    ( Makefile
        { entries =
            [ Assignment SimpleAssign "CC" "gcc"
            , Assignment RecursiveAssign "LC" "ghc"
            , Rule
                (Target "all")
                [Dependency "hello"]
                []
            , Rule
                (Target "hello")
                [ Dependency "foo"
                , Dependency "bar"
                ]
                [Command "$(CC) baz.o"]
            ]
        }
    )
```

## Generating

``` haskell
myMakefile :: Makefile
myMakefile =
    Makefile
        { entries =
            [ Assignment SimpleAssign "foo" "bar"
            , Rule (Target "baz") [Dependency "qux"] [Command "rm -rf /"]
            ]
        }
```

Running `encodeMakefile :: Makefile -> Text`:

``` Makefile
foo:=bar
baz: qux
	rm -rf /
``
