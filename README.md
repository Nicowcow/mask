[![CircleCI](https://circleci.com/gh/nmattia/makefile.svg?style=svg)](https://circleci.com/gh/nmattia/makefile)

# Haskell Makefile Parser and Generator

Parse and generate `Makefile`s. The project is available on hackage
[(latest)](http://hackage.haskell.org/package/makefile).

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
```


# Release checklist


1. Make sure you're on (latest) master.

1. Bump the version in `makefile.cabal`: `0.MAJOR.MINOR.PATCH`.

> Given a version number MAJOR.MINOR.PATCH, increment the:
>
> MAJOR version when you make incompatible API changes,
> MINOR version when you add functionality in a backwards-compatible manner, and
> PATCH version when you make backwards-compatible bug fixes.

1. Commit the updated `makefile.cabal` file with commit name `Release
   v1.MAJOR.MINOR.PATCH`, as well as the updated documentation.
1. Tag the commit with `git tag v1.MAJOR.MINOR.PATCH`.
1. Run `stack upload --pvp-bounds both .` to upload `makefile` to `hackage`.
1. Push with `git push --follow-tags`.
