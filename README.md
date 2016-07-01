# Haskell Makefile Parser

Simple Haskell Makefile parser. Documentation is located
[here](http://docs.nmattia.com/mask/).


Example:

``` Makefile

# I am a comment, and I want to say that the variable CC will be
# the compiler to use.
CC=g++
# Hey!, I am comment number 2. I want to say that CFLAGS will be the
# options I'll pass to the compiler.
CFLAGS=-c -Wall

all: hello

hello: main.o factorial.o hello.o
	$(CC) main.o factorial.o hello.o -o hello

main.o: main.cpp
	$(CC) $(CFLAGS) main.cpp

factorial.o: factorial.cpp
	$(CC) $(CFLAGS) factorial.cpp

hello.o: hello.cpp
	$(CC) $(CFLAGS) hello.cpp

clean:
	rm *o hello
```

Running `parseMakefile :: IO (Either String Makefile)` (output cleaned up):

``` haskell
Right (
    Makefile {
        entries = [
            Assignment "CC" "g++",
            Assignment "CFLAGS" "-c -Wall",
            Rule (Target "all")
                 [Dependency "hello"]
                 [],
            Rule (Target "hello")
                 [Dependency "main.o",Dependency "factorial.o",Dependency "hello.o"]
                 [Command "$(CC) main.o factorial.o hello.o -o hello"],
            Rule (Target "main.o")
                 [Dependency "main.cpp"]
                 [Command "$(CC) $(CFLAGS) main.cpp"],
            Rule (Target "factorial.o")
                 [Dependency "factorial.cpp"]
                 [Command "$(CC) $(CFLAGS) factorial.cpp"],
            Rule (Target "hello.o")
                 [Dependency "hello.cpp"]
                 [Command "$(CC) $(CFLAGS) hello.cpp"],
            Rule (Target "clean")
                 []
                 [Command "rm *o hello"]
        ]
    }
)
```
