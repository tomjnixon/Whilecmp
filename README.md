Whilecmp
========

A quick while to x86 compiler in Haskell.

Install
-------

You'll need at least `ghc`, `haskell-parsec` and `cabal-install`.

To install, run

	cabal install --prefix=$HOME

in the current directory.

This will install the compiler, called `whilecmp`.

Use it
------

To interpret:

	$ whilecmp -i examples/fac.while 
	fac = 362880
	n = 1

To compile:

	$ whilecmp -c examples/fac.while fac.s
	$ nasm -f elf fac.s
	$ gcc fac.o -o fac
	$ ./fac
	fac = 362880
	n = 1

About
-----

Made by [Tom Nixon](https://github.com/tomjnixon) (Haskell code), and [Matt Leach](https://github.com/matty3269) (x86 code).

MIT Licenced; see `LICENSE`.
