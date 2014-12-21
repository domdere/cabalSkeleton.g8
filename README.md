# Giter8 Haskell Template

An empty **Haskell** project using **cabal**.  Handy for projects that only involve **Haskell**, with maybe a little bit of **C**.

Includes a default test suite that uses [**doctest**] [doctest-github]

# Giter8

This template uses [**Giter8**] [giter8] to fill out default values in the cabal project file.

## Installing Giter8

Detailed instructions for installing **Giter8** can be found [**here**] [giter8-install].

But for **UNIX**y systems the following will suffice:

    curl https://raw.github.com/n8han/conscript/master/setup.sh | sh
    ~/bin/cs n8han/giter8
    ~/bin/g8

This will install `cs` and `g8` in `~/bin`.  It would be worthwhile adding `~/bin` to your `PATH` at this point.

## Using Giter8

This template can be downloaded with the command (assuming `~/bin` is added to your path):

    g8 git://github.it.nicta.com.au/dre/cabalSkeleton.g8.git

[giter8]: https://github.com/n8han/giter8 "n8han/giter8 on github.com"
[giter8-install]: https://github.com/n8han/giter8/blob/master/README.markdown#installation "Installation instructions for Giter8"
[doctest-github]: https://github.com/sol/doctest-haskell "sol/doctest on GitHub.com"
[cabal-skeleton]: https://github.com/domdere/cabalSkeleton.g8 "domdere/cabalSkeleton.g8 on GitHub.com"
