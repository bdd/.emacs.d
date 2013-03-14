# bdd's ~/.emacs.d #

Berk D. Demir's Emacs configuration and a few custom Elisp functions.


## Version ##

Target version is Emacs 24.3 running on Mac OS X (10.8.3).

Compiled from [Homebrew](http://mxcl.github.com/homebrew/):

     brew install emacs --cocoa --srgb

If you don't use Homebrew or don't have the compiler chain you can download
a compiled version *(without the fullscreen patch)* from
[Emacs for Mac OS X](http://www.emacsformacosx.com).


## Installation ##

Almost all packages are sourced from [MELPA](http://melpa.milkbox.net/)
repository.  The only exception is `use-package`, a macro to ensure presence of
packages and isolate their configuration.  It is sourced as a Git submodule and
the submodule system needs to be initialized before launching Emacs.

    git submodule update --init --recursive

Packages from MELPA will be downloaded and installed at first launch.

## Staying up-to-date ##

Occasionally invoke `list-packages` interactively.
Type `U` to mark the updated packages for installation and type `x` to execute
batch installation.
