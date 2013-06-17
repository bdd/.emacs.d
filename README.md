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
repository.  At first launch they will be downloaded and installed.

The only exception is the latest version of `org-mode`.  Emacs ships with
an older version of so you need to manually install from `list-packages' menu.


## Staying up-to-date ##

Occasionally invoke `list-packages` interactively.
Type `U` to mark the updated packages for installation and type `x` to execute
batch installation.
