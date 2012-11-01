# bdd's Emacs #

Berk D. Demir's Emacs configuration and a few personal Elisp functions.


## Version ##

Target version is Emacs 24.2 running on Mac OS X (10.8.2).

Compiled from [Homebrew](http://mxcl.github.com/homebrew/):

        brew install emacs --cocoa --srgb

If you don't use Homebrew or don't have the compiler chain you can download
a compiled version *(without the fullscreen patch)* from
[Emacs for Mac OS X](http://www.emacsformacosx.com).


## Installation ##

Most of the external packages are coming from ELPA and compatible repositories.
A few packages, especially the most critical one `use-package` is sourced as a
git submodule.  A post-install script is provided to take care of git submodule
initialization (registration and clone) and then install all the specified
packages from ELPA repositories.

    ./post-install.els


## Staying up-to-date ##

Occasionally invoke `list-packages` interactively.
Type `U` to mark the updated packages for installation and type `x` to execute
batch installation.
