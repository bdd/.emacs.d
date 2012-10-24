# bdd's Emacs #

Berk D. Demir's Emacs configuration and a few personal Elisp functions.


## Version ##

Target version is Emacs 24.2 running on Mac OS X (10.8.2).

Compiled from [Homebrew](http://mxcl.github.com/homebrew/):

        brew install emacs --cocoa --srgb

If you don't use Homebrew or don't have the compiler chain you can download
compiled version *(without the fullscreen patch)* from
[Emacs for Mac OS X](http://www.emacsformacosx.com).


## Installation ##

Although the majority of third party packages are coming from ELPA or compatible
package repositories, at least one critical extension, `use-package` is
integrated as a git submodule.  Before launching Emacs with the new config,
bring in all git submodules first.

    git submodule update --init --recursive

After you launch Emacs, run `install-my-elpa-pkgs` interactively.  Which means
typing `M-x install-my-elpa-pkgs <RET>`

All the packages and their dependencies will be downloaded.

To keep the packages up-to-date, occasionally `list-packages` interactively and
type `u` to mark the updated packages for installation and type `x` to execute
batch installation.
