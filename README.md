# bdd's ~/.emacs.d #

Berk D. Demir's Emacs configuration and a few custom Elisp functions.


## Version ##

Target version is Emacs 24.4.1 running on Mac OS X (10.10).

Compiled from [Homebrew](http://brew.sh)

     brew install emacs --cocoa

If you don't use Homebrew, you can download an alternate binary distribution
from [Emacs for Mac OS X](http://www.emacsformacosx.com).


## Installation ##

Almost all packages are sourced from [MELPA](http://melpa.milkbox.net/)
repository.  At first launch they will be downloaded and installed.

The only exception is the latest version of `org-mode`.  Emacs ships with an
older version. You will need to manually upgrade it to the latest version with
`M-x package-install <RET> org <RET>`.


## Staying up-to-date ##

`M-x list-packages <RET> U <RET> x <RET>`

`U` marks the updated packages for installation and `x` to executes the batch.
