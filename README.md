#swank-gambit

##A [Gambit](http://gambitscheme.org/) Scheme backend for [SLIME](http://common-lisp.net/project/slime).

To setup a barebones SLIME session in Emacs:
```elisp
(require 'slime)
(slime-setup '(slime-repl))
```

From the command line:
```sh
gsi swank-gambit.scm
```

In Emacs:
```elisp
M-x slime-connect ENT ENT ENT
```

