###swank-gambit

**A [Gambit](http://gambitscheme.org/) Scheme backend for [SLIME](https://github.com/slime/slime).**

To setup a barebones SLIME session in Emacs:
```elisp
(require 'slime)
(slime-setup '(slime-repl))
(add-hook 'scheme-mode-hook (lambda () (slime-mode t)))
```

From the command line:
```sh
$ gsi swank-gambit.scm
```

In Emacs:
```elisp
M-x slime-connect
```
