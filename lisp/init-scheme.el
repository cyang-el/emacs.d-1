;;; init-scheme.el --- Slime support for Common Lisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'geiser)
(require-package 'geiser-chicken)
(require-package 'geiser-gambit)
(require-package 'geiser-guile)
(require-package 'geiser-racket)
(require-package 'geiser-chibi)
(require-package 'geiser-gauche)
(require-package 'scribble-mode)

(add-hook 'scribble-mode-hook #'geiser-mode)
(add-to-list 'auto-mode-alist '("\\.scrbl\\'" . scribble-mode))

(provide 'init-scheme)
;;; init-scheme.el ends here
