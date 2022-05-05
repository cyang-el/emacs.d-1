;;; init-elm.el --- Support for the Elm language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'elixir-mode)

(add-to-list 'auto-mode-alist '("\\.exs\\.ex\\'" . elixir-mode))

(provide 'init-elixir)
;;; init-elixir.el ends here
