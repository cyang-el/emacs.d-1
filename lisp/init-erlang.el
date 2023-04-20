;;; init-erlang.el --- Support for the Erlang language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'erlang)
  (require 'erlang-start))


;; more https://www.lambdacat.com/post-modern-emacs-setup-for-erlang/

(provide 'init-erlang)
;;; init-erlang.el ends here
