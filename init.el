;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(let ((minver "25.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "26.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; (setq warning-minimum-level :emergency)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* t) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))
(add-to-list 'image-types 'svg)

;; scratch buffer

(setq initial-major-mode 'text-mode)

(setq initial-scratch-message
      (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))


;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


;; Bootstrap config


(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH


;; Allow users to provide an optional "init-preload-local.el"
(require 'init-preload-local nil t)

;; Load configs for specific features and modes
(maybe-require-package 'diminish)
(maybe-require-package 'scratch)
(maybe-require-package 'command-log-mode)

(require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-themes)
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flycheck)

(require 'init-recentf)
(require 'init-minibuffer)
(require 'init-hippie-expand)
(require 'init-company)
(require 'init-windows)
(require 'init-sessions)
(require 'init-mmm)

(require 'init-editing-utils)
(require 'init-whitespace)

(require 'init-vc)
(require 'init-darcs)
(require 'init-git)
(require 'init-github)

(require 'init-projectile)

(require 'init-compile)
(require 'init-crontab)
(require 'init-textile)
(require 'init-markdown)
(require 'init-csv)
(require 'init-erlang)
(require 'init-javascript)
(require 'init-php)
(require 'init-org)
(require 'init-nxml)
(require 'init-html)
(require 'init-css)
(require 'init-haml)
(require 'init-http)
(require 'init-python)
(require 'init-haskell)
(require 'init-elm)
(require 'init-purescript)
(require 'init-ruby)
(require 'init-rails)
(require 'init-sql)
(require 'init-elixir)
(require 'init-ocaml)
(require 'init-j)
(require 'init-nim)
(require 'init-rust)
(require 'init-toml)
(require 'init-yaml)
(require 'init-docker)
(require 'init-terraform)
(require 'init-nix)
(maybe-require-package 'nginx-mode)

(require 'init-paredit)
(require 'init-lisp)
(require 'init-slime)
(require 'init-clojure)
(require 'init-clojure-cider)
(require 'init-common-lisp)
(require 'init-scheme)

(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'init-misc)

(require 'init-folding)
(require 'init-dash)

;;(require 'init-twitter)
;; (require 'init-mu)
(require 'init-ledger)
;; Extra packages which don't require any configuration

(require 'google-c-style)
(add-hook 'c-mode-common-hook
          (lambda()
            (subword-mode)
            (google-set-c-style)
            (google-make-newline-indent)
            (setq c-basic-offset 4)))

(maybe-require-package 'sudo-edit)
(maybe-require-package 'gnuplot)
(maybe-require-package 'lua-mode)
(maybe-require-package 'htmlize)
(maybe-require-package 'xterm-color)
(when *is-a-mac*
  (maybe-require-package 'osx-location))
(maybe-require-package 'dotenv-mode)
(maybe-require-package 'shfmt)

(when (maybe-require-package 'uptimes)
  (setq-default uptimes-keep-count 200)
  (add-hook 'after-init-hook (lambda () (require 'uptimes))))

(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))

(maybe-require-package 'direnv)
(require 'init-direnv)
(maybe-require-package 'json-navigator)

;; nyan-mode
(maybe-require-package 'nyan-mode)
(nyan-mode t)

;; rmsbolt
;; https://gitlab.com/jgkamat/rmsbolt
(maybe-require-package 'rmsbolt)

;; koka
(require 'koka-mode)

;; tla+ https://github.com/mrc/tla-tools
(maybe-require-package 'polymode)
(require 'tla-pcal-mode)(require 'tla-tools)

;;; Basic cloudformation setup
(maybe-require-package 'cfn-mode)
(add-auto-mode 'cfn-mode "\\.template\\'")

;; lsp-mode
(setq lsp-keymap-prefix "C-c l")
(maybe-require-package 'lsp-mode)
(setq lsp-prefer-flymake nil)
(setq lsp-keep-workspace-alive nil)
(maybe-require-package 'lsp-ui)

;; eglot
(maybe-require-package 'eglot)

;; go
(add-to-list 'load-path "~/.emacs.d/go-mode.el")
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
;; (maybe-require-package 'go-mode)
(add-hook 'go-mode-hook #'eglot-ensure)

;; c++, using ccls
(add-hook 'c++-mode-hook #'eglot-ensure)

;; c, using clangd
(add-hook 'c-mode-hook #'eglot-ensure)
;; (add-hook 'kotlin-mode-hook #'lsp)

;; tidalcycle
(maybe-require-package 'tidal)
(setq tidal-boot-script-path "/home/cy/.cabal/store/ghc-9.4.7/tidal-1.9.5-8504329ed407541fafc4841d341cf62ec3b0e4427089da4d0ccfc8fe4ef0546c/share/BootTidal.hs
")

;; plantuml
(add-to-list 'load-path "~/.emacs.d/plantuml-mode/plantuml-mode.el")
(autoload 'plantuml-mode "plantuml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
;; (maybe-require-package 'plantuml-mode)
;; Sample jar configuration
(setq plantuml-jar-path "~/.emacs.d/jars/plantuml-1.2024.4.jar")
(setq plantuml-default-exec-mode 'jar)

;; mermaid js
(autoload 'mermaid-mode "mermaid-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.mmd\\'" . mermaid-mode))

;; k8s
(add-to-list 'load-path "~/.emacs.d/kubed")
(require 'kubed)

;; imenu-list
(maybe-require-package 'imenu-list)

;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

;; https://depp.brause.cc/eyebrowse/
;; (maybe-require-package 'eyebrowse)
;; (eyebrowse-mode t)


;; Locales (setting them earlier in this file doesn't work in X)
(require 'init-locales)

;; Allow users to provide an optional "init-local" containing personal settings
(require 'init-local nil t)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
