;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(setq byte-compile-warnings '(cl-functions))

(let ((minver "25.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "26.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(setq warning-minimum-level :emergency)

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

;; (require 'init-javascript)
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

;; https://github.com/pietroiusti/draw-tree
;; Example:
;;
;; (draw-tree '((a) (b . c) (d e)))  ==>
;; "
;; [o|o]---[o|o]---[o|/]
;;  |       |       |
;; [o|/]    |      [o|o]---[o|/]
;;  |       |       |       |
;;  a       |       d       e
;;          |
;;         [o|o]--- c
;;          |
;;          b
;; "
(load-file "~/.emacs.d/draw-tree.el")

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

;; zig
(unless (version< emacs-version "24")
  (add-to-list 'load-path "~/.emacs.d/zig-mode/")
  (autoload 'zig-mode "zig-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode)))

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
(add-to-list 'load-path "~/.emacs.d/plantuml-mode/")
(autoload 'plantuml-mode "plantuml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
;; (maybe-require-package 'plantuml-mode)
;; Sample jar configuration
(setq plantuml-jar-path "~/.emacs.d/jars/plantuml-1.2024.4.jar")
(setq plantuml-default-exec-mode 'jar)

;; mermaid js, odin, pony
(add-to-list 'load-path "~/.emacs.d/modes/")

;; mermaid
(autoload 'mermaid-mode "mermaid-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.mmd\\'" . mermaid-mode))

;; odin
(autoload 'odin-mode "odin-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.odin\\'" . odin-mode))

;; pony
(autoload 'pony-mode "pony-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.pony\\'" . mermaid-mode))

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
(setq custom-file (expand-file-name "customs.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (when (file-exists-p custom-file)
                                    (load custom-file)))

;; ts, tsx, react

(maybe-require-package 'typescript-mode)
(maybe-require-package 'web-mode)
(maybe-require-package 'dtrt-indent)

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :config
  (setq typescript-indent-level 2))

;; Web-mode for better TSX support
(use-package web-mode
  :mode ("\\.tsx\\'" . web-mode)
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-content-types-alist '(("jsx" . "\\.tsx\\'"))))

;; Auto-detect indentation
(use-package dtrt-indent
  :config
  (dtrt-indent-mode 1))

;; Company for auto-completion
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))

;; Eglot for language server protocol integration
(use-package eglot
  :hook ((typescript-mode web-mode) . eglot-ensure)
  :config
  ;; Add tsx support to eglot
  (add-to-list 'eglot-server-programs
               '((typescript-mode web-mode) . ("typescript-language-server" "--stdio")))

  ;; Configure eglot
  (setq eglot-autoshutdown t)
  (setq eglot-confirm-server-initiated-edits nil))

;; Prettier for code formatting
(use-package prettier-js
  :hook ((typescript-mode web-mode) . prettier-js-mode)
  :config
  (setq prettier-js-args '("--single-quote")))

;; Flycheck for real-time syntax checking
(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; https://depp.brause.cc/eyebrowse/
;; (maybe-require-package 'eyebrowse)
;; (eyebrowse-mode t)

;; https://github.com/progfolio/elpaca
(defvar elpaca-installer-version 0.9)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install a package via the elpaca macro
;; See the "recipes" section of the manual for more details.

;; (elpaca example-package)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;;When installing a package used in the init file itself,
;;e.g. a package which adds a use-package key word,
;;use the :wait recipe keyword to block until that package is installed/configured.
;;For example:
;;(use-package general :ensure (:wait t) :demand t)

;; Expands to: (elpaca evil (use-package evil :demand t))
(use-package evil :ensure t :demand t)

;;Turns off elpaca-use-package-mode current declaration
;;Note this will cause evaluate the declaration immediately. It is not deferred.
;;Useful for configuring built-in emacs features.
(use-package emacs :ensure nil :config (setq ring-bell-function #'ignore))

;; https://codeberg.org/meow_king/typst-ts-mode/wiki/Installation.md
(use-package typst-ts-mode
  :ensure (:type git :host codeberg :repo "meow_king/typst-ts-mode"))

;; Using provided command to build grammar from Source

;; After installing typst-ts-mode(which will be mentioned later), execute command typst-ts-mc-install-grammar to install or update the grammar, which will always install the correct version of Typst tree sitter grammar.

;; Note: This command calls treesit-install-language-grammar to install the grammar file. According to its documentation, you may need Git, a C compiler and (sometimes) a C++ compiler, and the linker to be installed and on PATH.


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
