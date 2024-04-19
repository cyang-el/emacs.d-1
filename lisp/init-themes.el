;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'color-theme-sanityinc-solarized)
(require-package 'color-theme-sanityinc-tomorrow)
(require-package 'gruvbox-theme)
(require-package 'solarized-theme)
(require-package 'monokai-theme)
(require-package 'zenburn-theme)
(require-package 'tao-theme)
(require-package 'one-themes)
(require-package 'anti-zenburn-theme)

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

;; Toggle between light and dark

(defun light ()
  "Activate a light color theme."
  (interactive)
  ;; (setq custom-enabled-themes '(gruvbox-light-soft))
  (setq custom-enabled-themes '(solarized-light))
  ;; (setq custom-enabled-themes '(anti-zenburn))
  ;; (setq custom-enabled-themes '(tao-yang))
  ;; (setq custom-enabled-themes '(one-light))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  ;; (setq custom-enabled-themes '(monokai))
  (setq custom-enabled-themes '(zenburn))
  ;; (setq custom-enabled-themes '(tao-yin))
  ;; (setq custom-enabled-themes '(gruvbox-dark-soft))
  ;; (setq custom-enabled-themes '(solarized-dark))
  (reapply-themes))

(add-hook 'after-init-hook 'light)


(when (maybe-require-package 'dimmer)
  (setq-default dimmer-fraction 0.15)
  (add-hook 'after-init-hook 'dimmer-mode)
  (with-eval-after-load 'dimmer
    ;; TODO: file upstream as a PR
    (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all))))
  (with-eval-after-load 'dimmer
    ;; Don't dim in terminal windows. Even with 256 colours it can
    ;; lead to poor contrast.  Better would be to vary dimmer-fraction
    ;; according to frame type.
    (defun sanityinc/display-non-graphic-p ()
      (not (display-graphic-p)))
    (add-to-list 'dimmer-exclusion-predicates 'sanityinc/display-non-graphic-p)))

;; font and size
;; (set-face-attribute 'default nil :height 100 :font "Source Code Pro")

(defun my-big-font ()
  "My big font."
  (interactive)
  (set-face-attribute 'default nil :height 170))

(defun my-small-font ()
  "My small font."
  (interactive)
  (set-face-attribute 'default nil :height 110))

(add-hook 'after-init-hook 'my-small-font)

;; windows size
(setq initial-frame-alist '
      ((top . 0) (left . 0) (width . 159) (height . 50)))

;; cursor
(setf cursor-type 'box)

(provide 'init-themes)
;;; init-themes.el ends here
