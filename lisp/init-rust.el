;;; init-rust.el --- Support for the Rust language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'rustic)
(require-package 'company)
(setq rustic-analyzer-command '("~/.rustup/toolchains/stable-x86_64-apple-darwin/bin/rust-analyzer"))
(setq rustic-lsp-server 'rust-analyzer)
(setq rustic-lsp-client 'lsp-mode)

(provide 'init-rust)
;;; init-rust.el ends here
