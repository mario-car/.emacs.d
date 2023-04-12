;; -*- lexical-binding: t -*-

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Turn off the alarm bell.
(setq ring-bell-function #'ignore)

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t)

(setq use-file-dialog nil
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Performance tweaking for modern machines
;; Disable frequency of GC. This helps performance both during init
;; and after init. Value is in bytes so this is 100MB, as suggested in
;; <https://github.com/emacs-lsp/lsp-mode#performance>.
(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1024 1024))

;; Better default modes
(electric-pair-mode t)
(show-paren-mode 1)
(recentf-mode t)
