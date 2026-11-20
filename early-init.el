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
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t
      make-pointer-invisible t)

;; Set before any package is loaded, so stale .elc files are never
;; preferred over newer .el sources.
(setq load-prefer-newer t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Performance tweaking for modern machines.
;; A high GC threshold (100MB) speeds up startup; init.el lowers it on
;; `emacs-startup-hook' so pauses stay short during editing.
(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1024 1024))

(set-face-attribute 'default nil
                    :family "JetBrainsMono Nerd Font"  ; must match an installed family exactly (see `fc-list : family`)
                    :height 110
                    :weight 'regular)

;; variable-pitch for org/prose
(set-face-attribute 'variable-pitch nil
                    :family "Noto Sans"
                    :height 130)


(setq-default left-margin-width 1
              right-margin-width 1)
(add-to-list 'default-frame-alist '(internal-border-width . 1))

(pixel-scroll-precision-mode 1)
