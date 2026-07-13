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
      inhibit-startup-buffer-menu t
      make-pointer-invisible t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Performance tweaking for modern machines
;; Disable frequency of GC. This helps performance both during init
;; and after init. Value is in bytes so this is 100MB, as suggested in
;; <https://github.com/emacs-lsp/lsp-mode#performance>.
(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1024 1024))

(set-face-attribute 'default nil
                    :family "JetBrains Mono"  ; or "Iosevka", "Fira Code", "Cascadia Code", "JetBrains Mono"
                    :height 120
                    :weight 'regular)

;; variable-pitch for org/prose
(set-face-attribute 'variable-pitch nil
                    :family "Cantarell"
                    :height 130)


(setq-default left-margin-width 1
              right-margin-width 1)
(add-to-list 'default-frame-alist '(internal-border-width . 12))


;; Better default modes
(electric-pair-mode t)
(show-paren-mode 1)
(recentf-mode t)
(pixel-scroll-precision-mode 1)
