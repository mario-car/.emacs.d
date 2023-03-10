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

;; Initialise installed packages
(setq package-enable-at-startup nil)

(defvar package-quickstart)

;; Allow loading from the package cache
(setq package-quickstart t)

(setq native-comp-async-report-warnings-errors 'silent) ; emacs28 with native compilation
