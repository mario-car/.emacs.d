;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist '(("\"\"" . "\"~/.emacs.d/backup\"")))
 '(calendar-week-start-day 1)
 '(gptel-default-mode 'org-mode)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount
   '(4 ((shift) . hscroll) ((meta)) ((control meta) . global-text-scale)
       ((control) . text-scale)))
 '(org-babel-load-languages '((java . t) (shell . t) (emacs-lisp . t)))
 '(package-selected-packages
   '(avy-zap bash-completion bind-chord cape cider corfu
	     dumb-jump eglot-booster eglot-java elfeed
	     embark-consult expand-region git-gutter gptel gptel-magit
	     helpful key-chord kind-icon lsp-java lsp-mode macrostep magit
	     marginalia mood-line orderless org-present pomo-cat rg
	     use-package-chords vertico visual-regexp vterm yasnippet yasnippet-snippets))
 '(project-ignore-buffer-conditions '("/target/"))
 '(project-vc-ignores '("target/"))
 '(scroll-margin 1)
 '(send-mail-function 'sendmail-send-it)
 '(type-break-interval 1200))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
