;;; init.el --- Personal Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Single-file config.  Navigate sections with `M-g o' (consult-outline).
;; Third-party packages are listed in `package-selected-packages' in
;; custom.el.  `M-x package-install' adds to it, `M-x package-delete'
;; removes from it, and missing ones are installed at startup.

;;; Code:

;;; Package management and custom file

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(defun my/install-missing-packages ()
  "Install packages from `package-selected-packages' that are missing.
Archive contents are refreshed first, so packages newer than the
local archive cache (or a fresh setup with no cache) are found.
Does nothing, and needs no network, when all packages are installed.
Errors (no network, bad signature, ...) are reported, not fatal, so
the rest of the init still runs."
  (when (seq-remove #'package-installed-p package-selected-packages)
    (with-demoted-errors "Package install failed: %S"
      (package-refresh-contents)
      (package-install-selected-packages 'noconfirm))))

;; Must run before the `use-package' forms below, some of which load
;; their package immediately.
(my/install-missing-packages)

;; Packages load lazily unless a block says `:demand' or enables a mode
;; in `:init'.
(setq use-package-always-defer t)

;;; Better defaults

(setq uniquify-buffer-name-style 'forward
      window-resize-pixelwise t
      use-short-answers t
      make-backup-files nil
      sentence-end-double-space nil
      kill-do-not-save-duplicates t
      isearch-lazy-count t
      isearch-allow-motion t     ; M-<, M->, C-v and M-v move between matches.
      isearch-motion-changes-direction t
      search-whitespace-regexp ".*?"
      global-auto-revert-non-file-buffers t ; Auto-revert non-file buffers too.
      disabled-command-function nil
      echo-keystrokes 1e-6      ; Display keystrokes in the echo area immediately.
      tab-bar-show 1            ; Hide the tab bar when there is only one tab.
      use-dialog-box nil        ; Don't pop up UI dialogs when prompting.
      enable-recursive-minibuffers t
      view-read-only t          ; Visit read-only files in `view-mode'.
      ;; Follow symlinks when opening files, so the buffer's directory is
      ;; the real one.
      find-file-visit-truename t
      ;; When killing in Emacs, push the system clipboard to the kill ring
      ;; first so it can still be reached with `yank-pop'.
      save-interprogram-paste-before-kill t)

;; Backups stay off, but auto-save is on so a crash doesn't lose
;; unsaved work.  Auto-save and lock files go to one directory instead
;; of littering projects with #file# and .#file files.
(let ((auto-save-dir (expand-file-name "auto-save/" user-emacs-directory)))
  (make-directory auto-save-dir t)
  (setq auto-save-default t
        auto-save-file-name-transforms `((".*" ,auto-save-dir t))
        lock-file-name-transforms `((".*" ,auto-save-dir t))))

;; early-init.el raises the GC threshold to speed up startup.  Lower it
;; afterwards so collections stay short during normal editing.
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 32 1024 1024))))

;; Do not allow the cursor in the minibuffer prompt.  The custom setter
;; also enables `cursor-intangible-mode' in the minibuffer.
(setopt minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

;; Better support for files with long lines.
(setq-default bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)

(global-auto-revert-mode 1)   ; Revert buffers when the file changes on disk.
(repeat-mode 1)
(global-hl-line-mode 1)
(global-so-long-mode 1)       ; Tame major modes in files with huge lines.
(electric-pair-mode 1)
(save-place-mode 1)           ; Return to the last position in visited files.

(use-package recentf
  :init (recentf-mode 1)
  :custom (recentf-max-saved-items 200))

(load-theme 'modus-vivendi-tinted t)

;;; Keyboard and platform

;; Function keys as modifier keys.
(keymap-set function-key-map "<f5>" #'event-apply-alt-modifier)
(keymap-set function-key-map "<f7>" #'event-apply-super-modifier)
(keymap-set function-key-map "<f8>" #'event-apply-hyper-modifier)

;; Try to stop Windows from hijacking the super key, and turn <apps>
;; (menu) into the hyper modifier.  On Linux rebind <apps> to Hyper via
;; xmodmap.
(when (eq system-type 'windows-nt)
  (setq w32-pass-rwindow-to-system nil
        w32-lwindow-modifier 'super
        w32-pass-apps-to-system nil
        w32-apps-modifier 'hyper)
  (w32-register-hot-key [s-]))

;; Easier to press `repeat' command.
(bind-key "<f6>" #'repeat)

;;; Minibuffer completion

(use-package vertico
  :init (vertico-mode 1)
  :bind (:map vertico-map
         ("M-RET"   . nil)
         ("M-s"     . nil)
         ("M-i"     . vertico-insert)
         ("C-M-n"   . vertico-next-group)
         ("C-M-p"   . vertico-previous-group)
         ;; Exit with the typed input instead of the selected candidate
         ;; (the default M-RET binding is removed above).
         ("C-j"     . vertico-exit-input)
         ("C->"     . embark-become)
         ("C-*"     . embark-act-all)
         ("M-s o"   . embark-export)
         ("C-c C-o" . embark-export)
         ("C-l"     . embark-export))
  :config
  (setq vertico-count 10
        vertico-resize t
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions))

(use-package vertico-directory
  :after vertico
  ;; More convenient directory navigation commands.
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names.
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-repeat
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind (("C-x ." . vertico-repeat)
         :map vertico-map
         ("M-P" . vertico-repeat-previous)
         ("M-N" . vertico-repeat-next)))

;; Persist minibuffer history over restarts.  Vertico sorts by history.
(use-package savehist
  :init (savehist-mode 1)
  :config
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))
                                        (eglot (styles orderless)))))

(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init (marginalia-mode 1))

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h"   . consult-history)
         ("C-c m"   . consult-mode-command)
         ("C-c M-k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ; orig. repeat-complex-command
         ("C-x b"   . consult-buffer)              ; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ; orig. bookmark-jump
         ;; Fast register access
         ("M-#"     . consult-register-load)
         ("M-'"     . consult-register-store)      ; orig. abbrev-prefix-mark
         ("C-M-#"   . consult-register)
         ("M-y"     . consult-yank-pop)            ; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e"   . consult-compile-error)
         ("M-g f"   . consult-flymake)
         ("M-g g"   . consult-goto-line)           ; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ; orig. goto-line
         ("M-g o"   . consult-outline)
         ("M-g h"   . consult-org-heading)
         ("M-g a"   . consult-org-agenda)
         ("M-g m"   . consult-mark)
         ("M-g k"   . consult-global-mark)
         ("M-g i"   . consult-imenu)
         ("M-g I"   . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d"   . consult-find)
         ("M-s D"   . consult-locate)
         ("M-s g"   . (lambda () (interactive) (consult-grep default-directory)))
         ("M-s G"   . consult-git-grep)
         ("M-s r"   . consult-ripgrep)
         ("M-s l"   . consult-line)
         ("M-s L"   . consult-line-multi)
         ("M-s k"   . consult-keep-lines)
         ("M-s u"   . consult-focus-lines)
         ("M-s e"   . consult-isearch-history)
         :map isearch-mode-map
         ("M-e"     . consult-isearch-history)     ; orig. isearch-edit-string
         ("M-s e"   . consult-isearch-history)     ; orig. isearch-edit-string
         ("M-s l"   . consult-line)                ; lets consult-line detect isearch
         ("M-s L"   . consult-line-multi))         ; lets consult-line detect isearch
  :init
  ;; Better register previews, also for the built-in register commands.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Select xref locations with preview.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Expensive previews only on demand (M-.).
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult-source-recent-file consult-source-project-recent-file consult-source-bookmark
   :preview-key "M-.")
  (setq consult-narrow-key "<"))

(use-package embark
  :bind (("C-."   . embark-act)
         ("C-;"   . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  ;; Replace the key help with a completing-read interface.
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers.
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :demand t)

;;; In-buffer completion and snippets

;; hippie-expand is dabbrev-expand on steroids.
(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand)
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package corfu
  :custom
  (corfu-cycle t)
  :init
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  (corfu-echo-mode 1)
  :config
  (eldoc-add-command #'corfu-insert)
  (keymap-set corfu-map "M-p" #'corfu-popupinfo-scroll-down)
  (keymap-set corfu-map "M-n" #'corfu-popupinfo-scroll-up)
  (keymap-set corfu-map "M-d" #'corfu-popupinfo-toggle))

(use-package cape
  :bind (("C-c p p"  . completion-at-point)
         ("C-c p t"  . complete-tag)
         ("C-c p d"  . cape-dabbrev)
         ("C-c p h"  . cape-history)
         ("C-c p f"  . cape-file)
         ("C-c p k"  . cape-keyword)
         ("C-c p s"  . cape-elisp-symbol)
         ("C-c p a"  . cape-abbrev)
         ("C-c p l"  . cape-line)
         ("C-c p w"  . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _"  . cape-tex)
         ("C-c p ^"  . cape-tex)
         ("C-c p &"  . cape-sgml)
         ("C-c p r"  . cape-rfc1345))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

;; Icons in Corfu popups.  `:demand' is required: with
;; `use-package-always-defer' an `:after' block is never loaded otherwise.
(use-package kind-icon
  :after corfu
  :demand t
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Same background as Corfu.
  (kind-icon-blend-background nil)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Inline completion preview.
(use-package completion-preview
  :bind (:map completion-preview-active-mode-map
              ("TAB" . completion-at-point)
              ("M-f" . completion-preview-insert)
              ("M-n" . completion-preview-next-candidate)
              ("M-p" . completion-preview-prev-candidate))
  :init (global-completion-preview-mode 1))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :bind (:map yas-minor-mode-map
              ("TAB"   . nil)
              ("<tab>" . nil)
              ("C-x f" . yas-expand)))

;; The official snippet collection.
(use-package yasnippet-snippets
  :after yasnippet)

;;; Buffers and windows

(use-package winner
  :init (winner-mode 1))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer-mode . (lambda ()
                          (ibuffer-switch-to-saved-filter-groups "default")))
  :custom
  (ibuffer-default-display-maybe-show-predicates t)
  (ibuffer-expert t)
  (ibuffer-movement-cycle nil)
  (ibuffer-old-time 24)       ; Hours before a buffer is considered old.
  (ibuffer-formats
   '((mark modified read-only " "
           (name 16 -1)
           " "
           (size 6 -1 :right)
           " "
           (mode 16 16)
           " " filename)
     (mark " "
           (name 16 -1)
           " " filename)))
  (ibuffer-maybe-show-regexps nil)
  (ibuffer-saved-filter-groups
   '(("default"
      ("Magit"
       (or
        (mode . magit-status-mode)
        (mode . magit-log-mode)
        (name . "\\*magit")
        (name . "magit-")
        (name . "git-monitor")))
      ("Clojure"
       (or
        (mode . clojure-mode)
        (mode . clojurescript-mode)
        (mode . clojurec-mode)
        (mode . cider-repl-mode)
        (name . "\\*cider")))
      ("Commands"
       (or
        (mode . shell-mode)
        (mode . eshell-mode)
        (mode . term-mode)
        (mode . vterm-mode)
        (mode . compilation-mode)))
      ("Python"
       (or
        (mode . python-mode)
        (mode . python-ts-mode)
        (mode . inferior-python-mode)))
      ("C++"
       (or
        (mode . c-mode)
        (mode . c++-mode)))
      ("Java"
       (or
        (mode . java-mode)
        (mode . java-ts-mode)))
      ("Xml"
       (mode . nxml-mode))
      ("Lisp"
       (mode . emacs-lisp-mode))
      ("Dired"
       (mode . dired-mode))
      ("Gnus"
       (or
        (mode . message-mode)
        (mode . mail-mode)
        (mode . gnus-group-mode)
        (mode . gnus-summary-mode)
        (mode . gnus-article-mode)
        (name . "^\\.newsrc-dribble")
        (name . "^\\*\\(sent\\|unsent\\|fetch\\)")
        (name . "^ \\*\\(nnimap\\|nntp\\|nnmail\\|gnus\\|server\\|mm\\*\\)")
        (name . "\\(Original Article\\|canonical address\\|extract address\\)")))
      ("Org"
       (or
        (name . "^\\*Calendar\\*$")
        (name . "^\\*Org Agenda")
        (name . "^ \\*Agenda")
        (mode . org-mode)))
      ("Emacs"
       (or
        (name . "^\\*scratch\\*$")
        (name . "^\\*Messages\\*$")
        (name . "^\\*\\(Customize\\|Help\\|helpful\\)")
        (name . "\\*\\(Echo\\|Minibuf\\)"))))))
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-shrink-to-minimum-size t t)
  (ibuffer-use-other-window t))

;; Window management, inspired by "Demystifying Emacs's Window Manager":
;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(defun display-buffer-2-windows (buffer alist)
  "If only one window is available split it and display BUFFER there.
ALIST is the option channel for display actions (see `display-buffer')."
  (when (eq (length (window-list nil 'no-minibuf)) 1)
    (display-buffer-pop-up-window buffer alist)))

;; Only public display actions, so this keeps working across Emacs
;; versions.
(setq display-buffer-base-action
      '((display-buffer-reuse-window
         display-buffer-2-windows
         display-buffer-in-previous-window
         display-buffer-use-some-window
         display-buffer-pop-up-frame)))

;; `helpful' names its buffers "*helpful ...*", so match both.
(add-to-list 'display-buffer-alist
             '("\\*\\(Help\\|helpful .*\\)\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)))

(add-to-list 'display-buffer-alist
             '("\\*Completions\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)
               (window-height . 10)))

;; Show dictionary definitions on the left.
(add-to-list 'display-buffer-alist
             '("^\\*Dictionary\\*"
               (display-buffer-in-side-window)
               (side . left)
               (window-width . 70)))

(keymap-global-set "C-c d" #'dictionary-lookup-definition)

;; Pop up dedicated buffers in a different window, and treat manual
;; buffer switching (C-x b) like programmatic switching.
(setopt switch-to-buffer-in-dedicated-window 'pop
        switch-to-buffer-obey-display-actions t)

(use-package project
  :config
  (add-to-list 'project-switch-commands '(consult-project-buffer "Project Buffers" ?b))
  (add-to-list 'project-switch-commands '(project-dired "Dired root" ?D))
  (add-to-list 'project-switch-commands '(consult-ripgrep "Consult ripgrep" ?r))
  (add-to-list 'project-switch-commands '(magit-project-status "Magit status" ?m)))

;;; Editing and navigation

;; Typing with an active region replaces it; DEL deletes it.
(use-package delsel
  :demand t
  :config (delete-selection-mode 1))

;; zap-to-char with avy: choose which occurrence to kill to.
(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package avy
  :bind (("s-." . avy-goto-char-2)
         ("s-s" . avy-goto-char-timer))
  :init
  ;; Set directly: `isearch-mode-map' exists before avy loads.
  (keymap-set isearch-mode-map "M-j" #'avy-isearch)
  :config
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

;; `query-replace' with live highlighting of matches and replacements.
(use-package visual-regexp
  :bind ([remap query-replace] . vr/query-replace))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; less-like keys in `view-mode' (used for read-only files).
(with-eval-after-load 'view
  (keymap-set view-mode-map "N" #'View-search-last-regexp-backward)
  (keymap-set view-mode-map "?" #'View-search-regexp-backward)
  (keymap-set view-mode-map "G" #'View-goto-line-last)
  (keymap-set view-mode-map "j" #'View-scroll-line-forward)
  (keymap-set view-mode-map "k" #'View-scroll-line-backward))

;; Explicitly set the mark.
(defun push-mark-no-activate ()
  "Push point to `mark-ring' without activating the region.
Equivalent to \\[set-mark-command] when `transient-mark-mode' is
disabled."
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(keymap-global-set "C-`" #'push-mark-no-activate)

(defun jump-to-mark ()
  "Jump to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with a prefix argument."
  (interactive)
  (set-mark-command 1))
(keymap-global-set "M-`" #'jump-to-mark)

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(keymap-global-set "C-x C-x" #'exchange-point-and-mark-no-activate)

;; Pulse the current line after big jumps.
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))
(dolist (command '(scroll-up-command scroll-down-command
                   recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))

(defun copy-whole-line ()
  "Copy the current line, without leading indentation."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (kill-ring-save (point) (line-end-position))))

;; DWIM case commands act on the region if active, else on the word.
(keymap-global-set "M-u" #'upcase-dwim)
(keymap-global-set "M-l" #'downcase-dwim)
(keymap-global-set "M-c" #'capitalize-dwim)
;; Exchange any two non-overlapping regions in a buffer.
(keymap-global-set "C-x C-M-t" #'transpose-regions)
(keymap-global-set "M-W" #'ffap-copy-string-as-kill)
(keymap-global-set "s-w" #'copy-whole-line)
(keymap-global-set "s-<tab>" #'mode-line-other-buffer)
(keymap-global-set "s-k" #'kill-paragraph)
(keymap-global-set "s-z" #'copy-from-above-command)
(keymap-global-set "C-S-h" #'backward-kill-word)
;; Killing a buffer other than the current one is rare.
(keymap-global-set "C-x k" #'kill-current-buffer)

;;; Personal keymap

(defun my/shell-fullscreen ()
  "Open `shell' in a single window with point at the end of the output."
  (interactive)
  (shell)
  (delete-other-windows)
  (goto-char (point-max)))

;; Replaces `suspend-frame' on C-z, which is only ever hit by accident.
(defvar-keymap my-keymap
  :doc "Personal command prefix, on C-z and <insert>."
  "t s" #'my/shell-fullscreen
  "s"   #'isearch-forward
  "r"   #'isearch-backward
  "b"   #'consult-buffer
  "x"   #'xref-etags-mode)
(keymap-global-set "C-z" my-keymap)
(keymap-global-set "<insert>" my-keymap)

;;; Help

(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-symbol]   . helpful-symbol)
         ([remap describe-key]      . helpful-key)
         :map help-map
         ("F"   . helpful-function)
         ("M-f" . helpful-macro)
         ("C"   . helpful-command)
         ;; Not on global C-c C-d: C-c C-<key> belongs to major modes
         ;; (e.g. CIDER's documentation prefix).
         (","   . helpful-at-point)))

;; Interactively expand Elisp macros.
(use-package macrostep
  :bind ("C-c e" . macrostep-expand))

;;; Shell

(use-package sh-script
  :config
  ;; Unbind `sh-set-indent' so C-c = stays the ediff prefix.
  (keymap-set sh-mode-map "C-c =" nil))

(with-eval-after-load 'comint
  (keymap-set comint-mode-map "C-c o" #'comint-delete-output))

(defun my/shell-mode-setup ()
  "Settings for `shell-mode' buffers."
  (setq-local comint-scroll-to-bottom-on-input t
              comint-input-ignoredups t)
  (auto-fill-mode -1)
  (abbrev-mode 1)
  (add-hook 'comint-output-filter-functions #'comint-truncate-buffer nil t))
(add-hook 'shell-mode-hook #'my/shell-mode-setup)

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;;; Org

(defun my/org-clock-in-state (state)
  "Switch STATE to DOING when clocking in on a TODO entry."
  (if (equal state "TODO") "DOING" state))

(defun my/org-save-after-refile (&rest _)
  "Save all Org buffers after a refile."
  (org-save-all-org-buffers))

(use-package org
  ;; Load after 2s of idle time so the first org buffer opens instantly.
  :defer 2
  :bind (:map org-mode-map
              ;; Org remaps C-<up>/C-<down> to its paragraph commands but
              ;; not the other `backward-paragraph' bindings like M-{.
              ([remap backward-paragraph] . org-backward-paragraph)
              ([remap forward-paragraph]  . org-forward-paragraph))
  :bind* (("C-c a" . org-agenda)
          ("C-c c" . org-capture))
  :config
  ;; Preload `org-modules' (ol-gnus, ol-eww, ...).  Org otherwise loads
  ;; them on the first `org-mode' activation, which is most of the
  ;; first-open delay.
  (org-load-modules-maybe)

  ;; Inserting a heading mid-entry goes after the entry, not inside it.
  (setq org-insert-heading-respect-content t)

  ;; Make C-a, C-e, and C-k smarter with regard to headline tags.
  (setq org-special-ctrl-a/e t
        org-special-ctrl-k t)

  ;; Sparse-tree highlighting breaks `org-indent-mode' invisibility.
  (setq org-highlight-sparse-tree-matches nil)

  (setq org-agenda-files '("~/org/"))

  ;; Enforce dependencies and hide blocked tasks in the agenda.
  (setq org-enforce-todo-dependencies t
        org-agenda-dim-blocked-tasks 'invisible)

  ;; Open Org Agenda in full screen.
  (setq org-agenda-window-setup 'only-window)

  ;; Only show files with something to report in clock reports.
  (setq org-agenda-clockreport-parameter-plist
        '(:link t :maxlevel 3 :stepskip0 t :fileskip0 t))

  (setq org-agenda-custom-commands
        '(("n" "Agenda and all TODOs"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-clockreport-mode t)
                        (org-agenda-files '("~/org/tasklist.org" "~/org/tasklist.org_archive" "~/org/scheduled.org" "~/org/birthdays.org"))))
            (todo "TODO|DOING"
                  ((org-agenda-files
                    '("~/org/tasklist.org"))))
            (tags "LOGGING=1"
                  ((org-agenda-files
                    '("~/org/tasklist.org")))))
           nil)
          ("k" "Kirocage lead"
           ((todo "BLOCKED|REVIEW" ((org-agenda-overriding-header "Needs my attention")))
            (todo "DOING|NEXT"     ((org-agenda-overriding-header "In flight")))
            (tags-todo "+decision" ((org-agenda-overriding-header "Open decisions")))
            (tags-todo "+finding"  ((org-agenda-overriding-header "Findings to triage"))))
           ((org-agenda-files '("~/org/kirocage.org"))
            (org-agenda-dim-blocked-tasks nil)))))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
          (sequence "⚑(T)" "λ(I)" "❓(H)" "|" "✔(D)" "✘(C)"))
        org-todo-keyword-faces '(("HANGUP" . warning)
                                 ("❓" . warning))
        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success))
        org-tags-column -80
        org-log-done 'time
        org-fold-catch-invisible-edits 'smart
        org-startup-indented t
        org-ellipsis " ▾"
        org-pretty-entities nil
        org-hide-emphasis-markers t)

  ;; Clocking in on a TODO entry switches it to DOING.
  (setq org-clock-in-switch-to-state #'my/org-clock-in-state)

  ;; Refile targets.
  (setq org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets '(("tasklist.org" :maxlevel . 3)
                             ("scheduled.org" :maxlevel . 3)
                             ("someday.org" :maxlevel . 3)
                             ("gtd.org" :maxlevel . 3))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  (advice-add 'org-refile :after #'my/org-save-after-refile)

  (setq org-capture-templates
        `(("i" "Idea" entry (file ,(concat org-directory "/idea.org"))
           "*  %^{Title} %?\n%U\n%a\n")
          ("t" "Todo" entry (file ,(concat org-directory "/gtd.org"))
           "* TODO %?\n%U\n%a\n" :clock-resume t)
          ("n" "Note" entry (file ,(concat org-directory "/note.org"))
           "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
          ("j" "Journal" entry (file ,(concat org-directory "/journal.org"))
           "*  %^{Title} %?\n%U\n%a\n" :clock-resume t)
          ("b" "Book" entry (file ,(concat org-directory "/book.org"))
           "* Topic: %^{Topic} -- Author: %^{Author} %? Added: %U"))))

;; Preload the Org agenda and capture libraries when idle, after Org.
(use-package org-agenda  :defer 3)
(use-package org-capture :defer 3)

;;; Dired

(use-package dired
  :init
  ;; Search DIR recursively for files matching a glob pattern.
  (keymap-global-set "C-<" #'find-name-dired)
  ;; Find files in DIR that contain matches for REGEXP.
  (keymap-global-set "C->" #'find-grep-dired)
  :bind (:map dired-mode-map
              ("J" . dired-up-directory))
  :config
  (setq dired-clean-confirm-killing-deleted-buffers nil ; Just kill them.
        dired-auto-revert-buffer t      ; Revert silently on revisit.
        dired-dwim-target t             ; Suggest the other Dired window as target.
        dired-kill-when-opening-new-dired-buffer t
        dired-listing-switches "-alh"
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-make-directory-clickable t
        dired-mouse-drag-files t)
  ;; `repeat-mode' makes j after C-x C-j repeat `dired-jump'; I want j
  ;; to stay `dired-goto-file'.
  (keymap-set dired-jump-map "j" nil))

;;; Version control and diffs

(use-package ediff
  :bind (("C-c = b" . ediff-buffers)
         ("C-c = B" . ediff-buffers3)
         ("C-c = c" . compare-windows)
         ("C-c = =" . ediff-files)
         ("C-c = f" . ediff-files)
         ("C-c = F" . ediff-files3)
         ("C-c = m" . count-matches)
         ("C-c = r" . ediff-revision)
         ("C-c = p" . ediff-patch-file)
         ("C-c = P" . ediff-patch-buffer)
         ("C-c = l" . ediff-regions-linewise)
         ("C-c = w" . ediff-regions-wordwise)))

;; Allow q to quit transient popups (Magit), in addition to C-g.
(use-package transient
  :config (transient-bind-q-to-quit))

(use-package magit
  ;; Load after 4s of idle time, after Org, so `magit-status' opens fast.
  :defer 4
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch))
  :init
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
  :config
  ;; Don't offer to save buffers; I save them myself.
  (setq magit-save-repository-buffers nil
        ;; Word-granularity differences within diff hunks.
        magit-diff-refine-hunk t)
  (transient-append-suffix 'magit-merge "-n"
    '("-u" "Allow unrelated" "--allow-unrelated-histories"))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))
  (transient-append-suffix 'magit-fetch "-t"
    '("-u" "Unshallow" "--unshallow")))

;; Changed lines in the fringe (in the margin for terminal frames,
;; which have no fringe).  diff-hl works through VC and refreshes with
;; Magit.
;; Hunk commands: C-x v [ / C-x v ] (move), C-x v n (revert),
;; C-x v * (show), C-x v S (stage).
(use-package diff-hl
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode         . diff-hl-dired-mode))
  :init
  (setq diff-hl-global-modes '(not org-mode))
  (global-diff-hl-mode 1)
  ;; `diff-hl-margin-mode' is global, so a daemon (which may serve
  ;; terminal frames) also uses the margin.
  (when (or (daemonp) (not (display-graphic-p)))
    (diff-hl-margin-mode 1))
  :config
  ;; Update indicators as you type, not only on save.
  (diff-hl-flydiff-mode 1))

;;; Programming

;; Each built-in *-ts-mode declares its own grammar source, and
;; `treesit-auto-install-grammar' (default `ask') offers to install a
;; missing grammar when the mode is first used.
(setopt treesit-enabled-modes '(java-ts-mode python-ts-mode))

;; Jump to definition by grep when no smarter xref backend exists.
(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 100))

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l o" . eglot-code-actions-organize-imports)
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format))
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible)
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-extend-to-xref nil)
  (eglot-report-progress nil)
  ;; Formatting and hover stay enabled: `eglot-format' needs the former
  ;; and eldoc documentation at point needs the latter.
  (eglot-ignored-server-capabilities
   '(:documentOnTypeFormattingProvider
     :colorProvider
     :foldingRangeProvider)))

;;; Clojure

;; clojure-mode already maps .clj, .cljs, .cljc and .edn to the right
;; major modes.  CIDER picks the REPL type from the major mode, so .cljs
;; must stay in `clojurescript-mode'.
(use-package clojure-mode
  ;; Move by camelCase sub-words (Java class names).
  :hook (clojure-mode . subword-mode))

(defun my/cider-start-http-server ()
  "Load the buffer, switch the REPL to its ns and evaluate (ns/start)."
  (interactive)
  (cider-load-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))

(defun my/cider-reset ()
  "Evaluate (user/reset) in the REPL."
  (interactive)
  (cider-interactive-eval "(user/reset)"))

(defun my/cider-user-ns ()
  "Switch the REPL to the user namespace."
  (interactive)
  (cider-repl-set-ns "user"))

(use-package cider
  ;; Bound in `cider-mode-map': C-c C-v is CIDER's eval prefix and C-M-r
  ;; is `isearch-backward-regexp', so those keys are left alone.
  :bind (:map cider-mode-map
              ("C-c S" . my/cider-start-http-server)
              ("C-c R" . my/cider-reset)
              ("C-c u" . my/cider-user-ns))
  :custom
  (cider-repl-wrap-history t)
  (cider-repl-pop-to-buffer-on-connect t)
  ;; Show the error buffer on errors.  It is also selected, because
  ;; `cider-auto-select-buffer' defaults to t.
  (cider-show-error-buffer t))

;;; Applications

;; Not on global C-c C-o: C-c C-<key> belongs to major modes
;; (org-open-at-point, comint-delete-output, ...).
(use-package browse-url
  :bind ("C-c w" . browse-url-at-point))

(use-package elfeed
  :bind ("C-x w w" . elfeed)
  :config
  (setq elfeed-feeds
        '(("http://nullprogram.com/feed/" blog emacs)
          "http://www.50ply.com/atom.xml"
          "https://planet.emacslife.com/atom.xml"
          ("http://nedroid.com/feed/" webcomic)
          ("https://cestlaz.github.io/rss.xml" Zamansky))))

(use-package gptel
  :custom (gptel-default-mode 'org-mode))

;;; Mode line

(use-package mood-line
  :init (mood-line-mode 1))

;;; init.el ends here
