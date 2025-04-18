;; Better default settings
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      window-resize-pixelwise t
      frame-resize-pixelwise t
      load-prefer-newer t
      backup-by-copying t
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      use-short-answers t
      make-backup-files nil
      auto-save-default nil
      sentence-end-double-space nil
      kill-do-not-save-duplicates t
      isearch-lazy-count t
      isearch-allow-motion t		; When isearching, enable M-<, M->, C-v and M-v to skip between matches in an intuitive fashion.
      isearch-motion-changes-direction t
      search-whitespace-regexp ".*?"
      global-auto-revert-non-file-buffers t ; Auto-revert all buffers, not only file-visiting buffers.
      disabled-command-function nil
      echo-keystrokes 1e-6		; display keystrokes in the echo area immediately
      tab-bar-show 1			; hide tab bar when it is the only tab, and show again when additional tabs created
      indent-tabs-mode nil              ; use spaces instead of tabs
      whitespace-action '(cleanup auto-cleanup) ; cleanup whitespace
      use-dialog-box nil)		;don't pop up UI dialogs when prompting
;; Load custom.el
(load custom-file)
(package-install-selected-packages)
(setq use-package-always-defer t)

(use-package modus-themes
  :init
  (load-theme 'modus-operandi-tinted t))

;; Function keys as modifier keys
  (keymap-set function-key-map "<f5>" #'event-apply-alt-modifier)
  (keymap-set function-key-map "<f7>" #'event-apply-super-modifier)
  (keymap-set function-key-map "<f8>" #'event-apply-hyper-modifier)


;; Try to stop Windows from hijacking super key
(when (eq system-type 'windows-nt)
  (setq w32-pass-rwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)
  (w32-register-hot-key [s-]))

;; Turn <apps> (menu) into hyper modifier
;; On Linux rebind <apps> to Hyper via xmodmap
(when (eq system-type 'windows-nt)
  (setq w32-pass-apps-to-system nil)
  (setq w32-apps-modifier 'hyper)) ; Menu/App key


;; Enable use-package chords
(use-package use-package-chords
  :demand t
  :init (key-chord-mode 1))
(use-package emacs
  :chords (("xx" . xref-etags-mode)))

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
;; Turn on repeat mode
(repeat-mode)

(defvar org-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'org-next-visible-heading)
    (define-key map "p" 'org-previous-visible-heading)
    (define-key map "f" 'org-forward-heading-same-level)
    (define-key map "b" 'org-backward-heading-same-level)
    (define-key map "u" 'outline-up-heading)
    map)
  "Keymap to repeat org key sequences.  Used in `repeat-mode'.")
(put 'org-next-visible-heading 'repeat-map 'org-repeat-map)
(put 'org-previous-visible-heading 'repeat-map 'org-repeat-map)
(put 'org-forward-heading-same-level 'repeat-map 'org-repeat-map)
(put 'org-backward-heading-same-level 'repeat-map 'org-repeat-map)
(put 'outline-up-heading 'repeat-map 'org-repeat-map)

(defvar-keymap smerge-repeat-key-map
  :doc "Keymap to repeat smerge key sequence."
  :repeat t
  "<" #'smerge-diff-base-upper
  "=" #'smerge-diff-upper-lower
  ">" #'smerge-diff-base-lower
  "C" #'smerge-combine-with-next
  "E" #'smerge-ediff
  "R" #'smerge-refine
  "RET" #'smerge-keep-current
  "a" #'smerge-keep-all
  "b" #'smerge-keep-base
  "l" #'smerge-keep-lower
  "n" #'smerge-next
  "p" #'smerge-prev
  "r" #'smerge-resolve
  "u" #'smerge-keep-upper)

;; Easier to press `repeat' command
(bind-key "<f6>" #'repeat)

;; hippie-expand is dabrev-expand on steroids? 
(use-package hippie-expand
  :bind ([remap dabbrev-expand] . hippie-expand)
  :commands (hippie-expand)
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; Enable vertico
(use-package vertico
  :commands vertico-mode
  :init (vertico-mode 1)
  :bind (:map vertico-map
         ("M-RET"   . nil)
         ("M-s"     . nil)
         ("M-i"     . vertico-insert)
         ("C-M-n"   . vertico-next-group)
         ("C-M-p"   . vertico-previous-group)
         ("C-j"     . (lambda () (interactive)
	        	(if minibuffer--require-match
	        	    (minibuffer-complete-and-exit)
	        	  (exit-minibuffer))))
         ("C->"     . embark-become)
         (">"       . embark-become)
         ("C-<tab>"   . embark-act-with-completing-read)
         ("C-o"     . embark-minimal-act)
         ("C-M-o"   . embark-minimal-act-noexit)
         ("C-*"     . embark-act-all)
         ("M-s o"   . embark-export)
         ("C-c C-o" . embark-export)
         ("C-l"     . embark-export))
  :config
  (setq vertico-count 10
        vertico-resize t)
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode)
  :config
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-repeat
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind (("C-x ." . vertico-repeat)
	 :map vertico-map
	 ("M-P"   . vertico-repeat-previous)
	 ("M-N"   . vertico-repeat-next)))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c M-k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g h" . consult-org-heading)
         ("M-g a" . consult-org-agenda)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . (lambda () (interactive) (consult-grep default-directory)))
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key "M-.")

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Enable `winner' feature which provides an undo/redo stack for windows
(use-package winner
  :ensure nil
  :init
  (winner-mode +1))

;; Package `transpose-frame' provides simple commands to mirror,
;; rotate, and transpose Emacs windows: `flip-frame', `flop-frame',
;; `transpose-frame', `rotate-frame-clockwise',
;; `rotate-frame-anticlockwise', `rotate-frame'.
(use-package transpose-frame
  :bind* (("C-x C-z R" . #'transpose-frame)
          ("C-x C-z r" . #'rotate-frame-clockwise)
          ("C-x C-z f" . #'flip-frame)
          ("C-x C-z F" . #'flop-frame)
	  :repeat-map transpose-frame-repeat-map
	  ("r" . #'rotate-frame-clockwise)
	  ("R" . #'transpose-frame)
	  ("f" . #'flip-frame)
	  ("F" . #'flop-frame)))

;; Package `buffer-move' provides simple commands to swap Emacs
;; windows: `buf-move-up', `buf-move-down', `buf-move-left',
;; `buf-move-right'.
(use-package buffer-move
  :bind* (("C-x C-z <up>" . #'buf-move-up)
	  ("C-x C-z <down>" . #'buf-move-down)
	  ("C-x C-z <left>" . #'buf-move-left)
	  ("C-x C-z <right>" . #'buf-move-right)))

;; Feature `ibuffer' provides a more modern replacement for the
;; `list-buffers' command.
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-default-display-maybe-show-predicates t)
  (ibuffer-expert t)
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
      ("Coq"
       (or
        (mode . coq-mode)
        (name . "\\<coq\\>")
        (name . "_CoqProject")))
      ("Commands"
       (or
        (mode . shell-mode)
        (mode . eshell-mode)
        (mode . term-mode)
        (mode . compilation-mode)))
      ("Haskell"
       (or
        (mode . haskell-mode)
        (mode . haskell-cabal-mode)
        (mode . haskell-literate-mode)))
      ("Rust"
       (or
        (mode . rust-mode)
        (mode . cargo-mode)
        (name . "\\*Cargo")
        (name . "^\\*rls\\(::stderr\\)?\\*")
        (name . "eglot")))
      ("Nix"
       (mode . nix-mode))
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
        (name . "^\\*\\(Customize\\|Help\\)")
        (name . "\\*\\(Echo\\|Minibuf\\)"))))))
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-shrink-to-minimum-size t t)
  (ibuffer-use-other-window t)
  :init
  (add-hook 'ibuffer-mode-hook
            #'(lambda ()
                (ibuffer-switch-to-saved-filter-groups "default"))))

(use-package emacs
  :config
  (defun display-buffer-2-windows (buffer alist)
    "If only one window is available split it and display BUFFER there.
ALIST is the option channel for display actions (see `display-buffer')."
    (when (eq (length (window-list nil 'no-minibuf)) 1)
      (display-buffer--maybe-pop-up-window buffer alist)))

  (setq display-buffer-base-action
        '((display-buffer--maybe-same-window
           display-buffer-reuse-window
           display-buffer--maybe-pop-up-frame
           display-buffer-2-windows
           display-buffer-in-previous-window
           display-buffer-use-some-window
           display-buffer-pop-up-frame)))

  ;; Feature `saveplace' provides a minor mode for remembering the
  ;; location of point in each file you visit, and returning it there
  ;; when you find the file again.
  (save-place-mode +1))

;; Window configuration for special windows.
;; This section inspired by the article "Demystifying Emacs’s Window
;; Manager" found here:
;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(add-to-list 'display-buffer-alist
             '("\\*Help\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)))

(add-to-list 'display-buffer-alist
             '("\\*Completions\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)
               (window-height . 10)))

;; Show dictionary definition on the left
(add-to-list 'display-buffer-alist
             '("^\\*Dictionary\\*"
               (display-buffer-in-side-window)
               (side . left)
               (window-width . 70)))

;; define a key to define the word at point.
(define-key global-map (kbd "C-c d") #'dictionary-lookup-definition)

;; pop up dedicated buffers in a different window.
(customize-set-variable 'switch-to-buffer-in-dedicated-window 'pop)
;; treat manual buffer switching (C-x b for example) the same as
;; programmatic buffer switching.
(customize-set-variable 'switch-to-buffer-obey-display-actions t)

;; turn off forward and backward movement cycling
(customize-set-variable 'ibuffer-movement-cycle nil)
;; the number of hours before a buffer is considered "old" by
;; ibuffer.
(customize-set-variable 'ibuffer-old-time 24)
;; prefer the more full-featured built-in ibuffer for managing
;; buffers.
(global-set-key [remap list-buffers] #'ibuffer-list-buffers)

;; Follow symlinks when opening files. This has the concrete impact,
;; for instance, that when you edit init.el with M-P e e i and then
;; later do C-x C-f, you will be in the Radian repository instead of
;; your home directory.
(setq find-file-visit-truename t)

(use-package project
  :ensure nil
  :config
  (defun my-project-root ()
    (interactive)
    (let ((project (project-current)))
      (while (ignore-errors (cdr project))
	(setq project (cdr project)))
      (dired project)))
  (add-to-list 'project-switch-commands '(consult-project-buffer "Project Buffers" ?b))
  (add-to-list 'project-switch-commands '(my-project-root "Dired root" ?D))
  (add-to-list 'project-switch-commands '(consult-ripgrep "Consult ripgrep" ?r))
  (add-to-list 'project-switch-commands '(magit-project-status "Magit status" ?m)))

;; Feature `delsel' provides an alternative behavior for certain
;; actions when you have a selection active. Namely: if you start
;; typing when you have something selected, then the selection will be
;; deleted; and if you press DEL while you have something selected, it
;; will be deleted rather than killed. (Otherwise, in both cases the
;; selection is deselected and the normal function of the key is
;; performed.)
(use-package delsel
  :demand t
  :config

  (delete-selection-mode +1))

;; Improved zap-to-char function
;; Let's you select to which character should it kill
(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

;; try to browse documents in read-only mode for easier navigation
(setq view-read-only t)
(defun my-view-mode-keys ()
  (keymap-set view-mode-map "N" #'View-search-last-regexp-backward)
  (keymap-set view-mode-map "?" #'View-search-regexp-backward) ; Less does this.
  (keymap-set view-mode-map "G" #'View-goto-line-last)
  (keymap-set view-mode-map "j" #'View-scroll-line-forward)
  (keymap-set view-mode-map "k" #'View-scroll-line-backward))
(add-hook 'view-mode-hook 'my-view-mode-keys)


;; jump to character
(use-package avy
  :bind (("s-." . avy-goto-char-timer)))

;; Package `visual-regexp' provides an alternate version of
;; `query-replace' which highlights matches and replacements as you
;; type.
(use-package visual-regexp
  :bind (([remap query-replace] . #'vr/query-replace)))

;; Package `visual-regexp-steroids' allows `visual-regexp' to use
;; regexp engines other than Emacs'; for example, Python or Perl
;; regexps.
(use-package visual-regexp-steroids
  :demand t
  :after visual-regexp
  :config

  ;; Use Emacs-style regular expressions by default, instead of
  ;; Python-style.
  (setq vr/engine 'emacs))

;; expand-region is useful. But there are existing keybindings for
;; selecting text like M-@ or C-M-@, so will try to rely on those
;; instead.
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :bind (:map yas-minor-mode-map
	      ("TAB" . nil)
	      ("<tab>" . nil)
	      ("C-x f" . yas-expand)))
					
;; the official snippet collection https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets
  :after (yasnippet))


(use-package corfu
  :bind (:map corfu-map
	      ("RET"   . nil)
	      ("S-<return>" . corfu-insert))
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-exclude-modes'.
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode 1)
  (corfu-echo-mode 1)
  :config
  (eldoc-add-command #'corfu-insert)
  (define-key corfu-map (kbd "M-p") #'corfu-popupinfo-scroll-down)
  (define-key corfu-map (kbd "M-n") #'corfu-popupinfo-scroll-up)
  (define-key corfu-map (kbd "M-d") #'corfu-popupinfo-toggle))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  (setq completions-detailed t))

;; Corfu Extensions (Cape)
(use-package cape
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
	 ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  :config
  ;; Silence the pcomplete capf, no errors or messages!
  ;; Important for corfu
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

;;;;; Kind Icon (For Corfu)
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)) ; Enable `kind-icon'

;; Package `dumb-jump' provides a mechanism to jump to the definitions
;; of functions, variables, etc. in a variety of programming
;; languages. The advantage of `dumb-jump' is that it doesn't try to
;; be clever, so it "just works" instantly for dozens of languages
;; with zero configuration.
(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 100)
  :config
  ;; use completion system instead of popup window
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package sh-script
  :config
  ;; This is bind to sh-set-indent, and I don't use this. Since at the
  ;; moment I have no use for this keybinding, and it conflicts with
  ;; my keybinding for diff package, I'll turn it off.
  (keymap-set sh-mode-map "C-c =" nil))


;; shell settings
(add-hook 'shell-mode-hook (lambda () (setq comint-scroll-to-bottom-on-input t
                                            comint-input-ignoredups t)
			     (keymap-set comint-mode-map "C-c C-o" nil)
			     (keymap-set comint-mode-map "C-c o" 'comint-delete-output)))
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'shell-mode-hook (lambda () (auto-fill-mode -1)
                             (abbrev-mode 1)))

(with-eval-after-load "shell"
  (defun get-tgf-job-id ()
  "Yank first word on the line, and go to the prompt."
  (move-beginning-of-line nil)
  (mark-word)
  (kill-ring-save nil nil t)
  (end-of-buffer))

(defun print-dtjob ()
  "Print dtjob from the tgf job on current line."
  (interactive)
  (get-tgf-job-id)
  (insert "dt ")
  (yank)
  (comint-send-input))

(defun open-tgf-log ()
  "Open tgf log on current line"
  (interactive)
  (get-tgf-job-id)
  (insert "ol ")
  (yank)
  (comint-send-input))

(keymap-set shell-mode-map "C-<return>" #'print-dtjob)
(keymap-set shell-mode-map "S-<return>" #'open-tgf-log)

;; Select JOB_ID from 'tgq' or 'tgr' output.
;; Basically, select first word, but when used in context of TGF
;; output, that is JOB_ID
(fset 'jobid
   (kmacro-lambda-form [?\C-a ?\C-  ?\M-f ?\M-w ?\C-y] 0 "%d"))
(keymap-set shell-mode-map "C-x C-z s" #'jobid)

;; Select whole line, and move to the end of buffer (prompt).(
(fset 'job-select-all
(kmacro-lambda-form [?\C-p ?\C-a ?\C-k ?\M-> ?\C-y] 0 "%d"))
(keymap-set shell-mode-map "C-x C-z a" #'job-select-all))

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
;; When the lines in a buffer are so long that performance could suffer to an unacceptable degree, we say “so long”2 to the buffer’s major mode
(global-so-long-mode 1)

;; Package `helpful' provides a complete replacement for the built-in
;; Emacs help facility which provides much more contextual information
;; in a better format.
(use-package helpful
  :bind (;; Remap standard commands.
         ([remap describe-function] . #'helpful-callable)
         ([remap describe-variable] . #'helpful-variable)
         ([remap describe-symbol]   . #'helpful-symbol)
         ([remap describe-key]      . #'helpful-key)

         ;; Suggested bindings from the documentation at
         ;; https://github.com/Wilfred/helpful.

         :map help-map
         ("F" . #'helpful-function)
         ("M-f" . #'helpful-macro)
         ("C" . #'helpful-command)

         :map global-map
         ("C-c C-d" . #'helpful-at-point)))

;; Package `macrostep' provides a facility for interactively expanding
;; Elisp macros.
(use-package macrostep
  :bind (("C-c e" . #'macrostep-expand)))

(use-package org
  :bind (:map org-mode-map

              ;; By default, Org maps C-<up> to
              ;; `org-backward-paragraph' instead of
              ;; `backward-paragraph' (and analogously for C-<down>).
              ;; However, it doesn't do the same remapping for the
              ;; other bindings of `backward-paragraph' (e.g. M-{).
              ;; Here we establish that remapping.
              ([remap backward-paragraph] . #'org-backward-paragraph)
              ([remap forward-paragraph] . #'org-forward-paragraph))
  :bind* (;; Add the global keybindings for accessing Org Agenda and
          ;; Org Capture that are recommended in the Org manual.
          ("C-c a" . #'org-agenda)
          ("C-c c" . #'org-capture))
  :config

  ;; If you try to insert a heading in the middle of an entry, don't
  ;; split it in half, but instead insert the new heading after the
  ;; end of the current entry.
  (setq org-insert-heading-respect-content t)

  ;; Make C-a, C-e, and C-k smarter with regard to headline tags.
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)

  ;; Disable `auto-fill-mode', it interferes with my other scripts
  (auto-fill-mode -1)

  ;; When you create a sparse tree and `org-indent-mode' is enabled,
  ;; the highlighting destroys the invisibility added by
  ;; `org-indent-mode'. Therefore, don't highlight when creating a
  ;; sparse tree.
  (setq org-highlight-sparse-tree-matches nil)

  ;; Default org directory
  (setq org-agenda-files '("~/org/"))

  ;; Make it possible to dim or hide blocked tasks in the agenda view.
  (setq org-enforce-todo-dependencies t)

  ;; Hide blocked tasks in the agenda view.
  (setq org-agenda-dim-blocked-tasks 'invisible)

  ;; Open Org Agenda in full screen
  (setq org-agenda-window-setup 'only-window)

  ;; Specific files to show for custom agenda view
  (setq org-agenda-custom-commands
        '(("n" "Agenda and all TODOs"
           ((agenda "" nil)
            (todo "TODO|DOING"
                  ((org-agenda-files
                    '("~/org/tasklist.org" "~/org/SP.org")))))
           nil)))

  ;; Set todo keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
          (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)"))
        org-todo-keyword-faces '(("HANGUP" . warning)
                                 ("❓" . warning))
        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success))

        org-tags-column -80
        org-log-done 'time
        org-catch-invisible-edits 'smart
        org-startup-indented t
	org-ellipsis " ▾"
        org-pretty-entities nil
        org-hide-emphasis-markers t)

  ;; Refile targets
  (setq org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets (quote (("tasklist.org" :maxlevel . 3)
                                   ("scheduled.org" :maxlevel . 3)
                                   ("someday.org" :maxlevel . 3)
                                   ("gtd.org" :maxlevel . 3)
                                   ("SP.org" :maxlevel . 3)))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

  ;; Add it after refile
  (advice-add 'org-refile :after
	      (lambda (&rest _)
	        (org-save-all-org-buffers)))

  ;; Capture templates
  (setq org-capture-templates
        `(("i" "Idea" entry (file ,(concat org-directory "/idea.org"))
           "*  %^{Title} %?\n%U\n%a\n")
          ("t" "Todo" entry (file ,(concat org-directory "/gtd.org"))
           "* TODO %?\n%U\n%a\n" :clock-resume t)
          ("n" "Note" entry (file ,(concat org-directory "/note.org"))
           "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
          ("j" "Journal" entry (file, (concat org-directory "/journal.org"))
           "*  %^{Title} %?\n%U\n%a\n" :clock-resume t)
	  ("b" "Book" entry (file, (concat org-directory "/book.org"))
	   "* Topic: %^{Topic} -- Author: %^{Author} %? Added: %U")))


  ;; Custom macros for LA
  (fset 'copy-previous-analysis
	(kmacro-lambda-form [?\C-n ?\C-c ?\C-p ?\M-f ?\C-f ?\C-\M-@ ?\M-w ?\C-r ?\C-y ?\C-r return ?\M-x ?o ?r ?g ?- ?s ?h ?o ?w ?- ?s ?u ?b ?t ?r ?e ?e return ?\C-c ?\C-n ?\M-b ?\M-f ?\C-  ?\C-r ?- ?- ?- return ?\C-a ?\M-w ?\C-u ?\C-  ?\C-u ?\C-  ?\M-x ?o ?r ?g ?- ?s ?h ?o ?w ?- ?s ?u ?b ?t ?r ?e ?e return ?\C-c ?\C-n ?\C-o ?\C-y ?\C-c ?\C-t ?d] 0 "%d"))


  (keymap-set org-mode-map "C-x C-z a" #'copy-previous-analysis)


  (fset 'append-analysis
        (kmacro-lambda-form [?\C-n ?\C-c ?\C-p ?\C-c ?\C-n ?\C-  ?\C-r ?- ?- ?- return ?\C-a ?\M-w ?\C-c ?\C-p ?\M-f ?\C-f ?\C-\M-@ ?\M-w ?\M-> ?\C-r ?\C-y return ?\C-c ?\C-n ?\C-o ?\C-y ?\M-y] 0 "%d"))

  (keymap-set org-mode-map "C-x C-z A" #'append-analysis)


  (fset 'goto-previous-analysis
        (kmacro-lambda-form [?\C-n ?\C-c ?\C-p ?\M-f ?\C-f ?\C-\M-@ ?\M-w ?\C-r ?\C-y ?\C-r return ?\M-x ?o ?r ?g ?- ?s ?h ?o ?w ?- ?s ?u ?b ?t ?r ?e ?e return] 0 "%d"))

  (keymap-set org-mode-map "C-x C-z s" #'goto-previous-analysis)

  (fset 'TR
        (kmacro-lambda-form [?\C-a escape ?\C-  ?\M-w escape ?\C-  ?\C-c ?\C-l ?h ?t ?t ?p ?s ?: ?/ ?/ ?m ?h ?w ?e ?b ?. ?e ?r ?i ?c ?s ?s ?o ?n ?. ?s ?e ?/ ?T ?R ?E ?d ?i ?t ?W ?e ?b ?/ ?f ?a ?c ?e ?s ?/ ?o ?o ?/ ?o ?b ?j ?e ?c ?t ?. ?x ?h ?t ?m ?l ?? ?e ?r ?i ?r ?e ?f ?= ?\C-y return return] 0 "%d"))

  (keymap-set org-mode-map "C-x C-z T" #'TR)

  (fset 'ticket
        (kmacro-lambda-form [?\C-a escape ?\C-  ?\C-w ?\C-c ?\C-l ?h ?t ?t ?p ?s ?: ?/ ?/ ?e ?t ?e ?a ?m ?p ?r ?o ?j ?e ?c ?t ?. ?i ?n ?t ?e ?r ?n ?a ?l ?. ?e ?r ?i ?c ?s ?s ?o ?n ?. ?c ?o ?m ?/ ?b ?r ?o ?w ?s ?e ?/ ?P backspace ?\C-y return ?\C-y return] 0 "%d"))

  (keymap-set org-mode-map "C-x C-z t" #'ticket)

  (fset 'posijediti
	(kmacro-lambda-form [?\C-a escape ?\C-f ?\C-f ?\C-f ?\C-f ?= ?\C-e ?= ?\C-n] 0 "%d"))

  (keymap-set org-mode-map "C-x C-z p" #'posijediti))

(use-package dired
  :ensure nil
  :init
  ;; Search DIR recursively for files matching the globbing pattern PATTERN
  (keymap-global-set "C-<" #'find-name-dired)
  ;; Find files in DIR that contain matches for REGEXP
  (keymap-global-set "C->" #'find-grep-dired)
  :bind (:map dired-mode-map
              ("J" . dired-up-directory))
  :config
    ;; Disable the prompt about whether I want to kill the Dired buffer
  ;; for a deleted directory. Of course I do! It's just a Dired
  ;; buffer, after all. Note that this variable, for reasons unknown
  ;; to me, is defined in `dired-x', but only affects the behavior of
  ;; functions defined in `dired'.
  (setq dired-clean-confirm-killing-deleted-buffers nil)

  ;; Instantly revert Dired buffers on re-visiting them, with no
  ;; message. (A message is shown if insta-revert is either disabled
  ;; or determined dynamically by setting this variable to a
  ;; function.)
  (setq dired-auto-revert-buffer t)

  ;; Commands which ask for a destination directory, such as those
  ;; which copy and rename files or create links for them, try to
  ;; guess the default target directory for the operation. Normally,
  ;; they suggest the Dired buffer’s default directory, but if the
  ;; option dired-dwim-target is non-nil, and if there is another
  ;; Dired buffer displayed in some window, that other buffer’s
  ;; directory is suggested instead.
  (setq dired-dwim-target t)

  ;; Prevent dired to open new buffer for every ensubdirectory
  (setf dired-kill-when-opening-new-dired-buffer t)

  ;; Human readable size
  (setq dired-listing-switches "-alh")

  (setq dired-recursive-copies 'always
	dired-recursive-deletes 'always
	dired-make-directory-clickable t
	dired-mouse-drag-files t)
 ;; In Emacs 29 there is a binding for `repeat-mode' which let you
 ;; repeat C-x C-j just by following it up with j.  For me, this is a
 ;; problem as j calls `dired-goto-file', which I often use.
 (define-key dired-jump-map (kbd "j") nil))

;; Comparing files and buffers, and finding differences.
(use-package ediff
  :ensure nil
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

;; Feature `smerge-mode' provides an interactive mode for visualizing
;; and resolving Git merge conflicts.
(use-package smerge-mode)

;; Package `transient' is the interface used by Magit to display
;; popups.
(use-package transient
  :config

  ;; Allow using `q' to quit out of popups, in addition to `C-g'. See
  ;; <https://magit.vc/manual/transient.html#Why-does-q-not-quit-popups-anymore_003f>
  ;; for discussion.
  (transient-bind-q-to-quit))

;; Package `magit' provides a full graphical interface for Git within
;; Emacs.
(use-package magit
  :bind (;; This is the primary entry point for Magit. Binding to C-x
         ;; g is recommended in the manual:
         ;; https://magit.vc/manual/magit.html#Getting-Started
         ("C-x g" . #'magit-status)
         ;; Alternate transient entry point; binding recommended in
         ;; <https://magit.vc/manual/magit.html#Transient-Commands>.
         ("C-x M-g" . #'magit-dispatch)
         ;; Completing the trio of bindings in `magit-file-mode-map'.
         ("C-c M-g" . #'magit-file-dispatch))

  :init

  ;; Suppress the message we get about "Turning on
  ;; magit-auto-revert-mode" when loading Magit.
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))

  :config

  ;; Don't try to save unsaved buffers when using Magit. We know
  ;; perfectly well that we need to save our buffers if we want Magit
  ;; to see them.
  (setq magit-save-repository-buffers nil)

  (transient-append-suffix
    'magit-merge "-n"
    '("-u" "Allow unrelated" "--allow-unrelated-histories"))

  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))

  (transient-append-suffix 'magit-fetch "-t"
    '("-u" "Unshallow" "--unshallow"))

  ;; Show word-granularity differences within diff hunks
  (setq magit-diff-refine-hunk t))

;; Package `git-gutter' adds a column to the left-hand side of each
;; window, showing which lines have been added, removed, or modified
;; since the last Git commit.
(use-package git-gutter
  :commands (git-gutter:previous-hunk
             git-gutter:next-hunk
             git-gutter:end-of-hunk
             git-gutter:revert-hunk)
  :init

  ;; Disable in Org mode, as per
  ;; <https://github.com/syl20bnr/spacemacs/issues/10555> and
  ;; <https://github.com/syohex/emacs-git-gutter/issues/24>.
  ;; Apparently, the mode-enabling function for global minor modes
  ;; gets called for new buffers while they are still in
  ;; `fundamental-mode', before a major mode has been assigned. I
  ;; don't know why this is the case, but adding `fundamental-mode'
  ;; here fixes the issue.
  (setq git-gutter:disabled-modes '(fundamental-mode org-mode))

  (global-git-gutter-mode +1)
  :config

  ;; Don't prompt when reverting hunk.
  (setq git-gutter:ask-p nil))

;; Package `rg' just provides an interactive command `rg' to run the
;; search tool of the same name.
(use-package rg
  :bind (("M-s R" . #'rg))
  :config
  (rg-enable-default-bindings))

;; Feature `browse-url' provides commands for opening URLs in
;; browsers.
(use-package browse-url
  :bind ("C-c C-o" . #'browse-url-at-point))

;; Explicitly set the mark
(defun push-mark-no-activate ()
  "Pushes 'point' to 'mark-ring' and does not activate the
region. Equivalent to \\[set-mark-command] when
\\[transient-mark-mode] is disabled."
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(keymap-global-set "C-`" #'push-mark-no-activate)
(defun jump-to-mark ()
  "Jumps to the local mark, respecting the 'mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix
argument."
  (interactive)
  (set-mark-command 1))
(keymap-global-set "M-`" #'jump-to-mark)
(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(keymap-set global-map "C-x C-x" #'exchange-point-and-mark-no-activate)


;; cooking for pulse line
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))
(dolist (command '(scroll-up-command scroll-down-command
                                     recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))

;; treesit is part of Emacs 29, but grammars still need to be downloaded and installed.
;; This snippet was taken from:
;; https://www.nathanfurnal.xyz/posts/building-tree-sitter-langs-emacs/
(use-package treesit
  :ensure nil
  :commands (treesit-install-language-grammar nf/treesit-install-all-languages)
  :init
  (setq treesit-language-source-alist
   '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
     (c . ("https://github.com/tree-sitter/tree-sitter-c"))
     (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
     (css . ("https://github.com/tree-sitter/tree-sitter-css"))
     (go . ("https://github.com/tree-sitter/tree-sitter-go"))
     (html . ("https://github.com/tree-sitter/tree-sitter-html"))
     (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
     (java . ("https://github.com/tree-sitter/tree-sitter-java"))
     (json . ("https://github.com/tree-sitter/tree-sitter-json"))
     (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
     (make . ("https://github.com/alemuller/tree-sitter-make"))
     (python . ("https://github.com/tree-sitter/tree-sitter-python"))
     (php . ("https://github.com/tree-sitter/tree-sitter-php"))
     (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
     (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
     (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
     (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
     (zig . ("https://github.com/GrayJack/tree-sitter-zig"))))

  (add-to-list 'major-mode-remap-alist
             '(java-mode . java-ts-mode))
  :config
  (defun nf/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	      (treesit-install-language-grammar lang)
	      (message "`%s' parser was installed." lang)
	      (sit-for 0.75)))))

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l o" . eglot-code-actions-organize-imports)
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format))
  :config
  (advice-add 'eglot-completion-at-point :around  #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible)

  (setq completion-category-overrides '((eglot (styles orderless))))
  (setq completion-category-defaults nil)

  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref nil)
  (eglot-report-progress nil)
  (eglot-ignored-server-capabilities
   '(:hoverProvider
     ;; :documentHighlightProvider
     :documentFormattingProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider
     :colorProvider
     :foldingRangeProvider))
  ;; (eglot-stay-out-of '(yasnippet))
  )

(use-package jarchive
  :after eglot
  :config
  (jarchive-mode))

(use-package eglot-booster
	:after eglot
	:config	(eglot-booster-mode))

(use-package clojure
  :init
  (eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
     (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)))

  ;; Wrap when navigating history.
  (setq cider-repl-wrap-history t)

  ;; Use clojure mode for other extensions
  (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

  ;; provides minibuffer documentation for the code you're typing into the repl
  (add-hook 'cider-mode-hook 'eldoc-mode)

  ;; go right to the REPL buffer when it's finished connecting
  (setq cider-repl-pop-to-buffer-on-connect t)

  ;; When there's a cider error, show its buffer and switch to it
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)

  ;; This is useful for working with camel-case tokens, like names of
  ;; Java classes (e.g. JavaClassName)
  (add-hook 'clojure-mode-hook 'subword-mode)

  :config
  ;; key bindings
  ;; these help me out with the way I usually develop web apps
  (defun cider-start-http-server ()
    (interactive)
    (cider-load-current-buffer)
    (let ((ns (cider-current-ns)))
      (cider-repl-set-ns ns)
      (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
      (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))

  (defun cider-refresh ()
    (interactive)
    (cider-interactive-eval (format "(user/reset)")))

  (defun cider-user-ns ()
    (interactive)
    (cider-repl-set-ns "user")))

(use-package elfeed
  :bind
  ("C-x w w" . elfeed)
  :config
  (setq elfeed-feeds
	'(("http://nullprogram.com/feed/" blog emacs)
          "http://www.50ply.com/atom.xml"  ; no autotagging
	  "https://planet.emacslife.com/atom.xml"
          ("http://nedroid.com/feed/" webcomic)
	  ("https://cestlaz.github.io/rss.xml" Zamansky))))

(use-package emacs
  :bind
  ("M-W" . ffap-copy-string-as-kill)
  ("s-w" . copy-whole-line)
  ("s-<tab>" . crux-switch-to-previous-buffer)
  ("s-k" . kill-paragraph)
  :config
  (defun copy-whole-line ()
    "Copy whole line without whitespace at the `beginning-of-line'"
    (interactive)
    (save-excursion
      (back-to-indentation)
      (kill-ring-save (point)
                      (line-end-position))))
  (defun crux-switch-to-previous-buffer ()
    "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))
  :init
  ;; Emacs provides Do-What-I-Mean versions of various
  ;; editing commands: They act on the region when the region is active,
  ;; and on an appropriate semantic unit otherwise. Replace "upcase-word"
  ;; and "downcase-word" with "upcase-dwim" and "downcase-dwim"
  ;; respectively, and you can safely eject the bindings for
  ;; "upcase-region" and "downcase-region".
  (keymap-global-set "M-u" #'upcase-dwim)
  (keymap-global-set "M-l" #'downcase-dwim)
  (keymap-global-set "M-c" #'capitalize-dwim)
  ;; Exchange any two non-overlapping regions in a buffer
  (keymap-global-set "C-x C-M-t" #'transpose-regions)

  ;; Personal keymap
  ;; Replace suspend-frame which this keymap since suspend-frame is
  ;; never useful and always unexpected and by accident.
  (progn
    ;; define key sequence
    (define-prefix-command 'my-keymap)
    (keymap-global-set "C-z" #'my-keymap)
    (keymap-set my-keymap "t s" #'(lambda () (interactive) (shell)
                                    (delete-other-windows)
                                    (end-of-buffer)))
					; populate with personal keybinding sequences
    )

  ;; Maybe useful
  (bind-key "s-z" #'copy-from-above-command)
  (keymap-global-set "C-S-h" #'backward-kill-word)

  ;; If you have something on the system clipboard, and then kill
  ;; something in Emacs, then by default whatever you had on the system
  ;; clipboard is gone and there is no way to get it back. Setting the
  ;; following option makes it so that when you kill something in Emacs,
  ;; whatever was previously on the system clipboard is pushed into the
  ;; kill ring. This way, you can paste it with `yank-pop'.
  (setq save-interprogram-paste-before-kill t)

  ;; I hardly ever want to to kill buffer other then the one I'm in when
  ;; I call this function.
  (keymap-global-set "C-x k" #'kill-current-buffer))

(use-package codium
  :vc (:url https://github.com/Exafunction/codeium.el)
  :init
  (setq-default codeium-enterprise t)
  (setq-default codeium-portal-url
		"https://codingbuddy.onprem.gic.ericsson.se")
  (setq-default codeium-api-url
		"https://codingbuddy.onprem.gic.ericsson.se/_route/api_server")
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point))


