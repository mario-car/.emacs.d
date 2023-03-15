;; -*- lexical-binding: t -*-

;; Turn F20 keysim into super modifier
;; On Linux, F20 is automatically interpreted as "Super" modifier key.
;; This hack is only needed on MS Windows
(when (eq system-type 'windows-nt)
  (global-set-key (kbd "<f20>") nil) ;; bound to clipboard-kill-region by default
  (define-key function-key-map (kbd "<f20>") 'event-apply-super-modifier)
  (setq w32-pass-rwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)
  (w32-register-hot-key [s-]))

;; Turn <apps> (menu) into hyper modifier
;; On Linux rebind <apps> to Hyper via xmodmap
(when (eq system-type 'windows-nt)
  (setq w32-pass-apps-to-system nil)
  (setq w32-apps-modifier 'hyper)) ; Menu/App key

;; Prevent Custom from modifying this file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;; Some basic settings
(setq use-short-answers t)
(put 'overwrite-mode 'disabled t)


;; set up straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


(defmacro radian-defadvice (name arglist where place docstring &rest body)
  "Define an advice called NAME and add it to a function.
ARGLIST is as in `defun'. WHERE is a keyword as passed to
`advice-add', and PLACE is the function to which to add the
advice, like in `advice-add'. PLACE should be sharp-quoted.
DOCSTRING and BODY are as in `defun'."
  (declare (indent 2)
           (doc-string 5))
  (unless (stringp docstring)
    (error "Radian: advice `%S' not documented'" name))
  (unless (and (listp place)
               (= 2 (length place))
               (eq (nth 0 place) 'function)
               (symbolp (nth 1 place)))
    (error "Radian: advice `%S' does not sharp-quote place `%S'" name place))
  `(progn
     ;; You'd think I would put an `eval-and-compile' around this. It
     ;; turns out that doing so breaks the ability of
     ;; `elisp-completion-at-point' to complete on function arguments
     ;; to the advice. I know, right? Apparently this is because the
     ;; code that gets the list of lexically bound symbols at point
     ;; tries to `macroexpand-all', and apparently macroexpanding
     ;; `eval-and-compile' goes ahead and evals the thing and returns
     ;; only the function symbol. No good. But the compiler does still
     ;; want to know the function is defined (this is a Gilardi
     ;; scenario), so we pacify it by `eval-when-compile'ing something
     ;; similar (see below).
     (defun ,name ,arglist
       ,(let ((article (if (string-match-p "^:[aeiou]" (symbol-name where))
                           "an"
                         "a")))
          (format "%s\n\nThis is %s `%S' advice for\n`%S'."
                  docstring article where
                  (if (and (listp place)
                           (memq (car place) ''function))
                      (cadr place)
                    place)))
       ,@body)
     (eval-when-compile
       (declare-function ,name nil))
     (advice-add ,place ',where #',name)
     ',name))

(defmacro radian-defhook (name arglist hooks docstring &rest body)
  "Define a function called NAME and add it to a hook.
ARGLIST is as in `defun'. HOOKS is a list of hooks to which to
add the function, or just a single hook. DOCSTRING and BODY are
as in `defun'."
  (declare (indent 2)
           (doc-string 4))
  (unless (listp hooks)
    (setq hooks (list hooks)))
  (dolist (hook hooks)
    (unless (string-match-p "-\\(hook\\|functions\\)$" (symbol-name hook))
      (error "Symbol `%S' is not a hook" hook)))
  (unless (stringp docstring)
    (error "Radian: no docstring provided for `radian-defhook'"))
  (let ((hooks-str (format "`%S'" (car hooks))))
    (dolist (hook (cdr hooks))
      (setq hooks-str (format "%s\nand `%S'" hooks-str hook)))
    `(progn
       (defun ,name ,arglist
         ,(format "%s\n\nThis function is for use in %s."
                  docstring hooks-str)
         ,@body)
       (dolist (hook ',hooks)
         (add-hook hook ',name)))))

(defmacro radian-flet (bindings &rest body)
  "Temporarily override function definitions using `cl-letf*'.
BINDINGS are composed of `defun'-ish forms. NAME is the function
to override. It has access to the original function as a
lexically bound variable by the same name, for use with
`funcall'. ARGLIST and BODY are as in `defun'.

\(fn ((defun NAME ARGLIST &rest BODY) ...) BODY...)"
  (declare (indent defun))
  `(cl-letf* (,@(cl-mapcan
                 (lambda (binding)
                   (when (memq (car binding) '(defun lambda))
                     (setq binding (cdr binding)))
                   (cl-destructuring-bind (name arglist &rest body) binding
                     (list
                      `(,name (symbol-function #',name))
                      `((symbol-function #',name)
                        (lambda ,arglist
                          ,@body)))))
                 bindings))
     ,@body))

;;; Startup optimizations

;; Disable frequency of GC. This helps performance both during init
;; and after init. Value is in bytes so this is 100MB, as suggested in
;; <https://github.com/emacs-lsp/lsp-mode#performance>.
(setq gc-cons-threshold (* 100 1024 1024))

;;; Set up package management
;;;; straight.el

;;;; use-package

;; Package `use-package' provides a handy macro by the same name which
;; is essentially a wrapper around `with-eval-after-load' with a lot
;; of handy syntactic sugar and useful features.
(straight-use-package 'use-package)

;; When configuring a feature with `use-package', also tell
;; straight.el to install a package of the same name, unless otherwise
;; specified using the `:straight' keyword.
(setq straight-use-package-by-default t)

;; Tell `use-package' to always load features lazily unless told
;; otherwise. It's nicer to have this kind of thing be deterministic:
;; if `:demand' is present, the loading is eager; otherwise, the
;; loading is lazy. See
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading.
(setq use-package-always-defer t)


;; Package `blackout' provides a convenient function for customizing
;; mode lighters. It supports both major and minor modes with the same
;; interface, and includes `use-package' integration. The features are
;; a strict superset of those provided by similar packages `diminish',
;; `delight', and `dim'.
(use-package blackout
  :straight (:host github :repo "radian-software/blackout")
  :demand t)

;;; Configure ~/.emacs.d paths

;; Package `no-littering' changes the default paths for lots of
;; different packages, with the net result that the ~/.emacs.d folder
;; is much more clean and organized.
(use-package no-littering
  :demand t)

;;; Prevent Emacs-provided Org from being loaded

;; Our real configuration for Org comes much later. Doing this now
;; means that if any packages that are installed in the meantime
;; depend on Org, they will not accidentally cause the Emacs-provided
;; (outdated and duplicated) version of Org to be loaded before the
;; real one is registered.

(straight-register-package 'org)
(straight-register-package 'org-contrib)

;;; el-patch

;; Package `el-patch' provides a way to override the definition of an
;; internal function from another package by providing an s-expression
;; based diff which can later be validated to ensure that the upstream
;; definition has not changed.
(use-package el-patch)

;; Only needed at compile time, thanks to Jon
;; <https://github.com/radian-software/el-patch/pull/11>.
(eval-when-compile
  (require 'el-patch))

;;; Keybindings

;; Package `bind-key' provides a macro by the same name (along with
;; `bind-key*' and `unbind-key') which provides a much prettier API
;; for manipulating keymaps than `define-key' and `global-set-key' do.
;; It's also the same API that `:bind' and similar keywords in
;; `use-package' use.
(use-package bind-key
  :demand t)

;; Modern versions of Emacs provide Do-What-I-Mean versions of various
;; editing commands: They act on the region when the region is active,
;; and on an appropriate semantic unit otherwise. Replace "upcase-word"
;; and "downcase-word" with "upcase-dwim" and "downcase-dwim"
;; respectively, and you can safely eject the bindings for
;; "upcase-region" and "downcase-region".
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-region)
(global-set-key (kbd "M-c") 'capitalize-dwim)
;; Exchange any two non-overlapping regions in a buffer
(global-set-key (kbd "C-x C-M-t") 'transpose-regions)

;; Personal keymap
;; Replace suspend-frame which this keymap since suspend-frame is
;; never useful and always unexpected and by accident.
(progn
  ;; define key sequence
  (define-prefix-command 'my-keymap)
  (global-set-key (kbd "C-z" ) my-keymap )
  (define-key my-keymap (kbd "t s") 'shell)
  ; populate with personal keybinding sequences
  )

;; Easier to press `repeat' command
(bind-key "<f5>" #'repeat)
;; Maybe useful
(bind-key "s-z" #'copy-from-above-command)


;;;; Clipboard integration

;; If you have something on the system clipboard, and then kill
;; something in Emacs, then by default whatever you had on the system
;; clipboard is gone and there is no way to get it back. Setting the
;; following option makes it so that when you kill something in Emacs,
;; whatever was previously on the system clipboard is pushed into the
;; kill ring. This way, you can paste it with `yank-pop'.
(setq save-interprogram-paste-before-kill t)

;;;; Mouse integration

;; Mouse integration works out of the box in windowed mode but not
;; terminal mode. The following code to fix it was based on
;; <https://stackoverflow.com/a/8859057/3538165>.
(unless (display-graphic-p)

  ;; Enable basic mouse support (click and drag).
  (xterm-mouse-mode t)

  ;; Note that the reason for the next two functions is that
  ;; `scroll-down' and `scroll-up' scroll by a "near full screen"
  ;; by default, whereas we want a single line.

  (eval-and-compile
    (defun radian-scroll-down ()
      "Scroll down one line."
      (interactive)
      (scroll-down 1))

    (defun radian-scroll-up ()
      "Scroll up one line."
      (interactive)
      (scroll-up 1)))

  ;; Enable scrolling with the mouse wheel.
  (bind-key "<mouse-4>" #'radian-scroll-down)
  (bind-key "<mouse-5>" #'radian-scroll-up))

;;; Candidate selection

;; Allow doing a command that requires candidate-selection when you
;; are already in the middle of candidate-selection. Sometimes it's
;; handy!
(setq enable-recursive-minibuffers t)

;; Package `vertico' is an incremental completion and narrowing
;; framework.
(use-package vertico
  ;; :commands vertico-mode
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive
                                ))
  :after minibuffer
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

(use-package vertico-multiform
  :commands vertico-multiform-mode
  :bind (:map vertico-map
              ("M-q" . vertico-multiform-flat)
              ("C-l" . my/vertico-multiform-unobtrusive)
              ("C-M-l" . embark-export))
  :init (vertico-multiform-mode 1)
  :config
  (setq vertico-multiform-categories
         '((file)
           (imenu buffer)
           (consult-location buffer)
           (consult-grep buffer)
           (notmuch-result reverse)
           (minor-mode reverse)
           (reftex-label (:not unobtrusive))
           (citar-reference reverse)
           (xref-location reverse)
           (history reverse)
           (url reverse)
           (consult-compile-error reverse)
           (buffer flat (vertico-cycle . t))
           (t flat)))
   (setq vertico-multiform-commands
         '((tab-bookmark-open reverse)
           (dired-goto-file unobtrusive)
           (affe-find reverse)
           (execute-extended-command)
           (consult-project-buffer flat)
           (consult-dir-maybe reverse)
           (consult-dir reverse)
           (consult-flymake reverse)
           (consult-history reverse)
           (consult-completion-in-region reverse)
           (consult-recoll)
           (completion-at-point reverse)
           (org-roam-node-find reverse)
           (embark-completing-read-prompter reverse)
           (embark-act-with-completing-read reverse)
           (embark-prefix-help-command reverse)
           (embark-bindings reverse)
           (consult-org-heading reverse)
           (consult-dff unobtrusive)
           (xref-find-definitions reverse)
           (my/eshell-previous-matching-input reverse)
           (tmm-menubar reverse)))

   (defun my/vertico-multiform-unobtrusive ()
     "Toggle between vertico-unobtrusive and vertico-reverse."
     (interactive)
     (vertico-multiform-vertical 'vertico-reverse-mode)))

(use-package vertico-quick
  :after vertico
  :bind (:map vertico-map
         ("M-i" . vertico-quick-insert)
         ("C-'" . vertico-quick-exit)
         ("C-o" . vertico-quick-embark))
  :config
  (defun vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg))))

(use-package vertico-directory
  :hook (rfn-eshadow-update-overlay vertico-directory-tidy)
  :after vertico
  :bind (:map vertico-map
         ("DEL"   . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word)
         ("C-w"   . vertico-directory-delete-word)
         ("RET"   . vertico-directory-enter)))

(use-package vertico-repeat
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind (("C-x ." . vertico-repeat)
         ("H-."   . vertico-repeat)))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode)
  :config
  (add-to-list 'savehist-additional-variables
               'vertico-repeat-history))

(use-package vertico-buffer
  :after vertico
  ;; :hook (vertico-buffer-mode . vertico-buffer-setup)
  :config
  (setq vertico-buffer-display-action 'display-buffer-reuse-window))

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
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
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

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

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

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  ;; (setq consult-project-root-function
  ;;       (lambda ()
  ;;         (when-let (project (project-current))
  ;;           (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
  )

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
  :ensure t

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
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; Window management

;; I hardly ever want to to kill buffer other then the one I'm in when
;; I call this function.
(global-set-key [remap kill-buffer] 'kill-this-buffer)

;; Feature `winner' provides an undo/redo stack for window
;; configurations, with undo and redo being C-c left and C-c right,
;; respectively. (Actually "redo" doesn't revert a single undo, but
;; rather a whole sequence of them.) For instance, you can use C-x 1
;; to focus on a particular window, then return to your previous
;; layout with C-c left.
(use-package winner
  :demand t
  :config

  (winner-mode +1))

;; Package `transpose-frame' provides simple commands to mirror,
;; rotate, and transpose Emacs windows: `flip-frame', `flop-frame',
;; `transpose-frame', `rotate-frame-clockwise',
;; `rotate-frame-anticlockwise', `rotate-frame'.
(use-package transpose-frame
  :bind* (("C-x C-z R" . #'transpose-frame)
          ("C-x C-z r" . #'rotate-frame-clockwise)
          ("C-x C-z f" . #'flip-frame)
          ("C-x C-z F" . #'flop-frame)))

;; Package `buffer-move' provides simple commands to swap Emacs
;; windows: `buf-move-up', `buf-move-down', `buf-move-left',
;; `buf-move-right'.
(use-package buffer-move
  :bind* (("C-x C-z k" . #'buf-move-up)
          ("C-x C-z j" . #'buf-move-down)
          ("C-x C-z h" . #'buf-move-left)
          ("C-x C-z l" . #'buf-move-right)))

;; Feature `ibuffer' provides a more modern replacement for the
;; `list-buffers' command.
(use-package ibuffer
  :bind (([remap list-buffers] . #'ibuffer)))

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
           display-buffer-pop-up-frame))))

;;; Finding files

;; Follow symlinks when opening files. This has the concrete impact,
;; for instance, that when you edit init.el with M-P e e i and then
;; later do C-x C-f, you will be in the Radian repository instead of
;; your home directory.
(setq find-file-visit-truename t)

;; Disable the warning "X and Y are the same file" which normally
;; appears when you visit a symlinked file by the same name. (Doing
;; this isn't dangerous, as it will just redirect you to the existing
;; buffer.)
(setq find-file-suppress-same-file-warnings t)

;; Feature `saveplace' provides a minor mode for remembering the
;; location of point in each file you visit, and returning it there
;; when you find the file again.
(use-package saveplace
  :demand t
  :config
  (save-place-mode +1))

;; Package `projectile' keeps track of a "project" list, which is
;; automatically added to as you visit Git repositories, Node.js
;; projects, etc. It then provides commands for quickly navigating
;; between and within these projects.
(use-package projectile
  :defer 1
  :bind-keymap* (("C-c p" . projectile-command-map))
  :config

  ;; Use Vertico (via `completing-read') for Projectile instead of
  ;; IDO.
  (setq projectile-completion-system 'default)

  ;; When switching projects, open it in dired root.
  (setq projectile-switch-project-action 'projectile-dired)

  ;; (def-projectile-commander-method ?\C-m
  ;;   "Find file in project."
  ;;   (call-interactively #'find-file))

  ;; Enable the mode again now that we have all the supporting hooks
  ;; and stuff defined.
  (projectile-mode +1)

  ;; (defun radian--projectile-indexing-method-p (method)
  ;;   "Non-nil if METHOD is a safe value for `projectile-indexing-method'."
  ;;   (memq method '(native alien)))

  ;; (put 'projectile-indexing-method 'safe-local-variable
  ;;      #'radian--projectile-indexing-method-p)

  ;; ;; Can't bind M-r because some genius bound ESC. *Never* bind ESC!
  ;; (dolist (key '("C-r" "R"))
  ;;   (bind-key key #'projectile-replace-regexp projectile-command-map))
  (bind-key "R" #'projectile-replace-regexp projectile-command-map)

  :blackout t)

(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master")
  :bind ("C-c P" . consult-projectile))


(defvar radian--dirs-to-delete nil
  "List of directories to try to delete when killing buffer.
This is used to implement the neat feature where if you kill a
new buffer without saving it, then Radian will prompt to see if
you want to also delete the parent directories that were
automatically created.")

(defun radian--advice-find-file-create-directories
    (find-file filename &rest args)
  "Automatically create and delete parent directories of new files.
This advice automatically creates the parent directory (or directories) of
the file being visited, if necessary. It also sets a buffer-local
variable so that the user will be prompted to delete the newly
created directories if they kill the buffer without saving it.

This advice has no effect for remote files.

This is an `:around' advice for `find-file' and similar
functions.

FIND-FILE is the original `find-file'; FILENAME and ARGS are its
arguments."
  (if (file-remote-p filename)
      (apply find-file filename args)
    (let ((orig-filename filename)
          ;; For relative paths where none of the named parent
          ;; directories exist, we might get a nil from
          ;; `file-name-directory' below, which would be bad. Thus we
          ;; expand the path fully.
          (filename (expand-file-name filename))
          ;; The variable `dirs-to-delete' is a list of the
          ;; directories that will be automatically created by
          ;; `make-directory'. We will want to offer to delete these
          ;; directories if the user kills the buffer without saving
          ;; it.
          (dirs-to-delete ()))
      ;; If the file already exists, we don't need to worry about
      ;; creating any directories.
      (unless (file-exists-p filename)
        ;; It's easy to figure out how to invoke `make-directory',
        ;; because it will automatically create all parent
        ;; directories. We just need to ask for the directory
        ;; immediately containing the file to be created.
        (let* ((dir-to-create (file-name-directory filename))
               ;; However, to find the exact set of directories that
               ;; might need to be deleted afterward, we need to
               ;; iterate upward through the directory tree until we
               ;; find a directory that already exists, starting at
               ;; the directory containing the new file.
               (current-dir dir-to-create))
          ;; If the directory containing the new file already exists,
          ;; nothing needs to be created, and therefore nothing needs
          ;; to be destroyed, either.
          (while (not (file-exists-p current-dir))
            ;; Otherwise, we'll add that directory onto the list of
            ;; directories that are going to be created.
            (push current-dir dirs-to-delete)
            ;; Now we iterate upwards one directory. The
            ;; `directory-file-name' function removes the trailing
            ;; slash of the current directory, so that it is viewed as
            ;; a file, and then the `file-name-directory' function
            ;; returns the directory component in that path (which
            ;; means the parent directory).
            (setq current-dir (file-name-directory
                               (directory-file-name current-dir))))
          ;; Only bother trying to create a directory if one does not
          ;; already exist.
          (unless (file-exists-p dir-to-create)
            ;; Make the necessary directory and its parents.
            (make-directory dir-to-create 'parents))))
      ;; Call the original `find-file', now that the directory
      ;; containing the file to found exists. We make sure to preserve
      ;; the return value, so as not to mess up any commands relying
      ;; on it.
      (prog1 (apply find-file orig-filename args)
        ;; If there are directories we want to offer to delete later,
        ;; we have more to do.
        (when dirs-to-delete
          ;; Since we already called `find-file', we're now in the
          ;; buffer for the new file. That means we can transfer the
          ;; list of directories to possibly delete later into a
          ;; buffer-local variable. But we pushed new entries onto the
          ;; beginning of `dirs-to-delete', so now we have to reverse
          ;; it (in order to later offer to delete directories from
          ;; innermost to outermost).
          (setq-local radian--dirs-to-delete (reverse dirs-to-delete))
          ;; Now we add a buffer-local hook to offer to delete those
          ;; directories when the buffer is killed, but only if it's
          ;; appropriate to do so (for instance, only if the
          ;; directories still exist and the file still doesn't
          ;; exist).
          (add-hook 'kill-buffer-hook
                    #'radian--kill-buffer-delete-directory-if-appropriate
                    'append 'local)
          ;; The above hook removes itself when it is run, but that
          ;; will only happen when the buffer is killed (which might
          ;; never happen). Just for cleanliness, we automatically
          ;; remove it when the buffer is saved. This hook also
          ;; removes itself when run, in addition to removing the
          ;; above hook.
          (add-hook 'after-save-hook
                    #'radian--remove-kill-buffer-delete-directory-hook
                    'append 'local))))))

(defun radian--kill-buffer-delete-directory-if-appropriate ()
  "Delete parent directories if appropriate.
This is a function for `kill-buffer-hook'. If
`radian--advice-find-file-create-directories' created the
directory containing the file for the current buffer
automatically, then offer to delete it. Otherwise, do nothing.
Also clean up related hooks."
  (when (and
         ;; Stop if the local variables have been killed.
         (boundp 'radian--dirs-to-delete)
         ;; Stop if there aren't any directories to delete (shouldn't
         ;; happen).
         radian--dirs-to-delete
         ;; Stop if `radian--dirs-to-delete' somehow got set to
         ;; something other than a list (shouldn't happen).
         (listp radian--dirs-to-delete)
         ;; Stop if the current buffer doesn't represent a
         ;; file (shouldn't happen).
         buffer-file-name
         ;; Stop if the buffer has been saved, so that the file
         ;; actually exists now. This might happen if the buffer were
         ;; saved without `after-save-hook' running, or if the
         ;; `find-file'-like function called was `write-file'.
         (not (file-exists-p buffer-file-name)))
    (cl-dolist (dir-to-delete radian--dirs-to-delete)
      ;; Ignore any directories that no longer exist or are malformed.
      ;; We don't return immediately if there's a nonexistent
      ;; directory, because it might still be useful to offer to
      ;; delete other (parent) directories that should be deleted. But
      ;; this is an edge case.
      (when (and (stringp dir-to-delete)
                 (file-exists-p dir-to-delete))
        ;; Only delete a directory if the user is OK with it.
        (if (y-or-n-p (format "Also delete directory `%s'? "
                              ;; The `directory-file-name' function
                              ;; removes the trailing slash.
                              (directory-file-name dir-to-delete)))
            (delete-directory dir-to-delete)
          ;; If the user doesn't want to delete a directory, then they
          ;; obviously don't want to delete any of its parent
          ;; directories, either.
          (cl-return)))))
  ;; It shouldn't be necessary to remove this hook, since the buffer
  ;; is getting killed anyway, but just in case...
  (radian--remove-kill-buffer-delete-directory-hook))

(defun radian--remove-kill-buffer-delete-directory-hook ()
  "Clean up directory-deletion hooks, if necessary.
This is a function for `after-save-hook'. Remove
`radian--kill-buffer-delete-directory-if-appropriate' from
`kill-buffer-hook', and also remove this function from
`after-save-hook'."
  (remove-hook 'kill-buffer-hook
               #'radian--kill-buffer-delete-directory-if-appropriate
               'local)
  (remove-hook 'after-save-hook
               #'radian--remove-kill-buffer-delete-directory-hook
               'local))

(dolist (fun '(find-file           ; C-x C-f
               find-alternate-file ; C-x C-v
               write-file          ; C-x C-w
               ))
  (advice-add fun :around #'radian--advice-find-file-create-directories))

;; Feature `auth-source' reads and writes secrets from files like
;; ~/.netrc for TRAMP and related packages, so for example you can
;; avoid having to type in a particular host's password every time.
(use-package auth-source
  :config

  (defvar radian--auth-source-blacklist-file
    (no-littering-expand-var-file-name "auth-source/blacklist.el")
    "File to store `auth-source' user blacklist.
The contents are a list of MD5 hashes, one for each potential
password that the user has decided not to save.")

  (radian-defadvice radian--advice-auth-source-persist-blacklist
      (func file add)
    :around #'auth-source-netrc-saver
    "Allow user to permanently disable prompt to save credentials."
    (let* ((key (format "%s %s" file (rfc2104-hash 'md5 64 16 file add)))
           (blacklist
            (ignore-errors
              (with-temp-buffer
                (insert-file-contents radian--auth-source-blacklist-file)
                (read (current-buffer))))))
      (unless (listp blacklist)
        (setq blacklist nil))
      (if (member key blacklist)
          ?n
        (radian-flet ((defun auth-source-read-char-choice (prompt choices)
                        (let ((choice (funcall auth-source-read-char-choice
                                               prompt choices)))
                          (when (= choice ?N)
                            (push key blacklist)
                            (make-directory
                             (file-name-directory
                              radian--auth-source-blacklist-file)
                             'parents)
                            (with-temp-file radian--auth-source-blacklist-file
                              (print blacklist (current-buffer)))
                            (setq choice ?n))
                          choice)))
          (funcall func file add))))))

;;; Saving files

;; Don't make backup files.
(setq make-backup-files nil)

;; Don't make autosave files.
(setq auto-save-default nil)

;; Don't make lockfiles.
(setq create-lockfiles nil)

(defun radian-set-executable-permission (allowed)
  "Enable or disable executable permission on the current file.
If ALLOWED is non-nil, enable permission; otherwise, disable
permission."
  (interactive (list (not current-prefix-arg)))
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file"))
  (with-demoted-errors "Could not set permissions: %S"
    (set-file-modes buffer-file-name (file-modes-symbolic-to-number
                                      (if allowed
                                          "+x"
                                        "-x")
                                      (file-modes buffer-file-name)))
    (message "Executable permission %s"
             (if allowed "enabled" "disabled"))))

(bind-key* "s-x" #'radian-set-executable-permission)

;;; Editing
;;;; Text formatting

(add-to-list 'safe-local-variable-values '(auto-fill-function . nil))

(add-to-list 'safe-local-eval-forms '(visual-line-mode +1))

(blackout 'visual-line-mode)

;; When region is active, make `capitalize-word' and friends act on
;; it.
(bind-key "M-c" #'capitalize-dwim)
(bind-key "M-l" #'downcase-dwim)
(bind-key "M-u" #'upcase-dwim)

;; When filling paragraphs, assume that sentences end with one space
;; rather than two.
(setq sentence-end-double-space nil)

;; Trigger auto-fill after punctutation characters, not just
;; whitespace.
(mapc
 (lambda (c)
   (set-char-table-range auto-fill-chars c t))
 "!-=+]};:'\",.?")


(blackout 'auto-fill-mode)

(defun radian--do-auto-fill ()
  "Replacement for `do-auto-fill' that respects `normal-auto-fill-function'.
The reason we need this is that in order to enable auto-fill
globally, we are supposed to set the default value of variable
`auto-fill-function'. However, some major modes set
`normal-auto-fill-function' (itself normally set to
`do-auto-fill', which is what we generally set the default value
of variable `auto-fill-function' to), expecting `auto-fill-mode'
to be enabled afterwards (which copies the value of
`normal-auto-fill-function' into variable `auto-fill-function').
However, since we enable auto-fill globally by means of setting
variable `auto-fill-function' directly, this setting gets lost.
The workaround is to set variable `auto-fill-function' globally
to a function which looks up the value of
`normal-auto-fill-function' \(generally just `do-auto-fill') and
calls that. This is a slight inversion of the usual flow of
control and might make you slightly uncomfortable, but we'll just
have to live with it :3"
  (funcall normal-auto-fill-function))

;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Turning-on-auto_002dfill-by-default.html
(setq-default auto-fill-function #'radian--do-auto-fill)

;; Determine text fill prefix from major mode indentation rules,
;; except in text modes. This shouldn't be necessary, but sometimes
;; the adaptive fill heuristics can mess up major modes (e.g. I've run
;; into trouble with `svelte-mode').

(setq-default adaptive-fill-mode nil)

(radian-defhook radian--adaptive-fill-enable ()
  text-mode-hook
  "Re-enable `adaptive-fill-mode' in `text-mode' and derived."
  (setq-local adaptive-fill-mode t))

(radian-defhook radian--adaptive-fill-disable ()
  sgml-mode-hook
  "Re-disable `adaptive-fill-mode' for `sgml-mode' and derived.
Apparently, such modes are derived from `text-mode', even though
they are definitely programming-oriented."
  (setq-local adaptive-fill-mode nil))

(define-minor-mode radian-fix-whitespace-mode
  "Minor mode to automatically fix whitespace on save.
If enabled, then saving the buffer deletes all trailing
whitespace and ensures that the file ends with exactly one
newline."
  :after-hook
  (if radian-fix-whitespace-mode
      (progn
        (setq require-final-newline t)
        (add-hook 'before-save-hook #'delete-trailing-whitespace nil 'local))
    (setq require-final-newline nil)
    (remove-hook 'before-save-hook #'delete-trailing-whitespace 'local)))

(define-globalized-minor-mode radian-fix-whitespace-global-mode
  radian-fix-whitespace-mode radian-fix-whitespace-mode)

(radian-fix-whitespace-global-mode +1)

(put 'radian-fix-whitespace-mode 'safe-local-variable #'booleanp)

;; Feature `whitespace' provides a minor mode for highlighting
;; whitespace in various special ways.
(use-package whitespace
  :init

  (define-minor-mode radian-highlight-long-lines-mode
    "Minor mode for highlighting long lines."
    :after-hook
    (if radian-highlight-long-lines-mode
        (progn
          (setq-local whitespace-style '(face lines-tail))
          (setq-local whitespace-line-column 79)
          (whitespace-mode +1))
      (whitespace-mode -1)
      (kill-local-variable 'whitespace-style)
      (kill-local-variable 'whitespace-line-column)))

  :blackout t)

;; Feature `outline' provides major and minor modes for collapsing
;; sections of a buffer into an outline-like format.
(use-package outline
  :demand t
  :config

  (define-globalized-minor-mode global-outline-minor-mode
    outline-minor-mode outline-minor-mode)

  (global-outline-minor-mode +1)

  :blackout outline-minor-mode)

;;;; Kill and yank

(radian-defadvice radian--advice-stop-kill-at-whitespace
    (kill-line &rest args)
  :around #'kill-line
  "Prevent `kill-line' from killing through whitespace to a newline.
This affects the case where you press \\[kill-line] when point is
followed by some whitespace and then a newline. Without this
advice, \\[kill-line] will kill both the whitespace and the
newline, which is inconsistent with its behavior when the
whitespace is replaced with non-whitespace. With this advice,
\\[kill-line] will kill just the whitespace, and another
invocation will kill the newline."
  (let ((show-trailing-whitespace t))
    (apply kill-line args)))

;; Eliminate duplicates in the kill ring. That is, if you kill the
;; same thing twice, you won't have to use M-y twice to get past it to
;; older entries in the kill ring.
(setq kill-do-not-save-duplicates t)

(radian-defadvice radian--advice-disallow-password-copying (func &rest args)
  :around #'read-passwd
  "Don't allow copying a password to the kill ring."
  (cl-letf (((symbol-function #'kill-region)
             (lambda (beg end &optional region)
               (if region
                   (delete-region (region-beginning) (region-end))
                 (delete-region beg end)))))
    (apply func args)))

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

;; Custom function to copy whole line.
;; It will copy from the first character on the line until the end of
;; line. It will ignore whitespace at the beginning of line.
(defun copy-whole-line ()
  "Copy whole line"
  (interactive)
  (save-excursion
    (back-to-indentation)
    (kill-ring-save (point)
                    (line-end-position))))
(bind-key "s-w" #'copy-whole-line)
(bind-key "M-W" #'ffap-copy-string-as-kill)

;; Improved zap-to-char function
;; Let's you select to which character should it kill
(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

;;;; Undo/redo

;; Feature `warnings' allows us to enable and disable warnings.
(use-package warnings
  :config

  ;; Ignore the warning we get when a huge buffer is reverted and the
  ;; undo information is too large to be recorded.
  (add-to-list 'warning-suppress-log-types '(undo discard-info)))

;;;; Navigation

;; Feature `subword' provides a minor mode which causes the
;; `forward-word' and `backward-word' commands to stop at
;; capitalization changes within a word, so that you can step through
;; the components of CamelCase symbols one at a time.
(use-package subword
  :demand t
  :config

  (global-subword-mode +1)

  :blackout t)

(radian-defadvice radian--advice-allow-unpopping-mark
    (set-mark-command &optional arg)
  :around #'set-mark-command
  "Allow \\[set-mark-command] to step in reverse.
If a negative prefix argument is given (like
\\[negative-argument] \\[set-mark-command]), then it will step in
the reverse direction from \\[universal-argument]
\\[set-mark-command]."
  ;; Based on https://stackoverflow.com/a/14539202/3538165.
  (interactive "P")
  (if (< (prefix-numeric-value arg) 0)
      ;; If we don't have any marks set, no-op.
      (when mark-ring
        ;; I can't remember how this code works. Sorry.
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
        (when (null (mark t)) (ding))
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring)))))
    ;; If no prefix argument, or prefix argument is nonnegative, defer
    ;; to the original behavior.
    (funcall set-mark-command arg)))

(radian-defadvice radian--advice-allow-unpopping-global-mark
    (pop-global-mark &optional arg)
  :around #'pop-global-mark
  "Allow \\[pop-global-mark] to step in reverse.
If a negative prefix argument is given (like
\\[negative-argument] \\[pop-global-mark]), then it will step in
the reverse direction from \\[pop-global-mark]."
  (interactive "P")
  (if arg
      ;; Tweaked from the implementation of `pop-global-mark'.
      (progn
        (or global-mark-ring
            (error "No global mark set"))
        ;; We need to do this earlier than `pop-global-mark' does the
        ;; corresponding action in order to properly undo its
        ;; behavior.
        (setq global-mark-ring (nconc (list (car (last global-mark-ring)))
                                      (butlast global-mark-ring)))
        (while (and global-mark-ring
                    (not (marker-buffer (car (last global-mark-ring)))))
          (setq global-mark-ring (butlast global-mark-ring)))
        (let* ((marker (car (last global-mark-ring)))
               (buffer (marker-buffer marker))
               (position (marker-position marker)))
          (set-buffer buffer)
          (or (and (>= position (point-min))
                   (<= position (point-max)))
              (if widen-automatically
                  (widen)
                (error
                 "Global mark position is outside accessible part of buffer")))
          (goto-char position)
          (switch-to-buffer buffer)))
    (funcall pop-global-mark)))

;; Feature `bookmark' provides a way to mark places in a buffer. I
;; don't use it, but some other packages do.
(use-package bookmark)

;; try to browse documents in read-only mode for easier navigation
(setq view-read-only t)
(defun my-view-mode-keys ()
  (define-key view-mode-map (kbd "N") 'View-search-last-regexp-backward)
  (define-key view-mode-map "?" 'View-search-regexp-backward) ; Less does this.
  (define-key view-mode-map "G" 'View-goto-line-last)
  (define-key view-mode-map "j" 'View-scroll-line-forward)
  (define-key view-mode-map "k" 'View-scroll-line-backward))
(add-hook 'view-mode-hook 'my-view-mode-keys)


;; jump to character
(use-package avy
  :bind (("s-." . avy-goto-char-timer)))

;;;; Find and replace

(use-package isearch :straight (:type built-in)
  :config
  ;; Make isearch show number of candidates on the mode line
  (setq isearch-lazy-count t)
  ;; Interpret a space character as a wildcard
  (setq search-whitespace-regexp ".*")
  (setq isearch-lax-whitespace t))

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
  :bind (([remap query-replace-regexp] . #'radian-query-replace-literal))
  :config

  ;; Use Emacs-style regular expressions by default, instead of
  ;; Python-style.
  (setq vr/engine 'emacs)

  (defun radian-query-replace-literal ()
    "Do a literal query-replace using `visual-regexp'."
    (interactive)
    (let ((vr/engine 'emacs-plain))
      (call-interactively #'vr/query-replace))))

;;;; Select text

;; expand-region is useful. But there are existing keybindings for
;; selecting text like M-@ or C-M-@, so will try to rely on those
;; instead.
(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("s-o" . change-outer)
         ("s-i" . change-inner)
         ("s-O" . copy-outer)
         ("s-I" . copy-inner))
  :config
  ;; this code snippets have been taken from Magnar Sveen
  ;; https://github.com/magnars/change-inner.el
  (eval-when-compile (require 'cl))
  (defun ci--flash-region (start end)
    "Temporarily highlight region from START to END."
    (let ((overlay (make-overlay start end)))
      (overlay-put overlay 'face 'secondary-selection)
      (overlay-put overlay 'priority 100)
      (run-with-timer 0.2 nil 'delete-overlay overlay)))

  (defun change-inner* (yank? search-forward-char)
    "Works like vim's ci command. Takes a char, like ( or \" and
kills the innards of the first ancestor semantic unit starting with that char."
    (let* ((expand-region-fast-keys-enabled nil)
           (expand-region-smart-cursor nil)
           (char (or search-forward-char
                     (char-to-string
                      (read-char
                       (if yank?
                           "Yank inner, starting with:"
                         "Change inner, starting with:")))))
           (q-char (regexp-quote char))
           (starting-point (point)))
      (when search-forward-char
        (search-forward char (point-at-eol)))
      (cl-flet ((message (&rest args) nil))
            (er--expand-region-1)
            (er--expand-region-1)
            (while (and (not (= (point) (point-min)))
                        (not (looking-at q-char)))
              (er--expand-region-1))
            (if (not (looking-at q-char))
                (if search-forward-char
                    (error "Couldn't find any expansion starting with %S" char)
                  (goto-char starting-point)
                  (setq mark-active nil)
                  (change-inner* yank? char))
              (er/contract-region 1)
              (if yank?
                  (progn
                    (copy-region-as-kill (region-beginning) (region-end))
                    (ci--flash-region (region-beginning) (region-end))
                    (goto-char starting-point))
                (kill-region (region-beginning) (region-end)))))))

  (defun change-inner (arg)
    (interactive "P")
    (change-inner* arg nil))

  (defun copy-inner ()
    (interactive)
    (change-inner* t nil))

  (defun change-outer* (yank? search-forward-char)
    "Works like vim's ci command. Takes a char, like ( or \" and
kills the first ancestor semantic unit starting with that char."
    (let* ((expand-region-fast-keys-enabled nil)
           (expand-region-smart-cursor nil)
           (char (or search-forward-char
                     (char-to-string
                      (read-char
                       (if yank?
                           "Yank outer, starting with:"
                         "Change outer, starting with:")))))
           (q-char (regexp-quote char))
           (starting-point (point)))
      (when search-forward-char
        (search-forward char (point-at-eol)))
      (cl-flet ((message (&rest args) nil))
            (when (looking-at q-char)
              (er/expand-region 1))
            (while (and (not (= (point) (point-min)))
                        (not (looking-at q-char)))
              (er/expand-region 1))
            (if (not (looking-at q-char))
                (if search-forward-char
                    (error "Couldn't find any expansion starting with %S" char)
                  (goto-char starting-point)
                  (setq mark-active nil)
                  (change-outer* yank? char))
              (if yank?
                  (progn
                    (copy-region-as-kill (region-beginning) (region-end))
                    (ci--flash-region (region-beginning) (region-end))
                    (goto-char starting-point))
                (kill-region (region-beginning) (region-end)))))))

  (defun change-outer (arg)
    (interactive "P")
    (change-outer* arg nil))

  (defun copy-outer ()
    (interactive)
    (change-outer* t nil)))

;;; Electricity: automatic things
;;;; Autorevert

;; Bind revert-buffer to a easily accessable key
(bind-key "C-c u" #'revert-buffer)

;; Feature `autorevert' allows the use of file-watchers or polling in
;; order to detect when the file visited by a buffer has changed, and
;; optionally reverting the buffer to match the file (unless it has
;; unsaved changes).
(use-package autorevert
  :defer 2
  :init

  (defun radian--autorevert-silence ()
    "Silence messages from `auto-revert-mode' in the current buffer."
    (setq-local auto-revert-verbose nil))

  :config

  ;; Turn the delay on auto-reloading from 5 seconds down to 1 second.
  ;; We have to do this before turning on `auto-revert-mode' for the
  ;; change to take effect. (Note that if we set this variable using
  ;; `customize-set-variable', all it does is toggle the mode off and
  ;; on again to make the change take effect, so that way is dumb.)
  (setq auto-revert-interval 1)

  (global-auto-revert-mode +1)

  ;; Auto-revert all buffers, not only file-visiting buffers. The
  ;; docstring warns about potential performance problems but this
  ;; should not be an issue since we only revert visible buffers.
  (setq global-auto-revert-non-file-buffers t)

  ;; Since we automatically revert all visible buffers after one
  ;; second, there's no point in asking the user whether or not they
  ;; want to do it when they find a file. This disables that prompt.
  (setq revert-without-query '(".*"))

  (defun radian-autorevert-inhibit-p (buffer)
    "Return non-nil if autorevert should be inhibited for BUFFER."
    (or (null (get-buffer-window))
        (with-current-buffer buffer
          (or (null buffer-file-name)
              (file-remote-p buffer-file-name)))))

  :blackout auto-revert-mode)

;;;; Automatic delimiter pairing

;; Package `smartparens' provides an API for manipulating paired
;; delimiters of many different types, as well as interactive commands
;; and keybindings for operating on paired delimiters at the
;; s-expression level. It provides a Paredit compatibility layer.
(use-package smartparens
  :demand t
  :config

  ;; Load the default pair definitions for Smartparens.
  (require 'smartparens-config)


  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;; keybinding management
  (define-key smartparens-mode-map (kbd "C-c s C-M-f") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "C-c s C-M-b") 'sp-backward-sexp)

  (define-key smartparens-mode-map (kbd "C-c s C-M-d") 'sp-down-sexp)
  (define-key smartparens-mode-map (kbd "C-c s C-M-a") 'sp-backward-down-sexp)
  (define-key smartparens-mode-map (kbd "C-c s C-S-d") 'sp-beginning-of-sexp)
  (define-key smartparens-mode-map (kbd "C-c s C-S-a") 'sp-end-of-sexp)

  (define-key smartparens-mode-map (kbd "C-c s C-M-e") 'sp-up-sexp)
  (define-key smartparens-mode-map (kbd "C-c s C-M-u") 'sp-backward-up-sexp)
  (define-key smartparens-mode-map (kbd "C-c s C-M-t") 'sp-transpose-sexp)

  (define-key smartparens-mode-map (kbd "C-c s C-M-n") 'sp-forward-hybrid-sexp)
  (define-key smartparens-mode-map (kbd "C-c s C-M-p") 'sp-backward-hybrid-sexp)

  (define-key smartparens-mode-map (kbd "C-c s C-M-k") 'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "C-c s C-M-w") 'sp-copy-sexp)

  (define-key smartparens-mode-map (kbd "C-c s M-<delete>") 'sp-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "C-c s M-<backspace>") 'sp-backward-unwrap-sexp)

  (define-key smartparens-mode-map (kbd "C-c s C-<right>") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-c s C-<left>") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-c s C-M-<left>") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-c s C-M-<right>") 'sp-backward-barf-sexp)

  (define-key smartparens-mode-map (kbd "C-c s M-D") 'sp-splice-sexp)
  (define-key smartparens-mode-map (kbd "C-c s C-M-<delete>") 'sp-splice-sexp-killing-forward)
  (define-key smartparens-mode-map (kbd "C-c s C-M-<backspace>") 'sp-splice-sexp-killing-backward)
  (define-key smartparens-mode-map (kbd "C-c s C-S-<backspace>") 'sp-splice-sexp-killing-around)

  (define-key smartparens-mode-map (kbd "C-c s C-]") 'sp-select-next-thing-exchange)
  (define-key smartparens-mode-map (kbd "C-c s C-<left_bracket>") 'sp-select-previous-thing)
  (define-key smartparens-mode-map (kbd "C-c s C-M-]") 'sp-select-next-thing)

  (define-key smartparens-mode-map (kbd "C-c s M-F") 'sp-forward-symbol)
  (define-key smartparens-mode-map (kbd "C-c s M-B") 'sp-backward-symbol)

  (define-key smartparens-mode-map (kbd "C-c s C-\"") 'sp-change-inner)
  (define-key smartparens-mode-map (kbd "C-c s M-i") 'sp-change-enclosing)

  (bind-key "C-c s f" (lambda () (interactive) (sp-beginning-of-sexp 2)) smartparens-mode-map)
  (bind-key "C-c s b" (lambda () (interactive) (sp-beginning-of-sexp -2)) smartparens-mode-map)

  ;; (bind-key "C-M-s"
  ;;           (defhydra smartparens-hydra ()
  ;;             "Smartparens"
  ;;             ("d" sp-down-sexp "Down")
  ;;             ("e" sp-up-sexp "Up")
  ;;             ("u" sp-backward-up-sexp "Up")
  ;;             ("a" sp-backward-down-sexp "Down")
  ;;             ("f" sp-forward-sexp "Forward")
  ;;             ("b" sp-backward-sexp "Backward")
  ;;             ("k" sp-kill-sexp "Kill" :color blue)
  ;;             ("q" nil "Quit" :color blue))
  ;;           smartparens-mode-map)

  (bind-key "C-c s t" 'sp-prefix-tag-object smartparens-mode-map)
  (bind-key "C-c s p" 'sp-prefix-pair-object smartparens-mode-map)
  (bind-key "C-c s y" 'sp-prefix-symbol-object smartparens-mode-map)
  (bind-key "C-c s h" 'sp-highlight-current-sexp smartparens-mode-map)
  (bind-key "C-c s e" 'sp-prefix-save-excursion smartparens-mode-map)
  (bind-key "C-c s s c" 'sp-convolute-sexp smartparens-mode-map)
  (bind-key "C-c s s a" 'sp-absorb-sexp smartparens-mode-map)
  (bind-key "C-c s s e" 'sp-emit-sexp smartparens-mode-map)
  (bind-key "C-c s s p" 'sp-add-to-previous-sexp smartparens-mode-map)
  (bind-key "C-c s s n" 'sp-add-to-next-sexp smartparens-mode-map)
  (bind-key "C-c s s j" 'sp-join-sexp smartparens-mode-map)
  (bind-key "C-c s s s" 'sp-split-sexp smartparens-mode-map)
  (bind-key "C-c s s r" 'sp-rewrap-sexp smartparens-mode-map)
  (defvar mario-map)
  (define-prefix-command 'mario-map)
  (bind-key "C-c s s x" mario-map smartparens-mode-map)
  (bind-key "C-c s s x x" 'sp-extract-before-sexp smartparens-mode-map)
  (bind-key "C-c s s x a" 'sp-extract-after-sexp smartparens-mode-map)
  (bind-key "C-c s s x s" 'sp-swap-enclosing-sexp smartparens-mode-map)

  (bind-key "C-c C-x C-t" 'sp-transpose-hybrid-sexp smartparens-mode-map)

  (bind-key ";" 'sp-comment emacs-lisp-mode-map)

  ;; Enable Smartparens functionality in all buffers.
  (smartparens-global-mode +1)

  ;; Highlight matching delimiters.
  (show-smartparens-global-mode +1)

  ;; Prevent all transient highlighting of inserted pairs.
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)

  ;; Don't disable autoskip when point moves backwards. (This lets you
  ;; open a sexp, type some things, delete some things, etc., and then
  ;; type over the closing delimiter as long as you didn't leave the
  ;; sexp entirely.)
  (setq sp-cancel-autoskip-on-backward-movement nil)

  :blackout t)

;;;; Code reformatting

;; Package `apheleia' implements a sophisticated algorithm for
;; applying code formatters asynchronously on save without moving
;; point or modifying the scroll position.
(use-package apheleia
  :straight (:host github :repo "radian-software/apheleia")
  :init

  (apheleia-global-mode +1)

  (radian-defadvice radian--save-buffer-reformat-maybe (func &optional arg)
    :around #'save-buffer
    "Make it so \\[save-buffer] with prefix arg inhibits reformatting."
    (let ((apheleia-mode (and apheleia-mode (member arg '(nil 1)))))
      (funcall func)))

  ;; We need to do this both before and after Apheleia is loaded
  ;; because the autoloading is set up such that the minor mode
  ;; definition is evaluated twice.
  (blackout 'apheleia-mode " Aph")

  :blackout " Aph")

;;;; Snippet expansion

;; Package `yasnippet' allows the expansion of user-defined
;; abbreviations into fillable templates. The only reason we have it
;; here is because it gets pulled in by LSP, and we need to unbreak
;; some stuff.
(use-package yasnippet
  :bind (:map yas-minor-mode-map

              ;; Disable TAB from expanding snippets, as I don't use it and
              ;; it's annoying.
              ("TAB" . nil)
              ("<tab>" . nil))
  :config

  ;; Reduce verbosity. The default value is 3. Bumping it down to 2
  ;; eliminates a message about successful snippet lazy-loading setup
  ;; on every(!) Emacs init. Errors should still be shown.
  (setq yas-verbosity 2)

  ;; Make it so that Company's keymap overrides Yasnippet's keymap
  ;; when a snippet is active. This way, you can TAB to complete a
  ;; suggestion for the current field in a snippet, and then TAB to
  ;; move to the next field. Plus, C-g will dismiss the Company
  ;; completions menu rather than cancelling the snippet and moving
  ;; the cursor while leaving the completions menu on-screen in the
  ;; same location.
  (use-package company
    :config

    ;; This function translates the "event types" I get from
    ;; `map-keymap' into things that I can pass to `lookup-key' and
    ;; `define-key'. It's a hack, and I'd like to find a built-in
    ;; function that accomplishes the same thing while taking care of
    ;; any edge cases I might have missed in this ad-hoc solution.
    (defun radian--yasnippet-normalize-event (event)
      "This function is a complete hack, do not use.
But in principle, it translates what we get from `map-keymap'
into what `lookup-key' and `define-key' want."
      (if (vectorp event)
          event
        (vector event)))

    ;; Here we define a hybrid keymap that delegates first to
    ;; `company-active-map' and then to `yas-keymap'.
    (defvar radian--yasnippet-then-company-keymap
      ;; It starts out as a copy of `yas-keymap', and then we
      ;; merge in all of the bindings from `company-active-map'.
      (let ((keymap (copy-keymap yas-keymap)))
        (map-keymap
         (lambda (event company-cmd)
           (let* ((event (radian--yasnippet-normalize-event event))
                  (yas-cmd (lookup-key yas-keymap event)))
             ;; Here we use an extended menu item with the
             ;; `:filter' option, which allows us to dynamically
             ;; decide which command we want to run when a key is
             ;; pressed.
             (define-key keymap event
               `(menu-item
                 nil ,company-cmd :filter
                 (lambda (cmd)
                   ;; There doesn't seem to be any obvious
                   ;; function from Company to tell whether or not
                   ;; a completion is in progress (à la
                   ;; `company-explicit-action-p'), so I just
                   ;; check whether or not `company-my-keymap' is
                   ;; defined, which seems to be good enough.
                   (if company-my-keymap
                       ',company-cmd
                     ',yas-cmd))))))
         company-active-map)
        keymap)
      "Keymap which delegates to both `company-active-map' and `yas-keymap'.
The bindings in `company-active-map' only apply if Company is
currently active.")

    (radian-defadvice radian--advice-company-overrides-yasnippet
        (yas--make-control-overlay &rest args)
      :around #'yas--make-control-overlay
      "Allow `company' keybindings to override those of `yasnippet'."
      ;; The function `yas--make-control-overlay' uses the current
      ;; value of `yas-keymap' to build the Yasnippet overlay, so to
      ;; override the Yasnippet keymap we only need to dynamically
      ;; rebind `yas-keymap' for the duration of that function.
      (let ((yas-keymap radian--yasnippet-then-company-keymap))
        (apply yas--make-control-overlay args))))

  :blackout yas-minor-mode)

;;; IDE features
;;;; Virtual environments
;;;;; Python

;; Package `pyvenv' provides functions for activating and deactivating
;; Python virtualenvs within Emacs. It's mostly not needed anymore now
;; that `lsp-python-ms' is configured to discover the appropriate
;; Pipenv or Poetry virtualenv, but maybe it will come in handy
;; someday.
(use-package pyvenv)

;;;; Language servers

;; Package `lsp-mode' is an Emacs client for the Language Server
;; Protocol <https://langserver.org/>. It is where we get all of our
;; information for completions, definition location, documentation,
;; and so on.
(use-package lsp-mode
  :init

  (defcustom radian-lsp-disable nil
    "If non-nil, then LSP is not allowed to be enabled.
For use in file-local variables."
    :type 'boolean
    :safe #'booleanp)

  (radian-defhook radian--lsp-enable ()
    after-change-major-mode-hook
    "Enable `lsp-mode' for most programming modes.
Do this on `after-change-major-mode-hook' instead of
`prog-mode-hook' and `text-mode-hook' because we want to make
sure regular mode hooks get a chance to run first, for example to
set LSP configuration (see `lsp-python-ms')."
    (when (derived-mode-p #'prog-mode #'text-mode)
      (unless (or radian-lsp-disable
                  (null buffer-file-name)
                  (derived-mode-p
                   ;; `lsp-mode' doesn't support Elisp, so let's avoid
                   ;; triggering the autoload just for checking that, yes,
                   ;; there's nothing to do for the *scratch* buffer.
                   #'emacs-lisp-mode
                   ;; Disable for modes that we currently use a specialized
                   ;; framework for, until they are phased out in favor of
                   ;; LSP.
                   #'clojure-mode
                   #'ruby-mode))
        (lsp))))

  :config

  ;; As per <https://github.com/emacs-lsp/lsp-mode#performance>.
  (setq read-process-output-max (* 1024 1024))

  (defun radian--advice-lsp-mode-silence (format &rest args)
    "Silence needless diagnostic messages from `lsp-mode'.

This is a `:before-until' advice for several `lsp-mode' logging
functions."
    (or
     (string-match-p "No LSP server for %s" format)
     (string-match-p "Connected to %s" format)
     (string-match-p "Unable to calculate the languageId" format)
     (string-match-p
      "There are no language servers supporting current mode" format)
     ;; Errors we get from gopls for no good reason (I can't figure
     ;; out why). They don't impair functionality.
     (and (stringp (car args))
          (or (string-match-p "^no object for ident .+$" (car args))
              (string-match-p "^no identifier found$" (car args))))))

  (dolist (fun '(lsp-warn lsp--warn lsp--info lsp--error))
    (advice-add fun :before-until #'radian--advice-lsp-mode-silence))

  ;; If we don't disable this, we get a warning about YASnippet not
  ;; being available, even though it is. I don't use YASnippet anyway,
  ;; so don't bother with it.
  (setq lsp-enable-snippet nil)

  (radian-defadvice radian--lsp-run-from-node-modules (command)
    :filter-return #'lsp-resolve-final-function
    "Find LSP executables inside node_modules/.bin if present."
    (cl-block nil
      (prog1 command
        (when-let ((project-dir
                    (locate-dominating-file default-directory "node_modules"))
                   (binary
                    (radian--path-join
                     project-dir "node_modules" ".bin" (car command))))
          (when (file-executable-p binary)
            (cl-return (cons binary (cdr command))))))))

  (radian-defhook radian--lsp-teardown ()
    kill-emacs-hook
    "Ignore the LSP server getting killed.
If we don't do this, then when killing Emacs we may be prompted
with whether we want to restart the LSP server that has just been
killed (which happens during Emacs shutdown)."
    (setq lsp-restart nil))

  ;; Looks like `lsp-mode' doesn't know about LaTeX yet.
  (add-to-list 'lsp-language-id-configuration '(latex-mode . "latex"))

  ;; Also, it has a bunch of regexps which are completely wrong.
  (setq lsp-language-id-configuration
        (mapcar
         (lambda (link)
           (if (and (stringp (car link))
                    (string-match "\\`\\.\\*\\.\\(.+\\)\\'" (car link)))
               (cons
                (format "\\.%s\\'" (match-string 1 (car link))) (cdr link))
             link))
         lsp-language-id-configuration))

  ;; Disable LSP reformatting your code as you type. We use Apheleia
  ;; for that instead.
  (setq lsp-enable-on-type-formatting nil)

  :blackout " LSP")

(use-package lsp-java
  :hook (java-mode . (lambda () (require 'lsp-java))))

;;;; Indentation

;; Don't use tabs for indentation. Use only spaces. Otherwise,
;; whenever the indent level does not equal the tab width (e.g. in
;; Emacs Lisp code, the indent level is 2 and the tab width is 8),
;; *both* tabs and spaces will be used for indentation. Disgusting.
(setq-default indent-tabs-mode nil)

(defun radian-indent-defun ()
  "Indent the surrounding defun."
  (interactive)
  (save-excursion
    (when (beginning-of-defun)
      (let ((beginning (point)))
        (end-of-defun)
        (let ((end (point)))
          (let ((inhibit-message t)
                (message-log-max nil))
            (indent-region beginning end)))))))

(bind-key* "C-M-q" #'radian-indent-defun)

;;;; Autocompletion

;; Package `company' provides an in-buffer autocompletion framework.
;; It allows for packages to define backends that supply completion
;; candidates, as well as optional documentation and source code. Then
;; Company allows for multiple frontends to display the candidates,
;; such as a tooltip menu. Company stands for "Complete Anything".
(use-package company
  :defer 0.5
  :init

  (defvar radian--company-backends-global
    '(company-capf
      company-files
      (company-dabbrev-code company-keywords)
      company-dabbrev)
    "Values for `company-backends' used everywhere.
If `company-backends' is overridden by Radian, then these
backends will still be included.")

  :bind (:filter company-mode

         ;; Remap the standard Emacs keybindings for invoking
         ;; completion to instead use Company. You might think this
         ;; could be put in the `:bind*' declaration below, but it
         ;; seems that `bind-key*' does not work with remappings.
         ([remap completion-at-point] . #'company-manual-begin)
         ([remap complete-symbol] . #'company-manual-begin)

         ;; The following are keybindings that take effect whenever
         ;; the completions menu is visible, even if the user has not
         ;; explicitly interacted with Company.

         :map company-active-map

         ;; Make TAB always complete the current selection, instead of
         ;; only completing a common prefix.
         ("<tab>" . #'company-complete-selection)
         ("TAB" . #'company-complete-selection)

         ;; When was the last time you used the C-s binding for
         ;; searching candidates? It conflicts with buffer search,
         ;; anyway. Same for the scroll commands.
         ("C-s" . nil)
         ([remap scroll-down-command] . nil)
         ([remap scroll-up-command] . nil)

         ;; The following are keybindings that only take effect if the
         ;; user has explicitly interacted with Company. Note that
         ;; `:map' from above is "sticky", and applies also below: see
         ;; https://github.com/jwiegley/use-package/issues/334#issuecomment-349473819.

         :filter (company-explicit-action-p)

         ;; Make RET trigger a completion if and only if the user has
         ;; explicitly interacted with Company, instead of always
         ;; doing so.
         ("<return>" . #'company-complete-selection)
         ("RET" . #'company-complete-selection)

         ;; We then make <up> and <down> abort the completions menu
         ;; unless the user has interacted explicitly. Note that we
         ;; use `company-select-previous' instead of
         ;; `company-select-previous-or-abort'. I think the former
         ;; makes more sense since the general idea of this `company'
         ;; configuration is to decide whether or not to steal
         ;; keypresses based on whether the user has explicitly
         ;; interacted with `company', not based on the number of
         ;; candidates.
         ;;
         ;; Note that M-p and M-n work regardless of whether explicit
         ;; interaction has happened yet, and note also that M-TAB
         ;; when the completions menu is open counts as an
         ;; interaction.
         ("<up>" . #'company-select-previous)
         ("<down>" . #'company-select-next))

  :bind* (:filter company-mode

          ;; The default keybinding for `completion-at-point' and
          ;; `complete-symbol' is M-TAB or equivalently C-M-i. We
          ;; already remapped those bindings to `company-manual-begin'
          ;; above. Here we make sure that they definitely invoke
          ;; `company-manual-begin' even if a minor mode binds M-TAB
          ;; directly.
          ("M-TAB" . #'company-manual-begin))

  :config

  ;; Make completions display twice as soon.
  (setq company-idle-delay 0.15)

  ;; Make completions display when you have only typed one character,
  ;; instead of three.
  (setq company-minimum-prefix-length 1)

  ;; Always display the entire suggestion list onscreen, placing it
  ;; above the cursor if necessary.
  (setq company-tooltip-minimum company-tooltip-limit)

  ;; Always display suggestions in the tooltip, even if there is only
  ;; one. Also, don't display metadata in the echo area. (This
  ;; conflicts with ElDoc.)
  (setq company-frontends '(company-pseudo-tooltip-frontend))

  ;; Show quick-reference numbers in the tooltip. (Select a completion
  ;; with M-1 through M-0.)
  (setq company-show-quick-access t)

  ;; Prevent non-matching input (which will dismiss the completions
  ;; menu), but only if the user interacts explicitly with Company.
  (setq company-require-match #'company-explicit-action-p)

  ;; Only search the current buffer to get suggestions for
  ;; `company-dabbrev' (a backend that creates suggestions from text
  ;; found in your buffers). This prevents Company from causing lag
  ;; once you have a lot of buffers open.
  (setq company-dabbrev-other-buffers nil)

  ;; Make the `company-dabbrev' backend fully case-sensitive, to
  ;; improve the UX when working with domain-specific words that have
  ;; particular casing.
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)

  ;; When candidates in the autocompletion tooltip have additional
  ;; metadata, like a type signature, align that information to the
  ;; right-hand side. This usually makes it look neater.
  (setq company-tooltip-align-annotations t)

  (defvar-local radian--company-buffer-modified-counter nil
    "Last return value of `buffer-chars-modified-tick'.
Used to ensure that Company only initiates a completion when the
buffer is modified.")

  (radian-defadvice radian--advice-company-complete-on-change ()
    :override #'company--should-begin
    "Make Company trigger a completion when the buffer is modified.
This is in contrast to the default behavior, which is to trigger
a completion when one of a whitelisted set of commands is used.
One specific improvement this brings about is that you get
completions automatically when backspacing into a symbol."
    (let ((tick (buffer-chars-modified-tick)))
      (unless (equal tick radian--company-buffer-modified-counter)
        ;; Only trigger completion if previous counter value was
        ;; non-nil (i.e., don't trigger completion just as we're
        ;; jumping to a buffer for the first time).
        (prog1 (and radian--company-buffer-modified-counter
                    (not (and (symbolp this-command)
                              (string-match-p
                               "^\\(company-\\|undo-\\|undo$\\)"
                               (symbol-name this-command)))))
          (setq radian--company-buffer-modified-counter tick)))))

  (radian-defadvice radian--advice-company-update-buffer-modified-counter ()
    :after #'company--should-continue
    "Make sure `radian--company-buffer-modified-counter' is up to date.
If we don't do this on `company--should-continue' as well as
`company--should-begin', then we may end up in a situation where
autocomplete triggers when it shouldn't. Specifically suppose we
delete a char from a symbol, triggering autocompletion, then type
it back, but there is more than one candidate so the menu stays
onscreen. Without this advice, saving the buffer will cause the
menu to disappear and then come back after `company-idle-delay'."
    (setq radian--company-buffer-modified-counter
          (buffer-chars-modified-tick)))

  (global-company-mode +1)

  :blackout t)

;; Package `company-lsp' provides a Company backend for `lsp-mode'.
;; It's configured automatically by `lsp-mode'.
(use-package company-lsp)

(bind-key "M-/" 'hippie-expand)

;;;; Definition location

;; Feature `xref' provides the built-in Emacs interface for source
;; navigation, which various packages can plug into.
(use-package xref
  :config

  ;; When there are multiple options for where a symbol might be
  ;; defined, use the default `completing-read' mechanism to decide
  ;; between them (i.e., delegate to Vertico) rather than using the
  ;; janky built-in `xref' thingie.
  (when (and
         (boundp 'xref-show-definitions-function)
         (fboundp 'xref-show-definitions-completing-read))
    (setq xref-show-definitions-function
          #'xref-show-definitions-completing-read)))

;; Package `dumb-jump' provides a mechanism to jump to the definitions
;; of functions, variables, etc. in a variety of programming
;; languages. The advantage of `dumb-jump' is that it doesn't try to
;; be clever, so it "just works" instantly for dozens of languages
;; with zero configuration.
(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 80))

;;;; Display contextual metadata

;; Feature `eldoc' provides a minor mode (enabled by default) which
;; allows function signatures or other metadata to be displayed in the
;; echo area.
(use-package eldoc
  :demand t
  :config

  ;; For Emacs 26 and below, `eldoc--message' is not defined. For
  ;; Emacs 27 and above, `eldoc-message' is obsolete.
  (with-no-warnings
    (radian-defadvice radian--advice-eldoc-no-trample (func &rest args)
      :around #'eldoc-print-current-symbol-info
      "Prevent `eldoc' from trampling on existing messages."
      (radian-flet ((defun eldoc-message (&optional string)
                      (if string
                          (funcall eldoc-message string)
                        (setq eldoc-last-message nil)))
                    (defun eldoc--message (&optional string)
                      (if string
                          (funcall eldoc--message string)
                        (setq eldoc-last-message nil))))
        (apply func args))))

  ;; Always truncate ElDoc messages to one line. This prevents the
  ;; echo area from resizing itself unexpectedly when point is on a
  ;; variable with a multiline docstring.
  (setq eldoc-echo-area-use-multiline-p nil)

  ;; Original code from
  ;; https://github.com/PythonNut/emacs-config/blob/1a92a1ff1d563fa6a9d7281bbcaf85059c0c40d4/modules/config-intel.el#L130-L137,
  ;; thanks!

  (radian-defadvice radian--advice-eldoc-better-display-message-p (&rest _)
    :override #'eldoc--message-command-p
    "Make ElDoc smarter about when to display its messages.
By default ElDoc has a customizable whitelist of commands that it
will display its messages after. The idea of this is to not
trample on messages that other commands may have printed.
However, this is a hopeless endeavour because there are a
virtually unlimited number of commands that don't conflict with
ElDoc. A better approach is to simply check to see if a message
was printed, and only have ElDoc display if one wasn't."
    (member (current-message) (list nil eldoc-last-message)))

  :blackout t)

;;;; Syntax checking and code linting

;; Package `flycheck' provides a framework for in-buffer error and
;; warning highlighting. We kind of don't use it because we use
;; `lsp-ui' instead, but internally `lsp-ui' actually hacks Flycheck
;; to behave differently, so it is a dependency. We just don't enable
;; Flycheck anywhere else and rely on `lsp-ui' to handle things when
;; appropriate. However, interestingly, Flycheck is not marked as a
;; dependency of `lsp-ui', hence this declaration.
(use-package flycheck
  :config

  :blackout t)


;; Package `lsp-ui' provides a pretty UI for showing diagnostic
;; messages from LSP in the buffer using overlays. It's configured
;; automatically by `lsp-mode'.
(use-package lsp-ui
  :bind (("C-c f" . #'lsp-ui-sideline-apply-code-actions))
  :config

  (radian-defadvice radian--advice-lsp-ui-apply-single-fix
      (orig-fun &rest args)
    :around #'lsp-ui-sideline-apply-code-actions
    "Apply code fix immediately if only one is possible."
    (radian-flet ((defun completing-read (prompt collection &rest args)
                    (if (= (safe-length collection) 1)
                        (car collection)
                      (apply completing-read prompt collection args))))
      (apply orig-fun args)))

  )

;;; Language support

;;;; Clojure
;; https://clojure.org/

;; Package `clojure-mode' provides a major mode for Clojure.
(use-package clojure-mode)

;; Package `cider' provides integrated Clojure and ClojureScript REPLs
;; directly in Emacs, a Company backend that uses a live REPL
;; connection to retrieve completion candidates, and documentation and
;; source lookups for Clojure code.
(use-package cider
  :config

  ;; By default, any error messages that occur when CIDER is starting
  ;; up are placed in the *nrepl-server* buffer and not in the
  ;; *cider-repl* buffer. This is silly, since no-one wants to check
  ;; *nrepl-server* every time they start a REPL, and if you don't
  ;; then startup errors (including errors in anything loaded by the
  ;; :main namespace) are effectively silenced. So we copy everything
  ;; from the *nrepl-server* buffer to the *cider-repl* buffer, as
  ;; soon as the latter is available.
  ;;
  ;; Note that this does *not* help in the case of things going so
  ;; horribly wrong that the REPL can't even start. In this case you
  ;; will have to check the *nrepl-server* buffer manually. Perhaps an
  ;; error message that is visible from any buffer could be added in
  ;; future.
  ;;
  ;; Thanks to malabarba on Clojurians Slack for providing the
  ;; following code:

  (radian-defhook radian--cider-dump-nrepl-server-log ()
    cider-connected-hook
    "Copy contents of *nrepl-server* to beginning of *cider-repl*."
    (save-excursion
      (goto-char (point-min))
      (insert
       (with-current-buffer nrepl-server-buffer
         (buffer-string)))))

  ;; The CIDER welcome message often obscures any error messages that
  ;; the above code is supposed to be making visible. So, we need to
  ;; turn off the welcome message.
  (setq cider-repl-display-help-banner nil)

  ;; Sometimes in the CIDER REPL, when Emacs is running slowly, you
  ;; can manage to press TAB before the Company completions menu pops
  ;; up. This triggers a `completing-read', which is disorienting. So
  ;; we reset TAB to its default functionality (i.e. indent only) in
  ;; the CIDER REPL.
  (setq cider-repl-tab-command 'indent-for-tab-command)

  ;; Don't focus the cursor in the CIDER REPL once it starts. Since
  ;; the REPL takes so long to start up, especially for large
  ;; projects, you either have to wait for a minute without doing
  ;; anything or be prepared for your cursor to suddenly shift buffers
  ;; without warning sometime in the near future. This is annoying, so
  ;; turn off the behavior. For a historical perspective see [1].
  ;;
  ;; [1]: https://github.com/clojure-emacs/cider/issues/1872
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)

  :blackout t)

;;;; Go
;; https://golang.org/

;; Package `go-mode' provides a major mode for Go.
(use-package go-mode
  :config

  (defvar radian--go-defun-regexp
    "^\\(const\\|func\\|import\\|interface\\|package\\|type\\|var\\)"
    "Regexp matching top-level declarations in Go.")

  (defun radian--go-beginning-of-defun (&optional arg)
    "Move to beginning of current or previous top-level declaration."
    (cond
     ((null arg)
      (cl-block nil
        (while t
          (re-search-backward radian--go-defun-regexp nil 'noerror)
          (when (or (bobp)
                    (eq (get-text-property (point) 'face)
                        'font-lock-keyword-face))
            (cl-return)))))
     ((> arg 0)
      (dotimes (_ arg)
        (radian--go-beginning-of-defun)))
     ((< arg 0)
      ;; Yuck -- but we need to implement this, otherwise
      ;; `end-of-defun' just does the wrong thing :/
      (dotimes (_ (- arg))
        (radian--go-beginning-of-defun)
        (radian--go-end-of-defun)
        (radian--go-end-of-defun))
      (radian--go-beginning-of-defun))))

  (defun radian--go-end-of-defun ()
    "Move to end of current or previous top-level declaration.
Only works if `radian--go-beginning-of-defun' was just called
previously."
    (dotimes (_ 2)
      (cl-block nil
        (while t
          (re-search-forward radian--go-defun-regexp nil 'noerror)
          (when (or (eobp)
                    (save-excursion
                      (beginning-of-line)
                      (eq (get-text-property (point) 'face)
                          'font-lock-keyword-face)))
            (cl-return)))))
    (beginning-of-line)
    (go--backward-irrelevant 'stop-at-string)
    (forward-line))

  (radian-defhook radian--go-defun-setup ()
    go-mode-hook
    "Set up \\[beginning-of-defun] and \\[end-of-defun] correctly.
See <https://github.com/dominikh/go-mode.el/issues/232>."
    (setq-local beginning-of-defun-function #'radian--go-beginning-of-defun)
    (setq-local end-of-defun-function #'radian--go-end-of-defun))

  (use-package lsp-ui
    :config

    (radian-defadvice radian--advice-lsp-ui-organize-imports-more-cleanly
        (func actions &rest args)
      :around #'lsp-ui-sideline--code-actions
      "Clean up the \"Organize Imports\" code actions for Go.
Firstly, don't display \"Organize Imports\" or \"Organize All
Imports\" in the sideline, as gopls sometimes reports these code
actions when the indentation is wrong (rather than when imports
need to be changed). Secondly, filter out \"Organize All
Imports\" internally, so that applying a code action will default
to \"Organize Imports\" instead of prompting you to decide
between that and \"Organize All Imports\" (which does the same
thing as far as I can tell)."
      (let ((actions-to-keep nil)
            (actions-to-render nil))
        (dolist (action actions)
          (unless (equal "Organize All Imports" (gethash "title" action))
            (push action actions-to-keep)
            (unless (equal "Organize Imports" (gethash "title" action))
              (push action actions-to-render))))
        (setq actions-to-keep (nreverse actions-to-keep))
        (setq actions-to-render (nreverse actions-to-render))
        (when actions-to-render
          (apply func actions-to-render args))
        (setq lsp-ui-sideline--code-actions actions-to-keep)))))

;;;; Haskell
;; https://www.haskell.org/

;; Package `haskell-mode' provides a major mode and REPL integration
;; for Haskell.
(use-package haskell-mode
  :config

  ;; Enable REPL integration.
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)

  (radian-defadvice radian--advice-haskell-fix-back-to-indentation
      (back-to-indentation)
    :around #'back-to-indentation
    "Fix `back-to-indentation' in `literate-haskell-mode'.
Otherwise, it just moves point to column 0, which is wrong.

This works around an upstream bug; see
<https://github.com/haskell/haskell-mode/issues/1594>."
    (if (derived-mode-p 'literate-haskell-mode)
        (progn
          (beginning-of-line 1)
          (when-let ((c (char-after)))
            (when (= c ? )
              (forward-char)))
          (skip-syntax-forward " " (line-end-position))
          (backward-prefix-chars))
      (funcall back-to-indentation))))


;; Package `lsp-haskell' configures the HIE Haskell language server
;; for use with `lsp-mode'.
(use-package lsp-haskell
  :demand t
  :after (:all lsp-mode haskell-mode))

;;;; Lua
;; <http://www.lua.org/>

;; Package `lua-mode' provides a major mode for Lua code.
(use-package lua-mode)

;;;; Makefile

;; Feature `make-mode' provides major modes for editing Makefiles.
(use-package make-mode
  :blackout ((makefile-automake-mode . "Makefile")
             (makefile-gmake-mode . "Makefile")
             (makefile-makepp-mode . "Makefile")
             (makefile-bsdmake-mode . "Makefile")
             (makefile-imake-mode . "Makefile")))

;;;; Markdown
;; https://daringfireball.net/projects/markdown/

;; Package `markdown-mode' provides a major mode for Markdown.
(use-package markdown-mode
  :mode (;; Extension used by Hugo.
         ("\\.mmark\\'" . markdown-mode))

  :bind (;; C-c C-s p is a really dumb binding, we prefer C-c C-s C-p.
         ;; Same for C-c C-s q.
         :map markdown-mode-style-map
         ("C-p" . #'markdown-insert-pre)
         ("C-q" . #'markdown-insert-blockquote)
         :map markdown-mode-map
         ("TAB" . #'radian-markdown-tab)
         ;; Try to override all the bindings in
         ;; `markdown-mode-map'...
         ("<S-iso-lefttab>" . #'radian-markdown-shifttab)
         ("<S-tab>" . #'radian-markdown-shifttab)
         ("<backtab>" . #'radian-markdown-shifttab))
  :config

  (defun radian-markdown-tab ()
    "Do something reasonable when the user presses TAB.
This means moving forward a table cell, indenting a list item, or
performing normal indentation."
    (interactive)
    (cond
     ((markdown-table-at-point-p)
      (markdown-table-forward-cell))
     ((markdown-list-item-at-point-p)
      (markdown-demote-list-item))
     (t
      ;; Ew. But `markdown-indent-line' checks to see if
      ;; `this-command' is `markdown-cycle' before doing something
      ;; useful, so we have to.
      (let ((this-command 'markdown-cycle))
        (indent-for-tab-command)))))

  (defun radian-markdown-shifttab ()
    "Do something reasonable when the user presses S-TAB.
This means moving backward a table cell or unindenting a list
item."
    (interactive)
    (cond
     ((markdown-table-at-point-p)
      (markdown-table-backward-cell))
     ((markdown-list-item-at-point-p)
      (markdown-promote-list-item))))

  (radian-defadvice radian--disable-markdown-metadata-fontification (&rest _)
    :override #'markdown-match-generic-metadata
    "Prevent fontification of YAML metadata blocks in `markdown-mode'.
This prevents a mis-feature wherein if the first line of a
Markdown document has a colon in it, then it's distractingly and
usually wrongly fontified as a metadata block. See
https://github.com/jrblevin/markdown-mode/issues/328."
    (prog1 nil (goto-char (point-max)))))

;;;; Protobuf

;; Package `protobuf-mode' provides a major mode for Protobuf.
(use-package protobuf-mode)

;;;; Python
;; https://www.python.org/

;; Feature `python' provides a major mode for Python.
(use-package python
  :config

  ;; The only consistent style.
  (setq python-fill-docstring-style 'django)

  (radian-defhook radian--python-fix-outline-mode-config ()
    python-mode-hook
    "Prevent `python-mode' from overriding `outline-minor-mode' config.
If this hook is not used, then `python-mode' will override even a
file-local setting of e.g. `outline-regexp' with its own setting."
    (kill-local-variable 'outline-regexp)
    (kill-local-variable 'outline-level)
    (kill-local-variable 'outline-heading-end-regexp))

  (radian-defhook radian--python-no-reindent-on-colon ()
    python-mode-hook
    "Don't reindent on typing a colon.
See https://emacs.stackexchange.com/a/3338/12534."
    (setq electric-indent-chars (delq ?: electric-indent-chars)))

  ;; Default to Python 3. Prefer the versioned Python binaries since
  ;; some systems stupidly make the unversioned one point at Python 2.
  (cond
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python")))

  (radian-defhook radian--python-use-correct-executable ()
    python-mode-hook
    "Use correct executables for Python tooling."
    (save-excursion
      (save-match-data
        (when (or (looking-at "#!/usr/bin/env \\(python[^ \n]+\\)")
                  (looking-at "#!\\([^ \n]+/python[^ \n]+\\)"))
          (setq-local
           python-shell-interpreter
           (substring-no-properties (match-string 1))))))
    (with-no-warnings
      (setq-local
       lsp-pyright-python-executable-cmd
       python-shell-interpreter)))

  ;; I honestly don't understand why people like their packages to
  ;; spew so many messages.
  (setq python-indent-guess-indent-offset-verbose nil)

  (defun radian--python-find-virtualenv ()
    "Find a virtualenv corresponding to the current buffer.
Return either a string or nil."
    (cl-block nil
      (when (and (executable-find "poetry")
                 (locate-dominating-file default-directory "pyproject.toml"))
        (with-temp-buffer
          ;; May create virtualenv, but whatever.
          (when (= 0 (call-process
                      "poetry" nil '(t nil) nil "run" "which" "python"))
            (goto-char (point-min))
            (when (looking-at "\\(.+\\)/bin/python\n")
              (let ((venv (match-string 1)))
                (when (file-directory-p venv)
                  (cl-return venv)))))))
      (when (and (executable-find "pipenv")
                 (locate-dominating-file default-directory "Pipfile"))
        (with-temp-buffer
          ;; May create virtualenv, but whatever.
          (when (= 0 (call-process "pipenv" nil '(t nil) nil "--venv"))
            (goto-char (point-min))
            (let ((venv (string-trim (buffer-string))))
              (when (file-directory-p venv)
                (cl-return venv)))))))))

;; Package `lsp-pyright' downloads Microsoft's LSP server for Python.
;; We hate Microsoft and think they are going to try to kill off
;; open-source projects in the Python/LSP ecosystem, but there does
;; not appear to be a workable alternative to Pyright at present. See
;; https://github.com/microsoft/pylance-release/issues/4 for more
;; discussion.
(use-package lsp-pyright
  :demand t
  :after (:all lsp-mode python)
  :config

  (radian-defadvice radian--lsp-pyright-discover-virtualenvs
      (&rest _)
    :before-until #'lsp-pyright-locate-venv
    "Automatically discover Pipenv and Poetry virtualenvs."
    (radian--python-find-virtualenv)))

;;;; Ruby
;; https://www.ruby-lang.org/

;; Package `robe' provides a language server for Ruby which draws
;; information for autocompletions and source code navigation from a
;; live REPL in the project context. Start it with `robe-start'.
(use-package robe
  :init

  (add-hook 'ruby-mode-hook #'robe-mode)

  :blackout t)

;;;; Rust
;; https://www.rust-lang.org/

;; Package `rust-mode' provides a major mode for Rust.
(use-package rust-mode)

;;;; Scheme

;; http://www.schemers.org/

;; Package `geiser' provides REPL integration for several
;; implementations of Scheme.
(use-package geiser)

;;;; Shell
;; http://pubs.opengroup.org/onlinepubs/9699919799/utilities/sh.html
;; https://www.gnu.org/software/bash/
;; http://www.zsh.org/

(use-package sh-script
  :config
  ;; This is bind to sh-set-indent, and I don't use this. Since at the
  ;; moment I have no use for this keybinding, and it conflicts with
  ;; my keybinding for diff package, I'll turn it off.
  (define-key sh-mode-map "\C-c=" nil))

;; shell settings
(add-hook 'shell-mode-hook (lambda () (setq comint-scroll-to-bottom-on-input t
                                            comint-input-ignoredups t)))
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'shell-mode-hook (lambda () (company-mode -1)
                             (auto-fill-mode -1)))

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

(defun copy-whole-string ()
  "Copy URL at the point in shell, and open it in firefox."
  (interactive)
  (re-search-backward "[\n ]")
  (forward-char 1)
  (set-mark-command)
  (re-search-forward "[\n ]")
  (backward-char 1)
  (kill-ring-save nil nil t))

(define-key shell-mode-map (kbd "C-<return>") 'print-dtjob)
(define-key shell-mode-map (kbd "S-<return>") 'open-tgf-log)
(define-key shell-mode-map (kbd "M-W") 'copy-whole-string)

;; Select JOB_ID from 'tgq' or 'tgr' output.
;; Basically, select first word, but when used in context of TGF
;; output, that is JOB_ID
(fset 'jobid
   (kmacro-lambda-form [?\C-a ?\C-  ?\M-f ?\M-w ?\C-y] 0 "%d"))
(define-key shell-mode-map (kbd "C-x C-z s") 'jobid)

;; Select whole line, and move to the end of buffer (prompt).(
(fset 'job-select-all
(kmacro-lambda-form [?\C-a ?\C-  ?\C-e ?\M-w ?\M->] 0 "%d"))
(define-key shell-mode-map (kbd "C-x C-z a") 'job-select-all))

;;;; Web
;; https://developer.mozilla.org/en-US/docs/web/HTML
;; https://developer.mozilla.org/en-US/docs/Web/CSS
;; https://developer.mozilla.org/en-US/docs/Web/JavaScript

;; Feature `js' provides a major mode `js-mode' for JavaScript. We
;; don't use it (because `web-mode' is better), but we still configure
;; some of its variables because `json-mode' uses them.
(use-package js
  :config

  ;; Default is 4, and nobody should indent JSON with four spaces.
  (setq js-indent-level 2))

;; Package `web-mode' provides a major mode for HTML, CSS, JavaScript,
;; and every conceivable thing adjacent (TypeScript, JSX, TSX, PSP,
;; ASP, Handlebars, etc.) all at once.
(use-package web-mode
  ;; Unfortunately `web-mode' does not come with `auto-mode-alist'
  ;; autoloads. We have to establish them manually. This list comes
  ;; from the official website at <http://web-mode.org/> as of
  ;; 2018-07-09.
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ;; My additions.
         ("\\.ejs\\'" . web-mode)
         ("\\.[cm]?jsx?\\'" . web-mode)
         ("\\.tsx?\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.hbs\\'" . web-mode))
  ;; Use `web-mode' rather than `js-mode' for scripts.
  :interpreter (("js" . web-mode)
                ("node" . web-mode))
  :config

  ;; Indent by two spaces by default. Compatibility with Prettier.
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)

  ;; Not sure why anyone would want 1 space indent for inline scripts
  ;; and CSS. Set it to 2 for consistency.
  (setq web-mode-script-padding 2)
  (setq web-mode-style-padding 2)

  ;; Autocomplete </ instantly.
  (setq web-mode-enable-auto-closing t)

  ;; Insert matching tags automatically. Why this is "mode 2", I have
  ;; not the slightest idea.
  (setq web-mode-auto-close-style 2)

  ;; Don't insert quotes automatically. It messes with JSX.
  (setq web-mode-enable-auto-quoting nil)

  ;; Disable `web-mode' automatically reindenting a bunch of
  ;; surrounding code when you paste anything. It's real annoying if
  ;; it happens to not know how to indent your code correctly.
  (setq web-mode-enable-auto-indentation nil)

  ;; When using `web-mode' to edit JavaScript files, support JSX tags.
  (add-to-list 'web-mode-content-types-alist
               '("jsx" . "\\.[cm]?js[x]?\\'"))

  ;; Create line comments instead of block comments by default in
  ;; JavaScript. See <https://github.com/fxbois/web-mode/issues/619>.
  (let ((types '("javascript" "jsx")))
    (setq web-mode-comment-formats
          (cl-remove-if (lambda (item)
                          (member (car item) types))
                        web-mode-comment-formats))
    (dolist (type types)
      (push (cons type "//") web-mode-comment-formats)))

  (radian-defhook radian--web-js-fix-comments ()
    web-mode-hook
    "Fix comment handling in `web-mode' for JavaScript.
Note that this somewhat breaks HTML comments, but it's good
enough for the moment."

    ;; For some reason the default is to insert HTML comments even
    ;; in JavaScript.
    (setq-local comment-start "//")
    (setq-local comment-end "")

    ;; Needed since otherwise the default value generated by
    ;; `comment-normalize-vars' will key off the syntax and think
    ;; that a single "/" starts a comment, which completely borks
    ;; auto-fill.
    (setq-local comment-start-skip "// *")))

;;; Configuration file formats

;; Package `apache-mode' provides a major mode for .htaccess and
;; similar files.
(use-package apache-mode)

;; Package `crontab-mode' provides a major mode for crontab files.
(use-package crontab-mode)

;; Package `dockerfile-mode' provides a major mode for Dockerfiles.
(use-package dockerfile-mode)

;; Package `git-modes' provides major modes for .gitconfig,
;; .gitmodules, and .gitignore files.
(use-package git-modes)

;; Package `json-mode' provides a major mode for JSON.
(use-package json-mode
  :init/el-patch

  (defconst json-mode-standard-file-ext '(".json" ".jsonld")
    "List of JSON file extensions.")

  (defsubst json-mode--update-auto-mode (filenames)
    "Update the `json-mode' entry of `auto-mode-alist'.

FILENAMES should be a list of file as string.
Return the new `auto-mode-alist' entry"
    (let* ((new-regexp
            (rx-to-string
             `(seq (eval
                    (cons 'or
                          (append json-mode-standard-file-ext
                                  ',filenames)))
                   eot)))
           (new-entry (cons new-regexp 'json-mode))
           (old-entry (when (boundp 'json-mode--auto-mode-entry)
                        json-mode--auto-mode-entry)))
      (setq auto-mode-alist (delete old-entry auto-mode-alist))
      (add-to-list 'auto-mode-alist new-entry)
      new-entry))

  (defcustom json-mode-auto-mode-list '(".babelrc" ".bowerrc" "composer.lock")
    "List of filename as string to pass for the JSON entry of
`auto-mode-alist'.

Note however that custom `json-mode' entries in `auto-mode-alist'
won’t be affected."
    :group 'json-mode
    :type '(repeat string)
    :set (lambda (symbol value)
           "Update SYMBOL with a new regexp made from VALUE.

This function calls `json-mode--update-auto-mode' to change the
`json-mode--auto-mode-entry' entry in `auto-mode-alist'."
           (set-default symbol value)
           (setq json-mode--auto-mode-entry
                 (json-mode--update-auto-mode value))))

  (defvar json-mode--auto-mode-entry
    (json-mode--update-auto-mode json-mode-auto-mode-list)
    "Regexp generated from the `json-mode-auto-mode-list'.")

  :config

  (radian-defhook radian--fix-json-indentation ()
    json-mode-hook
    "Set the tab width to 2 for JSON."
    (setq-local tab-width 2)))

;; Package `pip-requirements' provides a major mode for
;; requirements.txt files used by Pip.
(use-package pip-requirements

  ;; The default mode lighter is "pip-require". Ew.
  :blackout "Requirements")

;; Package `pkgbuild-mode' provides a major mode for PKGBUILD files
;; used by Arch Linux and derivatives.
(use-package pkgbuild-mode)

;; Package `ssh-config-mode' provides major modes for files in ~/.ssh.
(use-package ssh-config-mode
  :blackout "SSH-Config")

;; Package `yaml-mode' provides a major mode for YAML.
(use-package yaml-mode)

;; When the lines in a buffer are so long that performance could suffer to an unacceptable degree, we say “so long”2 to the buffer’s major mode
(global-so-long-mode 1)
;;; Introspection

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
         ("C-c C-d" . #'helpful-at-point))

  :config

  ;; Make it so you can quit out of `helpful-key' with C-g, like for
  ;; every other command. Put this in a minor mode so it can be
  ;; disabled.
  (define-minor-mode radian-universal-keyboard-quit-mode
    "Minor mode for making C-g work in `helpful-key'."
    :global t
    (if radian-universal-keyboard-quit-mode
        (radian-defadvice radian--advice-helpful-key-allow-keyboard-quit
            (&rest _)
          :before #'helpful-key
          "Make C-g work in `helpful-key'."
          ;; The docstring of `add-function' says that if we make our
          ;; advice interactive and the interactive spec is *not* a
          ;; function, then it overrides the original function's
          ;; interactive spec.
          (interactive
           (list
            (let ((ret (read-key-sequence "Press key: ")))
              (when (equal ret "\^G")
                (signal 'quit nil))
              ret))))
      (advice-remove
       #'helpful-key #'radian--advice-helpful-key-allow-keyboard-quit)))

  (radian-universal-keyboard-quit-mode +1)

  (radian-defadvice radian--advice-helpful-clone-emacs-source (library-name)
    :before #'helpful--library-path
    "Prompt user to clone Emacs source code when looking up functions.
Otherwise, it only happens when looking up variables, for some
bizarre reason."
    (when (member (file-name-extension library-name) '("c" "rs"))
      (radian-clone-emacs-source-maybe))))

;;;; Emacs Lisp development

(defun radian-eval-buffer-or-region (&optional start end)
  "Evaluate the current region, or the whole buffer if no region is active.
In Lisp code, START and END denote the region to be evaluated;
they default to `point-min' and `point-max' respectively.

If evaluating a buffer visiting this file, then delegate instead
to `radian-reload-init'."
  (interactive)
  (if (and buffer-file-name
           (member (file-truename buffer-file-name)
                   (list
                    (when (bound-and-true-p early-init-file)
                      (file-truename early-init-file))
                    (file-truename user-init-file)
                    (file-truename radian-lib-file)
                    (file-truename radian-local-init-file)))
           (not (region-active-p)))
      (radian-reload-init)
    (let ((name nil))
      (if (region-active-p)
          (progn
            (setq start (region-beginning))
            (setq end (region-end))
            (setq name "region"))
        (setq start (point-min))
        (setq end (point-max))
        (setq name (buffer-name)))
      (let ((load-file-name (buffer-file-name)))
        (message "Evaluating %s..." name)
        (eval-region start end)
        (message "Evaluating %s...done" name)))))

;; This keybinding is used for evaluating a buffer of Clojure code in
;; CIDER, and for evaluating a buffer of Scheme code in Geiser.
(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (bind-key "C-c C-k" #'radian-eval-buffer-or-region map))

(defun radian-find-symbol (&optional symbol)
  "Same as `xref-find-definitions' but only for Elisp symbols.
SYMBOL is as in `xref-find-definitions'."
  (interactive)
  (let ((xref-backend-functions '(elisp--xref-backend))
        ;; Make this command behave the same as `find-function' and
        ;; `find-variable', i.e. always prompt for an identifier,
        ;; defaulting to the one at point.
        (xref-prompt-for-identifier t))
    (if symbol
        (xref-find-definitions symbol)
      (call-interactively 'xref-find-definitions))))

;; By default, C-h f, C-h v, and C-h o are bound to
;; `describe-function', `describe-variable', and `describe-symbol'
;; respectively. By analogy, C-h C-f, C-h C-v, and C-h C-o should be
;; bound as follows. (There's no `find-symbol' function by default for
;; some reason; note that `xref-find-definitions' is not a replacement
;; because it is major-mode dependent.) By further analogy, we should
;; bind `find-library'.
(bind-key "C-h C-f" #'find-function)
(bind-key "C-h C-v" #'find-variable)
(bind-key "C-h C-o" #'radian-find-symbol)
(bind-key "C-h C-l" #'find-library)

;; Let's establish a standard location for the Emacs source code.
(setq source-directory (expand-file-name "src" user-emacs-directory))

;; This is initialized to nil by `find-func' if the source is not
;; cloned when the library is loaded.
(setq find-function-C-source-directory
      (expand-file-name "src" source-directory))

(defun radian-clone-emacs-source-maybe ()
  "Prompt user to clone Emacs source repository if needed."
  (when (and (not (file-directory-p source-directory))
             (not (get-buffer "*clone-emacs-src*"))
             (yes-or-no-p "Clone Emacs source repository? "))
    (make-directory (file-name-directory source-directory) 'parents)
    (let ((compilation-buffer-name-function
           (lambda (&rest _)
             "*clone-emacs-src*")))
      (save-current-buffer
        (compile
         (format
          "git clone https://github.com/emacs-mirror/emacs.git %s"
          (shell-quote-argument source-directory)))))))

;; Feature `find-func' provides the ability for you to locate the
;; definitions of Emacs Lisp functions and variables.
(use-package find-func
  :config

  (radian-defadvice radian--advice-find-func-clone-emacs-source (&rest _)
    :before #'find-function-C-source
    "Clone Emacs source if needed to view definition."
    (radian-clone-emacs-source-maybe)))

;; Package `macrostep' provides a facility for interactively expanding
;; Elisp macros.
(use-package macrostep
  :bind (("C-c e" . #'macrostep-expand)))

;;;;; Emacs Lisp byte-compilation


;;;;; Emacs Lisp linting

;; Feature `checkdoc' provides some tools for validating Elisp
;; docstrings against common conventions.
(use-package checkdoc
  :init

  ;; Not sure why this isn't included by default.
  (put 'checkdoc-package-keywords-flag 'safe-local-variable #'booleanp))

;; Package `elisp-lint', not installed, provides a linting framework
;; for Elisp code. We use `with-eval-after-load' because `use-package'
;; is configured to try to `require' features during byte-compilation.
(with-eval-after-load 'elisp-lint
  :init

  ;; From the package. We need this because some packages set this as
  ;; a file-local variable, but we don't install the package so Emacs
  ;; doesn't know the variable is safe.
  (put 'elisp-lint-indent-specs 'safe-local-variable #'listp))

;; Package `package-lint' provides a command that lets you check for
;; common package.el packaging problems in your packages.
(use-package package-lint)

;;; Applications
;;;; Organization

;; Use `use-package' here because we already installed Org earlier.

;; Package `org' provides too many features to describe in any
;; reasonable amount of space. It is built fundamentally on
;; `outline-mode', and adds TODO states, deadlines, properties,
;; priorities, etc. to headings. Then it provides tools for
;; interacting with this data, including an agenda view, a time
;; clocker, etc. There are *many* extensions.
(use-package org
  :bind (:map org-mode-map

              ;; By default, Org maps C-<up> to
              ;; `org-backward-paragraph' instead of
              ;; `backward-paragraph' (and analogously for C-<down>).
              ;; However, it doesn't do the same remapping for the
              ;; other bindings of `backward-paragraph' (e.g. M-{).
              ;; Here we establish that remapping. (This is important
              ;; since we remap C-<up> and C-<down> to other things,
              ;; above. So otherwise there would be no easy way to
              ;; invoke `org-backward-paragraph' and
              ;; `org-forward-paragraph'.)
              ([remap backward-paragraph] . #'org-backward-paragraph)
              ([remap forward-paragraph] . #'org-forward-paragraph)

              ;; See discussion of this function below.
              ("C-M-RET" . #'radian-org-insert-heading-at-point)
              ("C-M-<return>" . #'radian-org-insert-heading-at-point))
  :bind* (;; Add the global keybindings for accessing Org Agenda and
          ;; Org Capture that are recommended in the Org manual.
          ("C-c a" . #'org-agenda)
          ("C-c c" . #'org-capture))
  :config

  ;; If you try to insert a heading in the middle of an entry, don't
  ;; split it in half, but instead insert the new heading after the
  ;; end of the current entry.
  (setq org-insert-heading-respect-content t)

  ;; But add a new function for recovering the old behavior (see
  ;; `:bind' above).
  (defun radian-org-insert-heading-at-point ()
    "Insert heading without respecting content.
This runs `org-insert-heading' with
`org-insert-heading-respect-content' bound to nil."
    (interactive)
    (let ((org-insert-heading-respect-content nil))
      (org-insert-heading)))

  ;; Show headlines but not content by default.
  (setq org-startup-folded 'content)

  ;; Make it possible to dim or hide blocked tasks in the agenda view.
  (setq org-enforce-todo-dependencies t)

  ;; Make C-a, C-e, and C-k smarter with regard to headline tags.
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)

  (put 'org-tags-exclude-from-inheritance 'safe-local-variable
       #'radian--list-of-strings-p)

  ;; When you create a sparse tree and `org-indent-mode' is enabled,
  ;; the highlighting destroys the invisibility added by
  ;; `org-indent-mode'. Therefore, don't highlight when creating a
  ;; sparse tree.
  (setq org-highlight-sparse-tree-matches nil)

  ;; Default org directory
  (setq org-agenda-files '("~/org/"))

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
        org-ellipsis (if (and (display-graphic-p) (char-displayable-p ?⏷)) "\t⏷" nil)
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
	   "* Topic: %^{Description}  %^g %? Added: %U")))


  ;; Custom macros for LA
(fset 'copy-previous-analysis
   (kmacro-lambda-form [?\C-n ?\C-c ?\C-p ?\M-f ?\C-f ?\C-\M-@ ?\M-w ?\C-r ?\C-y ?\C-r return ?\M-x ?o ?r ?g ?- ?s ?h ?o ?w ?- ?s ?u ?b ?t ?r ?e ?e return ?\C-c ?\C-n ?\M-b ?\M-f ?\C-  ?\C-r ?- ?- ?- return ?\C-a ?\M-w ?\C-u ?\C-  ?\C-u ?\C-  ?\M-x ?o ?r ?g ?- ?s ?h ?o ?w ?- ?s ?u ?b ?t ?r ?e ?e return ?\C-c ?\C-n ?\C-o ?\C-y ?\C-c ?\C-t ?d] 0 "%d"))


  (define-key org-mode-map (kbd "C-x C-z a") 'copy-previous-analysis)


  (fset 'append-analysis
        (kmacro-lambda-form [?\C-n ?\C-c ?\C-p ?\C-c ?\C-n ?\C-  ?\C-r ?- ?- ?- return ?\C-a ?\M-w ?\C-c ?\C-p ?\M-f ?\C-f ?\C-\M-@ ?\M-w ?\M-> ?\C-r ?\C-y return ?\C-c ?\C-n ?\C-o ?\C-y ?\M-y] 0 "%d"))

  (define-key org-mode-map (kbd "C-x C-z A") 'append-analysis)


  (fset 'goto-previous-analysis
        (kmacro-lambda-form [?\C-n ?\C-c ?\C-p ?\M-f ?\C-f ?\C-\M-@ ?\M-w ?\C-r ?\C-y ?\C-r return ?\M-x ?o ?r ?g ?- ?s ?h ?o ?w ?- ?s ?u ?b ?t ?r ?e ?e return] 0 "%d"))

  (define-key org-mode-map (kbd "C-x C-z s") 'goto-previous-analysis)

  (fset 'TR
        (kmacro-lambda-form [?\C-a escape ?\C-  ?\M-w escape ?\C-  ?\C-c ?\C-l ?h ?t ?t ?p ?s ?: ?/ ?/ ?m ?h ?w ?e ?b ?. ?e ?r ?i ?c ?s ?s ?o ?n ?. ?s ?e ?/ ?T ?R ?E ?d ?i ?t ?W ?e ?b ?/ ?f ?a ?c ?e ?s ?/ ?o ?o ?/ ?o ?b ?j ?e ?c ?t ?. ?x ?h ?t ?m ?l ?? ?e ?r ?i ?r ?e ?f ?= ?\C-y return return] 0 "%d"))

  (define-key org-mode-map (kbd "C-x C-z T") 'TR)

  (fset 'ticket
        (kmacro-lambda-form [?\C-a escape ?\C-  ?\C-w ?\C-c ?\C-l ?h ?t ?t ?p ?s ?: ?/ ?/ ?e ?t ?e ?a ?m ?p ?r ?o ?j ?e ?c ?t ?. ?i ?n ?t ?e ?r ?n ?a ?l ?. ?e ?r ?i ?c ?s ?s ?o ?n ?. ?c ?o ?m ?/ ?b ?r ?o ?w ?s ?e ?/ ?P backspace ?\C-y return ?\C-y return] 0 "%d"))

  (define-key org-mode-map (kbd "C-x C-z t") 'ticket)

  (fset 'posijediti
   (kmacro-lambda-form [?\C-a escape ?\C-f ?\C-f ?\C-f ?\C-f ?= ?\C-d ?\C-e ?= ?\C-n] 0 "%d"))

  (define-key org-mode-map (kbd "C-x C-z p") 'posijediti))


;;;;; Dired


;; Search DIR recursively for files matching the globbing pattern PATTERN
(global-set-key (kbd "C-<") 'find-name-dired)
;; Find files in DIR that contain matches for REGEXP
(global-set-key (kbd "C->") 'find-grep-dired)

;; Feature `dired' provides a simplistic filesystem manager in Emacs.
(with-eval-after-load "dired"

  ;; Dired has some trouble parsing out filenames that have e.g. leading
  ;; spaces, unless the ls program used has support for Dired. GNU ls
  ;; has this support, so if it is available we tell Dired (and the
  ;; `list-directory' command, not that it sees much use) to use it.
  ;;
  ;; This is in an advice so that we can defer the PATH search until
  ;; necessary.
  (radian-defadvice radian--use-gls-for-list-directory (&rest _)
    :before #'list-directory
    "Make Dired use GNU ls, if it is available."
    (when (executable-find "gls")
      (setq insert-directory-program "gls"))
    ;; Only do the check once, for efficiency.
    (advice-remove #'list-directory #'radian--use-gls-for-list-directory))

  (radian-defadvice radian--advice-dired-check-for-ls-dired (&rest _)
    :before #'dired-insert-directory
    "Check if ls --dired is supported ahead of time, and silently.

This advice prevents Dired from printing a message if your ls
does not support the --dired option. (We do this by performing
the check ourselves, and refraining from printing a message in
the problematic case.)"
    (when (eq dired-use-ls-dired 'unspecified)
      (setq dired-use-ls-dired
            (eq 0 (call-process insert-directory-program
                                nil nil nil "--dired")))))

  (define-key dired-mode-map (kbd "J") 'dired-up-directory)

  (add-hook 'dired-mode-hook #'radian--autorevert-silence)

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
  (setq dired-dwim-target t))

;; want dired-x loaded when I press dired-jump command
(use-package dired-x
  :straight nil
  :bind (;; Bindings for jumping to the current directory in Dired.
         ("C-x C-j" . #'dired-jump)
         ("C-x 4 C-j" . #'dired-jump-other-window))
  :config
  ;; Prevent annoying "Omitted N lines" messages when auto-reverting.
  (setq dired-omit-verbose nil))


;;;; Terminal emulator

;; Feature `term' provides a workable, though slow, terminal emulator
;; within Emacs.
(use-package term
  :bind (;; Allow usage of more commands from within the terminal.
         :map term-raw-map
         ("M-x" . #'execute-extended-command)
         ("C-h" . #'help-command)))

;;;; Version control

;; Comparing files and buffers, and finding differences.
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

;; Feature `smerge-mode' provides an interactive mode for visualizing
;; and resolving Git merge conflicts.
(use-package smerge-mode
  :blackout t)

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

  :config/el-patch

  ;; Prevent Emacs asking if we're sure we want to exit, if a
  ;; Magit-spawned git-credential-cache process is running.
  (defun magit-maybe-start-credential-cache-daemon ()
    "Maybe start a `git-credential-cache--daemon' process.

If such a process is already running or if the value of option
`magit-credential-cache-daemon-socket' is nil, then do nothing.
Otherwise start the process passing the value of that options
as argument."
    (unless (or (not magit-credential-cache-daemon-socket)
                (process-live-p magit-credential-cache-daemon-process)
                (memq magit-credential-cache-daemon-process
                      (list-system-processes)))
      (setq magit-credential-cache-daemon-process
            (or (--first (let* ((attr (process-attributes it))
                                (comm (cdr (assq 'comm attr)))
                                (user (cdr (assq 'user attr))))
                           (and (string= comm "git-credential-cache--daemon")
                                (string= user user-login-name)))
                         (list-system-processes))
                (condition-case nil
                    (el-patch-wrap 2
                      (with-current-buffer
                          (get-buffer-create " *git-credential-cache--daemon*")
                        (start-process "git-credential-cache--daemon"
                                       (el-patch-swap
                                         " *git-credential-cache--daemon*"
                                         (current-buffer))
                                       (magit-git-executable)
                                       "credential-cache--daemon"
                                       magit-credential-cache-daemon-socket)
                        (el-patch-add
                          (set-process-query-on-exit-flag
                           (get-buffer-process (current-buffer)) nil))))
                  ;; Some Git implementations (e.g. Windows) won't have
                  ;; this program; if we fail the first time, stop trying.
                  ((debug error)
                   (remove-hook
                    'magit-credential-hook
                    #'magit-maybe-start-credential-cache-daemon)))))))

  :config

  ;; The default location for git-credential-cache is in
  ;; ~/.config/git/credential. However, if ~/.git-credential-cache/
  ;; exists, then it is used instead. Magit seems to be hardcoded to
  ;; use the latter, so here we override it to have more correct
  ;; behavior.
  (unless (file-exists-p "~/.git-credential-cache/")
    (let* ((xdg-config-home (or (getenv "XDG_CONFIG_HOME")
                                (expand-file-name "~/.config/")))
           (socket (expand-file-name "git/credential/socket" xdg-config-home)))
      (setq magit-credential-cache-daemon-socket socket)))

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
    '("-u" "Unshallow" "--unshallow")))


;; Package `emacsql-sqlite' is a dependency of Forge which is used to
;; interact with the SQLite database that Forge uses to keep track of
;; information about pull requests.
(use-package emacsql-sqlite
  :init

  ;; Put the EmacSQL binary in the repository, not the build dir. That
  ;; way we don't have to recompile it every time packages get rebuilt
  ;; by straight.el. See
  ;; <https://github.com/radian-software/straight.el/issues/274> for not
  ;; having to use the internal function `straight--dir'.
  (setq emacsql-sqlite-data-root (straight--repos-dir "emacsql"))

  :config

  (radian-defadvice radian--advice-emacsql-no-compile-during-compile
      (&rest _)
    :before-until #'emacsql-sqlite-ensure-binary
    "Prevent EmacSQL from trying to compile stuff during byte-compilation.
This is a problem because Forge tries to get EmacSQL to compile
its binary at load time, which is bad (you should never do
anything significant at package load time) since it breaks CI."
    byte-compile-current-file))

;; Package `forge' provides a GitHub/GitLab/etc. interface directly
;; within Magit.
(use-package forge)

;; Package `git-gutter' adds a column to the left-hand side of each
;; window, showing which lines have been added, removed, or modified
;; since the last Git commit.
(use-package git-gutter
  :commands (git-gutter:previous-hunk
             git-gutter:next-hunk
             radian-git-gutter:beginning-of-hunk
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

  (radian-defhook radian--git-gutter-load ()
    find-file-hook
    "Load `git-gutter' when initially finding a file."
    (require 'git-gutter)
    (remove-hook 'find-file-hook #'radian--git-gutter-load))

  :config

  ;; Don't prompt when reverting hunk.
  (setq git-gutter:ask-p nil)

  (global-git-gutter-mode +1)

  (defun radian-git-gutter:beginning-of-hunk ()
    "Move to beginning of current diff hunk."
    (interactive)
    (git-gutter:awhen (git-gutter:search-here-diffinfo git-gutter:diffinfos)
      (let ((lines (- (git-gutter-hunk-start-line it) (line-number-at-pos))))
        ;; This will move backwards since lines will be negative.
        (forward-line lines))))

  ;; Shuffle around all the hooks. `git-gutter' puts itself on a bunch
  ;; of different things, but not exactly the right things. Remove all
  ;; its meddling, and then do the right thing (run on window or
  ;; buffer switch after a top-level command, after a buffer revert,
  ;; and after Apheleia runs).

  (remove-hook 'post-command-hook #'git-gutter:post-command-hook)
  (advice-remove #'quit-window #'git-gutter:quit-window)
  (advice-remove #'switch-to-buffer #'git-gutter:switch-to-buffer)

  (defvar radian--git-gutter-last-buffer-and-window nil
    "Cons of current buffer and selected window before last command.
This is used to detect when the current buffer or selected window
changes, which means that `git-gutter' needs to be re-run.")

  (radian-defhook radian--git-gutter-on-buffer-or-window-change ()
    post-command-hook
    "Update `git-gutter' when current buffer or selected window changes."
    (let ((new (cons (current-buffer) (selected-window))))
      (unless (equal new radian--git-gutter-last-buffer-and-window)
        (setq radian--git-gutter-last-buffer-and-window new)
        ;; Sometimes the current buffer has not gotten updated yet
        ;; after switching window, for example after `quit-window'.
        (with-current-buffer (window-buffer)
          (when git-gutter-mode
            (when buffer-file-name
              (unless (file-remote-p buffer-file-name)
                (git-gutter))))))))

  (use-package autorevert
    :config

    (radian-defhook radian--git-gutter-after-autorevert ()
      after-revert-hook
      "Update `git-gutter' after the buffer is autoreverted."
      (when git-gutter-mode
        (git-gutter))))

  (use-package apheleia
    :config

    (radian-defhook radian--git-gutter-after-apheleia ()
      apheleia-post-format-hook
      "Update `git-gutter' after Apheleia formats the buffer."
      (when git-gutter-mode
        (git-gutter))))

  :blackout git-gutter-mode)

;; Package `git-gutter-fringe' integrates with `git-gutter' to make
;; the gutter display use the window fringe rather than a column of
;; text.
;;
;; Note that we only even put the package on the load path if
;; `git-gutter-fringe' fringe is defined. The function might not be
;; defined if Emacs was not built with X/Cocoa support, and if that's
;; the case, then loading it will cause errors (and besides that, will
;; break `git-gutter' since the fringe stuff is not available).
;; However, we do need to load the package in order to byte-compile
;; this configuration. That's okay since it's only done in a
;; subprocess (so it won't break `git-gutter') but we still need to
;; fix the errors in that case. Hence the `eval-when-compile'.
(straight-register-package 'git-gutter-fringe)
(when (fboundp 'define-fringe-bitmap)
  (eval-when-compile
    (unless (fboundp 'define-fringe-bitmap)
      (fset 'define-fringe-bitmap #'ignore))
    (unless (boundp 'overflow-newline-into-fringe)
      (setq overflow-newline-into-fringe t)))
  (use-package git-gutter-fringe
    :demand t
    :after git-gutter
    :config

    (fringe-helper-define 'radian--git-gutter-blank nil
      "........"
      "........"
      "........"
      "........"
      "........"
      "........"
      "........"
      "........")

    (radian-defadvice radian--advice-git-gutter-remove-bitmaps
        (func &rest args)
      :around #'git-gutter-fr:view-diff-infos
      "Disable the cutesy bitmap pluses and minuses from `git-gutter-fringe'.
Instead, display simply a flat colored region in the fringe."
      (radian-flet ((defun fringe-helper-insert-region
                        (beg end _bitmap &rest args)
                      (apply fringe-helper-insert-region
                             beg end 'radian--git-gutter-blank args)))
        (apply func args)))))

;;;; External commands

;; Feature `compile' provides a way to run a shell command from Emacs
;; and view the output in real time, with errors and warnings
;; highlighted and hyperlinked.
(use-package compile
  :config

  ;; By default, run from root of current Git repository.
  (setq compile-command "git exec make ")

  ;; Automatically scroll the Compilation buffer as output appears,
  ;; but stop at the first error.
  (setq compilation-scroll-output 'first-error)

  ;; Don't ask about saving buffers when invoking `compile'. Try to
  ;; save them all immediately using `save-some-buffers'.
  (setq compilation-ask-about-save nil)

  ;; Actually, don't bother saving buffers at all. That's dumb. We
  ;; know to save our buffers if we want them to be updated on disk.
  (setq compilation-save-buffers-predicate
        (lambda ()))

  (radian-defadvice radian--advice-compile-pop-to-buffer (buf)
    :filter-return #'compilation-start
    "Pop to compilation buffer on \\[compile]."
    (prog1 buf
      (when-let ((win (get-buffer-window buf)))
        (select-window win)))))

;; Package `rg' just provides an interactive command `rg' to run the
;; search tool of the same name.
(use-package rg
  :bind (("M-s R" . #'rg))
  :config
  (rg-enable-default-bindings))

;;;; Internet applications

;; Feature `browse-url' provides commands for opening URLs in
;; browsers.
(use-package browse-url
  :init

  (defun radian--browse-url-predicate ()
    "Return non-nil if \\[browse-url-at-point] should be rebound."
    ;; All of these major modes provide more featureful bindings for
    ;; C-c C-o than `browse-url-at-point'.
    (not (derived-mode-p
          #'markdown-mode #'org-mode #'org-agenda-mode #'magit-mode)))

  :bind* (:filter (radian--browse-url-predicate)
                  ("C-c C-o" . #'browse-url-at-point)))

;;; Startup

(radian-defadvice radian--advice-inhibit-startup-message (&rest _)
  :override #'display-startup-echo-area-message
  "Unconditionally inhibit the startup message in the echo area.
This is the message that reads \"For more information about GNU
Emacs...\". Emacs suggests setting
`inhibit-startup-echo-area-message' to your username so that
other people using your configuration will still get to see this
spam. This advice, however, inhibits the message for everyone.")

;;; Miscellaneous

;; Enable all disabled commands.
(setq disabled-command-function nil)

;; Disable warnings from obsolete advice system. They don't provide
;; useful diagnostic information and often they can't be fixed except
;; by changing packages upstream.
(setq ad-redefinition-action 'accept)

;; Explicitly set the mark
(defun push-mark-no-activate ()
  "Pushes 'point' to 'mark-ring' and does not activate the
region. Equivalent to \\[set-mark-command] when
\\[transient-mark-mode] is disabled."
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-`") 'push-mark-no-activate)
(defun jump-to-mark ()
  "Jumps to the local mark, respecting the 'mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix
argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)
(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

;; For commands that are useful to use in continuous stream,
;; defrepeater helps in a way that you don't need to press complete
;; keybinding sequence, just continue pressing last character
(use-package defrepeater
    :init
    (defrepeater #'winner-undo)
    (define-key winner-mode-map [remap winner-undo] 'winner-undo-repeat)
    (global-set-key (kbd "C-x o") (defrepeater #'other-window))
    )

;;; Appearance

;; Display keystrokes in the echo area immediately, not after one
;; second. We can't set the delay to zero because somebody thought it
;; would be a good idea to have that value suppress keystroke display
;; entirely.
(setq echo-keystrokes 1e-6)

;; Don't suggest shorter ways to type commands in M-x, since they
;; don't apply when using Vertico.
(setq suggest-key-bindings 0)

;; Don't blink the cursor on the opening paren when you insert a
;; closing paren, as we already have superior handling of that from
;; Smartparens.
(setq blink-matching-paren nil)

(radian-defadvice radian--advice-read-passwd-hide-char (func &rest args)
  :around #'read-passwd
  "Display passwords as **** rather than .... in the minibuffer.
This is the default behavior is Emacs 27, so this advice only has
an effect for Emacs 26 or below."
  (let ((read-hide-char (or read-hide-char ?*)))
    (apply func args)))

(setq minibuffer-message-properties '(face minibuffer-prompt))

(when (display-graphic-p)

  ;; Prevent the cursor from blinking. Do it two ways: using the minor
  ;; mode only works during regular init, while using the variable
  ;; only works during early init.
  (blink-cursor-mode -1)
  (setq no-blinking-cursor t)

  ;; Use the same font for fixed-pitch text as the rest of Emacs (you
  ;; *are* using a monospace font, right?).
  (set-face-attribute 'fixed-pitch nil :family 'unspecified))

;; cooking for pulse line
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))
(dolist (command '(scroll-up-command scroll-down-command
                                     recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))

;; The variable tab-bar-show controls whether the Tab Bar mode is
;; turned on automatically. If the value is t, then tab-bar-mode is
;; enabled when using the commands that create new tabs. The value 1
;; hides the tab bar when it has only one tab, and shows it again when
;; more tabs are created. The value nil always keeps the tab bar
;; hidden.
(setq tab-bar-show 1)

;;;; Mode line

;; The following code customizes the mode line to something like:
;; [*] radian.el   18% (18,0)     [radian:develop*]  (Emacs-Lisp)

(defun radian-mode-line-buffer-modified-status ()
  "Return a mode line construct indicating buffer modification status.
This is [*] if the buffer has been modified and whitespace
otherwise. (Non-file-visiting buffers are never considered to be
modified.) It is shown in the same color as the buffer name, i.e.
`mode-line-buffer-id'."
  (propertize
   (if (and (buffer-modified-p)
            (buffer-file-name))
       "[*]"
     "   ")
   'face 'mode-line-buffer-id))

;; Normally the buffer name is right-padded with whitespace until it
;; is at least 12 characters. This is a waste of space, so we
;; eliminate the padding here. Check the docstrings for more
;; information.
(setq-default mode-line-buffer-identification
              (propertized-buffer-identification "%b"))

;; Make `mode-line-position' show the column, not just the row.
(column-number-mode +1)

;; https://emacs.stackexchange.com/a/7542/12534
(defun radian--mode-line-align (left right)
  "Render a left/right aligned string for the mode line.
LEFT and RIGHT are strings, and the return value is a string that
displays them left- and right-aligned respectively, separated by
spaces."
  (let ((width (- (window-total-width) (length left))))
    (format (format "%%s%%%ds" width) left right)))

(defcustom radian-mode-line-left
  '(;; Show [*] if the buffer is modified.
    (:eval (radian-mode-line-buffer-modified-status))
    " "
    ;; Show the name of the current buffer.
    mode-line-buffer-identification
    "   "
    ;; Show the row and column of point.
    mode-line-position
    ;; Show the active major and minor modes.
    "  "
    mode-line-modes)
  "Composite mode line construct to be shown left-aligned."
  :type 'sexp)

(defcustom radian-mode-line-right nil
  "Composite mode line construct to be shown right-aligned."
  :type 'sexp)

;; Actually reset the mode line format to show all the things we just
;; defined.
(setq-default mode-line-format
              '(:eval (replace-regexp-in-string
                       "%" "%%"
                       (radian--mode-line-align
                        (format-mode-line radian-mode-line-left)
                        (format-mode-line radian-mode-line-right))
                       'fixedcase 'literal)))

;;;; Color theme

(use-package modus-themes)
(load-theme 'modus-operandi t)

;;; Closing


;; Prune the build cache for straight.el; this will prevent it from
;; growing too large. Do this after the final hook to prevent packages
;; installed there from being pruned.
(straight-prune-build-cache)

;; Occasionally prune the build directory as well. For similar reasons
;; as above, we need to do this after local configuration.
(unless (bound-and-true-p radian--currently-profiling-p)
  (when (= 0 (random 100))
    (straight-prune-build-directory)))

;; Local Variables:
;; checkdoc-symbol-words: ("top-level")
;; indent-tabs-mode: nil
;; no-native-compile: t
;; outline-regexp: ";;;+ "
;; sentence-end-double-space: nil
;; End:
