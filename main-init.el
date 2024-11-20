;; Preamble
;;     This is at the beginning of main-init.el:

;; This file is generated from main-init.org
;; using (org-babel-tangle).  Do not
;; make changes in this file; they'll be lost!
(provide 'main-init)

;; Profile startup times
;;     This is from a good [[https://github.com/raxod502/straight.el][article]] about speeding up emacs startup.

;;     Use a hook so the message doesn't get clobbered by other messages.

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; GC threshold
;;     Temporarily increase GC threshold to 100MB then reduce to 8MB
;;     (was 800KB) after startup.

(setq gc-cons-threshold (* 100 1000 1000))
(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold (* 8 1000 1000))))

;; Setup proxies, certificates for package installation
;;     To get =package-install= or =straight.el= to work you may need to setup
;;     proxies and SSL certificates for those proxies

;;     Add certificate for corp proxies.  No problem if file doesn't exist.


(require 'gnutls)
(add-to-list 'gnutls-trustfiles (expand-file-name "~/.ssh/BBrootNEW.cer"))



;; This is specific to a corp desktop pc keyed off
;; the assumption I only ever run cygwin in that environment.


(when (or (string-equal system-type "windows-nt")
          (string-equal system-type "cygwin"))
  (setq password-cache-expiry nil)
  (setq url-proxy-services '(("http" . "proxy.bloomberg.com:81"))))



;; Assume if this is Mac OS X that I've setup nodeproxy for corp
;; access.


;; (when (eq system-type 'darwin)
;;   (setq url-proxy-services '(("http" . "localhost:8888")
;;                              ("https" . "localhost:8888"))))

;; Do some weird Mac OS X stuff for my environment

(when (eq system-type 'darwin)
    (setenv "PATH" "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_10:/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_10")
    (setq exec-path (cons "/usr/local/bin" exec-path)))

;; Configure frame geometry, fonts, transparency,


(scroll-bar-mode -1)
(setq-default cursor-type 'box)
(setq-default cursor-in-non-selected-windows 'hollow)
(setq-default line-spacing .2)
(setq initial-frame-alist
      '((width . 110)
        (height . 60)
        (top . 29)
        (left . 88)
        (cursor-color . "orange")))
(setq default-frame-alist
      '((menu-bar-lines . 0)
        (right-divider-width . 5)
        (ns-transparent-titlebar . t)
        (cursor-color . "orange")
        ;(inhibit-double-buffering . t)
        (vertical-scroll-bars . nil)))
;(set-face-attribute 'cursor nil :background "orange")
(when (eq 'ns (window-system))
  (add-to-list 'default-frame-alist
               '(alpha . (100 . 100))))

;; Install the straight.el package manager

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;(setq straight-use-package-by-default t)

;; package
;;     Use the emacs packaging system to automatically install some packages


(unless (boundp 'bootstrap-version)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  ;; (add-to-list 'package-archives
  ;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (package-initialize))

;; Setup use-package
;;    You may need to =M-x package-install use-package= before
;;    any of this works
   
;;    If a package is not available then ~use-package~ ignores it.
;;    You can also not use a package by adding =:disabled t= to use-package

;;    I also like having ~use-package~ collect some info about
;;    the loaded packages and how long they take to load.  You
;;    can see the results with =M-x use-package-report=.


(setq use-package-verbose t)
(straight-use-package 'use-package)
(setq use-package-compute-statistics t)
(require 'use-package)

;; bind-key
;;     Using bind-key lets you run =M-x describe-personal-keybindings=
;;     which is a nice way of keep track of what you've changed.

(use-package bind-key
  :bind (
         ;("C-c G" . 'goto-line)
         ("C-c o" . 'other-frame)
         ("<wheel-left>" . 'ignore)
         ("<wheel-right>" . 'ignore)
         ("<double-wheel-left>" . 'ignore)
         ("<double-wheel-right>" . 'ignore)
         ("<triple-wheel-left>" . 'ignore)
         ("<triple-wheel-right>" . 'ignore)
         ))

;; treesit-auto


;; (use-package treesit-auto
;;   :straight t
;;   :disabled t
;;   :config
;;   (progn
;;     (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
;;     (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
;;     (add-to-list 'major-mode-remap-alist
;;                  '(c-or-c++-mode . c-or-c++-ts-mode))
;;     (setq treesit-auto-install t)
;;     (global-treesit-auto-mode)))

;; cc-mode
;;     Configure to put .h in c++-mode

(use-package cc-mode
  :commands (c-mode c++-mode)
  :mode ("\\.h$" . c++-mode)
  :config
  (setq c-tab-always-indent nil))

;; go-mode
;;     Load `go-mode' and enable it in 'lsp-deferred-mode'

(use-package go-mode
  :straight t
  :hook eglot-ensure)

;; delsel
;;     I can't handle the active region getting deleted


(use-package delsel
  :config
  (delete-selection-mode -1))

;; desktop
;;     This causes the set of files being visited to be restored
;;     on startup.

(use-package desktop
  :config
  (progn
    (setq desktop-save t)
    (setq desktop-dirname "~/.emacs.d/")
    (setq desktop-restore-frames nil)
    (setq desktop-restore-eager 0)
    (setq desktop-restore-in-current-display t)
    (setq desktop-lazy-verbose nil)
    (setq desktop-lazy-idle-delay 20)
    (setq desktop-auto-save-timeout 300)
    (setq desktop-files-not-to-save "^$")
    (setq desktop-load-locked-desktop t)
    (desktop-save-mode 1)
    (add-to-list 'desktop-modes-not-to-save 'Info-mode)
    (add-to-list 'desktop-modes-not-to-save 'dired-mode)
    ))

;; display line numbers
;;     This is the built-in line numbers added with Emacs 26.1

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))

;; executable
;;     This makes saving shell scripts automatically make
;;     them executable.  It's considered a shell script if
;;     it starts with #!


(use-package executable
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

;; face-remap
;;     Change the font size in the current buffer (not the window)


(use-package face-remap
  :bind* (("C-c -" . text-scale-adjust)
          ("C-c +" . text-scale-adjust)))

;; jit-lock
;;     Setup lazy font locking


(use-package jit-lock
  :straight nil
  :config
  (setq jit-lock-defer-time 0.1)
  (setq jit-lock-stealth-time 3)
  (jit-lock-mode t))

;; jka-cmpr-hook
;;     Make visiting a *.gz automatically uncompress file


(use-package jka-cmpr-hook
  :straight nil
  :defer 5
  :config
  (auto-compression-mode 1))

;; mwheel
;;     Make sure the mouse wheel scrolls


(use-package mwheel
  :config
  (progn
    (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
    (setq mouse-wheel-progressive-speed nil)
    (mwheel-install)))

;; delight

;;     This package makes it easy to hide minor modes in the modeline.
;;     This needs to be loaded fairly early as otherwise the =:delight=
;;     won't work in with =use-package=.

(use-package delight
  :straight t)

;; flymake

(use-package flymake
  :delight " FM")

;; outline


(use-package outline
  :delight
  :hook (prog-mode . outline-minor-mode))

;; paren
;;     Highlight matching paren


(use-package paren
  :config
  (progn
    (setq show-paren-when-point-in-periphery nil)
    (show-paren-mode 1)))

;; savehist

(use-package savehist
  ;;
  ;; Save emacs's internal command history.
  :defer 5
  :config
  (progn
    (setq savehist-additional-variables
          '(compile-command
            grep-find-history
            grep-history
            grep-regexp-history
            grep-files-history))
    (savehist-mode 1)))

;; saveplace
;;     This records the location of every file you visit and
;;     restores when you vist a file, goes to that location.  I also save
;;     the file every couple hours because I don't always quit emacs 


(use-package saveplace
  :config
  (progn
    (setq-default save-place t)
    (setq save-place-limit nil)
    (run-at-time 3600  3600 'save-place-alist-to-file)))

;; server
;;     Make it so $EDITOR can popup in this emacs


(use-package server
  :config
  (progn
    (if (not (string-match "emacsclient" (or (getenv "EDITOR") "")))
        (setenv "EDITOR" "emacsclient"))
    (message "server-start")
    (server-start)))

;; tool-bar
;;     Turn the toolbar off.  I also turn it off in my .Xdefaults with:
    
;;     Emacs.toolBar:            0

;;     which keeps it from displaying on startup

(use-package tool-bar
  :config
  (tool-bar-mode -1))

;; uniquify
;;     Make it so buffers with the same name are are made unique by added
;;     directory path and killing a buffer renames all of them.

(use-package uniquify
  :config
  (progn
    (setq uniquify-buffer-name-style 'post-forward)
    (setq uniquify-after-kill-buffer-p t)))

;; vc
;;     Configure version control

;;     I found visiting a file to be really slow and realized
;;     it was from figuring out the version control


(use-package vc
  :config
  (setq vc-display-status nil)
  (setq vc-handled-backends '(Git)))

;; xterm-mouse-mode
;;     Makes the mouse work when running in an xterm/iterm or other
;;     terminal emulator.  Only enabled when no graphics

(use-package xt-mouse
  :unless (display-graphic-p)
  :config
  (xterm-mouse-mode))

(defun pw/xterm()
  "Re-install xterm handlers for tmux purposes"
  (interactive)
  (terminal-init-xterm)
  (pw/xterm-mouse))

(defun pw/xterm-mouse ()
  "Toggle xterm-mouse-mode.  Useful when re-connecting
with tmux and state is lost"
  (interactive)
  (xterm-mouse-mode -1)
  (sit-for .1)
  (xterm-mouse-mode 1))

(defun iterm-cut-base64 (text)
  "Take TEXT and send it to iterm to copy."
  (interactive)
  (let ((base-64 (base64-encode-string text :no-line-break)))
    (send-string-to-terminal (concat "\e]1337;Copy=:" base-64 "\a"))))

(setq mouse-drag-copy-region t)
(unless (display-graphic-p)
  (setq interprogram-cut-function 'iterm-cut-base64))

;; menu-bar (disabled)
;;     Turn the menubar off on terminal windows
    

(use-package menu-bar
  :config
  (menu-bar-mode (if (display-graphic-p) 1 -1)))

;; auto-revert
;;     "This section exists to turn off the string in the modeline"
    

(use-package autorevert
  :delight auto-revert-mode)
(use-package outline
  :delight outline-minor-mode)

;; vertico
;;    This is the base package.


(use-package vertico
  :straight t
  :bind (:map vertico-map
              ("C-i" . vertico-quick-insert)
              ("C-o" . vertico-quick-exit)
              ("M-B" . vertico-buffer)
              ("M-G" . vertico-multiform-grid)
              ("M-F" . vertico-multiform-flat)
              ("M-R" . vertico-multiform-reverse)
              ("M-U" . vertico-multiform-unobtrusive)
              ("TAB" . minibuffer-complete))
  :init
  (progn
   ;; Configure the display per command.
   ;; Use a buffer with indices for imenu
   ;; and a flat (Ido-like) menu for M-x.
   (setq vertico-multiform-commands
         '((consult-imenu buffer indexed)
           (execute-extended-command grid)))

   ;; Configure the display per completion category.
   ;; Use the grid display for files and a buffer
   ;; for the consult-grep commands.
   (setq vertico-multiform-categories
         '((file grid)
           (consult-grep buffer)))

   (vertico-mode)
   (vertico-multiform-mode)))

;; orderless


(use-package orderless
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(substring orderless basic))
  ;(setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion)))))

;; Use =fd= to find files

(defun consult--fd-builder (input)
  (let ((fd-command
         (if (eq 0 (process-file-shell-command "fdfind"))
             "fdfind"
           "fd")))
    (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                 (`(,re . ,hl) (funcall consult--regexp-compiler
                                        arg 'extended t)))
      (when re
        (cons (append
               (list fd-command
                     "--color=never" "--full-path"
                     (consult--join-regexps re 'extended))
               opts)
              hl)))))

(defun consult-fd (&optional dir initial)
  (interactive "P")
  (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt "Fd" dir))
               (default-directory dir))
    (find-file (consult--find prompt #'consult--fd-builder initial))))

;; Consult setup

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :straight t
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c M-x" . consult-mode-command)
         ;("C-c h" . consult-history)
         ("C-c k" . consult-ripgrep)
                                        ;("C-c k" . consult-kmacro)
         ("C-c g" . consult-fd)
                                        ;("C-c g" . consult-find)
         ("C-c G" . consult-goto-line)
                                        ;("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
                                        ;("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
                                        ;("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
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
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
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
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+" >

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  )

;; Add =consult-dir= for visiting files


(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; popper
;;     This makes it easy to make temporary (aka popup) buffers
;;     take less space and quickly disappear

(use-package popper
  :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\*Messages\*"
          "Output\*$"
          "\\*Async Shell Command\\*"
          "\\*Flymake Diagnostic\\*"
          occur-mode
          flymake-mode
          flymake-diagnostics-buffer-mode
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;; embark

(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  ;(add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;(setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t
  :after embark
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; company
;;     company-mode adds asynchronous prompts.  It's particularly useful
;;     when using eglot (emacs's language server protocol interface).


(use-package company
  :straight t
  :delight
  :bind (("C-c ." . company-complete))
  :hook ((prog-mode . company-mode)
         (LaTeX-mode . company-mode)))

;; Consult search with silver searcher


(use-package consult-ag
  :disabled t
  :straight t
  :bind ("C-c k" . consult-ag))

;; marginalia adds decorations to completions


(use-package marginalia
  :straight t
  :bind (("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Python environments

;;     This allows one to manually switch the python
;;     venv being used by specifying the directory
;;     with the venv.


(use-package pyvenv
  :straight t)

;; eglot

;;     # Update PATH
;;     # pip install 'python-lsp-server[all]' pylsp-rope rope python-lsp-ruff ruff


(defun pw/eglot-format-safe()
  (interactive)
  (if eglot--managed-mode
      (eglot-format-buffer)))
(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c r" . eglot-rename)
              ("C-c i" . eglot-code-action-organize-imports))
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (after-save . pw/eglot-format-safe)
         (eglot-managed-mode .
                             (lambda () (setq eldoc-documentation-functions
                                              (cons #'flymake-eldoc-function
                                                    (remove #'flymake-eldoc-function eldoc-documentation-functions)))))))

;; eldoc
;;     Configure so documentation is displayed on a single
;;     line in the mini-buffer.  Use `C-c h' to get the
;;     full help in a box.


(use-package eldoc
  :delight
  :init
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package eldoc-box
  :straight t
  :delight eldoc-box-hover-mode
  :bind (:map eglot-mode-map
              ("C-c h" . eldoc-box-help-at-point))
                                        ;:hook (eglot-managed-mode . eldoc-box-hover-mode)
  )

;; prescient
;;     Provides better sorting of selections

(use-package prescient
  :straight t
  :disabled t
  :after (ivy counsel)
  :config
  (progn
    (prescient-persist-mode +1)))
(use-package ivy-prescient
  :after (ivy counsel)
  :disabled t
  :straight t
  :config
  (progn (ivy-prescient-mode +1)))

;; ivy
;;     ~ivy~ changes completion so that matches are
;;     found via regular expressions and matches are
;;     navigable by moving up and down lines.  Replaces
;;     ~ido~ and ~iswitchb~.

(use-package ivy
  :straight t
  :disabled t
  :delight ivy-mode
  :bind (("C-c C-r" . 'ivy-resume))
  :config (progn
            (setq ivy-wrap t)
            (setq ivy-use-virtual-buffers t)
            (setq ivy-count-format "(%d/%d) ")
            (ivy-mode 1)))


;; This adds some nice info when choosing buffers

(use-package lsp-ui
  :straight t)
(use-package lsp-ivy
  :disabled t
  :straight t
  :after (ivy counsel lsp-mode))

(use-package lsp-mode
  :disabled t
  :straight t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c++-mode . lsp))
  :commands lsp)

;; counsel
;;     ~counsel~ builds on completion for ivy but adds
;;     searches across files.

(use-package counsel
  :disabled t
  :after ivy
  :straight t
  :delight counsel-mode
  :bind (("C-c g" .  'counsel-git)
         ("C-c j" .  'counsel-file-jump)
         ("C-c k" .  'counsel-ag)
         ;("C-x b" .  'counsel-switch-buffer)
         ;("C-c s" .  'counsel-switch-to-shell-buffer)
         )
  :config 
  (progn (counsel-mode -1)
         (setq counsel-find-file-ignore-regexp "\\.*\\(pyc\\|.o\\|.tsk\\)$")))

;; bb-style
;;     Bloomberg C++ coding style

(use-package bb-style
  :hook (c-mode-common . bb-c-mode))

;; ctrlf
;;     This replaces =swiper= and built in incremental search

(use-package ctrlf
  :straight t
  :defer 5
  :config
  (progn
    (setq ctrlf-auto-recenter nil) ; 't fails when multiple buffers open
    (setq ctrlf-mode-bindings
          '(([remap isearch-forward        ] . ctrlf-forward-fuzzy-regexp)
            ([remap isearch-backward       ] . ctrlf-backward-fuzzy-regexp)
            ([remap isearch-forward-regexp ] . ctrlf-forward-regexp)))
    (ctrlf-mode +1)))

;; git-link
;;     ~git-link~ makes it easy to get the url link directly to a
;;     github repo.  The following adds setup for bbgithub.

(use-package git-link
  :straight t
  :bind (("C-c b l" . 'git-link)
         ("C-c b h" . 'git-link-homepage))
  :config
  (progn
    (add-to-list 'git-link-remote-alist
                 '("bbgithub\\.dev\\.bloomberg\\.com" git-link-github))
    (add-to-list 'git-link-commit-remote-alist
                 '("bbgithub\\.dev\\.bloomberg\\.com" git-link-commit-github))))

;; scratch-ext
;;     Make *scratch* buffers get saved


(use-package scratch-ext
  :defer 5
  :straight t
  :config
  (save-excursion
    (setq scratch-ext-log-directory "~/.emacs.d/scratch")
    (if (not (file-exists-p scratch-ext-log-directory))
        (mkdir scratch-ext-log-directory t))
    (scratch-ext-create-scratch-if-necessary)
    (set-buffer "*scratch*")
    (scratch-ext-restore-last-scratch)))

;; compile
;;     Setup compilation buffers


(use-package compile
  :bind ("C-c c" . compile)
  :config
  (progn
    (setq compilation-scroll-output 'first-error)))

;; clang-format+
;;     Runs clang-format.  This is not enabled by default.  You can enable this
;;     on a per-directory tree basis by adding the file `.dir-locals.el` that looks like this:
;;     #+BEGIN_EXAMPLE
;;       ((c++-mode . ((mode . clang-format+))))
;;     #+END_EXAMPLE
;;     or as a shell script
;;     #+BEGIN_EXAMPLE
;;       echo '((c++-mode . ((mode . clang-format+))))' > .dir-locals.el
;;     #+END_EXAMPLE

(use-package clang-format+
  :straight t)

;; ansi-color

(use-package ansi-color
  :after compile
  :config
  (progn
    (defun pw/colorize-compilation-buffer ()
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region compilation-filter-start (point-max))))
    (add-hook 'compilation-filter-hook 'pw/colorize-compilation-buffer)
    (setq ansi-color-names-vector ; better contrast colors
  	["black" "red4" "green4" "yellow4"
  	 "#8be9fd" "magenta4" "cyan4" "white"])
    (setq ansi-color-map (ansi-color-make-color-map))))

;; ediff
;;     A nice graphical diff Make sure that ediff ignores all whitespace
;;     differences and highlights the individual differences


(use-package ediff
  :commands ediff-load-version-control
  :bind (("C-c =" . pw/ediff-current))
  :config
  (progn
    (setq ediff-window-setup-function 'ediff-setup-windows-plain)
    (setq ediff-split-window-function 'split-window-horizontally)
    (setq ediff-diff-options "-w")
    (setq-default ediff-auto-refine 'on))
  :init
  (progn
    (defun pw/ediff-current (arg)
      "Run ediff-vc-internal on the current file against it's latest revision.
       If prefix arg, use it as the revision number"
      (interactive "P")
      (ediff-load-version-control t)
      (let ((rev (if arg (format "%d" arg) "")))
        (funcall
         (intern (format "ediff-%S-internal" ediff-version-control-package))
         rev "" nil)))))

;; follow
;;     This makes a single file wrap around between two windows.
;;     Try ^X-3 and then move to the top or bottom of the window
;;     and the other window scrolls.  I bound F7 to do get
;;     rid of the other windows and split.


(use-package follow
  :bind ("<f7>" . follow-delete-other-windows-and-split))

;; grep
;;     ~rgrep~ recursively greps for a pattern.  It uses a key to specify
;;     filenames and ignores directories like CVS.  "cchh" is all C++
;;     files and headers.


(use-package grep
  ;:bind (("C-c g" . grep))
  :defer 5
  :config
  (progn
    (setq grep-files-aliases
          '(("all" . "* .*")
            ("el" . "*.el")
            ("ch" . "*.[ch]")
            ("c" . "*.c")
            ("cc" . "*.cc *.cxx *.cpp *.C *.CC *.c++")
            ("cchh" . "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++")
            ("hh" . "*.hxx *.hpp *.[Hh] *.HH *.h++")
            ("h" . "*.h")
            ("l" . "[Cc]hange[Ll]og*")
            ("m" . "[Mm]akefile* *.mk")
            ("tex" . "*.tex")
            ("texi" . "*.texi")
            ("asm" . "*.[sS]")
            ("code" . "*.c *.C *.h *.cpp *.cc *.f *.py")))))

;; hideshow
;;     Setup commands and menus to hide/show blocks of code

(use-package hideshow
  :commands hs-minor-mode
  :init
  (progn
    (add-hook 'c++-mode-hook 'hs-minor-mode)
    (add-hook 'c-mode-hook 'hs-minor-mode)))

;; org
;;     org-mode provides an outline, todo, diary, calendar like interface.

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (;("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c r" . org-capture))
  :config (progn
            (setq org-export-backends '(ascii html icalendar latex md))
            (setq org-list-allow-alphabetical t)))



;; Additionally, I have a number of customizations I like to use
;; for org-mode.


(use-package org-prefs
  :after org)

;; whitespace
;;     Make "bad" whitespace be visible.  This causes tabs, and whitespace
;;     at beginning and end of the buffer as well as at the end of the
;;     line to highlight
    
;;     Use =M-x whitespace-cleanup= to fix all problems


(use-package whitespace
  :bind ("C-c SPC" . whitespace-mode)
  :config
  (progn
    (setq whitespace-style '(face trailing tabs empty indentation::space lines-tail))
    (setq whitespace-line-column nil)))

;; anyins
;;     Freaky way to insert text
;;     1. Enter anyins-mode
;;     2. Move around; mark spots you want to insert text with RET
;;     3. To insert text

;;        a. =y= inserts each line from kill ring at each marked spot, or
;;        b.  =!= runs a shell command line 'seq -s ". \n" 1 3' generates
;;            numbers "1. "  "2. " "3. " and inserts it at each markets tpot

(use-package anyins
  :disabled t
  :straight t
  :bind ("C-c i" . anyins-mode))

;; python


(use-package python
  :init (setq python-flymake-command '("ruff" "--quiet" "--stdin-filename=stdin" "-")))

;; ruff
;;     This enabled the python code formatter =ruff= to run when
;;     a python file is saved.


(use-package ruff-format
  :disabled t
  :straight t
  :hook (python-mode . ruff-format-on-save-mode))

;; black
;;     This enables the python code formater =black= to run when
;;     a python file is saved.

;;     I disabled this on 2024-05-31 to use ruff-mode to format



(use-package python-black
  :disabled t
  :straight t
  :init
  (progn
    (setq python-black-extra-args '("--line-length" "79")))
  :hook (python-mode . python-black-on-save-mode))

;; comint-prefs
    
;;     Setup preferences for shell, compile and other comint based commands


(use-package comint-prefs
  :after comint
  :commands (comint-for-pete dbx-for-pete comint-watch-for-password-prompt pw/turn-off-fontlock)
  :init
  (progn
    (setq comint-terminfo-terminal "emacs")
    (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
    (add-hook 'comint-mode-hook 'comint-for-pete)
    (add-hook 'dbx-mode-hook 'dbx-for-pete)
    (add-hook 'compilation-mode-hook 'pw/turn-off-fontlock)))

;; csc-mode
;;     Bloomberg database schema

(use-package csc-mode
  :mode ("\\.csc2$" . csc-mode))

;; dash-at-point
;;     This integrates with =dash= to lookup documentation.

(use-package dash-at-point
  :straight t
  :bind (
         ("C-c d" . 'dash-at-point)
         ("C-c D" . 'dash-at-point-with-docset))
  :config (progn
            (add-to-list 'dash-at-point-mode-alist '(c++-mode . "bde,cpp"))))

;; docker
;;     Replaces the docker ui with an emacs interface


(use-package docker
  :straight t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :straight t
  :mode
  ("Dockerfile\\'" . dockerfile-mode)
  :config
  (setq-default docker-use-sudo nil))

(use-package docker-tramp
  :disabled t
  :straight t)

;; markdown-mode
;;     Highlighting for markdown

(use-package markdown-mode
  :straight t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; lrl-mode
;;     Bloomberg database params

(use-package lrl-mode
  :mode ("\\.lrl\\'" . lrl-mode))

;; magit
    
;;     Provide a way of interacting with a Git repository.
    
;;     Download package if not installed!

(use-package magit
  :straight t
  :bind (("C-c m" . magit-status)
         ("C-c C-m" . magit-dispatch-popup))
  :config (progn
            (add-hook 'magit-status-headers-hook 'magit-insert-repo-header)
            (add-hook 'magit-status-headers-hook 'magit-insert-remote-header)
            (remove-hook 'magit-status-headers-hook 'magit-insert-tags-header)
            (setq magit-commit-show-diff nil)
            (setq magit-refresh-verbose t)
            (setq magit-save-repository-buffers nil)
            (setq magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))
            (setq magit-view-git-manual-method 'man)
            (setq magit-auto-revert-tracked-only t)
            (magit-auto-revert-mode 1)))

;; forge
;;     This implements an interface to github that
;;     integrates with magit.  It's accessible with
;;     =N= in magit. 


(use-package forge
  :straight t
  :after magit
  :config
  (push '("bbgithub.dev.bloomberg.com"         ; GITHOST
        "bbgithub.dev.bloomberg.com/api/v3"  ; APIHOST
        "bbgithub.dev.bloomberg.com"         ; WEBHOST and INSTANCE-ID
        forge-github-repository)             ; CLASS
      forge-alist))

;; magit-todo

;;     Include TODO, etc in the magit buffer


(use-package magit-todos
  :after magit
  :disabled t
  :straight t
  :config (magit-todos-mode))

;; multiple-cursors
    
;;     You can place multiple cursors in a buffer
;;     and have whatever you do affect each item

(use-package multiple-cursors
  :disabled t
  :bind (("C-. e" . mc/edit-lines)
         ("C-. >" . mc/mark-next-like-this)
         ("C-. <" . mc/mark=previous-like-this)))

;; ag
;;     A fast search across lots of files.  Relies
;;     on package silver searcher for the executable
;;     to be installed.


(use-package ag
  :straight t
  :bind (("C-c f" . ag))
  :config (setq ag-reuse-buffers t))

;; pw-misc
    
;;     Some commands I find useful
    

(use-package pw-misc
  :after compile
  :bind (("C-c p" . pw/prev-frame)
         ("C-c \\" . pw/reindent)
         ("C-c e" . pw/eval-region-or-defun))
  :hook (compilation-mode-hook . pw/no-line-column-number))

;; pw-trunc-lines
    
;;     Toggle truncation of long lines

(use-package pw-trunc-lines
  :commands pw/trunc-lines
  :bind ("C-c $" . pw/trunc-lines)
  :hook ((c-mode-common makefile-gmake-mode compilation-mode shell-mode) . pw/trunc-lines))

;; rust
;;     Configure rust mode so it has syntax highlighting via treesitter and completion via
;;     eglot.  Assumes that rust rust-analyzer and rustfmt are installed.  I used homebrew.

(use-package rust-mode
  :straight t
  :hook eglot-ensure
  :config
  (setq rust-format-on-save t)
  :init
  (setq rust-mode-treesitter-derive t)
  )
(use-package cargo-mode
  :straight t
  :hook
  (rust-mode . cargo-minor-mode))

;; typescript and tsx modes

;;     I also installed the grammers with
;;     =treesit-install-language-grammer=.  The grammars are in
;;     https://github.com/tree-sitter/tree-sitter-typescript but the
;;     directory is =typescript/src= and =tsx/src=.


(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;; pw-shell-scomplete
;;     Use the existing completion framework to switch shell buffers.  This way it
;;     integrates smoothly with selectrum and prescient

(use-package pw-shell-scomplete
  :bind (("C-c s" . 'pw/shell-scomplete-to-shell-buffer)
         ("C-c 4 s" . 'pw/shell-scomplete-to-shell-other)))

;; treemacs

(use-package treemacs
  :straight t
  :bind (("C-c t" . treemacs))
  :config
  (progn
    (defun pw/treemacs-ignore (file path)
      (string-match-p "\\.pyc$\\|\\.sundev1\\.\\|\\.o$\\|\\.d$\\|__pycache__" file))
    (add-hook 'treemacs-ignored-file-predicates 'pw/treemacs-ignore)
    (setq treemacs-show-hidden-files nil)
    (setq treemacs-collapse-dirs 2)))

;; tree-sitter
;;     This is an experiment system for fast, incremental parsing
;;     of programming languages.  This uses highlighting and folding

(use-package tree-sitter
  :straight t
  :delight
  :config
  (progn
    (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
    (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :straight t)
(use-package ts-fold
  :disabled t
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :bind ("C-c @ t" . ts-fold-toggle)
)

;; tramp config
;;     And the actual elisp part of things:

(use-package tramp
  :config
  (progn
    (connection-local-set-profile-variables
     'remote-bb-zsh
     '((explicit-shell-file-name . "/opt/bb/bin/zsh")))
    (connection-local-set-profiles
     '((:application tramp :machine "folxdi-ob-963.bloomberg.com")
       (:application tramp :machine "xlnxdv-ob-490.bloomberg.com"))
     'remote-bb-zsh)
    (setq tramp-default-remote-shell "/opt/bb/bin/bash")
    (setq tramp-remote-path  (cons "/home/pware/usr/bin" (cons "/opt/bb/bin" tramp-remote-path)))
    (setq tramp-use-ssh-controlmaster-options nil)))

;; wgrep
;;     This lets you save the results from grep, edit those results and then
;;     saving the changes applies them to each file.

(use-package wgrep
  :defer 5
  :straight t)

;; zoom-frm
    
;;     Much like face-remap that adds test-scale-increase and
;;     text-scale-decrease I use this to change the entire window
;;     instead of the buffer

(use-package zoom-frm
  :straight t
  :bind* (("C-c [" . zoom-frm-out)
          ("C-c ]" . zoom-frm-in)
          ("C-c 0" . zoom-frm-unzoom)))

;; powerline
    
;;     Make the modeline have lots of pretty graphics.

;;     For `iterm2` I had to install some extra fonts
;;     for these to look good:

;;          https://github.com/powerline/fonts

;;     Disabled 2024-11-07 as I liked the basic timu-caribbean-theme
    

(use-package powerline
  :disabled t
  :straight (:host github :repo "milkypostman/powerline")
  :config
  (progn
    (powerline-default-theme)))

;; deeper-blue theme
;;     I like the darker background of this theme and the colors are a
;;     little brighter.  Swiched to this from =timu-caribbean-theme= on
;;     2024-11-19.


(load-theme 'deeper-blue)

;; timu-caribbean-theme
;;     I've liked these themes:

;;     - modus theme
;;     - nord theme
;;     - timu-caribbean-theme

;;     Currently, liking timu-carbbean-theme.  Ugg, switched
;;     to =deeper-blue= on 2024-11-19

(use-package timu-caribbean-theme
  :disabled t
  :straight t
  :config
  (setq timu-caribbean-mode-line-border t)
  (load-theme 'timu-caribbean t))

;; modus theme
;;     Previously, I liked the "nord" theme but recently
;;     the higher visibility offered by modus has been better.
;;     On 2024-10-31 I switched to timu-caribbean-theme

;;     I've chosen the darker theme, modus-vivendi with a few
;;     customizations.  It's well documented at:

;;     https://github.com/protesilaos/modus-themes


(use-package modus-themes
  :disabled t
  ;:straight t
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs nil
        modus-themes-bold-constructs t
        modus-themes-links '(no-underline)
        modus-themes-syntax '(faint alt-syntax green-strings yellow-comments))

  (load-theme 'modus-vivendi t))

;; nord theme
;;     I've been trying to find a theme that works well
;;     with iterm2, emacs-25 and emacs-26.

;;     If using iterm, you should change it to use following
;;     color scheme:

;;     https://github.com/arcticicestudio/nord-iterm2


(use-package nord-theme
  :disabled t
  :straight t
  :config
  (progn
    (setq nord-region-highlight 'snowstorm)
    (setq nord-uniform-mode-lines nil)
    (setq nord-comment-brightness 20)
    (load-theme 'nord t)))

;; Various preferences

;;    Allow narrow to region (e.g. =C-X n n=)

(put 'narrow-to-region 'disabled nil)

;; Clean startup

;;     Do not display message in the scratch buffer or the startup
;;     message or the message in the echo area.  You'll need to change
;;     =inhibit-startup-echo-area-message= to your login to disable start
;;     message in echo area.
    

(setq initial-scratch-message "")
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message "pware")

;; Configure the mode line

;;     Turn on displaying the date and time in the mode line.
;;     Enable displaying the line and column numbers in the mode line
;;     But don't do that if the buffer is >250k
;;     Do not blink the cursor

(setq display-time-day-and-date nil)
(setq line-number-display-limit 250000)
(display-time-mode)
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
(blink-cursor-mode -1)

;; Legacy (or I've been using emacs for too long)

;;     If at beginning of line, the Ctl-K kills including the newline
;;     (I'm hardwired to type Ctl-K twice so I keep it as =nil=.


;(setq kill-whole-line t)




;; Latest Emacs can wrap lines at word boundaries and will move the cursor
;; so it stays in the same column on screen.  I'm too used to the old style.

(setq-default word-wrap nil)
(setq line-move-visual nil)
(setq visual-line-mode nil)

;; Tune scrolling behaviour

;;     Make it so moving up or down does it one line at a time.

;;     - ~scroll-step~ 0 works better with Emacs which now supports
;;       ~scroll-conservatively~.
;;     - ~scroll-conservatively~ when > 100 then Emacs scrolls just
;;       enough to make point visible.  This actuall works well 
;;       for shell buffers but I also like it other places.
;;     - ~scroll-margin~ says to keep this many lines
;;        above or below so you get some context.
;;     - ~scroll-preserve-screen-position~ says when scrolling pages, keep
;;       point at same physical spot on screen.

(setq scroll-step 0)
(setq scroll-conservatively 101)
(setq scroll-margin 0)
(setq scroll-preserve-screen-position 'keep)



;; I set horizontal scrolling because I'd have trouble with
;; long lines in shell output.  This seemed to get
;; them to display faster by actually slowing things down

;; - ~hscroll-margin~ is how close cursor gets before
;;   doing horizontal scrolling
;; - ~hscroll-step~ is how far to scroll when marg is reached.


(setq hscroll-margin 1)
(setq hscroll-step 5)

;; Incremental search highlighting
;;     Incremental search settings

(setq lazy-highlight-max-at-a-time 10)
(setq lazy-highlight-initial-delay .25)
(setq lazy-highlight-interval 0)

;; Speed up long lines; no bidi
;;     Emacs has serious performace problems with
;;     long lines.  One thing that seems to help is
;;     hardcoding the bi-directional settings.  This
;;     will break right-to-left languages

(setq-default bidi-display-reordering nil)
(setq-default bidi-paragraph-direction 'left-to-right)

;; Misc settings
;;     Make the mark-ring allow =C-SPC= repeatedly
;;     pop the mark.  =C-X C-<SPC>= pops the global mark-ring
;;     and then =C-SPC= immediately after pops next item.

(setq set-mark-command-repeat-pop t)



;; Cause the gutter to display little arrows and
;; boxes if there is more to a file

(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines t)



;; Even though I did something with the mouse do not
;; popup a dialog box but prompt from the mode line

(setq use-dialog-box nil)



;; This _sounds_ like something that should be nil but
;; the reality is that when user input stops redisplay
;; a bunch of screen optimizations are lost.  The
;; default is prior to emacs-24 is nil

(setq redisplay-dont-pause t)



;; I don't like actual tabs being inserted

(setq-default indent-tabs-mode nil)

;; Cut and Paste
;;     Weird X11 stuff with the cut-and-paste.  I think these settings
;;     provide the best compromise.

;;     The world uses what is called a clipboard for copy-and-paste.  X11
;;     had a more flexible arrangement with a primary cut buffer that some
;;     X11 older clients still use.  Older clients typically means xterm
;;     and mrxvt.

;;     In Exceed, you need to set the config so that the "X Selection" tab
;;     has the "X Selection Associated with Edit Operations:" be
;;     "CLIPBOARD".

;;     The following puts killed text into the clipboard which makes it
;;     avaiable for all Windows clients given the above Exceed setting.

(setq x-select-enable-clipboard t)



;; The following puts killed text into the X11 primary cut buffer.
;; Text copied in an xterm can either be pasted into emacs with a
;; middle-mouse or the usual yank operations like =C-y=.  You cannot
;; paste such text into other Window's applications without going through
;; emacs.  Usualy middle mouse button in an xterm pastes the text
;; from emacs.

(setq x-select-enable-primary t)




;; Alternatively, in Exceed, set the "X Selection Associated with
;; Edit Operations:" to be "PRIMARY" and use these settings.  This lets
;; older xterm/mrxvt co-exist with Windows applications.

;; To copy to an xterm use left-mouse to select the text in emacs and
;; then usual paste with middle-mouse to paste to the xterm.


;(setq x-select-enable-clipboard nil)
;(setq x-select-enable-primary t)



;; Do not beep if I kill text in a read-only buffer

(setq kill-read-only-ok t)



;; Usually, my home directory is faster for saving files
;; then anywhere else.

(setq backup-directory-alist '(("." . "~/.backups")))



;; Make it so selecting the region highlights it and causes many
;; commands to work only on the region

(setq transient-mark-mode t)



;; Ignore some other file extensions

(setq completion-ignored-extensions (append completion-ignored-extensions '(".d" ".dd" ".tsk")))
