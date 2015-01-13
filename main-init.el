;;;
;;; I have this in my ~/.emacs and ~/.xemacs/init.el:
;;;
;;; (add-to-list 'load-path "~/usr/emacs")
;;; (require 'main-init)
;;;
;;; In my ~/.Xdefaults I have these lines
;;; Emacs*background: grey15
;;; Emacs*foreground: grey90
;;; Emacs*pointerColor: green
;;; Emacs*cursorColor: grey90
;;; Emacs.geometry: 135x65+50+0
;;; Emacs.verticalScrollBars: off
;;; Emacs.toolBar: 0
;;; ! Try this to list all potential server side fonts:
;;; !     $ xlsfonts -fn '*-*-*-*-*-*-*-*-*-*-*-m*'
;;; !     $ xlsfonts -fn '*-*-*-*-*-*-*-*-*-*-*-c*'
;;; ! The "m" means monospace; "c" for character cell.
;;; ! 
;;; ! You want this to be "loose" enough to handle bold and oblique.
;;; ! The "140" is point size time 10 (e.g. 14 points); "100" is
;;; !Emacs.font: -adobe-courier-*-*-normal--*-170-100-*-m-*-iso8859-1
;;; 
;;; ! Or if in linux with real fonts
;;; ! fc-list :spacing=mono
;;; ! fc-list :space=cell
;;; Emacs.font: Bitstream Vera Sans Mono-16
;;; 
;;; I switch between Emacs, XEmacs, AquaMacs (Mac OS X) and various
;;; versions of these.  They don't always have the same functionality.
;;; (e.g. (require) takes different args) If you are a normal person,
;;; you can probably dump most of these checks using fboundp and
;;; boundp

(provide 'main-init)

(require 'use-package)                  ;Download this!
(use-package bind-key)                  ;Download this!

;;
;; Using bind-key lets you run describe-personal-keybindings
;; which is a nice way of keep track of what you've changed.
(bind-key "C-c G" 'goto-line)
(bind-key "C-c o" 'other-frame)

;;
;; Use the emacs packaging system to automatically install some packages
(use-package package
  :init
  (progn
    ;; Try to detect being at Bloomberg
    (when (getenv "BBPATH")
      (setq url-proxy-services
            '(("no_proxy" . "^\\(localhost\\|10.*\\)")
              ("http" . "devproxy.bloomberg.com:82")
              ("https" . "devproxy.bloomberg.com:82"))))
    (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
    (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
    (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
    (package-initialize)
    (use-package pw-pkg-install
      :init
      (pw/ensure-pkg-installed
       'alect-themes 'anyins 'diminish 'num3-mode
       'scratch-ext 'sublime-themes 'zen-and-art-theme))))

;;;
;;;----------------------------------------------------------------------
;;; This section is for packages that are always loaded.
;;; The next section gives those that are loaded on demand
;;;----------------------------------------------------------------------
;;;

;; You can save bookmarks with `C-x r m' and jump to them wih `C-x r b'
;; This makes them save automatically
(use-package bookmark
  :init
  (setq bookmark-save-flag 1))

;;
;; If you like windows style cut and paste then try this.  ^C & ^X only
;; work when region is active, ^V and ^Z do paste and undo
;;
;; DISABLED
(use-package cua-base
  :disabled t
  :init
  (cua-mode 1))

;;
;; I can't handle the active region getting deleted
(use-package delsel
  :init
  (delete-selection-mode -1))

;;
;; This causes the set of files being visited to be restored
;; on startup.  The hook below causes the save file to 
;; be written more often as I found myself leaving emacs running
;; until it is killed.
(use-package desktop
  :init
  (progn
    (setq desktop-save t)
    (setq desktop-restore-frames nil)
    (setq desktop-restore-eager 5)
    (setq desktop-restore-in-current-display t)
    (setq desktop-lazy-verbose nil)
    (setq desktop-lazy-idle-delay 20)
    (desktop-save-mode 1)
    (add-to-list 'desktop-modes-not-to-save 'Info-mode)
    (add-to-list 'desktop-modes-not-to-save 'dired-mode)
    (if (not (boundp 'desktop-auto-save-timeout))
        (progn
          (defun pw/desktop-save ()
            (interactive)
            (when desktop-save-mode
            ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
              (desktop-save desktop-dirname t)))
          (add-hook 'auto-save-hook 'pw/desktop-save)))
    ))

;;
;; This makes saving shell scripts automatically make
;; them executable.  It's considered a shell script if
;; it starts with #!
(use-package executable
  :init
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p))

;;
;; `global-hl-line-mode' highlights the current line.  You should make sure
;; that `hl-line-face' is an appropriate, subtle color.  The sticky
;; flag keeps it highlighted in all windows
(use-package hl-line
  :init
  (progn
    (setq hl-line-sticky-flag t)
    (setq global-hl-line-sticky-flag t)
    (global-hl-line-mode 1)))

;;
;; The new emacs way of doing various completions
;;
;; DISABLED (use ido instead)
(use-package icomplete
  :disabled t
  (icomplete-mode 1))

;;
;; Use a fancy auto-complete for buffers and files
(use-package ido
  :init
  (progn
    (setq ido-default-buffer-method 'selected-window)
    (setq ido-default-file-method 'selected-window)
    (setq ido-enable-flex-matching t)
    (setq ido-enable-dot-prefix t)
    (setq ido-enable-tramp-completion nil)
    (setq ido-max-directory-size 100000)
    (setq ido-rotate-file-list-default t)
    (setq ido-enter-matching-directory 'first)
    (setq ido-use-virtual-buffers 'auto)
    (setq ido-separator "|")
    (setq ido-ignore-files (append ido-ignore-files '("\\`00")))
    (setq ido-ignore-buffers
          (list "\\` " ".*Completions.*"))
    (setq ido-work-directory-list-ignore-regexps
          (list "/bb/bin" "/bb/data" "/bb/data/tmp" "/bbsrc/apputil"))
    (ido-mode 1)))

;;
;; `iswitchb-mode' provides a nice completion for switching between
;; buffers.  The `iswitchb-use-virtual-buffers' and `recentf-mode'
;; adds recent files to the match 
;;
;; DISABLED (use ido instead)
(use-package iswitchb
  :disabled t
  :init
  (progn
    (setq iswitchb-default-method 'samewindow
          iswitchb-max-to-show 5
          iswitchb-use-virtual-buffers t)
    (recentf-mode 1)
    (iswitchb-mode 1)))

;;
;; Make visiting a *.gz automatically uncompress file
(use-package jka-cmpr-hook
  :init
  (auto-compression-mode 1))

;;
;; Make sure the mouse wheel scrolls
(use-package mwheel
  :init
  (progn
    (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
    (mwheel-install)))

;;
;; Highlight matching paren
(use-package paren
  :init
  (show-paren-mode 1))

;;
;; Save emacs's internal command history.
(use-package savehist
  :init
  (progn
    (setq savehist-additional-variables
          '(compile-command
            grep-find-history
            grep-history
            grep-regexp-history
            grep-files-history))
    (savehist-mode 1)))

;;
;; This records the location of every file you visit and
;; restores when you vist a file, goes to that location.  I also save
;; the file every couple hours because I don't always quit emacs
(use-package saveplace
  :init
  (progn
    (setq-default save-place t)
    (setq save-place-limit nil)
    (run-at-time 3600  3600 'save-place-alist-to-file)))
	 
;;
;; Make it so $EDITOR can popup in this emacs
(use-package server
  :init
  (progn
    (if (not (string-match "emacsclient" (or (getenv "EDITOR") "")))
        (setenv "EDITOR" "emacsclient"))
    (server-start t)))
  
;;
;; Turn the toolbar off.  I also turn it off in my .Xdefaults with:
;; Emacs.toolBar:            0
;; which keeps it from displaying on startup
(use-package tool-bar
  :init
  (tool-bar-mode -1))

;;
;; Make it so buffers with the same name are are made unique by added
;; directory path and killing a buffer renames all of them.
(use-package uniquify
  :init
  (progn
    (setq uniquify-buffer-name-style 'post-forward)
    (setq uniquify-after-kill-buffer-p t)))

;;;
;;;----------------------------------------------------------------------
;;; This section are non-standard packages that are always loaded
;;;----------------------------------------------------------------------
;;;

;;
;; Bloomberg C++ coding style
(use-package bb-style
  :init
  (progn
    ;; Use bb-style for C/C++; associate .h files with c++-mode instead of
    ;; c-mode
    (setq c-default-style "bb")
    (setq c-tab-always-indent nil)
    (add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
  ))

;;
;; Do not display these minor modes in mode-line
;;
;; Download package if not installed!
(use-package diminish
  :init
  (diminish 'abbrev-mode))

;;
;; Setup lazy font locking
(use-package pw-font-lock)

;;
;; Make *scratch* buffers get saved
;;
(use-package scratch-ext
  :init
  (save-excursion
    (setq scratch-ext-log-directory "~/.emacs.d/scratch")
    (if (not (file-exists-p scratch-ext-log-directory))
	(mkdir scratch-ext-log-directory t))
    (scratch-ext-create-scratch)
    (scratch-ext-restore-last-scratch)))

;;
;; I like the wilson theme from the sublime-themes
;; package.
;;
;; Download package if not installed!
(use-package sublime-themes
  ;;:ensure t
  :init
  (load-theme 'wilson t nil))

;;;
;;;----------------------------------------------------------------------
;;; Standard packages that defer loading until they are called (e.g. minimal
;;; cost on startup)
;;;----------------------------------------------------------------------
;;;

;;
;; Setup compilation buffers
(use-package compile
  :bind ("C-c c" . compile)
  :config
  (progn
    (setq compilation-scroll-output 'first-error)
    (use-package pw-misc
      :init 
      (add-hook 'compilation-mode-hook 'pw/no-line-column-number))
    (use-package ansi-color
      :init
      (progn
        (defun pw/colorize-compilation-buffer ()
          (let ((inhibit-read-only t))
            (ansi-color-apply-on-region compilation-filter-start (point-max))))
        (if (and (boundp 'compilation-fiter-hook) (fboundp 'ansi-color-apply-on-region))
            (add-hook 'compilation-filter-hook 'pw/colorize-compilation-buffer))))))
  
;;
;; A nice graphical diff Make sure that ediff ignores all whitespace
;; differences and highlights the individual differences
(use-package ediff
  :commands ediff-load-version-control
  :init
  (progn
    (setq ediff-diff-options "-w")
    (setq-default ediff-auto-refine 'on)

    (defun pw/ediff-current (arg)
      "Run ediff-vc-internal on the current file against it's latest revision.
If prefix arg, use it as the revision number"
      (interactive "P")
      (ediff-load-version-control t)
      (let ((rev (if arg (format "%d" arg) "")))
        (funcall
         (intern (format "ediff-%S-internal" ediff-version-control-package))
         rev "" nil)))
    (bind-key "C-c =" 'pw/ediff-current)))

;;
;; This makes a single file wrap around between two windows.
;; Try ^X-3 and then move to the top or bottom of the window
;; and the other window scrolls.  I bound F7 to do get
;; rid of the other windows and split.
(use-package follow
  :bind ("<f7>" . follow-delete-other-windows-and-split))

;; `rgrep' recursively greps for a pattern.  It uses a key to specify
;; filenames and ignores directories like CVS.  "cchh" is all C++
;; files and headers.
(use-package grep
  :bind (("C-c f" . rgrep)
         ("C-c g" . grep))
  :init
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
            ("m" . "[Mm]akefile*")
            ("tex" . "*.tex")
            ("texi" . "*.texi")
            ("asm" . "*.[sS]")
            ("code" . "*.c *.h *.cpp *.f")))))

;;
;; Setup commands and menus to hide/show blocks of code
(use-package hideshow
  :commands hs-minor-mode
  :init
  (progn
    (add-hook 'c++-mode-hook 'hs-minor-mode)
    (add-hook 'c-mode-hook 'hs-minor-mode)))

;;
;; Make it so line numbers show up in left margin
;; Used in C/C++ mode.
(use-package linum
  :commands linum-mode
  :init (add-hook 'prog-mode-hook 'linum-mode)
  :config (setq linum-format 'dynamic))

;;
;; org-mode provides an outline, todo, diary, calendar like interface.
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :commands orgstruct-mode
  :diminish orgstruct-mode
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c r" . org-capture))
  :init (add-hook 'c-mode-common-hook 'orgstruct-mode)
  :config
  (use-package org-prefs))

;; This is mostly for C++ but make it so whitespace that should not be there
;; is highlighted.  This causes tabs, and whitespace at beginning
;; and end of the buffer as well as at the end of the line to highlight
(use-package whitespace
  :bind ("C-c SPC" . whitespace-mode)
  :init
  (progn
    (setq whitespace-style '(face trailing tabs empty indentation::space lines-tail))
    (setq whitespace-line-column nil)))

;;;
;;;----------------------------------------------------------------------
;;; Non-standard packages that defer loading until they are called (e.g. minimal
;;; cost on startup)
;;;----------------------------------------------------------------------
;;;


;;
;; Freaky way to insert text
;; 1. Enter anyins-mode
;; 2. Move around; mark spots you want to insert text with RET
;; 3. To insert text
;;    a. ``y'' inserts each line from kill ring at each marked spot, or
;;    b.  ``!'' runs a shell command line 'seq -s ". \n" 1 3' generates
;; numbers "1. "  "2. " "3. " and inserts it at each markets tpot
;;
;; Download package if not installed!
(use-package anyins
  :bind ("C-c i" . anyins-mode))

;;
;; Setup preferences for shell, compile and other comint based commands
;;
;; Pete specific
(use-package comint-prefs
  :commands (comint-for-pete dbx-for-pete comint-watch-for-password-prompt)
  :init
  (progn
    (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
    (add-hook 'comint-mode-hook 'comint-for-pete)
    (add-hook 'dbx-mode-hook 'dbx-for-pete))  )

;;
;; Bloomberg database schema
(use-package csc-mode
  :mode ("\\.csc2$" . csc-mode))

;;
;; Bloomberg database params
(use-package lrl-mode
  :mode ("\\.lrl\\'" . lrl-mode))

;;
;; Make long strings of digits alternate
;; groups of 3 with bold.
;;
;; Download package if not installed!
(use-package num3-mode
  ;;:ensure t
  :commands num3-mode
  :diminish num3-mode
  :init (add-hook 'prog-mode-hook 'num3-mode)
  :config (make-face-bold 'num3-face-even))

;;
;; Some commands I find useful
;;
;; Pete specific
(use-package pw-misc
  :bind (("C-c p" . pw/prev-frame)
         ("C-c p" . pw/prev-frame)
         ("C-c -" . pw/font-size-decrease)
         ("C-c +" . pw/font-size-increase)
         ("C-c \\" . pw/reindent)
         ("C-c e" . pw/eval-region-or-defun)
   ))

;;
;; Toggle truncation of long lines
;;
;; Pete specific
(use-package pw-trunc-lines
  :commands pw/trunc-lines
  :bind ("C-c $" . pw/trunc-lines)
  :init
  (progn
    (add-hook 'prog-mode-hook 'pw/trunc-lines)
    (add-hook 'makefile-gmake-mode-hook 'pw/trunc-lines)
    (add-hook 'compilation-mode-hook 'pw/trunc-lines)
    (add-hook 'shell-mode-hook 'pw/trunc-lines)))

;;
;; Pete's back to make switching to a shell buffer
;; faster
;;
;; Pete specific
(use-package shell-switch
  :commands (shell-switch shell-switch-other-window)
  :init
  (progn
    (bind-key "C-c s" 'shell-switch)
    (bind-keys :prefix-map clt-c-4-keymap
               :prefix "C-c 4"
               ("s" . shell-switch-other-window))))

;;;
;;;----------------------------------------------------------------------
;;; Various preferences
;;;----------------------------------------------------------------------
;;;

;;
;; Force Mac OS X to use Consolas at 16pt
(if (eq (window-system) 'ns)
    (custom-set-faces '(default ((t (:height 160 :family "Consolas"))))))


;;
;; Do not display message in the scratch buffer or the startup message
;; or the message in the echo area
(setq initial-scratch-message "")
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message "pware")

;; 
;; Turn on displaying the date and time in the mode line.
;; Enable displaying the line and column numbers in the mode line
;; But don't do that if the buffer is >25k
(setq display-time-day-and-date t)
(setq line-number-display-limit 25000)
(display-time-mode)
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

;; If at beginning of line, the Ctl-K kills including the newline
;; (I'm hardwired to type Ctl-K twice)
;(setq kill-whole-line t)

;; Latest Emacs can wrap lines at word boundaries and will move the cursor
;; so it stays in the same column on screen.  I'm too used to the old style.
(setq-default word-wrap nil)
(setq line-move-visual nil)
(setq visual-line-mode nil)

;; Make it so moving up or down does it one line at a time.
;; `scroll-step' 0 works better with Emacs which now supports
;; `scroll-conservatively'.
;; `scroll-margin' says to keep this many lines
;; above or below so you get some context.
;; `scroll-preserve-screen-position' says when scrolling pages, keep
;; point at same physical spot on screen.
(setq scroll-step 0)
(setq scroll-conservatively 15)
(setq scroll-margin 2)
(setq scroll-preserve-screen-position 'keep)

;;
;; Incremental search settings
(setq lazy-highlight-max-at-a-time 10)
(setq lazy-highlight-initial-delay .5)
(setq lazy-highlight-interval .1)

;;
;; Turn off the scroll bars
(scroll-bar-mode -1)

;;
;; Cause the gutter to display little arrows and
;; boxes if there is more to a file
(setq indicate-buffer-boundaries '(left))
(setq indicate-empty-lines t)

;;
;; Even though I did something with the mouse do not
;; popup a dialog box but prompt from the mode line
(setq use-dialog-box nil)

;; 
;; This _sounds_ like something that should be nil but
;; the reality is that when user input stops redisplay 
;; a bunch of screen optimizations are lost.  The
;; default is prior to emacs-24 is nil
(setq redisplay-dont-pause t)

;;
;; I found visiting a file to be really slow and realized
;; it was from figuring out the version control
(setq vc-handled-backends '(Git SVN))

;; I don't like actual tabs being inserted
(setq-default indent-tabs-mode nil)


;; Weird X11 stuff with the cut-and-paste.  The world uses what is
;; called a clipboard for copy-and-paste.  X11 had a more flexible
;; arrangement with a primary cut buffer that some X11 older clients
;; still use.  I think these settings provide the best compromise.
;;
;; This does not put killed text into the X11 primary cut buffer;
;; instead you use the mouse or the shift selection.  You can use
;; mouse-2 to paste from X11 clients that use the primary buffer.
(setq selective-active-regions 'only)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary nil)

;; Do not beep if I kill text in a read-only buffer
(setq kill-read-only-ok t)

;;
;; Usually, my home directory is faster for saving files
;; then anywhere else.
(setq backup-directory-alist '(("." . "~/.backups")))

;; Make it so selecting the region highlights it and causes many
;; commands to work only on the region
(setq transient-mark-mode t)

;;
;; Ignore some other file extensions
(setq completion-ignored-extensions (append completion-ignored-extensions '(".d" ".dd" ".tsk")))

