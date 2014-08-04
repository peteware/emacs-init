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

;;
;; I prefer to load installed packages early in setup
;; so the rest of the code can test for the existance
;; of symbols.  Note that this means you cannot
;; use customize to configure package variables
(if (not (featurep 'package))
    (load "package" t))
(if (fboundp 'package-initialize)
    (progn
      (package-initialize)
      (setq package-enable-at-startup nil)))

(require 'ediff)
(require 'uniquify)
(require 'whitespace)
(require 'hideshow)


;; These are in my ~/usr/emacs directory
;(require 'tabbar)			;Provides tabs with buffer names
;(require 'color-theme-pw)		;Pete specific
(require 'comint-prefs)			;Pete specific
(require 'shell-switch)			;Pete specific though generally useful
(require 'bb-style)			;Bloomberg C++ coding style
(require 'lrl-mode) 			;Bloomberg
(require 'csc-mode)			;Bloomberg

;; org-mode provides an outline, todo, diary, calendar like interface.
(if (not (featurep 'org))
    (load "org" t))
;; org-prefs is in my ~/usr/emacs
(if (fboundp 'org-mode)
    (require 'org-prefs))

;; 
;; Turn on displaying the date and time.
;; 
(setq display-time-day-and-date t)
(display-time-mode)

;;
;; Highlight matching paren
;;
(if (fboundp 'show-paren-mode)
    (show-paren-mode 1))

;; 
;; This _sounds_ like something that should be nil but
;; the reality is that when user input stops redisplay 
;; a bunch of screen optimizations are lost.  The
;; default is prior to emacs-24 is nil
(setq redisplay-dont-pause t)

;(add-hook 'find-file-hook 'auto-insert)

;;
;; I found visiting a file to be really slow and realized
;; it was from figuring out the version control
;;(setq vc-handled-backends '(CVS Hg))
;;(setq vc-handled-backends '(Git SVN))
;; I prefer to use egg for handlig git instead of the built in
;; so remove git from this list
;(require 'egg)
(setq vc-handled-backends '(SVN))

;; I don't like actual tabs being inserted
(setq-default indent-tabs-mode nil)

;; Use bb-style for C/C++; associate .h files with c++-mode instead of
;; c-mode
(setq c-default-style "bb")
(setq c-tab-always-indent nil)
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

;; Make it so line numbers show up in left margin
;; Used in C/C++ mode.
(defun pw/linenum-mode ()
  "Enable linnum-mode which displays the line number in left margin."
  (interactive)
  (if (fboundp 'linum-mode)
      (linum-mode 1)))

(add-hook 'c++-mode-hook		'pw/linenum-mode)
(add-hook 'c-mode-hook			'pw/linenum-mode)
(add-hook 'python-mode-hook             'pw/linenum-mode)
(add-hook 'fortran-mode-hook            'pw/linenum-mode)

;; Setup commands and menus to hide/show blocks of code
(if (fboundp 'hs-minor-mode)
    (progn
      (add-hook 'c++-mode-hook 'hs-minor-mode)
      (add-hook 'c-mode-hook 'hs-minor-mode)))

;; If you like windows style cut and paste then try this.  ^C & ^X only
;; work when region is active, ^V and ^Z do paste and undo
;;(cua-mode 1)

;; I'm not used to this so I find it annoying when I have the
;; region selected and anything I do replaces the region.  Most
;; people probably prefer this to be t.
(if (fboundp 'pc-selection-mode)
    (pc-selection-mode -1))

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

;; If at beginning of line, the Ctl-K kills including the newline
;; (I'm hardwired to type Ctl-K twice)
;(setq kill-whole-line t)

;;
;; Turn the toolbar off.  I also turn it off in my .Xdefaults with:
;; Emacs.toolBar:            0
;; which keeps it from displaying on startup
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

;;
;; This causes the set of files being visited to be restored
;; on startup.  The hook below causes the save file to 
;; be written more often as I found myself leaving emacs running
;; until it is killed.
(if (fboundp 'desktop-save-mode)
    (progn
      (setq desktop-save t)
      (setq desktop-restore-eager 1)
      (setq desktop-lazy-verbose nil)
      (setq desktop-lazy-del-delay 60)
      (desktop-save-mode 1)
      (add-to-list 'desktop-modes-not-to-save 'Info-mode)
      (add-to-list 'desktop-modes-not-to-save 'dired-mode)
      (defun pw/desktop-save ()
	(interactive)
	;; Don't call desktop-save-in-desktop-dir, as it prints a message.
        (desktop-save desktop-dirname t))
      (add-hook 'auto-save-hook 'pw/desktop-save)
      ))

;;
;; This makes a single file wrap around between two windows.
;; Try ^X-3 and then move to the top or bottom of the window
;; and the other window scrolls.  I bound F7 to do get
;; rid of the other windows and split.
(if (fboundp 'turn-on-follow-mode)
    (progn
      ;(add-hook 'find-file-hook 'turn-on-follow-mode)
      (global-set-key [f7] 'follow-delete-other-windows-and-split)))

;;
;; This makes saving shell scripts automatically make
;; them executable.  It's considered a shell script if
;; it starts with #!
(if (fboundp 'executable-make-buffer-file-executable-if-script-p)
    (add-hook 'after-save-hook
              'executable-make-buffer-file-executable-if-script-p))


;; `iswitchb-mode' provides a nice completion for switching between
;; buffers.  The `iswitchb-use-virtual-buffers' and `recentf-mode'
;; adds recent files to the match 
(if (fboundp 'iswitchb-mode)
    (progn
      (setq iswitchb-default-method 'samewindow
	    iswitchb-max-to-show 5
	    iswitchb-use-virtual-buffers t)
      (recentf-mode 1)
      (iswitchb-mode 1)))

;; `ido-mode` does for find-file what iswitchb-mode does
;; for switch-to-buffer.  It was cool but I found it
;; it slowed down on big directories too much and
;; some very annoying interaction with tramp
;; (if (fboundp 'ido-mode)
;;     (progn
;;       (setq ido-default-buffer-method 'samewindow)
;;       (setq ido-enable-tramp-completion nil)
;;       (setq ido-ignore-buffers
;;             (list "\\'" ".*Completions.*"))
;;       (setq ido-work-directory-list-ignore-regexps
;;             (list "/bb/bin" "/bb/data" "/bb/data/tmp" "/bbsrc/apputil"))
;;       (ido-mode 1)
;;       ))
;; 

;; `global-hl-line-mode' highlights the current line.  You should make sure
;; that `hl-line-face' is an appropriate, subtle color.  The sticky
;; flag keeps it highlighted in all windows
(if (fboundp 'global-hl-line-mode)
    (progn
      (setq hl-line-sticky-flag t)
      (setq global-hl-line-sticky-flag t)
      (global-hl-line-mode 1)))


;; This records the location of every file you visit and
;; restores when you vist a file, goes to that location.  I also save
;; the file every couple hours because I don't always quit emacs
(setq-default save-place t)
(require 'saveplace)
(setq save-place-limit nil)
(run-at-time 3600  3600 'save-place-alist-to-file)
	 
;;
;; (if (not (featurep 'template))
;;     (load "template" t))
;; (if (fboundp 'template-initialize)
;;     (template-initialize))

;; Loaded from tabbar.el and it puts the names of similar buffers
;; in a tabbar at the top of each window.
;;
;; I had some trouble with scrolling and didn't find
;; this nearly as useful as I expected -- probably
;; because iswitchb-mode is so good
;(if (fboundp 'tabbar-mode)
;    (tabbar-mode 1))

;;
;; Do not display message in the scratch buffer or the startup message
;; or the message in the echo area
(setq initial-scratch-message "")
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message "pware")

;;
;; Usually, my home directory is faster for saving files
;; then anywhere else.
(setq backup-directory-alist '(("." . "~/.backups")))

;; Make it so buffers with the same name are are made unique by added
;; directory path and killing a buffer renames all of them.
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-after-kill-buffer-p t)


;; Latest Emacs can wrap lines at word boundaries and will move the cursor
;; so it stays in the same column on screen.  I'm to used to the old style.
(setq-default word-wrap nil)
(setq line-move-visual nil)
(setq visual-line-mode nil)

;; I hate buffers displaying in other frames unless I ask it to.
;; This reduces the set tha display in another buffer.
(setq obof-other-frame-regexps '("\\*Messages\\*" "\\*mail\\*" "\\*Colors\\*"))
(setq special-display-regexps '("[ ]?\\*Messages\\*[ ]?" "[ ]?\\*Open Recent\\*[ ]?" ".*SPEEDBAR.*"))

;; Make sure the mouse wheel scrolls
(if (fboundp 'mwheel-install)
    (mwheel-install))

;; Make it so moving up or down does it one line at a time.
;; `scroll-step' 0 works better with Emacs which now supports
;; `scroll-conservatively'.
;; `scroll-margin' says to keep this many lines
;; above or below so you get some context.
;; `scroll-preserve-screen-position' says when scrolling pages, keep
;; point at same physical spot on screen.
(setq scroll-step 0)
(setq scroll-margin 2)
(setq scroll-conservatively 15)
(setq scroll-preserve-screen-position 'keep)

;; I use `M-x compile' to do builds and I like the *compilation*
;; window to continuously scroll.  Below, I bind `^C-c' to
;; `compile'
;; I also like my compilation window to truncate long lines mostly
;; for the long link lines
(setq compilation-scroll-output t)
(add-hook 'compilation-mode-hook 'pw/trunc-lines)

;; Hide long lines in .mk
(add-hook 'makefile-gmake-mode-hook 'pw/trunc-lines)

;; Hide long lines in shell buffers
(add-hook 'shell-mode-hook 'pw/trunc-lines)

;; You can save bookmarks with `C-x r m' and jump to them wih `C-x r b'
;; This makes them save automatically
(setq bookmark-save-flag 1)

;; Make visiting a *.gz automatically uncompress file
(if (fboundp 'auto-compression-mode)
    (auto-compression-mode 1))

;; Make it so selecting the region highlights it and causes many
;; commands to work only on the region
(setq transient-mark-mode t)

;; This is mostly for C++ but make it so whitespace that should not be there
;; is highlighted.  This causes tabs, and whitespace at beginning
;; and end of the buffer as well as at the end of the line to highlight
(setq whitespace-style '(face trailing tabs empty indentation::space))

;; Make sure that ediff ignores all whitespace differences and
;; highlights the individual differences
(setq ediff-diff-options "-w")
(setq-default ediff-auto-refine 'on)

;; Save emacs's internal command history.
(setq savehist-additional-variables
      '(compile-command
	grep-find-history
	grep-history
	grep-regexp-history
	grep-files-history))
(savehist-mode 1)

;; Various ways of delaying fontification.  With these, the file is first
;; displayed and then in the background it is highlighted.  There are
;; different schemes between Emacs (jit-lock) and versions of XEmacs
(cond
 ((fboundp 'jit-lock-mode)
  (setq jit-lock-chunk-size 5000
	jit-lock-context-time .6
	jit-lock-defer-time .1
	jit-lock-stealth-nice 0.1
	jit-lock-stealth-time 5
	jit-lock-stealth-verbose nil)
  (jit-lock-mode t)
  )
 ((fboundp 'turn-on-lazy-shot)
  (add-hook 'font-lock-mode-hook 'turn-on-lazy-shot))
 ((fboundp 'turn-on-lazy-lock)
  (add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)
  (setq lazy-lock-stealth-time 10)
  (setq lazy-lock-minimum-size 10000)))

(defvar ctl-c-4-keymap (make-keymap)
  "Keymap used for C-c 4 commands.")
(define-key ctl-c-4-keymap "s" 'shell-switch-other-window)
(global-set-key [(control c) (?4)] ctl-c-4-keymap)
(global-set-key [(control c) (s)] 'shell-switch) ;from shell-switch.el
(global-set-key [(control c) (\ )] 'whitespace-mode)
(global-set-key [(control c) (c)] 'compile)
(global-set-key [(control c) (G)] 'goto-line)
(global-set-key [(control c) (g)] 'grep)
;; `rgrep' recursively greps for a pattern.  It uses a key to specify
;; filenames and ignores directories like CVS.  "cchh" is all C++
;; files and headers.
(cond
 ((fboundp 'rgrep)
  (global-set-key [(control c) (f)] 'rgrep))
 ((fboundp 'grep-all-files-in-current-directory-and-below)
  (global-set-key [(control c) (f)] 'grep-all-files-in-current-directory-and-below)
  ))

(global-set-key [(control c) (o)] 'other-frame)
(global-set-key [(control c) (p)] 'pw/prev-frame)
(defun pw/prev-frame ()
  "Switch to the previous frame."
  (interactive)
  (other-frame -1))

(defvar pw/font-size-list '(120 140 160 180 280 340)
  "List of font sizes for pw/font-size to iterate through.")
(defvar pw/font-size-index 3
  "Index of current font size for pw/font-size-change to iterate through.")
(defun pw/font-size-decrease()
  "Cycle through pw/font-size-list of fonts"
  (interactive)
  (setq pw/font-size-index (max 0 (- pw/font-size-index 1)))
  (set-face-attribute 'default (selected-frame) :height (nth pw/font-size-index pw/font-size-list)))

(defun pw/font-size-increase()
  "Set the font size to increase"
  (interactive)
  (setq pw/font-size-index (min (- (length pw/font-size-list) 1) (+ pw/font-size-index 1)))
  (set-face-attribute 'default (selected-frame) :height (nth pw/font-size-index pw/font-size-list)))

;; Make it easy to change the display size
(global-set-key [(control c) (-)] 'pw/font-size-decrease)
(global-set-key [(control c) (+)] 'pw/font-size-increase)

;; Compare file with latest version in version control
(global-set-key [(control c) (=)] 'pw/ediff-current)
(defun pw/ediff-current(arg)
  "Run ediff-vc-internal on the current file against it's latest revision.
If prefix arg, use it as the revision number"
  (interactive "P")
  (require 'ediff)
  (ediff-load-version-control t)
  (let ((rev (if arg (format "%d" arg) "")))
    (funcall
     (intern (format "ediff-%S-internal" ediff-version-control-package))
     rev "" nil))
  )

;; Re-indent region (or buffer) plus cleanup whitespace
(global-set-key [(control c) (\\)] 'pw/reindent)
(defun pw/reindent ()
  "Reindent and cleanup whitespace.  If the region is active then
this only applies to the region otherwise the entire buffer.
See `whitespace-style' to customize which whitespace is cleaned."
  (interactive)
  (if (use-region-p)
      (progn
	(indent-region (region-beginning) (region-end))
	(whitespace-cleanup-region (region-beginning) (region-end)))
    ;; else
    (indent-region (point-min) (point-max))
    (whitespace-cleanup)))

;; Toggle truncation of long lines
(global-set-key [(control c) ($)] 'pw/trunc-lines)
(defun pw/trunc-lines (&optional arg)
  "Toggle `truncate-lines' so long lines stay on a single line instead of wrapping.
If arg is positive, always enable truncate lines.  If arg is negative or zero,
disable truncate lines."
  (interactive "P")
  (cond
   ((and arg (> (prefix-numeric-value arg) 0))
    (setq truncate-lines t))
   ((and arg (<= (prefix-numeric-value arg) 0))
    (setq truncate-lines nil))
   (t
    (setq truncate-lines (not truncate-lines)))))

(defun pw/region-is-active-p()
  "Return if the region is active.  Accounts for difference between
Emacs and XEmacs."
  (if (fboundp 'region-active-p)
      (region-active-p)
    mark-active))

;; Evaluate either region of current function
(global-set-key [(control c) (e)] 'pw/eval-region-or-defun)
(defun pw/eval-region-or-defun ()
  "Eval the active region (if it's active) otherwise the current defun."
  (interactive)
  (if (pw/region-is-active-p)
      (eval-region (region-beginning) (region-end))
    (eval-defun nil)))

(if (fboundp 'server-start)
    (progn
      (if (not (string-match "emacsclient" (or (getenv "EDITOR") "")))
	  (setenv "EDITOR" "emacsclient"))
      (server-start t))
  ;;else
  (if (fboundp 'gnuserv-start)
      (progn
	(setenv "EDITOR" "gnuclient")
	(gnuserv-start))))

(if (and (featurep 'color-theme)
	 (featurep 'color-theme-pw))
    (color-theme-pw))

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
     ("http" . "devproxy.bloomberg.com:82")
     ("https" . "devproxy.bloomberg.com:82")))

