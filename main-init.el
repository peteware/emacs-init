;; Preamble
;;     This is at the beginning of main-init.el:

;; This file is generated from main-init.org
;; using (org-babel-tangle).  Do not
;; make changes in this file; they'll be lost!
(provide 'main-init)

;; Profile startup times
;;     This is from a good article https://blog.d46.us/advanced-emacs-startup/
;;     about speeding up emacs startup.

;;     Use a hook so the message doesn't get clobbered by other messages.

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; GC threshold
;;     Temporarily increase GC threshold to 100MB then reduce to 800MB
;;     (was 800KB) after startup.

(setq gc-cons-threshold (* 100 1000 1000))
(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold (* 8 1000 1000))))

;; Setup proxies, certificates for package installation
;;     To get ~package-install~ to work you may need to setup some
;;     proxies and some SSL certificates for those proxies

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


(setq-default cursor-type '(hbar . 3))
(setq-default cursor-in-non-selected-windows nil)
(setq initial-frame-alist
      '((width . 110)
        (height . 60)
        (top . 29)
        (left . 88)
        (cursor-background "red")))
(setq default-frame-alist
      '((width . 110)
        (height . 60)
        (top . 29)
        (left . 1.0)
        (cursor-background "red")))
(set-face-attribute 'cursor nil :background "red")
(when (eq 'ns (window-system))
  (add-to-list 'default-frame-alist
               '(font . "JetBrains Mono-14"))
  (add-to-list 'default-frame-alist
               '(alpha . (90 . 70))))

;; Install the straight.el package manager

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

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
;;    You can also not use a package by adding :disabled t to use-package

;;    I also like having ~use-package~ collect some info about
;;    the loaded packages and how long they take to load.  You
;;    can see the results with =M-x use-package-report=.


;(setq use-package-verbose t)
(straight-use-package 'use-package)
(setq use-package-compute-statistics t)
(require 'use-package)

;; bind-key
;;     Using bind-key lets you run describe-personal-keybindings
;;     which is a nice way of keep track of what you've changed.

(use-package bind-key
  :bind (
         ("C-c G" . 'goto-line)
         ("C-c o" . 'other-frame)
         ("<wheel-left>" . 'ignore)
         ("<wheel-right>" . 'ignore)
         ("<double-wheel-left>" . 'ignore)
         ("<double-wheel-right>" . 'ignore)
         ("<triple-wheel-left>" . 'ignore)
         ("<triple-wheel-right>" . 'ignore)
         ))

;; bookmark
;;     You can save bookmarks with =C-x r m= and jump to them wih =C-x r b=
;;     This makes them save automatically


(use-package bookmark
  :defer 5
  :config
  (setq bookmark-save-flag 1))

;; cc-mode
;;     Configure to put .h in c++-mode

(use-package cc-mode
  :commands (c-mode c++-mode)
  :mode ("\\.h$" . c++-mode)
  :config
  (setq c-tab-always-indent nil))

;; delsel
;;     I can't handle the active region getting deleted


(use-package delsel
  :defer 5
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
    (setq desktop-restore-eager 5)
    (setq desktop-restore-in-current-display t)
    (setq desktop-lazy-verbose nil)
    (setq desktop-lazy-idle-delay 20)
    (setq desktop-auto-save-timeout 7200)
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
  :defer 2
  :config
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p))

;; face-remap
;;     Change the font size in the current buffer (not the window)


(use-package face-remap
  :defer 5
  :bind* (("C-c -" . text-scale-decrease)
          ("C-c +" . text-scale-increase)))

;; jit-lock
;;     Setup lazy font locking


(use-package jit-lock
  :config
  (progn
    (setq jit-lock-defer-time 0.1)
    (jit-lock-mode t)))

;; jka-cmpr-hook
;;     Make visiting a *.gz automatically uncompress file


(use-package jka-cmpr-hook
  :defer 5
  :config
  (auto-compression-mode 1))

;; mwheel
;;     Make sure the mouse wheel scrolls


(use-package mwheel
  :defer 1
  :config
  (progn
    (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
    (setq mouse-wheel-progressive-speed nil)
    (mwheel-install)))

;; outline


(use-package outline
  :defer 5
  :hook (prog-mode . outline-minor-mode))

;; paren
;;     Highlight matching paren


(use-package paren
  :defer 5
  :config
  (progn
    (setq show-paren-when-point-in-periphery nil)
    (show-paren-mode 1)))

;; recentf

(use-package recentf
  ;;
  ;; Save list of recently visited files
  :defer 5
  :config
  (progn
    (setq recentf-max-saved-items 100)
    (setq recentf-auto-cleanup 3600)    ;cleanup after idle 1hr
    (recentf-mode 1)))

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
  :defer 5
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
  :defer 1
  :config
  (progn
    (setq uniquify-buffer-name-style 'post-forward)
    (setq uniquify-after-kill-buffer-p t)))

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

;; bb-style
;;     Bloomberg C++ coding style

(use-package bb-style
  :hook (c-mode-common . bb-c-mode))

;; delight

;;     This package makes it easy to hide minor
;;     modes in the modeline.  Uses for :diminish

(use-package delight
  :defer 5
  :straight t)

;; ivy
;;     ~ivy~ changes completion so that matches are
;;     found via regular expressions and matches are
;;     navigable by moving up and down lines.  Replaces
;;     ~ido~ and ~iswitchb~.

(use-package ivy
  :straight t
  :defer 1
  :delight ivy-mode
  :bind (("C-c C-r" . 'ivy-resume))
  :config (progn
            (setq ivy-wrap t)
            (setq ivy-use-virtual-buffers t)
            (setq ivy-count-format "(%d/%d) ")
            (ivy-mode)))


;; This adds some nice info when choosing buffers

(use-package ivy-rich
  :after ivy
  :straight (:host github :repo "Yevgnen/ivy-rich")
  :config (progn
            (plist-put ivy-rich-display-transformers-list 'ivy-switch-buffer
                       (plist-put (plist-get  ivy-rich-display-transformers-list 'ivy-switch-buffer)
                                  ':columns '((ivy-rich-candidate (:width 0.40))
                                              (ivy-rich-switch-buffer-size (:width 7))
                                              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                                              (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                                              (ivy-rich-switch-buffer-project (:width 15 :face success))
                                              (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))))

            (ivy-rich-mode 1)))

;; counsel
;;     ~counsel~ builds on completion for ivy but adds
;;     searches across files.

(use-package counsel
  :after ivy
  :straight t
  :delight counsel-mode
  :bind (("C-c g" .  'counsel-git)
         ("C-c j" .  'counsel-file-jump)
         ("C-c k" .  'counsel-ag)
         ("C-x b" .  'counsel-switch-buffer)
         ;("C-c s" .  'counsel-switch-to-shell-buffer)
         )
  :config 
  (progn (counsel-mode)
         (setq counsel-find-file-ignore-regexp "\\.*\\(pyc\\|.o\\|.tsk\\)$")))



;; And I have some hacks to be a little smarter when switching


(use-package counsel-shell-switch
  :after counsel
  :bind (("C-c s" . 'pw/counsel-switch-to-shell-buffer)))

;; swiper
;;     This changes incremental search to use ivy style completion
;;     but displays all the matching lines in the completion buffer.

(use-package swiper
  :after ivy
  :straight t
  :bind (("M-s" . 'swiper)
         ("C-s" . 'swiper-isearch)
         ("C-r" . 'swiper-isearch-backward)))

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
    (scratch-ext-create-scratch)
    (set-buffer "*scratch*")
    (scratch-ext-restore-last-scratch)))

;; toolkit-tramp


(use-package toolkit-tramp
  :defer 60
  :config
  (progn
    (setq password-cache-expiry nil)
    (setq tramp-use-ssh-controlmaster-options nil)))

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
  :commands orgstruct-mode
  :delight orgstruct-mode
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c r" . org-capture))
  :init (add-hook 'c-mode-common-hook 'orgstruct-mode)
  :config (progn
            (setq org-export-backends '(ascii html icalendar latex md))
            (setq org-list-allow-alphabetical t)))



;; Additionally, I have a number of customizations I like to use
;; for org-mode.


(use-package org-prefs
  :after org)

;; tramp
;;     This provides remote access to files and shells.  

(use-package tramp
  :defer t
  :config
  (setq tramp-use-ssh-controlmaster-options nil
        tramp-copy-size-limit 1024))

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
  :straight t
  :bind ("C-c i" . anyins-mode))

;; comint-prefs
    
;;     Setup preferences for shell, compile and other comint based commands


(use-package comint-prefs
  :after comint
  :commands (comint-for-pete dbx-for-pete comint-watch-for-password-prompt pw/turn-off-fontlock)
  :init
  (progn
    (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
    (add-hook 'comint-mode-hook 'comint-for-pete)
    (add-hook 'dbx-mode-hook 'dbx-for-pete)
    (add-hook 'compilation-mode-hook 'pw/turn-off-fontlock)))

;; csc-mode
;;     Bloomberg database schema

(use-package csc-mode
  :mode ("\\.csc2$" . csc-mode))

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
            (setq magit-commit-show-diff nil)
            (setq magit-refresh-verbose t)
            (setq magit-save-repository-buffers nil)
            (setq magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))
            (setq magit-view-git-manual-method 'man)
            (setq magit-auto-revert-tracked-only t)
            (setq vc-handled-backends nil)
            (magit-auto-revert-mode 1)))

;; magit-todo

;;     Include TODO, etc in the magit buffer


(use-package magit-todos
  :after magit
  :straight t
  :config (magit-todos-mode))

;; forge
;;     This implements an interface to github that
;;     integrates with magit


(use-package forge
  :disabled t
  :after magit)

;; multiple-cursors
    
;;     You can place multiple cursors in a buffer
;;     and have whatever you do affect each item

(use-package multiple-cursors
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
  :config
  (add-hook 'compilation-mode-hook 'pw/no-line-column-number))

(use-package pw-misc
  :bind (("C-c p" . pw/prev-frame)
         ("C-c \\" . pw/reindent)
         ("C-c e" . pw/eval-region-or-defun)))

;; pw-trunc-lines
    
;;     Toggle truncation of long lines

(use-package pw-trunc-lines
  :commands pw/trunc-lines
  :bind ("C-c $" . pw/trunc-lines)
  :hook ((c-mode-common makefile-gmake-mode compilation-mode shell-mode) . pw/trunc-lines))

;; treemacs

(use-package treemacs
  :straight t
  :bind (("C-x p" . treemacs-select-window)
         ("C-x t" . treemacs))
  :config
  (progn
    (defun pw/treemacs-ignore (file path)
      (string-match-p "\\.pyc$\\|\\.sundev1\\.\\|\\.o$\\|\\.d$\\|__pycache__" file))
    (add-hook 'treemacs-ignored-file-predicates 'pw/treemacs-ignore)
    (setq treemacs-show-hidden-files nil)
    (setq treemacs-collapse-dirs 2)))

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
  :bind* (("C-c [" . zoom-frm-out)
          ("C-c ]" . zoom-frm-in)))

;; powerline
    
;;     Make the modeline have lots of pretty graphics.

;;     For `iterm2` I had to install some extra fonts
;;     for these to look good:

;;          https://github.com/powerline/fonts

    

(use-package powerline
  :straight (:host github :repo "milkypostman/powerline")
  :config
  (progn
    (powerline-default-theme)))

;; nord theme
;;     I've been trying to find a theme that works well
;;     with iterm2, emacs-25 and emacs-26.

;;     If using iterm, you should change it to use following
;;     color scheme:

;;     https://github.com/arcticicestudio/nord-iterm2


(use-package nord-theme
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



;; Force Mac OS X to use Consolas at 16pt

(if (eq (window-system) 'ns)
    (custom-set-faces '(default ((t (:height 160 :family "Consolas" :cursor-color "red"))))))

;; Clean startup

;;     Do not display message in the scratch buffer or the startup message
;;     or the message in the echo area

(setq initial-scratch-message "")
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message "pware")

;; Configure the mode line

;;     Turn on displaying the date and time in the mode line.
;;     Enable displaying the line and column numbers in the mode line
;;     But don't do that if the buffer is >250k
;;     Do not blink the cursor

(setq display-time-day-and-date t)
(setq line-number-display-limit 250000)
(display-time-mode)
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
(blink-cursor-mode -1)

;; Legacy (or I've been using emacs for too long)

;;     If at beginning of line, the Ctl-K kills including the newline
;;     (I'm hardwired to type Ctl-K twice)

;;     ;(setq kill-whole-line t)


;;     Latest Emacs can wrap lines at word boundaries and will move the cursor
;;     so it stays in the same column on screen.  I'm too used to the old style.

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

;; Misc settings
;;     Cause the gutter to display little arrows and
;;     boxes if there is more to a file

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



;; I found visiting a file to be really slow and realized
;; it was from figuring out the version control

(setq vc-handled-backends nil)



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

;; menu-bar (disabled)
;;     Turn the menubar off.
    
;;     - *DISABLED* (Turns out I like the menu-bar!)


(use-package menu-bar
  :disabled t
  :config
  (menu-bar-mode -1))
