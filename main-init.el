
;; Preamble
;;     This is at the beginning of main-init.el:

;; [[file:~/usr/emacs/main-init.org::*Preamble][Preamble:1]]

(provide 'main-init)

;; Preamble:1 ends here

;; Setup proxies for package installation
;;     To get ~package-install~ to work you may need to setup some
;;     proxies.  This is specific to a corp desktop pc keyed off
;;     the assumption I only every run cygwin in that environment.

;; [[file:~/usr/emacs/main-init.org::*Setup%20proxies%20for%20package%20installation][Setup\ proxies\ for\ package\ installation:1]]

(when (or (string-equal system-type "windows-nt")
          (string-equal system-type "cygwin"))
  (setq password-cache-expiry nil)
  (setq url-proxy-services '(("http" . "proxy.bloomberg.com:81"))))

;; Setup\ proxies\ for\ package\ installation:1 ends here

;; Setup use-package
;;    You may need to =M-x package-install use-package= before
;;    any of this works
   
;;    If a package is not available then ~use-package~ ignores it.
;;    You can also not use a package by adding :disabled t to use-package

;; [[file:~/usr/emacs/main-init.org::*Setup%20use-package][Setup\ use-package:1]]

(eval-when-compile
  (require 'use-package))

(setq use-package-compute-statistics t)

;; Setup\ use-package:1 ends here

;; package
;;     Use the emacs packaging system to automatically install some packages

;; [[file:~/usr/emacs/main-init.org::*package][package:1]]

(use-package package
  :config
  (progn
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
    (package-initialize t)))

;; package:1 ends here

;; bind-key
;;     Using bind-key lets you run describe-personal-keybindings
;;     which is a nice way of keep track of what you've changed.

;; [[file:~/usr/emacs/main-init.org::*bind-key][bind-key:1]]

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

;; bind-key:1 ends here

;; bookmark
;;     You can save bookmarks with =C-x r m= and jump to them wih =C-x r b=
;;     This makes them save automatically

;; [[file:~/usr/emacs/main-init.org::*bookmark][bookmark:1]]

(use-package bookmark
  :defer 60
  :config
  (setq bookmark-save-flag 1))

;; bookmark:1 ends here

;; delsel
;;     I can't handle the active region getting deleted

;; [[file:~/usr/emacs/main-init.org::*delsel][delsel:1]]

(use-package delsel
  :config
  (delete-selection-mode -1))

;; delsel:1 ends here

;; desktop
;;     This causes the set of files being visited to be restored
;;     on startup.

;; [[file:~/usr/emacs/main-init.org::*desktop][desktop:1]]

(use-package desktop
  ;:defer 10
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

;; desktop:1 ends here

;; executable
;;     This makes saving shell scripts automatically make
;;     them executable.  It's considered a shell script if
;;     it starts with #!

;; [[file:~/usr/emacs/main-init.org::*executable][executable:1]]

(use-package executable
  ;:defer 60
  :config
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p))

;; executable:1 ends here

;; face-remap
;;     Change the font size in the current buffer (not the window)

;; [[file:~/usr/emacs/main-init.org::*face-remap][face-remap:1]]

(use-package face-remap
  :bind* (("C-c -" . text-scale-decrease)
          ("C-c +" . text-scale-increase)))

;; face-remap:1 ends here

;; jit-lock
;;     Setup lazy font locking

;; [[file:~/usr/emacs/main-init.org::*jit-lock][jit-lock:1]]

(use-package jit-lock
  :config
  (jit-lock-mode t))

;; jit-lock:1 ends here

;; jka-cmpr-hook
;;     Make visiting a *.gz automatically uncompress file

;; [[file:~/usr/emacs/main-init.org::*jka-cmpr-hook][jka-cmpr-hook:1]]

(use-package jka-cmpr-hook
  :config
  (auto-compression-mode 1))

;; jka-cmpr-hook:1 ends here

;; mwheel
;;     Make sure the mouse wheel scrolls

;; [[file:~/usr/emacs/main-init.org::*mwheel][mwheel:1]]

(use-package mwheel
  :config
  (progn
    (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
    (setq mouse-wheel-progressive-speed nil)
    (mwheel-install)))

;; mwheel:1 ends here

;; outline

;; [[file:~/usr/emacs/main-init.org::*outline][outline:1]]

(use-package outline
  :config
  (add-hook 'prog-mode-hook 'outline-minor-mode))

;; outline:1 ends here

;; paren
;;     Highlight matching paren

;; [[file:~/usr/emacs/main-init.org::*paren][paren:1]]

(use-package paren
  :defer 60
  :config
  (show-paren-mode 1))

;; paren:1 ends here

;; recentf

;; [[file:~/usr/emacs/main-init.org::*recentf][recentf:1]]

(use-package recentf
  ;;
  ;; Save list of recently visited files
  :defer 15
  :config
  (progn
    (setq recentf-max-saved-items 100)
    (setq recentf-auto-cleanup 3600)    ;cleanup after idle 1hr
    (recentf-mode 1)))

;; recentf:1 ends here

;; savehist

;; [[file:~/usr/emacs/main-init.org::*savehist][savehist:1]]

(use-package savehist
  ;;
  ;; Save emacs's internal command history.
  :defer 15
  :config
  (progn
    (setq savehist-additional-variables
          '(compile-command
            grep-find-history
            grep-history
            grep-regexp-history
            grep-files-history))
    (savehist-mode 1)))

;; savehist:1 ends here

;; saveplace
;;     This records the location of every file you visit and
;;     restores when you vist a file, goes to that location.  I also save
;;     the file every couple hours because I don't always quit emacs

;; [[file:~/usr/emacs/main-init.org::*saveplace][saveplace:1]]

(use-package saveplace
  :defer 30
  :config
  (progn
    (setq-default save-place t)
    (setq save-place-limit nil)
    (run-at-time 3600  3600 'save-place-alist-to-file)))

;; saveplace:1 ends here

;; scroll-bar
    
;;     Turn off the scroll bars

;; [[file:~/usr/emacs/main-init.org::*scroll-bar][scroll-bar:1]]

(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

;; scroll-bar:1 ends here

;; server
;;     Make it so $EDITOR can popup in this emacs

;; [[file:~/usr/emacs/main-init.org::*server][server:1]]

(use-package server
  :config
  (progn
    (if (not (string-match "emacsclient" (or (getenv "EDITOR") "")))
        (setenv "EDITOR" "emacsclient"))
    (message "server-start")
    (server-start)))

;; server:1 ends here

;; toolkit-tramp

;; [[file:~/usr/emacs/main-init.org::*toolkit-tramp][toolkit-tramp:1]]

(use-package toolkit-tramp
  :defer 60
  :config
  (setq password-cache-expiry nil))

;; toolkit-tramp:1 ends here

;; uniquify
;;     Make it so buffers with the same name are are made unique by added
;;     directory path and killing a buffer renames all of them.

;; [[file:~/usr/emacs/main-init.org::*uniquify][uniquify:1]]

(use-package uniquify
  :config
  (progn
    (setq uniquify-buffer-name-style 'post-forward)
    (setq uniquify-after-kill-buffer-p t)))

;; uniquify:1 ends here

;; atomic-chrome
;;     You must first install Atomic Chrome extension from Chrome Web
;;     Store and this allows editting text areas in Chrome via
;;     a two-way connection.

;; [[file:~/usr/emacs/main-init.org::*atomic-chrome][atomic-chrome:1]]

(use-package atomic-chrome
  :config
  (atomic-chrome-start-server))

;; atomic-chrome:1 ends here

;; bb-style
;;     Bloomberg C++ coding style

;; [[file:~/usr/emacs/main-init.org::*bb-style][bb-style:1]]

(use-package bb-style
  :config
  (progn
    ;; Use bb-style for C/C++; associate .h files with c++-mode instead of
    ;; c-mode
    (setq c-default-style "bb")
    (setq c-tab-always-indent nil)
    (add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
  ))

;; bb-style:1 ends here

;; delight

;;     This package makes it easy to hide minor
;;     modes in the modeline.  Uses for :diminish

;; [[file:~/usr/emacs/main-init.org::*delight][delight:1]]

(use-package delight
  :ensure t)

;; delight:1 ends here

;; fancy-narrow
;;     Causes narrow region to dim the
;;     rest of the buffer giving a much
;;     more natual look.

;; [[file:~/usr/emacs/main-init.org::*fancy-narrow][fancy-narrow:1]]

(use-package fancy-narrow
  :delight fancy-narrow-mode
  :config
  (fancy-narrow-mode 1))

;; fancy-narrow:1 ends here

;; ivy
;;     ~ivy~ changes completion so that matches are
;;     found via regular expressions and matches are
;;     navigable by moving up and down lines.  Replaces
;;     ~ido~ and ~iswitchb~.

;; [[file:~/usr/emacs/main-init.org::*ivy][ivy:1]]

(use-package ivy
  :ensure t
  :delight ivy-mode
  :bind (("C-c C-r" . 'ivy-resume))
  :config (progn
            (setq ivy-wrap t)
            (setq ivy-use-virtual-buffers t)
            (setq ivy-count-format "(%d/%d) ")
            (ivy-mode)))

;; ivy:1 ends here

;; counsel
;;     ~counsel~ builds on completion for ivy but adds
;;     searches across files.

;; [[file:~/usr/emacs/main-init.org::*counsel][counsel:1]]

(use-package counsel
  :after ivy
  :ensure t
  :delight counsel-mode
  :bind (("C-c g" .  'counsel-git)
         ("C-c j" .  'counsel-git-grep)
         ("C-c k" .  'counsel-ag)
         ("C-x l" .  'counsel-locate)
         ("C-S-o" .  'counsel-rhythmbox)
         )
  :config (progn (counsel-mode)))

;; counsel:1 ends here

;; swiper
;;     This changes incremental search to use ivy style completion
;;     but displays all the matching lines in the completion buffer.

;; [[file:~/usr/emacs/main-init.org::*swiper][swiper:1]]

(use-package swiper
  :after ivy
  :ensure t
  :bind (("C-s" . 'swiper)))

;; swiper:1 ends here

;; scratch-ext
;;     Make *scratch* buffers get saved

;; [[file:~/usr/emacs/main-init.org::*scratch-ext][scratch-ext:1]]

(use-package scratch-ext
  :ensure t
  :config
  (save-excursion
    (setq scratch-ext-log-directory "~/.emacs.d/scratch")
    (if (not (file-exists-p scratch-ext-log-directory))
        (mkdir scratch-ext-log-directory t))
    (scratch-ext-create-scratch)
    (set-buffer "*scratch*")
    (scratch-ext-restore-last-scratch)))

;; scratch-ext:1 ends here

;; compile
;;     Setup compilation buffers

;; [[file:~/usr/emacs/main-init.org::*compile][compile:1]]

(use-package compile
  :bind ("C-c c" . compile)
  :config
  (progn
    (setq compilation-scroll-output 'first-error)))

;; compile:1 ends here

;; ansi-color

;; [[file:~/usr/emacs/main-init.org::*ansi-color][ansi-color:1]]

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

;; ansi-color:1 ends here

;; ediff
;;     A nice graphical diff Make sure that ediff ignores all whitespace
;;     differences and highlights the individual differences

;; [[file:~/usr/emacs/main-init.org::*ediff][ediff:1]]

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

;; ediff:1 ends here

;; follow
;;     This makes a single file wrap around between two windows.
;;     Try ^X-3 and then move to the top or bottom of the window
;;     and the other window scrolls.  I bound F7 to do get
;;     rid of the other windows and split.

;; [[file:~/usr/emacs/main-init.org::*follow][follow:1]]

(use-package follow
  :bind ("<f7>" . follow-delete-other-windows-and-split))

;; follow:1 ends here

;; grep
;;     ~rgrep~ recursively greps for a pattern.  It uses a key to specify
;;     filenames and ignores directories like CVS.  "cchh" is all C++
;;     files and headers.

;; [[file:~/usr/emacs/main-init.org::*grep][grep:1]]

(use-package grep
  ;:bind (("C-c g" . grep))
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

;; grep:1 ends here

;; hideshow
;;     Setup commands and menus to hide/show blocks of code

;; [[file:~/usr/emacs/main-init.org::*hideshow][hideshow:1]]

(use-package hideshow
  :commands hs-minor-mode
  :init
  (progn
    (add-hook 'c++-mode-hook 'hs-minor-mode)
    (add-hook 'c-mode-hook 'hs-minor-mode)))

;; hideshow:1 ends here

;; linum
;;     Make it so line numbers show up in left margin Used in C/C++
;;     mode.  (Tried nlinum but had refresh problems)

;; [[file:~/usr/emacs/main-init.org::*linum][linum:1]]

(use-package linum
  :commands linum-mode
  :init (add-hook 'prog-mode-hook 'linum-mode)
  :config (setq linum-format 'dynamic))

;; linum:1 ends here

;; org
;;     org-mode provides an outline, todo, diary, calendar like interface.

;; [[file:~/usr/emacs/main-init.org::*org][org:1]]

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :commands orgstruct-mode
  :delight orgstruct-mode
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c r" . org-capture))
  :init (add-hook 'c-mode-common-hook 'orgstruct-mode)
  :config
  (use-package org-prefs))

;; org:1 ends here

;; Additionally, I have a number of customizations I like to use
;;     for org-mode.

;; [[file:~/usr/emacs/main-init.org::*org][org:1]]

(use-package org-prefs
  :after org)

;; org:1 ends here

;; whitespace
;;     Make "bad" whitespace be visible.  This causes tabs, and whitespace
;;     at beginning and end of the buffer as well as at the end of the
;;     line to highlight
    
;;     Use =M-x whitespace-cleanup= to fix all problems

;; [[file:~/usr/emacs/main-init.org::*whitespace][whitespace:1]]

(use-package whitespace
  :bind ("C-c SPC" . whitespace-mode)
  :config
  (progn
    (setq whitespace-style '(face trailing tabs empty indentation::space lines-tail))
    (setq whitespace-line-column nil)))

;; whitespace:1 ends here

;; pw-misc

;; [[file:~/usr/emacs/main-init.org::*pw-misc][pw-misc:1]]

(use-package pw-misc
  :after compile
  :config
  (add-hook 'compilation-mode-hook 'pw/no-line-column-number))

;; pw-misc:1 ends here

;; anyins
;;     Freaky way to insert text
;;     1. Enter anyins-mode
;;     2. Move around; mark spots you want to insert text with RET
;;     3. To insert text

;;        a. =y= inserts each line from kill ring at each marked spot, or
;;        b.  =!= runs a shell command line 'seq -s ". \n" 1 3' generates
;;            numbers "1. "  "2. " "3. " and inserts it at each markets tpot

;; [[file:~/usr/emacs/main-init.org::*anyins][anyins:1]]

(use-package anyins
  ;;
  ;; Download package if not installed!
  :ensure t
  :bind ("C-c i" . anyins-mode))

;; anyins:1 ends here

;; avy
;;     Fast way to jump to a specific character.  Prompts for
;;     a character and then displays all of them but replaced
;;     with leters a,b,c,...  You then type in which one to jump
;;     to.

;; [[file:~/usr/emacs/main-init.org::*avy][avy:1]]

(use-package avy
  :ensure t
  :bind (("M-s" . avy-goto-word-1))
  :config (setq avi-all-windows nil))

;; avy:1 ends here

;; beacon
;;     Highlight the line the point is on when the screen jumps around.

;; [[file:~/usr/emacs/main-init.org::*beacon][beacon:1]]

(use-package beacon
  :config
  (progn
    (beacon-mode 1)
    (setq beacon-push-mark 35)
    (setq beacon-color "#666600")))

;; beacon:1 ends here

;; comint-prefs
    
;;     Setup preferences for shell, compile and other comint based commands

;; [[file:~/usr/emacs/main-init.org::*comint-prefs][comint-prefs:1]]

(use-package comint-prefs
  :after comint
  :commands (comint-for-pete dbx-for-pete comint-watch-for-password-prompt)
  :init
  (progn
    (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
    (add-hook 'comint-mode-hook 'comint-for-pete)
    (add-hook 'dbx-mode-hook 'dbx-for-pete))  )

;; comint-prefs:1 ends here

;; csc-mode
;;     Bloomberg database schema

;; [[file:~/usr/emacs/main-init.org::*csc-mode][csc-mode:1]]

(use-package csc-mode
  :mode ("\\.csc2$" . csc-mode))

;; csc-mode:1 ends here

;; lrl-mode
;;     Bloomberg database params

;; [[file:~/usr/emacs/main-init.org::*lrl-mode][lrl-mode:1]]

(use-package lrl-mode
  :mode ("\\.lrl\\'" . lrl-mode))

;; lrl-mode:1 ends here

;; magit
    
;;     Provide a way of interacting with a Git repository.
    
;;     Download package if not installed!

;; [[file:~/usr/emacs/main-init.org::*magit][magit:1]]

(use-package magit
  :ensure t
  :bind (("C-c m" . magit-status)
         ("C-c C-m" . magit-dispatch-popup))
  :delight '(magit-wip-after-save-mode
             magit-wip-after-save-local-mode
             magit-wip-after-apply-mode
             magit-wip-before-change-mode
             auto-revert-mode)
  :config (progn
            (magit-wip-after-save-mode)
            (magit-wip-after-apply-mode)
            (magit-wip-before-change-mode)
            (add-hook 'magit-status-headers-hook 'magit-insert-repo-header)
            (add-hook 'magit-status-headers-hook 'magit-insert-remote-header)
            (setq magit-commit-show-diff nil)
            (setq auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffers-p)
            (remove-hook 'server-switch-hook 'magit-commit-diff)
            (setq magit-refresh-verbose t)
            (setq magit-save-repository-buffers nil)
            (setq magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))
            (setq magit-view-git-manual-method 'man)
            (setq vc-handled-backends nil)))

;; magit:1 ends here

;; multiple-cursors
    
;;     You can place multiple cursors in a buffer
;;     and have whatever you do affect each item

;; [[file:~/usr/emacs/main-init.org::*multiple-cursors][multiple-cursors:1]]

(use-package multiple-cursors
  :bind (("C-. e" . mc/edit-lines)
         ("C-. >" . mc/mark-next-like-this)
         ("C-. <" . mc/mark=previous-like-this)))

;; multiple-cursors:1 ends here

;; ag
;;     A fast search across lots of files.  Relies
;;     on package silver searcher for the executable
;;     to be installed.

;; [[file:~/usr/emacs/main-init.org::*ag][ag:1]]

(use-package ag
  :ensure t
  :bind (("C-c f" . ag))
  :config (setq ag-reuse-buffers t))

;; ag:1 ends here

;; pw-misc
    
;;     Some commands I find useful

;; [[file:~/usr/emacs/main-init.org::*pw-misc][pw-misc:1]]

(use-package pw-misc
  :bind (("C-c p" . pw/prev-frame)
         ("C-c \\" . pw/reindent)
         ("C-c e" . pw/eval-region-or-defun)))

;; pw-misc:1 ends here

;; pw-trunc-lines
    
;;     Toggle truncation of long lines

;; [[file:~/usr/emacs/main-init.org::*pw-trunc-lines][pw-trunc-lines:1]]

(use-package pw-trunc-lines
  :commands pw/trunc-lines
  :bind ("C-c $" . pw/trunc-lines)
  :init
  (progn
    (add-hook 'prog-mode-hook 'pw/trunc-lines)
    (add-hook 'makefile-gmake-mode-hook 'pw/trunc-lines)
    (add-hook 'compilation-mode-hook 'pw/trunc-lines)
    (add-hook 'shell-mode-hook 'pw/trunc-lines)))

;; pw-trunc-lines:1 ends here

;; shell-switch
    
;;     Pete's hack to make switching to a shell buffer
;;     faster

;; [[file:~/usr/emacs/main-init.org::*shell-switch][shell-switch:1]]

(use-package shell-switch
  :commands (shell-switch shell-switch-other-window)
  :init
  (progn
    (bind-key* "C-c s" 'shell-switch)
    (bind-keys* :prefix-map clt-c-4-keymap
                :prefix "C-c 4"
                ("s" . shell-switch-other-window))))

;; shell-switch:1 ends here

;; treemacs

;; [[file:~/usr/emacs/main-init.org::*treemacs][treemacs:1]]

(use-package treemacs
  :ensure t
  :bind (("C-x p" . treemacs-select-window)
         ("C-x t" . treemacs))
  :config
  (progn
    (defun pw/treemacs-ignore (file path)
      (string-match-p "\.pyc$\\|\.sundev1\.\\|\.o$" file))
    (add-hook 'treemacs-ignored-file-predicates 'pw/treemacs-ignore)
    (setq treemacs-show-hidden-files nil)
    (setq treemacs-collapse-dirs 2)))

;; treemacs:1 ends here

;; wgrep
;;     This lets you save the results from grep, edit those results and then
;;     saving the changes applies them to each file.

;; [[file:~/usr/emacs/main-init.org::*wgrep][wgrep:1]]

(use-package wgrep
  :ensure t)

;; wgrep:1 ends here

;; zoom-frm
    
;;     Much like face-remap that adds test-scale-increase and
;;     text-scale-decrease I use this to change the entire window
;;     instead of the buffer

;; [[file:~/usr/emacs/main-init.org::*zoom-frm][zoom-frm:1]]

(use-package zoom-frm
  :bind* (("C-c [" . zoom-frm-out)
          ("C-c ]" . zoom-frm-in)))

;; zoom-frm:1 ends here

;; powerline
    
;;     Make the modeline have lots of pretty graphics.

;; [[file:~/usr/emacs/main-init.org::*powerline][powerline:1]]

(use-package powerline
  :config
  (progn
    (powerline-center-theme)))

;; powerline:1 ends here

;; overcast-theme

;; [[file:~/usr/emacs/main-init.org::*overcast-theme][overcast-theme:1]]

(use-package overcast-theme
  :ensure t
  :config
  (load-theme 'overcast t))

;; overcast-theme:1 ends here

;; Various preferences

;;    Allow narrow to region (e.g. =C-X n n=)

;; [[file:~/usr/emacs/main-init.org::*Various%20preferences][Various\ preferences:1]]

(put 'narrow-to-region 'disabled nil)

;; Various\ preferences:1 ends here

;; Force Mac OS X to use Consolas at 16pt

;; [[file:~/usr/emacs/main-init.org::*Various%20preferences][Various\ preferences:1]]

(if (eq (window-system) 'ns)
    (custom-set-faces '(default ((t (:height 160 :family "Consolas"))))))

;; Various\ preferences:1 ends here

;; Clean startup

;;     Do not display message in the scratch buffer or the startup message
;;     or the message in the echo area

;; [[file:~/usr/emacs/main-init.org::*Clean%20startup][Clean\ startup:1]]

(setq initial-scratch-message "")
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message "pware")

;; Clean\ startup:1 ends here

;; Configure the mode line

;;     Turn on displaying the date and time in the mode line.
;;     Enable displaying the line and column numbers in the mode line
;;     But don't do that if the buffer is >250k
;;     Do not blink the cursor

;; [[file:~/usr/emacs/main-init.org::*Configure%20the%20mode%20line][Configure\ the\ mode\ line:1]]

(setq display-time-day-and-date t)
(setq line-number-display-limit 250000)
(display-time-mode)
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
(blink-cursor-mode -1)

;; Configure\ the\ mode\ line:1 ends here

;; Legacy (or I've been using emacs for too long)

;;     If at beginning of line, the Ctl-K kills including the newline
;;     (I'm hardwired to type Ctl-K twice)

;;     ;(setq kill-whole-line t)


;;     Latest Emacs can wrap lines at word boundaries and will move the cursor
;;     so it stays in the same column on screen.  I'm too used to the old style.

;; [[file:~/usr/emacs/main-init.org::*Legacy%20(or%20I've%20been%20using%20emacs%20for%20too%20long)][Legacy\ \(or\ I\'ve\ been\ using\ emacs\ for\ too\ long\):1]]

(setq-default word-wrap nil)
(setq line-move-visual nil)
(setq visual-line-mode nil)

;; Legacy\ \(or\ I\'ve\ been\ using\ emacs\ for\ too\ long\):1 ends here

;; Tune scrolling behaviour

;;     Make it so moving up or down does it one line at a time.

;;     - ~scroll-step~ 0 works better with Emacs which now supports
;;       ~scroll-conservatively~.
;;     - ~scroll-margin~ says to keep this many lines
;;        above or below so you get some context.
;;     - ~scroll-preserve-screen-position~ says when scrolling pages, keep
;;       point at same physical spot on screen.

;; [[file:~/usr/emacs/main-init.org::*Tune%20scrolling%20behaviour][Tune\ scrolling\ behaviour:1]]

(setq scroll-step 0)
(setq scroll-conservatively 15)
(setq scroll-margin 2)
(setq scroll-preserve-screen-position 'keep)

;; Tune\ scrolling\ behaviour:1 ends here

;; I set horizontal scrolling because I'd have trouble with
;;     long lines in shell output.  This seemed to get
;;     them to display faster by actually slowing things down
    
;;     - ~hscroll-margin~ is how close cursor gets before
;;       doing horizontal scrolling
;;     - ~hscroll-step~ is how far to scroll when marg is reached.

;; [[file:~/usr/emacs/main-init.org::*Tune%20scrolling%20behaviour][Tune\ scrolling\ behaviour:1]]

(setq hscroll-margin 1)
(setq hscroll-step 5)

;; Tune\ scrolling\ behaviour:1 ends here

;; Incremental search highlighting
;;     Incremental search settings

;; [[file:~/usr/emacs/main-init.org::*Incremental%20search%20highlighting][Incremental\ search\ highlighting:1]]

(setq lazy-highlight-max-at-a-time 10)
(setq lazy-highlight-initial-delay .5)
(setq lazy-highlight-interval .1)

;; Incremental\ search\ highlighting:1 ends here

;; Misc settings
;;     Cause the gutter to display little arrows and
;;     boxes if there is more to a file

;; [[file:~/usr/emacs/main-init.org::*Misc%20settings][Misc\ settings:1]]

(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines t)

;; Misc\ settings:1 ends here

;; Even though I did something with the mouse do not
;;     popup a dialog box but prompt from the mode line

;; [[file:~/usr/emacs/main-init.org::*Misc%20settings][Misc\ settings:1]]

(setq use-dialog-box nil)

;; Misc\ settings:1 ends here

;; This _sounds_ like something that should be nil but
;;     the reality is that when user input stops redisplay
;;     a bunch of screen optimizations are lost.  The
;;     default is prior to emacs-24 is nil

;; [[file:~/usr/emacs/main-init.org::*Misc%20settings][Misc\ settings:1]]

(setq redisplay-dont-pause t)

;; Misc\ settings:1 ends here

;; I found visiting a file to be really slow and realized
;;     it was from figuring out the version control

;; [[file:~/usr/emacs/main-init.org::*Misc%20settings][Misc\ settings:1]]

(setq vc-handled-backends nil)

;; Misc\ settings:1 ends here

;; I don't like actual tabs being inserted

;; [[file:~/usr/emacs/main-init.org::*Misc%20settings][Misc\ settings:1]]

(setq-default indent-tabs-mode nil)

;; Misc\ settings:1 ends here

;; More X11 configuration

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

;; [[file:~/usr/emacs/main-init.org::*More%20X11%20configuration][More\ X11\ configuration:1]]

(setq x-select-enable-clipboard t)

;; More\ X11\ configuration:1 ends here

;; The following puts killed text into the X11 primary cut buffer.
;;     Text copied in an xterm can either be pasted into emacs with a
;;     middle-mouse or the usual yank operations like =C-y=.  You cannot
;;     paste such text into other Window's applications without going through
;;     emacs.  Usualy middle mouse button in an xterm pastes the text
;;     from emacs.

;; [[file:~/usr/emacs/main-init.org::*More%20X11%20configuration][More\ X11\ configuration:1]]

(setq x-select-enable-primary t)

;; More\ X11\ configuration:1 ends here

;; Alternatively, in Exceed, set the "X Selection Associated with
;;     Edit Operations:" to be "PRIMARY" and use these settings.  This lets
;;     older xterm/mrxvt co-exist with Windows applications.
    
;;     To copy to an xterm use left-mouse to select the text in emacs and
;;     then usual paste with middle-mouse to paste to the xterm.

;; [[file:~/usr/emacs/main-init.org::*More%20X11%20configuration][More\ X11\ configuration:1]]

;(setq x-select-enable-clipboard nil)
;(setq x-select-enable-primary t)

;; More\ X11\ configuration:1 ends here

;; Do not beep if I kill text in a read-only buffer

;; [[file:~/usr/emacs/main-init.org::*More%20X11%20configuration][More\ X11\ configuration:1]]

(setq kill-read-only-ok t)

;; More\ X11\ configuration:1 ends here

;; Usually, my home directory is faster for saving files
;;     then anywhere else.

;; [[file:~/usr/emacs/main-init.org::*More%20X11%20configuration][More\ X11\ configuration:1]]

(setq backup-directory-alist '(("." . "~/.backups")))

;; More\ X11\ configuration:1 ends here

;; Make it so selecting the region highlights it and causes many
;;     commands to work only on the region

;; [[file:~/usr/emacs/main-init.org::*More%20X11%20configuration][More\ X11\ configuration:1]]

(setq transient-mark-mode t)

;; More\ X11\ configuration:1 ends here

;; Ignore some other file extensions

;; [[file:~/usr/emacs/main-init.org::*More%20X11%20configuration][More\ X11\ configuration:1]]

(setq completion-ignored-extensions (append completion-ignored-extensions '(".d" ".dd" ".tsk")))

;; More\ X11\ configuration:1 ends here

;; autorevert (disabled)
;;     Cause the buffer to be automatically update when the
;;     file changes.
   
;;     - *DISABLED*.  I found the emacs display would stop refreshing
;;                    after a number of files were loaded.

;; [[file:~/usr/emacs/main-init.org::*autorevert%20(disabled)][autorevert\ \(disabled\):1]]

(use-package autorevert
  :disabled t
  :delight auto-revert-mode
  :config
  (setq auto-revert-check-vc-info t)
  (global-auto-revert-mode))

;; autorevert\ \(disabled\):1 ends here

;; cua-base (disabled)
;;     If you like windows style cut and paste then try this.  ^C & ^X only
;;     work when region is active, ^V and ^Z do paste and undo
     
;;     - *DISABLED* (I hate this)

;; [[file:~/usr/emacs/main-init.org::*cua-base%20(disabled)][cua-base\ \(disabled\):1]]

(use-package cua-base
  :disabled t
  :config
  (cua-mode 1))

;; cua-base\ \(disabled\):1 ends here

;; hl-line (disabled)
;;     `global-hl-line-mode' highlights the current line.  You should make sure
;;     that `hl-line-face' is an appropriate, subtle color.  The sticky
;;     flag keeps it highlighted in all windows
    
;;     - *DISABLED* (trying out beacon-mode which briefly highlights line)

;; [[file:~/usr/emacs/main-init.org::*hl-line%20(disabled)][hl-line\ \(disabled\):1]]

(use-package hl-line
  :disabled t
  :config
  (progn
    (setq hl-line-sticky-flag t)
    (setq global-hl-line-sticky-flag t)
    (global-hl-line-mode 1)))

;; hl-line\ \(disabled\):1 ends here

;; ido (disabled)
    
;;     Use a fancy auto-complete for buffers and files
    
;;     - *DISABLED* using ivy

;; [[file:~/usr/emacs/main-init.org::*ido%20(disabled)][ido\ \(disabled\):1]]

(use-package ido
  :disabled t
  :defer 5
  :config
  (progn
    (setq ido-default-buffer-method 'selected-window)
    (setq ido-default-file-method 'selected-window)
    (setq ido-enable-flex-matching t)
    (setq ido-enable-dot-prefix t)
    (setq ido-enable-tramp-completion t)
    (setq ido-max-directory-size 100000)
    (setq ido-rotate-file-list-default t)
    (setq ido-enter-matching-directory 'first)
    (setq ido-use-virtual-buffers t)
    ;(setq ido-use-virtual-buffers 'auto)
    ;(setq ido-separator "|")
    (setq ido-ignore-files (append ido-ignore-files '("\\`00" "\\'*.tsk")))
    (setq ido-ignore-buffers
          (list "\\` " ".*Completions.*" "\\*Buffer List\\*" "\\*Messages\\*"))
    (setq ido-work-directory-list-ignore-regexps
          (list "/bb/bin" "/bb/data" "/bb/data/tmp" "/bbsrc/apputil"))
    (ido-mode 1)))

;; ido\ \(disabled\):1 ends here

;; ido-vertical (disabled)
;;     Causes ido-mode to display completions vertically
;;     and =Ctl n= and =Ctl p= move down and up in list

;; [[file:~/usr/emacs/main-init.org::*ido-vertical%20(disabled)][ido-vertical\ \(disabled\):1]]

(use-package ido-vertical-mode
  :after ido
  :defer 30
  :ensure t
  :disabled t
  :config
  (ido-vertical-mode 1))

;; ido-vertical\ \(disabled\):1 ends here

;; iswitchb (disabled)
    
;;     `iswitchb-mode' provides a nice completion for switching between
;;     buffers.  The `iswitchb-use-virtual-buffers' and `recentf-mode'
;;     adds recent files to the match
    
;;     - *DISABLED* (use ido instead)

;; [[file:~/usr/emacs/main-init.org::*iswitchb%20(disabled)][iswitchb\ \(disabled\):1]]

(use-package iswitchb
  :disabled t
  :config
  (progn
    (setq iswitchb-default-method 'samewindow
          iswitchb-max-to-show 5
          iswitchb-use-virtual-buffers t)
    (recentf-mode 1)
    (iswitchb-mode 1)))

;; iswitchb\ \(disabled\):1 ends here

;; tool-bar (disabled)
;;     Turn the toolbar off.  I also turn it off in my .Xdefaults with:
    
;;     Emacs.toolBar:            0

;;     which keeps it from displaying on startup

;; [[file:~/usr/emacs/main-init.org::*tool-bar%20(disabled)][tool-bar\ \(disabled\):1]]

(use-package tool-bar
  :config
  (tool-bar-mode -1))

;; tool-bar\ \(disabled\):1 ends here

;; menu-bar (disabled)
;;     Turn the menubar off.
    
;;     - *DISABLED* (Turns out I like the menu-bar!)

;; [[file:~/usr/emacs/main-init.org::*menu-bar%20(disabled)][menu-bar\ \(disabled\):1]]

(use-package menu-bar
  :disabled t
  :config
  (menu-bar-mode -1))

;; menu-bar\ \(disabled\):1 ends here

;; diminish (disabled)
;;     Do not display these minor modes in mode-line

;; [[file:~/usr/emacs/main-init.org::*diminish%20(disabled)][diminish\ \(disabled\):1]]

(use-package diminish
  :disabled t
  :config
  (diminish 'abbrev-mode))

;; diminish\ \(disabled\):1 ends here

;; git-getter-fringe+ (disabled)
;;     Display lines that have changed in the left margin.
;;     This works with linum-mode but not in a tty
    
;;     - *DISABLED* (slow loading)

;; [[file:~/usr/emacs/main-init.org::*git-getter-fringe%2B%20(disabled)][git-getter-fringe+\ \(disabled\):1]]

(use-package git-gutter-fringe+
  :disabled t
  :config (progn
            (setq git-gutter-fr+-side 'right-fringe)
            (global-git-gutter+-mode)))

;; git-getter-fringe+\ \(disabled\):1 ends here

;; magithub (disabled)
;;     Interact with github via magit
    
;;     - *DISABLED* (slow loading)

;; [[file:~/usr/emacs/main-init.org::*magithub%20(disabled)][magithub\ \(disabled\):1]]

(use-package magithub
  :after magit
  :disabled t
  :config
  (magithub-feature-autoinject t))

;; magithub\ \(disabled\):1 ends here

;; nlinum (disabled)
;;     Make it so line numbers show up in left margin
    
;;     - *DISABLED* (refresh problems on Mac OS X)

;; [[file:~/usr/emacs/main-init.org::*nlinum%20(disabled)][nlinum\ \(disabled\):1]]

(use-package nlinum
  :disabled t
  :commands nlinum-mode
  :init (add-hook 'prog-mode-hook 'nlinum-mode))

;; nlinum\ \(disabled\):1 ends here

;; fill-column-indicator (disabled)
    
;;     Make a vertical bar show at fill-column
    
;;     - *DISABLED* (didn't like it anymore)

;; [[file:~/usr/emacs/main-init.org::*fill-column-indicator%20(disabled)][fill-column-indicator\ \(disabled\):1]]

(use-package fill-column-indicator
  :disabled t
  :commands (fci-mode)
  :init (add-hook 'prog-mode-hook 'fci-mode))

;; fill-column-indicator\ \(disabled\):1 ends here

;; num3-mode (disabled)
    
;;     Make long strings of digits alternate groups of 3 with bold.
    
;;     - *DISABLED* (I got tired of this highlight)

;; [[file:~/usr/emacs/main-init.org::*num3-mode%20(disabled)][num3-mode\ \(disabled\):1]]

(use-package num3-mode
  :disabled t
  :ensure t
  :commands num3-mode
  :delight num3-mode
  :init (add-hook 'prog-mode-hook 'num3-mode)
  :config (make-face-bold 'num3-face-even))

;; num3-mode\ \(disabled\):1 ends here

;; color-identifiers-mode (disabled)
    
;;     Make each variable in a different color
    
;;     - *DISABLED* (too many colors)

;; [[file:~/usr/emacs/main-init.org::*color-identifiers-mode%20(disabled)][color-identifiers-mode\ \(disabled\):1]]

(use-package color-identifiers-mode
  :disabled t
  :delight color-identifiers-mode
  :init
  (add-hook 'prog-mode-hook
            'color-identifiers-mode)
  :delight color-identifiers-mode)

;; color-identifiers-mode\ \(disabled\):1 ends here

;; rainbow-identifiers (disabled)
    
;;     Make each variable a different color
    
;;     - *DISABLED* (using color-identifies-mode instead)

;; [[file:~/usr/emacs/main-init.org::*rainbow-identifiers%20(disabled)][rainbow-identifiers\ \(disabled\):1]]

(use-package rainbow-identifiers
  :disabled t
  :config
  (progn
    (add-hook 'prog-mode-hook
              'rainbow-identifiers-mode)))

;; rainbow-identifiers\ \(disabled\):1 ends here

;; smart-mode-line (disabled)
    
;;     Smart mode line displays a more graphical modeline.
    
;;     DISABLED (Use powerline mode instead)

;; [[file:~/usr/emacs/main-init.org::*smart-mode-line%20(disabled)][smart-mode-line\ \(disabled\):1]]

(use-package smart-mode-line
  :disabled t
  :config
  (progn
    (setq sml/theme 'dark)
    (sml/setup)))

;; smart-mode-line\ \(disabled\):1 ends here

;; sublime-themes (disabled)
;;     I like the wilson theme from the sublime-themes
;;     package.

;; [[file:~/usr/emacs/main-init.org::*sublime-themes%20(disabled)][sublime-themes\ \(disabled\):1]]

(use-package sublime-themes
  :disabled t
  :ensure t
  :config
  (load-theme 'wilson t nil))

;; sublime-themes\ \(disabled\):1 ends here

;; dracula-theme (disabled)

;; [[file:~/usr/emacs/main-init.org::*dracula-theme%20(disabled)][dracula-theme\ \(disabled\):1]]

(use-package dracula-theme
  :disabled t
  :ensure t
  :config
  (load-theme 'dracula t nil))

;; dracula-theme\ \(disabled\):1 ends here
