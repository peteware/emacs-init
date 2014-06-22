(setq my-base-path "~/usr/emacs")
(add-to-list 'load-path my-base-path)
(add-to-list 'load-path (concat my-base-path "/org-8.2.4/lisp"))
(add-to-list 'load-path (concat my-base-path "/org-8.2.4/contrib/lisp"))
(if (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path (concat my-base-path "/themes")))

(require 'main-init)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#d02090" "#ccaa8f" "#f6f3e8"])
 '(auto-compression-mode t nil (jka-compr))
 '(c-doc-comment-style (quote ((c++-mode . javadoc))))
 '(case-fold-search t)
 '(current-language-environment "English")
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes (quote ("b19b642b0d5be8ec4bc96698260575d3eb81a22064911a8036213facf3a9a6fa" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "f5e56ac232ff858afb08294fc3a519652ce8a165272e3c65165c42d6fe0262a0" default)))
 '(default-input-method "rfc1345")
 '(delete-selection-mode nil nil (delsel))
 '(grep-files-aliases (quote (("all" . "* .*") ("el" . "*.el") ("ch" . "*.[ch]") ("c" . "*.c") ("cc" . "*.cc *.cxx *.cpp *.C *.CC *.c++") ("cchh" . "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++") ("hh" . "*.hxx *.hpp *.[Hh] *.HH *.h++") ("h" . "*.h") ("l" . "[Cc]hange[Ll]og*") ("m" . "[Mm]akefile*") ("tex" . "*.tex") ("texi" . "*.texi") ("asm" . "*.[sS]") ("code" . "*.c *.h *.cpp *.f"))))
 '(indicate-empty-lines t)
 '(initial-frame-alist (quote ((menu-bar-lines . 1))))
 '(lazy-highlight-max-at-a-time 5)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(org-agenda-files (quote ("~/notes/xndf.org" "~/notes/todo.org" "~/notes/drqs.org" "~/notes/code.org")))
 '(org-capture-templates (quote (("t" "Todo" entry (file+headline "~/notes/todo.org" "Bloomberg Current") "** TODO %?%i %t
") ("c" "Code" entry (file+headline "~/notes/code.org" "Recent Code Reviews") "** TODO %?%t
*** %i
") ("n" "Notes" entry (file+datetree "~/notes/notes.org") "** %?
") ("d" "Drqs" entry (file+headline "~/notes/drqs.org" "Active DRQS") "** TODO %?%i %t
") ("f" "FXFA" entry (file+headline "~/notes/fxfa.org" "ToDo") "** TODO %?%i %t
") ("x" "XNDF" entry (file+headline "~/notes/xndf.org" "ToDo") "** TODO %?%i %t
"))))
 '(scroll-bar-mode nil)
 '(size-indication-mode t)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(viper-case-fold-search t)
 '(viper-no-multiple-ESC nil)
 '(viper-vi-style-in-minibuffer nil)
 '(viper-want-ctl-h-help t))

;; '(ediff-current-diff-A ((t (:background "#656555"))))
;; '(ediff-current-diff-B ((t (:background "#656555"))))
;; '(ediff-even-diff-A ((t (:background "6f6f6f"))))
;; '(ediff-even-diff-B ((t (:background "6f6f6f"))))
;; '(ediff-fine-diff-A ((t (:inverse-video t))))
;; '(ediff-fine-diff-Ancestor ((t (:background "dim gray" :foreground "Black"))))
;; '(ediff-fine-diff-B ((t (:inverse-video t))))
;; '(ediff-odd-diff-A ((t (:background "6f6f6f"))))
;; '(ediff-odd-diff-B ((t (:background "6f6f6f"))))
;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mouse ((t (:foreground "green"))))
 '(org-column ((t (:inverse-video t :height 1.0 :family "courier")))))

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
