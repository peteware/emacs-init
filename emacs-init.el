(setq my-base-path "~/usr/emacs")
(add-to-list 'load-path my-base-path)
;(add-to-list 'load-path (concat my-base-path "/org-8.2.4/lisp"))
;(add-to-list 'load-path (concat my-base-path "/org-8.2.4/contrib/lisp"))
(if (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path (concat my-base-path "/themes")))

(require 'main-init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#d02090" "#ccaa8f" "#f6f3e8"])
 '(current-language-environment "English")
 '(custom-safe-themes
   (quote
    ("96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "ce79400f46bd76bebeba655465f9eadf60c477bd671cbcd091fe871d58002a88" "e890fd7b5137356ef5b88be1350acf94af90d9d6dd5c234978cd59a6b873ea94" "b19b642b0d5be8ec4bc96698260575d3eb81a22064911a8036213facf3a9a6fa" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "f5e56ac232ff858afb08294fc3a519652ce8a165272e3c65165c42d6fe0262a0" default)))
 '(default-input-method "rfc1345")
 '(initial-frame-alist (quote ((menu-bar-lines . 1))))
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(viper-case-fold-search t)
 '(viper-no-multiple-ESC nil)
 '(viper-vi-style-in-minibuffer nil)
 '(viper-want-ctl-h-help t))

(custom-set-faces
 )

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
