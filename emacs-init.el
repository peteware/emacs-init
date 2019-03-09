
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(setq my-base-path "~/usr/emacs")
(add-to-list 'load-path my-base-path)
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
    ("eecacf3fb8efc90e6f7478f6143fd168342bbfa261654a754c7d47761cec07c8" "e297f54d0dc0575a9271bb0b64dad2c05cff50b510a518f5144925f627bb5832" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "b4fd44f653c69fb95d3f34f071b223ae705bb691fb9abaf2ffca3351e92aa374" "8f457891033a0d85a03277436f0a6a777eeffc7e0c084ec462768019071614d0" "570b92af8240383260eeb05bac7e95a03bc65b1cc34ceceff9d32e000e9292c9" "78a51bca37971dd3917d5fa1b6971e5e0d0665222b194e274dfc79d60560ba34" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "bb749a38c5cb7d13b60fa7fc40db7eced3d00aa93654d150b9627cabd2d9b361" "fc1137ae841a32f8be689e0cfa07c872df252d48426a47f70dba65f5b0f88ac4" "4c8372c68b3eab14516b6ab8233de2f9e0ecac01aaa859e547f902d27310c0c3" "6cf0e8d082a890e94e4423fc9e222beefdbacee6210602524b7c84d207a5dfb5" "11e5e95bd3964c7eda94d141e85ad08776fbdac15c99094f14a0531f31a156da" "595099e6f4a036d71de7e1512656e9375dd72cf60ff69a5f6d14f0171f1de9c1" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "7c33d91f9896614a9c28e96def4cbd818f0aa7f151d1fb5d205862e86f2a3939" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "c1af7190a6855a376f7a7563445687064af6d8bdca423136cb013c93fbfd1b00" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "ce79400f46bd76bebeba655465f9eadf60c477bd671cbcd091fe871d58002a88" "e890fd7b5137356ef5b88be1350acf94af90d9d6dd5c234978cd59a6b873ea94" "b19b642b0d5be8ec4bc96698260575d3eb81a22064911a8036213facf3a9a6fa" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "f5e56ac232ff858afb08294fc3a519652ce8a165272e3c65165c42d6fe0262a0" default)))
 '(default-input-method "rfc1345")
 '(initial-frame-alist (quote ((menu-bar-lines . 1))))
 '(package-selected-packages
   (quote
    (magit-popup ivy-rich discover ivy-hydra org "org" "org" "org" ox-gfm multiple-cursors overcast-theme delight counsel ivy magit-gh-pulls dracula-theme rtags magithub eopengrok mode-icons origami org-beautify-theme zoom-frm zenburn-theme use-package sublime-themes smart-mode-line scratch-ext powerline magit log4j-mode fill-column-indicator ctags color-theme-modern beacon avy anyins ag)))
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

;(custom-set-faces
; ;; custom-set-faces was added by Custom.
; ;; If you edit it by hand, you could mess it up, so be careful.
; ;; Your init file should contain only one such instance.
; ;; If there is more than one, they won't work right.
; '(sml/modes ((t (:inherit sml/global :background "gray12" :foreground "light gray" :weight normal)))))

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
(put 'erase-buffer 'disabled nil)
