
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
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(current-language-environment "English")
 '(custom-safe-themes
   '("a046f87a68ff2dedd4b994814f14b55e4f24da317f50adea3563c2921cdc4ac6"
     "dde643b0efb339c0de5645a2bc2e8b4176976d5298065b8e6ca45bc4ddf188b7"
     "e3b2bad7b781a968692759ad12cb6552bc39d7057762eefaf168dbe604ce3a4b"
     "bf390ecb203806cbe351b966a88fc3036f3ff68cd2547db6ee3676e87327b311"
     "0c46a9128995ad772ecbfc5a5193cb253a9a0200bcddf4d6895370e0a92545b4"
     "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045"
     "ea1710620648c2d1d9bb52440f150f3b9a9d2dc27cbd19e63efc5b35fd2dee2a"
     "a11043406c7c4233bfd66498e83600f4109c83420714a2bd0cd131f81cbbacea"
     "5f4dfda04fbf7fd55228266c8aab73953d3087cea7fd06dd7f8ff1e4a497c739"
     "2a7beed4f24b15f77160118320123d699282cbf196e0089f113245d4b729ba5d"
     "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d"
     "e30f381d0e460e5b643118bcd10995e1ba3161a3d45411ef8dfe34879c9ae333"
     "0cd56f8cd78d12fc6ead32915e1c4963ba2039890700458c13e12038ec40f6f5"
     "d61f6c49e5db58533d4543e33203fd1c41a316eddb0b18a44e0ce428da86ef98"
     "228c0559991fb3af427a6fa4f3a3ad51f905e20f481c697c6ca978c5683ebf43"
     "c616e584f7268aa3b63d08045a912b50863a34e7ea83e35fcab8537b75741956"
     "73a13a70fd111a6cd47f3d4be2260b1e4b717dbf635a9caee6442c949fad41cd"
     "c79c2eadd3721e92e42d2fefc756eef8c7d248f9edefd57c4887fbf68f0a17af"
     "da538070dddb68d64ef6743271a26efd47fbc17b52cc6526d932b9793f92b718"
     "9b1c580339183a8661a84f5864a6c363260c80136bd20ac9f00d7e1d662e936a"
     "a94f1a015878c5f00afab321e4fef124b2fc3b823c8ddd89d360d710fc2bddfc"
     "1b27e3b3fce73b72725f3f7f040fd03081b576b1ce8bbdfcb0212920aec190ad"
     "d21135150e22e58f8c656ec04530872831baebf5a1c3688030d119c114233c24"
     "8d5f22f7dfd3b2e4fc2f2da46ee71065a9474d0ac726b98f647bc3c7e39f2819"
     "66aea5b7326cf4117d63c6694822deeca10a03b98135aaaddb40af99430ea237"
     "946e871c780b159c4bb9f580537e5d2f7dba1411143194447604ecbaf01bd90c"
     "251348dcb797a6ea63bbfe3be4951728e085ac08eee83def071e4d2e3211acc3"
     "01e067188b0b53325fc0a1c6e06643d7e52bc16b6653de2926a480861ad5aa78"
     "de0b7245463d92cba3362ec9fe0142f54d2bf929f971a8cdf33c0bf995250bcf"
     "3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4"
     "003a9aa9e4acb50001a006cfde61a6c3012d373c4763b48ceb9d523ceba66829"
     "721bb3cb432bb6be7c58be27d583814e9c56806c06b4077797074b009f322509"
     "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da"
     "9bd5ee2b24759fbc97f86c2783d1bf8f883eb1c0dd2cf7bda2b539cd28abf6a9"
     "5c85b6f7f76fe0e0979da4c650dee525ae5185b134cb0fdfb12eeb580ea4dafb"
     "8ba0a9fc75f2e3b4c254183e814b8b7b8bcb1ad6ca049fde50e338e1c61a12a0"
     "a12ec87ff9e72a9561314c7ae2c82a373e1b7c80d0fe15579e282080c8d5aef2"
     "9f08dacc5b23d5eaec9cccb6b3d342bd4fdb05faf144bdcd9c4b5859ac173538"
     "26d613485834c8498d96a664d970e19b7d5286c39a78452f492ae5572cf1bd21"
     "aad7fd3672aad03901bf91e338cd530b87efc2162697a6bef79d7f8281fd97e3"
     "57d7e8b7b7e0a22dc07357f0c30d18b33ffcbb7bcd9013ab2c9f70748cfa4838"
     "ed92c27d2d086496b232617213a4e4a28110bdc0730a9457edf74f81b782c5cf"
     "8530b2f7b281ea6f263be265dd8c75b502ecd7a30b9a0f28fa9398739e833a35"
     "1a094b79734450a146b0c43afb6c669045d7a8a5c28bc0210aba28d36f85d86f"
     "5c83b15581cb7274085ba9e486933062652091b389f4080e94e4e9661eaab1aa"
     "da8e6e5b286cbcec4a1a99f273a466de34763eefd0e84a41c71543b16cd2efac"
     "392f19e7788de27faf128a6f56325123c47205f477da227baf6a6a918f73b5dc"
     "a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd"
     "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e"
     "d057f0430ba54f813a5d60c1d18f28cf97d271fd35a36be478e20924ea9451bd"
     "c74fff70a4cc37e2348dd083ae572c8b4baab4f6fb86adae5e0f2139a63e9a96"
     "eecacf3fb8efc90e6f7478f6143fd168342bbfa261654a754c7d47761cec07c8"
     "e297f54d0dc0575a9271bb0b64dad2c05cff50b510a518f5144925f627bb5832"
     "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11"
     "b4fd44f653c69fb95d3f34f071b223ae705bb691fb9abaf2ffca3351e92aa374"
     "8f457891033a0d85a03277436f0a6a777eeffc7e0c084ec462768019071614d0"
     "570b92af8240383260eeb05bac7e95a03bc65b1cc34ceceff9d32e000e9292c9"
     "78a51bca37971dd3917d5fa1b6971e5e0d0665222b194e274dfc79d60560ba34"
     "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb"
     "bb749a38c5cb7d13b60fa7fc40db7eced3d00aa93654d150b9627cabd2d9b361"
     "fc1137ae841a32f8be689e0cfa07c872df252d48426a47f70dba65f5b0f88ac4"
     "4c8372c68b3eab14516b6ab8233de2f9e0ecac01aaa859e547f902d27310c0c3"
     "6cf0e8d082a890e94e4423fc9e222beefdbacee6210602524b7c84d207a5dfb5"
     "11e5e95bd3964c7eda94d141e85ad08776fbdac15c99094f14a0531f31a156da"
     "595099e6f4a036d71de7e1512656e9375dd72cf60ff69a5f6d14f0171f1de9c1"
     "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b"
     "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4"
     "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6"
     "7c33d91f9896614a9c28e96def4cbd818f0aa7f151d1fb5d205862e86f2a3939"
     "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12"
     "c1af7190a6855a376f7a7563445687064af6d8bdca423136cb013c93fbfd1b00"
     "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223"
     "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
     "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e"
     "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4"
     "ce79400f46bd76bebeba655465f9eadf60c477bd671cbcd091fe871d58002a88"
     "e890fd7b5137356ef5b88be1350acf94af90d9d6dd5c234978cd59a6b873ea94"
     "b19b642b0d5be8ec4bc96698260575d3eb81a22064911a8036213facf3a9a6fa"
     "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879"
     "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
     "f5e56ac232ff858afb08294fc3a519652ce8a165272e3c65165c42d6fe0262a0"
     default))
 '(default-input-method "rfc1345")
 '(display-time-mode t)
 '(fci-rule-color "#383838")
 '(initial-frame-alist '((menu-bar-lines . 1)))
 '(line-number-mode nil)
 '(package-vc-selected-packages
   '((copilot :url "https://github.com/copilot-emacs/copilot.el" :branch
              "main")))
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(viper-case-fold-search t)
 '(viper-no-multiple-ESC nil)
 '(viper-vi-style-in-minibuffer nil)
 '(viper-want-ctl-h-help t))

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "nil" :slant normal :weight regular :height 180 :width normal)))))
