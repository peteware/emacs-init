;; Standard style for C++
;;
;; I have this code in my ~/.emacs (or ~/.xemacs/init.el):
;;
;; (require 'bb-style)
;; (setq c-default-style "bb")
;;
;; ;; This causes TAB not re-indent line.  See docs for more choices
;; (setq c-tab-always-indent nil)
;;
;; ;; Associate .h files with c++-mode instead of c-mode
;; (add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

(provide 'bb-style)
(defcustom bb-show-whitespace nil
  "If this variable is t, then show bad whitespace in c-mode.
See `bb-whitespace-style for definition of bad whitespace."
  :group 'bb-style)

(defcustom bb-whitespace-style '(trailing tabs empty indentation::space)
  "This is used to highlight bad white space if bb-show-whitespace is t.
See `whitespace-style' for full documenation but this is set so that:

trailing   any whitespace after text is highlighted
tabs       any tabs are highlighted
empty      empty lines at start or end of file are highlighted
indentation:space Any tabs at beginning of line are highlighted."
  :group 'bb-style)

(defcustom bb-disable-word-wrap t
  "If this variable is t, then disable word wrap mode in c-mode."
  :group 'bb-style)

(defcustom bb-enable-auto-hungry t
  "If this variable is t, then enable auto-hungry-sate in c-mode.
See `c-toggle-auto-newline' and `c-toggle-hungry-state' for
details but basically characters like `;' automatically insert
newline and re-indent and deleting backward deletes all
whitespace and deleting forward deletes all forward whitespace."
  :group 'bb-style)

(defun bb-c-mode()
  "Configure a few extra options for bb-style.  You can customize
the group bb-style to change options like displaying bad white,
how long lines word wrap, and if auto-hungry mode is enabled.
You need to re-visit any files at these changes take place in the
c++-mode-hook and c-mode hook."
  (interactive)
  (require 'whitespace)
  (setq comment-column 48)
  (setq fill-column 80)
  (setq indent-tabs-mode nil)
  (c-set-style "bb")
  (if (and (fboundp 'whitespace-mode) bb-show-whitespace)
      (progn
	(make-variable-buffer-local 'whitespace-style)
	(setq whitespace-style bb-whitespace-style)
	(whitespace-mode 1)))
  (if (and (boundp 'word-wrap) bb-disable-word-wrap)
      (setq word-wrap nil))
  (if bb-enable-auto-hungry
      (c-toggle-auto-hungry-state 1)))

(setq bb-style-alist
      '((c-basic-offset                 . 4)
        (c-comment-only-line-offset . (0 . -1000))
        ;; Put newline before&after braces
        (c-hanging-braces-alist     . ((substatement-open . (before after))
                                       (statement  . (before after))
                                       (namespace-open . ())
                                       (brace-list-open . (before after))))
        ;;
        ;;(c-hanging-colons-alist     . ((member-init-intro before)
        ;;			       (inher-intro)
        ;;			       (case-label after)
        ;;			       (label after)
        ;;			       (access-label after)))
        ;; This only works with auto-newline but helps a lot
        (c-cleanup-list             . (scope-operator
                                       list-close-comma
                                       compact-empty-funcall
                                       defun-close-semi))
        (c-doc-comment-style       . javadoc)
        (c-offsets-alist
         . ((string                . +)
            (inline-open           . 0)
            (extern-lang-open      . 0)
            (extern-lang-close     . 0)
            (func-decl-cont        . 0)
            (knr-argdecl-intro     . 0)
            (topmost-intro         . 0)
            (topmost-intro-cont    . 0)
            ;;(inher-cont            . +)
            (statement-case-open   . 0)
            (substatement-open     . 0)
            (label                 . 0)
            ;;(comment-intro         . 0)
            (arglist-cont          . c-lineup-argcont)
            (arglist-cont-nonempty . c-lineup-arglist)
            (arglist-close         . c-lineup-arglist)
            (stream-op             . +)
            (inclass               . +)
            (inextern-lang         . 0)
            (cpp-macro             . -100)
            (innamespace           . 0)
            (namespace-open        . nil)
            (namespace-close       . nil)
            ))
        ))
(c-add-style "bb" bb-style-alist)
