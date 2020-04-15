(provide 'org-prefs)
;(require 'org)

;(if (not (fboundp 'assoc-default))
;    (defun assoc-default (key alist &optional test default)
;      (cdr (assoc-if test alist :key key))))

(defun pw/c++-orgstruct ()
  (interactive)
  (make-local-variable 'orgstruct-heading-prefix-regexp)
  (setq orgstruct-heading-prefix-regexp "^// ")
  (turn-on-orgstruct))

(add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only
;(add-hook 'c++-mode-hook 'pw/c++-orgstruct)
;(add-hook 'c-mode-hook 'pw/c++-orgstruct)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq calendar-latitude 40.76)
(setq calendar-longitude -73.96)
(setq org-log-done 'time)
(setq org-cycle-include-plain-lists t)
(setq org-agenda-text-search-extra-files '(agenda-archives "~/notes/cmds.org" "~/notes/notes.org"))
(setq org-agenda-start-with-follow-mode nil)
(setq org-agenda-window-setup 'other-frame)
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-agenda-dim-blocked-tasks t)
(setq org-directory "~/notes/")
(setq org-default-notes-file (concat org-directory "/notes.org"))
;; (setq org-agenda-time-grid
;;       '((daily today)
;;         (0 16)
;;         "----------------"
;;           (org-heading t))
;;        (800 1000 1200 1400 1600 1800 2000)))
(setq org-agenda-files '("~/notes/todo.org" "~/notes/drqs.org" "~/notes/events.org"))
(setq org-time-clocksum-format '(:days "%dd " :hours "%d" :minutes ":%02d"))
(setq org-global-properties
        '((Effort_ALL . "0d 1d 2d 3d 4d 5d 7d")))
(setq org-columns-default-format "%60ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM")
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/notes/todo.org" "Bloomberg Current")
         "** TODO %?%i \n   :PROPERTIES:\n   :Added:    %U\n   :END:\n")
        ("c" "Code" entry (file+headline "~/notes/code.org" "Recent Code Reviews")
         "** TODO %?%i \n   :PROPERTIES:\n   :Added:    %U\n   :END:\n")
        ("n" "Notes" entry (file+datetree "~/notes/notes.org")
         "** %?\n")
        ("d" "Drqs" entry (file+headline "~/notes/drqs.org" "Active DRQS")
         "** TODO %?%i \n   :PROPERTIES:\n   :Added:    %U\n   :ORDERED:   t\n   :END:\n")
        ("b" "BREG" entry (file+headline "~/notes/drqs.org" "Active DRQS")
"** TODO DRQS %^{DRQS}: Remove BREG %^{BREG}%i
   :PROPERTIES:
   :Added:    %U
   :ORDERED:   t
   :END:
*** TODO DRQS %\\1: BREG %\\2: make values consistent
*** TODO DRQS %\\1: BREG %\\2: remove from code     %^g
*** TODO DRQS %\\1: BREG %\\2: code review
*** TODO DRQS %\\1: BREG %\\2: enable obsolete trace\n")
        ("f" "FWDSVC" entry (file+headline "~/notes/fwdsvc.org" "ToDo")
         "** TODO %?%i \n   :PROPERTIES:\n   :Added:    %U\n   :END:\n")
        ("F" "FRD" entry (file+headline "~/notes/frd.org" "ToDo")
         "** TODO %?%i \n   :PROPERTIES:\n   :Added:    %U\n   :END:\n")
        ("X" "FXFA" entry (file+headline "~/notes/fxfa.org" "ToDo")
         "** TODO %?%i \n   :PROPERTIES:\n   :Added:    %U\n   :END:\n")
        ("x" "XNDF" entry (file+headline "~/notes/xndf.org" "ToDo")
         "** TODO %?%i \n   :PROPERTIES:\n   :Added:    %U\n   :END:\n")
        ))

(setq org-todo-keywords
      '((sequence "TODO" "NEXT" "|" "DONE")))
(setq org-agenda-custom-commands
      '(("D" "Daily ActionList"
         ((agenda ""
                  ((org-agenda-span 1)
                   (org-agenda-show-log t)
                   (org-agenda-sorting-strategy '((agenda time-up priority-down tag-up)))
                   (org-deadline-warning-days 1))
                  (org-agenda-log-mode-items '(closed clock state))
                  )
          (todo "NEXT")))
        ("W" "Weekly Plan"
         ((agenda ""
                  ((org-agenda-span 'week)
                   (org-agenda-show-log t)
                   (org-agenda-sorting-strategy '((agenda time-up priority-down tag-up)))
                   (org-deadline-warning-days 1))
                  (org-agenda-log-mode-items '(closed clock state))
                  )
          (todo "NEXT")
          (tags-todo "-drqs+TODO=\"TODO\"")
          (tags-todo "drqs+TODO=\"TODO\"")))
        ))
