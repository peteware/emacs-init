(provide 'org-prefs)
(defun pw/c++-orgstruct ()
  (interactive)
  (make-local-variable 'orgstruct-heading-prefix-regexp)
  (setq orgstruct-heading-prefix-regexp "^// ")
  (turn-on-orgstruct))

(add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only
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
(setq org-agenda-files '("~/notes/todo.org" "~/notes/drqs.org" "~/notes/events.org" "~/notes/berg.org" "~/notes/grpscdb.org" "~/notes/mp-web-ui.org"))
(setq org-time-clocksum-format '(:days "%dd " :hours "%d" :minutes ":%02d"))
(setq org-global-properties
        '((Effort_ALL . "0d 1d 2d 3d 4d 5d 7d")))
(setq org-columns-default-format "%60ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM")
;;
;; Use =org-capture= which is bound to "C-c r"
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/notes/todo.org" "Bloomberg Current")
         "** TODO %?%i \n   :PROPERTIES:\n   :Added:    %U\n   :END:\n")
        ("c" "Code" entry (file+headline "~/notes/code.org" "Recent Code Reviews")
         "** TODO %?%i \n   :PROPERTIES:\n   :Added:    %U\n   :END:\n")
        ("n" "Notes" entry (file+datetree "~/notes/notes.org")
         "** %?\n" :unnarrowed t)
        ("d" "Drqs" entry (file+headline "~/notes/drqs.org" "Active DRQS")
         "** TODO %?%i \n   :PROPERTIES:\n   :Added:    %U\n   :ORDERED:   t\n   :END:\n")
        ("g" "GRPSCDB" entry (file+headline "~/notes/grpscdb.org" "ToDo")
         "** TODO %?%i \n   :PROPERTIES:\n   :Added:    %U\n   :END:\n")
        ("w" "WEB-UI" entry (file+headline "~/notes/mp-web-ui.org" "ToDo")
         "** TODO %?%i \n   :PROPERTIES:\n   :Added:    %U\n   :END:\n")
        ("b" "BERG" entry (file+headline "~/notes/berg.org" "ToDo")
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
