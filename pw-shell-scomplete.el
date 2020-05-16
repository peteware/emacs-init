(provide 'pw-shell-scomplete)

(defun pw/shell-scomplete-to-shell-buffer ()
  "Scomplete to a shell buffer, or create one."
  (interactive)
  (pw/shell-scomplete--to-shell
   (completing-read "Shell buffer: " (mapcar 'buffer-name (pw/shell-scomplete--buffers))
			nil nil nil 'shell-hist nil)))

(defun pw/shell-scomplete--shell-p (buffer)
  "Return if buffer is in shell mode."
  (if (not (bufferp buffer))
      nil
      (or (eq 'shell-mode (buffer-local-value 'major-mode buffer))
          (eq 'term-mode (buffer-local-value 'major-mode buffer)))))

(defun pw/shell-scomplete--buffers ()
  (seq-filter 'pw/shell-scomplete--shell-p (buffer-list)))

(defun pw/shell-scomplete--to-shell (name)
  "Display shell buffer with NAME and select its window.
Reuse any existing window already displaying the named buffer.
If there is no such buffer, start a new `shell' with NAME."
  (if (get-buffer name)
      (let ((start-buffer (buffer-name)))
        (pop-to-buffer name '((display-buffer-reuse-window
                               display-buffer-same-window)
                              (inhibit-same-window . nil)
                              (reusable-frames . visible)))
        (if (equal name start-buffer)
            (goto-char (point-max))))
    (shell name)))
