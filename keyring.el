;;; keyring.el --- a password manager

;;; Commentary:
;; This a password manager that aims to be modular, different
;; pasword backend can be used.
;; the palm backend can be found at:
;; https://kanis.fr/svn/trunk/bin/keyring.pl

;;; THANKS:
;; Stefan Reichoer for pwsafe.el which I copied from

;;; BUGS:

;;; INSTALLATION:

;; Put the following in your init file:
;; (require 'keyring)
;; (setq keyring-backend 'pwsafe)
;; Available backends are pwsafe and palm
;;
;; M-x keyring to display the list
;; Press enter to display the entry in the list

;;; TODO
;; add
;; edit
;; rename
;; delete
;; init empty database
;; display timout

;;; Code:

(defvar keyring-backend 'pwsafe
  "Back end used for keyring: 'palm or 'pwsafe")

(defvar keyring-file
  '((palm . "~/.jpilot/Keys-Gtkr.pdb")
    (pwsafe . "~/.pwsafe.dat"))
  "Alist of file assocciated with backend")

(defvar keyring-program
  '((palm . "keyring.pl")
    (pwsafe . "pwsafe"))
  "Alist of program associated with backend.")

(defvar keyring-keep-passwd nil
  "Whether to keep the passwd cached
When nil, don't cache the passwd
When a number: cache the passwd for the given number of seconds
Otherwise cache the passwd for ever")

(defvar keyring-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map (kbd "RET") 'keyring-display-mode)
    (define-key map "q" 'keyring-list-quit)
    (define-key map "s" 'isearch-forward)
    map))

(defvar keyring-display-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'keyring-display-quit)
    map))

(defconst keyring-list-buffer-name "*Keyring list*")
(defconst keyring-display-buffer-name "*Keyring display*")

(defvar keyring-keep-passwd-timer nil)
(defvar keyring-cached-passwd nil)

(defvar keyring-process-output nil)
(defvar keyring-process-running nil
  "t when a subprocess is running")

(defun keyring-filename (backend)
  (expand-file-name (cdr (assoc backend keyring-file))))

(defun keyring-read-passwd (prompt)
  (let ((passwd (or keyring-cached-passwd (read-passwd prompt))))
    (when keyring-keep-passwd
      (setq keyring-cached-passwd passwd)
      (when (numberp keyring-keep-passwd)
        (when (timerp keyring-keep-passwd-timer)
          (cancel-timer keyring-keep-passwd-timer))
        (setq keyring-keep-passwd-timer
              (run-with-timer keyring-keep-passwd nil
                              'keyring-clear-passwd-cache))))
    passwd))

(defun keyring-clear-passwd-cache ()
  "Clear the cached passwd in `keyring-cached-passwd'."
  (interactive)
  (when (interactive-p)
    (message (if keyring-cached-passwd
                 "Cleared the cached password"
               "Password not cached")))
  (setq keyring-cached-passwd nil))

(defun keyring-run (cmd &rest args)
  "wait for process to finish"
  (let ((process
         (apply 'start-process "keyring"
                (current-buffer)
                (cdr (assoc keyring-backend keyring-program))
                args)))
    (setq keyring-process-running t)
    (set-process-filter process 'keyring-process-filter)
    (set-process-sentinel process 'keyring-process-sentinel))
  (while keyring-process-running
    (sit-for 0.1)))

(defun keyring-process-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((start 0)
          (end 0)
          (output (concat keyring-process-output string))
          (line nil))
      (while (string-match "\n" output start)
        (setq end (match-end 0))
        (setq line (substring output start end))
        (keyring-process-filter-line proc line)
        (setq start end))
      (when (keyring-process-filter-prompt
             proc
             (if line
                 (substring output end (length output)) string))
        (setq keyring-process-output "")))))

(defun keyring-process-filter-line(proc line)
  "Filter called for LINE from PROC that ends with a new line"
  (funcall (intern
            (concat "keyring-"
                    (symbol-name keyring-backend)
                    "-filter-line")) proc line))

(defun keyring-process-filter-prompt(proc prompt)
  "Filter called for PROMPT from PROC.
A prompt doess not ends with a new line
Returns t when the output buffer needs to be cleared"
  (funcall (intern
            (concat "keyring-"
                    (symbol-name keyring-backend)
                    "-filter-prompt")) proc prompt))

(defun keyring-process-sentinel (process event)
  (setq keyring-process-running nil)
  (cond ((string-match "finished\\|killed\\|terminated" event))
        ((string-match "exited abnormally" event)
         (message "keyring process failed: %s" event))
        (t
         (message "keyring process had unknown event: %s" event))))

(defun keyring-send-string (proc string)
  (process-send-string proc (concat string "\n")))

;;;###autoload
(defun keyring-list-mode ()
  (interactive)
  (with-current-buffer (get-buffer-create keyring-list-buffer-name)
    (kill-all-local-variables)
    (setq major-mode 'keyring-list-mode
          mode-name "Keyring"
          buffer-undo-list t
          buffer-read-only nil)
    (use-local-map keyring-list-mode-map)
    (erase-buffer)
    (funcall (intern (concat "keyring-" (symbol-name keyring-backend)))
             'list)
    (goto-char (point-min))
    (setq buffer-read-only t))
  (switch-to-buffer keyring-list-buffer-name))

;;;###autoload
(defalias 'keyring 'keyring-list-mode
  "Alias to keyring-list-mode")

(defun keyring-display-mode ()
  (interactive)
  (let ((name (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (with-current-buffer (get-buffer-create keyring-display-buffer-name)
      (kill-all-local-variables)
      (setq major-mode 'keyring-display-mode
            mode-name "Keyring display"
            buffer-undo-list t
            buffer-read-only nil)
      (use-local-map keyring-display-mode-map)
      (erase-buffer)
      (funcall (intern (concat "keyring-" (symbol-name keyring-backend)))
               'display name)
      (goto-char (point-min))
      (setq buffer-read-only t))
    (switch-to-buffer keyring-display-buffer-name)))


(defun keyring-display-quit ()
  (interactive)
  (kill-buffer keyring-display-buffer-name)
  (switch-to-buffer keyring-list-buffer-name))

(defun keyring-list-quit ()
  (interactive)
  (keyring-clear-passwd-cache)
  (when (get-buffer keyring-display-buffer-name)
    (kill-buffer keyring-display-buffer-name))
  (kill-buffer keyring-list-buffer-name))

(defun keyring-display-timeout ()
  (message "Keyring display timeout")
  (kill-buffer keyring-display-buffer-name))

;;; palm backend
(defun keyring-palm (cmd &rest args)
  (apply (intern (concat "keyring-palm-" (symbol-name cmd))) args))

(defun keyring-palm-list ()
  (keyring-run 'list "--file" (keyring-filename 'palm) "--list")
  (delete-backward-char 1)
  (sort-lines nil (point-min) (point-max))
  (goto-char (point-min)))

(defun keyring-palm-display (name)
  (keyring-run 'display "--file" (keyring-filename 'palm)
               "--name" name)
  (goto-char (point-min)))

(defun keyring-palm-filter-prompt(proc prompt)
  "Process PROMPT of process, return t when prompt needs to be erased"
  (cond ((string-match "Enter passphrase for .*: " prompt)
         (keyring-send-string proc (keyring-read-passwd line)) t)
        (t nil)))

(defun keyring-palm-filter-line (proc line)
  "Process LINE of process, LINE alway ends with a new line"
  (cond ((< (length line) 2))
        (t (insert line))))

;;; pwsafe backend
(defun keyring-pwsafe (cmd &rest args)
  (apply (intern (concat "keyring-pwsafe-" (symbol-name cmd))) args))

(defun keyring-pwsafe-list ()
  (keyring-run 'list "-f" (keyring-filename 'pwsafe))
  (delete-backward-char 1)
  (sort-lines nil (point-min) (point-max))
  (goto-char (point-min)))

(defun keyring-pwsafe-display (name)
  (insert "Name: " name "\n")
  (keyring-run 'display "-pulE" name "-f" (keyring-filename 'pwsafe))
  (goto-char (point-min)))

(defun keyring-pwsafe-filter-prompt(proc prompt)
  "Process PROMPT of process, return t when prompt needs to be erased"
  (cond ((string-match "Enter passphrase for .*: " prompt)
         (keyring-send-string proc (keyring-read-passwd prompt)) t)
        (t nil)))

(defun keyring-pwsafe-filter-line (proc line)
  "Process LINE of process, LINE alway ends with a new line"
  (cond ((< (length line) 2))
        ((string-match
          "^Going to print login and password to stdout" line))
        ((string-match
          "^\\(username\\|password\\) for .*\\(:.*\\)" line)
         (insert (upcase-initials
                  (match-string 1 line))
                 (match-string 2 line) "\n"))
        ((string-match "^> \\(.*\\)" line)
         (insert "\n" (replace-regexp-in-string
                       "\\\\n" "\n" (match-string 1 line))))
        (t (insert line))))

(provide 'keyring)

;; Copyright (C) 2006-7  Ivan Kanis
;; Author: Ivan Kanis
;; 
;;
;; This program is free software ; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation ; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY ; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

