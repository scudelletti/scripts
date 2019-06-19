;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run Binary with Current File as Parameter                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ds-run-settings-default '(
  (ds-run-var-bin . "cat")
  (ds-run-var-suffix . "")
  (ds-run-var-path . nil)
  (ds-run-var-window-name . "ds-output")
  (ds-run-var-relative-path . t)
  (ds-run-var-file-hook . (lambda (file) file))))

; Expected to be overridden
(setq ds-run-settings `())

(defun ds-run-clear-config ()
  "Clear configuration"
  (interactive)
  (makunbound 'ds-run-var-bin)
  (makunbound 'ds-run-var-suffix)
  (makunbound 'ds-run-var-path)
  (makunbound 'ds-run-var-window-name)
  (markunbound 'ds-run-var-relative-path))

;; Setters
(defun ds-run-set-path (path)
  "Configure directory that the command will run"
  (interactive "sPath:")
  (setq ds-run-var-path path))

(defun ds-run-set-bin (bin)
  "Configure binary that will run"
  (interactive "sBin:")
  (setq ds-run-var-bin bin))

(defun ds-run-set-suffix (suffix)
  "Configure command suffix"
  (interactive "sSuffix:")
  (setq ds-run-var-suffix suffix))

(defun ds-run-set-relative-path (raw-flag)
  "Configure relative path for file name"
  (interactive (list (completing-read "sFlag: " '("nil" "t"))))
  (let* ((flag (if (equal raw-flag '("nil")) nil t)))
    (setq ds-run-var-relative-path flag)))

(defun ds-run-set-file-hook (fun)
  "Hook to change file name"
  (setq ds-run-var-file-hook fun))

(defun ds-run-get-config (hash key)
  (or
   (alist-get key hash)
   (alist-get key ds-run-settings-default)))

(defun ds-run-get (key)
  (ds-run-get-config
   (alist-get major-mode ds-run-settings)
   key))

;; Getters
(defun ds-run-bin ()
  (or (and (boundp 'ds-run-var-bin) ds-run-var-bin)
      (ds-run-get 'ds-run-var-bin)))

(defun ds-run-suffix ()
  (or (and (boundp 'ds-run-var-suffix) ds-run-var-suffix)
      (ds-run-get 'ds-run-var-suffix)))

(defun ds-run-path ()
  "Current Project path"
  (or (and (boundp 'ds-run-var-path) ds-run-var-path)
      (ds-run-get 'ds-run-var-path)
      (projectile-project-root)))

(defun ds-run-window-name ()
  (or (and (boundp 'ds-run-var-window-name) ds-run-var-window-name)
      (ds-run-get 'ds-run-var-window-name)))

(defun ds-run-relative-path ()
  (or (and (boundp 'ds-run-var-relative-path) ds-run-var-relative-path)
      (ds-run-get 'ds-run-var-relative-path)))

(defun ds-run-file-hook ()
  (or (and (boundp 'ds-run-var-file-hook) ds-run-var-file-hook)
      (ds-run-get 'ds-run-var-file-hook)))

;; Tmux
(defun ds-run-send-keys (command)
  "Send command to tmux"
  (interactive "sCommand:")
  (shell-command (format "tmux send-keys '%s' ENTER" command)))

(defun ds-run-find-or-create-window ()
  "Select or create a window based on configured window name"
  (shell-command
   (format "tmux select-window -t %s || tmux new-window -n %s" (ds-run-window-name) (ds-run-window-name))))

(defun ds-run-clear-panel ()
  "Clear panel"
  (ds-run-send-keys "clear"))

(defun ds-run-command (file-and-options)
  "Run binary in specific folder with file as argument"
  (ds-run-find-or-create-window)
  (ds-run-clear-panel)
  (ds-run-send-keys (format "cd %s" (ds-run-path)))
  (ds-run-send-keys (format "%s %s %s" (ds-run-bin) file-and-options (ds-run-suffix))))

(defun ds-run-file ()
  "Run binary in specific folder with file as argument"
  (interactive)
  (ds-run-command (ds-buffer-file-name)))

(defun ds-run-line ()
  "Run binary in specific folder with file as argument and line number format: file_name:number"
  (interactive)
  (ds-run-command (format "%s:%s" (ds-buffer-file-name) (count-lines 1 (point)))))

(defun ds-buffer-file-name ()
  "Buffer file name - Can be relative or not based on configuration"
  (funcall (ds-run-file-hook) (if (ds-run-relative-path)
      (ds-file-with-relative-path)
    (buffer-file-name))))

(defun ds-file-with-relative-path ()
  "Return path relative to "
  (cadr (split-string (buffer-file-name) (projectile-project-root))))

(defun ds-run (line_only_flag)
  "Run binary in specific folder with file as argument. When a simple prefix argument is present run the line only"
  (interactive "P")
  ;; '(4) is the argument when called like C-u M-x ds-run
  (if (equal '(4) line_only_flag)
      (ds-run-line)
    (ds-run-file)))
