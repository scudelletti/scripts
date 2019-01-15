;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run Binary with Current File as Parameter                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ds-run-initialize ()
  "Initialize Binary and Path"
  (if (not (boundp 'ds-run-var-bin)) (setq ds-run-var-bin "bundle exec rspec"))
  (if (not (boundp 'ds-run-var-suffix)) (setq ds-run-var-suffix ""))
  (if (not (boundp 'ds-run-var-path)) (setq ds-run-var-path nil))
  (if (not (boundp 'ds-run-var-window-name)) (setq ds-run-var-window-name "ds-output"))
  (if (not (boundp 'ds-run-var-relative-path)) (setq ds-run-var-relative-path t)))

(defun ds-run-clear-config ()
  "Clear configuration"
  (interactive)
  (makunbound 'ds-run-var-bin)
  (makunbound 'ds-run-var-suffix)
  (makunbound 'ds-run-var-path)
  (makunbound 'ds-run-var-window-name)
  (markunbound 'ds-run-var-relative-path)
  (ds-run-initialize))

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

(defun ds-run-send-keys (command)
  "Send command to tmux"
  (interactive "sCommand:")
  (shell-command (format "tmux send-keys '%s' ENTER" command)))

(defun ds-run-find-or-create-window ()
  "Select or create a window based on configured window name"
  (shell-command
   (format "tmux select-window -t %s || tmux new-window -n %s" ds-run-var-window-name ds-run-var-window-name)))

(defun ds-run-clear-panel ()
  "Clear panel"
  (ds-run-send-keys "clear"))

(defun ds-run-path ()
  "Current Project path"
  (or ds-run-var-path (projectile-project-root)))

(defun ds-run-command (file-and-options)
  "Run binary in specific folder with file as argument"
  (ds-run-initialize)
  (ds-run-find-or-create-window)
  (ds-run-clear-panel)
  (ds-run-send-keys (format "cd %s" (ds-run-path)))
  (ds-run-send-keys (format "%s %s %s" ds-run-var-bin file-and-options ds-run-var-suffix)))

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
  (if ds-run-var-relative-path
      (ds-file-with-relative-path)
    (buffer-file-name)))

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
