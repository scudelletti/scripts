;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run Binary with Current File as Parameter                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ds-run-initialize ()
  "Initialize Binary and Path"
  (if (not (boundp 'ds-run-var-bin)) (setq ds-run-var-bin "bundle exec rspec"))
  (if (not (boundp 'ds-run-var-path)) (setq ds-run-var-path (projectile-project-root)))
  (if (not (boundp 'ds-run-var-window-name)) (setq ds-run-var-window-name "ds-output")))

(defun ds-run-clear-config ()
  "Clear configuration"
  (interactive)
  (makunbound 'ds-run-var-bin)
  (makunbound 'ds-run-var-path)
  (makunbound 'ds-run-var-window-name)
  (ds-run-initialize))

(defun ds-run-set-path (path)
  "Configure directory that the command will run"
  (interactive "sPath:")
  (setq ds-run-var-path path))

(defun ds-run-set-bin (bin)
  "Configure binary that will run"
  (interactive "sBin:")
  (setq ds-run-var-bin bin))

(defun ds-run-send-keys (command)
  "Send command to tmux"
  (interactive "sCommand:")
  (shell-command (format "tmux send-keys '%s' ENTER" command)))

(defun ds-run-find-or-create-window ()
  "Select or create a window based on configured window name"
  (shell-command
   (format "tmux find-window %s || tmux new-window -n %s" ds-run-var-window-name ds-run-var-window-name)))

(defun ds-run-clear-panel ()
  "Clear panel"
  (ds-run-send-keys "clear"))

(defun ds-run-command (file-and-options)
"Run binary in specific folder with file as argument"
  (ds-run-initialize)
  (ds-run-find-or-create-window)
  (ds-run-clear-panel)
  (ds-run-send-keys (format "cd %s" ds-run-var-path))
  (ds-run-send-keys (format "%s %s" ds-run-var-bin file-and-options)))

(defun ds-run ()
  "Run binary in specific folder with file as argument"
  (interactive)
  (ds-run-command (buffer-file-name)))

(defun ds-run-line ()
  "Run binary in specific folder with file as argument and line number format: file_name:number"
  (interactive)
  (ds-run-command (format "%s:%s" (buffer-file-name) (count-lines 1 (point)))))
