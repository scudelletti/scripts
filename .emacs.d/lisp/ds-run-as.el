;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run Binary with Current File as Parameter                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ds-run-as-initialize ()
  "Initialize Binary and Path"
  (if (not (boundp 'ds-run-as-var-bin)) (setq ds-run-as-var-bin "bundle exec rspec"))
  (if (not (boundp 'ds-run-as-var-path)) (setq ds-run-as-var-path (projectile-project-root)))
  (if (not (boundp 'ds-run-as-var-window-name)) (setq ds-run-as-var-window-name "ds-output")))

(defun ds-run-as-set-path (path)
  "Configure directory that the command will run"
  (interactive "sPath:")
  (setq ds-run-as-var-path path))

(defun ds-run-as-set-bin (bin)
  "Configure binary that will run"
  (interactive "sBin:")
  (setq ds-run-as-var-bin bin))

(defun ds-run-as-send-keys (command)
  "Send command to tmux"
  (shell-command (format "tmux send-keys '%s' ENTER" command)))

(defun ds-run-as-find-or-create-window ()
  "Select or create a window based on configured window name"
  (shell-command
   (format "tmux find-window %s || tmux new-window -n %s" ds-run-as-var-window-name ds-run-as-var-window-name)))

(defun ds-run-as-clear-panel ()
  "Clear panel"
  (ds-run-as-send-keys "clear"))

(defun ds-run-as ()
  "Run binary in specific folder with file as argument"
  (interactive)
  (ds-run-as-initialize)
  (ds-run-as-find-or-create-window)
  (ds-run-as-clear-panel)
  (ds-run-as-send-keys (format "cd %s" ds-run-as-var-path))
  (ds-run-as-send-keys (format "%s %s" ds-run-as-var-bin (buffer-file-name))))
