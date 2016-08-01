;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run Binary with Current File as Parameter                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ds-run-as-initialize ()
  "Initialize Binary and Path"
  (if (not (boundp 'ds-run-as-var-bin)) (setq ds-run-as-var-bin "bundle exec rspec"))
  (if (not (boundp 'ds-run-as-var-path)) (setq ds-run-as-var-path (projectile-project-root))))

(defun ds-run-as-set-path (path)
  "Configure directory that the command will run"
  (interactive "sPath:")
  (setq ds-run-as-var-path path))

(defun ds-run-as-set-bin (bin)
  "Configure binary that will run"
  (interactive "sBin:")
  (setq ds-run-as-var-bin bin))

(defun ds-run-as-shell-inner-command ()
  "Return command that will be executed inside tmux"
  (mapconcat 'identity `("source ~/.profile" "&&" "cd" ,ds-run-as-var-path "&&" ,ds-run-as-var-bin ,(buffer-file-name) "&&" "$SHELL")  " "))

(defun ds-run-as-shell-external-command ()
  "Return tmux command with inner command"
  (interactive)
  (mapconcat 'identity `("tmux split-window '" ,(ds-run-as-shell-inner-command) "'") ""))

(defun ds-run-as ()
  "Run binary in specific folder with file as argument"
  (interactive)
  (ds-run-as-initialize)
  (async-shell-command (ds-run-as-shell-external-command)))
