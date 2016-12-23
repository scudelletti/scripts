;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switch Favourite Themes                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ds-switch-theme (theme)
  "Switch between favourite themes"
  (interactive "sTheme:")
  (if (string= theme "moe")
      (ds-swith-theme-set-moe-light)
      (ds-swith-theme-set-monokay)))

(defun ds-swith-theme-set-monokay ()
  "Sets monokay theme"
  (load-theme 'monokai t)

  ;; Configs that are somethimes overridden by other themes
  (set-face-attribute 'region nil :background "#666")
  (set-face-attribute 'minibuffer-prompt nil :foreground "#66D9EF" :background "#080808"))

(defun ds-swith-theme-set-moe-light ()
  "Sets moe-light theme"
  (require 'moe-theme)
  (moe-light))
