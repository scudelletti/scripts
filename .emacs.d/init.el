(require 'package)

;; Melpa Repo
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Marmalade Repo
(add-to-list 'package-archives
  '("marmalade" .
    "http://marmalade-repo.org/packages/"))

(package-initialize)

;; Poweline
(require 'powerline)
(powerline-default-theme)

;; Copy and Paste Trick for OSX
(defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

;; Whitespace
;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
(setq whitespace-display-mappings
  ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
  '(
;;    (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
    (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
;;    (newline-mark 10 [8629 10]) ; 10 LINE FEED
))
(global-whitespace-mode 1)

;; Smart Parens
(require 'smartparens-config)
(smartparens-global-mode t)

;; Projectile Conf
(require 'cl)
(require 'grizzl)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'grizzl)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;; RVM Integration
(require 'rvm)
(rvm-use-default)

;; Auto Complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20140519.650/dict")
(ac-config-default)

;; Robe - Code Navigation for Ruby
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)
(global-company-mode t)
(push 'company-robe company-backends)
(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

;; Keybinding
(windmove-default-keybindings 'meta)
;;(global-set-key (kbd "C-b <left>") 'shrink-window-horizontally)
;;(global-set-key (kbd "C-b <right>") 'enlarge-window-horizontally)
;;(global-set-key (kbd "S-M-<down>") 'shrink-window)
;;(global-set-key (kbd "S-M-<up>") 'enlarge-window)

;; Disable Dialog Box
(setq use-dialog-box nil)

;; Adjust Identation for Ruby
(setq ruby-deep-indent-paren nil)
(setq js-deep-indent-paren nil)

;; Line Numbers
(global-linum-mode t)

(setq js-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil)
                (setq c-indent-level 2))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes (quote ("60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" default)))
 '(js-indent-level 2)
 '(standard-indent 2)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
