(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(load-theme 'wombat)
(menu-bar-mode -1)
(tool-bar-mode -1)
(delete-selection-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)
(setq inhibit-startup-screen t)

;; (global-display-line-numbers-mode 1)
;;(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(require 'linum)
(global-linum-mode)
(setq linum-format "%4d\u2502")
(column-number-mode)
(add-to-list 'load-path "~/.emacs.d/config")
(require 'setup-helm)
(require 'setup-magit)
(require 'setup-org)
(require 'setup-rtags)
;;(require 'setup-split)
(provide 'setup-c)
(require 'move-text)
(require 'setup-framemove)
(require 'ks-mode)
(require 'kill-line)
(require 'increment)

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :ensure t)
(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)


(straight-use-package 'iedit)

(straight-use-package 'sqlite3)

(straight-use-package 'vterm)
(add-hook 'vterm-mode-hook
          (lambda () (interactive)
            (linum-mode -1)))


(straight-use-package 'ag)
(require 'ag)

(straight-use-package 'projectile)

(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

;;(straight-use-package 'projectile-ag)
;;(require 'projectile-ag)

(straight-use-package 'undo-tree)
(global-undo-tree-mode)

(straight-use-package 'anzu)
(require `anzu)
(global-anzu-mode +1)
(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)

(straight-use-package 'cmake-mode)
(require 'cmake-mode)

(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-stickyfunc-mode 1)

(semantic-mode 1)

(defun alexott/cedet-hook ()
  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))

(add-hook 'c-mode-common-hook 'alexott/cedet-hook)
(add-hook 'c-mode-hook 'alexott/cedet-hook)
(add-hook 'c++-mode-hook 'alexott/cedet-hook)


;; Enable EDE only in C/C++
(require 'ede)
(global-ede-mode)
