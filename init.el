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
;;(require 'setup-rtags)
(require 'setup-lsp)
;;(require 'setup-split)
(require 'setup-c)
(require 'move-text)
(require 'setup-framemove)
(require 'ks-mode)
(require 'kill-line)
(require 'increment)
(require 'setup-copilot)
(require 'setup-org)

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


(require 'ansi-color)
(defadvice pop-to-buffer (before cancel-other-window first)
  (ad-set-arg 1 nil))

(ad-activate 'pop-to-buffer)

;; Toggle window dedication
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

;; Press [pause] key in each window you want to "freeze"
(global-set-key [pause] 'toggle-window-dedicated)


(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;;(defun colorize-compilation-buffer ()
;;  (toggle-read-only)
;;  (ansi-color-apply-on-region compilation-filter-start (point))
;;  (toggle-read-only))
;;(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Enable EDE only in C/C++
(require 'ede)
(global-ede-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-ignore-list '("_deps"))
 '(ag-reuse-buffers t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
