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

(add-to-list 'load-path "~/.emacs.d/config")
(require 'setup-helm)
(require 'setup-magit)
(straight-use-package 'company)
(straight-use-package 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(straight-use-package 'company-irony)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony))


(setq-default c-default-style "stroustrup"
              c-basic-offset 8
              tab-width 8
              indent-tabs-mode t)


(straight-use-package 'projectile)

(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(straight-use-package 'undo-tree)
(global-undo-tree-mode)

(windmove-default-keybindings)
(load-theme 'wombat)
(menu-bar-mode -1)
(tool-bar-mode -1)
(delete-selection-mode 1)


(straight-use-package 'company-tabnine)
(require 'company-tabnine)
(add-to-list 'company-backends #'company-tabnine)
;; Trigger completion immediately.
(setq company-idle-delay 0)
;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)


;; (global-display-line-numbers-mode 1)
;;(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(global-linum-mode)
(setq linum-format "%4d\u2502")
(defalias 'yes-or-no-p 'y-or-n-p)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))


(defun my-change-number-at-point (change increment)
  (let ((number (number-at-point))
        (point (point)))
    (when number
      (progn
        (forward-word)
        (search-backward (number-to-string number))
        (replace-match (number-to-string (funcall change number increment)))
        (goto-char point)))))

(defun my-increment-number-at-point (&optional increment)
  "Increment number at point like vim's C-a"
  (interactive "p")
  (my-change-number-at-point '+ (or increment 1)))

(defun my-decrement-number-at-point (&optional increment)
  "Decrement number at point like vim's C-x"
  (interactive "p")
  (my-change-number-at-point '- (or increment 1)))

(global-set-key (kbd "C-c C-+") 'my-increment-number-at-point)
(global-set-key (kbd "C-c C--") 'my-decrement-number-at-point)

(setq split-height-threshold 120
      split-width-threshold 160)

(defun my-split-window-sensibly (&optional window)
  "replacement `split-window-sensibly' function which prefers vertical splits"
  (interactive)
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
	     (with-selected-window window
	       (split-window-right)))
	(and (window-splittable-p window)
	     (with-selected-window window
	       (split-window-below))))))

(setq split-window-preferred-function #'my-split-window-sensibly)

;;(defun my-sensible-window-split (&optional window)
;;  (cond
;;   ((and (> (window-width window)
;;	    (window-height window))
;;	 (window-splittable-p window 'horizontal))
;;    (with-selected-window window
;;      (split-window-right)))
;;   ((window-splittable-p window)
;;    (with-selected-window window
;;      (split-window-below)))))
;;
;;(setq split-window-preferred-function #'my-sensible-window-split)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(safe-local-variable-values
   '((eval progn
	   (require 'projectile)
	   (setq projectile-project-compilation-dir ".")
	   (puthash
	    (projectile-project-root)
	    "cmake -B build --fresh" projectile-configure-cmd-map)
	   (puthash
	    (projectile-project-root)
	    "cmake --build build" projectile-compilation-cmd-map))
     (eval progn
	   (require 'projectile)
	   (setq projectile-project-compilation-dir ".")
	   (puthash
	    (projectile-project-root)
	    "cmake -B build" projectile-configure-cmd-map)
	   (puthash
	    (projectile-project-root)
	    "cmake --build build" projectile-compilation-cmd-map)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
