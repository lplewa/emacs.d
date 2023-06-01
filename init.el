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
(straight-use-package 'irony-mode)

(require 'company)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(straight-use-package 'company-irony)
;;(require 'company-irony)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony))



(straight-use-package 'rtags)
(require 'rtags)
;; (straight-use-package 'helm-rtags)
(rtags-enable-standard-keybindings)
(setq rtags-use-helm t)

(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)
(setq rtags-completions-enabled t)
(push 'company-rtags company-backends)
(global-company-mode)
(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))

;; (autoload 'helm-imenu "imenu" nil t)
(defun setup-flycheck-rtags ()
  (interactive)
  (flycheck-select-checker 'rtags)
  ;; RTags creates more accurate overlays.
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))

(straight-use-package 'flycheck)
(require 'flycheck-rtags)
(add-hook 'c-mode-common-hook #'setup-flycheck-rtags)

(setq-default c-default-style "stroustrup"
              c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

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

(windmove-default-keybindings)
(load-theme 'wombat)
(menu-bar-mode -1)
(tool-bar-mode -1)
(delete-selection-mode 1)
(straight-use-package 'cmake-mode)
(require 'cmake-mode)


;; Trigger completion immediately.
(setq company-idle-delay 0)
;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)


;; (global-display-line-numbers-mode 1)
;;(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(global-linum-mode)
(setq linum-format "%4d\u2502")
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)

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

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-end-position)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
  line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-end-position)))))

;; kill a line, including whitespace characters until next non-whiepsace character
;; of next line
(defadvice kill-line (before check-position activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode
                                latex-mode plain-tex-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

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
 '(cmake-tab-width 8)
 '(compilation-scroll-output t)
 '(inhibit-startup-screen t)
 '(rtags-display-result-backend 'helm)
 '(rtags-imenu-syntax-highlighting t)
 '(safe-local-variable-values
   '((eval progn
           (require 'projectile)
           (setq projectile-project-compilation-dir ".")
           (puthash
            (projectile-project-root)
            "cmake -B build --fresh" projectile-configure-cmd-map)
           (puthash
            (projectile-project-root)
            "make -j -C build test_malloc_pools " projectile-compilation-cmd-map))
     (eval progn
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
