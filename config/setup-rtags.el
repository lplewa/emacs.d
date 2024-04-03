(provide `setup-rtags)

(straight-use-package 'company)
;;(straight-use-package 'irony-mode)

(require 'company)
;;(add-hook 'c++-mode-hook 'irony-mode)
;;(add-hook 'c-mode-hook 'irony-mode)
;;(add-hook 'objc-mode-hook 'irony-mode)
;;(straight-use-package 'company-irony)
;;(require 'company-irony)
;;(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;(eval-after-load 'company
;;    '(add-to-list 'company-backends 'company-irony))


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
(straight-use-package 'flycheck-rtags)
(require 'flycheck-rtags)
(add-hook 'c-mode-common-hook #'setup-flycheck-rtags)

;; Trigger completion immediately.
(setq company-idle-delay 0)
;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)

(setq rtags-display-result-backend 'helm)
(setq rtags-imenu-syntax-highlighting t)
