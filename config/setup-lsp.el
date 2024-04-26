(use-package lsp-mode :straight t
  :init
  (setq lsp-keymap-prefix "C-l")
  :commands lsp)

;; optionally
(use-package lsp-ui :straight t :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :straight t :commands helm-lsp-workspace-symbol)
;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package ccls :straight t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(setq ccls-executable "/usr/local/bin/ccls")

(use-package company
  :straight t
  :hook (c-mode . company-mode)
  :config
  (push 'company-capf company-backends)
  (setq lsp-completion-provider :capf)
  (setq company-idle-delay 0)
  (setq company-show-numbers t)
  )

(provide 'setup-lsp)

;;; setup-lsp.el ends here
