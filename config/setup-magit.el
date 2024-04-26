(provide 'setup-magit)

;;(straight-use-package 'magit)
(use-package magit
  :straight t
  :bind (:map magit-mode-map
              ("TAB" . magit-section-toggle)))

(straight-use-package 'forge)
(straight-use-package 'sqlite3)
(use-package code-review
  :straight (:host github :repo "phelrine/code-review" :branch "fix/closql-update")
  :defer t)

(setq code-review-auth-login-marker 'forge)
(global-set-key (kbd "C-c c x") (lambda () (interactive) (recompile)))
(global-set-key (kbd "C-c c g") (lambda () (interactive) (magit-status)))
(global-set-key (kbd "C-c c M-g") (lambda () (interactive) (magit-log-all)))
