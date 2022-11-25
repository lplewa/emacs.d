(provide 'setup-magit)

(straight-use-package 'magit)
(straight-use-package 'forge)
(global-set-key (kbd "C-c c x") (lambda () (interactive) (recompile)))
(global-set-key (kbd "C-c c g") (lambda () (interactive) (magit-status)))
(global-set-key (kbd "C-c c M-g") (lambda () (interactive) (magit-log-all)))
