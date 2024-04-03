(windmove-default-keybindings)
(straight-use-package 'framemove)
(framemove-default-keybindings)
(setq framemove-hook-into-windmove t)

(push '("\\*compilation\\*" . (nil (reusable-frames . t))) display-buffer-alist)
(push '("\\*ag search*\\*" . (nil (reusable-frames . t))) display-buffer-alist)

(provide 'setup-framemove)
