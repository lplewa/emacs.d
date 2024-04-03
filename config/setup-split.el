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

(provide 'setup-split)
