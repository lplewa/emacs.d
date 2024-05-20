(setq cmake-tab-width 4)
(setq compilation-scroll-output t)
(setq-default c-default-style "stroustrup"
              c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
;;  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
;;  (toggle-read-only)
  )
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(straight-use-package 'clang-format+)
(require 'clang-format+)
(add-hook 'c-mode-common-hook #'clang-format+-mode)

(provide 'setup-c)


;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))
