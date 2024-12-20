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

(straight-use-package 'diminish)
(straight-use-package 'use-package)
(straight-use-package 'yaml-mode)

(use-package doom-themes
  :ensure t
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;;  (doom-themes-neotree-config)
  ;; or for treemacs users
  ;;  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :config (setq doom-modeline-bar-width 3
                doom-modeline-enable-word-count t
                doom-modeline-icon t
                doom-modeline-major-mode-icon t
                doom-modeline-major-mode-color-icon t
                doom-modeline-lsp t))

(use-package flymake-cursor
  :straight t
  :config
  (add-hook 'flymake-mode-hook 'flymake-cursor-mode))

(load-theme 'wombat)
(menu-bar-mode -1)
(tool-bar-mode -1)
(delete-selection-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)
(setq inhibit-startup-screen t)

;; (global-display-line-numbers-mode 1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;;(require 'linum)
;;(global-linum-mode)
;;(setq linum-format "%4d\u2502")
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
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
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
(use-package undo-tree
  :straight t
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (undo-tree-visualizer-timestamps t)
  )

(use-package anzu
  :straight t
  :diminish anzu-mode)
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

;;(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;;(defun colorize-compilation-buffer ()
;;  (toggle-read-only)
;;  (ansi-color-apply-on-region compilation-filter-start (point))
;;  (toggle-read-only))
;;(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Enable EDE only in C/C++
(require 'ede)
(global-ede-mode)


(use-package so-long
  :ensure nil
  :config
  (global-so-long-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-ignore-list '("_deps"))
 '(ag-reuse-buffers t)
 '(cmake-tab-width 4)
 '(custom-safe-themes
   '("5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882" "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9" "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19" "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f" "013728cb445c73763d13e39c0e3fd52c06eefe3fbd173a766bfd29c6d040f100" "8b148cf8154d34917dfc794b5d0fe65f21e9155977a36a5985f89c09a9669aa0" "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "4b6cc3b60871e2f4f9a026a5c86df27905fb1b0e96277ff18a76a39ca53b82e1" "a9abd706a4183711ffcca0d6da3808ec0f59be0e8336868669dc3b10381afb6f" "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" "38c0c668d8ac3841cb9608522ca116067177c92feeabc6f002a27249976d7434" "c1d5759fcb18b20fd95357dcd63ff90780283b14023422765d531330a3d3cec2" "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9" "b754d3a03c34cfba9ad7991380d26984ebd0761925773530e24d8dd8b6894738" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" default))
 '(gdb-display-source-buffer-action '(nil (reusable-frames . t)))
 '(lsp-clients-clangd-args '("--header-insertion=never"))
 '(lsp-diagnostics-provider t)
 '(projectile-globally-ignored-file-suffixes '("~"))
 '(projectile-indexing-method 'alien)
 '(so-long-max-lines 5000)
 '(so-long-minor-modes
   '(font-lock-mode lsp-mode display-line-numbers-mode flymake-mode flyspell-mode glasses-mode goto-address-mode goto-address-prog-mode hi-lock-mode highlight-changes-mode hl-line-mode linum-mode nlinum-mode prettify-symbols-mode visual-line-mode whitespace-mode diff-hl-amend-mode diff-hl-flydiff-mode diff-hl-mode dtrt-indent-mode flycheck-mode hl-sexp-mode idle-highlight-mode rainbow-delimiters-mode smartparens-mode copilot-mode smartparens-strict-mode))
 '(tab-width 4)
 '(vterm-ignore-blink-cursor nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
