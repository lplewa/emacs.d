
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
