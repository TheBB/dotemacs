(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((flycheck-disabled-checkers emacs-lisp-checkdoc)
     (projectile-project-compilation-cmd . "make build")
     (projectile-project-test-cmd . "julia --project=. --eval 'using Pkg; Pkg.test(\"Aroma\")'")
     (projectile-project-test-cmd . "julia --project=. --color=yes --eval 'using Pkg; Pkg.test(\"Aroma\")'")
     (projectile-project-run-cmd . "julia --project=. run.jl")
     (projectile-project-run-cmd . "emacs")
     (projectile-project-compilaton-cmd . "make")
     (helm-make-build-dir . "doc")
     (projectile-project-run-cmd . "mkdir -p build; cd build; cmake ..; make run")
     (projectile-project-compilation-cmd . "mkdir -p build; cd build; cmake ..; make")
     (helm-make-build-dir . "Apps/bld-sd"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
