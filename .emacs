(setq mac-command-key-is-meta t) 

(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1) 

(setq auto-mode-alist  (append '(("\\.cl$" . lisp-mode)) auto-mode-alist))
(setq auto-mode-alist  (append '(("\\.clj$" . lisp-mode)) auto-mode-alist))


(eval-after-load "slime"
  '(progn
     (setq slime-lisp-implementations
           '((sbcl ("/usr/local/bin/sbcl"))
             (ecl ("/usr/bin/ecl"))
             (clisp ("/usr/bin/clisp"))))
     (slime-setup '(
                    slime-asdf
                    slime-autodoc
                    slime-editing-commands
                    slime-fancy-inspector
                    slime-fontifying-fu
                    slime-fuzzy
                    slime-indentation
                    slime-mdot-fu
                    slime-package-fu
                    slime-references
                    slime-repl
                    slime-sbcl-exts
                    slime-scratch
                    slime-xref-browser
                    ))
     (slime-autodoc-mode)
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function
  'slime-fuzzy-complete-symbol)))

;https://common-lisp.net/project/linedit/
(setq inferior-lisp-program "sbcl --noinform --no-linedit") 

(require 'slime)
