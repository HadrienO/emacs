
;; (defconst shared-config-path "~/.emacs.d/")
;; (load (concat shared-config-path "debian-init.el"))

;; ------------ Linux <-> Win7 compatibility

;; (if (equal system-name "DRIPC")   ;; system-type for by-OS conditions
;;     (progn
;;       (setq default-directory "D:/"
;;             my-file-to-display-at-startup "D:/Documents/Notes/Org/these.org")
;;       (setq GSdir "C:/Program Files/Ghostscript/9.16/bin/GSWIN64C.EXE")
;;       ;;---- LaTeX
;;       (setenv "PATH"
;;               (concat "C:/Program Files/MiKTeX 2.9/miktex/bin/x64" ";"
;;                       (getenv "PATH")))
;;       (setq doc-view-ghostscript-program GSdir)
;;       ;;---- R & ESS
;;       (setq inferior-R-program-name "c:/progra~1/R/R-3.0.2/bin/R.exe")
;;       (add-to-list 'load-path
;;         "C:\\Program Files (x86)\\GNU Emacs 24.3\\site-lisp\\ess")
;;       ;;---- Python
;;       (setenv "PYTHONPATH" "C:\\Python-Anaconda2\\")
;;       (setq python-shell-interpreter
;;             "C:\\Python-Anaconda2\\python.exe"
;;             python-shell-interpreter-args
;;             "-i C:\\Python-Anaconda2\\Scripts\\ipython2-script.py console --matplotlib=qt")
;;       )
;;   )

;; (setenv "PATH"
;;   (concat
;;    "C:/Program Files/MiKTeX 2.9/miktex/bin/x64" ";"
;;    (getenv "PATH")
;;   )
;; )
;; (setq doc-view-ghostscript-program "C:/Program Files/Ghostscript/9.16/bin/gswin64c.exe")
