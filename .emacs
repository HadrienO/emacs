;; ========================================================================== ;;
;;                            Emacs Configuration                             ;;
;;        ------------------------------------------------------------        ;;
;;                                                                            ;;
;; Configuration is split between different files:                            ;;
;;  - this '.emacs'           general and system-specific config              ;;
;;  + in '.emacs.d/my-lisp/                                                   ;;
;;    - custom-functions.el   custom function definitions                     ;;
;;    - custom-keybinds.el    custom keybinds definitions                     ;;
;;    - init-finalization.el  session custom to run if everything went ok     ;;
;;                                                                            ;;
;; Notes:                                                                     ;;
;;  - see R & ESS section if ESS hangs when starting a new R session          ;;
;; ========================================================================== ;;


(warn (format
       (concat
        "\n - Problem with dired-single-magic-buffer: ..."
        "\n... climbing the folders' tree 'above' the dir it was called from"
        "seems to deactivate the use of the magic-buffer"
        "and makes dired-subtree buggy"
        "\n-> caused by symlinks??"
        )))
;; -------------------------------------------------------------
;; --------------  Required packages installation --------------
;; -------------------------------------------------------------

;; (setq package-archives '(("elpa" . "http://tromey.com/elpa/")
;;                          ("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("marmalade" . "http://marmalade-repo.org/packages/")))


(defun my-ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it's not.
Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         package
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         nil)))
   packages))

(require 'package)
(when (>= emacs-major-version 24)
  "Set-up package installation from internet"
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t))

(package-initialize)
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; -------------------------------------------------------------
;; add required packages here (see function above)

(setq package-list '(
             ido-vertical-mode
             smart-tabs-mode
             palette
             magit
             rebox2
             dired-subtree
             dired-single
		     ;;org            ; built-in
		     ;;flyspell       ; built-in
		     ;;package        ; built-in
		     ;;recentf        ; built-in
		     ;;ipython        ; non-existant ?
		     ;;ess-site       ; installed via apt-get
))

(mapcar 'my-ensure-package-installed package-list)

;; -------------------------------------------------------------
;; ----------------------  INITIALIZATION  ---------------------
;; -------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/my-lisp")

(load "debian-init.el")			; paths and system-specific variables defined here
(load "custom-functions.el")	; commands and custom functions defined here

;; ------------ General settings
(defvar col-background-base "#eedcc2")
(defvar col-foreground-base "#111111")
(defvar col-cursor-base "red")		 

(setq default-frame-alist
      `((top . 000) (left . 335)
        (width . 80) (height . 43)
        (vertical-scroll-bars . right)
        (cursor-color . ,col-cursor-base)
        (foreground-color . ,col-foreground-base)
        (background-color . ,col-background-base)
        (background-color . ,col-background-base)
        ;; dunno why, but background-color must be added twice to work
        ;; on the init frame (and initial-frame-alist is nil)
        )
      )

(defalias 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode 1)        ; overwrite selected text
(show-paren-mode 1)              ; show matching parenthesis
(setq desktop-restore-eager 5)   ; nbr of buffers immediatly restored at startup
(desktop-save-mode 0)            ; turned on at the end of init if everything is ok
;; (require 'tool-bar)           ; disabling toolbar
(tool-bar-mode -1)
(column-number-mode t)

(defvar mouse-wheel-scroll-window-under-mouse t
  "*When non-nil, wheel scrolling affects window
 beneath the mouse pointer, else the current buffer.")

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent 'complete)
(setq compilation-scroll-output t)

(auto-fill-mode -1)                              ; Turn off auto-fill by default
(remove-hook 'text-mode-hook #'turn-on-auto-fill)
;; (setq-default auto-fill-function 'do-auto-fill)  ; Autofill in all modes
;; (setq fill-column 78)                            ; wordwrap i this column

;; ------------ Disabling some functions
(put 'delete-region 'disabled
     "Text deleted this way cannot be yanked back!\n")
(put 'kill-sentence 'disabled
     "Warning: sentence is not always properly defined!\n")
;; (put 'upcase-region 'disabled nil) ; remove keybind instead
;;-----

(require 'recentf)    ; Recently opened documents
(recentf-mode 1)

(require 'org)
;; add R and python to languages processed by babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (python . t)
   (latex . t)
   (emacs-lisp . t)
   ))

;; increase latex equation size 
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.4))
;; fontification by default
(setq org-src-fontify-natively t)
(setq org-log-done t)

(require 'whitespace)
;; (global-set-key "\C-c_w" 'whitespace-mode)
(set-face-attribute 'whitespace-space nil
                    :foreground "firebrick"
                    :background "#F7E8CC")
(copy-face 'whitespace-space 'whitespace-newline)
(copy-face 'whitespace-space 'whitespace-tab)

(set-face-attribute 'whitespace-indentation nil
                    :background "#f09292")
(set-face-attribute 'whitespace-line nil
                    :foreground "#593284"
                    :background "#F3C265")
(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal
      ;; try (insert-char 182 ) to see it
      '(
        (newline-mark 10 [182 10]) ;10 LI FEED
        (space-mark 32 [183] [46]) ;32 SPACE,183 MDLE DOT,46 FULL STOP
        (tab-mark 9 [32 187 9] [32 92 9])
        ;; (tab-mark 9 [9655 9] [92 9]);9 TAB,9655 WH. R-POINTING TRIANGLE
        ))

(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-use-faces t)
(set-face-attribute 'ido-vertical-first-match-face nil
                    :background "#e5b7c0")
(set-face-attribute 'ido-vertical-only-match-face nil
		    :weight 'bold
                    :background nil
		    :foreground nil)
(set-face-attribute 'ido-vertical-match-face nil
                    :foreground "#b00000")
(ido-vertical-mode 1)

(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)      ; navigate options with up/down
; (setq ido-vertical-define-keys 'C-n-and-C-p-only)       ; only C-p and C-n
; (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)  ; navigate history with <- & ->


(require 'flyspell)
(dolist (hook '(LaTeX-mode-hook html-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(require 'smart-tabs-mode)
(smart-tabs-insinuate 'c 'c++
                      'javascript 'java)
;; (add-hook 'python-mode-hook
;;           (lambda () (setq indent-tabs-mode t)))
(add-hook 'c-mode-common-hook
          (lambda () (setq indent-tabs-mode t)))
(add-hook 'javascript-mode-hook
          (lambda () (setq indent-tabs-mode t)))

(require 'rebox2)
(add-to-list 'rebox-templates
             '(29 325 "?? ========= ??" "?? box123456 ??" "?? ========= ??")
             )
(rebox-register-all-templates)

;; dired-subtree customization
(require 'dired)
(require 'dired-single)
(require 'dired-subtree)
;; (require 'color)

(let ((cnt 0)
      (bkgrd-sat (hexrgb-saturation col-background-base))
      (bkgrd-val (hexrgb-value col-background-base)))
  (while (< cnt 6)
    (setq cnt (1+ cnt))
    (set-face-attribute
     (intern (concat "dired-subtree-depth-"
                     (int-to-string cnt)
                     "-face")
             )
     nil
     :background
     (hexrgb-increment-value
      (hexrgb-increment-saturation col-background-base
                                   (* cnt (/ (- 1 bkgrd-sat) 18.0)))
      (* cnt (/ bkgrd-val -18.0)))
     )
    )
  )

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode t)
            (setq dired-single-use-magic-buffer t)
            ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doc-view-continuous t)
 '(org-agenda-files (quote ("~/documents/Notes/org/todo.org")))
 ;; '(preview-gs-command GSdir)
 '(safe-local-variable-values (quote ((outline-minor-mode)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; -------------------------------------------------------------
;; ------------------------   R & ESS   ------------------------
;; -------------------------------------------------------------

;;(require 'ess-site)

(setq ess-ask-for-ess-directory nil)
(setq ess-local-process-name "R")
(setq ansi-color-for-comint-mode 'filter)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)

;; Solve problem if ess.help not attached
;; (add-hook 'ess-mode-hook
;; 	  'ess--R-load-ESSR        ;worked wiht M-: (ess--R-load-ESSR)
;; 	  ;;'R-initialize-on-start   ;not tried
;; )

;; -------------------------------------------------------------
;; -------------------------   PYTHON   ------------------------
;; ------------------------------------------------------------

(setenv "PYTHONPATH" "/home/hadrien/documents/Programming/Libraries/Python/")
;; (setenv "PYTHONPATH" (shell-command-to-string "$SHELL --login -c 'echo -n $PYTHONPATH'"))

;; set folding option for Python and other languages
;; (load "outline-mode-folding-python-elisp-shell.el")

;; (setq python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;       python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;       python-shell-completion-setup-code
;;       "from IPython.core.completerlib import module_completion"
;;       python-shell-completion-module-string-code
;;       "';'.join(module_completion('''%s'''))\n"
;;       python-shell-completion-string-code
;;       "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
;; note that setting `venv-location` is not necessary if you
;; use the default location (`~/.virtualenvs`), or if the
;; the environment variable `WORKON_HOME` points to the right place
(setq venv-location "/media/hadrien/data-debian/python-virtualenv")

(setq my-current-venv (if venv-current-name venv-current-name "none"))
(setq-default mode-line-format (cons mode-line-format
                                     '("     python-venv: "
                                       (:eval venv-current-name)
                                       )))
(venv-workon "theano")


;; -------------------------------------------------------------
;; ----------------------   TeX & AUCTEX   ---------------------
;; -------------------------------------------------------------
;; the wrapping up of the two loads make sure
;; auctex is loaded only when editing tex files.
(eval-after-load "tex-mode"
  '(progn
     (load "auctex.el" nil nil t)
     (load "preview-latex.el" nil nil t)))

(set-default 'preview-scale-function 1.2)

;;-- AUCTeX replaces latex-mode-hook with LaTeX-mode-hook
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (setq TeX-auto-save t)
	    (setq TeX-parse-self t)
	    (setq TeX-PDF-mode t)
	    ;; (setq-default TeX-master nil)
	    (reftex-mode t)
	    (TeX-fold-mode t))
	  )

;;-- add fontification for packages macros
(setq font-latex-match-reference-keywords
      '(
        ;;built-in
        ("subfile" "[{")
        ("ProvidesPackage" "{[")
        ;; hyperref
        ("hyperref" "[{")
        ;; natbib
        ("citet" "[[{")
        ("citep" "[[{")
        ("citealp" "[[{")
        ;; custom
        ("reffig" "[{")
        ("reftxt" "[{")
        ))
(setq font-latex-match-function-keywords
      '(
	("RequirePackage" "[{")
	;; custom
	("rez" "[[{{{{")
	))
(setq font-latex-user-keyword-classes
      '(("my-warning-commands"
         (("tocomplete") ("testwar"))
         (:background "#6EE6FF" :foreground "red" :weight 'bold)
         noarg)
        )
      )

;; (setq font-latex-match-warning-keywords
;;       '(
;; 	;; custom
;; 	"tocomplete"
;; 	)
;;       )

;; (add-hook 'LaTeX-mode-hook
;;           (lambda ()
;;            (font-lock-add-keywords nil
;;             '(("\\<\\(FIXME\\):" 1
;;                font-lock-warning-face t)))))

;; -------------------------------------------------------------
;; ------------------------   Outline   ------------------------
;; -------------------------------------------------------------

(add-hook 'bibtex-mode-hook 'outline-minor-mode)
(add-hook 'bibtex-mode-hook 'hide-body)
;; (add-hook 'bibtex-mode-hoox
;;  (lambda ()
;;    (setq (make-local-variable 'outline-regexp) "^%@[:alphanum:]+{")))

(add-hook 'html-mode-hook 'outline-minor-mode)
;; extra outline headers {to outline from commented headers}
(add-hook 'LaTeX-mode-hook 'outline-minor-mode)
(setq TeX-outline-extra
      '(("%chapter" 1)
        ("%section" 2)
        ("%subsection" 3)
        ("%subsubsection" 4)
        ("%paragraph" 5)))

;; add font locking to the headers
(font-lock-add-keywords
 'latex-mode
 '(("^%\\(chapter\\|\\(sub\\|subsub\\)?section\\|paragraph\\)"
    0 'font-lock-keyword-face t)
   ("^%chapter{\\(.*\\)}"       1 'font-latex-sectioning-1-face t)
   ("^%section{\\(.*\\)}"       1 'font-latex-sectioning-2-face t)
   ("^%subsection{\\(.*\\)}"    1 'font-latex-sectioning-3-face t)
   ("^%subsubsection{\\(.*\\)}" 1 'font-latex-sectioning-4-face t)
   ("^%paragraph{\\(.*\\)}"     1 'font-latex-sectioning-5-face t)))

;; globalizing outline-minor-mode et toggle it on
;;outline-minor-mode-prefix
;; (define-globalized-minor-mode my-global-outline-mode outline-minor-mode
;;   (lambda () (outline-minor-mode 1)))

;; (my-global-outline-mode 1)

;; -------------------------------------------------------------
;; ----------------------  Caching directories  ----------------
;; -------------------------------------------------------------
;; (eval-after-load
;;     "filecache"
;;   '(progn
;;      (message "Loading file cache...")
;;      (file-cache-add-file "~/.emacs")
;;      ;(file-cache-add-directory-recursively "D:/Documents/TestFiles/")
;;      ;(file-cache-add-directory-using-locate "D:/Documents/TestFiles/")
;;      ;(file-cache-add-directory-list load-path)
;;      ;(file-cache-add-directory "~/")
;;      ;(file-cache-add-file-list (list "~/foo/bar" "~/baz/bar"))
;;      ))

;;; ------- if rather create a cache file to load

;; (defun file-cache-save-cache-to-file (file)
;;   "Save contents of `file-cache-alist' to FILE.
;; For later retrieval using `file-cache-read-cache-from-file'"
;;   (interactive "FFile: ")
;;   (with-temp-file (expand-file-name file)
;;     (prin1 file-cache-alist (current-buffer))))

;; (defun file-cache-read-cache-from-file (file)
;;   "Clear `file-cache-alist' and read cache from FILE.
;; The file cache can be saved to a file using
;; `file-cache-save-cache-to-file'."
;;   (interactive "fFile: ")
;;   (file-cache-clear-cache)
;;     (save-excursion
;;       (set-buffer (find-file-noselect file))
;;       (beginning-of-buffer)
;;       (setq file-cache-alist (read (current-buffer)))))

(load "custom-keybinds.el")			; custom keybinds defined here
(load "init-finalisation.el")		; post-init: load session, split frame, ...
