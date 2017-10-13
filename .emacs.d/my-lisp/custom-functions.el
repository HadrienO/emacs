;; ------------ custom keyboard macro
;; see shortcuts.org


;; -------------------------------------------------------------
;; ----------------------   OVERWRITING   ----------------------
;; -------------------------------------------------------------

;; ---- Wheel scrolling
;;
;; check what are original functions

(defun mouse-wheel-scroll-line (event)
  "Scroll the current buffer by `mouse-wheel-scroll-amount'.
If mouse-wheel-scroll-window-under-mouse is non-nil, scroll the window
beneath the mouse pointer, else scroll current buffer."
  (interactive "e")
  (let ((owin (selected-window)))
    (save-excursion
      (and mouse-wheel-scroll-window-under-mouse
	   (condition-case nil
	       (mouse-set-point event)
	     (error nil)))
      (condition-case nil
	  (if (< (car (cdr (cdr event))) 0)
	      (scroll-up mouse-wheel-scroll-amount)
	    (scroll-down mouse-wheel-scroll-amount))
	(error nil)))
    (or (eq owin (selected-window))
	(select-window owin))))

(defun mouse-wheel-scroll-screen (event)
  "Scroll buffer by `mouse-wheel-scroll-amount'.
If mouse-wheel-scroll-window-under-mouse is non-nil, scroll the window
beneath the mouse pointer, else scroll current buffer."
  (interactive "e")
  (let ((owin (selected-window)))
    (save-excursion
      (and mouse-wheel-scroll-window-under-mouse
	   (condition-case nil
	       (mouse-set-point event)
	     (error nil)))
      (condition-case nil
	  (if (< (car (cdr (cdr event))) 0)
	      (scroll-up)
	    (scroll-down))
	(error nil)))
    (or (eq owin (selected-window))
	(select-window owin))))


;; ---- Horizontal Scrolling
;;
;; (DocView mode only)

;; (defun activate-hscroll-hook ()
;;   (message
;;   (if (boundp 'truncate-lines)
;;       ;;(setq-default truncate-lines t) ; activate in every mode
;;       (setq truncate-lines t)
;;     (progn
;;       (hscroll-global-mode t)
;;       (setq hscroll-margin 1)
;;       (setq auto-hscroll-mode 1)
;;       (setq automatic-hscrolling t)
;;    )))

;; (add-hook 'doc-view-mode-hook 'activate-hscroll-hook)


;; -------------------------------------------------------------
;; ------------------------   GENERAL   ------------------------
;; -------------------------------------------------------------
;;
;;  Functions start with 'my-' for completion and easier lookup
;;  Many of them are stealed from other users,
;;  I will cite them...

(defun my-revert-buffer-no-confirm ()
  "Revert buffer without confirmation
source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el)"
    (interactive)
    (revert-buffer t t)
    )
(global-set-key (kbd "<f5>") 'my-revert-buffer-no-confirm)

(defun my-paste-function (&optional beg end)
  (interactive)
  (let	((beg (cond	(beg beg)
			((region-active-p) (region-beginning))
			(t (line-beginning-position))))
	 (end (cond 	(end end)
			((region-active-p)(copy-marker (region-end)))
			(t (line-end-position)))))

    (kill-ring-save beg end)
    (goto-char end)
    (newline)
    (yank))
  )
(global-set-key "\C-x\w" 'my-paste-function)

(defun my-go-to-column (column)          
  (interactive "nColumn: ")            
  (move-to-column column t)
  )
(global-set-key (kbd "M-g M-c") 'my-go-to-column)

(defun my-file-path-to-clipboard ()
  ;; thx to scottfrazer @
  ;;    http://stackoverflow.com/questions/2416655/file-path-to-clipboard-in-emacs
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename)))
  )

(defun my-sudo-save ()
  ;; thx to erreina @
  ;;   https://www.emacswiki.org/emacs/SudoSave
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name)))
  )
  
(defun my-insert-date (&optional format)
  "Insert present date and time in the current buffer."
  "C-u prefix allows to specify the format (default: dd-mm-yyyy at hh:mi)"
  "src: http://ergoemacs.org/emacs/elisp_datetime.html"
  (interactive (list
                (if current-prefix-arg
                    (read-from-minibuffer
                     "Format (see 'format-time-string'):")
                       nil)))
  (if format
      (insert (format-time-string format))
    (insert (format-time-string "%d-%m-%Y at %R"))))

;; (defun my-show-indentation ()
;;   "Toggle displaying of indentation between t and nil"
;;   (interactive)
;;   (setq tmp-style whitespace-style)
;;   (setq whitespace-style
;;         '(face
;;           trailing
;;           space-mark
;;           tab-mark
;;           indentation::space
;;           indentation::tab
;;           newline
;;           newline-mark))
;;   (whitespace-mode not(whitespace-mode))
;; )
;; (global-set-key "\C-csi" 'my-show-indentation) ;bugged

;; Create a commentary box (for header)
;; -> make one for commented title allowing file hiding/wrapping?

;; (defun my-comment-box (pos1 pos2 param1 param2)
;;   (setq my-comment-box-width 75)
;;   (setq my-comment-box-height 10)
;;   (interactive "r\nsParam1: \nsParam2:")
;;   (message "Buffer Name: %s" (buffer-name))
;;   ;; locally define start of line char (comment char),
;;   ;; end of line, and opening/closing line
;;   (let ((sc (comment-start))
;; 	(ec (if (string= "" comment-end)
;; 		comment-start
;; 	      comment-end))
;; 	(sc-len (length sc))
;; 	(ec-len (length ec))
;; 	(boxl (concat
;; 	       sol
;; 	       (make-string (- my-comment-box-width (+ sc-len ec-len)) ?-)
;; 	       eol))
;; 	(box-emp (concat
;; 		  sol
;; 		  (make-string (- my-comment-box-width (+ sc-len ec-len)) ? )
;; 		  eol))
;; 	)
;;     )
;;   )
;; ;; (if (< my-comment-box-width (+ 3 sc-len ec-len))
;; ;;     (message (
;; ;; (insert (
;; ;; )
;; (insert (format "AAAa: %d,%d,%s,%s" pos1 pos2 param1 param2))
;; ;; (with-current-buffer (buffer-name)
;; ;;   (princ (format "AAAa: %d,%d,%s,%s" pos1 pos2 param1 param2)))
;; 					; (local-set-key "\C-c\w" 'ess-execute-screen-options)
;; )


  
  
;; -------------------------------------------------------------
;; ---------------------   MODE SPECIFIC   ---------------------
;; -------------------------------------------------------------

;; ---- ESS & R

;; Adapt output width to buffer width by Ctr-C + W
(defun my-ess-post-run-hook ()
  (ess-execute-screen-options)
  (local-set-key "\C-c\w" 'ess-execute-screen-options))
(add-hook 'ess-post-run-hook 'my-ess-post-run-hook)

;; Problem with following commenting function since update (not required anymore?)
;; (defun uncomment-region (beg end)
;;   "Like `comment-region' invoked with a C-u prefix arg."
;;   (interactive "r")
;;   (comment-region beg end -1))
;; (define-key ess-mode-map (kbd "C-x C-d") 'comment-region)
;; (define-key ess-mode-map (kbd "C-x C-u") 'uncomment-region)


;; Use shift-enter to split window & launch R (if not running), execute highlighted
;; region (if R running & area highlighted), or execute current line
;; (and move to next line, skipping comments). Nice.
;;    See http://www.emacswiki.org/emacs/EmacsSpeaksStatistics,
;;    FelipeCsaszar.
;; Adapted to splitt vertically instead of horizontally.

(defun my-ess-start-R ()
  (interactive)
  (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
      (progn
        (delete-other-windows)
        (setq w1 (selected-window))
        (setq w1name (buffer-name))
        (setq w2 (split-window w1 nil t))
        (R)
        (set-window-buffer w2 "*R*")
        (set-window-buffer w1 w1name)
        ))
  )

(defun my-ess-eval ()
  (interactive)
  (my-ess-start-R)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
    (call-interactively 'ess-eval-line-and-step)))


;; ---- Python

(defun my-python-reinstate-current-directory ()
  "When running Python, add the current directory ('') to the head of sys.path.
For reasons unexplained, run-python passes arguments to the
interpreter that explicitly remove '' from sys.path. This means
that, for example, using `python-send-buffer' in a buffer
visiting a module's code will fail to find other modules in the
same directory. Adding this function to `inferior-python-mode-hook' reinstates
the current directory in Python's search path."
  (python-send-string "sys.path[0:0] = ['']"))

(add-hook 'inferior-python-mode-hook 'my-python-reinstate-current-directory)

;; Bind Shift-RET to evaluate current line

; (add-hook 'python-mode-hook
          ; 'my-python-send-statement)

; (defun my-python-send-statement ()
  ; (interactive)
  ; (local-set-key [S-return] 'my-python-send-statement)
  ; (end-of-line)
  ; (set-mark (line-beginning-position))
  ; (call-interactively 'python-shell-send-region)
  ; (python-shell-send-string "; print()"))

 ; (defun my-python-start ()
    ; (interactive)
    ; (if (not (member "*Python*" (mapcar (function buffer-name) (buffer-list))))
      ; (progn
		; (delete-other-windows)
		; (setq w1 (selected-window))
		; (setq w1name (buffer-name))
		; (setq w2 (split-window w1 nil t))
		; (run-python)
		; (set-window-buffer w2 "*Python*")
		; (set-window-buffer w1 w1name))))

(defun my-python-start ()
	(interactive)
	(run-python))

(defun my-python-send-region (&optional beg end)
  (interactive)
  (let ((beg
         (cond (beg beg)
               ((region-active-p) (region-beginning))
               (t (line-beginning-position))
               )
         )
        (end
         (cond (end end)
               ((region-active-p)(copy-marker (region-end)))
               (t (line-end-position))
               )
         ))
    (python-shell-send-region beg end))
  (python-nav-forward-statement)
  ;; (let ((beg (cond (beg beg)
  ;;          ((region-active-p)
  ;; (region-beginning))
  ;; (t (line-beginning-position))))
  ;; (end (cond (end end)
  ;; ((region-active-p)
  ;; (copy-marker (region-end)))
  ;; (t (line-end-position))))))
  ;;(t (let((skipLine 1))) (line-end-position))))))
  ;; (when (equal 'skipLine 1) (forward-line 1))
  ;; (python-shell-send-region beg end)
  ;; (python-next-statement))
  )
(add-hook 'python-mode-hook
  '(lambda () (local-set-key [(shift return)] 'my-python-send-region)))

(defun my-compile ()
  "Use compile to run python programs"
  (interactive)
  (compile (concat "python " (buffer-name))))
(setq compilation-scroll-output t)

(add-hook 'python-mode-hook
  '(lambda () (local-set-key "\C-c\C-c" 'my-compile)))


;; -------------------------------------------------------------
;; ----------------------   PARSING FUN   ----------------------
;; -------------------------------------------------------------

;; https://stackoverflow.com/questions/2228477/parsing-in-emacs-lisp

(defun gettok ()
  (and *token* (pop *token*)))
(defun peektok ()
  (and *token* (car *token*)))

(defun rdh/expr ()
  (rdh/expr-tail (rdh/factor)))

(defun rdh/expr-tail (expr)
  (let ((tok (peektok)))
    (cond ((or (null tok)
           (equal tok ")"))
       expr)
      ((member tok '(+ -))
       (gettok)
       (let ((fac (rdh/factor)))
         (rdh/expr-tail (list tok expr fac))))
      (t (error "bad expr")))))

(defun rdh/factor ()
  (rdh/factor-tail (rdh/term)))

(defun rdh/factor-tail (fac)
  (let ((tok (peektok)))
    (cond ((or (null tok)
           (member tok '(")" + -)))
       fac)
      ((member tok '(* /))
       (gettok)
       (let ((term (rdh/term)))
         (rdh/factor-tail (list tok fac term))))
      (t (error "bad factor")))))

(defun rdh/term ()
  (let* ((prim (rdh/prim))
         (tok (peektok)))
    (cond ((or (null tok)
               (member tok '(")" + - / *)))
           prim)
          ((equal tok '^)
           (gettok)
           (list tok prim (rdh/term)))
          (t (error "bad term")))))

(defun rdh/prim ()
  (let ((tok (gettok)))
    (cond ((numberp tok) tok)
      ((equal tok "(")
       (let* ((expr (rdh/expr))
          (tok (peektok)))
         (if (not (equal tok ")"))
         (error "bad parenthesized expr")
           (gettok)
           expr)))
      (t (error "bad prim")))))

;; Test it with:
;; (setq *token* '( 3 ^ 5 ^ 7 + 5 * 3 + 7 / 11))
;; (rdh/expr)
