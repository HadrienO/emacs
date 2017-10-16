;; -------------------------------------------------------------------------- ;;
;;                           Custom Functions File                            ;;
;;           ------------------------------------------------------           ;;
;; This file contains every functions and commands that I define or           ;;
;; overwrite myself.                                                          ;;
;;                                                                            ;;
;; The majority of these definitions is inspired (if not simply               ;;
;; copied) from online sources (forums, tutorials, ...). I'll force           ;;
;; myself to cite original authors as much as possible, but I've              ;;
;; already lost track of many. I'm really sorry for that, and I'll            ;;
;; amend the file if I come across the source again.                          ;;
;;                                                                            ;;
;; Notes:                                                                     ;;
;;                                                                            ;;
;; - New functions' names are prefixed by "my-" for easier search             ;;
;;   and completion.                                                          ;;
;;                                                                            ;;
;; - Some modes' hooks are defined in `.emacs' and `custom-keybinds.el',      ;;
;;   but they're not meant to use as standalone commands.                     ;;
;; -------------------------------------------------------------------------- ;;

;; -------------------------------------------------------------
;; ----------------------   OVERWRITING   ----------------------
;; -------------------------------------------------------------

;; ---- Wheel scrolling
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

(defun my-revert-buffer-no-confirm ()
  "Revert buffer without confirmation.

source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el)"
    (interactive)
    (revert-buffer t t)
    )

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

(defun my-go-to-column (column)          
  (interactive "nColumn: ")            
  (move-to-column column t)
  )

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

(defun my-box-comment-region (beg end title)
  "Make a standadized comment box used for file headers.

Adapted from Trey Jackson
   @ https://stackoverflow.com/questions/5558876/a-custom-comment-box-on-emacs
   see: `my-box-comment-region-fit-text'"
  (interactive "*r\nsEnter a title (leave empty if not desired): ")
  (save-restriction
    (narrow-to-region beg end)
    (uncomment-region beg end)         ; first, uncomment
    (let ((start-char comment-start)
          (end-char comment-end)
          (tot-len 80)
          (inwidth 0)
          (spc-len 0))
      (if (string= "" comment-end)
          ;; if 'comment-start' is a single char and 'commend-end'
          ;; not definded, use two 'comment-start' as border, and
          ;; set 'end-char' to 'start-char'
          (progn
            (if (= 1 (length start-char))
                (setq start-char (make-string 2 (string-to-char start-char))))
            (setq end-char start-char))
        )
      (setq inwidth (- tot-len 2 (length start-char) (length end-char)))  ; 2 for l/r padding
      (kill-region beg end t)
      (with-temp-buffer
        (insert (make-string inwidth ?-)) ; box top border
        (newline)
        (if (not (string= "" title))
            ;; if non empty, insert and format title (fill & center)
            (progn
              (insert title)
              (setq fill-column (- inwidth 25))
              (fill-region (point-min) (point-max))
              (newline)
              (insert (make-string (- inwidth 20) ?-))
              (setq fill-column inwidth)
              (center-region (point-min) (point-max))
              (goto-char (point-max))
              (newline)
              ))
        (yank)
        (pop kill-ring) ;so command doesn't modify kill-ring
        (end-of-line)
        (if (> (current-column) 0) (newline))
        (insert (make-string inwidth ?-)) ; box bottom border
        (string-rectangle (point-min)
                          (progn
                            (goto-char (point-max))
                            (line-beginning-position))
                          (format "%s " start-char))
        (goto-char (point-min))
        (move-to-column (- tot-len (length end-char)))
        (let ((top (point)))
          (goto-char (point-max))
          (move-to-column (- tot-len (length end-char)))
          (string-rectangle top (point)
                            (format " %s" start-char)))
        (end-of-line)
        (kill-rectangle (point-min) (point)) ;don't append to user kill-ring
        )
      (yank-rectangle)
      (newline)
      )
    )
  )

(defun my-box-comment-region-fit-text (beg end)
  "Make a standadized comment box which fit the text in the region.
See `my-box-comment-region' for a box with fixed width.
See `comment-box' and `rebox-mode' (package 'rebox2').

Script from Trey Jackson
   @ https://stackoverflow.com/questions/5558876/a-custom-comment-box-on-emacs"
  
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (comment-region beg end -1)         ; first, uncomment
    (string-rectangle (point-min)
                      (progn (goto-char (point-max)) (line-beginning-position))
                      "  | ")
    (goto-char (point-min))
    (let ((max-len 0))
      (while (< (point) (point-max))
        (end-of-line)
        (setq max-len (max max-len (current-column)))
        (forward-line 1))
      (previous-line)
      (end-of-line)
      (insert (make-string (- max-len (current-column)) ?\ ))
      (goto-char (point-min))
      (end-of-line)
      (insert (make-string (- max-len (current-column)) ?\ ))
      (end-of-line)
      (let ((top (point)))
        (goto-char (point-max))
        (previous-line)
        (end-of-line)
        (string-rectangle top (point) " | "))
      (let ((line-seg (concat "  +" (make-string (- max-len 2) ?-) "+ \n")))
        (goto-char (point-max))
        (insert line-seg)
        (goto-char (point-min))
        (insert line-seg)))
    (comment-region (point-min) (point-max))))

(defun my-rebox-comment-region (beg end title)
  "Make a standadized comment box used for file headers, using `rebox-mode'.
   see: `my-box-comment-region' and `my-box-comment-region-fit-text'
for variants based on built-in functions only."
  (interactive "*r\nsEnter a title (leave empty if no title needed): ")
  (save-restriction
    (narrow-to-region beg end)
    (uncomment-region beg end)         ; first, uncomment
    (kill-region beg end t)
    (let* ((mode major-mode)
           (tot-len 80)
           (style 29)
          )
      (with-temp-buffer
        (funcall mode) ; for rebox to use the correct comment char
        (if (not (string= "" title))
            (progn
              (insert title)
              (setq fill-column (- tot-len 25))
              (fill-region (point-min) (point-max))
              (newline)
              (insert (make-string (- tot-len 20) ?-))
              (setq fill-column tot-len)
              (center-region (point-min) (point-max))
              (goto-char (point-max))
              (newline)
              ))
        (yank)
        ;; (pop kill-ring) ;so command doesn't modify kill-ring
        (end-of-line)
        (if (= (current-column) 0) (forward-line -1))
        (move-to-column (- tot-len 2) t)
        (set-mark (point-min))
        (rebox-cycle style)
        (goto-char (point-max))
        (if (= (current-column) 0) (forward-line -1))
        (end-of-line)
        (kill-rectangle (point-min) (point)) ;don't append to user kill-ring
        )
      (yank-rectangle)
      (newline)
      )
    )
  )

(defun my-rebox-comment-region-2 (beg end title)
  "Make a standadized comment box used for file headers, using `rebox-mode'.
   see: `my-box-comment-region' and `my-box-comment-region-fit-text'
for variants based on built-in functions only."
  (interactive "*r\nsEnter a title (leave empty if no title needed): ")
  (save-restriction
    (narrow-to-region beg end)
    (uncomment-region beg end)          ;first, uncomment
    (kill-region beg end t)
    (let* ((mode major-mode)
           (box-width 80)
           (rebox-min-fill-column box-width)    ;box width: 80
           (style 29)                           ;?? ====== ?? borders
           )
      (with-temp-buffer
        (funcall mode) ; for rebox to use the correct comment char
        (if (not (string= "" title))
            (progn
              (insert title)
              (setq fill-column (- box-width 25))
              (fill-region (point-min) (point-max))
              (newline)
              (insert (make-string (- box-width 20) ?-))
              (setq fill-column (- box-width 6))
              (center-region (point-min) (point-max))
              (goto-char (point-max))
              (newline)
              ))
        (set-mark (point))
        (yank) ;yank original text in temp buffer
        (setq fill-column (- box-width 6))
        (my-fill-lines (mark) (point))  ;fill lines to inside width
        ;; (pop kill-ring) ;so command doesn't modify kill-ring
        (end-of-line)
        (set-mark (point-min))
        ;; (setq rebox-min-fill-column box-width)    ;box width: 80
        (rebox-cycle style)
        (goto-char (point-max))
        (if (= (current-column) 0)
            (progn
              (forward-line -1)
              (end-of-line)))
        (kill-rectangle (point-min) (point)) ;don't append to user kill-ring
        )
      (yank-rectangle)
      (newline)
      )
    )
  )

(defun my-fill-lines (beg end)
  "Fill region line by line, or fill current line if there is no
active region."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (if (not (region-active-p))
          (progn
            (beginning-of-line)
            (setq beg (point))
            (end-of-line)
            (setq end (point))))
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (beginning-of-line)
        (let ((beg (point)))
          (end-of-line)
          (fill-region beg (point) nil t)
          (forward-line 1)
          )))))
;; -------------------------------------------------------------
;; ---------------------   MODE SPECIFIC   ---------------------
;; -------------------------------------------------------------

;; ---- ESS & R

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

;; (defun my-python-start ()
;;   (interactive)
;;   (if (not (member "*Python*" (mapcar (function buffer-name) (buffer-list))))
;;       (progn
;; 		(delete-other-windows)
;; 		(setq w1 (selected-window))
;; 		(setq w1name (buffer-name))
;; 		(setq w2 (split-window w1 nil t))
;; 		(run-python)
;; 		(set-window-buffer w2 "*Python*")
;; 		(set-window-buffer w1 w1name))))

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
  ;; (python-next-statement))
  )

(defun my-compile ()
  "Use compile to run python programs"
  (interactive)
  (compile (concat "python " (buffer-name))))

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
