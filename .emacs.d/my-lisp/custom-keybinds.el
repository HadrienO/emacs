;; -------------------------------------------------------------
;; --------------    Custom Keybinds for Emacs    --------------
;; -------------------------------------------------------------
;; 
;;    ----- Global -----
;;  C-x <arrow>		windmove-up/down/right/left
;;  mouse-3			browse-url-at-mouse
;;
;;  C-x g			magit-status
;;  C-x d			dired-single-magic-buffer			repl: ido-list-directory
;;
;;  C-c l			org-store-link
;;  C-c a			org-agenda
;;
;;  C-c s w			whitespace-mode
;;  C-c s c			rainbow-mode
;;
;;    ----- Local  -----
;; ** outline-minor-mode
;;  C-<tab>			outline-mode-toggle-children
;;  C-c C-<tab>		outline-mode-toggle-children
;;
;; ** dired-mode
;;  <tab>			dired-subtree-toggle)
;;  <return>		dired-single-buffer)
;;  mouse-1			dired-single-buffer-mouse)
;;  ^				dired-single-buffer ".."
;;
;; ** r/ess-mode
;;  S-<return>		my-ess-eval
;;  C-c M-c			ess-eval-paragraph
;;  C-<up>			comint-previous-input (in R terminal)
;;  C-<down>		comint-next-input (in R terminal)
;; -------------------------------------------------------------
;;
;; Note:	(global-set-key <k> <b>) == (define-key (current-global-map) <k> <b>)
;; 			(local-set-key <k> <b>)  == (define-key (current-local-map) <k> <b>)
;;  
;; ____________________________________________________________


;; --------------------
;; Navigation
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

(global-set-key [down-mouse-3] 'browse-url-at-mouse)   ; Make URL clickable

;; --------------------
;; Outline-mode
(add-hook 'outline-minor-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-<tab>")
                           outline-mode-prefix-map)
            (local-set-key (kbd "C-<tab>")
                           'outline-toggle-children
                           )
        ))

;; --------------------
;; Org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; --------------------
;; ws-mode
(global-set-key "\C-csw" 'whitespace-mode)

;; --------------------
;; Rainbow-mode
(global-set-key "\C-csc" 'rainbow-mode)

;; --------------------
;; Magit-mode
(global-set-key (kbd "C-x g") 'magit-status)

;; --------------------
;; Dired
(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
  (define-key dired-mode-map (kbd "<tab>") 'dired-subtree-toggle)
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
        (function
         (lambda nil (interactive) (dired-single-buffer "..")))))
;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
        ;; we're good to go; just add our bindings
        (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

(global-set-key (kbd "C-x d") 'dired-single-magic-buffer)
;; (global-set-key [(control f5)] (function
;;         (lambda nil (interactive)
;;         (dired-single-magic-buffer default-directory))))
;; (global-set-key [(shift f5)] (function
;;         (lambda nil (interactive)
;;         (message "Current directory is: %s" default-directory))))
;; (global-set-key [(meta f5)] 'dired-single-toggle-buffer-name)

;; --------------------
;; R & ESS
(defun my-ess-mode-keys ()
  "my personal keybinds for R files."
  (interactive)
  (local-set-key [(shift return)] 'my-ess-eval)
  (local-set-key (kbd "C-c M-c") 'ess-eval-paragraph)
  )
(add-hook 'ess-mode-hook 'my-ess-mode-keys)


(add-hook 'inferior-ess-mode-hook
	  '(lambda()
	     (local-set-key [C-up] 'comint-previous-input)
	     (local-set-key [C-down] 'comint-next-input)
         )
      )

;; --------------------
;; Python
(add-hook 'python-mode-hook '(lambda ()
     (define-key python-mode-map "\C-m" 'newline-and-indent)))
     ;; (local-set-key [(shift return)] "\C-c\C-r")))

;; --------------------
;; Defined in .emacs, to move??
;; (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)      ; navigate options with up/down
