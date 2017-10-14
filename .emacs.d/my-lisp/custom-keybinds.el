;; -------------------------------------------------------------
;; --------------    Custom Keybinds for Emacs    --------------
;; -------------------------------------------------------------
;; 
;;    ----- Global -----
;;  C-x g			magit-status
;;  C-x <arrow>		windmove-up/down/right/left
;;  mouse-3			browse-url-at-mouse
;;
;;
;;
;;   ----- Org-mode -----
;;  C-c l			org-store-link
;;  C-c a			org-agenda
;;
;;   ----- ws-mode -----
;;	C-c s w			whitespace-mode
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
;; Org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; --------------------
;; ws-mode
(global-set-key "\C-csw" 'whitespace-mode)

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

(global-set-key (kbd "C-x C-M-d") 'dired-single-magic-buffer)
(global-set-key [(control f5)] (function
        (lambda nil (interactive)
        (dired-single-magic-buffer default-directory))))
(global-set-key [(shift f5)] (function
        (lambda nil (interactive)
        (message "Current directory is: %s" default-directory))))
(global-set-key [(meta f5)] 'dired-single-toggle-buffer-name)


;; --------------------
;; Defined in .emacs, to move??
;; (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)      ; navigate options with up/down
