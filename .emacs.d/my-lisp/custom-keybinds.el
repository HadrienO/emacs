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
;; Defined in .emacs, to move??
;; (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)      ; navigate options with up/down
