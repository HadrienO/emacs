;; ========================================================================== ;;
;;                            Custom Keybinds File                            ;;
;;        ------------------------------------------------------------        ;;
;; Here are defined all my keybinds for easy lookup and management            ;;
;;                                                                            ;;
;;    ----- Unbinding -----                                                   ;;
;;  C-z             `suspend-frame' (also bound to C-x C-z)                   ;;
;;  C-x C-u         `upcase-region'                                           ;;
;;                                                                            ;;
;;    ----- Global -----                                                      ;;
;;  C-x <arrow>     `windmove-up'/down/right/left                             ;;
;;  mouse-3         `browse-url-at-mouse'                                     ;;
;;  M-g M-c         `my-go-to-column'                                         ;;
;;  C-x w           `my-paste-function'                                       ;;
;;  <f5>            `my-revert-buffer-no-confirm'                             ;;
;;                                                                            ;;
;;  C-x g           `magit-status'                                            ;;
;;  C-x d           `dired-single-magic-buffer' (repl: `ido-list-directory')  ;;
;;                                                                            ;;
;;  C-c l           `org-store-link'                                          ;;
;;  C-c a           `org-agenda'                                              ;;
;;                                                                            ;;
;;  C-c s w         `whitespace-mode'                                         ;;
;;  C-c s c         `rainbow-mode'                                            ;;
;;                                                                            ;;
;;    ----- Local  -----                                                      ;;
;; ** outline-minor-mode **                                                   ;;
;;  C-<tab>         `outline-mode-toggle-children'                            ;;
;;  C-c C-<tab>     `outline-mode' prefix command                             ;;
;;                                                                            ;;
;; ** dired-mode **                                                           ;;
;;  <tab>           `dired-subtree-toggle'                                    ;;
;;  <return>        `dired-single-buffer'                                     ;;
;;  mouse-1         `dired-single-buffer-mouse'                               ;;
;;  ^               (dired-single-buffer "..")                                ;;
;;                                                                            ;;
;; ** r/ess-mode **                                                           ;;
;;  S-<return>      `my-ess-eval'                                             ;;
;;  C-c M-c         `ess-eval-paragraph'                                      ;;
;;  C-<up>          `comint-previous-input' (in R terminal)                   ;;
;;  C-<down>        `comint-next-input' (in R terminal)                       ;;
;;                                                                            ;;
;; ** python **                                                               ;;
;;  C-m             `newline-and-indent'                                      ;;
;;  S-<return>      `my-python-send-region'                                   ;;
;;  C-c C-c         `my-compile'                                              ;;
;;           ------------------------------------------------------           ;;
;;                                                                            ;;
;; Note: (global-set-key ...) == (define-key (current-global-map) ...)        ;;
;;       (local-set-key ...)  == (define-key (current-local-map) ...)         ;;
;;                                                                            ;;
;; TODO:                                                                      ;;
;; - keybind to switch  `org-cycle-include-plain-lists'                       ;;
;;                                                                            ;;
;; ========================================================================== ;;

;; --------------------
;; Unbinding
(global-unset-key (kbd "C-z")) ; suspend-frame (minimize) 
(global-unset-key (kbd "C-x C-u")) ;upcase-region interfere with `undo',
                                   ;but I should use "C-_" for that         

;; --------------------
;; General keybinds
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

(global-set-key (kbd "M-g M-c") 'my-go-to-column)
(global-set-key [down-mouse-3] 'browse-url-at-mouse)   ; Make URL clickable

(global-set-key "\C-x\w" 'my-paste-function)

(global-set-key (kbd "<f5>") 'my-revert-buffer-no-confirm)

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
  "Personal keybinds for R files."
  (interactive)
  (local-set-key [(shift return)] 'my-ess-eval)
  (local-set-key (kbd "C-c M-c") 'ess-eval-paragraph)
  )
(add-hook 'ess-mode-hook 'my-ess-mode-keys)

(add-hook 'ess-post-run-hook
          '(lambda()
             (ess-execute-screen-options)
             (local-set-key "\C-c\w" 'ess-execute-screen-options)
             ))
(add-hook 'inferior-ess-mode-hook
          '(lambda()
             (local-set-key [C-up] 'comint-previous-input)
             (local-set-key [C-down] 'comint-next-input)
             ))

;; --------------------
;; Python
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map "\C-m" 'newline-and-indent)
             (local-set-key [(shift return)] 'my-python-send-region)
             (local-set-key "\C-c\C-c" 'my-compile)
             ))

;; --------------------
;; LaTeX-mode
;; (add-hook 'LaTeX-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-c C-e") 'latex-close-block)
;;             ))

;; --------------------
;; Defined in .emacs, to move??
;; (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)      ; navigate options with up/down
