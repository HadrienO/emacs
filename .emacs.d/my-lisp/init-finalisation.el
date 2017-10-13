  

;; (add-hook 'desktop-after-read-hook
;;           (lambda()
;;             (setq w1 (window-at 0 0))
;;             (cond ((= (count-windows) 1)
;;                    (setq w2 (split-window w1 nil t))
;;                    (set-window-buffer (split-window w2 t nil) "*Messages*")
;;                    )
;;                   ((= (count-windows) 2)
;;                    (setq w2 (window-at (frame-width) 0))
;;                    (split-window w2 t nil)
;;                    )
;;                   (t
;;                    (setq w2 (window-at (frame-width) 0))
;;                    )
;;                   )
;;             (select-window w2)
;;             (find-file my-file-to-display-at-startup)
;;             (message "---- Window list: `%s'" (window-list))
;;             (message "---- Loaded file: `%s'" (buffer-name))
;;             (select-window w1)
;;             )
;;           )


(add-hook 'desktop-after-read-hook
          (lambda()
            (setq w1 (window-at 0 0))
            (cond ((= (count-windows) 1)
                   (setq w2 (split-window w1 nil t))
                   (set-window-buffer (split-window w2 t nil)
                                      "*Messages*")
                   )
                  ((= (count-windows) 2)
                   (message "!!!!! width: `%s'" (frame-width))
                   (setq w2 (window-at (frame-width) 0))
                   (message "!!!!! width2: `%s'" (frame-width))
                   (split-window w2 t nil)
                   )
                  (t
                   (message "!!!!! width: `%s'" (frame-width))
                   (setq w2 (window-at (frame-width) 0))
                   (message "!!!!! width2: `%s'" (frame-width))
                   )
                  )
            (select-window w2)
            (find-file my-file-to-display-at-startup)
            (message "---- Window list: `%s'" (window-list))
            (message "---- Loaded file: `%s'" (buffer-name))
            (select-window w1)
            )
)


(desktop-save-mode 1)



      
;; --- trying to get window layout to open 'my-file-to-display-at-startup'
;; in appropriate window based on desktop layout at startup
;;
;; (setq my-window-list-at-startup (window-list)
;;       my-window-count-at-startup (length my-window-list-at-startup)
;;       my-buffer-list-at-startup (buffer-list)
;;       my-buffer-visible-at-startup (mapcar #'window-buffer (window-list))
;;       )
;; (if (> (length my-window-list-at-startup)
;;        2)
;;     (progn
;;       ;; select the most top-right window
;;       (mapcar #'window-edges (window-list))  ; (left top right bottom)
;;       (window-state-get)
;;
;;     ;;   (select-window (elt my-window-list-at-startup
;;     ;;                         (- my-window-count-at-startup 1)
;;     ;; (set-window-buffer (elt my-window-list-at-startup
;;     ;;                         (- my-window-count-at-startup 1)
;;     ;;                         "emacs_shortcuts.org")
;;       )
;;   ("~/")
;;   ( "~/documents/notes/org/emacs_shortcuts.org")
;;  )
