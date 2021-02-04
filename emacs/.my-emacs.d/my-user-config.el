;; GDB settings for projects

(defun my-gdb-start (initfile binary)
  "my-gdb-start"
  (gdb (concat "arm-none-eabi-gdb-py -i=mi -x " initfile " " binary))
  )

(defun my-gdb-start-evt ()
  "my-gdb-start-evt"
  (interactive)
  (my-gdb-start "/home/pekka/work/ee/evt.gdb"
                "/home/pekka/work/ee/cube_projects/eagle/build/lightning-stm32h7-firmware.elf")
  )

(defun my-gdb-start-bootloader ()
  "my-gdb-start-bootloader"
  (interactive)
  (my-gdb-start "/home/pekka/work/ee/evt.gdb"
                "/home/pekka/work/ee/cube_projects/bootloader/build/lightning-stm32h7-bootloader.elf")
  )

(defun my-gdb-start-tac ()
  "my-gdb-start-tac"
  (interactive)
  (my-gdb-start "/home/pekka/work/ee/evt.gdb"
                "/home/pekka/work/ee/tac/eddie-eagle-tac-fw/build/tac.elf")
  )

(defun my-gdb-start-tracepen ()
  "my-gdb-start-tracepen"
  (interactive)
  (my-gdb-start "/home/pekka/work/TracePen/cube/Tracepen/init.gdb"
                "/home/pekka/work/TracePen/cube/Tracepen/build/Tracepen.elf")
  )

(spacemacs/declare-prefix "o" "other")

(spacemacs/set-leader-keys
  "o1" 'my-gdb-start-evt
  "o2" 'my-gdb-start-bootloader
  "o3" 'my-gdb-start-tac
  "o4" 'my-gdb-start-tracepen
  "o5" 'my-gdb-start-yard
  "oo" 'rtags-find-symbol-at-point
  )

(setq ob-mermaid-cli-path "/home/pekka/bin/node_modules/.bin/mmdc")
(setq markdown-command "/usr/bin/pandoc -F mermaid-filter")

(setq mouse-wheel-scroll-amount '(1))
(setq mouse-wheel-progressive-speed t)
(setq ring-bell-function 'ignore)

;; (with-eval-after-load 'org
;;   (setq org-agenda-files (quote ("~/todo.org")))
;;   ;; (setq org-duration-format '(("d" . nil) ("h" . t) ("min" . t)))
;;   (setq org-effort-durations
;;         `(("min" . 1)
;;           ("h" . 60)
;;           ;; eight-hour days
;;           ("d" . ,(* 60 8))
;;           ;; five-day work week
;;           ("w" . ,(* 60 8 5))
;;           ;; four weeks in a month
;;           ("m" . ,(* 60 8 5 4))
;;           ;; work a total of 12 months a year --
;;           ;; this is independent of holiday and sick time taken
;;           ("y" . ,(* 60 8 5 4 12))))
;;   )

;; (with-eval-after-load 'org-duration
;;   (setq org-duration-units
;;         `(("min" . 1)
;;           ("h" . 60)
;;           ;; eight-hour days
;;           ("d" . ,(* 60 8))
;;           ;; five-day work week
;;           ("w" . ,(* 60 8 5))
;;           ;; four weeks in a month
;;           ("m" . ,(* 60 8 5 4))
;;           ;; work a total of 12 months a year --
;;           ;; this is independent of holiday and sick time taken
;;           ("y" . ,(* 60 8 5 4 12))))
;;   )

;; org-mode

(setq org-capture-templates
      '(
        ("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks inbox")
         "* TODO %?\n  %i\n  %a")

        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a"))

      org-agenda-files (quote ("~/org/todo.org"))
      )

;; _ considered part of a word
;; For python
(add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;; For ruby
(add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;; For Javascript
(add-hook 'js2-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;; For C/C++
(add-hook 'c-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

(defun my-indent-style (name style offset width tab-mode)
  (progn
    (c-set-style style)
    (setq
     c-basic-offset offset
     tab-width width
     indent-tabs-mode tab-mode
     )
    (message "Indent style for %s" name)
    )
  )
(defun my-indent-select (number)
  "Select predefined C indenting style"
  (interactive "nIndent style: 0=kernel 1=nuttx 2=wp: ")
  (cond
   ((eq number 0) (my-indent-style "kernel" "linux" 8 8 t))
   ((eq number 1) (my-indent-style "nuttx" "bsd" 2 2 nil))
   ((eq number 2) (my-indent-style "wp" "bsd" 4 4 nil))
   )
  )

(add-hook 'rtags-jump-hook 'evil-set-jump)
(load "~/.mu4e")
(setq history-delete-duplicates t)

;; gerrit

(setq
 gerrit-host "gerrit.in.haltian.com:8443"
 gerrit-use-gitreview-interface nil
 )
(progn
  ;; (add-hook 'magit-status-sections-hook #'gerrit-magit-insert-status t)
  (global-set-key (kbd "C-x i") 'gerrit-upload-transient)
  (global-set-key (kbd "C-x o") 'gerrit-download))
