(setq
 gc-cons-threshold 100000000
 use-package-always-ensure t
 inhibit-startup-screen t
 initial-scratch-message nil
 sentence-end-double-space nil
 ring-bell-function 'ignore
 use-dialog-box nil
 mark-even-if-inactive nil
 kill-whole-line t
 case-fold-search nil
 make-backup-files nil

 ediff-window-setup-function 'ediff-setup-windows-plain
 ediff-split-window-function 'split-window-horizontally
 ediff-merge-split-window-function 'split-window-horizontally

 compilation-scroll-output 'first-error
 compilation-auto-jump-to-first-error t

 recentf-max-saved-items 2000
 indent-tabs-mode nil

 global-auto-revert-non-file-buffers t

 custom-file (locate-user-emacs-file "custom-vars.el")
 bookmark-save-flag 1

 ;; symbolic links to not ask for confirmation
 vc-handled-backends nil
 find-file-visit-truename nil
)

;; Tree-sitter mode remap
(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (c-mode . c-ts-mode)
   (c++-mode . c++-ts-mode)
   (cpp-mode . cpp-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)))

(setq gdb-many-windows t
      gdb-debuginfod-enable nil
      gdb-debuginfod-enable-setting nil)

(setq scroll-step 1
      scroll-conservatively 10000
      auto-window-vscroll nil)

(modify-syntax-entry ?_ "w")

(global-display-line-numbers-mode 1)

(setq-default show-trailing-whitespace t)

(dolist (hook '(vterm-mode-hook
		special-mode-hook
		term-mode-hook
		comint-mode-hook
		compilation-mode-hook
		minibuffer-setup-hook))
  (add-hook hook
	    (lambda () (setq-local show-trailing-whitespace nil))))

(load custom-file 'noerror 'nomessage)

(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(set-fringe-mode 10)

(recentf-mode 1)
(winner-mode 1)
(show-paren-mode 1)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

(set-face-attribute 'default nil :font "Hack-10" :width 'condensed)
;;(set-face-attribute 'default nil :font "RobotoMono-10" :width 'condensed)


(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     '("org" . "https://orgmode.org/elpa/"))

(package-initialize)

(eval-when-compile
  (require 'use-package))

(use-package quelpa
  :ensure)

(use-package quelpa-use-package
  :demand
  :config
  (quelpa-use-package-activate-advice))

;;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;;(add-to-list 'load-path "/home/pekka/.emacs.d/auth-source-xoauth2")
;;(require 'mu4e)
;;(setq user-mail-address "pekka.ervasti@haltian.com"
;;      mu4e-get-mail-command "offlineimap"
;;      mu4e-view-show-addresses t
;;      mu4e-attachment-dir (expand-file-name "~/Downloads/")
;;      )

   ;;; Call the oauth2ms program to fetch the authentication token
(defun fetch-access-token ()
  (with-temp-buffer
    (call-process "oauth2ms" nil t nil "--encode-xoauth2")
    (buffer-string)))

   ;;; Add new authentication method for xoauth2
(cl-defmethod smtpmail-try-auth-method
  (process (_mech (eql xoauth2)) user password)
  (let* ((access-token (fetch-access-token)))
    (smtpmail-command-or-throw
     process
     (concat "AUTH XOAUTH2 " access-token)
     235)))

   ;;; Register the method
(with-eval-after-load 'smtpmail
  (add-to-list 'smtpmail-auth-supported 'xoauth2))

(setq message-send-mail-function   'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.office365.com"
      smtpmail-smtp-server         "smtp.office365.com"
      smtpmail-stream-type  'starttls
      smtpmail-smtp-service 587)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  ;; (setq undo-tree-auto-save-history nil)
  ;; (setq undo-tree-history-directory-alist (concat user-emacs-directory "undo-tree"))
  )

(use-package compat)
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-symbol-word-search t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
  (global-evil-surround-mode 1)
  )

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t)

(defun pe/switch-to-previous-buffer ()
  "Switch to previously visited buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun pe/switch-to-scratch-buffer ()
  "Switch to *scratch* buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun pe/switch-to-messages-buffer ()
  "Switch to *Messages* buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun pe/delete-compilation-window ()
  "Delete all windows showing *compilation* buffer"
  (interactive)
  (delete-windows-on "*compilation*")
  )

;; (defun pe/toggle-tabs-mode ()
;;   "Toggle indent-tabs-mode value between nil and t."
;;   (interactive)
;;   (progn
;;    (if indent-tabs-mode
;;        (setq indent-tabs-mode nil)
;;      (setq indent-tabs-mode t)
;;      )
;;    (message "indent-tabs-mode set to %s" indent-tabs-mode)
;;    )
;;   )

;; (use-package flycheck
;;   :ensure t)

(use-package general
  :ensure t
  :init
  (general-create-definer pe/leader-def
    :keymaps '(dired-mode-map override)
    :states 'motion
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    )
  (pe/leader-def
    "ac" 'calc
    "a" '(:ignore t :which-key "app")
    "am" 'mu4e
    ;; "bd" 'evil-delete-buffer
    "bd" 'kill-current-buffer
    "b" '(:ignore t :which-key "buffer")
    "bm" 'pe/switch-to-messages-buffer
    "bn" 'evil-buffer-new
    "bs" 'pe/switch-to-scratch-buffer
    "cd" 'pe/delete-compilation-window
    "c" '(:ignore t :which-key "compile/comment")
    "ck" 'kill-compilation
    "cl" 'comment-or-uncomment-region
    "e" '(:ignore t :which-key "error")
    "el" 'consult-flymake
    "en" 'flymake-goto-next-error
    "ep" 'flymake-goto-prev-error
    "fc" 'write-file
    "Fd" 'delete-frame
    "fF" 'consult-find
    "ff" 'find-file
    "f" '(:ignore t :which-key "file")
    "F" '(:ignore t :which-key "frame")
    "Fn" 'make-frame-command
    "fo" 'ff-find-other-file
    "fO" 'ff-find-other-file-other-window
    "Fo" 'other-frame
    "fs" 'save-buffer
    "g" '(:ignore t :which-key "git")
    "hh" 'xref-go-back
    "i" '(:ignore t :which-key "insert")
    "ii" 'xref-find-references
    "is" 'yas/insert-snippet
    "ji" 'consult-imenu
    "jI" 'consult-imenu-multi
    "j" '(:ignore t :which-key "jump")
    "jj" 'avy-goto-char-timer
    "js" 'avy-goto-symbol-1
    "mb" 'bookmark-jump
    "md" 'bookmark-delete
    "ml" 'consult-bookmark
    "mL" 'bookmark-bmenu-list
    "mm" 'bookmark-set
    "o" '(:ignore t :which-key "org")
    "q" '(:ignore t :which-key "quit")
    "qq" 'save-buffers-kill-terminal
    "*" 'rg-dwim
    "s" '(:ignore t :which-key "search")
    "SPC" 'execute-extended-command
    "sr" 'consult-ripgrep
    "ss" 'swiper
    "sS" 'swiper-thing-at-point
    ;; :states '(normal visual)
    "TAB" 'pe/switch-to-previous-buffer
    "t" '(:ignore t :which-key "toggles")
    "tl" 'linum-mode
    "tt" 'indent-tabs-mode
    "tw" 'whitespace-mode
    "'" 'vterm-other-window
    "wd" 'delete-window
    "w" '(:ignore t :which-key "window")
    "w" '(:keymap evil-window-map :which-key "window")
    "wm" 'delete-other-windows
    ;; "ws" 'split-window-below
    "wU" 'winner-redo
    "wu" 'winner-undo
    ;; "wv" 'split-window-right
    )
 )

(use-package consult
  :ensure t
  :config
  (pe/leader-def
    ;; :states '(normal visual)
    "bb" 'consult-buffer
    "fr" 'consult-recent-file
    )
  (setq xref-show-xrefs-function 'consult-xref
	xref-show-definitions-function 'consult-xref)
  )

(use-package which-key
  :config
  (which-key-mode 1)
  )

(use-package vertico
  :config
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t
	vertico-sort-function 'vertico-sort-history-alpha)
  (vertico-mode 1))

;; (use-package vertico-repeat
;; 	      :after vertico
;; 	      :ensure nil)

(use-package orderless
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

(use-package embark
  :ensure t
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (general-define-key
   :keymaps 'global
   "C-SPC" 'embark-act
   "S-SPC" 'embark-dwim)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; (use-package ranger
;;   :config
;;   (setq ranger-show-hidden t)
;;   (pe/leader-def
;;     "ar" 'ranger)
;;   (general-define-key
;;    :states 'normal
;;    "-" 'deer))

;; minibuffer input history
(savehist-mode 1)

;; save last place visited
(save-place-mode 1)

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(use-package winum
  :ensure t
  :init
  (defvar-keymap winum-keymap
    :doc "Keymap for winum-mode actions."
    "M-0" 'winum-select-window-0-or-10
    "M-1" 'winum-select-window-1
    "M-2" 'winum-select-window-2
    "M-3" 'winum-select-window-3
    "M-4" 'winum-select-window-4
    "M-5" 'winum-select-window-5
    "M-6" 'winum-select-window-6
    "M-7" 'winum-select-window-7
    "M-8" 'winum-select-window-8
    "M-9" 'winum-select-window-9)
  :config
  (winum-mode 1)
  (pe/leader-def
    ;; :states '(normal visual)
    ;; :keymaps 'override
    "1" 'winum-select-window-1
    "2" 'winum-select-window-2
    "3" 'winum-select-window-3
    "4" 'winum-select-window-4
    "5" 'winum-select-window-5
    "6" 'winum-select-window-6
    "7" 'winum-select-window-7
    "8" 'winum-select-window-8
    "9" 'winum-select-window-9
    )
  )

(use-package magit-section)

(use-package magit
  :ensure t
  :config
  (pe/leader-def
    ;; :states '(normal visual)
    "gs" 'magit-status
    "gb" 'magit-blame
    "gl" 'magit-log-buffer-file))

(use-package magit-section) ;; used by org-roam

(use-package git-timemachine
  :ensure t
  :config
  (pe/leader-def
    ;; :states '(normal visual)
    "gt" 'git-timemachine))

(use-package projectile
  :ensure t
  :config
  ;; (setq projectile-indexing-method 'native)
  (setq projectile-indexing-method 'alien)
  ;; (setq compilation-read-command nil)
  (projectile-mode 1)

  ;; Use the faster searcher to handle project files: ripgrep "rg"
  (setq projectile-generic-command
	 (let ((rg-cmd ""))
	   (dolist (dir '("**/.ccls-cache/**" "**/.repo/**" "**/.cache/**"))
	     (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
	   (setq rg-ignorefile
		 (concat "--ignore-file" " "
			 (expand-file-name "rg_ignore" user-emacs-directory)))
	   (concat "rg -L -0 --files --color=never --hidden" rg-cmd " " rg-ignorefile)))

  (pe/leader-def
    ;; :states '(normal visual)
    "p" '(:keymap projectile-command-map :which-key "projectile")))

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1))

(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode 1))

(use-package ccls)

;; (use-package lsp-mode
;;   :ensure t
;;   :custom (lsp-enable-file-watchers nil)
;;   :config
;;   (setq lsp-enable-on-type-formatting nil)
;;   (setq lsp-enable-indentation nil)
;;   (setq lsp-lens-enable nil)
;;   (add-hook 'c-ts-mode-hook 'lsp)
;;   (add-hook 'c++-ts-mode-hook 'lsp)
;;   (add-hook 'cpp-ts-mode-hook 'lsp)
;;   (add-hook 'python-ts-mode-hook 'lsp)
;;   (add-hook 'javascript-ts-mode-hook 'lsp)
;;   (add-hook 'sh-ts-mode-hook 'lsp)
;;   (setq lsp-enabled-clients '(clangd pyright bash-ls rust-analyzer) ;; ccls
;; 	lsp-semantic-tokens-enable t) ;; ifdef gray outs
;;   (setq lsp-clients-clangd-args
;;     '("--header-insertion=never" "--query-driver=/opt/gcc-arm/bin/arm-none-eabi-gcc")) ;; query-driver for cross compilation headers
;;   (pe/leader-def
;;     "L" '(:keymap lsp-command-map :which-key "lsp")))

;; (use-package lsp-ui)

;; (use-package lsp-pyright
;;   :ensure t
;;   :custom (lsp-pyright-multi-root nil)
;;   :hook (python-mode . (lambda ()
;; 			 (require 'lsp-pyright)
;; 			 (lsp))))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; (use-package ccls
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) .
;;          (lambda () (require 'ccls) (lsp))))

(use-package avy
  :ensure t)

(use-package rg
  :ensure t)

(use-package ag
  :ensure t)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t
	dashboard-items '((recents  . 5)
			  (bookmarks . 5)
			  (projects . 5)
			  (agenda . 5)
			  (registers . 5))
	dashboard-set-heading-icons t
	dashboard-set-file-icons nil))

(use-package counsel)

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
   ([remap describe-function] . counsel-describe-function)
   ([remap describe-command] . helpful-command)
   ([remap describe-variable] . counsel-describe-variable)
   ([remap describe-key] . helpful-key)
   )

(use-package org
  :ensure t
  :config
  (org-indent-mode 1)
  (setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
	 "* TODO %?\n  %i\n  %a")
	("j" "Journal" entry (file+datetree "~/org/journal.org")
	 "* %?\nEntered on %U\n  %i\n  %a")))
  (setq org-clock-idle-time 10)
  (pe/leader-def
    ;; :states '(normal visual)
    ;; Clock
    "oC" '(:ignore t :which-key "clock")
    "oCc" 'org-clock-cancel
    "oCd" 'org-clock-display
    "oCe" 'org-evaluate-time-range
    "oCg" 'org-clock-goto
    "oCi" 'org-clock-in
    "oCI" 'org-clock-in-last
    "oCj" 'spacemacs/org-clock-jump-to-current-clock
    "oCo" 'org-clock-out
    "oCR" 'org-clock-report
    "oCr" 'org-resolve-clocks

    "od" '(:ignore t :which-key "time")
    "odd" 'org-deadline
    "ods" 'org-schedule
    "odt" 'org-time-stamp
    "odT" 'org-time-stamp-inactive

    "oe" '(:ignore t :which-key "export")
    "oee" 'org-export-dispatch

    "of" '(:ignore t :which-key "feed")
    "ofi" 'org-feed-goto-inbox
    "ofu" 'org-feed-update-all

    "oa" 'org-agenda

    "oc" 'org-capture

    "op" 'org-priority

    "oT" '(:ignore t :which-key "toggle/todo")
    "oTc" 'org-toggle-checkbox
    "oTe" 'org-toggle-pretty-entities
    "oTi" 'org-toggle-inline-images
    "oTn" 'org-num-mode
    "oTl" 'org-toggle-link-display
    "oTt" 'org-show-todo-tree
    "oTT" 'org-todo
    "oTV" 'space-doc-mode
    "oTx" 'org-latex-preview

    ;; More cycling options (timestamps, headlines, items, properties)
    "oL" 'org-shiftright
    "oH" 'org-shiftleft
    "oJ" 'org-shiftdown
    "oK" 'org-shiftup

    ;; Change between TODO sets
    "C-S-l" 'org-shiftcontrolright
    "C-S-h" 'org-shiftcontrolleft
    "C-S-j" 'org-shiftcontroldown
    "C-S-k" 'org-shiftcontrolup

    ;; Subtree editing
    "os" '(:ignore t :which-key "subtree")
    "osa" 'org-toggle-archive-tag
    "osA" 'org-archive-subtree-default
    "osb" 'org-tree-to-indirect-buffer
    "osd" 'org-cut-subtree
    "osy" 'org-copy-subtree
    "osh" 'org-promote-subtree
    "osj" 'org-move-subtree-down
    "osk" 'org-move-subtree-up
    "osl" 'org-demote-subtree
    "osn" 'org-narrow-to-subtree
    "osw" 'widen
    "osr" 'org-refile
    "oss" 'org-sparse-tree
    "osS" 'org-sort

    ;; tables
    "ot" '(:ignore t :which-key "table")
    "ota" 'org-table-align
    "otb" 'org-table-blank-field
    "otc" 'org-table-convert
    "otdc" 'org-table-delete-column
    "otdr" 'org-table-kill-row
    "ote" 'org-table-eval-formula
    "otE" 'org-table-export
    "otf" 'org-table-field-info
    "oth" 'org-table-previous-field
    "otH" 'org-table-move-column-left
    "otic" 'org-table-insert-column
    "otih" 'org-table-insert-hline
    "otiH" 'org-table-hline-and-move
    "otir" 'org-table-insert-row
    "otI" 'org-table-import
    "otj" 'org-table-next-row
    "otJ" 'org-table-move-row-down
    "otK" 'org-table-move-row-up
    "otl" 'org-table-next-field
    "otL" 'org-table-move-column-right
    "otn" 'org-table-create
    "otN" 'org-table-create-with-table.el
    "otr" 'org-table-recalculate
    "otR" 'org-table-recalculate-buffer-tables
    "ots" 'org-table-sort-lines
    "ottf" 'org-table-toggle-formula-debugger
    "otto" 'org-table-toggle-coordinate-overlays
    "otw" 'org-table-wrap-region

    ;; Source blocks / org-babel
    "ob" '(:ignore t :which-key "babel")
    "obp" 'org-babel-previous-src-block
    "obn" 'org-babel-next-src-block
    "obe" 'org-babel-execute-maybe
    "obo" 'org-babel-open-src-block-result
    "obv" 'org-babel-expand-src-block
    "obu" 'org-babel-goto-src-block-head
    "obg" 'org-babel-goto-named-src-block
    "obr" 'org-babel-goto-named-result
    "obb" 'org-babel-execute-buffer
    "obs" 'org-babel-execute-subtree
    "obd" 'org-babel-demarcate-block
    "obt" 'org-babel-tangle
    "obf" 'org-babel-tangle-file
    "obc" 'org-babel-check-src-block
    "obj" 'org-babel-insert-header-arg
    "obl" 'org-babel-load-in-session
    "obi" 'org-babel-lob-ingest
    "obI" 'org-babel-view-src-block-info
    "obz" 'org-babel-switch-to-session
    "obZ" 'org-babel-switch-to-session-with-code
    "oba" 'org-babel-sha1-hash
    "obx" 'org-babel-do-key-sequence-in-edit-buffer
    "ob." 'spacemacs/org-babel-transient-state/body
    ;; Multi-purpose keys
    ;; (or dotspacemacs-major-mode-leader-key ",") 'org-ctrl-c-ctrl-c
    "o*" 'org-ctrl-c-star
    "o-" 'org-ctrl-c-minus
    "o#" 'org-update-statistics-cookies
    "o RET"   'org-ctrl-c-ret
    "o M-RET" 'org-meta-return
    ;; attachments
    "oA" 'org-attach
    ;; insertion
    "oi" '(:ignore t :which-key "insertion")
    "oib" 'org-insert-structure-template
    "oid" 'org-insert-drawer
    "oie" 'org-set-effort
    "oif" 'org-footnote-new
    "oih" 'org-insert-heading
    "oiH" 'org-insert-heading-after-current
    "oii" 'org-insert-item
    "oiK" 'spacemacs/insert-keybinding-org
    "oil" 'org-insert-link
    "oin" 'org-add-note
    "oip" 'org-set-property
    "ois" 'org-insert-subheading
    "oit" 'org-set-tags-command
    ;; region manipulation
    ;; "xb" (spacemacs|org-emphasize spacemacs/org-bold ?*)
    ;; "xc" (spacemacs|org-emphasize spacemacs/org-code ?~)
    ;; "xi" (spacemacs|org-emphasize spacemacs/org-italic ?/)
    ;; "xo" 'org-open-at-point
    ;; "xr" (spacemacs|org-emphasize spacemacs/org-clear ? )
    ;; "xs" (spacemacs|org-emphasize spacemacs/org-strike-through ?+)
    ;; "xu" (spacemacs|org-emphasize spacemacs/org-underline ?_)
    ;; "xv" (spacemacs|org-emphasize spacemacs/org-verbatim ?=))

    "or" '(:ignore t :which-key "roam")
    "orl" 'org-roam-buffer-toggle
    "orf" 'org-roam-node-find
    "org" 'org-roam-graph
    "ori" 'org-roam-node-insert
    "orc" 'org-roam-capture
    "orj" 'org-roam-dailies-capture-today
    )
  )

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org-roam"))
  :init
  (setq org-roam-v2-ack t
	org-return-follows-link  t)
  :config
  (setq org-startup-indented t)
  (org-roam-db-autosync-mode))

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode))

;; (use-package org-bullets
;;   :ensure t
;;   :hook (org-mode . org-bullets-mode))

;; (use-package org-modern
;;   :ensure t
;;   :hook (org-mode . org-modern-mode)
;;   :hook (org-agenda-finalize . org-modern-agenda))

;; temporary fix for https://github.com/Somelauw/evil-org-mode/issues/93
;; (fset 'evil-redirect-digit-argument 'ignore)
;; (add-to-list 'evil-digit-bound-motions 'evil-org-beginning-of-line)
;; (evil-define-key 'motion 'evil-org-mode
;;   (kbd "0") 'evil-org-beginning-of-line)

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(org-babel-do-load-languages 'org-babel-load-languages
			     '(
			       (shell . t)
			       (C . t)
			       (python . t)
			       ;; (restclient . t)
			       ;; (verb . t)
			       ))
(setq org-confirm-babel-evaluate nil)

(use-package perspective
  :config
  (pe/leader-def
    ;; :states '(normal visual)
    "l" '(:keymap perspective-map :which-key "persp")
    "ll" 'xref-find-definitions
    "lL" 'xref-find-definitions-other-window)
  (setq persp-suppress-no-prefix-key-warning t
	persp-initial-frame-name "1")
  (persp-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t)

(use-package plantuml-mode
  :ensure t
  :config
  (setq plantuml-jar-path "/home/pekka/tools/plantuml/plantuml.jar"
	plantuml-default-exec-mode 'jar
	plantuml-output-type "png"))

(use-package chronos
  :ensure t
  :config
  (pe/leader-def
    ;; :states '(normal visual)
    "ac" '(:keymap chronos-mode-map :which-key "chronos")
    )
  )

(use-package erc
  :ensure t
  :config
  (setq erc-nick "poko"
	erc-user-full-name "pokomaister"
	erc-track-shorten-start 8
	erc-hide-list '("JOIN" "PART" "QUIT")
	;; erc-autojoin-channels-alist '(("#systemcrafters"))
	))

(use-package dtrt-indent
  :ensure t)

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
	'(("https://www.hs.fi/rss/tuoreimmat.xml" news hs)
	  ("https://sachachua.com/blog/feed/" emacs)
	  ("https://www.iltalehti.fi/rss/uutiset.xml" news il)
	  ("https://www.is.fi/rss/tuoreimmat.xml" news is)
	  ("https://www.kaleva.fi/rss" uutiset kaleva)
	  ("http://feeds.arstechnica.com/arstechnica/technology-lab" tech ars)
	  ("https://lwn.net/headlines/rss" tech lwn)
	  ))
  (pe/leader-def
    "ae" 'elfeed))

(use-package vterm
  :ensure t)

(display-time-mode 1)

(use-package lispyville
  :init
  ;; (general-add-hook '(emacs-lisp-mode-hook lisp-mode-hook) #'lispyville-mode)
  (add-hook 'lispy-mode-hook #'lispyville-mode)
  :config
  (lispyville-set-key-theme '(operators c-w additional atom-motions commentary slurp/barf-lispy)))

(use-package lispy
  :ensure t)

;; Flutter / Dart

(use-package dart-mode)
(use-package lsp-dart
  :after dart-mode
  :config
  (setq lsp-dart-flutter-sdk-dir "/home/pekka/snap/flutter/common/flutter"))

(use-package lsp-treemacs)
(use-package hover)

(use-package popup :ensure t)
(use-package google-translate :ensure t)

(use-package treemacs
  :ensure t
  :config
  (pe/leader-def
    ;; :states '(normal visual)
    "ft" 'treemacs))

(use-package treemacs-evil)

(use-package keycast
  :ensure t
  :config
  ;; This works with doom-modeline, inspired by this comment:
  ;; https://github.com/tarsius/keycast/issues/7#issuecomment-627604064
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
	(add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update)))
  (add-to-list 'global-mode-string '("" mode-line-keycast " "))
  (keycast-mode))

(use-package pdf-tools :ensure t :config (setq revert-without-query '(".pdf")))
(use-package sudo-edit :ensure t)

(add-to-list 'load-path "~/.my-user-config")
(require 'my-user-config)
(require 'my-erc-sasl-config)

(add-hook 'edebug-mode-hook 'evil-normalize-keymaps)
(add-hook 'dart-mode-hook 'lsp)

;; EAF
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
;; (require 'eaf)
;; (require 'eaf-pdf-viewer)

;; EAF evil
(define-key key-translation-map (kbd "SPC")
    (lambda (prompt)
      (if (derived-mode-p 'eaf-mode)
          (pcase eaf--buffer-app-name
            ("browser" (if  eaf-buffer-input-focus
                           (kbd "SPC")
                         (kbd eaf-evil-leader-key)))
            ("pdf-viewer" (kbd eaf-evil-leader-key))
            ("image-viewer" (kbd eaf-evil-leader-key))
            (_  (kbd "SPC")))
        (kbd "SPC"))))

(use-package ztree
  :ensure t)

(use-package git-gutter-fringe
  :ensure t
  :config
  (pe/leader-def
    ;; :states '(normal visual)
    "gn" 'git-gutter:next-hunk
    "gp" 'git-gutter:previous-hunk
    )
  (global-git-gutter-mode 1))

(use-package evil-mc
  :ensure t
  :config
  (global-evil-mc-mode 1))

(use-package drag-stuff :ensure t
  :config
  (general-define-key
    "M-k" 'drag-stuff-up
    "M-j" 'drag-stuff-down))

(use-package atomic-chrome
  :config
  (atomic-chrome-start-server))

(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode and related modes
  ;; (indent-tabs-mode -1)
  (dtrt-indent-mode 1)
  (modify-syntax-entry ?_ "w" c-ts-mode-syntax-table)
  (modify-syntax-entry ?_ "w" c++-ts-mode-syntax-table)
  )

(add-hook 'c-ts-mode-hook 'my-c-mode-common-hook)
(add-hook 'c++-ts-mode-hook 'my-c-mode-common-hook)
(org-indent-mode 1)

;; (add-to-list 'load-path "~/.emacs.d/vpe/")
;; (require 'projectile-transient-menu)

(use-package docker
  :ensure t
  :config
  (pe/leader-def
   ;; :states '(normal visual)
   "do" 'docker)
  )

(defun pe/recompile-all-packages ()
    "Force re-compile all packages."
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force)
  )

(pe/leader-def
  ;; :states '(normal visual)
  "ca" 'pe/recompile-all-packages
  "ua" 'package-upgrade-all)

(use-package logview
  :config
  (pe/leader-def
    ;; :states '(normal visual)
    "lv" '(:keymap logview-mode-map :which-key "logview")
    )
  (setq auto-revert-verbose nil) ;; do not spam when live reverting log buffers
  (setq
	logview-additional-level-mappings
        '(("WP SDK LOG"
           (error       "E:")
	   (warning     "W:")
	   (information "I:")
	   (debug       "D:")
	   ))

        logview-additional-timestamp-formats
        '(("10ms counter"
           (regexp . "\\w+")
           ;; (regexp . "\[.*\]\[\([0-9]+\)\]")
           (datetime-options :any-decimal-separator t)
           ))

	logview-additional-submodes
	'(("WP SDK LOG"
	   ;; (format . "TIMESTAMP LEVEL MESSAGE")
	   (format . "THREAD LEVEL MESSAGE")
	   (levels . "WP SDK LOG")
	   ;; (timestamp "10ms counter")
	   )
	  ("Dummy"
	   (format . "MESSAGE")
	   ))
	))

;; (defun pe/logview-activate-map ()
;;     ""
;;   (setq overriding-local-map logview-mode-map)
;;   )

;; (add-hook 'logview-mode-hook 'pe/logview-activate-map)

(setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
	(c . ("https://github.com/tree-sitter/tree-sitter-c"))
	(cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
	(cmake . ("https://github.com/uyha/tree-sitter-cmake"))
	(css . ("https://github.com/tree-sitter/tree-sitter-css"))
	(go . ("https://github.com/tree-sitter/tree-sitter-go"))
	(html . ("https://github.com/tree-sitter/tree-sitter-html"))
	(javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
	(json . ("https://github.com/tree-sitter/tree-sitter-json"))
	(lua . ("https://github.com/Azganoth/tree-sitter-lua"))
	(make . ("https://github.com/alemuller/tree-sitter-make"))
	(ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" "ocaml/src" "ocaml"))
	(python . ("https://github.com/tree-sitter/tree-sitter-python"))
	(php . ("https://github.com/tree-sitter/tree-sitter-php"))
	(typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "typescript/src" "typescript"))
	(ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
	(rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
	(sql . ("https://github.com/m-novikov/tree-sitter-sql"))
	(toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
	(zig . ("https://github.com/GrayJack/tree-sitter-zig"))))

(defun nf/treesit-install-all-languages ()
  "Install all languages specified by `treesit-language-source-alist'."
  (interactive)
  (let ((languages (mapcar 'car treesit-language-source-alist)))
    (dolist (lang languages)
      (treesit-install-language-grammar lang)
      (message "`%s' parser was installed." lang)
      (sit-for 0.75))))

;; Simple gerrit support for magit
(transient-define-suffix magit-push-current-for-gerrit-review (args)
  "Push the current branch to its push-remote for gerrit review.

When the push-remote is not configured, then read the push-remote
from the user, set it, and then push to it.  With a prefix
argument the push-remote can be changed before pushed to it."
  :if #'magit-get-current-branch
  :description #'magit-push--pushbranch-for-review-description
  (interactive (list (magit-push-arguments)))
  (pcase-let ((`(,branch ,remote ,changed)
               (magit--select-push-remote "push there")))
    (when changed
      (magit-confirm 'set-and-push
        (string-replace
         "%" "%%"
         (format "Really use \"%s\" as push-remote and push \"%s\" there"
                 remote branch))))
    (run-hooks 'magit-credential-hook)
    (magit-run-git-async "push" "-v" args remote
                         (format "refs/heads/%s:refs/for/%s"
                                 branch branch))))

(defun magit-push--pushbranch-for-review-description ()
  (format "gerrit review for %s" (magit-push--pushbranch-description)))

(transient-append-suffix 'magit-push "p"
  '("g" magit-push-current-for-gerrit-review))

(use-package repo
  :ensure t
  :config
  (pe/leader-def
    "R" 'repo-status)
  (general-def repo-mode-map
    "j" #'next-line
    "k" #'previous-line))


(use-package yaml-mode :ensure t)

(use-package rustic
  :ensure
  ;; :bind (:map rustic-mode-map
  ;;             ("M-j" . lsp-ui-imenu)
  ;;             ("M-?" . lsp-find-references)
  ;;             ("C-c C-c l" . flycheck-list-errors)
  ;;             ("C-c C-c a" . lsp-execute-code-action)
  ;;             ("C-c C-c r" . lsp-rename)
  ;;             ("C-c C-c q" . lsp-workspace-restart)
  ;;             ("C-c C-c Q" . lsp-workspace-shutdown)
  ;;             ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  )

(use-package eat)
(use-package zeal-at-point
  :config
  (pe/leader-def
    "z" '(:ignore t :which-key "zeal")
    "zp" 'zeal-at-point))

;; (use-package ob-restclient)
(use-package restclient)
(use-package verb
  :ensure t
  :init
  (pe/leader-def
      "ov" '(:keymap verb-command-map :which-key "verb")))

;; (use-package wptool
;;   :config
;;   (pe/leader-def
;;     "a" '(:ignore t :which-key "app")
;;     "aw" 'wptool-dispatch))

(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

(use-package devdocs
  :ensure t)

;; (require 'dap-gdb-lldb)
;; (add-hook 'dap-stopped-hook
;;           (lambda (arg) (call-interactively #'dap-hydra)))

(use-package dape
  :ensure t
  ;; :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  ;; :hook
  ;; Save breakpoints on quit
  ;; ((kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  ;;  (after-init . dape-breakpoint-load))

  :init
  ;; To use window configuration like gud (gdb-mi)
  (setq dape-buffer-window-arrangement 'gud)

  :config
  ;; Info buffers to the right
  (setq dape-buffer-window-arrangement 'right)

  ;; Global bindings for setting breakpoints with mouse
  ;; (dape-breakpoint-global-mode)

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-info)
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  (add-hook 'dape-on-stopped-hooks 'dape-info)
  (add-hook 'dape-on-stopped-hooks 'dape-repl)

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-on-start-hooks (lambda () (save-some-buffers t t)))

  ;; Projectile users
  (setq dape-cwd-fn 'projectile-project-root)
  (pe/leader-def
    "dd" 'dape)
  )

;; Github Copilot

(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "zerolfx/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el"))
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-mode-map (kbd "C-<tab>") 'copilot-accept-completion)
  )

(use-package consult-org-roam
   :ensure t
   :after org-roam
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key "M-.")
   :bind
   ;; Define some convenient keybindings as an addition
   ("C-c n e" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n l" . consult-org-roam-forward-links)
   ("C-c n r" . consult-org-roam-search))

(use-package proced
  :ensure t
  :defer t
  :custom
  (proced-enable-color-flag t)
  (proced-tree-flag t)
  :config
  (pe/leader-def
    "ip" 'proced))

(use-package lorem-ipsum
  :ensure t
  :config
  (pe/leader-def
    "lI" 'lorem-ipsum-insert-paragraphs))

(use-package pyenv-mode
  :ensure t
  :config
  (pyenv-mode))

(use-package pyvenv
  :ensure t)

(use-package tmr
  :ensure t
  :config
  (pe/leader-def
    "at" 'tmr))

(use-package dirvish
  :ensure t
  :config
  ;; Don't worry, Dirvish is still performant even if you enable all these attributes
  (setq dirvish-attributes
	'(vc-state subtree-state all-the-icons collapse file-time file-size))
  (dirvish-override-dired-mode)
  (general-define-key
   :states 'normal
   "-" 'dirvish)
  (evil-collection-define-key 'normal 'dired-mode-map
    "q" 'dirvish-quit
    "h" 'dired-up-directory
    "l" 'dired-find-file
    " " nil ;; unbind SPC in dired-mode-map
    )
  )

(use-package all-the-icons
  :ensure t)

(use-package clang-format
  :ensure t
  :config
  (pe/leader-def
    "cf" 'clang-format-buffer))

(use-package octave
  :ensure t)
