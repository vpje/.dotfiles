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
 recentf-max-saved-items 200
 indent-tabs-mode nil
 )

(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)

(recentf-mode 1)
(winner-mode 1)
(show-paren-mode 1)

(set-face-attribute 'default nil :font "Hack-10" :width 'condensed)
;;(set-face-attribute 'default nil :font "RobotoMono-10" :width 'condensed)

(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/use-package")
  (require 'use-package))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     '("org" . "https://orgmode.org/elpa/"))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(add-to-list 'load-path "/home/pekka/.emacs.d/auth-source-xoauth2")
;; (require 'mu4e)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  )

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-symbol-word-search t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
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

(defun pe/toggle-tabs-mode ()
  "Toggle indent-tabs-mode value between nil and t."
  (interactive)
  (progn
   (if indent-tabs-mode
       (setq indent-tabs-mode nil)
     (setq indent-tabs-mode t)
     )
   (message "indent-tabs-mode set to %s" indent-tabs-mode)
   )
  )

(use-package general
  :ensure t
  :init
  (general-create-definer pe/leader-def
    :states '(normal insert emacs visual)
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    )
  (pe/leader-def
    :states '(normal insert emacs visual)
    "a" '(:ignore t :which-key "app")
    "am" 'mu4e
    "bd" 'evil-delete-buffer
    "b" '(:ignore t :which-key "buffer")
    "bm" 'pe/switch-to-messages-buffer
    "bn" 'evil-buffer-new
    "bs" 'pe/switch-to-scratch-buffer
    "c" '(:ignore t :which-key "compile/comment")
    "ck" 'kill-compilation
    "cl" 'comment-or-uncomment-region
    "cd" 'pe/delete-compilation-window
    "e" '(:ignore t :which-key "error")
    "el" 'flymake-show-diagnostics-buffer
    "en" 'flymake-goto-next-error
    "ep" 'flymake-goto-prev-error
    "'" 'vterm-other-window
    "fF" 'consult-find
    "ff" 'find-file
    "f" '(:ignore t :which-key "file")
    "fs" 'save-buffer
    "g" '(:ignore t :which-key "git")
    "j" '(:ignore t :which-key "jump")
    "ji" 'consult-imenu
    "jI" 'consult-imenu-multi
    "o" '(:ignore t :which-key "org")
    "q" '(:ignore t :which-key "quit")
    "qq" 'save-buffers-kill-terminal
    "s" '(:ignore t :which-key "search")
    "SPC" 'execute-extended-command
    "sr" 'consult-ripgrep
    "ss" 'swiper
    "sS" 'swiper-thing-at-point
    "t" '(:ignore t :which-key "toggles")
    "tw" 'whitespace-mode
    "tl" 'linum-mode
    "tt" 'pe/toggle-tabs-mode
    "TAB" 'pe/switch-to-previous-buffer
    "wd" 'delete-window
    "w" '(:keymap evil-window-map :which-key "window")
    "w" '(:ignore t :which-key "window")
    "wm" 'delete-other-windows
    "wU" 'winner-redo
    "wu" 'winner-undo
    "*" 'rg-dwim
    ;; "ws" 'split-window-below
    ;; "wv" 'split-window-right
    )
  (general-define-key
   :states 'normal
   "M-." 'lsp-find-definition)
  )

(use-package consult
  :ensure t
  :config
  (pe/leader-def
    :states '(normal visual)
    "bb" 'consult-buffer
    "fr" 'consult-recent-file
    )
  (setq xref-show-xrefs-function 'consult-xref
	xref-show-definitions-function 'consult-xref)
  )

(use-package which-key
  :config
  (which-key-mode 1))

(use-package vertico
  :config
  (vertico-mode 1))

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

(use-package ranger
  :config
  (pe/leader-def
    :states '(normal visual)
    "ar" 'ranger)
  (general-define-key
   :states 'normal
   :keymaps 'override
   "-" 'deer))

(savehist-mode 1)

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(use-package winum
  :ensure t
  :config
  (winum-mode 1)
  (pe/leader-def
    :states '(normal visual insert emacs)
    :keymaps 'override
    "1" 'winum-select-window-1
    "2" 'winum-select-window-2
    "3" 'winum-select-window-3
    "4" 'winum-select-window-4
    "5" 'winum-select-window-5
    "6" 'winum-select-window-6
    "7" 'winum-select-window-7
    "8" 'winum-select-window-8
    "9" 'winum-select-window-9
    ))

(use-package magit
  :ensure t
  :config
  (pe/leader-def
    :states '(normal visual)
    "gs" 'magit-status))

(use-package projectile
  :ensure t
  :config
  (setq projectile-globally-ignored-directories '(".cache"))
  (projectile-mode 1)
  (pe/leader-def
    :states '(normal visual)
    "p" '(:keymap projectile-command-map :which-key "projectile")))

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1))

(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode 1))

(use-package lsp-mode
  :ensure t
  :config
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)
  (add-hook 'python-mode-hook 'lsp)
  (pe/leader-def
    :states '(normal visual)
    "L" '(:keymap lsp-command-map :which-key "lsp")))

(use-package lsp-ui)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp)))) 

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

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
  (pe/leader-def
    :states '(normal visual)
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

    "oee" 'org-export-dispatch
    "ofi" 'org-feed-goto-inbox
    "ofu" 'org-feed-update-all

    "oa" 'org-agenda

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
    "oRET"   'org-ctrl-c-ret
    "oM-RET" 'org-meta-return
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
  (setq org-roam-v2-ack t)
  :config
  (org-roam-db-autosync-mode))

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode))

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

(use-package perspective
  :config
  (pe/leader-def
    :states '(normal visual)
    "l" '(:keymap perspective-map :which-key "persp"))
  (persp-mode))

;; (use-package yasnippet
;;   :ensure t
;;   :config
;;   (yas-global-mode))

(use-package plantuml-mode
  :ensure t
  :config
  (setq plantuml-jar-path "/home/pekka/tools/plantuml.jar"
	plantuml-default-exec-mode 'jar
	plantuml-output-type "png"))

(use-package chronos
  :ensure t
  :config
  (pe/leader-def
    :states '(normal visual)
    "ac" '(:keymap chronos-mode-map :which-key "chronos")
    )
  )

(use-package erc
  :ensure t
  :config
  (setq erc-nick "pekka"
	erc-user-full-name "Pekka Ervasti"
	erc-track-shorten-start 8
	;; erc-autojoin-channels-alist '(("#systemcrafters"))
	))

(use-package dtrt-indent
  :ensure t
  :config
  (dtrt-indent-mode 1))

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
          )))

(display-time-mode 1)

(load "~/.dotfiles/emacs/.my-emacs.d/my-user-config.el")
(add-hook 'edebug-mode-hook 'evil-normalize-keymaps)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom shit
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(erc chronos plantuml-mode yasnippet perspective org-superstar evil-mu4e mu4e evil-org evil-org-mode org-mode evil-surround org-roam consult ag zenburn-theme winum which-key vertico undo-tree solarized-theme smartparens rg ranger projectile orderless marginalia magit lsp-pyright helpful general evil-collection embark doom-themes doom-modeline dashboard counsel company avy))
 '(safe-local-variable-values
   '((projectile-project-compilation-cmd . "rm -rf build && make -j8 target_board=takki_silabs_v2 debug_print=yes")
     (projectile-project-install-cmd . "tools/flash.sh")
     (projectile-project-compilation-cmd . "rm -rf build && bear make -j8 target_board=takki_silabs_v2")
     (projectile-project-install-cmd . "cd tools && BOARD=1 ./configure_image_and_flash_board_dev.sh")
     (projectile-project-compilation-cmd . "rm -rf build && make -j8 target_board=takki_silabs_v2")
     (projectile-project-compilation-cmd . "rm -rf build && make target_board=nrf")
     (projectile-project-install-cmd . "scp -r thingsee_gateway root@192.168.0.100:/usr/lib/python3.8/site-packages/")
     (projectile-project-install-cmd . "scp -r thingsee_gateway root@87.100.199.182:/usr/lib/python3.8/site-packages/")
     (projectile-project-compilation-cmd . "")
     (projectile-project-install-cmd . "cd tools && BOARD=0 ./configure_image_and_flash_board_dev.sh")
     (projectile-project-compilation-cmd . "rm -rf build && make -j8 target_board=nrf")
     (projectile-compilation-dir . projectile-project-root)))
 '(vertico-sort-function 'vertico-sort-alpha))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
