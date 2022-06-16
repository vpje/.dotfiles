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

 recentf-max-saved-items 200
 indent-tabs-mode nil

 global-auto-revert-non-file-buffers t

 custom-file (locate-user-emacs-file "custom-vars.el")
 )

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

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(add-to-list 'load-path "/home/pekka/.emacs.d/auth-source-xoauth2")
(require 'mu4e)
(setq user-mail-address "pekka.ervasti@haltian.com"
      smtpmail-default-smtp-server "smtp.account1.example.com"
      smtpmail-local-domain "account1.example.com"
      smtpmail-smtp-server "smtp.account1.example.com"
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 25)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil)
  ;; (setq undo-tree-history-directory-alist (concat user-emacs-directory "undo-tree"))
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

(use-package flycheck
  :ensure t)

(use-package general
  :ensure t
  :init
  (general-create-definer pe/leader-def
    :states '(normal emacs visual)
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    )
  (pe/leader-def
    :states '(normal emacs visual)
    "a" '(:ignore t :which-key "app")
    "ac" 'calc
    "am" 'mu4e
    ;; "bd" 'evil-delete-buffer
    "bd" 'kill-this-buffer
    "b" '(:ignore t :which-key "buffer")
    "bm" 'pe/switch-to-messages-buffer
    "bn" 'evil-buffer-new
    "bs" 'pe/switch-to-scratch-buffer
    "c" '(:ignore t :which-key "compile/comment")
    "ck" 'kill-compilation
    "cl" 'comment-or-uncomment-region
    "cd" 'pe/delete-compilation-window
    "e" '(:ignore t :which-key "error")
    "el" 'flycheck-list-errors
    "en" 'flycheck-next-error
    "ep" 'flycheck-previous-error
    "'" 'vterm-other-window
    "fF" 'consult-find
    "ff" 'find-file
    "f" '(:ignore t :which-key "file")
    "fc" 'write-file
    "fs" 'save-buffer
    "F" '(:ignore t :which-key "frame")
    "Fn" 'make-frame-command
    "Fd" 'delete-frame
    "Fo" 'other-frame
    "g" '(:ignore t :which-key "git")
    "j" '(:ignore t :which-key "jump")
    "ji" 'consult-imenu
    "jI" 'consult-imenu-multi
    "jj" 'avy-goto-char-timer
    "js" 'avy-goto-symbol-1
    "i" '(:ignore t :which-key "insert")
    "is" 'yas/insert-snippet
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
    "tt" 'indent-tabs-mode
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
    "hh" 'xref-go-back
    "r" 'lsp-find-references
    )
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

;; minibuffer input history
(savehist-mode 1)

;; save last place visited
(save-place-mode 1)

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

(use-package git-timemachine
  :ensure t
  :config
  (pe/leader-def
    :states '(normal visual)
    "gt" 'git-timemachine))

(use-package projectile
  :ensure t
  :config
  ;; (setq projectile-indexing-method 'native)
  ;; (setq projectile-globally-ignored-directories '(".cache"))
  (setq projectile-indexing-method 'alien)
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
  (add-hook 'javascript-mode-hook 'lsp)
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

(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

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
  (org-indent-mode)
  (setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
	 "* TODO %?\n  %i\n  %a")
	("j" "Journal" entry (file+datetree "~/org/journal.org")
	 "* %?\nEntered on %U\n  %i\n  %a")))
  (setq org-clock-idle-time 10)
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
  (setq org-roam-v2-ack t
	org-return-follows-link  t)
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

(org-babel-do-load-languages 'org-babel-load-languages
			     '(
			       (shell . t)
			       (C . t)
			       (python . t)
			       ))
(setq org-confirm-babel-evaluate nil)

(use-package perspective
  :config
  (pe/leader-def
    :states '(normal visual)
    "l" '(:keymap perspective-map :which-key "persp")
    "ll" 'lsp-find-definition)
  (setq persp-suppress-no-prefix-key-warning t)
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
	  )))

(use-package vterm
  :ensure t)

(display-time-mode 1)

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
    :states '(normal visual)
    "ft" 'treemacs))

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

(use-package pdf-tools :ensure t)
(use-package sudo-edit :ensure t)

(load "~/.dotfiles/emacs/.my-emacs.d/my-user-config.el")

(add-hook 'edebug-mode-hook 'evil-normalize-keymaps)
(add-hook 'dart-mode-hook 'lsp)

(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(require 'eaf)
(require 'eaf-pdf-viewer)

(use-package ztree
  :ensure t)

(use-package git-gutter-fringe
  :ensure t
  :config
  (pe/leader-def
    :states '(normal visual)
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

(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode and related modes
  ;; (indent-tabs-mode -1)
  (dtrt-indent-mode 1)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(add-to-list 'load-path "~/.emacs.d/vpe/")
(require 'projectile-transient-menu)
