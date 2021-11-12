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

(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/use-package")
  (require 'use-package))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     '("org" . "https://orgmode.org/elpa/"))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(add-to-list 'load-path "/home/pekka/.emacs.d/auth-source-xoauth2")
(require 'mu4e)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  )

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
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
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun pe/switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun pe/switch-to-messages-buffer ()
  (interactive)
  (switch-to-buffer "*Messages*"))

(use-package general
  :ensure t
  :init
  (general-create-definer pe/leader-def
    :prefix "SPC")
  (pe/leader-def
    :states '(normal visual)
    "a" '(:ignore t :which-key "app")
    "f" '(:ignore t :which-key "file")
    "g" '(:ignore t :which-key "git")
    "s" '(:ignore t :which-key "search")
    "b" '(:ignore t :which-key "buffer")
    "e" '(:ignore t :which-key "error")
    "w" '(:ignore t :which-key "window")
    "am" 'mu4e
    "ff" 'find-file
    "fF" 'consult-find
    "fs" 'save-buffer
    "ss" 'swiper
    "sr" 'consult-ripgrep
    "'" 'eshell
    "wv" 'split-window-right
    "ws" 'split-window-below
    "wd" 'delete-window
    "qq" 'save-buffers-kill-terminal
    ;; "M-." 'xref-find-definitions
    "cl" 'comment-or-uncomment-region
    "bd" 'evil-delete-buffer
    "TAB" 'pe/switch-to-previous-buffer
    "el" 'flymake-show-diagnostics-buffer
    "en" 'flymake-goto-next-error
    "ep" 'flymake-goto-prev-error
    "wm" 'delete-other-windows
    "wu" 'winner-undo
    "wr" 'winner-redo
    "bs" 'pe/switch-to-scratch-buffer
    "bn" 'evil-buffer-new
    "bm" 'pe/switch-to-messages-buffer
    ;; "o" org-mode-map
    )
  )

(use-package counsel
  :config
  (counsel-mode 1)
  (pe/leader-def
    :states '(normal visual)
    "SPC" 'counsel-M-x
    ))

(use-package consult
  :ensure t
  :config
  (pe/leader-def
    :states '(normal visual)
    "bb" 'consult-buffer
    "fr" 'consult-recent-file
    ))

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
   :states 'normal
   :keymaps 'override
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
    :states '(normal visual)
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
  (projectile-mode 1)
  (pe/leader-def
    :states '(normal visual)
    "p" projectile-command-map))

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
   "l" lsp-command-map))

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
  :ensure t)

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t
	org-roam-directory "~/org-roam"
	))

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode))

;; temporary fix for https://github.com/Somelauw/evil-org-mode/issues/93
(fset 'evil-redirect-digit-argument 'ignore)
(add-to-list 'evil-digit-bound-motions 'evil-org-beginning-of-line)
(evil-define-key 'motion 'evil-org-mode
  (kbd "0") 'evil-org-beginning-of-line)

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)   ; or use a nicer switcher, see below
  :config
  (pe/leader-def
    :states '(normal visual)
    "l" perspective-map)
  (persp-mode))

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
   '(perspective org-superstar evil-mu4e mu4e evil-org evil-org-mode org-mode evil-surround org-roam consult ag zenburn-theme winum which-key vertico undo-tree solarized-theme smartparens rg ranger projectile orderless marginalia magit lsp-pyright helpful general evil-collection embark doom-themes doom-modeline dashboard counsel company avy))
 '(safe-local-variable-values
   '((projectile-project-compilation-cmd . "rm -rf build && make target_board=nrf")
     (projectile-project-install-cmd . "scp -r thingsee_gateway root@192.168.0.100:/usr/lib/python3.8/site-packages/")
     (projectile-project-install-cmd . "scp -r thingsee_gateway root@87.100.199.182:/usr/lib/python3.8/site-packages/")
     (projectile-project-compilation-cmd . "")
     (projectile-project-install-cmd . "cd tools && BOARD=0 ./configure_image_and_flash_board_dev.sh")
     (projectile-project-compilation-cmd . "rm -rf build && make -j8 target_board=nrf")
     (projectile-compilation-dir . projectile-project-root))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
