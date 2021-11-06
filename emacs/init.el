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
 )

(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)

(recentf-mode)
(winner-mode)

(set-face-attribute 'default nil :font "Hack-10" :width 'condensed)

(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/use-package")
  (require 'use-package))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     '("org" . "https://orgmode.org/elpa/"))

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

(defun pe/switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun pe/switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch"))

(use-package general
  :ensure t
  :init
  (general-create-definer pe/leader-def
    :prefix "SPC")
  (pe/leader-def
    :states '(normal visual)
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
    "bm" ''
    ;; "o" org-mode-map
    )
  )

(use-package counsel
  :config
  (counsel-mode)
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
  (which-key-mode))

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

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t))

(load "~/.dotfiles/emacs/.my-emacs.d/my-user-config.el")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom shit
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-roam consult ag zenburn-theme winum which-key vertico undo-tree solarized-theme smartparens rg ranger projectile orderless marginalia magit lsp-pyright helpful general evil-collection embark doom-themes doom-modeline dashboard counsel company avy))
 '(safe-local-variable-values
   '((projectile-project-install-cmd . "scp -r thingsee_gateway root@87.100.199.182:/usr/lib/python3.8/site-packages/")
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
