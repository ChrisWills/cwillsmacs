
(setq scroll-conservatively 100)
(setq scroll-preserve-screen-position t)
(setq max-mini-window-height 20)
;; amx won't persist state between sessions but setting this to the empty string
;; seems to fix it.
(setq help-window-select t)

;; We deliberately do not load ~/cwills-emacs-custom.el - it is strictly used as
;; a way to figured out what the formating is for setting a custom variable.
(setq custom-file "~/cwills-emacs-custom.el")

(if (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq-default indent-tabs-mode nil)          ; use spaces instead of tabs
(setq-default tab-width 2)                   ; 2-space tabs
(setq-default fill-column 80)                ; 80 character line width
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq vc-follow-symlinks t)
;;(set-default 'initial-buffer-choice 5)
;;(setq inhibit-splash-screen t)

;; Set the default face
(set-face-attribute 'default nil
                    :font "DejaVu Sans Mono"
                    :height 120)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font "DejaVu Sans Mono"
                    ;;:font "Source Code Pro"
                    :height 120)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    :font "DejaVu Sans"
                    ;;:font "Noto Sans"
                    ;;:font "Iosevka Aile"
                    ;;:font "Iosevka"
                    :height 120
                    :weight 'regular)

;; Save location within a file
(save-place-mode t)
;;(global-hl-line-mode 1)                      ; highlight current line
(column-number-mode 1)
(electric-indent-mode 1)                     ; smart auto-indent
(setq-default electric-indent-inhibit t)     ; ... disable indenting previous line (WHY?!)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(add-to-list 'load-path "~/.cwills-emacs.d/elisp")
(add-to-list 'custom-theme-load-path "~/.cwills-emacs.d/themes")

(setq package-user-dir (concat user-emacs-directory nil "/packages"))
(add-to-list 'custom-theme-load-path (concat user-emacs-directory nil "/themes"))
;; Initialize package sources
(require 'package)

(setq package-archives  '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(require 'help-fns+)
(require 'recentf)

(setq display-buffer-alist
      `(
        (".*\\(eshell side-window\\).*"
         display-buffer-in-side-window
         (side . bottom)
         (slot . 0)
         (window-height . 0.25)
         (preserve-size . (nil . t))
         )))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-height 15)
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1))

(use-package all-the-icons
  :config (setq all-the-icons-scale-factor 1.0))

;;(use-package modus-themes
;;  :ensure t
;;  :config
;;  ;; Add all your customizations prior to loading the themes
;;  (setq modus-themes-italic-constructs t
;;        modus-themes-bold-constructs t)
;;
;;  (setq modus-themes-org-blocks 'gray-background)
;;  ;; Maybe define some palette overrides, such as by using our presets
;;;;  (setq modus-themes-common-palette-overrides
;;;;       modus-themes-preset-overrides-intense)
;;
;;  ;; Load the theme of your choice.
;;;;  (load-theme 'modus-operandi t)
;;;;  (load-theme 'modus-operandi-tinted t)
;;;;  (load-theme 'modus-operandi-deuteranopia t)
;;  (load-theme 'modus-vivendi-tinted t)
;;
;;  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(use-package doom-modeline
  :ensure t
  :config
  ;;(setq doom-modeline-height 40)
  (setq doom-modeline-height 30)
  (setq doom-modeline-icon t)
  (doom-modeline-mode 1))

(use-package doom-themes
  :ensure t
  :after doom-modeline
  :custom-face
;;  (custom-face-tag ((t (:foreground "#6c71c4" :weight normal :height 1.2))))
;;  (custom-group-subtitle ((t (:foreground "#b58900" :weight bold))))
;;  (custom-group-tag ((t (:inherit variable-pitch :foreground "#268bd2" :height 1.2))))
;;  (custom-variable-tag ((t (:inherit variable-pitch :foreground "#2aa198" :height 1.2))))
;;  (font-lock-builtin-face ((t (:foreground "#268bd2" :weight bold))))
;;  (font-lock-constant-face ((t (:foreground "#268bd2" :weight bold))))
;;  (font-lock-preprocessor-face ((t (:foreground "#268bd2" :weight normal))))
;;  (highlight-numbers-number ((t (:foreground "#6c71c4" :weight normal))))
;;  (org-code ((t (:foreground "#586e75"))))
;;  (org-meta-line ((t (:foreground "#586e75" :slant italic))))
;;  (rainbow-delimiters-depth-1-face ((t (:foreground "#2aa198"))))
;;  (rainbow-delimiters-depth-2-face ((t (:foreground "#b58900"))))
;;  (rainbow-delimiters-depth-3-face ((t (:foreground "#268bd2"))))
;;  (rainbow-delimiters-depth-4-face ((t (:foreground "#6c71c4"))))
;;  (rainbow-delimiters-depth-5-face ((t (:foreground "#859900"))))
;;  (rainbow-delimiters-depth-6-face ((t (:foreground "#b58900"))))
;;  (rainbow-delimiters-depth-7-face ((t (:foreground "#268bd2"))))
;;  (rainbow-delimiters-depth-8-face ((t (:foreground "#6c71c4"))))
;;  (rainbow-delimiters-depth-9-face ((t (:foreground "#859900"))))
;;  (org-document-info-keyword ((t (:inherit (shadow) :height 1.3))))
;;  (org-document-title ((t (:weight bold :foreground "#268bd2" :height 1.3))))
;;  (outline-1
;;   ((t (:weight bold :extend t :foreground "#268bd2" :height 1.3))))
;;  (outline-2
;;   ((t (:weight bold :extend t :foreground "#859900" :height 1.2))))
;;  (outline-3
;;   ((t (:weight bold :extend t :foreground "#35a69c" :height 1.15))))
;;  (outline-4
;;   ((t (:weight bold :extend t :foreground "#1e6fa8" :height 1.1))))
 ;; (header-line ((t (:inherit (default)))))
  ;;(magit-diff-removed-highlight ((t (:extend t :background "#2E2A2D" :foreground "#dc322f" :weight bold))))
  ;;; (font-lock-builtin-face ((t (:foreground "#6c71c4"))))
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  (load-theme 'doom-cwills-solarized-dark t)

  ;; Enable flashing mode-line on errors
  ;;(doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;(doom-themes-neotree-config)
  ;; or for treemacs users
  ;;(setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;;(doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(defun cw/prog-mode-setup ()
  (setq show-trailing-whitespace t)
  ;; Clickable links
  (goto-address-prog-mode 1)
  (eldoc-mode 0)
  (auto-fill-mode 1)
  (setq comment-auto-fill-only-in-comments t))

(add-hook 'prog-mode-hook 'cw/prog-mode-setup)

(defun cw/text-mode-setup ()
  (setq show-trailing-whitespace t)
  ;; Clickable links
  (goto-address-prog-mode 1)
  (auto-fill-mode 1))

(add-hook 'text-mode-hook 'cw/text-mode-setup)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package smartparens
  :init (require 'smartparens-config)
  :hook (prog-mode . smartparens-mode))

(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode)
  :config
  (set-face-attribute 'highlight-parentheses-highlight nil :weight 'ultra-bold)
  (setq highlight-parentheses-colors '("Springgreen3" "IndianRed1" "IndianRed3" "IndianRed4"))
  )

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1)
  (which-key-setup-side-window-bottom)
  ;(which-key-setup-minibuffer)
  (which-key-mode))

;;(use-package all-the-icons-ivy-rich
;;  :init (all-the-icons-ivy-rich-mode 1)
;;  :config (setq inhibit-compacting-font-caches t))

(use-package ivy-rich
  :init (ivy-rich-mode 1)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

;;(use-package prescient)
;;(use-package ivy-prescient)
(use-package amx
  :init (amx-mode 1))

;; Too slow
;;(use-package helpful
;;  :custom
;;  (counsel-describe-function-function #'helpful-callable)
;;  (counsel-describe-variable-function #'helpful-variable)
;;  :bind
;;  ([remap describe-function] . counsel-describe-function)
;;  ([remap describe-command] . helpful-command)
;;  ([remap describe-variable] . counsel-describe-variable)
;;  ([remap describe-key] . helpful-key)
;;  ([remap describe-symbol] . helpful-symbol))

(use-package general)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  ;; This makes the state icon work when running in the terminal as long as the
  ;; all-the-icons fonts are installed
  (setq evil-normal-state-tag "●")
  (setq evil-insert-state-tag "●")
  (setq evil-visual-state-tag "●")
  (setq evil-emacs-state-tag "●")
  (setq evil-operator-state-tag "●")
  (setq evil-motion-state-tag "●")
  (setq evil-replace-state-tag "●")
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

  (evil-set-undo-system 'undo-fu))

(use-package undo-fu)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1)
  (general-define-key
   :states 'visual
   :keymaps 'evil-surround-mode-map
   "s" 'evil-surround-region
   "S" 'evil-substitute))

(use-package eshell
  :config
  (setq eshell-highlight-prompt nil)
  (setq eshell-buffer-name "eshell")

  (defun cw/eshell-generic (&optional arg style)
    (interactive "P")
    (cl-assert eshell-buffer-name)
    (let ((cw/eshell-buffer-name
           (cond ((eq style 'side-window)
                  "eshell side-window")
                 (t eshell-buffer-name)
                 (nil eshell-buffer-name))))
      (let ((buf
             (cond ((numberp arg)
		                (get-buffer-create (format "%s<%d>"
					                                     cw/eshell-buffer-name
					                                     arg)))
		               (arg
		                (generate-new-buffer cw/eshell-buffer-name))
		               (t
		                (get-buffer-create cw/eshell-buffer-name)))))
        (cl-assert (and buf (buffer-live-p buf)))
        (with-current-buffer buf nil
                             (unless (derived-mode-p 'eshell-mode)
                               (eshell-mode)))
        (cond ((eq style 'side-window)
               (select-window (display-buffer buf)))
              ((eq style 'other-window)
               (switch-to-buffer-other-window buf))
              (t (pop-to-buffer-same-window buf))
              (nil (pop-to-buffer-same-window buf))))))

  (defun cw/eshell-side-window ()
      (interactive)
    (cw/eshell-generic nil 'side-window))

  (defun cw/eshell-other-window (&optional arg)
    (interactive)
    (cw/eshell-generic arg 'other-window))

  (defun cw/eshell-other-window-new-buf ()
    (interactive)
    (cw/eshell-other-window 'N)))

;; This makes eshell look cool - copied from spacemacs
(use-package eshell-prompt-extras
  :after eshell
  :config (setq eshell-prompt-function #'epe-theme-lambda))

(use-package dired
  :ensure nil
  :custom ((dired-listing-switches "-al --group-directories-first"))
  :config
  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map
   "l" 'dired-find-file
   "h" 'dired-up-directory))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom (all-the-icons-dired-monochrome nil))
;;(use-package treemacs-icons-dired
;;  :hook (dired-mode . 'all-the-icons-dired-mode))

;; Magit Configuration ---------------------------------------------------------

(use-package magit)

;; Org Mode Configuration ------------------------------------------------------

(defun cw/org-mode-setup ()
  (require 'org-tempo)
  (org-indent-mode 1)
  (auto-fill-mode 0)
  ;;(variable-pitch-mode 1)
  (visual-line-mode 1)
  (setq header-line-format " ")
  ;;(set-face-attribute 'header-line nil :inherit '(fixed-pitch) :height 1.0)
  ;;(set-face-attribute 'org-code nil :inherit '(fixed-pitch) :foreground nil)
  ;;(set-face-attribute 'org-block nil :inherit '(fixed-pitch) :foreground nil)
  ;;;;(set-face-attribute 'org-verbatim nil :inherit '(fixed-pitch) :foreground "#859900")
  (add-to-list 'org-structure-template-alist '(sh . "src shell"))
  (add-to-list 'org-structure-template-alist '(el . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '(py . "src python")))

(use-package org
  :hook (org-mode . cw/org-mode-setup)
  :config
  (setq org-ellipsis " ↴")
  (setq org-hide-emphasis-markers t))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-present
  :after org)

;; Center and pad org-mode to make it look almost list a webpage/standard document editor
(defun cw/org-mode-visual-fill-setup ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text 1)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . cw/org-mode-visual-fill-setup))

(use-package org-babel-eval-in-repl
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t))))

(use-package haskell-mode)

;; Eglot -----------------------------------------------------------------------

(use-package company)
(use-package flycheck)
(use-package eglot)
(use-package ggtags
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda ()
           (setq-local imenu-create-index-function
                                #'ggtags-build-imenu-index)
           (ggtags-mode 1)))
  :config
  (general-define-key
   :states 'normal
   :keymaps 'ggtags-mode-map
   :prefix ","
   "f d" #'ggtags-find-definition
   "i m" #'counsel-imenu)
  )
(use-package counsel-gtags)

;; LSP Mode --------------------------------------------------------------------

;;(use-package company)
;;(use-package flycheck)
;;
;;(setq gc-cons-threshold 100000000)
;;(setq read-process-output-max (* 1024 1024)) ;; 1mb
;;(setq ccls-executable "/usr/bin/ccls")
;;(use-package lsp-mode
;;  :commands lsp
;;  :init
;;  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
;;  :config
;;  (lsp-enable-which-key-integration t))
;;
;;(use-package lsp-ui :commands lsp-ui-mode)
;;(use-package lsp-ivy)
;;
;;(use-package ccls
;;  :hook ((c-mode c++-mode objc-mode cuda-mode) .
;;         (lambda () (require 'ccls) (lsp))))



;; Custom stuff ----------------------------------------------------------------


;; Global Keybindings
(general-def
  "C-x b" '(ivy-switch-buffer :which-key "switch-buffer")
  "C-h ESC" nil
  [remap describe-function] 'counsel-describe-function
  [remap describe-variable] 'counsel-describe-variable
  [remap describe-symbol] 'counsel-describe-symbol)

;; SPACE Leader Keybindisngs
(general-create-definer cw/my-leader-def
  :states '(normal insert visual emacs)
  :keymaps '(override global)
  :prefix "SPC"
  :global-prefix "C-SPC")

(cw/my-leader-def
  "SPC" '(counsel-M-x :which-key "M-x")
  "TAB" '(evil-switch-to-windows-last-buffer :which-key "last buffer")
  "b"  '(:ignore t :which-key "buffer")
  "bb" '(ivy-switch-buffer :which-key "switch-buffer")
  "bd" '(kill-current-buffer :which-key "kill-buffer")
  ;; Bind w as a prefix for cw-window-map which is a custom map
  "w"  '(:prefix-command cw-window-map :which-key "window")
  "f"  '(:ignore t :which-key "file")
  "ff" '(counsel-find-file :which-key "find-file")
  "fj" '(dired-jump :which-key "dired-jump")
  "f e d" #'(lambda () (interactive) (switch-to-buffer (find-file-noselect "~/cwills-emacs-init.el")))
  "t"  '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme"))

;; Define a custom keymap
(general-def
  :prefix-map 'cw-window-map
  "-" '(split-window-below :which-key "split-window-below")
  "/" '(split-window-right :which-key "split-window-right")
  "m" '(delete-other-windows :which-key "delete-other-windows")
  "d" '(delete-window :which-key "delete-window")
  "TAB" '(other-window :which-key "other-window"))

(use-package epg
  :ensure nil
  :custom
  (safe-local-variable-values '((epa-file-encrypt-to . cwills\.dev@gmail\.com)))
  :config
  ;; Always prompt for passowrds in the echo area - never in a gui
  (setq epg-pinentry-mode 'loopback))
