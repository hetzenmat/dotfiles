(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(let ((packages '(vertico
		  orderless
		  marginalia
		  consult
		  embark
		  company
		  auctex
		  doom-modeline
		  doom-themes
		  pdf-tools
		  vterm
		  dashboard))
      (needs-refresh t))
  (dolist (package packages)
    (unless (package-installed-p package)
      (when needs-refresh
	(package-refresh-contents)
	(setq needs-refresh nil))
      (package-install package))))

(global-display-line-numbers-mode)
(global-hl-line-mode)
(recentf-mode)
(setq recentf-max-saved-items 10000)
(savehist-mode)
(save-place-mode)
(electric-pair-mode 1)
(show-paren-mode 1)
(setq calendar-date-style 'iso
      column-number-mode t
      custom-file (concat user-emacs-directory "custom.el")
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(load custom-file)

(defun swap-window-direction (direction arg)
  (let ((other-window (windmove-find-other-window direction arg))
	(current-buffer (window-buffer)))
    (cond ((null other-window)
	   (user-error "Cannot swap, there is no window %s from selected window" direction))
	  ((or (window-minibuffer-p (selected-window))
	       (window-minibuffer-p other-window))
	   (user-error "Cannot swap window with minibuffer"))
	  (t
	   (unless arg
	     (set-window-buffer (selected-window) (window-buffer other-window)))
	   (set-window-buffer other-window current-buffer)
	   (select-window other-window)))))

(windmove-default-keybindings 'super)
(global-set-key [C-s-right] (lambda (&optional arg)
			      (interactive "P")
			      (swap-window-direction 'right arg)))
(global-set-key [C-s-left] (lambda (&optional arg)
			     (interactive "P")
			     (swap-window-direction 'left arg)))
(global-set-key [C-s-down] (lambda (&optional arg)
			     (interactive "P")
			     (swap-window-direction 'down arg)))
(global-set-key [C-s-up] (lambda (&optional arg)
			   (interactive "P")
			   (swap-window-direction 'up arg)))

(setq dashboard-items '((recents . 10)
			(bookmarks . 10)))
(dashboard-setup-startup-hook)

(vertico-mode)

(marginalia-mode)

(pdf-tools-install)

(setq consult-async-min-input 1)

(global-set-key (kbd "C-x b") #'consult-buffer)
(global-set-key (kbd "C-x r b") #'consult-bookmark)
(global-set-key (kbd "M-y") #'consult-yank-pop)
(global-set-key (kbd "M-g o") #'consult-outline)
(global-set-key (kbd "M-g i") #'consult-imenu)
(global-set-key (kbd "M-g I") #'consult-imenu-multi)
(global-set-key (kbd "M-s M-s") #'consult-find)
(global-set-key (kbd "M-s s") #'consult-find)
(global-set-key (kbd "M-s l") #'consult-line)
(global-set-key (kbd "M-s M-l") #'consult-line)
(global-set-key (kbd "M-s L") #'consult-line-multi)

(global-set-key (kbd "C-.") #'embark-act)
(global-set-key (kbd "M-.") #'embark-dwim)
(global-set-key (kbd "C-h B") #'embark-bindings)

(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

(setq TeX-parse-self t ; parse on load
      TeX-auto-save t  ; parse on save
      ;; Use hidden directories for AUCTeX files.
      TeX-auto-local ".auctex-auto"
      TeX-style-local ".auctex-style"
      TeX-source-correlate-mode t
      TeX-source-correlate-method 'synctex
      ;; Don't start the Emacs server when correlating sources.
      TeX-source-correlate-start-server nil
      ;; Automatically insert braces after sub/superscript in `LaTeX-math-mode'.
      TeX-electric-sub-and-superscript t
      ;; Just save, don't ask before each compilation.
      TeX-save-query nil
      font-latex-fontify-script nil
      TeX-electric-escape t)

(setq company-minimum-prefix-length 1
      company-idle-delay 0)

(add-hook 'after-init-hook 'global-company-mode)

(defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)
(setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(setq enable-recursive-minibuffers t)

(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

(load-theme 'doom-gruvbox t)

(setq gc-cons-threshold (* 16 1024 1024))
