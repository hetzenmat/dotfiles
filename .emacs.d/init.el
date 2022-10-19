(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(setq column-number-mode t)

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

(setq company-minimum-prefix-length 1)

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

(load-theme 'doom-gruvbox t)

(setq gc-cons-threshold (* 16 1024 1024))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dashboard vterm pdf-tools doom-themes doom-modeline auctex company embark consult marginalia orderless vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )