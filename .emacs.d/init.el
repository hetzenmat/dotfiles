(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;(unless (package-installed-p 'use-package)
;  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)


(use-package emacs
  :init
  (setq enable-recursive-minibuffers t
	tab-always-indent 'complete
	recentf-max-saved-items 10000
	calendar-date-style 'iso
	column-number-mode t
	backup-directory-alist `((".*" . ,temporary-file-directory))
	auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
	vc-follow-symlinks t
	read-file-name-completion-ignore-case t
	read-buffer-completion-ignore-case t
	completion-ignore-case t)
  (setq-default cursor-type 'bar)
  (global-display-line-numbers-mode)
  (global-hl-line-mode)
  (recentf-mode)
  (savehist-mode)
  (save-place-mode)
  (electric-pair-mode)
  (show-paren-mode))

(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))
					(eglot (styles orderless)))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package which-key
  :init
  (setq which-key-show-early-on-C-h t
	which-key-idle-delay 0.5)
  (which-key-mode))

(use-package dashboard
  :init
  (setq dashboard-items '((recents . 10)
			  (bookmarks . 10)))
  (dashboard-setup-startup-hook))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)

;; (use-package embark
;;   :bind
;;   (("C-." . embark-act)         ;; pick some comfortable binding
;;    ("C-;" . embark-dwim)        ;; good alternative: M-.
;;    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

;;   :init
;;   ;; Optionally replace the key help with a completing-read interface
;;   (setq prefix-help-command #'embark-prefix-help-command)

;;   ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
;;   ;; strategy, if you want to see the documentation from multiple providers.
;;   (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
;;   ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

;;   :config

;;   ;; Hide the mode line of the Embark live/completions buffers
;;   (add-to-list 'display-buffer-alist
;;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
;; (use-package embark-consult
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))

;; (use-package vterm
;;   :hook
;;   (vterm-mode . (lambda ()
;; 		  (hl-line-mode -1)
;; 		  (display-line-numbers-mode -1))))

(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  :bind
  (:map corfu-map
        ([tab] . corfu-next)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode))

(use-package eglot)

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

(windmove-default-keybindings 'shift)
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


(setq org-clock-into-drawer nil)

(add-hook 'focus-out-hook (lambda ()
			    (when (and buffer-file-name (buffer-modified-p)) (save-buffer))))

(require 'mu4e)

(setq mu4e-contexts
    `( 
       ,(make-mu4e-context
          :name "GMail"
          :enter-func (lambda () (mu4e-message "Switch to the Work context"))
          ;; no leave-func
          ;; we match based on the maildir of the message
          ;; this matches maildir /Arkham and its sub-directories
          :match-func (lambda (msg)
			(message msg)
                        (when msg
                          (string-match-p "^/gmail" (mu4e-message-field msg :maildir))))
          :vars '( ( user-mail-address	       . "mhetzenberger@gmail.com" )
                   ( user-full-name	       . "Matthias Hetzenberger" )
		   ( mu4e-sent-folder          . "/gmail/Sent" )
                   ))
       ,(make-mu4e-context
          :name "Work"
          :enter-func (lambda () (mu4e-message "Switch to the Work context"))
          ;; no leave-func
          ;; we match based on the maildir of the message
          ;; this matches maildir /Arkham and its sub-directories
          :match-func (lambda (msg)
			(message msg)
                        (when msg
                          (string= "/tu-work" (mu4e-message-field msg :maildir))))
          :vars '( ( user-mail-address	       . "matthias.hetzenberger@tuwien.ac.at" )
                   ( user-full-name	       . "Matthias Hetzenberger" )
		   ( mu4e-sent-folder          . "/tu-work/Sent" )
		   ( mu4e-maildir )
                   ))))

(use-package pdf-tools)

(require 'ob-haskell)

(defun get-src-info (id)
  ;; Look for a source block named SOURCE-NAME.  If
  ;; found, assume it is unique; do not look after
  ;; `:noweb-ref' header argument.
  (org-with-point-at 1
     (let ((r (org-babel-named-src-block-regexp-for-name id)))
       (and (re-search-forward r nil t)
	    (not (org-in-commented-heading-p))
            (org-babel-get-src-block-info t)))))

(defun src-block-info->name (i)
  (nth 4 i))

(defun maybe-liquid (exec-hs body params)
  (when (assq :liquid params)
    (let* ((this-block-info (org-babel-get-src-block-info t))
	   (this-block-name (src-block-info->name this-block-info))
	   (use-list        (split-string (or (cdr (assoc :use params)) "")))
	   (use-blocks-info (list this-block-info))
	   info
	   )
      (dolist (name use-list)
	(when (string= name this-block-name) (error "Cannot use current block."))
	(setq info (get-src-info name))
	(when (null info)
	  (error "No source block found named '%s'" name))
	(setq use-blocks-info (cons info use-blocks-info)))
      (message "use: %S\nblocks: %S" use-list use-blocks-info))))


(setq org-confirm-babel-evaluate nil)
(setq org-return-follows-link t)

(advice-add 'org-babel-execute:haskell :around #'maybe-liquid)

(require 'auctex)

(setq TeX-parse-self t
      TeX-source-correlate-mode t
      TeX-source-correlate-start-server t
      tex-fontify-script nil
      font-latex-fontify-script nil
;;      TeX-view-program-list '(("Okular" "okular --unique %o"))
      TeX-view-program-selection '((output-pdf "Okular")))


;(setq read-file-name-completion-ignore-case t
;      read-buffer-completion-ignore-case t
;      completion-ignore-case t
;      require-final-newline nil
;      mode-require-final-newline nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrains Mono" :foundry "JB" :slant normal :weight regular :height 120 :width normal)))))
;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
; '(blink-cursor-mode nil)
; '(column-number-mode t)
; '(global-display-line-numbers-mode t)
; '(package-selected-packages
;   '(haskell-mode racket-mode merlin-company merlin which-key vterm vertico use-package tuareg treemacs pdf-tools orderless marginalia flycheck evil embark-consult eglot dashboard corfu consult-lsp company cape))
; '(tool-bar-mode nil))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("515ebca406da3e759f073bf2e4c8a88f8e8979ad0fdaba65ebde2edafc3f928c" "263e3a9286c7ab0c4f57f5d537033c8a5943e69d142e747723181ab9b12a5855" "6b839977baf10a65d9d7aed6076712aa2c97145f45abfa3bad1de9d85bc62a0e" "9d01a8af1bdd5c79b136dc5eb23b90d53675c3f4cb938dc15c4d8bc98d2bb86e" "df1ed4aa97d838117dbda6b2d84b70af924b0380486c380afb961ded8a41c386" "c42587b19ee1c9aa1a9dd1d8ace37ece24ca2a322243035cd6ba07f44fb466db" "f12083eec1537fc3bf074366999f0ee04ab23ab3eaba57614785d88b9db2a5d4" "c1638a7061fb86be5b4347c11ccf274354c5998d52e6d8386e997b862773d1d2" "64045b3416d83e5eac0718e236b445b2b3af02ff5bcd228e9178088352344a92" "c7a926ad0e1ca4272c90fce2e1ffa7760494083356f6bb6d72481b879afce1f2" "b29ba9bfdb34d71ecf3322951425a73d825fb2c002434282d2e0e8c44fce8185" "0f76f9e0af168197f4798aba5c5ef18e07c926f4e7676b95f2a13771355ce850" default))
 '(mu4e-search-include-related nil)
 '(package-selected-packages
   '(auctex undo-tree ef-themes evil avy embark-consult embark modus-themes pdf-tools vterm corfu dashboard which-key marginalia orderless vertico magit haskell-mode consult company)))
(put 'narrow-to-region 'disabled nil)
