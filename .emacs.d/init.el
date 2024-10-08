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
  ;;(setq-default cursor-type 'bar)
  ;;(global-display-line-numbers-mode)
  ;;(global-hl-line-mode)
  (recentf-mode)
  (savehist-mode)
  (save-place-mode)
  ;;(electric-pair-mode)
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
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'first)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  :bind
  (:map corfu-map
        ([tab] . corfu-next)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode))

(use-package eglot)


(require 'windmove)
(keymap-global-set "s-h" 'windmove-left)
(keymap-global-set "M-<left>" 'windmove-left)

(keymap-global-set "s-l" 'windmove-right)
(keymap-global-set "M-<right>" 'windmove-right)

(keymap-global-set "s-j" 'windmove-up)
(keymap-global-set "M-<up>" 'windmove-up)

(keymap-global-set "s-k" 'windmove-down)
(keymap-global-set "M-<down>" 'windmove-down)

(keymap-global-set "M-o" 'other-window)
(keymap-global-set "s-b" 'consult-buffer)

(setq org-clock-into-drawer nil
      org-edit-src-content-indentation 0)

(add-hook 'focus-out-hook (lambda ()
			    (when (and buffer-file-name (buffer-modified-p)) (save-buffer))))

(require 'mu4e)
(setq mu4e-search-threads nil)


(require 'compile)

(setq compilation-scroll-output 'first-error)

(add-hook 'haskell-mode-hook
	  (lambda ()
	    (define-key haskell-mode-map (kbd "<f5>")
			(lambda () (interactive)
			  (save-buffer)
			  (compile (format "liquid %s" (buffer-file-name)))))))

(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (eglot-ensure)
	    (define-key LaTeX-mode-map (kbd "<f5>")
			(lambda () (interactive)
			  (save-buffer)
			  (compile (format "latexmk -quiet -f -pdf -shell-escape -synctex=1 -interaction=nonstopmode %s" (buffer-file-name)))))))


(add-hook 'easycrypt-mode-hook
	  (lambda ()
	    (define-key easycrypt-mode-map (kbd "<next>") #'proof-assert-next-command-interactive)
	    (define-key easycrypt-mode-map (kbd "<prior>") #'proof-undo-last-successful-command)))

(setq display-buffer-alist '(("*compilation*\\|*vterm*\\|*eshell*"
			     (display-buffer-in-side-window)
			     (window-height . 0.25)
			     (side . bottom))
      ))

(require 'compile)

(push (lambda (buf _) (ansi-color-apply-on-region (point-min) (point-max))) compilation-finish-functions)

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
(defun src-block-info->language (i)
  (nth 0 i))
(defun src-block-info->body (i)
  (nth 1 i))
(defun src-block-info->arguments (i)
  (nth 2 i))

(defun src-block-info->use-list (i)
  (split-string (or (@ :arguments i) "")))

(setq liquid-cmd "liquid")

(defun @ (key alist)
    (cdr (assq key alist)))

(defun org-babel-get-src-block-info-alist (&optional no-eval datum)
  (pcase (org-babel-get-src-block-info no-eval datum)
    (`(,language ,body ,arguments ,switches ,name ,start ,coderef)
     `((:language . ,language)
       (:body . ,body)
       (:arguments . ,arguments)
       (:switches . ,switches)
       (:name . ,name)
       (:start . ,start)
       (:coderef . ,coderef)))))

(require 'dash)

(defun maybe-liquid (exec-hs body params)
  (when (assq :liquid params)
    (let* ((this-block-info (org-babel-get-src-block-info-alist t))
	   (this-block-name (@ :name this-block-info))
	   (use-blocks-todo (if this-block-name (list this-block-name) nil))
	   (use-blocks-info (if this-block-name nil `((,this-block-name . ,this-block-info))))
	   info
	   (tmp-src-file (org-babel-temp-file "Haskell-src-" ".hs"))
	   )
      (when (src-block-info->use-list this-block-info)
	(unless (and (stringp this-block-name) (> (length this-block-name) 0))
	  (error "Block needs a name.")))      
      
      (while use-blocks-todo
	(setq block-name (car use-blocks-todo)
	      use-blocks-todo (cdr use-blocks-todo))
	(unless (assoc block-name use-blocks-info)
	  (setq info (get-src-info block-name))
	  (when (null info)
	    (error "No source block found named '%s'" block-name))
	  (setq use-blocks-info (cons `(,block-name . ,info) use-blocks-info))
	  (setq use-blocks-todo (append use-blocks-todo (src-block-info->use-list info)))
	  ))
      (unless (--all? (string= "haskell" (@ :language it)) use-blocks-info)
	(error "All blocks must use the same language"))
      (setq body (mapconcat (lambda (e) (@ :body (cdr e))) use-blocks-info "\n\n"))
      (with-temp-file tmp-src-file
	(progn
	  (insert body)
	  (newline)
	  (unless (string-match-p "^[[:blank:]]*main[[:blank:]]*=" body)
	    (insert (format "main = putStrLn \"Successfully verified %s\"\n" (or this-block-name ""))))))
      
      (ansi-color-apply (org-babel-eval
       (format "%s %s" liquid-cmd (org-babel-process-file-name tmp-src-file)) ""))
      
      )))


(setq org-confirm-babel-evaluate nil)
(setq org-return-follows-link t)

(defun merge-use-params (alists)
  (let (result
	cur-list
	use-list)
    (dolist (alist alists)
      (setq cur-list nil)
      (when alist
	(pcase-dolist ((and cell `(,key . ,val)) alist)
	  (if (eq key :use)
	      (push val use-list)
	    (push cell cur-list)))
	(when cur-list
	  (push cur-list result))))
    (when use-list
      (push `((:use . ,(mapconcat #'identity use-list " "))) result))
    result))

(advice-add 'org-babel-merge-params :filter-args #'merge-use-params)

(advice-add 'org-babel-execute:haskell :around #'maybe-liquid)




;(require 'auctex)
;(setq TeX-parse-self t
;      TeX-source-correlate-mode t
;      TeX-source-correlate-start-server t
;      tex-fontify-script nil
;      font-latex-fontify-script nil
;;      TeX-view-program-list '(("Okular" "okular --unique %o"))
;      TeX-view-program-selection '((output-pdf "Okular")))


;(setq read-file-name-completion-ignore-case t
;      read-buffer-completion-ignore-case t
;      completion-ignore-case t
;      require-final-newline nil
;      mode-require-final-newline nil)

;; (custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(default ((t (:family "JetBrains Mono" :foundry "JB" :slant normal :weight regular :height 120 :width normal)))))
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
   '(proof-general cape auctex undo-tree ef-themes evil avy embark-consult embark modus-themes pdf-tools vterm corfu dashboard which-key marginalia orderless vertico magit haskell-mode consult company))
 '(proof-strict-read-only nil))
(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
