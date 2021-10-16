(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(toggle-frame-fullscreen)
(setq visible-bell t)
(column-number-mode 1)
(global-linum-mode)
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		shell-mode-hook))
  (add-hook mode (lambda () (linum-mode 0))))

(leaf all-the-icons
  :ensure t
  :require t)

(leaf doom-themes
    :ensure t
    :require t)
(load-theme 'doom-gruvbox t)

(leaf doom-modeline
  :ensure t
  :require t
  :init (doom-modeline-mode 1))

(set-face-attribute 'default nil :font "Ubuntu Mono")

(leaf dynamic-fonts
    :ensure t
    :require t
    )
(dynamic-fonts-setup)

(leaf swiper
  :ensure t
  :require t)

(leaf counsel
  :ensure t
  :require t
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 (minibuffer-local-map
	  ("C-r" . counsel-minibuffer-history))))

(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

(leaf ivy
  :ensure t
  :require t
  :diminish
  :bind (("C-s" . swiper)
	 (ivy-minibuffer-map
	  ("TAB" . ivy-alt-done)
	  ("C-l" . ivy-alt-done)
	  ("C-j" . ivy-next-line)
	  ("C-k" . ivy-previous-line))
	 (ivy-switch-buffer-map
	  ("C-k" . ivy-previous-line)
	  ("C-l" . ivy-alt-done)
	  ("C-d" . ivy-switch-buffer-kill))
	 (ivy-reverse-i-search-map
	  ("C-k" . ivy-previous-line)
	  ("C-d" . ivy-reverse-i-search-kill)))
  :config
  (ivy-mode 1))

(leaf ivy-rich
  :ensure t
  :require t
  :init (ivy-rich-mode 1))

(leaf which-key
  :ensure t
  :require t
  :init (which-key-mode)
  :diminish
  :config (setq which-key-idle-delay 3.0))

(leaf helpful
  :ensure t
  :require t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(winner-mode 1)

(leaf magit
  :ensure t
  :require t)

(leaf projectile
  :ensure t
  :require t
  :diminish
  :config (projectile-mode)
  :bind
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/")
    (setq projectile-project-search-path '("~/Projects/")))
  (setq projectile-switch-project-action #'dw/switch-project-action))

(leaf counsel-projectile
  :ensure t
  :require t
  :after projectile
  :bind (("C-M-p" . counsel-projectile-find-file))
  :config
  (counsel-projectile-mode))

(leaf rainbow-delimiters
  :ensure t
  :require t
  :hook (prog-mode . rainbow-delimiters-mode)
  :hook (org-mode . rainbow-delimiters-mode))


