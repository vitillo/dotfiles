;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install custom packages

(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
			 ("gnu" .  "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(cider
                      jedi
                      company
                      elisp-slime-nav
                      exec-path-from-shell
                      flycheck
                      ace-jump-mode
                      magit
                      smartparens
                      helm
                      neotree
                      json-mode
                      gist
                      markdown-mode
                      guide-key
                      popwin
                      window-number
                      cyberpunk-theme
                      solarized-theme
                      zenburn-theme
                      multi-term
                      elscreen
                      win-switch
                      smart-mode-line
                      projectile
                      evil
                      evil-surround
                      evil-leader
                      evil-visualstar
                      evil-jumper
                      evil-nerd-commenter))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General settings

;; On OS X Emacs doesn't use the shell PATH if it's not started from
;; the shell. Let's fix that:
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Disable splash screen
(setq inhibit-startup-message t)

;; Enable uniquify
(require 'uniquify)

;; Shell-mode settings
(unless (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "/bin/zsh")
  (setq shell-file-name "/bin/zsh"))

;; Use srgb colorspace on OSX
(setq ns-use-srgb-colorspace t)

;; Disable scrollbar, toolbar and menubar
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Use abbreviations for yes/no menus
(defalias 'yes-or-no-p 'y-or-n-p)

;; Disable vc backend, slow as hell when loading and saving files
(require 'vc)
(setq vc-handled-backends ())

;; Personal data
(setq user-full-name "Roberto Agostino Vitillo")
(setq user-mail-address "ra.vitillo@gmail.com")

;; Save backup files in a dedicated directory
(setq backup-directory-alist '(("." . "~/.saves")))

;; Font
(when (member "Inconsolata" (font-family-list))
  (set-face-attribute 'default nil :font "Inconsolata-14"))

;; make the fringe thinner (default is 8 in pixels)
(fringe-mode 4)

;; show parenthesis match
(show-paren-mode 1)
(setq show-paren-style 'expression)

;; Toggle line highlighting in all buffers
(global-hl-line-mode t)

;; Start maximized
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("f0a99f53cbf7b004ba0c1760aa14fd70f2eabafe4e62a2b3cf5cabae8203113b" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(magit-use-overlays nil))

(setq default-directory (concat (getenv "HOME") "/"))

;; Disable bell
(setq ring-bell-function 'ignore)

;; Enable whitespace-mode
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))
(global-whitespace-mode +1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Switch command and alt key on OSX
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use solaraized color theme
(if (display-graphic-p)
  (progn
    (load-theme 'solarized-dark t)
    (setq solarized-distinct-fringe-background t)
    ;; make the modeline high contrast)
    (setq solarized-high-contrast-mode-line t)
    ;; Use less bolding
    (setq solarized-use-less-bold t)
    ;; Use more italics
    (setq solarized-use-more-italic t)
    ;; Use less colors for indicators such as git:gutter, flycheck and similar.
    (setq solarized-emphasize-indicators nil))
  (load-theme 'zenburn t))

(setq x-underline-at-descent-line t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Smartparens configuration

;;;;;;;;;
;; global
(require 'smartparens-config)
(sp-pair "'" nil :actions :rem)
(sp-pair "`" nil :actions :rem)

;; don't autoescape strings
(setq sp-autoescape-string-quote nil)

;; highlights matching pairs
(show-smartparens-global-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;
;; keybinding management

(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

(define-key sp-keymap (kbd "C-M-a") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-M-e") 'sp-end-of-sexp)

(define-key sp-keymap (kbd "C-M-p") 'sp-backward-up-sexp)
(define-key sp-keymap (kbd "C-M-n") 'sp-down-sexp)

(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

(define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

;;;;;;;;;;;;;;;;;;
;; pair management

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

;;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :bind "C-*")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))

;;; html-mode
(sp-with-modes '(html-mode sgml-mode)
  (sp-local-pair "<" ">"))

;;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "C-("))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Flycheck configuration
(global-flycheck-mode +1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Neotree configuration
(require 'neotree)

;; Hack to make neotree play ball with popwin
;; https://github.com/jaypei/emacs-neotree/issues/50#issuecomment-54249309
(when neo-persist-show
  (add-hook 'popwin:before-popup-hook
            (lambda () (setq neo-persist-show nil)))
  (add-hook 'popwin:after-popup-hook
            (lambda () (setq neo-persist-show t))))

;; Redefine keybinding for evil
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Guide-key configuration
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-c" "C-x"))
(setq guide-key/recursive-key-sequence-flag t)
(guide-key-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Multi-term configuration

(require 'multi-term)

(setq multi-term-program "/bin/zsh")
(global-set-key (kbd "C-x M-m") '(lambda ()(interactive)(multi-term)))

;; Forward navigation keys to the shell
(setq term-bind-key-alist
      (list
	(cons "C-c C-j" 'term-line-mode)
	(cons "C-c C-k" 'term-char-mode)
	(cons "C-c C-c" 'term-send-raw)
	(cons "C-p" 'term-send-raw)
	(cons "C-n" 'term-send-raw)
	(cons "C-a" 'term-send-raw)
	(cons "C-e" 'term-send-raw)
	(cons "M-b" 'term-send-backward-word)
	(cons "M-f" 'term-send-forward-word)
	(cons "M-d" 'term-send-forward-kill-word)
	(cons "C-k" 'term-send-raw)
	(cons "C-v" 'scroll-up-command)))

;; Make sure yanking works:
(add-hook 'term-mode-hook (lambda ()
			    (define-key term-raw-map (kbd "C-y") 'term-paste)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Window-number configuration
(require 'window-number)
(window-number-mode 1)
(window-number-meta-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Elscreen configuration

;; Change prefix key
(setq elscreen-prefix-key (kbd "C-c t"))

(elscreen-start)

;; Disable tabs display
(setq elscreen-display-tab nil)

;; Put tabs display in your frame title bar instead.
(defun elscreen-frame-title-update ()
  (when (elscreen-screen-modified-p 'elscreen-frame-title-update)
    (let* ((screen-list (sort (elscreen-get-screen-list) '<))
           (title (concat "| " (mapconcat
                                (lambda (screen)
                                  (let* ((label (elscreen-status-label screen))
                                         (label (if (string= label "+") label "")))
                                   (format "%s%d%s |"
                                          label
                                          (+ 1 screen)
                                          label)))
                                screen-list " "))))
      (if (fboundp 'set-frame-name)
          (set-frame-name title)
        (setq frame-title-format title)))))

(eval-after-load "elscreen"
  '(add-hook 'elscreen-screen-update-hook 'elscreen-frame-title-update))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Projectile configuration
(projectile-global-mode)

;; seems to be causing some issues and not loading correctly the files
;; (setq projectile-enable-caching t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helm configuration
(require 'helm-config)
(helm-mode t)
(setq helm-locate-command)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cider configuration
(require 'cider)

(setq cider-show-error-buffer nil)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Evil configuration
(setq evil-want-C-u-scroll t)

;; Enable evil mode
(evil-mode 1)

;; Use Emacs keybindings when in insert mode
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map [escape] 'evil-normal-state)

; Enable leader key
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "q" 'delete-window
  "u" 'undo-tree-visualize
  "o" 'neotree-toggle)

;; Set indentation level
(setq evil-shift-width 2)

;; Indent on new line in insert mode
;; (define-key evil-insert-state-map (kbd "RET") 'evil-ret-and-indent)

;; Redefine key to enter the command line
(define-key evil-normal-state-map (kbd ";") 'evil-ex)

;; Disable evil mode for the following modes
(add-hook 'eshell-mode-hook 'evil-emacs-state)
(add-hook 'term-mode-hook 'evil-emacs-state)
(add-hook 'cider-repl-mode-hook 'evil-emacs-state)

;; C-g as general purpose escape key sequence for evil.
(defun my-esc (prompt)
  "Functionality for escaping generally.  Includes exiting Evil insert state and C-g binding. "
  (cond
   ;; If we're in one of the Evil states that defines [escape] key, return [escape] so as
   ;; Key Lookup will use it.
   ((or (evil-insert-state-p) (evil-replace-state-p) (evil-visual-state-p)) [escape])
   ;; This is the best way I could infer for now to have C-c work during evil-read-key.
   ;; Note: As long as I return [escape] in normal-state, I don't need this.
   ;;((eq overriding-terminal-local-map evil-read-key-map) (keyboard-quit) (kbd ""))
   (t (kbd "C-g"))))

(define-key key-translation-map (kbd "C-g") 'my-esc)

;; Works around the fact that Evil uses read-event directly when in operator state, which
;; doesn't use the key-translation-map.
(define-key evil-operator-state-map (kbd "C-g") 'keyboard-quit)

;; Magit from avsej
(evil-add-hjkl-bindings magit-log-mode-map 'emacs)
(evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard-item
  "L" 'magit-key-mode-popup-logging)
(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard-item
  "l" 'magit-key-mode-popup-logging
  "h" 'magit-toggle-diff-refine-hunk)

;; Enable ace-jump mode with evil-mode
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

;; Remap 0 to ^
(define-key evil-motion-state-map "0" #'evil-first-non-blank-of-visual-line)

;; Enable evil-surround
(require 'evil-surround)
(global-evil-surround-mode 1)

;; Enable evil-jumper
(require 'evil-jumper)

;; Enable nerd-commenter
(evilnc-default-hotkeys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Win-switch configuration
(win-switch-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Popwin configuration
(require 'popwin)

(popwin-mode 1)

(setq popwin:special-display-config
      '(("*Help*" :height 20)
        ("*Completions*" :noselect t)
        ("*Messages*" :noselect t :height 20)
        ("*Apropos*" :noselect t :height 20)
        ("*compilation*" :noselect t)
        ("*Backtrace*" :height 20)
        ("*Messages*" :height 20)
        ("*Occur*" :noselect t)
        ("*Ido Completions*" :noselect t :height 20)
        ("\\*helm.*" :regexp t :height 20)
        ("*magit-commit*" :noselect t :height 40 :width 80 :stick t)
        ("*magit-diff*" :noselect t :height 40 :width 80)
        ("*magit-edit-log*" :noselect t :height 15 :width 80)
        ("\\*ansi-term\\*.*" :regexp t :height 20)
        ("*shell*" :height 20)
        (".*overtone.log" :regexp t :height 20)
        ("*gists*" :height 20)
        ("*sldb.*":regexp t :height 20)
        ("*cider-error*" :height 20 :stick t)
        ("*cider-doc*" :height 20)
        ("*cider-src*" :height 20)
        ("*cider-result*" :height 20)
        ("*cider-macroexpansion*" :height 20)
        ("\\*cider-repl.*" :regexp t :height 20 :stick t)
        ("*Kill Ring*" :height 20)
        ("*Compile-Log*" :height 20 :stick t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Smart-mode-line configuration
(require 'smart-mode-line)
(sml/setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Jedi configuration
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(jedi:install-server)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Company mode configuration
(global-company-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Globally enforced key-mappings

;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(define-key my-keys-minor-mode-map (kbd "M-h") 'win-switch-left)
(define-key my-keys-minor-mode-map (kbd "M-l") 'win-switch-right)
(define-key my-keys-minor-mode-map (kbd "M-k") 'win-switch-up)
(define-key my-keys-minor-mode-map (kbd "M-j") 'win-switch-down)
(define-key my-keys-minor-mode-map (kbd "M-o") 'switch-window)
(define-key my-keys-minor-mode-map (kbd "M-J") (lambda () (interactive) (enlarge-window 5)))
(define-key my-keys-minor-mode-map (kbd "M-K") (lambda () (interactive) (enlarge-window -5)))
(define-key my-keys-minor-mode-map (kbd "M-H") (lambda () (interactive) (enlarge-window -5 t)))
(define-key my-keys-minor-mode-map (kbd "M-L") (lambda () (interactive) (enlarge-window 5 t)))
(define-key my-keys-minor-mode-map (kbd "M-x") 'helm-M-x)
(define-key my-keys-minor-mode-map (kbd "C-x C-f") 'helm-find-files)
(define-key my-keys-minor-mode-map (kbd "C-x c o") 'helm-occur)
(define-key my-keys-minor-mode-map (kbd "C-x C-b") 'helm-buffers-list)
(define-key my-keys-minor-mode-map (kbd "C-x t") 'helm-elscreen)
(define-key my-keys-minor-mode-map (kbd "C-x m") 'multi-term-next)
(define-key my-keys-minor-mode-map (kbd "C-x g") 'magit-status)
(define-key my-keys-minor-mode-map (kbd "C-@") 'helm-buffers-list)
(define-key my-keys-minor-mode-map (kbd "C-SPC") 'helm-buffers-list)
(define-key my-keys-minor-mode-map (kbd "M-y") 'helm-show-kill-ring)
(define-key my-keys-minor-mode-map (kbd "C-c 1") (lambda () (interactive) (elscreen-goto 0)))
(define-key my-keys-minor-mode-map (kbd "C-c 2") (lambda () (interactive) (elscreen-goto 1)))
(define-key my-keys-minor-mode-map (kbd "C-c 3") (lambda () (interactive) (elscreen-goto 2)))
(define-key my-keys-minor-mode-map (kbd "C-c 4") (lambda () (interactive) (elscreen-goto 3)))
(define-key my-keys-minor-mode-map (kbd "C-c 5") (lambda () (interactive) (elscreen-goto 4)))
(define-key my-keys-minor-mode-map (kbd "C-c 6") (lambda () (interactive) (elscreen-goto 5)))
(define-key my-keys-minor-mode-map (kbd "C-c 7") (lambda () (interactive) (elscreen-goto 6)))
(define-key my-keys-minor-mode-map (kbd "C-c 8") (lambda () (interactive) (elscreen-goto 7)))
(define-key my-keys-minor-mode-map (kbd "C-c 9") (lambda () (interactive) (elscreen-goto 8)))

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Language specific settings

;; Use soft tabs
(setq-default indent-tabs-mode nil)

;; Python
(add-hook 'python-mode-hook (lambda ()
			      (modify-syntax-entry ?_ "w")
			      (modify-syntax-entry ?. "w")))

;; C/C++
(setq-default c-basic-offset 2)

(add-hook 'c-mode-common-hook (lambda ()
				(modify-syntax-entry ?_ "w")))

;;Javascript
(require 'js)
(setq js-indent-level 2)

;; Clojure
(add-hook 'clojure-mode-hook (lambda ()
                               (dolist (c (string-to-list ":_-?!#*/>"))
                                 (modify-syntax-entry c "w"))))
(add-hook 'clojure-mode-hook 'smartparens-mode)
(add-hook 'cider-repl-mode-hook 'smartparens-mode)

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (dolist (c (string-to-list ":_-?!#*/>"))
                                    (modify-syntax-entry c "w"))))
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (turn-on-eldoc-mode)
				  (setq mode-name "EL")))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)


(add-hook 'ielm-mode-hook (lambda ()
                            (run-hooks 'prelude-interactive-lisp-coding-hook)
			    (turn-on-eldoc-mode)))

;; enable elisp-slime-nav-mode
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
