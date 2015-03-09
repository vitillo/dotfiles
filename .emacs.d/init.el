;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install custom packages

(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
			 ("gnu" .  "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(perspective
                      ein-mumamo
                      scala-mode2
                      ensime
                      cider
                      jedi
                      ess
                      company
                      elisp-slime-nav
                      exec-path-from-shell
                      flycheck
                      ace-jump-mode
                      magit
                      smartparens
                      projectile
                      helm
                      helm-projectile
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
                      win-switch
                      smart-mode-line
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

;; Start maximized
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(magit-use-overlays nil)
 '(persp-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq default-directory (concat (getenv "HOME") "/"))

;; Disable bell
(setq ring-bell-function 'ignore)

;; Enable whitespace-mode
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face trailing lines-tail))
(global-whitespace-mode +1)

;; Mouse support
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))

;; Use zenburn theme
(load-theme 'zenburn t)

(setq x-underline-at-descent-line t)

;; Disable annoying warnings when loading theme
(setq sml/no-confirm-load-theme t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Switch command and alt key on OSX
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Smartparens configuration

;; global
(require 'smartparens-config)
(sp-pair "'" nil :actions :rem)
(sp-pair "`" nil :actions :rem)

;; don't autoescape strings
(setq sp-autoescape-string-quote nil)

;; highlights matching pairs
(show-smartparens-global-mode t)

;; keybinding management

(define-key sp-keymap (kbd "C-M-f") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-previous-sexp)

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

;; pair management

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :bind "C-*")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))

;; html-mode
(sp-with-modes '(html-mode sgml-mode)
  (sp-local-pair "<" ">"))

;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "C-("))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Flycheck configuration
(global-flycheck-mode +1)

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
  "u" 'undo-tree-visualize)

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

;; Switch 0 and ^
(define-key evil-motion-state-map "0" #'evil-first-non-blank-of-visual-line)
(define-key evil-motion-state-map "^" #'evil-beginning-of-line)

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
        ("\\*sbt.*" :regexp t :height 20 :stick t)
        ("\\*ensime.*" :regexp t :height 20 :stick t)
        ("*Kill Ring*" :height 20)
        ("*Compile-Log*" :height 20 :stick t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Smart-mode-line configuration
(require 'smart-mode-line)
(sml/setup)
(sml/apply-theme 'dark)
(setq rm-blacklist '(" my-keys" " Helm" " Guide" " WS" " Undo-Tree" " company"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Jedi configuration
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(jedi:install-server)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Company mode configuration
(global-company-mode 1)
(setq company-idle-delay 0)
(setq company-dabbrev-downcase nil)
;;(setq company-backends '(completion-at-point-functions company-dabbrev company-css company-cmake company-files))

;; cycle with tab
(setq company-selection-wrap-around t)
(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "C-p") 'company-select-previous)
     (define-key company-active-map (kbd "C-n") 'company-select-next)
     (define-key company-active-map (kbd "TAB") 'company-select-next)
     (define-key company-active-map [tab] 'company-select-next)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Window-number configuration
(require 'window-number)
(window-number-mode )
(window-number-meta-mode 1)

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
(define-key my-keys-minor-mode-map (kbd "C-x m") 'multi-term-next)
(define-key my-keys-minor-mode-map (kbd "C-x g") 'magit-status)
(define-key my-keys-minor-mode-map (kbd "C-@") 'helm-buffers-list)
(define-key my-keys-minor-mode-map (kbd "C-SPC") 'helm-buffers-list)
(define-key my-keys-minor-mode-map (kbd "M-y") 'helm-show-kill-ring)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Language specific settings

;; Use soft tabs
(setq-default indent-tabs-mode nil)

;; R
(require 'ess-site)
(add-hook 'ess-mode-hook (lambda ()
                           (company-mode -1)))

;; Python
(add-hook 'python-mode-hook (lambda ()
			      (modify-syntax-entry ?_ "w")
			      (modify-syntax-entry ?. "w")))

;; C/C++
(setq-default c-basic-offset 2)

(add-hook 'c-mode-common-hook (lambda ()
				(modify-syntax-entry ?_ "w")))

;;Javascript
(setq js-indent-level 2)
(add-hook 'js2-mode-hook (lambda ()
                           (dolist (c (string-to-list "_"))
                             (modify-syntax-entry c "w"))))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsm\\'" . js2-mode))

;; Clojure
(add-hook 'clojure-mode-hook (lambda ()
                               (smartparens-mode)
                               (dolist (c (string-to-list ":_-?!#*/>"))
                                 (modify-syntax-entry c "w"))))
(add-hook 'cider-repl-mode-hook 'smartparens-mode)

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (smartparens-mode)
                                  (turn-on-eldoc-mode)
                                  (setq mode-name "EL")
                                  (dolist (c (string-to-list ":_-?!#*/>"))
                                    (modify-syntax-entry c "w"))))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

(add-hook 'ielm-mode-hook (lambda ()
                            (run-hooks 'prelude-interactive-lisp-coding-hook)
			    (turn-on-eldoc-mode)))

;; Scala
(require 'ensime)
(add-hook 'scala-mode-hook (lambda ()
                             (ensime-scala-mode-hook)
                             (setq ensime-sbt-compile-on-save nil)))


;; enable elisp-slime-nav-mode
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
