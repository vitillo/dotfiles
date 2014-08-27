;; Install packages
(require 'package)

(setq package-archives '(("melpa" .  "http://melpa.milkbox.net/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)
(package-refresh-contents)

(defvar my-packages '(neotree
		      elscreen
                      multi-term
                      projectile
                      solarized-theme
                      evil
                      surround
		      evil-leader
		      evil-paredit))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Start maximized
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Setup projectile
(projectile-global-mode)

;; Setup elscreen
(elscreen-start)

(setq elscreen-prefix-key "\C-a")

;; disable tabs display
(setq elscreen-display-tab nil)

;; Put tabs display in your frame title bar instead.
(defun elscreen-frame-title-update ()
  (when (elscreen-screen-modified-p 'elscreen-frame-title-update)
    (let* ((screen-list (sort (elscreen-get-screen-list) '<))
           (screen-to-name-alist (elscreen-get-screen-to-name-alist))
           (title (concat "| " (mapconcat
                   (lambda (screen)
                     (format "%d%s %s |"
                             screen (elscreen-status-label screen)
                             (get-alist screen screen-to-name-alist)))
                   screen-list " "))))
      (if (fboundp 'set-frame-name)
          (set-frame-name title)
        (setq frame-title-format title)))))

(eval-after-load "elscreen"
  '(add-hook 'elscreen-screen-update-hook 'elscreen-frame-title-update))

;; Setup evil

; Enable scrolling with C-u, must be called before toggling evil-mode
(setq evil-want-C-u-scroll t)

; Enable evil mode
(evil-mode 1)

; Enable leader key
(global-evil-leader-mode)

; Redefine key to enter the command line
(define-key evil-normal-state-map (kbd ";") 'evil-ex)

; Define leader mappings
(evil-leader/set-leader ",")
(evil-leader/set-key
  "o" 'neotree-toggle
  "e" 'ido-switch-buffer
  "q" 'delete-window
  "j" 'ace-jump-mode
  "u" 'undo-tree-visualize)

; Disable evil mode for the following modes
(add-hook 'eshell-mode-hook 'evil-emacs-state)
(add-hook 'term-mode-hook 'evil-emacs-state)
(add-hook 'term-mode-hook 'yas-minor-mode)

; Enable evil-surround
(require 'surround)
(global-surround-mode 1)

;; Neotree configuration
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;; Setup multi-term
(require 'multi-term)
(setq multi-term-program "/bin/zsh")

;; Use solaraized colort theme
(color-theme-solarized-dark)

;; Switch command and alt key on OSX
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; ansi-term
(global-set-key "\C-x\C-s" '(lambda ()(interactive)(multi-term)))

;;; C-c as general purpose escape key sequence for evil.
;;;
(defun my-esc (prompt)
  "Functionality for escaping generally.  Includes exiting Evil insert state and C-g binding. "
  (cond
   ;; If we're in one of the Evil states that defines [escape] key, return [escape] so as
   ;; Key Lookup will use it.
   ((or (evil-insert-state-p) (evil-replace-state-p) (evil-visual-state-p)) [escape])
   ;; This is the best way I could infer for now to have C-c work during evil-read-key.
   ;; Note: As long as I return [escape] in normal-state, I don't need this.
   ;;((eq overriding-terminal-local-map evil-read-key-map) (keyboard-quit) (kbd ""))
   (t (kbd "C-c"))))
(define-key key-translation-map (kbd "C-c") 'my-esc)
;; Works around the fact that Evil uses read-event directly when in operator state, which
;; doesn't use the key-translation-map.
(define-key evil-operator-state-map (kbd "C-c") 'keyboard-quit)

;; Window resize function
(defun win-resize-top-or-bot ()
  "Figure out if the current window is on top, bottom or in the
middle"
  (let* ((win-edges (window-edges))
	 (this-window-y-min (nth 1 win-edges))
	 (this-window-y-max (nth 3 win-edges))
	 (fr-height (frame-height)))
    (cond
     ((eq 0 this-window-y-min) "top")
     ((eq (- fr-height 1) this-window-y-max) "bot")
     (t "mid"))))

(defun win-resize-left-or-right ()
  "Figure out if the current window is to the left, right or in the
middle"
  (let* ((win-edges (window-edges))
	 (this-window-x-min (nth 0 win-edges))
	 (this-window-x-max (nth 2 win-edges))
	 (fr-width (frame-width)))
    (cond
     ((eq 0 this-window-x-min) "left")
     ((eq (+ fr-width 4) this-window-x-max) "right")
     (t "mid"))))

(defun win-resize-enlarge-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window -10))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window 10))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window -10))
   (t (message "nil"))))

(defun win-resize-minimize-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window 10))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window -10))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window 10))
   (t (message "nil"))))

(defun win-resize-enlarge-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally -10))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally 10))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally -10))))

(defun win-resize-minimize-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally 10))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally -10))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally 10))))

;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(define-key my-keys-minor-mode-map (kbd "C-h") 'win-switch-left)
(define-key my-keys-minor-mode-map (kbd "C-l") 'win-switch-right)
(define-key my-keys-minor-mode-map (kbd "C-k") 'win-switch-up)
(define-key my-keys-minor-mode-map (kbd "C-j") 'win-switch-down)
(define-key my-keys-minor-mode-map (kbd "C-=") 'elscreen-next)
(define-key my-keys-minor-mode-map (kbd "C--") 'elscreen-previous)
(define-key my-keys-minor-mode-map (kbd "M-t") 'elscreen-create)
(define-key my-keys-minor-mode-map (kbd "C-M-k") 'win-resize-minimize-vert)
(define-key my-keys-minor-mode-map (kbd "C-M-j") 'win-resize-enlarge-vert)
(define-key my-keys-minor-mode-map (kbd "C-M-h") 'win-resize-minimize-horiz)
(define-key my-keys-minor-mode-map (kbd "C-M-l") 'win-resize-enlarge-horiz)
(define-key my-keys-minor-mode-map (kbd "C-M-k") 'win-resize-enlarge-horiz)
(define-key my-keys-minor-mode-map (kbd "C-M-j") 'win-resize-minimize-horiz)
(define-key my-keys-minor-mode-map (kbd "C-M-h") 'win-resize-enlarge-vert)
(define-key my-keys-minor-mode-map (kbd "C-M-l") 'win-resize-minimize-vert)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

