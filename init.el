;;; init.el --- setting for mule -*- Mode: Emacs-Lisp -*-
;;; Commentary:

;;; Code:

;;; package setting
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("elpa-gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile (require 'use-package))


;;; font lock
(global-font-lock-mode t)
(make-face 'my-face-warn-zenkaku)
(make-face 'my-face-warn-over-columns)
(make-face 'my-face-warn)
(make-face 'my-face-tab)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (progn
    (font-lock-add-keywords
     major-mode
     '(
       ("\t" 0 'my-face-tab append)
       ("　" 0 'my-face-warn-zenkaku append)
       (" +\t" 0 'my-face-warn append)
       ("[ \t]+$" 0 'my-face-warn append)
       ))
    (font-lock-add-keywords
     'erlang-mode
     '(
       ;; warning line exceeds 120 columns
       ("^[^\n]\\{120\\}\\(.*\\)$" 1 'my-face-warn-over-columns t)
       ;; warning spaces just after brackets
       ("\\(\\({\\|\\[\\|(\\)[[:space:]]+\\)[^[:space:]\n]" 1 'my-face-warn t)
       ;; warning spaces just before brackets
       ("[^[:space:]\n]\\([[:space:]]+\\(}\\|\\]\\|)\\)\\)" 1 'my-face-warn t)
       ;; warning no space just after comma
       (",[^[:space:]*?\n]" 0 'my-face-warn t)
       ))
    ))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)


;;; key bindings
(load-library "term/bobcat")
(terminal-init-bobcat)

;; swap \C-w and \M-w
(global-set-key "\C-w" 'kill-ring-save)
(global-set-key "\M-w" 'kill-region)

(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(global-set-key [M-v] 'scroll-down)
(global-set-key "\C-z" 'scroll-down)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-c\C-g" 'keyboard-quit)
(global-set-key [C-tab] 'dabbrev-expand)
(global-set-key "\C-]" 'dabbrev-expand)
(global-unset-key [mouse-1])
(global-unset-key [down-mouse-1])
(global-unset-key [drag-mouse-1])
(global-unset-key [double-mouse-1])
(global-unset-key [double-down-mouse-1])
(global-unset-key [triple-mouse-1])
(global-unset-key [triple-down-mouse-1])

(define-key Buffer-menu-mode-map " " 'Buffer-menu-other-window)

(global-unset-key "\C-x6")
(global-set-key "\C-x6" 'make-frame-on-display)


;;; misc global settings
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(setq next-line-add-newlines nil)
(setq diff-switches "-u")
(display-time)
(setq line-number-mode t)
(setq electric-indent-mode nil)
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)


;;; cc-mode
(load "cc-mode")
(setq auto-mode-alist
      (append '(("\\.C$"  . c++-mode)
                ("\\.cc$" . c++-mode)
                ("\\.H$" . c++-mode)
                ("\\.hh$" . c++-mode)
                ("\\.c$"  . c++-mode)   ; to edit C code
                ("\\.h$"  . c++-mode)   ; to edit C code
                ("\\.tex$" . latex-mode)
                ("\\.perl$" . perl-mode)
                ("\\.pl$" . perl-mode)
                ("\\.prl$" . perl-mode)
                ("/[Mm]akefile$" . makefile-mode)
                ("\\.dbsml$" . indented-text-mode)
                ("\\.bsml$" . indented-text-mode)
                ) auto-mode-alist))

(setq c-style-alist
      (append
       '(("ts"
          (c-offsets-alist . ((statement-block-intro . +)
                              (knr-argdecl-intro . 5)
                              (substatement-open . +)
                              (label . 0)
                              (statement-case-open . +)
                              (statement-cont . +)
                              (inline-open . 0)
                              )))
         ("knf"
          (c-auto-newline . nil)
          (c-recognize-knr-p . t)
          (c-basic-offset . 8)
          (indent-tabs-mode . t)
          (c-comment-only-line-offset . 0)
          (c-cleanup-list . (brace-else-brace
                             empty-defun-braces
                             defun-close-semi
                             list-close-comma
                             scope-operator))
          (c-hanging-braces-alist . ((defun-open . (before after))
                                     (defun-close . (before))
                                     (class-open . (after))
                                     (class-close . nil)
;                                     (inline-open . nil)
;                                     (inline-close . nil)
;                                     (block-open . (after))
;                                     (block-close . (before))
                                     (substatement-open . nil)
                                     (statement-case-open . nil)
                                     (brace-list-open . nil)
                                     (brace-list-close . nil)
                                     (brace-list-intro . nil)
                                     (brace-list-entry . nil)
                                     ))
          (c-offsets-alist . ((knr-argdecl-intro . +)
;                              (arglist-cont-nonempty . 4)
                              (knr-argdecl . 0)
;                              (block-open . -)
                              (label . -9999)
                              (statement-cont . 4)
                              (inline-open . 0)
                              (namespace-open . 0)
                              (namespace-close . 0)
                              (innamespace . 0)
                              )))
         ("knf4"
          (c-auto-newline . nil)
          (c-recognize-knr-p . t)
          (c-basic-offset . 4)
          (tab-width . 4)
          (indent-tabs-mode . t)
          (c-comment-only-line-offset . 0)
          (c-cleanup-list . (brace-else-brace
                             empty-defun-braces
                             defun-close-semi
                             list-close-comma
                             scope-operator))
          (c-hanging-braces-alist . ((defun-open . (before after))
                                     (defun-close . (before))
                                     (class-open . (after))
                                     (class-close . nil)
;                                     (inline-open . nil)
;                                     (inline-close . nil)
;                                     (block-open . (after))
;                                     (block-close . (before))
                                     (substatement-open . nil)
                                     (statement-case-open . nil)
                                     (brace-list-open . nil)
                                     (brace-list-close . nil)
                                     (brace-list-intro . nil)
                                     (brace-list-entry . nil)
                                     ))
          (c-offsets-alist . ((knr-argdecl-intro . +)
;                              (arglist-cont-nonempty . 4)
                              (knr-argdecl . 0)
;                              (block-open . -)
                              (label . -9999)
                              (statement-cont . 4)
                              (inline-open . 0)
                              (namespace-open . 0)
                              (namespace-close . 0)
                              (innamespace . 0)
                              )))
         ) c-style-alist))

(defvar c-set-default-style-history nil)
;(setq c-default-style "knf")
(defun c-set-default-style (style)
  "Set default style value to STYLE for C."
  (interactive (list (let ((completion-ignore-case t)
                           (prompt "Which C style? "))
                       (completing-read prompt c-style-alist nil t
                                        (cons c-default-style 0)
                                        'c-set-default-style-history))))
  (custom-set-variables
   '(c-default-style style)))
(global-set-key "\C-c," 'c-set-default-style)
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key "(" 'self-insert-command)
            (local-set-key ")" 'self-insert-command)
            (local-set-key ";" 'self-insert-command)
            (local-set-key "," 'self-insert-command)
            (local-set-key "*" 'self-insert-command)
            (local-set-key "/" 'self-insert-command)
            ))


;;; window system settings
(tool-bar-mode 0)
(when (eq system-type 'windows-nt)
  (set-face-attribute 'default nil
                      :family "ＭＳ ゴシック"
                      :height 130)
  (set-fontset-font "fontset-default"
                    'japanese-jisx0208
                    '("ＭＳ ゴシック" . "jisx0208-sjis"))

  (set-clipboard-coding-system 'sjis-dos)
  (set-w32-system-coding-system 'sjis)
  (set-keyboard-coding-system 'sjis)
  (setq my-frame-param (list '(foreground-color . "black")
                             '(background-color . "white")
                             '(border-color . "black")
                             '(mouse-color . "white")
                             '(cursor-color . "black")
                             '(width . 160)
                             '(height . 50)
                             ))
  (setq default-frame-alist (append my-frame-param default-frame-alist))
  (setq initial-frame-alist (append my-frame-param initial-frame-alist))
  (global-unset-key "\C-o")
  (when (functionp 'mw32-ime-initialize)
    (setq-default mw32-ime-mode-line-state-indicator "[--]")
    (setq mw32-ime-mode-line-state-indicator-list '("[--]" "[あ]""[--]"))
    (set-input-method "MW32-IME")
    (mw32-ime-initialize))
  )

(when (eq system-type 'gnu/linux)
  (setq my-frame-param (list
                        '(foreground-color . "black")
                        '(background-color . "white")
                        '(border-color . "black")
                        '(mouse-color . "white")
                        '(cursor-color . "red")
                        '(font . "fontset-standard")
                        '(width . 160)
                        '(height . 60)
                        ))
  (setq default-frame-alist (append my-frame-param default-frame-alist))
  (setq initial-frame-alist (append my-frame-param initial-frame-alist))
  (setq-default line-spacing 2)
  (set-fontset-font "fontset-standard"
                    'ascii
                    (font-spec :family "RictyDiminished" :size 14) nil 'prepend)
  (set-fontset-font "fontset-standard"
                    'japanese-jisx0213.2004-1
                    (font-spec :family "RictyDiminished") nil 'prepend)
  (require 'mozc)
  (setq default-input-method "japanese-mozc")
  (global-set-key "\C-o" 'toggle-input-method)
  )


;;; flycheck
(use-package flycheck
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-erlang-include-path '("../include")
        flycheck-erlang-library-path '()
        flycheck-check-syntax-automatically '(save)))


;;; rust
(use-package company-racer
  :if (file-directory-p "~/.cargo/bin")
  :config
  (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-racer))
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'rust-mode-hook #'flycheck-rust-setup)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq company-tooltip-align-annotations t)
  )


;;; erlang
(use-package erlang-start
  :if (file-directory-p "~/erlang/tools/emacs")
  :load-path "~/erlang/tools/emacs"
  :init
  (setq erlang-root-dir "~/erlang/otp")
  (setq exec-path (cons "~/erlang/otp/bin" exec-path))
  )


;;;
(use-package helm
  :config
  (require 'helm-config)
  (global-set-key (kbd "C-c h") 'helm-mini)
  (global-set-key (kbd "C-c y") 'helm-show-kill-ring)
  )


;;;
(use-package auto-complete-c-headers
  :config
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (global-auto-complete-mode t)
  (define-key ac-completing-map (kbd "C-n") 'ac-next)
  (define-key ac-completing-map (kbd "C-p") 'ac-previous)
  (define-key ac-completing-map (kbd "C-j") 'ac-complete)
  (setq ac-auto-start nil)
  (ac-set-trigger-key "TAB")
  )


;;;
;;; http://gifnksm.hatenablog.jp/entry/20100131/1264956220
;;;
(defun beginning-of-indented-line (current-point)
  "インデント文字を飛ばした行頭に戻る.  ただし, CURRENT-POINT から行頭までの間にインデント文字しかない場合は, 行頭に戻る."
  (interactive "d")
  (if (string-match
       "^[ \t]+$"
       (save-excursion
         (buffer-substring-no-properties
          (progn (beginning-of-line) (point))
          current-point)))
      (beginning-of-line)
    (back-to-indentation)))

(defun beginning-of-visual-indented-line (current-point)
  "インデント文字を飛ばした行頭に戻る.  ただし, CURRENT-POINT から行頭までの間にインデント文字しかない場合は, 行頭に戻る."
  (interactive "d")
  (let ((vhead-pos (save-excursion (progn (beginning-of-visual-line) (point))))
        (head-pos (save-excursion (progn (beginning-of-line) (point)))))
    (cond
     ;; 物理行の1行目にいる場合
     ((eq vhead-pos head-pos)
      (if (string-match
           "^[ \t]+$"
           (buffer-substring-no-properties vhead-pos current-point))
          (beginning-of-visual-line)
        (back-to-indentation)))
     ;; 物理行の2行目以降の先頭にいる場合
     ((eq vhead-pos current-point)
      (backward-char)
      (beginning-of-visual-indented-line (point)))
     ;; 物理行の2行目以降の途中にいる場合
     (t (beginning-of-visual-line)))))
(global-set-key "\C-a" 'beginning-of-visual-indented-line)
(global-set-key "\C-e" 'end-of-visual-line)


;;; dtrt-indent --- guess indentation.
(use-package dtrt-indent
  :load-path "~/.emacs.d/site-lisp/dtrt-indent"
  :config
  (progn
    (defcustom guess-style-lighter-format-func
      'guess-style-lighter-default-format-func
      "*Function used for formatting the lighter in `guess-style-info-mode'.
This has to be a function that takes no arguments and returns a info string
for the current buffer."
      :group 'guess-style
      :type 'function)

    (defun guess-style-lighter-default-format-func ()
      (concat (when (boundp 'c-basic-offset) (format " >%d" c-basic-offset))
	      " " (if indent-tabs-mode (format "t%d" tab-width) "spc")))

    (define-minor-mode guess-style-info-mode
      "Minor mode to show guessed variables in the mode-line.
Customize `guess-style-lighter-format-func' to change the variables."
      nil nil nil)

    (define-globalized-minor-mode global-guess-style-info-mode
      guess-style-info-mode (lambda () (guess-style-info-mode 1)))

    ;; providing a lighter in `define-minor-mode' doesn't allow :eval forms
    (add-to-list 'minor-mode-alist
		 '(guess-style-info-mode
		   ((:eval (funcall guess-style-lighter-format-func)))))
    (global-guess-style-info-mode 1))
  )


;;; Customs:
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "Firebrick"))))
 '(font-lock-constant-face ((t (:foreground "DarkOrange3"))))
 '(font-lock-function-name-face ((t (:foreground "Blue"))))
 '(font-lock-keyword-face ((t (:foreground "#009999"))))
 '(font-lock-string-face ((t (:foreground "Gray50"))))
 '(font-lock-type-face ((t (:foreground "#00AA00"))))
 '(font-lock-variable-name-face ((t (:foreground "#886600"))))
 '(my-face-tab ((t (:foreground "#DDFFFF" :underline t))) t)
 '(my-face-warn ((t (:background "#FFAAAA" :underline t))) t)
 '(my-face-warn-over-columns ((t (:background "gray"))) t)
 '(my-face-warn-zenkaku ((t (:background "gray"))) t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style "knf")
 '(dtrt-indent-min-soft-tab-superiority 5000.0)
 '(dtrt-indent-mode t nil (dtrt-indent))
 '(safe-local-variable-values
   (quote
    ((tab-stop . 4)
     (c-offsets-alist
      (knr-argdecl-intro . +)
      (knr-argdecl . 0)
      (label . -9999)
      (statement-cont . 4)
      (inline-open . 0))
     (c-offsets-alist
      (knr-argdecl-intro . +)
      (knr-argdecl . 0)
      (label . -9999)
      (statement-cont . 4)
      (namespace-open . 0)
      (namespace-close . 0)
      (innamespace . 0))
     (c-offsets-alist
      (knr-argdecl-intro . +)
      (knr-argdecl . 0)
      (label . -9999)
      (statement-cont . 4)
      (inline-open . 0)
      (namespace-open . 0)
      (namespace-close . 0)
      (innamespace . 0))
     (c-hanging-braces-alist
      (defun-open before after)
      (defun-close before)
      (class-open after)
      (class-close)
      (substatement-open)
      (statement-case-open)
      (brace-list-open)
      (brace-list-close)
      (brace-list-intro)
      (brace-list-entry))
     (c-cleanup-list brace-else-brace empty-defun-braces defun-close-semi list-close-comma scope-operator)
     (c-comment-only-line-offset . 0)
     (c-recognize-knr-p . t)
     (c-auto-newline)))))

(provide 'init)
;;; init.el ends here
