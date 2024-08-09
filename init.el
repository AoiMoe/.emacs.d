;;; init.el --- setting for mule -*- Mode: Emacs-Lisp -*-
;;; Commentary:

;;; Code:

;;; package setting
(package-initialize)
(customize-set-variable 'package-archives
                        `(,@package-archives
                          ("melpa" . "https://melpa.org/packages/")))

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
(setq line-number-mode t)
(setq electric-indent-mode nil)
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(setq split-width-threshold nil)
(setq-default indent-tabs-mode nil)


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
                             '(width . 220)
                             '(height . 60)
                             ))
  (setq default-frame-alist (append my-frame-param default-frame-alist))
  (setq initial-frame-alist (append my-frame-param initial-frame-alist))
  (global-unset-key "\C-o")

  ;; tr-ime
  (unless (package-installed-p 'tr-ime)
    (package-refresh-contents)
    (package-install 'tr-ime))
  (tr-ime-advanced-install)
  ;; IM のデフォルトを IME に設定
  (setq default-input-method "W32-IME")
  ;; IME のモードライン表示設定
  (setq-default w32-ime-mode-line-state-indicator "[--]")
  (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]""[--]"))
  ;; init
  (w32-ime-initialize)
  ;; key
  (global-set-key "\C-o" 'toggle-input-method)
  (global-set-key [henkan] 'toggle-input-method)
  ;; IME 制御（yes/no などの入力の時に IME を off にする）
  (wrap-function-to-control-ime 'universal-argument t nil)
  (wrap-function-to-control-ime 'read-string nil nil)
  (wrap-function-to-control-ime 'read-char nil nil)
  (wrap-function-to-control-ime 'read-from-minibuffer nil nil)
  (wrap-function-to-control-ime 'y-or-n-p nil nil)
  (wrap-function-to-control-ime 'yes-or-no-p nil nil)
  (wrap-function-to-control-ime 'map-y-or-n-p nil nil)
  (wrap-function-to-control-ime 'register-read-with-preview nil nil)
  )

(when (eq system-type 'gnu/linux)
  (when window-system
    (setq my-frame-param (list
                          '(foreground-color . "black")
                          '(background-color . "white")
                          '(border-color . "black")
                          '(mouse-color . "white")
                          '(cursor-color . "red")
                          '(font . "fontset-standard")
                          '(width . 220)
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
    (global-set-key [henkan] 'toggle-input-method)
    )
  (add-to-list 'load-path (expand-file-name "~/.local/mozc-emacs/share/emacs/site-lisp/emacs-mozc"))
  (require 'mozc)
  (setq mozc-candidate-style 'echo-area)
  (setq default-input-method "japanese-mozc")
  (global-set-key "\C-o" 'toggle-input-method)
  )

(when (eq window-system 'mac)
  (setq my-frame-param (list
                        '(foreground-color . "black")
                        '(background-color . "white")
                        '(border-color . "black")
                        '(mouse-color . "white")
                        '(cursor-color . "red")
                        '(width . 220)
                        '(height . 60)
                        ))
  (setq default-frame-alist (append my-frame-param default-frame-alist))
  (setq initial-frame-alist (append my-frame-param initial-frame-alist))
  (setq-default line-spacing 2)
  (set-face-attribute 'default nil :family "Ricty Diminished" :height 180)
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty Diminished"))
  (set-fontset-font nil 'japanese-jisx0213.2004-1 (font-spec :family "Ricty Diminished"))
  (set-fontset-font nil 'japanese-jisx0213-2 (font-spec :family "Ricty Diminished"))
  (set-fontset-font nil 'katakana-jisx0201 (font-spec :family "Ricty Diminished"))
  (set-fontset-font nil '(#x0080 . #x024F) (font-spec :family "Ricty Diminished"))
  (set-fontset-font nil '(#x0370 . #x03FF) (font-spec :family "Ricty Diminished"))
  (define-key global-map [?¥] [?\\])
  (global-set-key [?\M-¥] 'delete-horizontal-space)
  )

;;; windmove
(global-set-key (kbd "C-S-h") 'windmove-left)
(global-set-key (kbd "C-S-l") 'windmove-right)
(global-set-key (kbd "C-S-k") 'windmove-up)
(global-set-key (kbd "C-S-j") 'windmove-down)

;;; ace-window
(use-package ace-window
  :ensure t
  :bind
  ("C-x o" . ace-window)
  :custom
  (aw-keys '(?j ?k ?l ?i ?o ?h ?y ?u ?p))
  (aw-ignore-current t)
  (aw-ignore-on nil)
  :custom-face
  (aw-leading-char-face ((t (:height 4.0 :foreground "red"))))
  )


;;; hiwin
(use-package hiwin
  :ensure t
  :config
  (set-face-background 'hiwin-face "#F0F0F0")
  (set-face-extend 'hiwin-face t)
  (setq hiwin-always-active-buffer-name-regexp "^\\(\\*helm\\| \\*LV\\*\\)")
  :init
  (hiwin-activate)
  )

;;; which-key
(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  )

;;; company
(use-package company
  :ensure t
  :after company-statistics
  :bind (("M-<tab>" . company-complete)
         :map company-active-map
         ;; C-n, C-pで補完候補を次/前の候補を選択
         ("M-n" . nil)                      ;; M-nで次の候補への移動をキャンセル
         ("M-p" . nil)                      ;; M-pでの前の候補への移動をキャンセル
         ("C-n" . company-select-next)      ;; 次の補完候補を選択
         ("C-p" . company-select-previous);; 前の補完候補を選択
         ("C-s" . company-filter-candidates) ;; C-sで絞り込む
         :map company-search-map
         ;; 検索候補の移動をC-nとC-pで移動する
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :init
  ;; 全バッファで有効にする
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (define-key emacs-lisp-mode-map (kbd "C-M-i") nil) ;; CUI版のためにemacs-lisp-modeでバインドされるC-M-iをアンバインド
  (global-set-key (kbd "C-M-i") 'company-complete)   ;; CUI版ではM-<tab>はC-M-iに変換されるのでそれを利用
  (setq completion-ignore-case t)
  (setq company-idle-delay 0)                    ;; 待ち時間を0秒にする
  (setq company-minimum-prefix-length 2)         ;; 補完できそうな文字が2文字以上入力されたら候補を表示
  (setq company-selection-wrap-around t)         ;; 候補の一番下でさらに下に行こうとすると一番上に戻る
  (setq company-transformers '(company-sort-by-occurrence company-sort-by-backend-importance))) ;; 利用頻度が高いものを候補の上に表示する

(use-package company-statistics
  :ensure t
  :init
  (company-statistics-mode))


;;; treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "C-x C-o") #'treemacs-select-window))
  :custom
  (treemacs-width 50)
  :bind
  (:map global-map
        ("C-x C-o"   . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  )


;; paren
(use-package paren
  :ensure t
  :hook
  (after-init . show-paren-mode)
  :custom
  (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  (set-face-extend 'show-paren-match t)
  :custom-face
  (show-paren-match ((nil (:background "#e0e0e0"))))
  )


;;; magit
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  )


;;; lsp
(use-package lsp-mode
  :ensure t
  :config
  (defun lsp-mode-init ()
    (lsp)
    (global-set-key (kbd "M-*") 'xref-pop-marker-stack)
    (global-set-key (kbd "M-.") 'xref-find-definitions)
    (global-set-key (kbd "M-/") 'xref-find-references))
  (setq lsp-prefer-capf t)
  :init
  (setq lsp-keymap-prefix "C-c C-l")
  )
(use-package lsp-ui
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'at-point) ;; top, bottom, or at-point
  (lsp-ui-doc-max-width 150)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-peek-enable t)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit (not (eq window-system 'mac)))
  (lsp-ui-doc-show-with-cursor nil)
  :preface
  (defun toggle-lsp-ui-doc ()
    (interactive)
    (if lsp-ui-doc-show-with-cursor
        (progn
          (setq lsp-ui-doc-show-with-cursor nil)
          (lsp-ui-doc--hide-frame))
         (setq lsp-ui-doc-show-with-cursor t)))
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :bind
  (:map lsp-mode-map
        ("C-c C-r" . lsp-ui-peek-find-references)
        ("C-c C-j" . lsp-ui-peek-find-definitions)
        ("C-c i"   . lsp-ui-peek-find-implementation)
        ("C-c m"   . lsp-ui-imenu)
        ("C-c s"   . lsp-ui-sideline-mode)
        ("C-c d"   . toggle-lsp-ui-doc)
        )
  )
(use-package lsp-treemacs 
  :ensure t
  :commands lsp-treemacs-errors-list
  )

;;; flycheck
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-erlang-include-path '("../include" ".." "../deps" "../_build/default/lib")
        flycheck-erlang-executable (expand-file-name "~/erlang/otp/bin/erlc")
        flycheck-check-syntax-automatically '(save)))


;;; rust
(use-package rust-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  )

(use-package company-racer
  :ensure t
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
(use-package erlang
  :ensure t
  :if (file-directory-p "~/erlang/tools/emacs")
  :load-path "~/erlang/tools/emacs"
  :init
  (setq erlang-root-dir "~/erlang/otp")
  (setq exec-path (cons "~/erlang/otp/bin" exec-path))
  (add-hook 'erlang-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (setq erlang-indent-guard 4)
              (setq erlang-argument-indent 4)))
  )


;;; golang
(use-package go-mode
  :ensure t
  :init
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'before-save-hook 'gofmt-before-save)
  )

;;; protobuf
(use-package protobuf-mode
  :ensure t
  )

;;;
(use-package helm
  :ensure t
  :config
;  (require 'helm-config)
  (global-set-key (kbd "C-c h") 'helm-mini)
  (global-set-key (kbd "C-c y") 'helm-show-kill-ring)
  )


;;;
;; (use-package auto-complete-c-headers
;;   :config
;;   (add-to-list 'ac-sources 'ac-source-c-headers)
;;   (global-auto-complete-mode t)
;;   (define-key ac-completing-map (kbd "C-n") 'ac-next)
;;   (define-key ac-completing-map (kbd "C-p") 'ac-previous)
;;   (define-key ac-completing-map (kbd "C-j") 'ac-complete)
;;   (setq ac-auto-start nil)
;;   (ac-set-trigger-key "TAB")
;;   )


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
  :ensure t
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

;;; Lua
(use-package lua-mode
  :ensure t
  :init
  (add-hook 'lua-mode-hook
            (lambda()
              (setq indent-tabs-mode nil)
              (setq lua-indent-level 2)))
  )

;;; Customs:
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:height 4.0 :foreground "red"))) t)
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
 '(my-face-warn-zenkaku ((t (:background "gray"))) t)
 '(show-paren-match ((nil (:background "#e0e0e0")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style "knf")
 '(display-time-mode t)
 '(dtrt-indent-min-soft-tab-superiority 5000.0)
 '(dtrt-indent-mode t nil (dtrt-indent))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(company-statistics hiwin lsp-treemacs xref ht magit protobuf-mode treemacs which-key neotree company go-mode lsp-ui lsp-mode lua-mode rust-mode auto-complete-c-headers helm flycheck use-package))
 '(ruby-insert-encoding-magic-comment nil)
 '(safe-local-variable-values
   '((flycheck-gcc-include-path quote
                                (".."))
     (flycheck-gcc-include quote
                           (".."))
     (tab-stop . 4)
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
     (c-auto-newline)))
 '(tool-bar-mode nil))

(provide 'init)
;;; init.el ends here
