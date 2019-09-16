;;; basic-config.el --- My emacs minimal configuration -*- lexical-binding: t -*-

;; Produce backtraces when errors occur
(setq debug-on-error t)

;;;; startup
;; Speed up startup
(setq gc-cons-threshold 80000000) ;;80MB
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 800000) ;;800KB
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))))

;; Do not use garbage-collect when use minibuffer
;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun eye-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun eye-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'eye-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'eye-minibuffer-exit-hook)


;;
;; 启动时间统计
;;
;; 自定义计算时间
(defvar init-start (current-time))
(add-hook 'after-init-hook
          (lambda ()
            (message (format "\n\nEmacs startup time: %.6f seconds.\n\n\n" (- (float-time (current-time)) (float-time init-start))))
            ))

;;;; system env
(setq is-windows (or
		  (eq system-type 'windows-nt)
		  (eq system-type 'cygwin)))
(setq is-linux (eq system-type 'gnu/linux))
(setq is-mac (eq system-type 'darwin))

(setq is-gui (display-graphic-p))
(setq is-terminal (not (display-graphic-p)))

;; 使用 emacsclient 需要先启动服务
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (if (not (equal t (server-running-p)))
		(server-start))))

;;;; load custom-file before all init-* config
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(unless (file-exists-p custom-file)
					;(f-touch custom-file)
  (shell-command (concat "touch " custom-file))
  )
(load custom-file t t)

;;;; packages
(require 'package)
(setq package-enable-at-startup nil) ; not activat installed packages
(setq package-archives
      '(("gnu" . "http://elpa.emacs-china.org/gnu/")
		("melpa" . "http://elpa.emacs-china.org/melpa/")
		("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")))
(package-initialize) ; activate installed packages
;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


;;;; ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching
(setq ido-auto-merge-delay-time 10000) ;; disable auto search file


;;;; misc
;; 防止退出时卡死在 Saving clipboard to X clipboard manager 状态
(setq x-select-enable-clipboard-manager nil)

(setq inhibit-startup-message t) ;; 禁用启动后显示的消息 buffer
(setq initial-scratch-message nil) ;; 禁止显示 *scratch* buffer 中默认出现的文本
(setq inhibit-compacting-font-caches t) ;; 防止卡顿，Don’t compact font caches during GC.
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)
(setq use-file-dialog nil)
(setq use-dialog-box nil)

(put 'suspend-frame 'disabled t) ;; 禁止 Ctrl+z 时挂起 emacs

;; 用 y/n 代替 yes/no 的回答
(defalias 'yes-or-no-p 'y-or-n-p) ;; (fset 'yes-or-no-p 'y-or-n-p) 相同的效果

(setq ring-bell-function 'ignore) ;; 禁止出现烦人的响铃

(setq truncate-lines t) ;; 不自动折行

;; Fix load slow, https://github.com/raxod502/radian/issues/180
(when tool-bar-mode
  (tool-bar-mode -1)) ;; 禁用工具栏
(when menu-bar-mode
  (menu-bar-mode -1)) ;; 禁用菜单栏
;; (when (and is-gui scroll-bar-mode)
  ;; (scroll-bar-mode -1)) ;; 禁用滚动条 emacs26 -nw will be error

(setq frame-title-format "Editor") ;; 自定义标题栏

;; 去掉窗口边缘和分割窗口时分割条的边缘
;; http://emacsredux.com/blog/2015/01/18/customizing-the-fringes/
;; linux maybe need gdb, and use mouse to set breakpoint on fringe, so only hide fringe on windows.
(when (and is-windows is-gui)
  (set-window-fringes nil 10 0) ;; border side
  (fringe-mode '(10 . 0)) ;; middle of split frame
  )

(when is-gui
  (blink-cursor-mode -1) ;; 取消光标闪烁
  (add-hook 'after-init-hook
	    (lambda ()
	      (set-cursor-color "#00A876"))))

(setq mouse-yank-at-point t) ;; 强制粘贴时粘贴到光标处

;; @see https://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
;; (setq split-width-threshold nil) ;;不允许自动左右分屏
(setq split-height-threshold nil) ;;不允许自动上下分屏

;; 不要自动分割窗口 @see https://github.com/ecxr/handmadehero/blob/master/misc/.emacs
;; (setq split-window-preferred-function nil)

(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; 鼠标滚轮滑动一次滚动多少行
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 5) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)


;; (setq electric-pair-pairs '((?\{ . ?\})
                            ;; (?\( . ?\))
                            ;; (?\[ . ?\])
                            ;; (?\" . ?\")))
(electric-pair-mode 0) ;;t自动输出成对括号

(show-paren-mode 1) ;;高亮匹配的括号



;; enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; save clipboard contents into kill-ring before replace theme
(setq save-interprogram-paste-before-kill t)

(delete-selection-mode 1)

;; Kill buffers without asking
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))


(setq ibuffer-expert t) ;;don't ask when delete

;; Don't open a file in a new frame
(when (boundp 'ns-pop-up-frames)
  (setq ns-pop-up-frames nil))

;;(setq track-eol t) ;; 保持光标上下移动时一直在行尾，需要设置line-move-visual为nil
;; (setq line-move-visual t)		;在长行中移动
(global-visual-line-mode 1)


;; display the real names on mode-line when visiting a symbolink
(setq find-file-visit-truename t)


;; 最大化
(defun maximize-frame ()
  "Maximizes the active frame in Windows"
  (interactive)
  (set-frame-parameter nil 'fullscreen 'maximize)
  ;; Send a `WM_SYSCOMMAND' message to the active frame with the
  ;; `SC_MAXIMIZE' parameter.
  (if is-windows
      (w32-send-sys-command 61488)))

(defun fullscreen-toggle ()
  "Toggle fullscreen/maximize status."
  (interactive)
  (if (equal 'fullboth (frame-parameter nil 'fullscreen))
      (maximize-frame)
    (set-frame-parameter nil 'fullscreen 'fullboth)))


(when is-gui
    (add-hook 'after-init-hook 'maximize-frame))


;;;; font
(when is-gui
  (when is-linux
    (setq en-font-name "Inconsolata")
    (setq cn-font-name "YaHei Consolas Hybrid")
    (setq en-font-size 16)
    (setq cn-font-size 16)
    )
  (when is-windows
    ;; Inconsolata
    ;; Fira Code
    ;; Droid Sans Mono Wide
    (setq en-font-name "Inconsolata")
    (setq cn-font-name "Microsoft YaHei")
    (setq en-font-size 16)
    (setq cn-font-size 14)
    )

  ;; 获取屏幕分辨率自动增大字体
  (when (and is-gui
	     (> (x-display-pixel-width) 1366)
	     (> (x-display-pixel-height) 768))
    (setq en-font-size (+ en-font-size 2))
    (setq cn-font-size (+ cn-font-size 2)))


  (defun eye-update-font-size ()
    ;; English font
    (set-face-attribute
     'default nil
     :font (font-spec :family en-font-name
		      :weight 'normal
		      :slant 'normal
		      :size en-font-size))
    ;; Chinese font
    (if (display-graphic-p)
	(dolist (charset '(kana han symbol cjk-misc bopomofo))
	  (set-fontset-font
	   (frame-parameter nil 'font)
	   charset
	   (font-spec :family cn-font-name
		      :weight 'normal
		      :slant 'normal
		      :size cn-font-size))))
    )

  (defun eye/increase-font-size ()
    "Increase font size of english and chinese."
    (interactive)
    (setq en-font-size (+ en-font-size 1))
    (setq cn-font-size (+ cn-font-size 1))
    (eye-update-font-size)
    )

  (defun eye/decrease-font-size ()
    "Decrease font size of english and chinese."
    (interactive)
    (setq en-font-size (- en-font-sizeeval 1))
    (setq cn-font-size (- cn-font-size 1))
    (eye-update-font-size)
    (if (equal (frame-parameter nil 'fullscreen) 'maximize)
	(maximize-frame))
    )

  (eye-update-font-size)
  )


;;;; History
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

(require 'saveplace)


(require 'recentf)
(setq recentf-max-saved-items 200)
;;(add-to-list 'recentf-exclude (expand-file-name package-user-dir))
(add-to-list 'recentf-exclude ".cache")
(add-to-list 'recentf-exclude ".cask")
(add-to-list 'recentf-exclude "bookmarks")
(add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")


;; save minibuffer history
(require 'savehist)
(setq enable-recursive-minibuffers t ; Allow commands in minibuffers
      history-length 100
      savehist-additional-variables '(mark-ring
				      global-mark-ring
				      search-ring
				      regexp-search-ring
				      extended-command-history)
      savehist-autosave-interval nil ;;不开启自动保存，否则会不断的分配内存
      )


;; for quick startup
(add-hook 'after-init-hook
	  (lambda ()
	    ;; (save-place-mode 1)
	    (recentf-mode 1)
	    (savehist-mode 1)))

;;;; Backup
(defvar user-cache-directory "~/tmp/emacs_cache")

(unless (file-directory-p "~/tmp")
  (make-directory "~/tmp"))
(unless (file-directory-p user-cache-directory)
  (make-directory user-cache-directory))
(unless (file-directory-p (concat user-cache-directory "/bak"))
  (make-directory (concat user-cache-directory "/bak")))
;; 备份文件 file~，指定备份目录后，文件名为 !drive_f!dirname!dirname!filename~
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)
(setq backup-directory-alist '(("." . "~/tmp/emacs_cache/bak")))
;; 临时文件 #file#
(setq auto-save-file-name-transforms '((".*" "~/tmp/emacs_cache/bak" t))) ;; 设置备份文件目录
;; (setq auto-save-default t) ;; 是否开启自动备份临时文件，auto-save.el 中会修改这个变量
;; (setq auto-save-timeout 5)

(global-auto-revert-mode 1)

(setq delete-by-moving-to-trash t)	;删除文件或目录时，移到回收站

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(require 'init-gbk)

;;;; modeline
;; Copy from https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-modeline.el
;; @see http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; But I need global-mode-string,
;; @see http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_360.html
;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
              (list
			   " "
			   '(:eval mode-line-front-space)
			   ;;'(:eval (if simple-fly-insert-state-p "-INS" "    "))

			   ;;'(:eval (format "-%s- " evil-state))



			   ;;"%e"
			   ;;mode-line-front-space
			   " "
               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize (if (buffer-modified-p)
                                       "%b *"
                                     "%b")
                                   'face nil
                                   'help-echo (buffer-file-name)))
			   " %I "

               '(:eval (format "%s" buffer-file-coding-system))

               " "

               ;; the current major mode for the buffer.
               ""

               '(:eval (propertize "%m" 'face nil
                                   'help-echo buffer-file-coding-system))
               " "


               ;; insert vs overwrite mode, input-method in a tooltip
               '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                                   'face nil
                                   'help-echo (concat "Buffer is in "
                                                      (if overwrite-mode "overwrite" "insert") " mode")))

               ;; is this buffer read-only?
               '(:eval (when buffer-read-only
                         (concat ","  (propertize "RO"
                                                  'face nil
                                                  'help-echo "Buffer is read-only"))))
               " "
			   "%n " ;; narrow state

			   ;;'(:eval (if (string-match-p "wubi" (format "%s" current-input-method))
				;;		   "WB"
				;;		 "EN"))

			   " "
			   ;;global-mode-string, org-timer-set-timer in org-mode need this
               (propertize "%M" 'face nil)

               ;; line and column
               "" ;; '%02' to set to 2 chars at least; prevents flickering
               "%02l"
               ;; (propertize "%02l" 'face 'font-lock-type-face) ","
               ;; (propertize "%02c" 'face 'font-lock-type-face)
               " "


			   ;; '(:eval (if (bound-and-true-p which-function-mode)
			   ;; which-func-format
			   ;; ""))

               " --"
               ;; i don't want to see minor-modes; but if you want, uncomment this:
               ;; minor-mode-alist  ;; list of minor modes
               "%-" ;; fill with '-'
               ))



;; Show modeline information on top header
;; (setq-default header-line-format mode-line-format) ; Copy mode-line
;; (setq-default mode-line-format nil) ; Remove mode-line
;;(set-face-attribute 'header-line nil :background "white" :foreground "black")

(which-function-mode)
(defun set-header-line ()
  (setq header-line-format
        '((which-function-mode ("" which-func-format " ")))))

;;;; bookmark
;; 自动保存书签
(add-hook 'kill-emacs-hook
          '(lambda ()
             (bookmark-save)))



;;;; imenu
(require 'imenu)
(setq imenu-auto-rescan t)
(setq imenu-auto-rescan-maxout 500000)


;;;; dired
(require 'dired)

(defun dired-dotfiles-toggle ()
  "Show/hide dot-files"
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
        (progn
          (set (make-local-variable 'dired-dotfiles-show-p) nil)
          (message "h")
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines))
      (progn (revert-buffer) ; otherwise just revert to re-show
             (set (make-local-variable 'dired-dotfiles-show-p) t)))))



(require 'wdired)
(require 'dired-x) ;; 支持 dired-jump 进入后自动定位到当前文件名位置

;; 打开 .dired 后缀文件时，自动进入 dired-virtual-mode 模式。
(setq auto-mode-alist (cons '("[^/]\\.dired$" . dired-virtual-mode)
			    auto-mode-alist))


(define-key dired-mode-map (kbd "<f12>s") 'dired-dotfiles-toggle)

;; 使用 windows 程序打开文件
(when is-windows
  (require 'w32-browser))


;;;; local
(setq user-full-name "owensys")
(setq user-mail-address "owensys@hotmail.com")



;;;; ibuffer
;; ibuffer
(setq ibuffer-saved-filter-groups
      '(("Default"
         ("Hidden(g则不显示此分组)"  (name . "^ "))
         ("Helm"  (or (name . "^\\*helm\\|^\\*ac-mode-")))
         ("Help"  (or (name . "^\\*help\\|^\\*ac-mode-")))
         ("Woman"  (name . "^\\*WoMan.*\\*$"))
         ("Compile"  (name . "^*.*compil[ea].*$"))
         ("Gtalk"  (or (name . "^\\*.*jabber") (name . "*fsm-debug*")))
         ("ERC"  (mode . erc-mode))
         ("Custom"  (mode . Custom-mode))
         ("Shell"  (mode . shell-mode))
         ("Mail" (or (mode . mew-summary-mode) (mode . mew-draft-mode)(mode . mew-message-mode)))
         ("VC"  (or (name . "*magit-") (name . "^\\*vc")(mode . diff-mode) (mode . vc-dir-mode)))
         ("Magit "  (name . "*magit:"))
         ("Emacs"  (name . "^\\*.*$"))
         ("Dired"  (mode . dired-mode))
         ("Go"  (mode . go-mode))
         ("Python"  (mode . python-mode))
         ("EL"  (or (mode . emacs-lisp-mode) (mode . lisp-interaction-mode)))
         ("C++" (mode . c++-mode))
         ("Text" (name . ".txt"))
         )))

(add-hook 'ibuffer-mode-hook
            '(lambda ()
                (ibuffer-auto-mode 1)
                (ibuffer-switch-to-saved-filter-groups "EL")))
(setq ibuffer-show-empty-filter-groups nil)


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-tramp)

;;;; cpp
(require 'cc-mode)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; outline fold
(require 'outline)
(defun cpp-outline-setup ()
  (interactive)
  (outline-minor-mode 1)
  (setq outline-regexp "^class\\|^struct\\|^enum\\|^[a-zA-Z][a-zA-Z0-9 _&\*]+::"))
;;(add-hook 'c++-mode-hook 'cpp-outline-setup)

(defun eye/find-header-or-source-file (&optional is-open-other-window)
  "Find the header or source file of this one."
  (interactive "P")
  (let ((full-base-name (file-name-sans-extension buffer-file-name)) ;;无后缀的文件路径
	(header-or-source-path nil))

    (cond ((string-match "\\.h" buffer-file-name)
	   (if (file-exists-p (concat full-base-name ".c"))
	       (setq header-or-source-path (concat full-base-name ".c"))
	     (setq header-or-source-path (concat full-base-name ".cpp"))))

	  ((string-match "\\.c" buffer-file-name)
	   (setq header-or-source-path (concat full-base-name ".h")))

	  ((string-match "\\.cpp" buffer-file-name)
	   (setq header-or-source-path (concat full-base-name ".h")))

	  (t (message "File name no suffix")))
    (if header-or-source-path
	(if is-open-other-window (find-file-other-window header-or-source-path)
	  (find-file header-or-source-path))
      (error "Unable to find a header or source file"))))

(require 'autoinsert)

(defun c++-mode-untabify ()
   (if (string= (substring mode-name 0 3) "C++")
       (save-excursion
         (delete-trailing-whitespace)
         (untabify (point-min) (point-max)))))

(defun set-tab-width-hook ()
  (setq-default c-default-style "linux"
        c-basic-offset 2
        default-tab-width 2
        tab-width 2
        indent-tabs-mode nil
        tab-stop-list nil
        )
  (setq c-default-style "linux" ;; 大括号缩进位置，https://en.wikipedia.org/wiki/Indentation_style
        c-basic-offset 2        ;; tab 缩进量
        default-tab-width 2
        tab-width 2
        indent-tabs-mode nil
        tab-stop-list nil
        )
  )

(defun cpp-setup-tab ()
  (interactive)
  (insert "  "))

(defun eye/save-buffer ()
"Replace tab to space before save-buffer."
  (interactive)
  (if (string= (substring mode-name 0 3) "C++")
      (untabify (point-min) (point-max)))
  (save-buffer))
      

(defun eye-setup-c++ ()
  (define-auto-insert '(c++-mode . "C++ skeleton")
    '(
      (upcase (concat "_"
                      (replace-regexp-in-string
                       "[^A-Za-z0-9]" "_"
                       (file-name-nondirectory buffer-file-name))))
      "/*******************************************************************************" \n
      "** Copyright: owen.tec" \n
      "** Author: owen" \n
      "** Date: 2019-09-10" \n
      "** Description: " \n
      "*******************************************************************************/" \n
      "#ifndef " str \n "#define " str "\n\n\n"
      "#endif"
      ))


  (define-key c++-mode-map (kbd "<M-up>") 'beginning-of-defun)
  (define-key c++-mode-map (kbd "<M-down>") 'end-of-defun)
  (define-key c++-mode-map (kbd "<f5>") 'make-without-asking)
  ;;(define-key c++-mode-map (kbd "TAB") 'cpp-setup-tab)

  (add-hook 'c-mode-hook 'set-tab-width-hook)
  (add-hook 'c++-mode-hook 'set-tab-width-hook)
  ;;(add-hook 'write-file-hooks 'cpp-mode-untabify)


  )

(eval-after-load 'cc-mode 
  (eye-setup-c++))

(global-font-lock-mode -1)

(require 'compile)

;;;; keys
(require 'base-toolkit)

(add-to-list 'load-path (expand-file-name "lisp/simple-fly-keys" user-emacs-directory))
(require 'simple-fly-keys)

(defun command-setup ()
  (setq mode-line-front-space "C")
  (simple-fly-define-keys simple-fly-\,-map
			  '(("b" . switch-to-buffer)
			    ("r" . replace-rectangle)
			    ("a" . mark-whole-buffer)
			    ("m" . imenu)
			    ))
  (define-key simple-fly-s-map (kbd "f") 'eye/save-buffer)
  )

(defun insert-setup ()
  (setq mode-line-front-space "I"))

(add-hook 'simple-fly-command-mode-activate-hook 'command-setup)
(add-hook 'simple-fly-insert-mode-activate-hook 'insert-setup)

(add-hook 'recentf-dialog-mode-hook 'simple-fly-insert-mode-activate)
(add-hook 'help-mode-hook 'simple-fly-insert-mode-activate)

(simple-fly-keys 1)

;; running on msys2, can't use C-c, it is <pause>
(when is-terminal
  ;;https://wiki.archlinux.org/index.php/emacs#Emacs-nox_output_gets_messy
  ;;https://emacs-china.org/t/emacs/6763/10
  (global-set-key (kbd "<f7>") 'redraw-display)
  (define-key global-map (kbd "C-x <pause>") 'kill-emacs))

;; guide-key show keybindings
;; not use which-key, it's require 24.4, centos7 default emacs-version is 24.3
(require-package 'guide-key)
(require 'guide-key)
;;(setq guide-key/guide-key-sequence '("," "e" "s"))
(setq guide-key/guide-key-sequence t) ;; any prefixes will pop up bindings.
(setq guide-key/idle-delay 0.5)
(setq guide-key/popup-window-position 'right) ;; `right', `bottom', `left' and `top'
(guide-key-mode 1)


(provide 'basic-config)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
