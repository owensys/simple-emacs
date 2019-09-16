;;;; Encoding
(setq locale-coding-system 'gbk)     ;; 设置emacs 使用 gbk
(set-language-environment 'Chinese-GB) ;; 设置为中文简体语言环境
(set-keyboard-coding-system 'gbk)    ;; 设置键盘输入时的字符编码
;; 解决粘贴中文出现乱码的问题
(if (eq system-type 'windows-nt)
    (progn
      ;; (setq selection-coding-system 'utf-16le-dos) ;; 修复从网页剪切文本过来时显示 \nnn \nnn 的问题
      ;; (set-default selection-coding-system 'utf-16le-dos)
      (set-selection-coding-system 'utf-16le-dos) ;; 别名set-clipboard-coding-system
      )
  (set-selection-coding-system 'gbk))
(prefer-coding-system 'gbk)
;; 文件默认保存为 gbk
(set-buffer-file-coding-system 'gbk)
(set-default buffer-file-coding-system 'utf8)
(set-default-coding-systems 'gbk)
;; 防止终端中文乱码
(set-terminal-coding-system 'gbk)
(modify-coding-system-alist 'process "*" 'gbk)
(setq default-process-coding-system '(gbk . gbk))
;; 解决文件目录的中文名乱码
(setq-default pathname-coding-system 'gbk)
(set-file-name-coding-system 'gbk)


(provide 'init-gbk)
