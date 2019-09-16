;;; 实用函数
;;(require 'xah-functions)
(defun eye/kill-inner-word ()
  "Kills the entire word your cursor is in. Equivalent to 'ciw' in vim."
  (interactive)
  ;;(forward-char 1) 
  (backward-word)
  (kill-word 1))


(defun eye/copy-whole-word ()
  (interactive)
  (save-excursion
    (forward-char 1)
    (backward-word)
    (kill-word 1)
    (yank)))

(defun eye/copy-whole-line ()
  "Copies a line without regard for cursor position."
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      (point-at-bol)
      (point-at-eol)))))

(defun eye/copy-paragraph ()
  "Copy paragraphes at point"
  (interactive)
  (let ((beg (progn (backward-paragraph 1) (point)))
        (end (progn (forward-paragraph 1) (point))))
    (copy-region-as-kill beg end)))

(defun eye/capitalize-word ()
  (interactive)
  (save-excursion
    (forward-char 1)
    (backward-word)
    (capitalize-word 1)))

(defun eye/upcase-word ()
  (interactive)
  (save-excursion
    (forward-char 1)
    (backward-word)
    (upcase-word 1)))

(defun eye/downcase-word ()
  (interactive)
  (save-excursion
    (forward-char 1)
    (backward-word)
    (downcase-word 1)))

(defun eye/current-word ()
  (interactive)
  (let (p1 p2 w)
    (save-excursion
      (skip-chars-backward "-_A-Za-z0-9")
      (setq p1 (point))
      (skip-chars-forward "-_A-Za-z0-9")
      (setq p2 (point)))
    (copy-region-as-kill p1 p2)
    (substring-no-properties (car kill-ring))))

(defun eye/goto-line ()
  "Auto enable and disable linum-mode."
  (interactive)
  (unless (bound-and-true-p linum-mode)
    (linum-mode))
  (let ((num (read-string "Goto line: ")))
    (goto-line (string-to-number num))
    (end-of-line))
  (linum-mode -1))


;;https://www.emacswiki.org/emacs/SwitchingBuffers
(defun eye/quick-switch-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; Following window splits
;; After split a window, let the focus in the new split window.
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))


(defun close-all-buffers ()
  "Kill all buffers without regard for their origin."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


(defun kill-unused-buffers ()
  (interactive)
  (ignore-errors
    (save-excursion
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (if (and (string-prefix-p "*" (buffer-name)) (string-suffix-p "*" (buffer-name)))
            (kill-buffer buf))
        ))))


(defun eye/create-scratch-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun eye/show-full-path ()
  "Show the file full path with current buffer."
  (interactive)
  (message (expand-file-name (buffer-file-name))))


;;; Quick insert new line
(defun eye/new-next-line ()
  "在当前行下方快速添加新的一行。"
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun eye/new-previous-line ()
  "在当前行上方快速添加新的一行。"
  (interactive)
  (beginning-of-line)
  (if (eq 1 (point))
      (progn
	(newline)
	(previous-line))
    (progn
      (previous-line)
      (move-end-of-line 1)
      (newline)
      (indent-for-tab-command))))


(defun eye/beginniing-of-line ()
  "移动到行首加强版，重复按 C-a，在忽略空白的行首和真实行首来回切换。"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

(defun eye/beginning-of-line-or-block ()
  "不在行首时，移到行首或缩进处，在行首时，移到上一段落
Move cursor to beginning of line or previous paragraph.

• When called first time, move cursor to beginning of char in current line. (if already, move to beginning of line.)
• When called again, move cursor backward by jumping over any sequence of whitespaces containing 2 blank lines.

URL `http://ergoemacs.org/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version 2018-06-04"
  (interactive)
  (let (($p (point)))
    (if (equal (point) (line-beginning-position))
        (if (re-search-backward "\n[\t\n ]*\n+" nil "move")
            (progn
	      (skip-chars-backward "\n\t ")
	      ;; (forward-char )
	      )
          (goto-char (point-min)))
      (progn
	(if (eq last-command this-command)
	    (beginning-of-line)
          (back-to-indentation))))))

(defun cycle-line-position ()
  "循环移动到行首或行尾或缩进处"
  (interactive)
  (cond
   ;; 在行尾时移到缩进处
   ((eq (point) (line-end-position))
    (back-to-indentation))
   ;; 在行首时移到行尾
   ((eq (point) (line-beginning-position))
    (end-of-line))
   ;; 默认移到行首
   (t (beginning-of-line))))

;; http://ergoemacs.org/emacs/emacs_kill-ring.html
(defun delete-forward-word-no-copy (arg)
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg) (point))))

(defun delete-inner-word-no-copy ()
  (interactive)
  (backward-word)
  (delete-forward-word-no-copy 1))

(defun delete-end-of-line-no-copy ()
  (interactive)
  (delete-region
   (point)
   (progn (end-of-line 1) (point))))

(defun delete-beginning-of-line-no-copy ()
  (interactive)
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)))

(defun delete-line-no-copy ()
  (interactive)
  (let (p1 p2)
    (beginning-of-line 1)
    (setq p1 (point))
    (end-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)
	(delete-forward-char 1)))

(defun yank-with-indent ()
  (interactive)
  (if (member major-mode
	      '(emacs-lisp-mode
		lisp-interaction-mode
		c++-mode
		org-mode
		python-mode
		c-mode
		php-mode))
      (progn
	(yank)
	(call-interactively 'indent-region))
    (yank)
    ))

(defun indent-or-expand ()
  "当光标前面是空白时,缩进,否则执行展开"
  (interactive)
  (if (or
       (eq (point) (line-beginning-position))
       (char-equal (char-before (point)) ?\s)
       (char-equal (char-before (point)) ?\t))
      (indent-for-tab-command)
    (hippie-expand)))

(defun eye/scroll-up ()
  (interactive)
  (previous-line 3))

(defun eye/scroll-down ()
  (interactive)
  (next-line 3))

(defun scroll-down-defun-or-lines ()
  "移动到函数后面或者下移几行（当移动行数小于5行时）"
  (interactive)
  (let ((line (line-number-at-pos (point)))
	(distance 0))
    (end-of-defun)
    (setq distance (- (line-number-at-pos (point)) line))
    (if (<= distance 5)
	(next-line 5)))
  )


(defun scroll-up-defun-or-lines ()
  "移动到函数前面或者上移几行（当移动行数小于5行时）"
  (interactive)
  (let ((line (line-number-at-pos (point)))
	(distance 0))
    (beginning-of-defun)
    (setq distance (- line (line-number-at-pos (point))))
    (if (<= distance 5)
	(previous-line 5)))
  )


(defun eye/indent-region-or-buffer ()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indent selected region."))
      (progn
        (indent-region (point-min) (point-max))
        (message "Indent buffer.")))
    )
  )


;; Quick ediff files from dired
;; Mark 2 files in dired, and press "e" into ediff. if only marked one file, then ask second file in prompt.
(defun ora-ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
		(wnd (current-window-configuration)))
	(if (<= (length files) 2)
		(let ((file1 (car files))
			  (file2 (if (cdr files)
						 (cadr files)
					   (read-file-name
						"file: "
						(dired-dwim-target-directory)))))
		  (if (file-newer-than-file-p file1 file2)
			  (ediff-files file2 file1)
			(ediff-files file1 file2))
		  (add-hook 'ediff-after-quit-hook-internal
					(lambda ()
					  (setq ediff-after-quit-hook-internal nil)
					  (set-window-configuration wnd))))
	  (error "no more than 2 files should be marked"))))


;; quick generate virtual.dired file and open it
(defun eye/virtual-dir ()
  "Create and open a virtual directory file.
use command: ls -lR > virtual.dired
"
  (interactive)
  ;; Check ls can use
  (unless (executable-find "ls")
    (error "Unkown command 'ls'"))
  (let (dir path cmd)
    ;; get directory path
    (setq dir (read-directory-name "Directory: "))
    (unless (equal "/" (s-right 1 dir)) ;; check last / charactor
      (setq dir (concat dir "/")))
    (setq path (concat dir "virtual.dired"))
    (setq cmd (concat "ls -lR " dir " > " path))
    (message cmd)
    (when (or (y-or-n-p "Create or update?") (not (file-exists-p path)))
      (setq cmd (read-string "Command:" cmd))
      (eshell-command cmd))
    (if (file-exists-p path)
        (find-file path)
      (message "Can not create virtual.dired file."))))

(defun eye/sudo-save ()
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))


(defun eye/delete-file-and-buffer ()
  "Kill the current buffer and delete the file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (delete-file filename)
      (message"Deleted file %s" filename)
      (kill-buffer))))


(defun eye/grep ()
  (interactive)
  (let* ((cur-word (thing-at-point 'word))
         (cmd (concat "grep --color -irHn " cur-word " " (buffer-file-name))))
    (setq cmd (read-from-minibuffer "command:" cmd))
    (grep-apply-setting 'grep-command cmd)
    (grep cmd)))


(defun eye/save-buffer ()
  "Save the buffer after untabifying it."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (untabify (point-min) (point-max))))
  (save-buffer))


(defun eye/remove-big-M (buffer)
  "Automate M-% C-q C-m RET RET"
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match ""))))


(defun eye/outline-hide-body ()
  "Hide outline body and move to beginning of buffer."
  (interactive)
  (outline-hide-body)
  (beginning-of-buffer))


(defun append-to-list (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR.

The return value is the new value of LIST-VAR."
  (unless (consp elements)
    (error "ELEMENTS must be a list"))
  (let ((list (symbol-value list-var)))
    (if list
        (setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))

(defun eye/replace-string-buffer ()
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(call-interactively 'replace-string)))


(defun eye/new-frame ()
  (interactive)
  (new-frame '((height . 50) (width . 100))))


(defun delete-window-or-frame (&optional window frame force)
  (interactive)
  (if (= 1 (length (window-list frame)))
      (delete-frame frame force)
    (delete-window window)))



(defun eye/open-thunar ()
  "Open thunar of current buffer directory."
  (when (and default-directory (executable-find "thunar"))
    (start-process "File manager" nil "thunar" default-directory)))

(defun eye/open-explorer ()
  "Open explorer of current buffer directory."
  (when (and default-directory (file-directory-p default-directory)
	     (eq system-type 'windows-nt))
    (let ((dir default-directory)
	  (explorer (replace-regexp-in-string "/" "\\\\" (executable-find "C:/Windows/SysWOW64/explorer")))
	  (command))
      (setq dir (encode-coding-string
		 (replace-regexp-in-string "/" "\\\\" dir) 'gbk-dos))
      (setq command (concat explorer " " dir))
      (shell-command command nil nil)
      (message command))
    ))

(defun eye/open-file-manager ()
  "Open external file manager."
  (interactive)
  (cond ((eq system-type 'windows-nt)
	 (eye/open-explorer))
	((eq system-type 'gnu/linux)
	 (eye/open-thunar))
	(t (message "Not support current system."))
	))


(provide 'base-toolkit)
