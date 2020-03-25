;;; sxiv.el --- Run the sxiv image viewer -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Maintainer: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Keywords: multimedia
;; Homepage: https://gitlab.com/contrapunctus/sxiv.el
;; Package-Requires: ((dash "2.16.0") (emacs "25.1"))
;; Version: 0.3.1

;; This is free and unencumbered software released into the public domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; For more information, please refer to <https://unlicense.org>

;;; Commentary:
;; The sole command and primary entry point is `sxiv'.
;;
;; `sxiv-filter' is the process filter, to insert subdirectories (via
;; `sxiv-insert-subdirs') and mark files marked in sxiv (via
;; `sxiv-dired-mark-files').

(require 'dash)

;;; Code:

(defgroup sxiv nil
  "Run the Simple X Image Viewer."
  :group 'multimedia)

(defcustom sxiv-arguments '("-a" "-f" "-o")
  "Arguments to be passed to the sxiv process.
It must contain \"-o\" for marking in Dired buffers to function."
  :type '(repeat string))

(defcustom sxiv-exclude-strings '()
  "Exclude files whose paths match these strings."
  :type '(repeat string))

(defvar sxiv--directory nil
  "Directory `sxiv' was called from.
Used by `sxiv-filter' to know where to mark files.")

(defun sxiv-dired-marked-files-p ()
  "Return t if there are marked files in the current Dired buffer.
With no marked files, or if not in a Dired buffer, return nil."
  (if (equal major-mode 'dired-mode)
      (if (save-excursion
            (goto-char (point-min))
            (re-search-forward dired-re-mark nil t))
          t
        nil)
    nil))

(defun sxiv-insert-subdirs (paths)
  "Insert subdirectories from PATHS into the current Dired buffer.
Return PATHS unchanged."
  (mapc (lambda (path)
          ;; is the file a direct child? (i.e. exists in the current directory?)
          (unless (file-exists-p (file-name-nondirectory path))
            (dired-insert-subdir (file-name-directory path))))
        paths)
  paths)

(defun sxiv-dired-mark-files (files)
  "Mark FILES in the current (dired) buffer."
  (dired-mark-if
   (and (not (looking-at-p dired-re-dot))
        (not (eolp))
        (let ((fn (dired-get-filename t t)))
          (and fn (--find (equal fn it) files))))
   "file"))

(defun sxiv-filter (_process output)
  "Open a `dired' buffer and mark any files marked by the user in `sxiv'.
Used as process filter for `sxiv'.

OUTPUT is the output of the sxiv process as a string."
  (find-file sxiv--directory)
  (--> (split-string output "\n")
       (-drop-last 1 it)
       (sxiv-insert-subdirs it)
       (sxiv-dired-mark-files it)))

(defun sxiv (&optional prefix)
  "Run sxiv(1), the Simple X Image Viewer.
By default, when run in a Dired buffer, open all files in the
current directory. Files marked in sxiv will be marked in Dired.

If run from a Dired buffer with marked files, open only those
files.

With prefix argument PREFIX, or when only provided directories,
run recursively (-r).

If run from a text file containing one file name per line, open
the files listed."
  (interactive "P")
  (let* ((path-at-point (dired-file-name-at-point))
         (fn-at-point   (when (and path-at-point
                                   ;; REVIEW - also check if file is an image?
                                   (file-regular-p path-at-point))
                          (file-relative-name path-at-point)))
         (paths         (cond ((sxiv-dired-marked-files-p)
                               (dired-get-marked-files))
                              ((derived-mode-p 'text-mode)
                               (--> (buffer-substring-no-properties (point-min)
                                                                    (point-max))
                                    (split-string it "\n")))
                              (t (directory-files default-directory))))
         (paths         (--remove (or (equal it ".")
                                      (equal it "..")
                                      ;; Currently, this takes effect even
                                      ;; when running from a text
                                      ;; file...should that be the case?
                                      (-find (lambda (exclude)
                                               (string-match-p exclude it))
                                             sxiv-exclude-strings))
                                  paths))
         ;; recurse with prefix arg, or if every path is a directory
         (recurse       (or prefix
                            (-every? #'file-directory-p paths)))
         ;; remove directories if not running recursively
         (paths         (if recurse
                            paths
                          (seq-remove #'file-directory-p paths)))
         (fn-at-point-index (when fn-at-point
                              (--find-index (equal fn-at-point it)
                                            paths)))
         (fn-at-point-index (when fn-at-point-index
                              (-> (1+ fn-at-point-index)
                                  (number-to-string))))
         (recurse       (if recurse "-r" ""))
         (proc          (progn
                          (message "Running sxiv...")
                          (make-process :name "sxiv"
                                        :buffer "sxiv"
                                        :command
                                        (append '("sxiv")
                                                sxiv-arguments
                                                (when fn-at-point-index
                                                  (list "-n" fn-at-point-index))
                                                (list recurse "--")
                                                paths)
                                        :connection-type 'pipe
                                        :stderr "sxiv-errors"))))
    (setq sxiv--directory default-directory)
    (set-process-filter proc #'sxiv-filter)))

;; Local Variables:
;; nameless-current-name: "sxiv"
;; End:

(provide 'sxiv)

;;; sxiv.el ends here
