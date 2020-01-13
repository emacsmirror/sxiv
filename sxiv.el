;;; sxiv.el --- Run the sxiv image viewer -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Maintainer: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Package-Requires: (dash (emacs "25.1"))
;; Version: 0.1.0

;;; Commentary:
;;

(require 'dash)

;;; Code:

(defgroup sxiv nil
  "Run the Simple X Image Viewer."
  :group 'external)

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

(defun sxiv-filter (_process output)
  "Open a `dired' buffer and mark any files marked by the user in `sxiv'.
Used as process filter for `sxiv'.

OUTPUT is the output of the sxiv process as a string."
  (find-file sxiv--directory)
  (--> output
       (split-string it "\n")
       (-drop-last 1 it)
       (sxiv-dired-mark-files it)))

(defun sxiv-dired-mark-files (files)
  "Mark FILES in the current (dired) buffer."
  (dired-mark-if
   (and (not (looking-at-p dired-re-dot))
        (not (eolp))
        (let ((fn (dired-get-filename t t)))
          (and fn (--find (equal fn it) files))))
   "file"))

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
  (let* ((paths   (cond ((sxiv-dired-marked-files-p)
                         (dired-get-marked-files))
                        ((derived-mode-p 'text-mode)
                         (--> (buffer-substring-no-properties (point-min)
                                                              (point-max))
                              (split-string it "\n")))
                        (t (directory-files default-directory))))
         (paths   (--remove (or (equal it ".")
                                (equal it "..")
                                (-find (lambda (exclude)
                                         (string-match-p exclude it))
                                       sxiv-exclude-strings))
                            paths))
         ;; recurse with prefix arg, or if every path is a directory
         (recurse (or prefix
                      (-every? #'file-directory-p paths)))
         ;; remove directories if not running recursively
         (paths   (if recurse
                      paths
                    (seq-remove #'file-directory-p paths)))
         (recurse (if recurse "-r" ""))
         (proc    (make-process :name "sxiv"
                                :buffer "sxiv"
                                :command
                                (append '("sxiv")
                                        sxiv-arguments
                                        `(,recurse "--")
                                        paths)
                                :connection-type 'pipe
                                :stderr "sxiv-errors")))
    (setq sxiv--directory default-directory)
    (set-process-filter proc #'sxiv-filter)))

;; Local Variables:
;; nameless-current-name: "sxiv"
;; End:

(provide 'sxiv)

;;; sxiv.el ends here
