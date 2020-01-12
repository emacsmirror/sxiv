;;; sxiv.el --- Run the sxiv image viewer from Emacs -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Maintainer: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Package-Requires: (dash)
;; Version: 0.1.0

;;; Commentary:
;;

(require 'dash)

;;; Code:

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

(defun sxiv-filter (_process _output)
  "Open a `dired' buffer and mark any files marked by the user in `sxiv'.
Used as process filter for `sxiv'."
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
                              (split-string it "\n")
                              (-drop-last 1 it))) ;; why?
                        (t (directory-files default-directory))))
         (paths   (--remove (or (equal it ".")
                                (equal it ".."))
                            paths))
         (recurse (or prefix
                      (-every? #'file-directory-p paths)))
         ;; remove directories if not running recursively
         (paths   (->> (if recurse
                           paths
                         (seq-remove #'file-directory-p paths))
                       (mapcar #'shell-quote-argument)
                       (--reduce (concat acc " " it))))
         (recurse (if recurse "-r" ""))
         (proc    (make-process :name "sxiv"
                                :buffer "sxiv"
                                :command
                                (list shell-file-name
                                      shell-command-switch
                                      (concat "sxiv -afo "
                                              recurse
                                              " -- "
                                              paths))
                                :connection-type 'pipe
                                :stderr "sxiv-errors")))
    (setq sxiv--directory default-directory)
    (set-process-filter proc #'sxiv-filter)))

;; Local Variables:
;; nameless-current-name: "sxiv"
;; End:

(provide 'sxiv)

;;; sxiv.el ends here
