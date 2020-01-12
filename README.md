# sxiv.el
Launch sxiv (Simple X Image Viewer) from Emacs, with Dired integration.

## Installation
You can get `sxiv.el` from

`sxiv.el` requires [dash.el](https://github.com/magnars/dash.el)

## Usage
`M-x sxiv` - entry point. Run it in a Dired buffer containing images. Files marked in sxiv will be marked in Dired.

If the Dired buffer has marked files, open only those files.

With prefix argument, or when only provided directories, run recursively.

Run it from a text file containing one file name per line to open the listed files.

## TODO
* Create user-customizable variable to hold default arguments
* Mark files in subdirectories if run recursively (by inserting the subdirectory into the current buffer)
* Let user edit options (ideally with transient.el) when called with null argument/two prefix arguments
* When running with a lot of files, sxiv may take some time to start. Signal to the user that it is starting, and let them kill it if they want.
* What should be the behavior when we open Dired-marked files, then mark files within sxiv?

## Bugs
* `sxiv-dired-marked-files-p` doesn't work as intended with non '*' markers (e.g. C or D)
