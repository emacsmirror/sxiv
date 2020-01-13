# sxiv.el
Launch [sxiv](https://github.com/muennich/sxiv) (Simple X Image Viewer) from Emacs, with Dired integration.

## Installation
You can get `sxiv.el` from

`sxiv.el` requires [dash.el](https://github.com/magnars/dash.el) and an installed [sxiv](https://github.com/muennich/sxiv) in your $PATH

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

## Limitations
* Currently falls flat if run in directories with a huge number of images.
* `sxiv-dired-marked-files-p` doesn't work as intended with non '*' markers (e.g. C or D)

## Comparison with [picpocket](https://github.com/johanclaesson/picpocket)
At a cursory glance, picpocket (v20180914)
* ✔️ does not depend on an external program
* ✔️ has better key customizability (to change the keys for sxiv itself, you need to modify its C source)
* ✔️ supports tagging images
* ✖️ has no way to mark files for batch operations
* ✖️ does not seem to center images in fullscreen mode

## Contributions and contact
Feedback and MRs very welcome. 🙂

Contact the creator and other Emacs users in the Emacs room on the Jabber network - [xmpp:emacs@salas.suchat.org?join](xmpp:emacs@salas.suchat.org?join) ([web chat](https://inverse.chat/#converse/room?jid=emacs@salas.suchat.org))

(For help in getting started with Jabber, [click here](https://xmpp.org/getting-started/))

## License
sxiv.el is released under your choice of [Unlicense](https://unlicense.org/) and the [WTFPL](http://www.wtfpl.net/).

(See files [LICENSE](LICENSE) and [LICENSE.1](LICENSE.1)).

## Thanks
wasamasa, bpalmer and #emacs for all their help and support
