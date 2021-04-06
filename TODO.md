# TODO
1. [x] Create user-customizable variable to hold default arguments
2. [x] Start sxiv on the file at point (using `-n ...`)
3. [x] Let user specify paths to be excluded.
4. [x] Mark files in subdirectories if run recursively (by inserting the subdirectory into the current buffer)
5. [ ] Let user edit options (ideally with transient.el) when called with null argument/two prefix arguments.
    * When files are marked in Dired - only display marked files, or ignore marks and run as usual
    * When files are marked in Dired and we mark files in sxiv - replace the selection, or unmark the files marked this time
    * Other options like running recursively, modifying arguments, etc
6. [ ] When running with a lot of files, sxiv may take some time to start. Signal to the user that it is starting, and let them kill it if they want.
7. [ ] `sxiv-exclude-strings` does not work recursively, because only the directories are passed to the process. Adding all files to the path might cause it to fail (bash length limit), or take a long time.
       * Use find(1) to pass the files?
8. [ ] Make it work in find-dired buffers too
9. [ ] Bug - sometimes, if a lot of files (usually over 50) are marked in sxiv, the input received by `sxiv-insert-subdirs` is incomplete - the first file name is a trailing segment of an actual existing filename, e.g. if a file is called `"foo/bar_baz.jpg"`, I might get something like `"r_baz.jpg"` as the first element.
    * I have no idea what's causing this or how to fix it. Help needed! :(
10. [ ] Optimize startup speed, especially with a large number of files.
11. [ ] When using a text file, mark files marked in sxiv.
    * Maybe using multiple-cursors or iedit?
12. [x] When launching from a text file, open sxiv with the image at point (using `-n`).
