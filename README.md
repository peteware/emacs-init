# Emacs setup
My customizations for emacs are contained principally in the file
main-init.el but there are a number of additional customizations that
you can load directly (usually each file has documentation about
customizing it)

## Files

- emacs-init.el - My .emacs file
- main-init.el - My main customization file
- bb-style.el - Setup for C++/C identation
- comint-prefs.el - Setup for running shells, gdb, dbx
- csc-mode.el - Mode for editing comdb2 .csc files
- lrl-mode.el - Mode for editing comdb2 .lrl files
- org-prefs.el - Preferences for org-mode.  Includes remember templates, etc
- org-wiki.el - A partial attempt to export org files with wiki markup
- shell-switch.el - Binds ``C-c s'' to switch between shell-mode buffers.

## Setup

1. Choose where you want to have your emacs setup:

   ```sh
     $ mkdir ~/usr
     $ cd ~/usr
   ```

1. Clone this repository emacs config files::
 
  ```sh
     $ git clone git@bbgithub.dev.bloomberg.com:pware/emacs-init emacs
   ```

1. Choose which branch a release-M.N is most stable,
   "master" is pretty stable and "pete" is
   what I'm actually using:

   ```sh
     $ cd emacs
     $ git branch
       master
       pete
     * release-1.0
     $ git branch release-1.0
   ```

1. I setup my .emacs like this.  You can do the same except
   it has my customizations so it may not be great for you:

   ```sh
     $ ln -s ~/usr/emacs/emacs-init.el ~/.emacs
   ```

1. Alternatively setup your .emacs with::

   ```elisp
     (add-to-list 'load-path "~/usr/emacs")
     (require 'main-init)
   ```
