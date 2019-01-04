# desi-emacs-init

This is my personal Emacs init setup. It has these files:

## `init.el`

- Auto-installs packages from MELPA Stable. If my packages mess up or if I want to wipe them, I can just delete `.emacs.d/elpa/` and restart Emacs.
- Loads the theme and font face. 
- Maximises the window
- Loads all of the other init files in the order listed below.

## `init/emacs-env.el`

Handles settings that apply to Emacs itself as a text editor. Frame settings (toolbars, line numbers); file behaviours (backups, autosaves), text editing behaviours (matching pairs, indenting, filling).

## `init/my-keys-mode.el`

This file defines a minor mode that makes it possible to overwrite major mode bindings. The actual rebinds are done in `init/keybinds.el` and `init/enable-packages.el`.

## `init/custom-functions.el`

The definitions of custom functions.

## `init/org-settings.el`

Settings for `org-mode` and `org-journal`. Directory locations, tags and keywords, capture templates, overall appearance.

## `init/keybinds.el`

Rebind some core Emacs and built-in package commands, but try to preserve as many as possible.

## `init/enable-packages.el`

Enable third-party packages, initialise their settings, and set their keybinds.

## `init/write-env.el`

Sets up my prose writing environment. Markdown, CriticMarkup, word count goals, linter, visual improvements like a screen-centered text column, and others.

## `init/snippets.el`

Auto-replacing snippets for common bits of text. Of particular note is the 3,663 snippets that auto-correct words that are commonly misspelled, or correctly output foreign loan words that have accented letters. This comes from a large list that was first put together by Jim Biancolo in Autohotkey <http://www.biancolo.com/blog/autocorrect/>.

## `init/unicode-insertion.el`

A key translation map for inserting special characters since alt codes are a pain in Emacs. Special characters are inserted by pressing `<f8>` and then a mnemonic key. For example, `<f8> *` inserts a degree mark °, or `<f8> m` inserts an em dash —, or `<f8> u` inserts a lowercase mu μ, or `<f8> SPC` inserts a non-breaking space  .
