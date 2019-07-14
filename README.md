# emacs.d

Cross-platform configuration files for GNU Emacs.

Requires the function `org-babel-load-file` to be present, otherwise
the literate config file won't be tangled and loaded.

Consists of two files:

 - `emacs-config.org`, a
   [literate](https://thewanderingcoder.com/2015/02/literate-emacs-configuration/)
   configuration file
 - `init.el`, which *only* tangles and loads `emacs-config.org`.

That's it!

## Installation

Make sure your version of Emacs is relatively recent (25+) and that
`org-mode` is present.

Clone this repository to your `$HOME` folder, or wherever `.emacs.d`
happens to live on your operating system (On Windows 10:
`C:\Users\user\AppData\Roaming\`).
