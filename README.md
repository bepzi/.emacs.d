## emacs.d

My configuration files for GNU Emacs, tested on Linux and Windows.

It requires the function `org-babel-load-file` to be present,
otherwise the literate config file won't be tangled and loaded. It
consists of two files:

 - `emacs-config.org`, the
   [literate](https://thewanderingcoder.com/2015/02/literate-emacs-configuration/)
   configuration file.
 - `init.el`, which tangles and loads `emacs-config.org`.

That's it!

### Installation

 - Make sure your version of Emacs is relatively recent (25+).

 - Clone this repository to your `$HOME` folder, or wherever
 `.emacs.d` should live on your operating system. On Windows 10,
 you'll want to put it in `C:\Users\<user>\AppData\Roaming\.emacs.d`.

 - Launch Emacs; it may take a minute or so for the packages to
 download and compile. After this is done, though, starting Emacs
 should be quite quick: under a couple of seconds at most.

#### Daemon mode with `emacsclient`

If you're on Linux with `systemd`, I also recommend installing a user
service that launches Emacs in daemon mode, then adding a `.desktop`
file that launches `emacsclient` instead of `emacs`.

 - Enable the `systemd` service. This comes bundled with Emacs 26+,
 but in case you don't already have it, just add this file as
 `~/.config/systemd/user/default.target.wants/emacs.service`:

```text
[Unit]
Description=Emacs text editor
Documentation=info:emacs man:emacs(1) https://gnu.org/software/emacs/

[Service]
Type=notify
ExecStart=/usr/bin/emacs --fg-daemon
ExecStop=/usr/bin/emacsclient --eval "(kill-emacs)"
# The location of the SSH auth socket varies by distribution, and some
# set it from PAM, so don't override by default.
# Environment=SSH_AUTH_SOCK=%t/keyring/ssh
Restart=on-failure

[Install]
WantedBy=default.target
```

 - Run `systemctl --user enable --now emacs.service`.

 - Copy `emacs.desktop`, which is in this repo, to
   `~/.local/share/applications/emacs.desktop`.

 - Launch Emacs through some sort of GUI launcher. `emacsclient`
   should open instead of `emacs`; you'll know if it worked if the
   window appears instantly. Note that invoking `emacs` in the
   terminal will still load a fresh instance of Emacs, the slow way.
