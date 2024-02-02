# plist

## Plist Made Simplistic

> What is this tool all about?

This tool lets you manage plist files on macOS. It keeps an eye on changes in plist files and spits out `PlistBuddy` commands when you update preferences in macOS or apps. It's useful for turning those preference settings into a setup script, so you can have a reproducible environment.

> Why the love for `PlistBuddy` over `defaults`?

Unlike `defaults write`, `PlistBuddy` can handle nested preference values. But `PlistBuddy` is often ignored in favor of `defaults write`. That's why it's the "Pissed Buddy"!

## Installation

> How do I install the tool?

You need to install devenv and direnv. Check out the [devenv getting started page](https://devenv.sh/getting-started/#installation) and the [direnv installation guide](https://devenv.sh/automatic-shell-activation/#installing-direnv) to get them set up on your system.

Once you've got devenv and direnv installed, just run these commands:

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/8ta4/plist/main/install.sh)"
cd plist
```

## Usage

> How do I use the tool?

1. Open your terminal and make sure you're in the project directory where plist is installed.

1. `cd` into the project directory where `plist` is installed.

1. Run this command:

   ```bash
   plist
   ```

   That'll start the monitoring process for plist files.

1. Now, go ahead and change some preferences in macOS, like your Dock settings. The tool will be watching for any plist file changes and will generate `PlistBuddy` commands for you.

1. After you've made some changes, you'll see a command like this in the tool's output:

   ```bash
   /usr/libexec/PlistBuddy -c "Delete ':autohide'" -c "Add ':autohide' bool 'true'" "$HOME/Library/Preferences/com.apple.dock.plist"
   ```

1. If you want to use this command in a setup script, just copy and paste the generated `PlistBuddy` command into your script.

Check out [this video](https://youtu.be/XvtGb3GxfWw?t=8272) where I used the plist tool to generate a command to configure the Dock.

> How can I monitor plist files that need root access?

To monitor plist files that need root permission (like those in `var/root`), use `sudo`:

```bash
sudo plist
```
