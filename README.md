# plist

## Plist Made Simplistic

> What is this tool all about?

This tool lets you manage plist files on macOS. It keeps an eye on changes in plist files and spits out `PlistBuddy` commands when you update preferences in macOS or apps. It's useful for turning those preference settings into a setup script, so you can have a reproducible environment.

[![](https://raw.githubusercontent.com/8ta4/plist-media/f271d49d4c8616afcc99e5c515ca192103001507/timeline.gif)](https://youtu.be/-gOFmlQP9IA)

> Why the love for `PlistBuddy` over `defaults`?

Unlike `defaults write`, `PlistBuddy` can handle nested preference values. But `PlistBuddy` is often ignored in favor of `defaults write`. That's why it's the "Pissed Buddy"!

## Installation

> How do I install the tool?

1. Make sure you're using a Mac with Apple silicon.

1. Install [Homebrew](https://brew.sh/#install).

1. Run this command in your terminal:

   ```sh
   brew install 8ta4/plist/plist
   ```

## Usage

> How do I use the tool?

1. Open your terminal.

1. Run this command:

   ```bash
   plist
   ```

   That'll start the monitoring process for plist files.

1. Change some preferences in macOS, like your Dock settings. `plist` will be watching for any plist file changes and will generate `PlistBuddy` commands. You'll see a command like this in the tool's output:

   ```bash
   /usr/libexec/PlistBuddy -c "Delete ':autohide'" -c "Add ':autohide' bool 'true'" "$HOME/Library/Preferences/com.apple.dock.plist"
   ```

1. Copy and paste the generated `PlistBuddy` command into your setup script.

Check out [this video](https://youtu.be/XvtGb3GxfWw?t=8272) where I used the plist tool to generate a command to configure the Dock.

> How can I monitor plist files that need root access?

To monitor plist files that need root permission (like those in `/var/root`), use `sudo`:

```bash
sudo plist
```

> Can this tool spot every preference change?

Nah, `plist` can't spot every preference change because some preferences are not stored in plist files. Some settings might be in databases or other formats that `plist` doesn't watch.
