# plist

## Introduction

> What is this tool all about?

Oh, it's a handy little tool to help with managing macOS preference files, or plist files, as they're called. Basically, it keeps an eye on changes in plist files and spits out `PlistBuddy` commands when you update preferences in macOS or apps. It's super useful for turning those preference settings into a setup script, so you can have a reproducible environment.

> I've heard of `defaults write` for managing plist files. Why use `PlistBuddy` instead?

`PlistBuddy` is way more powerful and flexible when it comes to working with plist files. Unlike `defaults write`, `PlistBuddy` can handle nested preference values, so it's easier to manage complex plist structures. Plus, you know how `PlistBuddy` is often overlooked in favor of `defaults write`? That's why it's the "Pissed Buddy"!

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

1. First, open your terminal and make sure you're in the project directory where plist is installed. If you're not sure, use the cd command to navigate there.

2. Run this command:

   ```bash
   plist
   ```

   That'll start the monitoring process for plist files.

3. Now, go ahead and change some preferences in macOS, like your Dock settings. The tool will be watching for any plist file changes and will generate `PlistBuddy` commands for you.

4. After you've made some changes, you'll see a command like this in the tool's output:

   ```bash
   /usr/libexec/PlistBuddy -c "Delete ':autohide'" -c "Add ':autohide' bool 'true'" "$HOME/Library/Preferences/com.apple.dock.plist"
   ```

5. If you want to use this command in a setup script, just copy and paste the generated `PlistBuddy` command into your script.

Check out [this video](https://youtu.be/CPLy3ImVZk8?t=190) where I used the plist tool to generate a command to configure the Dock.
