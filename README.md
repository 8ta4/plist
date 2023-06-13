# plist

## Introduction

> What is this tool all about?

Oh, it's a handy little tool to help with managing macOS preference files, or plist files, as they're called. Basically, it keeps an eye on changes in plist files and spits out `PlistBuddy` commands when you update preferences in macOS or apps. It's super useful for turning those preference settings into a setup script, so you can have a reproducible environment.

> I've heard of `defaults write` for managing plist files. Why use `PlistBuddy` instead?

`PlistBuddy` is way more powerful and flexible when it comes to working with plist files. Unlike `defaults write`, `PlistBuddy` can handle nested preference values, so it's easier to manage complex plist structures. Plus, you know how `PlistBuddy` is often overlooked in favor of `defaults write`? That's why it's the "Pissed Buddy"!
