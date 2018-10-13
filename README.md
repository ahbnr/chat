# chat

Simple p2p cli chat application.

I wrote this application to be able to easily share links and files on the cli
with others on the network, but I wanted it to be more comfortable than netcat.

**Please do keep in mind**, that I wrote this application solely for personal use,
that means:

* I don't plan on adding any more documentation except the
  already extensive comments
* no tests (!)
* the application does clean up after unexpected exceptions
  (usually network issues), but it does not provide
  explanations to the user or try to recover

## Table of Contents

* [Features](#features)
* [What is still missing?](#what-is-still-missing)
* [How does it work?](#how-does-it-work)
* [Prerequisites](#prerequisites)
* [Building](#building)
* [Usage](#usage)
* [Installation](#installation)
* [TL;DR](#tldr)

## Features

* automatically connects to other peers
* redirects stdin to them
* redirects their messages to stdout

That's it.

## What is still missing?

* encryption
* ability to send binary files
  (sending text files does work already: `cat myFile | chat`)

## How does it work?

A short outline of what happens when starting the application:

1. A random tcp port is opened to listen for incoming connections
* for each new connection, stdin is duplicated and redirected to the connected peer
* all incoming messages are redirected to stdout
2. The application starts listening for UDP pings in a multicast group
* if a ping is received, it answers with an identification string and its tcp port
  via an UDP pong
3. A ping is sent to a multicast group, where other peers are listening
* other peers answer to the ping
* the application connects to every discovered peer in the same manner as in 1.

## Prerequisites

You'll need the Haskell development tool [stack](https://haskellstack.org).

## Building

```sh
stack build
```

## Usage

Just run it.

```sh
stack exec -- chat
```

* you can get additional debug information by using the `-d` flag

## Installation

For now, I only provide instructions for Arch Linux:

### Pre-Built

(old pre-release 1.0.1:)

```sh
curl -LO "https://github.com/ahbnr/chat/releases/download/v1.0.1-alpha/chat-r14.ab8b76d-1-x86_64.pkg.tar.xz"
sudo pacman -U "chat-r14.ab8b76d-1-x86_64.pkg.tar.xz"
```

### From source

```sh
curl -O "https://raw.githubusercontent.com/ahbnr/chat/master/archlinux/PKGBUILD"
makepkg -s PKGBUILD
sudo pacman -U chat*.pkg.tar.xz
```

## TL;DR

```sh
git clone https://github.com/ahbnr/chat.git
cd chat
stack build
stack exec chat
```

*Doesn't work?* You **need [stack](#prerequisites)**!
