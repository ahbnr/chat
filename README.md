# chat

[![Build Status](https://ahbnr.de/jenkins/buildStatus/icon?job=chat)](https://ahbnr.de/jenkins/job/chat/)

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
* redirects stdin (or file contents) to them
* redirects their messages to stdout

That's it.

## What is still missing?

* encryption
* authentication
* Oftentimes a bug causes input supplied via pipes or the `-f` flag to be lost

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

For chatting, just run it.

```sh
stack exec -- chat
```

You can use pipes to send the output of other programs to chat peers:
(Currently a bug causes input supplied via pipes or the `--file` flag to be lost)

```sh
cat someFile | stack exec -- chat
```

You can directly read from files instead of stdin:

```sh
stack exec -- chat --file myFile
```

* you can get additional debug information by using the `-d` flag

## Installation

The latest Linux binary can be obtained from [here](https://ahbnr.de/jenkins/job/chat/lastSuccessfulBuild/artifact/.stack-work/install/x86_64-linux/lts-12.8/8.4.3/bin/chat).
Packages for Arch Linux are also available:

### Pre-Built

```sh
curl -O "https://raw.githubusercontent.com/ahbnr/chat/master/archlinux/bin/PKGBUILD"
makepkg -s PKGBUILD
sudo pacman -U chat*.pkg.tar.xz
```

### From source

```sh
curl -O "https://raw.githubusercontent.com/ahbnr/chat/master/archlinux/git/PKGBUILD"
makepkg -s PKGBUILD
sudo pacman -U chat*.pkg.tar.xz
```

## TL;DR

```sh
# Download source
git clone --depth=1 https://github.com/ahbnr/chat.git
cd chat

# Download stack build tool
curl -sSLO https://get.haskellstack.org/stable/linux-x86_64.tar.gz
tar -xf linux-x86_64.tar.gz
cp stack-*/stack .
rm -rf stack-*

# Building
./stack build
./stack exec chat
```
