# CobolCraft

[![Build](https://github.com/meyfa/CobolCraft/actions/workflows/build.yml/badge.svg)](https://github.com/meyfa/CobolCraft/actions/workflows/build.yml)
[![Test](https://github.com/meyfa/CobolCraft/actions/workflows/test.yml/badge.svg)](https://github.com/meyfa/CobolCraft/actions/workflows/test.yml)

A Minecraft server written in COBOL. It supports Minecraft 1.21.4 (the latest version at time of writing).

## Features

The following features are already working:

- [X] infinite terrain generation and dynamic chunk loading
- [X] persisting world and player data to disk
- [X] support for Minecraft's file formats (import existing worlds)
- [X] multiplayer (configurable number of concurrent players)
- [X] ping/server status (i.e., show as online in the server list)
- [X] breaking and placing blocks
- [X] block interaction (right-clicking, e.g., to open doors)
- [X] player inventory
- [X] crafting, both 2x2 and 3x3
- [X] item entities and item pickup
- [X] chat
- [X] commands (in-game and via an interactive console)
- [X] configuration via server.properties
- [X] whitelist (persistent; stored in whitelist.json)
- [X] extremely basic block/player collision and entity physics
- [X] fall damage, death, and respawning

Note that blocks with multiple states, orientations, or interactive blocks require large amounts of specialized code
to make them behave properly, which is way beyond the scope of this project.
Some are supported, however:

- torches (all variants)
- slabs (all variants)
- stairs (non-connecting)
- rotated pillars, such as logs or basalt
- buttons (non-interactive)
- doors (including interaction)
- trapdoors (including interaction)
- beds

## How-to

CobolCraft was developed using GnuCOBOL and is meant to be run on Linux.
Support for other operating systems such as Windows has not been tested.
However, it is possible to use Docker for a platform-independent deployment.

To deploy on Linux, make sure all prerequisites are installed:

* GnuCOBOL 3.1.2 or later (e.g., from the `gnucobol` package on Debian/Ubuntu)
    - 3.2 or later is highly recommended for performance reasons - check `cobc -version`
* `make`
* `gcc`, `g++`
* `zlib` (e.g. `zlib1g-dev` on Debian/Ubuntu)
* `curl` (needed to download the official server .jar)
* Java 21 or later (needed to extract data from the server .jar)

Run the following commands to build and run CobolCraft:

```sh
make --jobs=j$(nproc)
make run
```

Or, run CobolCraft using Docker:

```sh
# pull the image from Docker Hub
docker pull meyfa/cobolcraft:latest

# or build it yourself
git clone https://github.com/meyfa/CobolCraft.git cobolcraft && cd cobolcraft
docker build --tag meyfa/cobolcraft .

docker run --rm --interactive --tty \
     --publish 25565:25565 \
     --volume "$(pwd)/server.properties:/app/server.properties" \
     --volume "$(pwd)/whitelist.json:/app/whitelist.json" \
     --volume "$(pwd)/world:/app/world" \
    meyfa/cobolcraft
```

To configure the server, edit the `server.properties` file.
This file is generated automatically on first run with default values for all supported options:

* `server-port` (default: 25565)
* `level-name` (default: "world")
* `white-list` (default: false)
* `motd` (default: "CobolCraft")
* `max-players` (default: 10; maximum: 100)

Note: By default, the server is only accessible via localhost (i.e., only on your own system via `localhost:25565`).
To make it accessible from the outside (your local network, via VPN, port forwarding, on a rented server, ...), you
can start the Docker container like this:

```sh
docker run --rm -it -p 0.0.0.0:25565:25565 meyfa/cobolcraft
```

## Why?

While I had zero prior COBOL experience, I had heard a lot of rumors and noticed a stigma surrounding COBOL.
This intrigued me to find out more about this language - and the best way to learn a language is to write something
with it.

In retrospect, due to the sheer complexity and scale of Minecraft's code, choosing to write a Minecraft server
in COBOL was both the best and worst idea I could have had.
For one, it is necessary to invent a lot of things from scratch that are quite easy in other languages.
This includes parsing and encoding JSON and all kinds of binary data, implementing real-time multiplayer networking,
and translating large amounts of an inherently object-oriented system (Minecraft) to a procedural language.
However, adapting to such a steep learning curve forces me to research and understand the language and its concepts
in-depth, which is very rewarding.

If you too have never written COBOL before but are interested in CobolCraft, I recommend reading the GnuCOBOL
Programmer's Guide:
https://gnucobol.sourceforge.io/HTML/gnucobpg.html

To learn more about the Minecraft protocol, you can refer to the wiki.vg documentation:
https://minecraft.wiki/w/Minecraft_Wiki:Projects/wiki.vg_merge/Protocol

In some cases, it may also be helpful to look at real server traffic (e.g., using Wireguard) to better understand the
flow of information.

## Program Overview

This section provides a high-level overview of CobolCraft from a software design viewpoint.

### Source Components

The program entrypoint is `main.cob`, with further sources located in the `src/` directory, notably `src/server.cob`.

In true COBOL fashion, there is also a code generator (itself written in COBOL) that generates additional source code
from JSON data like Minecraft's default datapack.
This can be found in the `codegen/` directory.

The `cpp/` directory contains C++ sources that are used to interface with the operating system in ways that are not
feasible in COBOL, such as low-level TCP socket management, precise timing, or process signal handling.

All sources (COBOL and C++) are compiled into a single `cobolcraft` binary.

### Data Extraction

The official Minecraft (Java Edition) server and client applications contain large amounts of data such as:

* block and item types
* entity types
* biomes
* protocol IDs for packets
* tags (e.g., which blocks are mineable with a pickaxe)

The CobolCraft `Makefile` has a target to download the .jar and extract this data as JSON, which is used in two ways:

* at compile-time to automatically generate COBOL code, such as for loot tables of all the block types
* at runtime to load the data into memory.

A custom-built generic JSON parser (written in COBOL and fully unit-tested) is used for both tasks.

### Tests

Unit tests are available in the `tests/` directory.
The main goal here should be to test encoding and decoding of JSON and binary data and other things that are hard to
debug, while testing the game logic itself is not so critical.

Also see Updating.md for a guide regarding updating the server to a new Minecraft version and the steps to test it.

## Legal Notices

This project is licensed under the MIT License; see LICENSE for further information.

"Minecraft" is a trademark of Mojang Synergies AB.
CobolCraft is neither affiliated with nor endorsed by Mojang.
