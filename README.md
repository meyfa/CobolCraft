# CobolCraft

A Minecraft server written in COBOL. It supports Minecraft 1.20.4 (the latest version at time of writing).

## Progress/TODO

The following features are already working:

- [X] respond to ping (i.e., show as online in the server list)
- [X] whitelist (single player, edit `main.cob` to enable/set username)
- [X] player login
- [X] sending chunk data to client
- [X] player movement
- [X] player inventory (limited to creative mode)
- [X] breaking and placing blocks
- [X] multiplayer
- [ ] chat
- [ ] persisting data across restarts

## How-to

CobolCraft was developed using GnuCOBOL and is meant to be run on Linux.
Support for other operating systems such as Windows has not been tested.
However, it is possible to use Docker for a platform-independent deployment.

To deploy on Linux, make sure all prerequisites are installed:

* `cobc` (e.g. from the `gnucobol` APT package on Debian)
* `make`
* `g++`
* `curl` (needed to download the official server .jar)
* a recent version of Java (needed to extract data from the server .jar)

Then execute `make` to build, followed by `make run` to start a server on port 25565.

Or, using Docker:

```sh
docker build -t cobolcraft .
docker run --rm -p 25565:25565 cobolcraft
```

To configure the server, edit the variables in `main.cob` (limited options available).

Note: By default, the server is only accessible via localhost (i.e., only on your own system via `localhost:25565`).
To make it accessible from the outside (your local network, via VPN, port forwarding, on a rented server, ...), you
can start the Docker container like this:

```sh
docker run --rm -p 0.0.0.0:25565:25565 cobolcraft
```

## Why?

Well, there are quite a lot of rumors and stigma surrounding COBOL.
This intrigued me to find out more about this language, which is best done with some sort of project, in my opinion.
You heard right - I had no prior COBOL experience going into this.

Writing a Minecraft server was perhaps not the best idea for a first COBOL project, since COBOL offers basically
no functionality regarding low-level data manipulation (bits and bytes) which the Minecraft protocol needs lots of.
But remember: I didn't know this starting out, and quitting before having a working prototype was not on the table!
A lot of this functionality had to be implemented completely from scratch.
For large, complex data, I opted for recording packets from a "real" server via Wireshark (see `blobs/` directory) and
playing them back to clients.

If you too have never written COBOL before but are interested in CobolCraft, I recommend reading the GnuCOBOL
Programmer's Guide:
https://gnucobol.sourceforge.io/HTML/gnucobpg.html

To learn more about the Minecraft protocol, you can refer to https://wiki.vg/Protocol.
In some cases, it may be helpful to look at real server traffic to better understand the flow of information.

## Program Overview

This section provides a high-level overview of CobolCraft from a software design viewpoint.

### Source Components

The program entrypoint is `main.cob`.
The remaining COBOL sources are located in the `src/` directory, including `src/server.cob`, which contains the bulk
of CobolCraft.

Some functions had to be implemented in C++, such as process signal handling, retrieving accurate timestamps, or
conversion of IEEE-754 floating-point numbers.
These sources are located in the `cpp/` directory and get compiled into a shared library (`.so` on Linux).

TCP sockets are managed by the CBL_GC_SOCKET socket library located in the `CBL_GC_SOCKET/` directory.

### Packet Blobs

CobolCraft makes use of network data captured from an instance of the official server application.
This data is located in the `blobs/` directory and is decoded at run-time.

### Data Extraction

The official Minecraft (Java Edition) server and client applications contain large amounts of data such as:

* block and item types
* entity types
* biomes

Fortunately, the freely available server .jar offers a command-line interface for extracting this data as JSON.
The CobolCraft `Makefile` has a target that downloads the .jar and extracts the JSON data from it.
The JSON files are evaluated at runtime using a custom-built generic JSON parser, such that CobolCraft can
inter-operate successfully with the Minecraft client without distributing potentially copyrighted material.

## Legal Notices

This project (except 3rd-party contents as stated below) is licensed under the MIT License.
See LICENSE for further information.

This project includes the 3rd-party CBL_GC_SOCKET shared library, licensed under the LGPL v3.
See CBL_GC_SOCKET/COPYING.lesser for further information.
Note that line 939 of `CBL_GC_SOCKET/cob_socket.cpp` has been modified (compared to the original distribution of that
library) to fix improper `snprintf` usage.

"Minecraft" is a trademark of Mojang Synergies AB.
CobolCraft is neither affiliated with nor endorsed by Mojang.
