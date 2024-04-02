# CobolCraft

A Minecraft server written in COBOL. It supports Minecraft 1.20.4 (the latest version at time of writing).

## Progress/TODO

The following features are already working:

- [X] basic TCP server
- [X] respond to ping (i.e., show as online in the server list)
- [X] whitelist (single player, edit `server.cob` to enable/set username)
- [X] player login
- [ ] player movement
- [ ] sending chunk data to client
- [ ] block placement
- [ ] chat
- [ ] multiplayer
- [ ] persisting data across restarts

## How-to

CobolCraft was developed using GnuCOBOL and is meant to be run on Linux.
Support for other operating systems such as Windows has not been tested.
However, it is possible to use Docker for a platform-independent deployment.

To deploy on Linux, make sure all prerequisites are installed:

* `cobc` (e.g. from the `gnucobol` APT package on Debian)
* `make`
* `g++`

Then execute `make` to build, followed by `make run` to start a server on port 25565.

Or, using Docker:

```sh
docker build -t cobolcraft .
docker run --rm -p 25565:25565 cobolcraft
```

## Why?

Well, there are quite a lot of rumors and stigma surrounding COBOL.
This intrigued me to find out more about this language, which is best done with some sort of project, in my opinion.
You heard right - I had no prior COBOL experience going into this.

Writing a Minecraft server was perhaps not the best idea for a first COBOL project, since COBOL offers basically
no functionality regarding low-level data manipulation (bits and bytes) which the Minecraft protocol needs lots of.
But remember: I didn't know this starting out, and quitting before having a working prototype was not on the table!

If you too have never written COBOL before but are interested in CobolCraft, I recommend reading the GnuCOBOL
Programmer's Guide:
https://gnucobol.sourceforge.io/HTML/gnucobpg.html

## Legal Notices

This project (except 3rd-party contents as stated below) is licensed under the MIT License.
See LICENSE for further information.

This project includes the 3rd-party CBL_GC_SOCKET shared library, licensed under the LGPL v3.
See CBL_GC_SOCKET/COPYING.lesser for further information.

"Minecraft" is a trademark of Mojang Synergies AB.
