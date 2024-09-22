# LuaNES
NES emulator in lua+love2d based/inspired on [optcarrot](https://github.com/mame/optcarrot). Note that the emulator is written in pure lua(JIT) and it should be possible to write a "front end" for it in something other than love2d (Like games that embed lua and let you play audio/video from memory).

Here's what it looks like: https://streamable.com/x7f5u

Here's Castlevania 3 (A game which uses the [MMC5](https://www.nesdev.org/wiki/MMC5) mapper): https://streamable.com/2ucq3l

LuaNES running inside Garry's Mod: https://share.epic-domain.com/2019-06-29_18-23-19975b9f0bca56cbe9c.webm (Courtesy of [ZehMatt](https://github.com/nico-abram/LuaNES/pull/1#issuecomment-541408792))

# Running

Get https://love2d.org/ and run `love . path/to/rom.nes` in the repo folder.

# Overview

The NES basically has 3 "main" or big components: the [PPU](https://wiki.nesdev.com/w/index.php/PPU) (Picture Processing Unit), [CPU](https://wiki.nesdev.com/w/index.php/CPU) and [APU](https://wiki.nesdev.com/w/index.php/APU) (Audio Processing Unit).
ROMs "map" their memory to CPU addreses with different [mappers](https://wiki.nesdev.com/w/index.php/Mapper) (Often mirroring memory), depending on a code in the rom [header](https://wiki.nesdev.com/w/index.php/INES).
main.lua is the love2d entrance point (And the only file with love2d specific code). nes.lua ties everything together. The cpu is fairly straightforward (You can find the instruction set [here](http://obelisk.me.uk/6502/reference.html)). The APU is very much not finished (Currently, it outputs something that sounds more-or-less correct, but it has a lot of issues). The PPU generates the expected video output (There's a test case that it doesnt "pass" for a very specific thing). The PPU, APU and pads are mapped to memory (Status bytes, read/write addresses, etc. Check #Reference more details and the nesdev for details).

The pulse_test.asm file was assembled using [n65](https://github.com/safiire/n65).

# Reference

optcarrot docs:

https://www.slideshare.net/mametter/optcarrot-a-pureruby-nes-emulator
https://github.com/mame/optcarrot/blob/master/doc/internal.md 

cpu instructions:

http://obelisk.me.uk/6502/reference.html
http://www.oxyron.de/html/opcodes02.html

Other:

http://www.dustmop.io/blog/2015/04/28/nes-graphics-part-1/ This has a really nice graphical representation of how the PPU works

https://wiki.nesdev.com/w/index.php contains almost everything you could wish for

http://www.fceux.com/web/help/fceux.html?Introduction.html fceux docs

https://github.com/TASVideos/fceux is an emulator in c++, and as far as i could find it's supposed to be the most "correct" emulator out there

http://www.qmtpro.com/~nes/misc/nestest.log This can be used to compare the output of printing state for every instruction for running tests/nestest.rom . Note that in order to have it run all the tests "automatically" it should start reading from address C000. Last i checked the output was pretty much correct.

Wikipedia pages might also be helpful.

# Progress

It should be fairly functional. I've managed to run Super Mario, Tetris, Super Mario Bros 2, Castlevania 2 and a number of "modern" open source ROMs. Audio has many issues, but is there. Video output seems fine in the games I've tested. The CPU emulation is mostly correct. The most important things are probably fixing audio generation (The APU), adding the second controller, working on full savestates and normal saves, and maybe improving the love2d "frontend" (i.e a rom file selector instead of having to give it through the command line, adding configuration for controller buttons, and potentially adding debugging features).

# Controls

For now, it uses the WASD keys for movement, O for the A button, P for B, I for select and enter for start. In the future, there will probably be a configuration file with a lua table in it.
