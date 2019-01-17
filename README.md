# LuaNES
NES emulator in lua+love based/inspired on optcarrot (https://github.com/mame/optcarrot).

# Running

Get https://love2d.org/ and run `love . rom.nes` in the repo folder.

# Overview

The NES basically has 3 "main" or big components: the PPU (Picture Processing Unit), CPU and APU (Audio Processing Unit).
ROMs "map" their memory to CPU addreses with different mappers, depending on a code in the rom header. 

Reference:

optcarrot docs:

https://www.slideshare.net/mametter/optcarrot-a-pureruby-nes-emulator
https://github.com/mame/optcarrot/blob/master/doc/internal.md 

cpu instructions:

http://www.oxyron.de/html/opcodes02.html

Other:

http://www.dustmop.io/blog/2015/04/28/nes-graphics-part-1/ This has a really nice graphical representation of how the PPU works

https://wiki.nesdev.com/w/index.php contains almost everything you could wish for

http://www.fceux.com/web/help/fceux.html?Introduction.html fceux docs

https://github.com/TASVideos/fceux is an emulator in c++, and as far as i could find it's supposed to be the most "correct" emulator out there

http://www.qmtpro.com/~nes/misc/nestest.log This can be used to compare the output of printing state for every instruction for running tests/nestest.rom . Note that in order to have it run all the tests "automatically" it should start reading from address C000. Last i checked the output was pretty much correct.

Wikipedia pages might also be helpful.

# Progress

Should be somewhat functional. Audio is not being output yet (I'm not sure if you can even stream raw samples in love2d), and the output from the APU hasn't been tested (At least the timing of it seems to be correct). It's also still not capping at 60 fps, so the speed will vary. Performance needs to improve a little bit more to be able to run at a somewhat stable 60fps.
