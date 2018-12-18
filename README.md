# luacarrot
NES emulator in lua+love based/inspired on optcarrot (https://github.com/mame/optcarrot).
A lot of things are just manually "translated" code from ruby.

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

The code itself: 
Expect a LOT of bit.ope since luajit has no bitwise operators (These will probably get aliased soon-ish to something shorter, or at least imported locally).
The code itself is going to be kind-of translated ruby with some changes, and probably a lot of sprinkled prints left from debugging. UTILS takes care of some ruby features/syntax used heavily in optcarrot, and they may be replaced with more lua-ish alternatives later on.
There might be some commented ruby left in some places (Probably because i wasn't very sure about the translation being accurate).
Since OOP is heavily used and, other than for the ROM mapping hierarchy its use wouldn't be hard to replace, other alternatives might be better and they should be refactored eventually.

The CPU should be more or less complete. Right now, if you try to run something, it will enter an infinite loop because of the APU not mapping a couple memory addresses used in CPU:do_clock() which make CPU:run_once() run forever.
I think further testing the CPU would require at least a prototype implementation of the APU (I run into something similar with the PPU earlier). That's probably what I'll start with next.

The APU is not even started.

The PPU is kind-of "prototyped" (It probably doesn't display anything useful yet, but most of the "required" code should be somewhat translated, except probably mostly `poke_2007` and `setup_lut`)

The ROM: I've only somewhat tested one mapper (0x00, default and most common afaik). I'm almost sure the last one in the rom.lua file won't work as is. The others *might*. I'm pretty sure the non-mapper specific ROM code is fine.

Love2d: There's a probably naive implementation of writing the output pixels from the ppu to screen. *If* (Big if) that's correct then the only love2d interaction left should be input.

Pads (Generic input component, iirc 1 or 2 memory addresses per pad or something) are not even started (Should be simple).

# Ruby - Lua

Things to notice if you compare with optcarrot:

f[a,b] on functions calls (f(a,b))

n[x] on numbers is equivalent to UTILS.nthBitIsSetInt(n,x)

^ between 2 numbers is a binary xor

luajit doesnt support binary operators

lua indexing normally starts at 1. A couple things might use 0-indexing since it made things easier with the current model. Once everything is more or less working as intended normalizing all table indexing should be considered.I've tried to avoid using ipairs, since according to a page on luajit performance tricks they can make things slower, and doing so allows me to make the array/numeric table functions in UTILS work for both 0 and 1 indexed tables by doing "t[0] and 0 or 1" for the first index

ruby indexing starts at 0

<< is an lshift for 2 numbers, but "[]<< x" inserts x into the [] object

ruby fibers are *quite* similar to lua coroutines (Used for PPU)

