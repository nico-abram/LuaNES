local complex = require "libs/complex"

local band, bor, bxor, bnot, lshift, rshift = bit.band, bit.bor, bit.bxor, bit.bnot, bit.lshift, bit.rshift
local map, rotatePositiveIdx, nthBitIsSet, nthBitIsSetInt =
    UTILS.map,
    UTILS.rotatePositiveIdx,
    UTILS.nthBitIsSet,
    UTILS.nthBitIsSetInt

PALETTE = {}
local PALETTE = PALETTE

function PALETTE:defacto_palette()
    local p =
        UTILS.flat_map(
        {
            {1.00, 1.00, 1.00}, -- default
            {1.00, 0.80, 0.81}, -- emphasize R
            {0.78, 0.94, 0.66},
            -- emphasize G
            {0.79, 0.77, 0.63}, -- emphasize RG
            {0.82, 0.83, 1.12}, -- emphasize B
            {0.81, 0.71, 0.87}, -- emphasize RB
            {0.68, 0.79, 0.79}, -- emphasize GB
            {0.70, 0.70, 0.70} -- emphasize RGB
        },
        function(t)
            local rf, gf, bf = t[1], t[2], t[3]
            -- RGB default palette (I don't know where this palette came from)
            return UTILS.map(
                {
                    0x666666,
                    0x002a88,
                    0x1412a7,
                    0x3b00a4,
                    0x5c007e,
                    0x6e0040,
                    0x6c0600,
                    0x561d00,
                    0x333500,
                    0x0b4800,
                    0x005200,
                    0x004f08,
                    0x00404d,
                    0x000000,
                    0x000000,
                    0x000000,
                    0xadadad,
                    0x155fd9,
                    0x4240ff,
                    0x7527fe,
                    0xa01acc,
                    0xb71e7b,
                    0xb53120,
                    0x994e00,
                    0x6b6d00,
                    0x388700,
                    0x0c9300,
                    0x008f32,
                    0x007c8d,
                    0x000000,
                    0x000000,
                    0x000000,
                    0xfffeff,
                    0x64b0ff,
                    0x9290ff,
                    0xc676ff,
                    0xf36aff,
                    0xfe6ecc,
                    0xfe8170,
                    0xea9e22,
                    0xbcbe00,
                    0x88d800,
                    0x5ce430,
                    0x45e082,
                    0x48cdde,
                    0x4f4f4f,
                    0x000000,
                    0x000000,
                    0xfffeff,
                    0xc0dfff,
                    0xd3d2ff,
                    0xe8c8ff,
                    0xfbc2ff,
                    0xfec4ea,
                    0xfeccc5,
                    0xf7d8a5,
                    0xe4e594,
                    0xcfef96,
                    0xbdf4ab,
                    0xb3f3cc,
                    0xb5ebf2,
                    0xb8b8b8,
                    0x000000,
                    0x000000
                },
                function(rgb)
                    local r = math.min(math.floor(band(rshift(rgb, 16), 0xff) * rf), 0xff)
                    local g = math.min(math.floor(band(rshift(rgb, 8), 0xff) * gf), 0xff)
                    local b = math.min(math.floor(band(rshift(rgb, 0), 0xff) * bf), 0xff)
                    --return bor(0x00000000, lshift(r, 16), lshift(g, 8), b)
                    return {r, g, b}
                end
            )
        end
    )
    return p
end
--[
-- Nestopia generates a palette systematically (cool!), but it is not compatible with nes-tests-rom
function PALETTE:nestopia_palette()
    return UTILS.map(
        range(0, 511),
        function(n)
            local tint, level, color = band(rshift(n, 6), 7), band(rshift(n, 4), 3), band(n, 0x0f)
            local t = ({{-0.12, 0.40}, {0.00, 0.68}, {0.31, 1.00}, {0.72, 1.00}})[level + 1]
            local level0, level1 = t[1], t[2]
            if color == 0x00 then
                level0 = level1
            end
            if color == 0x0d then
                level1 = level0
            end
            if color >= 0x0e then
                level0 = 0
                level1 = 0
            end
            local y = (level1 + level0) * 0.5
            local s = (level1 - level0) * 0.5
            local iq = complex.convpolar(s, math.pi / 6 * (color - 3))
            if tint ~= 0 and color <= 0x0d then
                if tint == 7 then
                    y = (y * 0.79399 - 0.0782838) * 1.13
                else
                    level1 = (level1 * (1 - 0.79399) + 0.0782838) * 0.5
                    y = y - level1 * 0.5
                    if tint == 3 or tint == 5 or tint == 6 then
                        level1 = level1 * 0.6
                        y = y - level1
                    end
                    iq = iq + complex.convpolar(level1, math.pi / 12 * (({0, 6, 10, 8, 2, 4, 0, 0})[tint + 1]) * 2 - 7)
                end
            end
            return UTILS.map(
                {{105, 0.570}, {251, 0.351}, {15, 1.015}},
                function(pair)
                    local angle, gain = pair[1], pair[2]

                    local clr =
                        y + ((complex.convpolar(gain * 2, (angle - 33) * math.pi / 180) * complex.conjugate(iq))[1])
                    return math.min(math.max(0, math.floor(clr * 255)), 255)
                end
            )
        end
    )
end
--]]
