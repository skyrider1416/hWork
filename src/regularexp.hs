-- tests of Regular Expressions
-- import Text.Regex.Posix
import Text.Regex.TDFA

s = "The Quick Brown fox jumps over the lazy dog."

pat = "[aeiou]"

sn = "123.456"

patd = "[0-9]*"

patw = "[ \t]+"