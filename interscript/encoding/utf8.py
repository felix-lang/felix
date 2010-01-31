#line 63 "interscript/src/utf8.ipk"
def utf8(i):
  if i < 0x80:
    return chr(i)
  if i < 0x800:
    return chr(0xC0 | (i>>6) & 0x1F)+\
      chr(0x80 | i & 0x3F)
  if i < 0x10000:
    return chr(0xE0 | (i>>12) & 0xF)+\
      chr(0x80 | (i>>6) & 0x3F)+\
      chr(0x80 | i & 0x3F)
  if i < 0x200000:
    return chr(0xF0 | (i>>18) & 0x7)+\
      chr(0x80 | (i>>12) & 0x3F)+\
      chr(0x80 | (i>>6) & 0x3F)+\
      chr(0x80 | i & 0x3F)
  if i < 0x4000000:
    return chr(0xF8 | (i>>24) & 0x3)+\
      chr(0x80 | (i>>18) & 0x3F)+\
      chr(0x80 | (i>>12) & 0x3F)+\
      chr(0x80 | (i>>6) & 0x3F)+\
      chr(0x80 | i & 0x3F)
  return chr(0xFC | (i>>30) & 0x1)+\
    chr(0x80 | (i>>24) & 0x3F)+\
    chr(0x80 | (i>>18) & 0x3F)+\
    chr(0x80 | (i>>12) & 0x3F)+\
    chr(0x80 | (i>>6) & 0x3F)+\
    chr(0x80 | i & 0x3F)

def seq_to_utf8(a):
  s = ''
  for ch in a: s = s + utf8(ch)
  return s

def parse_utf8(s,i):
  lead = ord(s[i])
  if lead & 0x80 == 0:
    return lead & 0x7F,i+1 # ASCII
  if lead & 0xE0 == 0xC0:
    return ((lead & 0x1F) << 6)|\
      (ord(s[i+1]) & 0x3F),i+2
  if lead & 0xF0 == 0xE0:
    return ((lead & 0x1F)<<12)|\
      ((ord(s[i+1]) & 0x3F) <<6)|\
      (ord(s[i+2]) & 0x3F),i+3
  if lead & 0xF8 == 0xF0:
    return ((lead & 0x1F)<<18)|\
      ((ord(s[i+1]) & 0x3F) <<12)|\
      ((ord(s[i+2]) & 0x3F) <<6)|\
      (ord(s[i+3]) & 0x3F),i+4
  if lead & 0xFC == 0xF8:
    return ((lead & 0x1F)<<24)|\
      ((ord(s[i+1]) & 0x3F) <<18)|\
      ((ord(s[i+2]) & 0x3F) <<12)|\
      ((ord(s[i+3]) & 0x3F) <<6)|\
      (ord(s[i+4]) & 0x3F),i+5
  if lead & 0xFE == 0xFC:
    return ((lead & 0x1F)<<30)|\
      ((ord(s[i+1]) & 0x3F) <<24)|\
      ((ord(s[i+2]) & 0x3F) <<18)|\
      ((ord(s[i+3]) & 0x3F) <<12)|\
      ((ord(s[i+4]) & 0x3F) <<6)|\
      (ord(s[i+5]) & 0x3F),i+6
  return lead, i+1 # error, just use bad character

