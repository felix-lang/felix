#line 24 "interscript/src/interscript_languages.ipk"
from interscript.encoding.utf8 import utf8
import re

interscript_phrases = {
  'Contents': {
    'es': "Contenidos"
   },
}

slash_u = re.compile(r'\\u(....)|\\U(........)')

def parse_escapes(text):
  pos = 0
  s = ''
  match = slash_u.search(text,pos)
  while match:
    first, last = match.start(0), match.end(0)
    s = s + text[pos:first]
    if match.group(1): hexcode = match.group(1)
    else: hexcode = match.group(2)
    value = hexval(hexcode)
    utf = utf8(value)
    s = s + utf
    pos = last
    match = slash_u.search(text,pos)
  s = s + text[pos:]
  return s

def tr_phrase(native_phrase, language):
  x = parse_escapes(native_phrase)
  d = interscript_phrases.get(x,{})
  tr = d.get(language,x)
  return parse_escapes(tr)

def phrase_list():
  keys = list(interscript_phrases.keys())
  keys.sort()
  return keys

def add_translation(native_phrase, **kwds):
  if native_phrase not in interscript_phrases:
    interscript_phrases[native_phrase]={}
  interscript_phrases[native_phrase].update(kwds)


