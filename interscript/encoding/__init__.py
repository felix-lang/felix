#line 63 "interscript/src/encoding.ipk"
# encoding subpackage

iso10646_signatures = (
  ('\xFE\xFF'        , 'utf16'),   # unicode, also ucs2
  ('\xFF\xFE'        , 'utf16le'), # little endian unicode, also ucs2le
  ('\x00\x00\xFE\xFE' , 'ucs4'),    # full four byte encoding
  ('\xFE\xFE\x00\x00' , 'ucs4le'),  # little endian four byte encoding
  ('\xEF\xBB\xBF'    , 'utf8')     # utf8
)

def autodetect(filename):
  f = open(filename,'rb')
  data = f.read(4)
  f.close()
  data = data +'XXXX'
  for signature,name in signatures:
    if signature == data[:len(signature)]:
      return name
  return 'utf8'

