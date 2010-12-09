#line 10 "interscript/src/options.ipk"
import re

longopts=re.compile('^--([A-Za-z][-A-Za-z_0-9]*)(?:=(.*))?$')
shortopts=re.compile('^-([A-Za-z]+)(?:=(.*))?$')


def getopt(args):
  wordno = 0
  result = []
  opts=[]
  while wordno<len(args):
    filename = ''
    word = args[wordno]
    #print 'word',word,
    match = longopts.match(word)
    if match:
      opts.append((match.group(1),match.group(2)))
      #print ':longopt'
    else:
      match = shortopts.match(word)
      if match:
        #print ':shortopt'
        for letter in match.group(1)[:-1]:
          opts.append((letter,None))
        opts.append((match.group(1)[-1],match.group(2)))
      else:
        #print ':filename'
        filename = args[wordno]
        result.append((opts,filename))
        opts=[]
    wordno = wordno + 1

  if opts:
    result.append((opts,filename))
  return result
