import os
import sys
import string

cmd = (
  '/usr/bin/time -p 2>tmp.time bin/flx --test %s %s >%s.output' %
  (sys.argv[1],string.join(sys.argv[2:]),sys.argv[1])
  )

result = os.system(cmd)
if result != 0:
  print "FAILED!"
  sys.exit(result)

f = open("tmp.time");
h = f.readlines()
f.close()
for line in h:
  if line[:4]=='user':
    utime = string.strip(line[4:])
    p = string.split(sys.argv[1],'/')[-1]
    p = string.split(p,'.')[0]
    p = (p + " .......................")[:20]
    print p,utime
