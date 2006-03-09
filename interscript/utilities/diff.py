#line 75 "interscript/src/diff.ipk"
import os
import tempfile
import string
import re

def compare_files(o,m):
  cmd = 'diff -q '+o+' '+m
  f = os.popen(cmd,'r')
  output = f.read()
  result = f.close()
  return len(output)==0

def compare_code_files(o,m,**kwds):
  # slack implementation
  return compare_files(o,m)

def diff_files(o,m,patch=None, context=10):
  cmd = 'diff -C'+str(context)+' '+o+' '+m
  f = os.popen(cmd,'r')
  output = f.read()
  result = f.close()
  if patch:
    f = open(patch,'w')
    f.write(output)
    f.close()
  return output

def diff_strings(o,m,context=0):
  foname = tempfile.mktemp()
  fmname = tempfile.mktemp()
  fo = open(foname,'w')
  fm = open(fmname,'w')
  fo.write(o)
  fm.write(m)
  fo.close()
  fm.close()
  result = diff_files(foname, fmname,context=context)
  os.unlink(foname)
  os.unlink(fmname)
  return result

def diff_lines(o,m,context=0):
  os = string.join(o,'\n')+'\n'
  om = string.join(m,'\n')+'\n'
  result = diff_strings(os,om,context=context)
  del os
  del om
  data = string.split(result,'\n')[:-1]
  if data == []: return []
  cs = data[0][0]
  cm = data[1][0]
  sep = data[2]
  lth = len(data)
  sections = []
  for i in range(2,lth):
    if data[i] == sep:
      sections.append([])
    else:
      sections[-1].append(data[i])
  del data
  del lth
  del sep

  for i in range(len(sections)):
    section = sections[i]
    sections[i] = []
    for j in range(len(section)):
      line = section[j]
      code = line[0]+line[1]
      if code == cs*2 or code == cm*2:
        k = 0
        first = 0
        count = 0
        while line[k] not in '0123456789': k = k + 1
        while line[k] in '0123456789':
          first = first * 10 +ord(line[k])-ord('0')
          k = k + 1
        first = first - 1
        sections[i].append([[first,0]])
      else:
        lineno = first + count
        count = count + 1
        sections[i][-1][0][1] = count
        sections[i][-1].append(('%3d'%(lineno+1))+':'+line)
  return sections

def patch_file(o,diff,m):
  cmd = 'patch -s -c -o ' + m + ' ' + o + ' -'
  print cmd
  f = os.popen(cmd,'w')
  f.write(diff)
  result = f.close()

def posix_patch(o,diff,m):
  patch_file(o,diff,m)

