#line 297 "interscript/src/compilers.ipk"
import os
import sys
import string

class config:
  def __init__(self,**kwds):
    self.libdirs = []
    self.incdirs = []
    self.libs = []
    self.macros = {}
    self.switches = {}
    self.extra = ''
    self.append_dict(kwds)

  def copy(self):
    c = config()
    c.libdirs = self.libdirs[:]
    c.incdirs = self.incdirs[:]
    c.libs = self.libs[:]
    c.macros = self.macros.copy()
    c.switches = self.switches.copy()
    c.extra = self.extra
    return c

  def append_kwds(self,**kwds):
    self.append_dict(kwds)

  def append_dict(self,kwds):
    if kwds.has_key('libdirs'):
      self.libdirs[-1:-1]=kwds['libdirs']
    if kwds.has_key('incdirs'):
      self.incdirs[-1:-1]=kwds['incdirs']
    if kwds.has_key('libs'):
      self.libs[-1:-1]=kwds['libs']
    if kwds.has_key('extra'):
      self.extra = self.extra + ' ' + kwds['extra']
    if kwds.has_key('macros'):
      macros = kwds['macros']
      for macro in macros:
        self.macros[macro] = macros[macro]
    if kwds.has_key('switches'):
      switches = kwds['switches']
      for switch in switches:
        self.switches[switch] = switches[switch]

  def __str__(self):
    s = self.extra
    for x in self.libdirs: s = s + ' -L' + x
    for x in self.incdirs : s = s + ' -I' + x
    for x in self.libs: s = s + ' -l' + x
    for x in self.macros.keys():
      s = s + ' -D' + x
      if self.macros[x]: s = s + '=' + self.macros[x]
    for x in self.switches.keys():
      s = s + ' -' + x + self.switches[x]
    return s

