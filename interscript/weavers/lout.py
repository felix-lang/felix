#line 30 "interscript/src/lout_weaver.ipk"
from interscript.weavers.base import weaver_base
import string

from interscript.drivers.sinks.bufdisk import named_file_sink
def mk_lout(pass_frame,basename, directory, prefix, eol, title, language):
  if language: filename = directory + language + '_' + basename + '.lout'
  else: filename = directory + basename + '.lout'
  w = named_file_sink(pass_frame,filename, prefix)
  if not title: title = basename
  return lout_weaver(pass_frame,w, Title=title,language=language,
    documentclass="report",
    Include = 'tab',
    InitialFont = "Times Base 10p",
    InitialSpace = "tex",
    CoverSheet = 'No',
    PageHeaders = "Titles"
   )

class lout_weaver(weaver_base):
    def __init__(self, pass_frame, writer, language='', **kwds):
        weaver_base.__init__(self, pass_frame, language)
        self.sink = writer
        if 'weavers' in self.process.trace:
          print 'initialising Lout weaver, writer',writer.get_sink_name()
        self.protocol = 'Lout'
        self.persistent_frame['protocol']=self.protocol
        self.tags = ['lout']
        self.enabled = 1
        self.acount = 1
        self.tag_stack = []
        self.comments = 0
        self.master = pass_frame.master
        self.list = []
        self.heading_level_offset = 0
        self.currentLevel = 0
        self.name = 'Lout weaver v1 for '+self.sink.name
        self.persistent_frame['name']=self.name
        self.verbatim = 0
        self.documentClass = None
        self.translating = 1
        self.withinCode = None
        self.prolog(kwds)
#line 74 "interscript/src/lout_weaver.ipk"
    def __del__(self):
        self.epilog()

#line 153 "interscript/src/lout_weaver.ipk"
    def prolog(self,kwds):
        if kwds.has_key('Include'):
            includes = kwds['Include']
            if type(includes) != type([]):
                includes = [includes]

            for i in includes:
                self._writeline("@Include{%s}" % i)

        self.documentClass = 'book'
        if kwds.has_key('documentclass'):
            self.documentClass = kwds['documentclass']

        self._writeline("@Include{ " + self.documentClass + "}")
        if self.documentClass == 'book':
            self._writeline("@Book")
        elif self.documentClass == 'report':
            self._writeline("@Report")
        else:
            self._writeline("@Document")
#line 187 "interscript/src/lout_weaver.ipk"
        docOptions = ['InitialFont', 'InitialLanguage',
                      'OptimizePages', 'PageHeaders', 'InitialSpace']

        if self.documentClass == 'report':
            docOptions = docOptions + ['Author', 'Title',
                                       'Institution', 'DateLine',
                                       'CoverSheet'
                                       ]
        elif self.documentClass == 'book':
            docOptions = docOptions + ['Title' , 'Author',
                                       'Publisher', 'DateLine'
                                       ]

        for k in docOptions:
            if kwds.has_key(k):
                self._writeline("    @%s{%s}" % (k, kwds[k]))

        if kwds.has_key("heading_level_offset"):
            self.heading_level_offset = int(kwds["heading_level_offset"])
        else:
            self.heading_level_offset = 0
#line 213 "interscript/src/lout_weaver.ipk"
        self._writeline("//")

        if self.documentClass == 'doc':
            self._writeline("@Text @Begin")
#line 229 "interscript/src/lout_weaver.ipk"
        self.inSubSubSection = 0
        self.inSubSection = 0
        if self.documentClass == 'report':
            self.inSection = 1
            self.sectionLevels = ['Section',
                                  'SubSection',
                                  'SubSubSection']
        elif self.documentClass == 'book':
            self.inSection = 0
            self.sectionLevels = ['Chapter' ,
                                  'Section',
                                  'SubSection',
                                  'SubSubSection']
        else:
            self.inSection = 0
            self.sectionLevels = ['Section',
                                  'SubSection',
                                  'SubSubSection']

        self.maxSectionLevels = len(self.sectionLevels)
#line 253 "interscript/src/lout_weaver.ipk"
    def epilog(self):
        self.closeLevels(0)
        if self.documentClass == 'doc':
            self._writeline("@End @Text")
#line 277 "interscript/src/lout_weaver.ipk"
    loutCharMap = { "@"  : "{@Char at}",
                    "\"" : "{@Char quotedbl}",
                    "\\" : "{@Char backslash}",
                    "{"  : "{@Char braceleft}",
                    "}"  : "{@Char braceright}",
                    "/"  : "{@Char slash}",
                    "#"  : "{@Char numbersign}",
                    "~"  : "{@Char tilde}",
                    "^"  : "{@Char asciicircum}",
                    "&"  : "{@Char ampersand}",
                    "$"  : "{@Char dollar}",
                    "|"  : "{@Char bar}"
                    }

    def cvt_code(self, text):
        n = []
        for c in text:
            if lout_weaver.loutCharMap.has_key(c):
                n.append(lout_weaver.loutCharMap[c])
            else:
                n.append(c)

        return string.join(n,'')

    def cvt_text(self, text):
        if self.translating:
            return self.cvt_code(text)
        else:
            return text
#line 325 "interscript/src/lout_weaver.ipk"
    def set_translating(self, translation=1):
        self.translating = translation
#line 335 "interscript/src/lout_weaver.ipk"
    def identifier_reference(self, hlevel=2, *args, **kwds):
        print "identifier_reference(hlevel=%d, %s, %s)" % (hlevel,
                                                           `args`,
                                                           `kwds`)
        #RESOLVE
        return
#line 343 "interscript/src/lout_weaver.ipk"
    def class_reference(self, hlevel=2, *args, **kwds):
        print "class_reference(hlevel=%d, %s, %s)" % (hlevel,
                                                           `args`,
                                                           `kwds`)
        #RESOLVE
        return
#line 351 "interscript/src/lout_weaver.ipk"
    def print_contents(self, *args, **kwds):
        print "print_contents(%s, %s)" % (`args`,
                                          `kwds`)
        #RESOLVE
        return
#line 358 "interscript/src/lout_weaver.ipk"
    def print_file_list(self, hlevel=2, *args, **kwds):
        print "print_file_list(hlevel=%d, %s, %s)" % (hlevel,
                                                      `args`,
                                                      `kwds`)
        #RESOLVE
        return
#line 366 "interscript/src/lout_weaver.ipk"
    def print_source_list(self, hlevel=2, *args, **kwds):
        print "print_source_list(hlevel=%d, %s, %s)" % (hlevel,
                                                        `args`,
                                                        `kwds`)
        #RESOLVE
        return
#line 399 "interscript/src/lout_weaver.ipk"
    def _writeline(self,line=''):
        if self.enabled:
            self.sink.writeline(line)

    def _write(self,line):
        if self.enabled:
            self.sink.write(line)

    def write(self,line):
        self._write(self.cvt_text(line))

    def writeline(self,line=''):
        self.write(line);
        self._writeline()

    def writecode(self,line):
        self._writeline('@F{'+self.cvt_code(line)+'}')

#line 421 "interscript/src/lout_weaver.ipk"
    def begin_displayed_text(self):
        self._write('@QuotedDisplay {\n')

    def end_displayed_text(self):
        self._write('}\n')

    def begin_displayed_code(self):
        self._writeline('@QD lines @Break {Courier Base} @Font lout @Space{')

    def end_displayed_code(self):
        self._writeline('}')

    def line_break(self):
        self._writeline('//')

    def page_break(self):
        self._writeline('@NP')

    def write_tagged(self, tag, data):
        # RESOLVE: What are the other tags here?
        if tag == "small":
            self._write("{-1p} @Font {%s}" % self.cvt_text(data))
        else:
            self.write(data)
        return
#line 500 "interscript/src/lout_weaver.ipk"
    def code_head(self, tangler, secno):
        self.withinCode = (tangler, secno)
        self._writeline(
                "@CNP @ID {{-2p} @Font{Start section to " + \
                self.cvt_code(tangler.sink.get_sink_name())+\
                '['+str(secno)+']}\n//.25f @HExpand{@FullWidthRule{}} //.25f\n')
        self._writeline("lines @Break{Courier Base -1p} @Font " +
                        "lout @Space{\n")

    def code_foot(self, tangler, secno):
        self._writeline("}")
        self._writeline(
            '//.25f @HExpand{@FullWidthRule{}} ' +
            '//.25f {-2p} @Font{End section to ' +
            self.cvt_code(tangler.sink.get_sink_name()) +
            '['+str(secno)+']}}')
        self.withinCode = None
#line 533 "interscript/src/lout_weaver.ipk"
    def start_code_line(self,count=None):
        if count:
            self._write('%6d: ' % count)
        else:
            self._write('       ')
        return

    def end_code_line(self):
        self._writeline()
        return

    markups = {'COMMENT' : '{Oblique} @Font {',
               'KEYWORD' : '{Bold} @Font {',
               }
    def write_code_fragment(self,frag,markup=None):
        haveMarkup = 0
        if markup and lout_weaver.markups.has_key(markup):
            self._write(lout_weaver.markups[markup])
            haveMarkup = 1

        if frag:
            self._write(self.cvt_code(frag))

        if haveMarkup:
            self._write('}')
        return

#line 582 "interscript/src/lout_weaver.ipk"
    def head(self,level, text, **kwds):
      atext=kwds.get('short_text')
      anchor=kwds.get('key','')
        level = level + self.heading_level_offset
        if level > self.maxSectionLevels:
            level = self.maxSectionLevels

        if anchor == '':
            anchor = atext

#line 604 "interscript/src/lout_weaver.ipk"
        if self.currentLevel > level:
            self.closeLevels(level)
#line 612 "interscript/src/lout_weaver.ipk"
        sectionType = self.sectionLevels[level-1]
        if self.currentLevel == level:
            # End the previous entity
            self._writeline("@End @%s" % sectionType)
        elif self.currentLevel < level:
            self.openLevels(level)
#line 624 "interscript/src/lout_weaver.ipk"
        self._writeline("@%s @Title{%s}" % (sectionType,
                                            self.cvt_text(text)))
        if anchor != '':
            self._writeline("    @Tag{%s}" % anchor)
        self._writeline("@Begin")
        self.currentLevel = level

#line 645 "interscript/src/lout_weaver.ipk"
    def openLevels(self, newLevel):
        assert newLevel > self.currentLevel, \
               "Invalid call to openLevels with %d, current %d" % \
               (newLevel, self.currentLevel)

        # We do nothing for level 1 unless the document class is doc.

        if newLevel == 1 and self.documentClass != 'doc':
            return

        while self.currentLevel < newLevel:
            lev = self.currentLevel
            self._writeline("@Begin%ss" % self.sectionLevels[lev])
            self.currentLevel = lev + 1
        return
#line 673 "interscript/src/lout_weaver.ipk"
    def closeLevels(self, newLevel):
        assert newLevel <= self.currentLevel, \
            "Incorrect closeLevels call (%d, current is %d)" % \
            (newLevel, self.currentLevel)
        if self.withinCode:
            self.code_foot(self.withinCode[0], self.withinCode[1])
        while self.currentLevel > newLevel:
            lev = self.currentLevel - 1
            sectionType = self.sectionLevels[lev]
            self._writeline("@End @%s" % sectionType)
            if self.documentClass == 'doc' or lev > 0:
                self._writeline("@End%ss" % sectionType)
            self.currentLevel = lev
        return
#line 728 "interscript/src/lout_weaver.ipk"
    def begin_table(self, *headings, **kwds):
        self._writeline("@Table")
        if kwds.has_key("caption"):
            self._writeline("    @Caption{%s}" % self.cvt_text(kwds['caption']))

        if kwds.has_key("tag"):
            self._writeline("    @Tag{%s}" % kwds['tag'])

        if kwds.has_key("location"):
            self._writeline("    @Location{%s}" % kwds['location'])

        self._writeline("{ @Tab")
        # Run through the rule options and do the right thing

        for opt in ("above", "below", "side", "between"):
            if kwds.has_key(opt):
                self._write("%s{%s} " % (opt, kwds[opt]))
        self._writeline()

        numCols = len(headings)
        colName = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        if numCols > len(colName):
            print "lout_weaver.begin_table: Using first %d columns" % len(colName)
            numCols = len(colName)

        self._write("    @Fmta{")
        for i in range(numCols):
            if i > 0:
                self._write(" ! ")
            self._write("@Col %s" % colName[i])
        self._writeline("}\n{")

        self._write("    @Rowa ")
        for i in range(numCols):
            self._write("%s{@B{%s}} " % (colName[i],
                                         self.cvt_text(headings[i])))
        self._writeline()
        return

    def end_table(self):
        self._writeline("}}")
        return
#line 782 "interscript/src/lout_weaver.ipk"
    def table_row(self, data):
        colName = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        numCols = len(data)
        if numCols > colName:
            print "lout_weaver.table_row: Using first %d columns" % len(colName)
            numCols = len(colName)

        self._write('    @Rowa ')
        for i in range(numCols):
            self._write("%s{%s} " % (colName[i], self.cvt_text(data[i])))
        self._writeline()
        return

    def table_rule(self):
        #RESOLVE
        return
#line 815 "interscript/src/lout_weaver.ipk"
    def begin_numbered_list(self, start=1, type='1'):
        self._write("\n@NumberedList")
        if start != 1:
            self._write(" start{%d}" % start)
        self._writeline()
        return

    def end_numbered_list(self):
        self._writeline("@EndList")

    def begin_numbered_list_item(self):
        self._write("@LI{")

    def end_numbered_list_item(self):
        self._writeline("}")
#line 846 "interscript/src/lout_weaver.ipk"
    def begin_bullet_list(self):
        self._writeline("\n@BulletList")

    def end_bullet_list(self):
        self._writeline("@EndList")

    def begin_bullet_list_item(self):
        self._write("@LI{")

    def end_bullet_list_item(self):
        self._writeline("}")
#line 869 "interscript/src/lout_weaver.ipk"
    def begin_keyed_list(self):
        self._writeline("\n@TaggedList")

    def end_keyed_list(self):
        self._writeline("@EndList")

    def begin_keyed_list_item(self, key):
        self._write("@DTI{")
        self.write(key)
        self._write("}{")

    def end_keyed_list_item(self):
        self._writeline("}")

#line 885 "interscript/src/lout_weaver.ipk"
    def echotangle(self, count, data):
        if self.comments:
            self._writeline(data)
        else:
            self._writeline("%6d: %s" % (count, self.cvt_code(data)))
        return

#line 894 "interscript/src/lout_weaver.ipk"
    def prose(self):            # Start paragraph
        self._write("\n@LP ")

    def par(self):                      # Paragraph separator.
        self._write("\n@LP ")

    def eop(self):                      # End of paragraph
        self._write("\n")

    def write_comment(self, v):
        #RESOLVE
        self.write_tagged('small', v)
#line 908 "interscript/src/lout_weaver.ipk"
    def begin_code(self):
        self._write("@F{")

    def end_code(self):
        self._write("}")

    def begin_emphasize(self):
        self._write("@I{")

    def end_emphasize(self):
        self._write("}")

    def begin_strong(self):
        self._write("@B{")

    def end_strong(self):
        self._write("}")

    def begin_italic(self):
        self._write("@I{")

    def end_italic(self):
        self._write("}")

    def begin_bold(self):
        self._write("@B{")

    def end_bold(self):
        self._write("}")

    def begin_big(self):
        self._write("{+2p} @Font{")

    def end_big(self):
        self._write("}")

    def begin_small(self):
        self._write("{-2p} @Font{")

    def end_small(self):
        self._write("}")
#line 951 "interscript/src/lout_weaver.ipk"
    def set_fc_anchor(self, file, count):
        #RESOLVE
        return

#line 957 "interscript/src/lout_weaver.ipk"
    def cite_url(self,url):
        self._write("{Helvetica Base} @Font {")
        self.write(url)
        self._write("}")
