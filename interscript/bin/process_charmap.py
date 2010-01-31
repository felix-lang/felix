#!/env python
import string,sys

file=sys.argv[1]
lines=open(file).readlines()

#defaults
code_set_name=file
comment_char='%'
escape_char='/'

aliases=[]
charmap=0
mapping={}

for l in lines:
    s=l.split()
    if not len(s):
        continue
    if s[0]=="<code_set_name>":
        code_set_name=s[1]
    if s[0]=="<comment_char>":
        comment_char=s[1]
    if s[0]=="<escape_char>":
        escape_char=s[1]
    if s[0]==comment_char and s[1]=="alias":
        aliases.append(s[2])
    if s[0]=="CHARMAP":
        charmap=1
    if charmap and s[0]=="END" and s[1]=="CHARMAP":
        charmap=0
    if charmap and len(s)>3:
        mapping[s[1]]=s[2]

print("from interscript.encoding import wstring")
print("#Aliases for "+code_set_name)
for a in aliases:
    print("wstring.install_alias("+repr(a)+","+repr(code_set_name)+")")

print("#Mapping\nwstring.install_encoding_map("+repr(code_set_name)+",{\n")
for char8,ucs in list(mapping.items()):
    if string.find(char8,escape_char+'x')==0:
        print("0x"+char8[2:]+':', end=' ')
    else:
        print("#Unsupported entry %s %s" % (char8,ucs))
        continue
    if string.find(ucs,'<U')==0:
        print("0x"+ucs[2:-1]+',')
    else:
        print("-1, #Unsupported entry %s %s" % (char8,ucs))

print('})')


