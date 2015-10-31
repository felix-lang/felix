Look in JEdit's help file for more detailed information how to
install edit modes. On my windows machine I did...

Step 1) Install JEdit. Get it from http://www.jedit.org/   (needs Java)

Step 2) Place the file felix.xml in the directory C:\Program\jEdit\modes

Step 3) Take a backup of the file named catalog

Step 4) Open the catalog file in a text editor and insert the following tag:

<MODE NAME="felix" FILE="felix.xml" FILE_NAME_GLOB="*.flx" />

Observe that this tag must be placed between the <MODES> and </MODES> tags!

Step 5) Save the catalog file.
It's not nessacary, but I highly recommend to also install the plug-in "Editor Scheme Selector" from http://www.jedit.org/
(It includes the PLEAC sheme which is my favourite:)
