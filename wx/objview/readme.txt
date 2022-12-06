Object Viewer 1.1
-----------------

Objview is a object-viewer for Progress V9.
You can view attributes of procedures and widgets while your 
Progress application is running.

It will show all Procedures, Windows, Frames and all other widgets 
in a treeview in the left pane. 
In the right pane you will see all attributes, properties, and optional 
the attribute-value in the 'Value Preview' pane.

Double-click on an attribute, or use the 'Value Preview' pane to 
examine attributes with lots of data, like INTERNAL-ENTRIES.

You can also examine the SESSION attributes or those of other procedures,
including the native Progress procedures, like those of the AppBuilder itself.

When a SmartObject is selected, you can view 'ADM Properties' via the 'View' menu.
Those ADM Properties contained in the Admprops temp-table are colored red,
and those via the get/set functions are in blue.

When you select a non-SmartObject, the viewer will return to (Widget) 'Attribute' mode.


Usage
-----

Objview works best when installed in your PRO*Tools.
add Object Viewer to your Protools palette, (right click) 
via the 'Customize' menu from protools,

Label     : Object Viewer
Program   : objview/objview.w
Persistent: no
Display   : yes
Image     : objview/objview.ico

or manually add this line to your gui\protools\protools.dat:

"Object Viewer" "objview/objview.w" "objview/objview" no yes 210 no


Handles:
-------
Objview tries to 'guess' handles, like in attributes as 'NEXT-SIBLING' etc. 
Using VALID-HANDLE it will trie to show you a name of the object. 
Sometimes other attributes like X or WIDTH-PIXELS have values that are 
'valid' handles by accident, and as a result objview shows you an objectname.
I trust that you can recognize a real valid handle when you see one ...



installation:
------------
Unpack this archive in a subdirectory 'objview' somewhere in your PROPATH.
You can add the contents of protools.dat to your own protools.dat.
Although this program may work in V8, it has all V9 attributes 'inside'.



OBJVIEW 
-------
written by Rob den Boer.
homepage: http://home.hccnet.nl/rc.den.boer/progress/index.html
email   : rc.den.boer@hccnet.nl

History:
-------
28-01-2000 initial release version 1.0

20-02-2000 version 1.1:
           view ADM properties of SmartObjects. (Menu 'View', ADM properties).
           MENU's and MENU-ITEMS are shown properly.
           BROWSE-COLUMNS of a BROWSE are shown.
           
