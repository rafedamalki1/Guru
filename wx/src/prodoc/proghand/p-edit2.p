/* p-edit2.p */

DEFINE VARIABLE e AS CHARACTER VIEW-AS EDITOR
                      INNER-CHARS 70 INNER-LINES 15 SCROLLBAR-VERTICAL.
DEFINE VARIABLE filename AS CHARACTER FORMAT "x(60)".
DEFINE VARIABLE got-file AS LOGICAL.
DEFINE VARIABLE status-ok AS LOGICAL.

DEFINE SUB-MENU filemenu
   MENU-ITEM fopen LABEL "Open..."
   MENU-ITEM fsave LABEL "Save As..."
   MENU-ITEM fexit LABEL "E&xit".
   
DEFINE MENU mainbar MENUBAR
   SUB-MENU filemenu LABEL "File".

FORM
  e
  WITH FRAME edit-frame.

FORM
  "Enter Filename:" SKIP
  filename
  WITH FRAME file-spec NO-LABELS VIEW-AS DIALOG-BOX.
     
ON RETURN OF filename IN FRAME file-spec
   APPLY "GO" TO filename.

ON CHOOSE OF MENU-ITEM fopen
   DO:
      FRAME file-spec:TITLE = "Open File".
      UPDATE filename WITH FRAME file-spec. 
      status-ok = e:READ-FILE(filename) IN FRAME edit-frame.
      IF NOT status-ok
      THEN MESSAGE "Could not read" filename.
   END.   
   
ON CHOOSE OF MENU-ITEM fsave
   DO:
      FRAME file-spec:TITLE = "Save File".
      UPDATE filename WITH FRAME file-spec.
      status-ok = e:SAVE-FILE(filename) IN FRAME edit-frame.
      IF NOT status-ok
      THEN MESSAGE "Could not write to" filename. 
   END.        
  
CURRENT-WINDOW:MENUBAR = MENU mainbar:HANDLE.
                 
ENABLE e WITH FRAME edit-frame NO-LABELS.

WAIT-FOR CHOOSE OF MENU-ITEM fexit OR
         WINDOW-CLOSE OF CURRENT-WINDOW.
