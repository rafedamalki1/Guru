/*filinfo.i*/
DEFINE VARIABLE outanvanv AS CHARACTER NO-UNDO.
DEFINE VARIABLE outdatornamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempfilinfo AS CHARACTER NO-UNDO.
DEFINE VARIABLE vc AS CHARACTER FORMAT "x(20)". 
DEFINE VARIABLE vcnr AS CHARACTER FORMAT "x(20)". 
DEFINE VARIABLE Urlsite AS CHARACTER NO-UNDO.
DEFINE VARIABLE Ekgnr AS CHARACTER NO-UNDO.
DEFINE VARIABLE vi AS INTEGER NO-UNDO.
   vc = FILL( ' ', 256 ).
   IF SEARCH("versioninfo.dll") NE ? THEN RUN getFullVersion ( OUTPUT vc, INPUT 256, OUTPUT vi ). 
   ELSE vc = PROVERSION.
   RUN INLOAPI.P (OUTPUT outanvanv, OUTPUT outdatornamn).
   FILE-INFO:FILE-NAME = PROGRAM-NAME(2).
   IF FILE-INFO:FILE-SIZE = ? THEN FILE-INFO:FILE-NAME = ENTRY(1,PROGRAM-NAME(2),".") + ".r".  
   {FILINFO2.I}