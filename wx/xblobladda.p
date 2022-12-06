/*
     Filename: XBLOBLADDA.P
      Created: 2004.02.06 15:38ELPAO     
     Modified: 
*/
DEFINE NEW SHARED VARIABLE appcon AS LOGICAL NO-UNDO.


DEFINE VARIABLE tempdir AS CHARACTER NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO FORMAT "x(25)".
DEFINE VARIABLE dirlist AS CHARACTER NO-UNDO FORMAT "x(60)".
DEFINE VARIABLE attrlist AS CHARACTER NO-UNDO FORMAT "x(2)".
DEFINE VARIABLE tempfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE blobid AS INTEGER NO-UNDO.
DEFINE VARIABLE exeinfovar AS CHARACTER NO-UNDO.
DEFINE VARIABLE blobproch AS HANDLE NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.

&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED
{BLOB.I}

appcon = FALSE.
RUN DYNBLOB.P PERSISTENT SET blobproch.

tempdir = "\\PC012\D\delad\pro9\guru\komp9\".
INPUT FROM OS-DIR(tempdir) NO-ECHO.
REPEAT:
   /*Hämtar filnamn, hela sökvägen och vilken typ av fil det är*/
   SET ^ dirlist ^ .   
   IF dirlist MATCHES "*.R" THEN DO:
      MESSAGE dirlist.
      tempfil = SUBSTRING(dirlist,R-INDEX(dirlist,"\") + 1).
      RUN blobcheck_UI IN blobproch (INPUT dirlist, OUTPUT blobid).
      exeinfovar = "".
      IF entry(2,tempfil,".") = "R" THEN DO: 
         exeinfovar = SEARCH((entry(1,tempfil,".") + ".w")).
         IF exeinfovar = ? THEN exeinfovar = SEARCH((entry(1,tempfil,".") + ".p")).
         IF exeinfovar = ? THEN exeinfovar = "".
         ELSE exeinfovar = ENTRY(NUM-ENTRIES(exeinfovar,"\") - 1, exeinfovar, "\").
      END.
      RUN blobskapa_UI IN blobproch (INPUT dirlist, INPUT-OUTPUT blobid, INPUT "PROG",
                                  INPUT exeinfovar, OUTPUT felmedd).
   END.
END.
INPUT CLOSE.
