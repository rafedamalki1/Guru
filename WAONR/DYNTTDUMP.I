
/*------------------------------------------------------------------------
    File        : DYNTTDUMP.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Jun 04 13:26:46 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE tname AS CHARACTER NO-UNDO. 
DEFINE VARIABLE dname AS CHARACTER NO-UNDO. 
DEFINE VARIABLE orgbufh AS HANDLE NO-UNDO.
DEFINE VARIABLE ttcopyh AS HANDLE NO-UNDO.
DEFINE VARIABLE ttbuffcopyh AS HANDLE NO-UNDO.
DEFINE VARIABLE cFile                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReadMode               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSchemaLocation         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOverrideDefaultMapping AS LOGICAL   NO-UNDO.
DEFINE VARIABLE initDir                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dialogVar               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cSourceType AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE tabnamn NO-UNDO
   FIELD NAMN AS CHARACTER
   FIELD DUMPNAMN AS CHARACTER
   FIELD WHEREFRASE AS CHARACTER
   FIELD UTDUMP AS LOGICAL
   INDEX NAMN IS PRIMARY NAMN.
PROCEDURE tabkoll_UI :
   DEFINE VARIABLE iField AS INTEGER NO-UNDO.
   DEFINE VARIABLE iNumber AS INTEGER NO-UNDO.
   DEFINE VARIABLE fnamn AS CHARACTER NO-UNDO.
   
   ASSIGN
   iNumber   = ttbuffcopyh:NUM-FIELDS.
   DO iField = 1 TO iNumber:
      fnamn = ttbuffcopyh:BUFFER-FIELD(iField):NAME.
      IF fnamn MATCHES "*%*" THEN DO:
         fnamn = REPLACE(fnamn,"%","x"). 
         ttbuffcopyh:BUFFER-FIELD(iField):XML-NODE-NAME = fnamn.   
      END. 
   END.         
         
END PROCEDURE.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
