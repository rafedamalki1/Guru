/*SELGARABATT.P INLÄSNING AV EBRMATERIEL*/       
DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.




DEFINE VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE satsvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE enrvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE melvar AS INTEGER NO-UNDO.
DEFINE VARIABLE melvar2 AS INTEGER NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO.
DEFINE VARIABLE pos1 AS INTEGER NO-UNDO.

DEFINE BUFFER mtrlbuff FOR MTRL.

DEFINE TEMP-TABLE tidin
   FIELD GRUPP                AS CHARACTER
   FIELD RABATT               AS CHARACTER.
   

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
   
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.   
{muswait.i}      
   filnamn = "F:\elpool\elpnj\KALMAR\SELGA\311035.SKV".
   EMPTY TEMP-TABLE tidin NO-ERROR.    

/*    wtidvar = wtidvar + "elef.q".                                                         */
/*    ASSIGN                                                                                */
/*    dlcvar = dlcvar + "QUOTER.EXE".                                                       */
/*    OS-COMMAND SILENT VALUE(dlcvar)                                                       */
/*    VALUE(filnamn) > VALUE(wtidvar).                                                      */
/*    INPUT FROM VALUE(wtidvar)                                                             */
/*    CONVERT TARGET "iso8859-1" SOURCE "ibm850" NO-ECHO.                                   */
/*    /*iso8859-1 swedish-7-bit ibm850"*/                                                   */
/*    REPEAT:                                                                               */
/*       DO TRANSACTION:                                                                    */
/*          SET words VIEW-AS EDITOR INNER-CHARS 78 INNER-LINES 80 WITH FRAME DDD WIDTH 80. */
/*          CREATE intid.                                                                   */
/*          ASSIGN intid.TIN = words.                                                       */
/*       END.                                                                               */
/*    END.                                                                                  */
/*    INPUT CLOSE.                                                                          */
/*    OUTPUT TO VALUE(wtidvar).                                                             */
/*    FOR EACH intid:                                                                       */
/*       PUT UNFORMATTED intid.TIN SKIP.                                                    */
/*    END.                                                                                  */
/*    OUTPUT CLOSE.   
                                                                      */
{AMERICANEUROPEAN.I}
   wtidvar = filnamn.
   INPUT FROM VALUE(wtidvar) CONVERT TARGET "iso8859-1" SOURCE "iso8859-1" NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidin.
         ASSIGN.
         IMPORT DELIMITER ";" tidin   NO-ERROR.
      END.               
   END.  
   RUN skapaenr_UI.           
/*    OS-DELETE VALUE(wtidvar). */

{EUROPEANAMERICAN.I}
PROCEDURE skapaenr_UI:
   OPEN QUERY MQ FOR EACH MTRL WHERE MTRL.KALKNR = 0 AND MTRL.LEVKOD = "3"
   NO-LOCK.
   DO TRANSACTION:
      GET FIRST MQ EXCLUSIVE-LOCK.
      FIND FIRST tidin WHERE tidin.GRUPP = MTRL.ENHET NO-LOCK NO-ERROR.
      IF AVAILABLE tidin THEN DO:
         MTRL.NPRIS = MTRL.BPRIS * ((100 - INTEGER(tidin.RABATT)) / 100).
      END.      
   END.
   REPEAT:
      DO TRANSACTION:
         GET NEXT MQ EXCLUSIVE-LOCK.
         IF AVAILABLE MTRL THEN DO:         
            FIND FIRST tidin WHERE tidin.GRUPP = MTRL.ENHET NO-LOCK NO-ERROR.
            IF AVAILABLE tidin THEN DO:
               MTRL.NPRIS = MTRL.BPRIS * ((100 - INTEGER(tidin.RABATT)) / 100).
            END.      

         END.
         ELSE LEAVE.
      END.
   END.   
END PROCEDURE.   

                
