/*VESABP3.P INLÄSNING AV EBRMATERIEL*/       
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
   FIELD ARBKOD                AS CHARACTER
   FIELD LOPNR                 AS INTEGER
   FIELD BENAMNING             AS CHARACTER
   FIELD ENHET                 AS CHARACTER
   FIELD TIM                   AS DECIMAL
   FIELD JOBB                  AS CHARACTER.
   

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
   
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.   
{muswait.i}   
{AMERICANEUROPEAN.I}   
   filnamn = "F:\elpool\elpnj\VESAB\KALKYLERP3.SKV".
   EMPTY TEMP-TABLE intid NO-ERROR. 
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
/*    OUTPUT CLOSE.                                                                         */
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

{EUROPEANAMERICAN.I}
PROCEDURE skapaenr_UI:
   FOR EACH tidin:
      DO TRANSACTION:
         IF tidin.LOPNR = 0 THEN DO:
            CREATE P3.
            ASSIGN
            P3.ARBKOD = tidin.ARBKOD
            P3.BENAMNING = tidin.BENAMNING
            P3.KATAR = 2003.
         END.
         ELSE DO:
            CREATE LOP3.
            ASSIGN         
            LOP3.ARBKOD = tidin.ARBKOD
            LOP3.LOPNR = tidin.LOPNR
            LOP3.BENAMNING = tidin.BENAMNING
            LOP3.ENHET = tidin.ENHET
            LOP3.F2 = tidin.TIM / 100
            LOP3.KATAR = 2003
            LOP3.FAST = FALSE.
            ASSIGN
            LOP3.BENAMNING = SUBSTRING(LOP3.BENAMNING,1,55).
            REPEAT WHILE LENGTH(LOP3.BENAMNING) < 59:
                LOP3.BENAMNING = LOP3.BENAMNING + " ".
            END.
            LOP3.BENAMNING = LOP3.BENAMNING + tidin.JOBB.
         END.
      END.
   END.

END PROCEDURE.   

                
