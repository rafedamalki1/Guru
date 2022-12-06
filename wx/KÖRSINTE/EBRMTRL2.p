/*EBRMTRL2.P INLÄSNING AV KALMAR DEPÅ*/       

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
DEFINE VARIABLE FILNAMN AS CHARACTER.

DEFINE BUFFER mtrlbuff FOR MTRLDEP.

DEFINE TEMP-TABLE tidin
   FIELD ENR                AS CHARACTER
   FIELD ENR2               AS CHARACTER   
   FIELD BENAMNING          AS CHARACTER   
   FIELD ENHET              AS CHARACTER     
   FIELD VAL                AS INTEGER
   INDEX ENR IS PRIMARY ENR.

   

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
   {AMERICANEUROPEAN.I}
{muswait.i}      
   FILNAMN = "f:\LÅGSPÄNNING.SKV".
   EMPTY TEMP-TABLE intid NO-ERROR. 
   EMPTY TEMP-TABLE tidin NO-ERROR. 
   
/*    RUN PROVAG.P.                                                                         */
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
   FOR EACH tidin WHERE tidin.ENR = "":
      DELETE tidin.
   END.       
   RUN skapaenr_UI.           
/*    OS-DELETE VALUE(wtidvar). */

{EUROPEANAMERICAN.I}
PROCEDURE skapaenr_UI:   
   OPEN QUERY KQ FOR EACH KONSTRUKTION WHERE KONSTRUKTION.KONSKOD = 26
   NO-LOCK.
   GET FIRST KQ NO-LOCK.
   DO WHILE AVAILABLE(KONSTRUKTION):
      OPEN QUERY MQ FOR EACH MTRLBER WHERE MTRLBER.KTYPKOD = KONSTRUKTION.KTYPKOD
      NO-LOCK.
      DO TRANSACTION:
         GET FIRST MQ EXCLUSIVE-LOCK.
         IF AVAILABLE MTRLBER THEN DO:
            FIND FIRST tidin WHERE tidin.ENR = MTRLBER.ENR NO-LOCK NO-ERROR.
            IF tidin.VAL = 0 THEN DO:
               FIND FIRST MTRL WHERE MTRL.KALKNR = 0 AND MTRL.LEVKOD = "100" AND
               MTRL.ENR = tidin.ENR2 NO-LOCK NO-ERROR.
               IF AVAILABLE MTRL THEN DO:               
                  ASSIGN
                  MTRLBER.ENR = tidin.ENR2
                  MTRLBER.BENAMNING = MTRL.BENAMNING
                  MTRLBER.ENHET = tidin.ENHET.
               END.
               ELSE DO:
                  ASSIGN
                  MTRLBER.ENR = tidin.ENR2
                  MTRLBER.BENAMNING = tidin.BENAMNING
                  MTRLBER.ENHET = tidin.ENHET.
               END.
            END.
            ELSE DO:
               ASSIGN
               MTRLBER.ENR = tidin.ENR2
               MTRLBER.BENAMNING = tidin.BENAMNING
               MTRLBER.ENHET = tidin.ENHET.
            END.
         END.
      END.
      REPEAT:
         DO TRANSACTION:         
            GET NEXT MQ EXCLUSIVE-LOCK.
            IF AVAILABLE MTRLBER THEN DO:
               FIND FIRST tidin WHERE tidin.ENR = MTRLBER.ENR NO-LOCK NO-ERROR.
               IF tidin.VAL = 0 THEN DO:
                  FIND FIRST MTRL WHERE MTRL.KALKNR = 0 AND MTRL.LEVKOD = "100" AND
                  MTRL.ENR = tidin.ENR2 NO-LOCK NO-ERROR.
                  IF AVAILABLE MTRL THEN DO:               
                     ASSIGN
                     MTRLBER.ENR = tidin.ENR2
                     MTRLBER.BENAMNING = MTRL.BENAMNING
                     MTRLBER.ENHET = tidin.ENHET.
                  END.
                  ELSE DO:
                     ASSIGN
                     MTRLBER.ENR = tidin.ENR2
                     MTRLBER.BENAMNING = tidin.BENAMNING
                     MTRLBER.ENHET = tidin.ENHET.
                  END.
               END.
               ELSE DO:
                  ASSIGN
                  MTRLBER.ENR = tidin.ENR2
                  MTRLBER.BENAMNING = tidin.BENAMNING
                  MTRLBER.ENHET = tidin.ENHET.
               END.
            END.
            ELSE LEAVE.
         END.
      END.
      GET NEXT KQ NO-LOCK.
   END.    
   FOR EACH tidin:
      DO TRANSACTION:      
         FIND FIRST MTRL WHERE MTRL.KALKNR = 0 AND MTRL.LEVKOD = "100" AND
         MTRL.ENR = tidin.ENR2 EXCLUSIVE-LOCK NO-ERROR.      
         IF NOT AVAILABLE MTRL THEN DO:
            CREATE MTRL.
            ASSIGN
            MTRL.ENR = tidin.ENR2
            MTRL.BENAMNING = tidin.BENAMNING
            MTRL.ENHET = tidin.ENHET
            MTRL.LEVKOD = "100"
            MTRL.KALKNR = 0.
         END.
         ELSE DO:
            MTRL.ENHET = tidin.ENHET.
         END.      
         {MTRLCREATE.I}    
         FIND FIRST BYTENR WHERE BYTENR.ENR = tidin.ENR2 AND
         BYTENR.ENR2 = tidin.ENR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE BYTENR THEN DO:
            CREATE BYTENR.
            ASSIGN
            BYTENR.ENR = tidin.ENR2
            BYTENR.BENAMNING = tidin.BENAMNING
            BYTENR.LEVKOD = "100"
            BYTENR.ENR2 = tidin.ENR
            BYTENR.LEVKOD2 = "1".
         END.
      END.
   END.
END PROCEDURE.   

                
