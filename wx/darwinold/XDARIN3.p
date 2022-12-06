/*XDARIN3.p INLÄSNING AV MODERDARS gruddata kunder*/       
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
DEFINE VARIABLE nrvar AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE tidin
   FIELD B1                 AS INTEGER /*företag*/
   FIELD B2                 AS CHARACTER /*distrikt*/
   FIELD B3                 AS INTEGER /*årtal*/
   FIELD B4                 AS CHARACTER /*spänningsnivå*/
/*    FIELD B5                 AS CHARACTER */
/*    FIELD B6                 AS CHARACTER */
   FIELD B7                 AS INTEGER /*luftledning*/
   FIELD B8                 AS INTEGER /*kabel*/
   FIELD B9                 AS INTEGER /*bland*/
   FIELD B10                AS INTEGER. /*totalt*/
/*    FIELD B11                AS LOGICAL. */
   
DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
   
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
   
DEFINE TEMP-TABLE temp_text
   FIELD B1 AS INTEGER /*företag*/
   FIELD B2 AS CHARACTER /*distrikt*/
   FIELD PROGNAMN AS CHARACTER FORMAT "X(100)".   

DEFINE BUFFER distbuff FOR STORDISTRIKT.   

DEFINE INPUT PARAMETER filnamn AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR temp_text.
{AMERICANEUROPEAN.I}
{muswait.i}         
RUN in_UI.
{musarrow.i}
{EUROPEANAMERICAN.I}
PROCEDURE in_UI: 
   FOR EACH intid:
      DELETE intid.
   END.
   FOR EACH tidin:
      DELETE tidin.
   END.    
   FOR EACH temp_text:
      DELETE temp_text.
   END.
   RUN PROVAG.P.
   ASSIGN
   dlcvar = dlcvar + "QUOTER.EXE"
   wtidvar = wtidvar + "kundata.q".   
   
   OS-COMMAND SILENT VALUE(dlcvar)
   VALUE(filnamn) > VALUE(wtidvar).   
   INPUT FROM VALUE(wtidvar) NO-ECHO.    
   /*CONVERT TARGET "iso8859-1" SOURCE "ibm850" NO-ECHO.
   iso8859-1 swedish-7-bit ibm850"*/
   REPEAT:
      DO TRANSACTION: 
         SET words VIEW-AS EDITOR INNER-CHARS 78 INNER-LINES 80 WITH FRAME DDD WIDTH 80.   
         CREATE intid.   
         ASSIGN intid.TIN = words.   
      END.
   END.
   INPUT CLOSE.               
   OUTPUT TO VALUE(wtidvar).
   FOR EACH intid:          
      PUT UNFORMATTED intid.TIN SKIP.     
   END.
   OUTPUT CLOSE.
   INPUT FROM VALUE(wtidvar) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidin.
         ASSIGN.
         IMPORT DELIMITER ";" tidin   NO-ERROR.
      END.               
   END.
   RUN skapasats_UI.           
   OS-DELETE VALUE(wtidvar).
END PROCEDURE.

PROCEDURE skapasats_UI:
   FIND FIRST tidin NO-LOCK NO-ERROR.
   FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = tidin.B1 NO-LOCK NO-ERROR.
   IF AVAILABLE AVDELNING THEN DO:
      FOR EACH tidin NO-LOCK:
         FIND FIRST STORDISTRIKT WHERE STORDISTRIKT.AVDELNINGNR = tidin.B1 AND
         STORDISTRIKT.VIDISTRIKT = tidin.B2 AND STORDISTRIKT.ARTAL = tidin.B3
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE STORDISTRIKT THEN DO:
            FIND FIRST temp_text WHERE temp_text.B2 = tidin.B2
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE temp_text THEN DO:
               CREATE temp_text.
               ASSIGN
               temp_text.B1 = tidin.B1
               temp_text.B2 = tidin.B2
               temp_text.PROGNAMN = "DISTRIKT SAKNAS".
            END.
         END.
      END.
      FIND FIRST temp_text NO-LOCK NO-ERROR.
      IF NOT AVAILABLE temp_text THEN DO:
         FOR EACH tidin NO-LOCK:
            RUN skapakund_UI.                                                                 
         END.
      END.
   END.
   ELSE DO:
      CREATE temp_text.
      ASSIGN
      temp_text.B1 = tidin.B1
      temp_text.B2 = tidin.B2
      temp_text.PROGNAMN = "FÖRETAG SAKNAS". 
   END.


   /*             FIND FIRST STORDISTRIKT WHERE STORDISTRIKT.AVDELNINGNR = tidin.B1 AND    */
/*             STORDISTRIKT.VIDISTRIKT = tidin.B2 AND STORDISTRIKT.ARTAL = tidin.B3     */
/*             NO-LOCK NO-ERROR.                                                        */
/*             IF AVAILABLE STORDISTRIKT THEN DO:                                       */
/*                RUN skapakund_UI.                                                     */
/*             END.                                                                     */
/*             ELSE DO:                                                                 */
/*                FIND LAST STORDISTRIKT USE-INDEX DISTRIKTID NO-LOCK NO-ERROR.         */
/*                nrvar = STORDISTRIKT.DISTRIKTID + 1.                                  */
/*                FIND FIRST STORDISTRIKT WHERE STORDISTRIKT.AVDELNINGNR = tidin.B1 AND */
/*                STORDISTRIKT.VIDISTRIKT = tidin.B2 AND STORDISTRIKT.ARTAL = 1999      */
/*                NO-LOCK NO-ERROR.                                                     */
/*                IF AVAILABLE STORDISTRIKT THEN DO:                                    */
/*                   CREATE distbuff.                                                   */
/*                   ASSIGN                                                             */
/*                   distbuff.AVDELNINGNR = STORDISTRIKT.AVDELNINGNR                    */
/*                   distbuff.DISTRIKTID = nrvar                                        */
/*                   distbuff.VIDISTRIKT = tidin.B2                                     */
/*                   distbuff.NAMN = STORDISTRIKT.NAMN                                  */
/*                   distbuff.ARTAL = tidin.B3.                                         */
/*                   FIND STORDISTRIKT WHERE STORDISTRIKT.DISTRIKTID = nrvar            */
/*                   NO-LOCK NO-ERROR.                                                  */
/*                   RUN skapakund_UI.                                                  */
/*                END.                                                                  */
/*                ELSE DO:                                                              */
/*                   CREATE STORDISTRIKT.                                               */
/*                   ASSIGN                                                             */
/*                   STORDISTRIKT.AVDELNINGNR = tidin.B1                                */
/*                   STORDISTRIKT.DISTRIKTID = nrvar                                    */
/*                   STORDISTRIKT.VIDISTRIKT = tidin.B2                                 */
/*                   STORDISTRIKT.NAMN = tidin.B2                                       */
/*                   STORDISTRIKT.ARTAL = tidin.B3.                                     */
/*                   RUN skapakund_UI.                                                  */
/*                END.                                                                  */
/*             END.                                                                     */
END PROCEDURE.

PROCEDURE skapakund_UI:   
   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "D" AND INLASTAB.INKODTYP = "1" AND
   INLASTAB.INKODPOSCH = SUBSTRING(tidin.B4,3,2) USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   FIND FIRST SPANNINGSNIV WHERE SPANNINGSNIV.INKODID = INLASTAB.INKODID
   NO-LOCK NO-ERROR.
   FIND FIRST KUNDSTOR WHERE KUNDSTOR.DISTRIKTID = STORDISTRIKT.DISTRIKTID AND
   KUNDSTOR.SPANID = SPANNINGSNIV.SPANID AND KUNDSTOR.ARTAL = STORDISTRIKT.ARTAL
   NO-LOCK NO-ERROR.
   IF AVAILABLE KUNDSTOR THEN DO:      
      ASSIGN
      KUNDSTOR.ANTALLUFT = tidin.B7
      KUNDSTOR.ANTALBLAND = tidin.B8
      KUNDSTOR.ANTALKABEL = tidin.B9
      KUNDSTOR.ANTALKUNDER = tidin.B10.
   END.
   ELSE DO:
      IF tidin.B7 > 0 OR tidin.B8 > 0 OR tidin.B9 > 0 OR tidin.B10 > 0 THEN DO:      
         CREATE KUNDSTOR.
         ASSIGN
         KUNDSTOR.DISTRIKTID = STORDISTRIKT.DISTRIKTID
         KUNDSTOR.ARTAL = STORDISTRIKT.ARTAL
         KUNDSTOR.ANTALLUFT = tidin.B7
         KUNDSTOR.ANTALBLAND = tidin.B8
         KUNDSTOR.ANTALKABEL = tidin.B9
         KUNDSTOR.ANTALKUNDER = tidin.B10
         KUNDSTOR.SPANID = SPANNINGSNIV.SPANID.
      END.
   END.   
END PROCEDURE.   

                
