/*XXDARIN5.P*/
/*Import av störningar*/       
DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.
DEFINE VARIABLE dlcvar AS CHARACTER NO-UNDO.



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
DEFINE VARIABLE nat1 AS INTEGER NO-UNDO.
DEFINE VARIABLE nat2 AS INTEGER NO-UNDO.
DEFINE VARIABLE nat3 AS INTEGER NO-UNDO.
DEFINE VARIABLE nat4 AS INTEGER NO-UNDO.
DEFINE VARIABLE felvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE stornr AS INTEGER NO-UNDO.
DEFINE VARIABLE kolldecimal AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE tidin   
   FIELD FORETAG            AS INTEGER 
   FIELD DISTRIKT           AS CHARACTER
   FIELD BDAT               AS DATE FORMAT "9999/99/99"
   FIELD BTID               AS INTEGER FORMAT "9999"
   FIELD C1                 AS INTEGER   
   FIELD D1                 AS CHARACTER
   FIELD D2                 AS CHARACTER 
   FIELD E1                 AS INTEGER
   FIELD E2                 AS CHARACTER
   FIELD F1                 AS INTEGER
   FIELD F2                 AS CHARACTER
   FIELD G1                 AS INTEGER
   FIELD G2                 AS INTEGER
   FIELD G3                 AS INTEGER
   FIELD H                  AS INTEGER
   FIELD J1                 AS INTEGER
   FIELD J1KLOCK            AS INTEGER FORMAT "9999"
   FIELD J2                 AS INTEGER
   FIELD J2KLOCK            AS INTEGER FORMAT "9999"
   FIELD J3                 AS CHARACTER
   FIELD K1                 AS INTEGER
   FIELD K2                 AS INTEGER
   FIELD L1                 AS INTEGER
   FIELD L2                 AS INTEGER
   FIELD M1                 AS INTEGER
   FIELD M2                 AS INTEGER
   FIELD N1                 AS INTEGER
   FIELD N2                 AS CHARACTER
   FIELD P1                 AS INTEGER
   FIELD P2                 AS INTEGER
   FIELD Q                  AS INTEGER
   FIELD R                  AS INTEGER
   FIELD S                  AS INTEGER
   FIELD T1                 AS INTEGER
   FIELD T2                 AS INTEGER
   FIELD T3                 AS INTEGER   
   FIELD FEL                AS CHARACTER
   FIELD FEL2               AS LOGICAL INITIAL FALSE
   FIELD OKVAR              AS LOGICAL INITIAL FALSE
   INDEX ORD FORETAG DISTRIKT BDAT BTID FEL FEL2 ASCENDING.
   
DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
   
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
   
DEFINE TEMP-TABLE temp_text
   FIELD B1 AS INTEGER /*företag*/
   FIELD B2 AS CHARACTER /*distrikt*/
   FIELD PROGNAMN AS CHARACTER FORMAT "X(100)". 

DEFINE TEMP-TABLE finns_temp
   FIELD AVDELNINGNR AS INTEGER /*företag*/
   FIELD DISTRIKT AS CHARACTER /*distrikt*/
   FIELD ARTAL AS INTEGER. 

DEFINE BUFFER distbuff FOR STORDISTRIKT.  

DEFINE INPUT PARAMETER filnamn AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR temp_text.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tidin.
DEFINE INPUT PARAMETER vart AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR finns_temp.
{AMERICANEUROPEAN.I}
{muswait.i}         
IF vart = 1 THEN RUN in_UI.

ELSE IF vart = 2 THEN RUN skapa_UI.
{EUROPEANAMERICAN.I}
{musarrow.i}

PROCEDURE in_UI: 
   FOR EACH intid:
      DELETE intid.
   END.
   FOR EACH tidin:
      DELETE tidin.
   END. 
   FOR EACH finns_temp:
      DELETE finns_temp.
   END.   

/*    ASSIGN                                                                                */
/*    dlcvar = dlcvar + "QUOTER.EXE"                                                        */
/*    wtidvar = wtidvar + "storning.q".                                                     */
/*                                                                                          */
/*    OS-COMMAND SILENT VALUE(dlcvar)                                                       */
/*    VALUE(filnamn) > VALUE(wtidvar).                                                      */
/*    INPUT FROM VALUE(wtidvar) NO-ECHO.                                                    */
/*    /*CONVERT TARGET "iso8859-1" SOURCE "ibm850" NO-ECHO.                                 */
/*    iso8859-1 swedish-7-bit ibm850"*/                                                     */
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
   INPUT FROM VALUE(filnamn) NO-ECHO.
   REPEAT:
      DO TRANSACTION:         
         CREATE tidin.
         ASSIGN.
         IMPORT DELIMITER ";" tidin   NO-ERROR.         
      END.               
   END.   
/*    FOR EACH tidin:   */
/*       DISPLAY tidin. */
/*    END.              */
   FOR EACH tidin WHERE tidin.FORETAG = 0:
      DELETE tidin.
   END.
/*    FOR EACH tidin:   */
/*       DISPLAY tidin. */
/*    END.              */
   RUN skapasats_UI.           
/*    OS-DELETE VALUE(wtidvar). */
END PROCEDURE.

PROCEDURE skapasats_UI:
   FIND FIRST tidin NO-LOCK NO-ERROR.
   FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = tidin.FORETAG NO-LOCK NO-ERROR.
   IF AVAILABLE AVDELNING THEN DO:
      FOR EACH tidin NO-LOCK:
         IF tidin.DISTRIKT = "1" THEN tidin.DISTRIKT = "001".
         ELSE IF tidin.DISTRIKT = "2" THEN tidin.DISTRIKT = "002".
         ELSE IF tidin.DISTRIKT = "3" THEN tidin.DISTRIKT = "003".
         ELSE IF tidin.DISTRIKT = "0" THEN tidin.DISTRIKT = "000".
         ELSE  tidin.DISTRIKT = tidin.DISTRIKT.
         FIND FIRST STORDISTRIKT WHERE STORDISTRIKT.AVDELNINGNR = tidin.FORETAG AND
         STORDISTRIKT.VIDISTRIKT = tidin.DISTRIKT AND STORDISTRIKT.ARTAL = YEAR(tidin.BDAT)
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE STORDISTRIKT THEN DO:           
            FIND LAST STORDISTRIKT USE-INDEX DISTRIKTID NO-LOCK NO-ERROR.                   
            nrvar = STORDISTRIKT.DISTRIKTID + 1.
            DO TRANSACTION:            
               CREATE STORDISTRIKT.                                               
               ASSIGN
               STORDISTRIKT.AVDELNINGNR = tidin.FORETAG
               STORDISTRIKT.DISTRIKTID = nrvar
               STORDISTRIKT.VIDISTRIKT = tidin.DISTRIKT            
               STORDISTRIKT.ARTAL = YEAR(tidin.BDAT).        
            END. 
         END.  
         ELSE DO:
            FIND FIRST STORNINGSTAB WHERE STORNINGSTAB.DISTRIKTID = STORDISTRIKT.DISTRIKTID
            NO-LOCK NO-ERROR.
            IF AVAILABLE STORNINGSTAB THEN DO:
               FIND FIRST finns_temp WHERE finns_temp.AVDELNINGNR = tidin.FORETAG AND
               finns_temp.DISTRIKT = tidin.DISTRIKT AND finns_temp.ARTAL = YEAR(tidin.BDAT)
               NO-LOCK NO-ERROR.
               IF NOT AVAILABLE finns_temp THEN DO:               
                  CREATE finns_temp.
                  ASSIGN
                  finns_temp.AVDELNINGNR = tidin.FORETAG
                  finns_temp.DISTRIKT = tidin.DISTRIKT
                  finns_temp.ARTAL = YEAR(tidin.BDAT).
               END.
            END.
         END.
      END.      
      FOR EACH tidin NO-LOCK:     
         RUN skapakund_UI.       
      END.      
   END.
   ELSE DO:
      CREATE temp_text.
      ASSIGN
      temp_text.B1 = tidin.FORETAG
      temp_text.B2 = tidin.DISTRIKT
      temp_text.PROGNAMN = "FÖRETAG SAKNAS". 
   END.   
END PROCEDURE.

PROCEDURE skapa_UI:    
   FIND LAST STORNINGSTAB WHERE 
   USE-INDEX STORNUMMERID NO-LOCK NO-ERROR.
   IF AVAILABLE STORNINGSTAB THEN DO: 
      stornr = STORNINGSTAB.STORNUMMERID.
   END.
   ELSE DO:
      stornr = 1.
   END.
   FOR EACH tidin WHERE tidin.FEL = "" AND tidin.FEL2 = FALSE AND tidin.OKVAR = FALSE:
      RUN skapa2_UI.
   END.
END PROCEDURE.

PROCEDURE skapakund_UI: 
   ASSIGN
   felvar = FALSE
   tidin.FEL2 = FALSE
   tidin.FEL = " ".
   
   IF tidin.C1 = 1 OR tidin.C1 = 2 THEN felvar = felvar.
   ELSE DO:
      IF tidin.C1 = 3 THEN tidin.C1 = 1.
      ELSE DO:      
         tidin.FEL = "C1 ".
         felvar = TRUE.
      END.
   END.   
      
   IF tidin.D1 NE "" THEN DO:  
      FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "D" AND INLASTAB.INKODTYP = "1" AND
      INLASTAB.INKODPOSCH = tidin.D1 USE-INDEX INKOD
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE INLASTAB THEN DO:
         tidin.FEL = tidin.FEL + "D1 ".
         felvar = TRUE.   
      END.
   END.
   ELSE DO:
      IF tidin.D2 NE "" THEN DO:
         FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "D" AND INLASTAB.INKODTYP = "1" AND
         INLASTAB.INKODPOSCH = tidin.D2 USE-INDEX INKOD
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE INLASTAB THEN DO:
            tidin.FEL = tidin.FEL + "D1 ".
            felvar = TRUE.   
         END.
      END.
      ELSE DO:
         tidin.FEL = tidin.FEL + "D1 ".
         felvar = TRUE.   
      END.   
   END.   
   
   IF tidin.D2 NE "" THEN DO:     
      FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "D" AND INLASTAB.INKODTYP = "2" AND
      INLASTAB.INKODPOSCH = tidin.D2 USE-INDEX INKOD
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE INLASTAB THEN DO:
         tidin.FEL = tidin.FEL + "D2 ".
         felvar = TRUE.   
      END.
   END.
   ELSE DO:
      IF tidin.C1 = 1 THEN DO:
         IF tidin.D1 NE "" THEN DO:
            FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "D" AND INLASTAB.INKODTYP = "2" AND
            INLASTAB.INKODPOSCH = tidin.D1 USE-INDEX INKOD
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE INLASTAB THEN DO:
               tidin.FEL = tidin.FEL + "D2 ".
               felvar = TRUE.   
            END.
         END.
         ELSE DO:
            tidin.FEL = tidin.FEL + "D2 ".
            felvar = TRUE.
         END.   
      END.
   END.  
   
   IF tidin.BDAT = ? THEN DO:
      tidin.FEL = tidin.FEL + "BDAT ".
      felvar = TRUE.
   END.   
   
   IF tidin.BTID = ? THEN DO:
      tidin.FEL = tidin.FEL + "BTID ".
      felvar = TRUE.
   END.   
   IF SUBSTRING(STRING(tidin.BDAT,"9999/99/99"),6,2) >= "01" AND
   SUBSTRING(STRING(tidin.BDAT,"9999/99/99"),6,2) <= "12" THEN DO:
      IF SUBSTRING(STRING(tidin.BDAT,"9999/99/99"),9,2) >= "01" AND 
      SUBSTRING(STRING(tidin.BDAT,"9999/99/99"),9,2) <= "31" THEN DO:
         felvar = felvar.
      END.
      ELSE DO:
         ASSIGN
         tidin.FEL2 = TRUE
         tidin.FEL = tidin.FEL + "BDAT "
         felvar = TRUE.
      END.
   END.
   ELSE DO:
      ASSIGN
      tidin.FEL2 = TRUE
      tidin.FEL = tidin.FEL + "BDAT "
      felvar = TRUE.
   END.
   IF SUBSTRING(STRING(tidin.BTID,"9999"),1,2) > "24" THEN DO:
      ASSIGN
      tidin.FEL2 = TRUE
      tidin.FEL = tidin.FEL + "BTID "
      felvar = TRUE.
   END.   
   
   IF SUBSTRING(STRING(tidin.BTID,"9999"),3,2) > "59" THEN DO:
      ASSIGN
      tidin.FEL2 = TRUE
      tidin.FEL = tidin.FEL + "BTID "
      felvar = TRUE.
   END.   
   

   IF tidin.E1 >= 0 AND tidin.E1 <= 5 THEN DO:      
      felvar = felvar.
   END.
   ELSE DO:
      tidin.FEL = tidin.FEL + "E1 ".
      felvar = TRUE.
   END.
   
   IF tidin.C1 = 1 THEN DO:
      IF tidin.F1 >= 0 AND tidin.F1 <= 4 THEN DO:
         felvar = felvar.
      END.
      ELSE DO:
         tidin.F1 = 0.
/*          tidin.FEL = tidin.FEL + "F1 ". */
/*          felvar = TRUE.                 */
      END.
   END.
   ELSE DO:
      tidin.F1 = 0.
   END.      
   
   IF tidin.C1 = 1 THEN DO:
      IF tidin.G1 >= 0 AND tidin.G1 <= 2 THEN DO:           
         felvar = felvar.
      END.
      ELSE DO:
         tidin.G1 = 0.
/*          tidin.FEL = tidin.FEL + "G1 ". */
/*          felvar = TRUE.                 */
      END.
   END.
   ELSE DO:
      tidin.G1 = 0.
   END.   
   
   IF tidin.C1 = 1 THEN DO:
      IF tidin.G2 >= 0 AND tidin.G2 <= 6 THEN DO:
         felvar = felvar.
      END.
      ELSE DO:
         tidin.G2 = 0.
/*          tidin.FEL = tidin.FEL + "G2 ". */
/*          felvar = TRUE.                 */
      END.
   END.
   ELSE DO:
      tidin.G2 = 0.
   END.      
   IF tidin.J1 NE 0 THEN DO:      
      IF SUBSTRING(STRING(tidin.J1,"999999"),1,2) >= "01" AND
      SUBSTRING(STRING(tidin.J1,"999999"),1,2) <= "31" THEN DO:
         IF SUBSTRING(STRING(tidin.J1,"999999"),3,2) > "24" THEN DO:
            ASSIGN
            tidin.FEL2 = TRUE
            tidin.FEL = tidin.FEL + "J1 "
            felvar = TRUE.
         END.
         ELSE DO:
            IF SUBSTRING(STRING(tidin.J1,"999999"),5,2) > "59" THEN DO:
               ASSIGN
               tidin.FEL2 = TRUE
               tidin.FEL = tidin.FEL + "J1 "
               felvar = TRUE.
            END.
            ELSE DO:               
               IF INTEGER(SUBSTRING(STRING(tidin.J1,"999999"),1,2)) = INTEGER(SUBSTRING(STRING(tidin.BDAT,"999999"),5,2)) THEN DO:                  
                  IF SUBSTRING(STRING(tidin.J1,"999999"),3,4) < SUBSTRING(STRING(tidin.BTID,"9999"),1,4) THEN DO:
                     ASSIGN
                     tidin.FEL2 = TRUE
                     tidin.FEL = tidin.FEL + "J1 "
                     felvar = TRUE.
                  END.
               END.
               ELSE IF INTEGER(SUBSTRING(STRING(tidin.J1,"999999"),1,2)) < INTEGER(SUBSTRING(STRING(tidin.BDAT,"999999"),5,2)) THEN DO:
/*                    IF integer(SUBSTRING(STRING(tidin.J1,"999999"),1,2)) > 5 THEN DO: */
/*                       ASSIGN                                                         */
/*                       tidin.FEL2 = TRUE                                              */
/*                       tidin.FEL = tidin.FEL + "J1 "                                  */
/*                       felvar = TRUE.                                                 */
/*                    END.                                                              */
               END.
               ELSE DO:
                  felvar = felvar.
               END.
            END.
         END.
      END.
      ELSE DO:
         ASSIGN
         tidin.FEL2 = TRUE
         tidin.FEL = tidin.FEL + "J1 "
         felvar = TRUE.
      END.
   END.   
   
   IF tidin.J2 NE 0 THEN DO:  
       IF SUBSTRING(STRING(tidin.J2,"999999"),1,2) >= "01" AND
       SUBSTRING(STRING(tidin.J2,"999999"),1,2) <= "31" THEN DO:
          IF SUBSTRING(STRING(tidin.J2,"999999"),3,2) > "24" THEN DO:
             ASSIGN
             tidin.FEL2 = TRUE
             tidin.FEL = tidin.FEL + "J2 "
             felvar = TRUE.
          END.
          ELSE DO:
             IF SUBSTRING(STRING(tidin.J2,"999999"),5,2) > "59" THEN DO:
                ASSIGN
                tidin.FEL2 = TRUE
                tidin.FEL = tidin.FEL + "J2 "
                felvar = TRUE.
             END.
             ELSE DO:
                IF INTEGER(SUBSTRING(STRING(tidin.J2,"999999"),1,2)) = INTEGER(SUBSTRING(STRING(tidin.BDAT,"999999"),5,2)) THEN DO:
                   IF SUBSTRING(STRING(tidin.J2,"999999"),3,4) < SUBSTRING(STRING(tidin.BTID,"9999"),1,4) THEN DO:
                      ASSIGN
                      tidin.FEL2 = TRUE
                      tidin.FEL = tidin.FEL + "J2 "
                      felvar = TRUE.
                   END.
                END.
                ELSE IF INTEGER(SUBSTRING(STRING(tidin.J2,"999999"),1,2)) < INTEGER(SUBSTRING(STRING(tidin.BDAT,"999999"),5,2)) THEN DO:
/*                     IF integer(SUBSTRING(STRING(tidin.J2,"999999"),1,2)) > 5 THEN DO: */
/*                       ASSIGN                                                          */
/*                       tidin.FEL2 = TRUE                                               */
/*                       tidin.FEL = tidin.FEL + "J2 "                                   */
/*                       felvar = TRUE.                                                  */
/*                    END.                                                               */
/*                    ELSE IF MONTH(tidin.BDAT) = 12 THEN DO:                            */
/*                        ASSIGN                                                         */
/*                        tidin.FEL2 = TRUE                                              */
/*                        tidin.FEL = tidin.FEL + "J2månad "                             */
/*                        felvar = TRUE.                                                 */
/*                    END.                                                               */
                END.
                ELSE DO:
                   IF tidin.J2 >= tidin.J1 THEN DO:
                      felvar = felvar.
                   END.
                   ELSE DO:
                      ASSIGN
                      tidin.FEL2 = TRUE
                      tidin.FEL = tidin.FEL + "J2 "
                      felvar = TRUE.
                   END.   
                END.
             END.
          END.
       END.
       ELSE DO:
          ASSIGN
          tidin.FEL2 = TRUE
          tidin.FEL = tidin.FEL + "J2 "
          felvar = TRUE.
       END.
    END.
/*    IF tidin.J1DAT NE ? THEN DO:                                             */
/*       IF SUBSTRING(STRING(tidin.J1DAT,"9999/99/99"),6,2) >= "01" AND        */
/*       SUBSTRING(STRING(tidin.J1DAT,"9999/99/99"),6,2) <= "12" THEN DO:      */
/*          IF SUBSTRING(STRING(tidin.J1DAT,"9999/99/99"),9,2) >= "01" AND     */
/*          SUBSTRING(STRING(tidin.J1DAT,"9999/99/99"),9,2) <= "31" THEN DO:   */
/*             IF SUBSTRING(STRING(tidin.J1KLOCK,"9999"),1,2) >= "00" AND      */
/*             SUBSTRING(STRING(tidin.J1KLOCK,"9999"),1,2) <= "24" THEN DO:    */
/*                IF SUBSTRING(STRING(tidin.J1KLOCK,"9999"),3,2) >= "00" AND   */
/*                SUBSTRING(STRING(tidin.J1KLOCK,"9999"),3,2) <= "59" THEN DO: */
/*                   felvar = felvar.                                          */
/*                END.                                                         */
/*                ELSE DO:                                                     */
/*                   ASSIGN                                                    */
/*                   tidin.FEL2 = TRUE                                         */
/*                   tidin.FEL = tidin.FEL + "J1KLOCK "                        */
/*                   felvar = TRUE.                                            */
/*                END.                                                         */
/*             END.                                                            */
/*             ELSE DO:                                                        */
/*                ASSIGN                                                       */
/*                tidin.FEL2 = TRUE                                            */
/*                tidin.FEL = tidin.FEL + "J1KLOCK "                           */
/*                felvar = TRUE.                                               */
/*             END.                                                            */
/*          END.                                                               */
/*          ELSE DO:                                                           */
/*             ASSIGN                                                          */
/*             tidin.FEL2 = TRUE                                               */
/*             tidin.FEL = tidin.FEL + "J1DAT "                                */
/*             felvar = TRUE.                                                  */
/*          END.                                                               */
/*       END.                                                                  */
/*       ELSE DO:                                                              */
/*          ASSIGN                                                             */
/*          tidin.FEL2 = TRUE                                                  */
/*          tidin.FEL = tidin.FEL + "J1DAT "                                   */
/*          felvar = TRUE.                                                     */
/*       END.                                                                  */
/*    END.                                                                     */
/*                                                                             */
/*    IF tidin.J2DAT NE ? THEN DO:                                             */
/*       IF SUBSTRING(STRING(tidin.J2DAT,"9999/99/99"),6,2) >= "01" AND        */
/*       SUBSTRING(STRING(tidin.J2DAT,"9999/99/99"),6,2) <= "12" THEN DO:      */
/*          IF SUBSTRING(STRING(tidin.J2DAT,"9999/99/99"),9,2) >= "01" AND     */
/*          SUBSTRING(STRING(tidin.J2DAT,"9999/99/99"),9,2) <= "31" THEN DO:   */
/*             IF SUBSTRING(STRING(tidin.J2KLOCK,"9999"),1,2) >= "00" AND      */
/*             SUBSTRING(STRING(tidin.J2KLOCK,"9999"),1,2) <= "24" THEN DO:    */
/*                IF SUBSTRING(STRING(tidin.J2KLOCK,"9999"),3,2) >= "00" AND   */
/*                SUBSTRING(STRING(tidin.J2KLOCK,"9999"),3,2) <= "59" THEN DO: */
/*                   felvar = felvar.                                          */
/*                END.                                                         */
/*                ELSE DO:                                                     */
/*                   ASSIGN                                                    */
/*                   tidin.FEL2 = TRUE                                         */
/*                   tidin.FEL = tidin.FEL + "J2KLOCK "                        */
/*                   felvar = TRUE.                                            */
/*                END.                                                         */
/*             END.                                                            */
/*             ELSE DO:                                                        */
/*                ASSIGN                                                       */
/*                tidin.FEL2 = TRUE                                            */
/*                tidin.FEL = tidin.FEL + "J2KLOCK "                           */
/*                felvar = TRUE.                                               */
/*             END.                                                            */
/*          END.                                                               */
/*          ELSE DO:                                                           */
/*             ASSIGN                                                          */
/*             tidin.FEL2 = TRUE                                               */
/*             tidin.FEL = tidin.FEL + "J2DAT "                                */
/*             felvar = TRUE.                                                  */
/*          END.                                                               */
/*       END.                                                                  */
/*       ELSE DO:                                                              */
/*          ASSIGN                                                             */
/*          tidin.FEL2 = TRUE                                                  */
/*          tidin.FEL = tidin.FEL + "J2DAT "                                   */
/*          felvar = TRUE.                                                     */
/*       END.                                                                  */
/*    END.                                                                     */
/*                                                                             */
/*    IF tidin.J1DAT NE ? THEN DO:                                             */
/*       IF tidin.J1DAT >= tidin.BDAT THEN DO:                                 */
/*          IF tidin.J2DAT >= tidin.J1DAT THEN DO:                             */
/*             IF tidin.J1DAT = tidin.BDAT THEN DO:                            */
/*                IF tidin.J1KLOCK > tidin.BTID THEN DO:                       */
/*                   IF tidin.J2DAT = tidin.J1DAT THEN DO:                     */
/*                      IF tidin.J2KLOCK >= tidin.J1KLOCK THEN DO:             */
/*                         musz = musz.                                        */
/*                      END.                                                   */
/*                      ELSE DO:                                               */
/*                         ASSIGN                                              */
/*                         tidin.FEL2 = TRUE                                   */
/*                         tidin.FEL = tidin.FEL + "TIDFEL "                   */
/*                         felvar = TRUE.                                      */
/*                      END.                                                   */
/*                   END.                                                      */
/*                END.                                                         */
/*                ELSE DO:                                                     */
/*                   ASSIGN                                                    */
/*                   tidin.FEL2 = TRUE                                         */
/*                   tidin.FEL = tidin.FEL + "TIDFEL "                         */
/*                   felvar = TRUE.                                            */
/*                END.                                                         */
/*             END.                                                            */
/*             ELSE DO:                                                        */
/*                IF tidin.J2DAT = tidin.J1DAT THEN DO:                        */
/*                   IF tidin.J2KLOCK >= tidin.J1KLOCK THEN DO:                */
/*                      musz = musz.                                           */
/*                   END.                                                      */
/*                   ELSE DO:                                                  */
/*                      ASSIGN                                                 */
/*                      tidin.FEL2 = TRUE                                      */
/*                      tidin.FEL = tidin.FEL + "TIDFEL "                      */
/*                      felvar = TRUE.                                         */
/*                   END.                                                      */
/*                END.                                                         */
/*             END.                                                            */
/*          END.                                                               */
/*          ELSE DO:                                                           */
/*             ASSIGN                                                          */
/*             tidin.FEL2 = TRUE                                               */
/*             tidin.FEL = tidin.FEL + "TIDFEL "                               */
/*             felvar = TRUE.                                                  */
/*          END.                                                               */
/*       END.                                                                  */
/*       ELSE DO:                                                              */
/*          ASSIGN                                                             */
/*          tidin.FEL2 = TRUE                                                  */
/*          tidin.FEL = tidin.FEL + "TIDFEL "                                  */
/*          felvar = TRUE.                                                     */
/*       END.                                                                  */
/*    END.                                                                     */
/*    ELSE DO:                                                                 */
/*       IF tidin.J2DAT >= tidin.BDAT THEN DO:                                 */
/*          IF tidin.J2DAT = tidin.BDAT THEN DO:                               */
/*             IF tidin.J2KLOCK <= tidin.BTID THEN DO:                         */
/*                ASSIGN                                                       */
/*                tidin.FEL2 = TRUE                                            */
/*                tidin.FEL = tidin.FEL + "TIDFEL "                            */
/*                felvar = TRUE.                                               */
/*             END.                                                            */
/*          END.                                                               */
/*       END.                                                                  */
/*       ELSE DO:                                                              */
/*          ASSIGN                                                             */
/*          tidin.FEL2 = TRUE                                                  */
/*          tidin.FEL = tidin.FEL + "TIDFEL "                                  */
/*          felvar = TRUE.                                                     */
/*       END.                                                                  */
/*    END.                                                                     */
   IF tidin.N1 >= 0 AND tidin.N1 <= 5 THEN DO:      
      felvar = felvar.
   END.
   ELSE DO:
      tidin.N1 = 0.
      felvar = felvar.      
/*       tidin.FEL = tidin.FEL + "N1 ". */
/*       felvar = TRUE.                 */
   END.
   IF tidin.P1 >= 0 AND tidin.P1 <= 3 THEN DO:      
      felvar = felvar.
   END.
   ELSE DO:
      tidin.P1 = 0.
      felvar = felvar.      
/*       tidin.FEL = tidin.FEL + "P1 ". */
/*       felvar = TRUE.                 */
   END.
   IF tidin.P2 >= 1 AND tidin.P2 <= 3 THEN DO:      
      felvar = felvar.
   END.
   ELSE DO:
      tidin.P2 = 0.
      felvar = felvar.      
/*       tidin.FEL = tidin.FEL + "P2 ". */
/*       felvar = TRUE.                 */     
   END.
   IF tidin.Q >= 0 AND tidin.Q <= 5 THEN DO:      
      felvar = felvar.
   END.
   ELSE DO:
      tidin.Q = 0.
      felvar = felvar.      
/*          tidin.FEL = tidin.FEL + "Q ". */
/*          felvar = TRUE.                */     
   END.
/*    IF tidin.R NE 0 THEN DO: */
   /*MATS TAPPER HAR BESTÄMT ATT VI SKALL TA IN STÖRNINGAR SOM HAR OKÄND ANLAGGNINGSDEL*/
      FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "R" AND
      INLASTAB.INKODPOSCH = STRING(tidin.R) USE-INDEX INKOD
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE INLASTAB THEN DO:
         tidin.FEL = tidin.FEL + "R ".
         felvar = TRUE.   
      END.
/*    END.                             */
/*    ELSE DO:                         */
/*       tidin.FEL = tidin.FEL + "R ". */
/*       felvar = TRUE.                */
/*    END.                             */
   IF tidin.S NE 0 THEN DO:      
      IF tidin.C1 = 2 THEN DO:
         tidin.S = 0.
      END.
      ELSE DO:
         FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "S" AND
         INLASTAB.INKODPOSCH = STRING(tidin.S) USE-INDEX INKOD
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE INLASTAB THEN DO:
            tidin.S = 93.   
         END.
      END.   
   END.
   ELSE DO:
      IF tidin.C1 = 1 THEN DO:
         tidin.S = 93.
      END.   
   END.
   IF tidin.T1 >= 1 AND tidin.T1 <= 2 THEN DO:      
      felvar = felvar.
   END.
   ELSE DO:
      IF tidin.T1 = 0 THEN DO:      
         felvar = felvar.
      END.
      ELSE DO:
         tidin.FEL = tidin.FEL + "T1 ".
         felvar = TRUE.
      END.   
   END.   
/*    IF felvar = FALSE THEN DO: */
/*       RUN skapa_UI.           */
/*    END.                       */
END PROCEDURE.   

PROCEDURE skapa2_UI: 
   stornr = stornr + 1.
   FIND FIRST STORDISTRIKT WHERE STORDISTRIKT.AVDELNINGNR = tidin.FORETAG AND
   STORDISTRIKT.VIDISTRIKT = tidin.DISTRIKT AND STORDISTRIKT.ARTAL = YEAR(tidin.BDAT)
   NO-LOCK NO-ERROR.

   CREATE STORNINGSTAB.
   ASSIGN
   STORNINGSTAB.DISTRIKTID = STORDISTRIKT.DISTRIKTID
   STORNINGSTAB.STORNUMMERID = stornr
   STORNINGSTAB.VSTORNUMMER = stornr
   STORNINGSTAB.INDATUM = TODAY
   STORNINGSTAB.INKLOCKAN = TIME
   STORNINGSTAB.HDATUM = tidin.BDAT
   STORNINGSTAB.HKLOCKAN = tidin.BTID / 100
   STORNINGSTAB.STORTYPID = tidin.C1
   STORNINGSTAB.MERJOBB = FALSE.
   
   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "D" AND INLASTAB.INKODTYP = "1" AND
   INLASTAB.INKODPOSCH = tidin.D1 USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   FIND FIRST SPANNINGSNIV WHERE SPANNINGSNIV.INKODID = INLASTAB.INKODID
   NO-LOCK NO-ERROR.
   STORNINGSTAB.FRANSPANID = SPANNINGSNIV.SPANID.
   
   IF tidin.D2 NE " " THEN DO:
      FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "D" AND INLASTAB.INKODTYP = "2" AND
      INLASTAB.INKODPOSCH = tidin.D2 USE-INDEX INKOD
      NO-LOCK NO-ERROR.
      IF AVAILABLE INLASTAB THEN DO:
         FIND FIRST SPANNINGSNIV WHERE SPANNINGSNIV.INKODID = INLASTAB.INKODID
         NO-LOCK NO-ERROR.
         STORNINGSTAB.FELSPANID = SPANNINGSNIV.SPANID.
      END.   
   END.   
   
   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "E" AND INLASTAB.INKODTYP = "1" AND
   INLASTAB.INKODPOSCH = STRING(tidin.E1) USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   FIND FIRST BRYTORGAN WHERE BRYTORGAN.INKODID = INLASTAB.INKODID
   NO-LOCK NO-ERROR.
   STORNINGSTAB.BRYTOID = BRYTORGAN.BRYTOID.
   STORNINGSTAB.BRYTORGLIT = tidin.E2.
   
   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "F" AND INLASTAB.INKODTYP = "1" AND
   INLASTAB.INKODPOSCH = STRING(tidin.F1) USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   IF AVAILABLE INLASTAB THEN DO:
      FIND FIRST SEKTIONERING WHERE SEKTIONERING.INKODID = INLASTAB.INKODID
      NO-LOCK NO-ERROR.
      STORNINGSTAB.SEKTIONERID = SEKTIONERING.SEKTIONERID.
      STORNINGSTAB.SEKTIONLIT = tidin.F2.
   END.   
   
   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "G" AND INLASTAB.INKODTYP = "1" AND
   INLASTAB.INKODPOSCH = STRING(tidin.G1) USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   IF AVAILABLE INLASTAB THEN DO:
      FIND FIRST UTLOSNINGSKYDD WHERE UTLOSNINGSKYDD.INKODID = INLASTAB.INKODID
      NO-LOCK NO-ERROR.
      STORNINGSTAB.UTLOSID = UTLOSNINGSKYDD.UTLOSID.
   END.   
   
   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "G" AND INLASTAB.INKODTYP = "2" AND
   INLASTAB.INKODPOSCH = STRING(tidin.G2) USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   IF AVAILABLE INLASTAB THEN DO:
      FIND FIRST RELAINDIKERING WHERE RELAINDIKERING.INKODID = INLASTAB.INKODID
      NO-LOCK NO-ERROR.
      ASSIGN
      STORNINGSTAB.RELINID = RELAINDIKERING.RELINID.
   END. 

   STORNINGSTAB.FELYID = tidin.G3.
   
   ASSIGN
   STORNINGSTAB.BORTMW = INTEGER(SUBSTRING(STRING(tidin.H,"999999999"),1,6))
   STORNINGSTAB.BORTKW = INTEGER(SUBSTRING(STRING(tidin.H,"999999999"),7,3)).
   

   IF tidin.J1 = 0 THEN DO:
      IF INTEGER(SUBSTRING(STRING(tidin.J2,"999999"),1,2)) < INTEGER(SUBSTRING(STRING(tidin.BDAT,"999999"),5,2)) THEN DO:
/*             IF integer(SUBSTRING(STRING(tidin.J2,"999999"),1,2)) > 5 THEN DO: */
            ASSIGN
            STORNINGSTAB.DATUM70% = ?
            STORNINGSTAB.KLOCKAN70% = ?.
            STORNINGSTAB.DATUM100% = DATE(MONTH(tidin.BDAT) + 1,INTEGER(SUBSTRING(STRING(tidin.J2,"999999"),1,2)),YEAR(tidin.BDAT)).
            STORNINGSTAB.KLOCKAN100% = INTEGER(SUBSTRING(STRING(tidin.J2,"999999"),3,4)) / 100. 
/*             END. */
      END.
      ELSE DO:      
         ASSIGN
         STORNINGSTAB.DATUM70% = ?
         STORNINGSTAB.KLOCKAN70% = ?.
         STORNINGSTAB.DATUM100% = DATE(MONTH(tidin.BDAT),INTEGER(SUBSTRING(STRING(tidin.J2,"999999"),1,2)),YEAR(tidin.BDAT)).
         STORNINGSTAB.KLOCKAN100% = INTEGER(SUBSTRING(STRING(tidin.J2,"999999"),3,4)) / 100.
      END.
   END.
   ELSE DO:
      IF tidin.J1 = tidin.J2 THEN DO:
         IF INTEGER(SUBSTRING(STRING(tidin.J2,"999999"),1,2)) < INTEGER(SUBSTRING(STRING(tidin.BDAT,"999999"),5,2)) THEN DO:             
/*              IF integer(SUBSTRING(STRING(tidin.J2,"999999"),1,2)) > 5 THEN DO: */
                ASSIGN
                STORNINGSTAB.DATUM70% = ?
                STORNINGSTAB.KLOCKAN70% = ?.
                STORNINGSTAB.DATUM100% = DATE(MONTH(tidin.BDAT) + 1,INTEGER(SUBSTRING(STRING(tidin.J2,"999999"),1,2)),YEAR(tidin.BDAT)).
                STORNINGSTAB.KLOCKAN100% = INTEGER(SUBSTRING(STRING(tidin.J2,"999999"),3,4)) / 100.             
/*              END. */
          END.
          ELSE DO:      
             ASSIGN
             STORNINGSTAB.DATUM70% = ?
             STORNINGSTAB.KLOCKAN70% = ?.
             STORNINGSTAB.DATUM100% = DATE(MONTH(tidin.BDAT),INTEGER(SUBSTRING(STRING(tidin.J2,"999999"),1,2)),YEAR(tidin.BDAT)).
             STORNINGSTAB.KLOCKAN100% = INTEGER(SUBSTRING(STRING(tidin.J2,"999999"),3,4)) / 100.
          END.
      END.
      ELSE DO:
         STORNINGSTAB.DATUM70% = DATE(MONTH(tidin.BDAT),INTEGER(SUBSTRING(STRING(tidin.J1,"999999"),1,2)),YEAR(tidin.BDAT)).
         STORNINGSTAB.KLOCKAN70% = INTEGER(SUBSTRING(STRING(tidin.J1,"999999"),3,4)) / 100.
         STORNINGSTAB.DATUM100% = DATE(MONTH(tidin.BDAT),INTEGER(SUBSTRING(STRING(tidin.J2,"999999"),1,2)),YEAR(tidin.BDAT)).
         STORNINGSTAB.KLOCKAN100% = INTEGER(SUBSTRING(STRING(tidin.J2,"999999"),3,4)) / 100.
      END.      
   END.  

/*    IF tidin.J1DAT = ? THEN DO:                                                    */
/*       ASSIGN                                                                      */
/*       STORNINGSTAB.DATUM70% = ?                                                   */
/*       STORNINGSTAB.KLOCKAN70% = ?                                                 */
/*       STORNINGSTAB.DATUM100% = tidin.J2DAT                                        */
/*       STORNINGSTAB.KLOCKAN100% = tidin.J2KLOCK / 100.                             */
/*    END.                                                                           */
/*    ELSE DO:                                                                       */
/*       IF (tidin.J1DAT = tidin.J2DAT) AND (tidin.J1KLOCK = tidin.J2KLOCK) THEN DO: */
/*          ASSIGN                                                                   */
/*          STORNINGSTAB.DATUM70% = ?                                                */
/*          STORNINGSTAB.KLOCKAN70% = ?                                              */
/*          STORNINGSTAB.DATUM100% = tidin.J2DAT                                     */
/*          STORNINGSTAB.KLOCKAN100% = tidin.J2KLOCK / 100.                          */
/*       END.                                                                        */
/*       ELSE DO:                                                                    */
/*          ASSIGN                                                                   */
/*          STORNINGSTAB.DATUM70% = tidin.J1DAT                                      */
/*          STORNINGSTAB.KLOCKAN70% = tidin.J1KLOCK / 100                            */
/*          STORNINGSTAB.DATUM100% = tidin.J2DAT                                     */
/*          STORNINGSTAB.KLOCKAN100% = tidin.J2KLOCK / 100.                          */
/*       END.                                                                        */
/*    END.                                                                           */
   IF INDEX(tidin.J3,",",1) = 0 THEN DO:
      STORNINGSTAB.AVBROTTSTID = DECIMAL(tidin.J3).
   END.
   ELSE DO:   
      kolldecimal = (LENGTH(tidin.J3) - INDEX(tidin.J3,",",1)).
      IF kolldecimal = 1 THEN DO:      
         STORNINGSTAB.AVBROTTSTID = DECIMAL(tidin.J3) / 10.
      END.      
      ELSE IF kolldecimal = 2 THEN DO:
         STORNINGSTAB.AVBROTTSTID = DECIMAL(tidin.J3) / 100.
      END.      
      ELSE IF kolldecimal = 3 THEN DO:
         STORNINGSTAB.AVBROTTSTID = DECIMAL(tidin.J3) / 1000.
      END.      
      ELSE IF kolldecimal = 4 THEN DO:
         STORNINGSTAB.AVBROTTSTID = DECIMAL(tidin.J3) / 10000.
      END.      
      ELSE IF kolldecimal = 5 THEN DO:
         STORNINGSTAB.AVBROTTSTID = DECIMAL(tidin.J3) / 100000.
      END.      
      ELSE IF kolldecimal = 6 THEN DO:
         STORNINGSTAB.AVBROTTSTID = DECIMAL(tidin.J3) / 1000000.
      END.      
   END.
   ASSIGN
   STORNINGSTAB.ANTALHSP = tidin.K1
   STORNINGSTAB.ANTALLSP = tidin.K2
   STORNINGSTAB.ANTALREGSTN = tidin.L1
   STORNINGSTAB.ANTALNATSTN = tidin.L2
   STORNINGSTAB.EJBORTKUND = tidin.M1
   STORNINGSTAB.EJBORTMW = INTEGER(SUBSTRING(STRING(tidin.M2,"999999999"),1,6))
   STORNINGSTAB.EJBORTKW = INTEGER(SUBSTRING(STRING(tidin.M2,"999999999"),7,3)).
   
   
   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "N" AND INLASTAB.INKODTYP = "1" AND
   INLASTAB.INKODPOSCH = STRING(tidin.N1) USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   IF AVAILABLE INLASTAB THEN DO:
      FIND FIRST STORDRIFTOMR WHERE STORDRIFTOMR.INKODID = INLASTAB.INKODID
      NO-LOCK NO-ERROR.
      STORNINGSTAB.STDRIFTID = STORDRIFTOMR.STDRIFTID.
      STORNINGSTAB.STDRIFTLIT = tidin.N2.
   END.   
   
   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "P" AND INLASTAB.INKODTYP = "1" AND
   INLASTAB.INKODPOSCH = STRING(tidin.P1) USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   IF AVAILABLE INLASTAB THEN DO:
      FIND FIRST NATSTRUKTUR WHERE NATSTRUKTUR.INKODID = INLASTAB.INKODID
      NO-LOCK NO-ERROR.
      STORNINGSTAB.STRUKIDP1 = NATSTRUKTUR.STRUKID.
   END.   
   
   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "P" AND INLASTAB.INKODTYP = "2" AND
   INLASTAB.INKODPOSCH = STRING(tidin.P2) USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   IF AVAILABLE INLASTAB THEN DO:
      FIND FIRST NATSTRUKTUR WHERE NATSTRUKTUR.INKODID = INLASTAB.INKODID
      NO-LOCK NO-ERROR.
      STORNINGSTAB.STRUKIDP2 = NATSTRUKTUR.STRUKID.
   END.   

   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "Q" AND INLASTAB.INKODTYP = "1" AND
   INLASTAB.INKODPOSCH = STRING(tidin.Q) USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   IF AVAILABLE INLASTAB THEN DO:
      FIND FIRST NATTYP WHERE NATTYP.INKODID = INLASTAB.INKODID
      NO-LOCK NO-ERROR.
      STORNINGSTAB.NATTYPID = NATTYP.NATTYPID.
   END.   

   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "R" AND
   INLASTAB.INKODPOSCH = STRING(tidin.R) USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   FIND FIRST ANLAGGNINGSDEL WHERE ANLAGGNINGSDEL.INKODID = INLASTAB.INKODID
   NO-LOCK NO-ERROR.
   STORNINGSTAB.ADELID = ANLAGGNINGSDEL.ADELID.
   
   
   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "S" AND
   INLASTAB.INKODPOSCH = STRING(tidin.S) USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   IF AVAILABLE INLASTAB THEN DO:
      FIND FIRST FELORSAK WHERE FELORSAK.INKODID = INLASTAB.INKODID
      NO-LOCK NO-ERROR.
      STORNINGSTAB.FELOID = FELORSAK.FELOID.
   END.   
   
   FIND FIRST INLASTAB WHERE INLASTAB.INKOD = "T" AND INLASTAB.INKODTYP = "1" AND
   INLASTAB.INKODPOSCH = STRING(tidin.T1) USE-INDEX INKOD
   NO-LOCK NO-ERROR.
   IF AVAILABLE INLASTAB THEN DO:
      FIND FIRST RESERVKRAFTMETOD WHERE RESERVKRAFTMETOD.INKODID = INLASTAB.INKODID
      NO-LOCK NO-ERROR.
      STORNINGSTAB.RESERVKID = RESERVKRAFTMETOD.RESERVKID. 
   END.   
   ASSIGN
   STORNINGSTAB.ANTALRESERVKRAFT = tidin.T2
   STORNINGSTAB.TIDRESERVKRAFT = tidin.T3.
      
END PROCEDURE.
