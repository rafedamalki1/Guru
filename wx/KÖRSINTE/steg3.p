/*STEG3.P*/
DEFINE VARIABLE prisvar AS DECIMAL NO-UNDO.
DEFINE TEMP-TABLE tidinah
   FIELD ENR                AS CHARACTER       
   FIELD BENAMNING          AS CHARACTER    
   FIELD BPRIS              AS DECIMAL  
   FIELD NPRIS              AS DECIMAL
   FIELD EXET               AS CHARACTER
   INDEX ENR IS PRIMARY ENR.


DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.

DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .

DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.   


DEFINE TEMP-TABLE TBEST NO-UNDO
FIELD   Antal AS INTEGER
FIELD   AONR AS CHARACTER
FIELD   BENAMNING AS CHARACTER
FIELD   BESTALLARE AS CHARACTER
FIELD   Bestdatum AS DATE
FIELD   BESTID AS CHARACTER
FIELD   Bestnr AS INTEGER
FIELD   DELNR AS INTEGER
FIELD   Depnr AS INTEGER
FIELD   Enhet AS CHARACTER
FIELD   Enr AS CHARACTER
FIELD   LEVDATUM AS DATE
FIELD   LEVKOD AS CHARACTER
FIELD   OFFERT AS LOGICAL
FIELD   Pris AS DECIMAL
FIELD   REKNR AS INTEGER
FIELD   REST AS LOGICAL
FIELD   SVINN AS LOGICAL
FIELD   UTTAG AS LOGICAL.

DEFINE BUFFER bbuff FOR BERBEST.
  
{muswait.i} 

ASSIGN
filnamn = "D:\ELPOOL\DELAD\PRO9\bytpris.skv".

EMPTY TEMP-TABLE intid NO-ERROR.
EMPTY TEMP-TABLE tidinah NO-ERROR.
{AMERICANEUROPEAN.I}
INPUT FROM VALUE(filnamn) NO-ECHO.
REPEAT:
   DO TRANSACTION: 
      CREATE tidinah.
      ASSIGN.
      IMPORT DELIMITER ";" tidinah   NO-ERROR.
   END.               
END.   
FOR EACH tidinah WHERE tidinah.ENR = "":
   DELETE tidinah.
END.            
      
OUTPUT TO D:\ELPOOL\DELAD\PRO9\koll.txt APPEND.
PUT "Start uttag. " SKIP.
OUTPUT CLOSE.

OPEN QUERY dq FOR EACH BERBEST WHERE BERBEST.Bestdatum > 04/01/06 AND BERBEST.LEVDATUM < 11/11/06 AND
BERBEST.OFFERT = FALSE AND berbest.uttag = TRUE AND BERBEST.levkod = "2" NO-LOCK.
GET FIRST dq NO-LOCK.
DO WHILE AVAILABLE(BERBEST):      
   FIND FIRST mtrldep WHERE mtrldep.enr = berbest.enr AND MTRLDEP.IBDATUM = ?
   NO-LOCK NO-ERROR.
   IF AVAILABLE mtrldep THEN DO: 
      prisvar = MTRLDEP.NPRIS.
      IF MTRLDEP.ENR = "0012848" THEN prisvar = 25.60.
      ELSE IF MTRLDEP.ENR = "0077165" THEN prisvar = 94.80.
      ELSE IF MTRLDEP.ENR = "0130190" THEN prisvar = 27.
      ELSE IF MTRLDEP.ENR = "0544041" THEN prisvar = 21.85.
      ELSE IF MTRLDEP.ENR = "0623131" THEN prisvar = 140.12.
      ELSE IF MTRLDEP.ENR = "0718346" THEN prisvar = 702.75.
      ELSE prisvar = prisvar.
      IF berbest.pris NE prisvar THEN DO:
         IF berbest.aonr NE "" THEN DO:
            FIND FIRST aonrtab WHERE aonrtab.aonr = berbest.aonr AND aonrtab.delnr = berbest.delnr NO-LOCK NO-ERROR.
            IF AVAILABLE aonrtab THEN DO:         
               IF aonrtab.aonravdatum = 01/01/91 THEN DO:                                    
                  FIND FIRST tidinah WHERE tidinah.ENR = BERBEST.ENR AND tidinah.BPRIS / 100 = BERBEST.PRIS AND
                  tidinah.EXET = "X" NO-LOCK NO-ERROR.
                  IF AVAILABLE tidinah THEN DO:                                    
                     CREATE tbest.
                     BUFFER-COPY BERBEST TO tbest.
                     DO TRANSACTION:    
                        CREATE BERBEST.                 
                        ASSIGN     
                        BERBEST.ENR = tbest.ENR
                        BERBEST.BENAMNING = tbest.BENAMNING
                        BERBEST.PRIS = tbest.PRIS
                        BERBEST.ENHET = tbest.ENHET
                        BERBEST.ANTAL = tbest.ANTAL 
                        BERBEST.LEVKOD = tbest.LEVKOD
                        BERBEST.AONR = tbest.AONR
                        BERBEST.DELNR = tbest.DELNR 
                        BERBEST.BESTID = tbest.BESTID
                        BERBEST.DEPNR = tbest.DEPNR
                        BERBEST.BESTDATUM = TODAY 
                        BERBEST.LEVDATUM = TODAY
                        SUBSTRING(BERBEST.BESTALLARE,1,59) = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)
                        BERBEST.UTTAG = FALSE.   
               
                        CREATE BERBEST.                 
                        ASSIGN
                        BERBEST.BESTNR = 0
                        BERBEST.REKNR = tbest.REKNR
                        BERBEST.ENR = tbest.ENR 
                        BERBEST.BENAMNING = tbest.BENAMNING
                        BERBEST.PRIS = prisvar
                        BERBEST.ENHET = tbest.ENHET
                        BERBEST.ANTAL = tbest.ANTAL 
                        BERBEST.LEVKOD = tbest.LEVKOD
                        BERBEST.AONR = tbest.AONR
                        BERBEST.DELNR = tbest.DELNR
                        BERBEST.BESTID = tbest.BESTID
                        BERBEST.DEPNR = tbest.DEPNR
                        BERBEST.BESTDATUM = TODAY 
                        BERBEST.LEVDATUM = TODAY
                        SUBSTRING(BERBEST.BESTALLARE,1,59) = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)
                        BERBEST.OFFERT = FALSE
                        BERBEST.UTTAG = TRUE. 
                     END.
                     delete tbest.
                  END.
               END.
            END.
         END.
      END.
   END.                     
   GET NEXT dq NO-LOCK.
END.
CLOSE QUERY dq.

OUTPUT TO D:\ELPOOL\DELAD\PRO9\koll.txt APPEND.
PUT "Klar uttag. " SKIP.
OUTPUT CLOSE.


OUTPUT TO D:\ELPOOL\DELAD\PRO9\koll.txt APPEND.
PUT "Start returer. " SKIP.
OUTPUT CLOSE.
/*OUTPUT TO d:\elpool\DELAD\PRO9\ejavslreturer.txt.*/


OPEN QUERY dq2 FOR EACH BERBEST WHERE BERBEST.Bestdatum > 04/01/06 AND BERBEST.LEVDATUM < 11/11/06 AND
BERBEST.OFFERT = FALSE AND berbest.uttag = FALSE AND BERBEST.levkod = "2" NO-LOCK.
GET FIRST dq2 NO-LOCK.
DO WHILE AVAILABLE(berbest):   
   FIND FIRST mtrldep WHERE mtrldep.enr = berbest.enr AND MTRLDEP.IBDATUM = ?
   NO-LOCK NO-ERROR.
   IF AVAILABLE mtrldep THEN DO: 
      prisvar = MTRLDEP.NPRIS.
      IF MTRLDEP.ENR = "0012848" THEN prisvar = 25.60.
      ELSE IF MTRLDEP.ENR = "0077165" THEN prisvar = 94.80.
      ELSE IF MTRLDEP.ENR = "0130190" THEN prisvar = 27.
      ELSE IF MTRLDEP.ENR = "0544041" THEN prisvar = 21.85.
      ELSE IF MTRLDEP.ENR = "0623131" THEN prisvar = 140.12.
      ELSE IF MTRLDEP.ENR = "0718346" THEN prisvar = 702.75.
      ELSE prisvar = prisvar.
      IF berbest.pris NE prisvar THEN DO:
         IF berbest.aonr NE "" THEN DO:
            FIND FIRST aonrtab WHERE aonrtab.aonr = berbest.aonr AND aonrtab.delnr = berbest.delnr NO-LOCK NO-ERROR.
            IF AVAILABLE aonrtab THEN DO:         
               IF aonrtab.aonravdatum = 01/01/91 THEN DO:                                                               
                  FIND FIRST tidinah WHERE tidinah.ENR = BERBEST.ENR AND tidinah.BPRIS / 100 = BERBEST.PRIS AND
                  tidinah.EXET = "X" NO-LOCK NO-ERROR.
                  IF AVAILABLE tidinah THEN DO:                                    
                     CREATE tbest.
                     BUFFER-COPY BERBEST TO tbest.
                     DO TRANSACTION:    
                        CREATE BERBEST.                 
                        ASSIGN
                        BERBEST.BESTNR = 0
                        BERBEST.REKNR = tbest.REKNR
                        BERBEST.ENR = tbest.ENR 
                        BERBEST.BENAMNING = tbest.BENAMNING
                        BERBEST.PRIS = tbest.PRIS
                        BERBEST.ENHET = tbest.ENHET
                        BERBEST.ANTAL = tbest.ANTAL 
                        BERBEST.LEVKOD = tbest.LEVKOD
                        BERBEST.AONR = tbest.AONR
                        BERBEST.DELNR = tbest.DELNR
                        BERBEST.BESTID = tbest.BESTID
                        BERBEST.DEPNR = tbest.DEPNR
                        BERBEST.BESTDATUM = TODAY 
                        BERBEST.LEVDATUM = TODAY
                        SUBSTRING(BERBEST.BESTALLARE,1,59) = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)
                        BERBEST.OFFERT = FALSE
                        BERBEST.UTTAG = TRUE. 
   
                        CREATE BERBEST.                 
                        ASSIGN     
                        BERBEST.ENR = tbest.ENR
                        BERBEST.BENAMNING = tbest.BENAMNING
                        BERBEST.PRIS = prisvar
                        BERBEST.ENHET = tbest.ENHET
                        BERBEST.ANTAL = tbest.ANTAL 
                        BERBEST.LEVKOD = tbest.LEVKOD
                        BERBEST.AONR = tbest.AONR
                        BERBEST.DELNR = tbest.DELNR 
                        BERBEST.BESTID = tbest.BESTID
                        BERBEST.DEPNR = tbest.DEPNR
                        BERBEST.BESTDATUM = TODAY 
                        BERBEST.LEVDATUM = TODAY
                        SUBSTRING(BERBEST.BESTALLARE,1,59) = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)
                        BERBEST.UTTAG = FALSE.                                    
                     END.
                     delete tbest.
                  END.
               END.
            END.
         END.
      END.
   END.
   GET NEXT dq2 NO-LOCK.
END.
CLOSE QUERY dq2.
OUTPUT TO D:\ELPOOL\DELAD\PRO9\koll.txt APPEND.
PUT "Speckörning klart. " SKIP.
OUTPUT CLOSE.
{EUROPEANAMERICAN.I}
