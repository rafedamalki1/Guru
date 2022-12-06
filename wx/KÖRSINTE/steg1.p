/*STEG1.P*/
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
{AMERICANEUROPEAN.I}
OUTPUT TO D:\ELPOOL\DELAD\PRO9\koll.txt APPEND.
PUT "Start uttag. " SKIP.
OUTPUT CLOSE.
OUTPUT TO d:\elpool\DELAD\PRO9\100406.txt.
OPEN QUERY dq FOR EACH BERBEST WHERE BERBEST.Bestdatum > 04/01/06 AND
BERBEST.OFFERT = FALSE AND berbest.uttag = TRUE AND berbest.levkod = "2" NO-LOCK.
GET FIRST dq NO-LOCK.
DO WHILE AVAILABLE(BERBEST):      
   FIND FIRST mtrldep WHERE mtrldep.enr = berbest.enr AND MTRLDEP.IBDATUM = ?
   NO-LOCK NO-ERROR.
   IF AVAILABLE mtrldep THEN DO: 
      IF berbest.pris NE mtrldep.npris THEN DO:
         IF berbest.aonr NE "" THEN DO:
            FIND FIRST aonrtab WHERE aonrtab.aonr = berbest.aonr AND aonrtab.delnr = berbest.delnr NO-LOCK NO-ERROR.
            IF AVAILABLE aonrtab THEN DO:         
               IF aonrtab.aonravdatum = 01/01/91 THEN DO:                                          
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
                     BERBEST.PRIS = MTRLDEP.NPRIS
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
   GET NEXT dq NO-LOCK.
END.
OUTPUT CLOSE.
CLOSE QUERY dq.

OUTPUT TO D:\ELPOOL\DELAD\PRO9\koll.txt APPEND.
PUT "Klar uttag. " SKIP.
OUTPUT CLOSE.


OUTPUT TO D:\ELPOOL\DELAD\PRO9\koll.txt APPEND.
PUT "Start returer. " SKIP.
OUTPUT CLOSE.
/*OUTPUT TO d:\elpool\DELAD\PRO9\ejavslreturer.txt.*/


OPEN QUERY dq2 FOR EACH BERBEST WHERE BERBEST.Bestdatum > 04/01/06 AND
BERBEST.OFFERT = FALSE AND berbest.uttag = FALSE AND BERBEST.levkod = "2" NO-LOCK.
GET FIRST dq2 NO-LOCK.
DO WHILE AVAILABLE(berbest):   
   FIND FIRST mtrldep WHERE mtrldep.enr = berbest.enr AND MTRLDEP.IBDATUM = ?
   NO-LOCK NO-ERROR.
   IF AVAILABLE mtrldep THEN DO: 
      IF berbest.pris NE mtrldep.npris THEN DO:
         IF berbest.aonr NE "" THEN DO:
            FIND FIRST aonrtab WHERE aonrtab.aonr = berbest.aonr AND aonrtab.delnr = berbest.delnr NO-LOCK NO-ERROR.
            IF AVAILABLE aonrtab THEN DO:         
               IF aonrtab.aonravdatum = 01/01/91 THEN DO:                                                                                                                       
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
                     BERBEST.PRIS = MTRLDEP.NPRIS
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
   GET NEXT dq2 NO-LOCK.
END.
CLOSE QUERY dq2.
OUTPUT TO D:\ELPOOL\DELAD\PRO9\koll.txt APPEND.
PUT "Speckörning klart. " SKIP.
OUTPUT CLOSE.

{EUROPEANAMERICAN.I}