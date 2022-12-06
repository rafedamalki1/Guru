/*SPECLULE.P*/
DEFINE TEMP-TABLE tidinah
   FIELD ENR                AS CHARACTER       
   FIELD BENAMNING          AS CHARACTER 
   FIELD ENHET              AS CHARACTER
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

ASSIGN
filnamn = "D:\ELPOOL\DELAD\PRO9\priser061111.skv".

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
OUTPUT TO d:\elpool\DELAD\PRO9\ANDRADEPRISER.txt.
FOR EACH tidinah:
   FIND FIRST mtrldep WHERE mtrldep.enr = tidinah.ENR AND MTRLDEP.IBDATUM = ?
   NO-LOCK NO-ERROR.
   IF AVAILABLE mtrldep THEN DO: 
      IF tidinah.EXET = "N" THEN DO:
         IF tidinah.BPRIS / 100 NE MTRLDEP.NPRIS THEN DO:
            PUT MTRLDEP.ENR " " MTRLDEP.benamning " " MTRLDEP.NPRIS " " tidinah.BPRIS / 100 SKIP.                        
         END.
      END.
      ELSE DO:
         IF tidinah.NPRIS / 100 NE MTRLDEP.NPRIS THEN DO:
            PUT MTRLDEP.ENR " " MTRLDEP.benamning " " MTRLDEP.NPRIS " " tidinah.NPRIS / 100 SKIP.                        
         END.
      END.      
   END.
END.
OUTPUT CLOSE.
{EUROPEANAMERICAN.I}
/*
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
OUTPUT TO d:\elpool\DELAD\PRO9\100406.txt.
OPEN QUERY dq FOR EACH BERBEST WHERE BERBEST.Bestdatum > 04/01/06 AND BERBEST.LEVDATUM < 11/11/06 AND
BERBEST.OFFERT = FALSE AND berbest.aonr = "100406" AND berbest.delnr = 0 NO-LOCK.
GET FIRST dq NO-LOCK.
DO WHILE AVAILABLE(BERBEST):   
   IF berbest.levkod = "2" THEN DO:
      FIND FIRST mtrldep WHERE mtrldep.enr = berbest.enr AND MTRLDEP.IBDATUM = ?
      NO-LOCK NO-ERROR.
      IF AVAILABLE mtrldep THEN DO: 
         IF berbest.pris NE mtrldep.npris THEN DO:
            IF berbest.aonr NE "" THEN DO:
               FIND FIRST aonrtab WHERE aonrtab.aonr = berbest.aonr AND aonrtab.delnr = berbest.delnr NO-LOCK NO-ERROR.
               IF AVAILABLE aonrtab THEN DO:         
                  IF aonrtab.aonravdatum = 01/01/91 THEN DO: 
                     
                     FIND FIRST tidinah WHERE tidinah.ENR = BERBEST.ENR AND tidinah.BPRIS / 100 = BERBEST.PRIS AND
                     tidinah.EXET = "X" NO-LOCK NO-ERROR.
                     IF AVAILABLE tidinah THEN DO:                                          
                                              
                           PUT berbest.bestnr " " berbest.aonr " " berbest.delnr " " berbest.enr " " berbest.benamning " " 
                           berbest.bestdatum " " berbest.levdatum " " tidinah.nPRIS / 100 " " berbest.antal " " 
                           berbest.levkod " " mtrldep.levkod " " berbest.svinn " " berbest.UTTAG SKIP.                        
                     END.
                     ELSE DO:
                        PUT berbest.bestnr " " berbest.aonr " " berbest.delnr " " berbest.enr " " berbest.benamning " " 
                        berbest.bestdatum " " berbest.levdatum " " BERBEST.PRIS " " berbest.antal " " 
                        berbest.levkod " " mtrldep.levkod " " berbest.svinn " " berbest.UTTAG SKIP.
                     END.
                  
                     /*
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
                     */
                  END.
               END.
            END.
         END.
      END.
   END.
   ELSE DO:
      PUT berbest.bestnr " " berbest.aonr " " berbest.delnr " " berbest.enr " " berbest.benamning " " 
      berbest.bestdatum " " berbest.levdatum " " berbest.pris " " berbest.antal " " 
      berbest.levkod " " mtrldep.levkod " " berbest.svinn " " berbest.UTTAG SKIP.
   END.
               
   
   GET NEXT dq NO-LOCK.
END.
OUTPUT CLOSE.

OUTPUT TO D:\ELPOOL\DELAD\PRO9\koll.txt APPEND.
PUT "Klar uttag. " SKIP.
OUTPUT CLOSE.

/*
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
      IF berbest.pris NE mtrldep.npris THEN DO:
         IF berbest.aonr NE "" THEN DO:
            FIND FIRST aonrtab WHERE aonrtab.aonr = berbest.aonr AND aonrtab.delnr = berbest.delnr NO-LOCK NO-ERROR.
            IF AVAILABLE aonrtab THEN DO:         
               IF aonrtab.aonravdatum = 01/01/91 THEN DO:                             
                  /*
                  PUT berbest.bestnr " " berbest.aonr " " berbest.delnr " " berbest.enr " " berbest.benamning " " 
                  berbest.bestdatum " " berbest.levdatum " " berbest.pris " " berbest.antal " " 
                  berbest.levkod " " mtrldep.npris " " mtrldep.levkod " " berbest.svinn " " berbest.rest SKIP.               
                  */
                  /*
                  FIND FIRST TBEST WHERE TBEST.ENR = BERBEST.ENR AND TBEST.PRIS = BERBEST.PRIS
                  NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE TBEST THEN DO:                  
                     CREATE TBEST.
                     ASSIGN
                     TBEST.ENR = BERBEST.ENR
                     TBEST.PRIS = BERBEST.PRIS.                     

                     PUT berbest.bestnr " " berbest.aonr " " berbest.delnr " " berbest.enr " " berbest.benamning " " 
                     berbest.bestdatum " " berbest.levdatum " " berbest.pris " " berbest.antal " " 
                     berbest.levkod " " mtrldep.npris " " mtrldep.levkod " " berbest.svinn " " berbest.rest SKIP.
                  END.*/
                  
                  /*20061116 nu måste jag göra ett uttag med dagens pris och en retur med det till ekonomi överkörda priset för 
                  att vara tillbaks på 0 eftersom returerna gick OK medans uttagen blev till ett nollsummespel.*/
                  /*
                  CREATE tbest.
                  BUFFER-COPY BERBEST TO tbest.
                  DO TRANSACTION:    
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
                  END.
                  delete tbest.
                  */
                  
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
   END.
   GET NEXT dq2 NO-LOCK.
END.

*/

OUTPUT TO D:\ELPOOL\DELAD\PRO9\koll.txt APPEND.
PUT "Speckörning klart. " SKIP.
OUTPUT CLOSE.

/*
OUTPUT TO D:\ELPOOL\DELAD\PRO9\koll.txt APPEND.
PUT "Start DUMPNING. " SKIP.
OUTPUT CLOSE.
OUTPUT TO d:\elpool\DELAD\PRO9\BERBEST.txt.
FOR EACH BERBEST NO-LOCK:
   EXPORT BERBEST.
END.
OUTPUT CLOSE.
*/


/*
OUTPUT TO d:\elpool\DELAD\PRO9\avsluttag.txt.
OPEN QUERY dq FOR EACH BERBEST WHERE BERBEST.Bestdatum > 04/01/06 AND 
BERBEST.OFFERT = FALSE AND berbest.uttag = TRUE NO-LOCK.
GET FIRST dq NO-LOCK.
DO WHILE AVAILABLE(berbest):   
   FIND FIRST mtrldep WHERE mtrldep.enr = berbest.enr AND MTRLDEP.IBDATUM = ?
   NO-LOCK NO-ERROR.
   IF AVAILABLE mtrldep THEN DO: 
      IF berbest.pris NE mtrldep.npris THEN DO:
         IF berbest.aonr NE "" THEN DO:
            FIND FIRST aonrtab WHERE aonrtab.aonr = berbest.aonr AND aonrtab.delnr = berbest.delnr NO-LOCK NO-ERROR.
            IF AVAILABLE aonrtab THEN DO:         
               IF aonrtab.aonravdatum NE 01/01/91 THEN DO:                             
                  PUT berbest.bestnr " " berbest.aonr " " berbest.delnr " " berbest.enr " " berbest.benamning " " berbest.bestdatum " " berbest.levdatum " " berbest.pris " " berbest.antal " " berbest.levkod " " mtrldep.npris " " mtrldep.levkod " " berbest.svinn " " berbest.rest SKIP.               
               END.
            END.
         END.
      END.
   END.
   GET NEXT dq NO-LOCK.
END.
OUTPUT CLOSE.


OUTPUT TO d:\elpool\DELAD\PRO9\avslreturer.txt.
OPEN QUERY dq FOR EACH BERBEST WHERE BERBEST.Bestdatum > 04/01/06 AND 
BERBEST.OFFERT = FALSE AND berbest.uttag = FALSE NO-LOCK.
GET FIRST dq NO-LOCK.
DO WHILE AVAILABLE(berbest):   
   FIND FIRST mtrldep WHERE mtrldep.enr = berbest.enr AND MTRLDEP.IBDATUM = ?
   NO-LOCK NO-ERROR.
   IF AVAILABLE mtrldep THEN DO: 
      IF berbest.pris NE mtrldep.npris THEN DO:
         IF berbest.aonr NE "" THEN DO:
            FIND FIRST aonrtab WHERE aonrtab.aonr = berbest.aonr AND aonrtab.delnr = berbest.delnr NO-LOCK NO-ERROR.
            IF AVAILABLE aonrtab THEN DO:         
               IF aonrtab.aonravdatum NE 01/01/91 THEN DO:                             
                  PUT berbest.bestnr " " berbest.aonr " " berbest.delnr " " berbest.enr " " berbest.benamning " " berbest.bestdatum " " berbest.levdatum " " berbest.pris " " berbest.antal " " berbest.levkod " " mtrldep.npris " " mtrldep.levkod " " berbest.svinn " " berbest.rest SKIP.               
               END.
            END.
         END.
      END.
   END.
   GET NEXT dq NO-LOCK.
END.
OUTPUT CLOSE.
*/

/*
   OUTPUT TO d:\elpool\DELAD\PRO9\DEPÅ.txt.
   FOR EACH MTRLDEP NO-LOCK:
      EXPORT MTRLDEP.
   END.
   OUTPUT CLOSE.

   OPEN QUERY qmtrl FOR EACH MTRLDEP WHERE MTRLDEP.DEPNR = 1 AND MTRLDEP.IBDATUM = ? 
   AND MTRLDEP.LEVKOD = "2" NO-LOCK.        
   DO TRANSACTION:
      GET FIRST qmtrl EXCLUSIVE-LOCK.
      IF AVAILABLE MTRLDEP THEN DO:
         FIND FIRST MTRL WHERE MTRL.LEVKOD = MTRLDEP.LEVKOD AND 
         MTRL.KALKNR = 0 AND MTRL.ENR = MTRLDEP.ENR USE-INDEX LEV NO-LOCK NO-ERROR.
         IF AVAILABLE MTRL THEN DO:                                           
            ASSIGN
            MTRLDEP.BENAMNING = MTRL.BENAMNING
            MTRLDEP.NPRIS = MTRL.NPRIS
            MTRLDEP.BPRIS = MTRL.BPRIS
            MTRLDEP.ENHET = MTRL.ENHET.
         END.
      END.
   END.
   REPEAT:
      DO TRANSACTION:
         GET NEXT qmtrl EXCLUSIVE-LOCK.
         IF NOT AVAILABLE MTRLDEP THEN LEAVE.
         ELSE DO:
            FIND FIRST MTRL WHERE MTRL.LEVKOD = MTRLDEP.LEVKOD AND 
            MTRL.KALKNR = 0 AND MTRL.ENR = MTRLDEP.ENR USE-INDEX LEV NO-LOCK NO-ERROR.
            IF AVAILABLE MTRL THEN DO:                                              
               ASSIGN
               MTRLDEP.BENAMNING = MTRL.BENAMNING
               MTRLDEP.NPRIS = MTRL.NPRIS
               MTRLDEP.BPRIS = MTRL.BPRIS
               MTRLDEP.ENHET = MTRL.ENHET.
            END.
         END.
      END.
   END.   
   */
   /*
OUTPUT TO d:\elpool\DELAD\PRO9\MTRLPRISER2.txt.
OPEN QUERY mq FOR EACH MTRLDEP WHERE MTRLDEP.IBDATUM = ? AND MTRLDEP.LEVKOD = "4" NO-LOCK.
GET FIRST mq NO-LOCK.
DO WHILE AVAILABLE(MTRLDEP):
   FIND FIRST MTRL WHERE MTRL.KALKNR = 0 AND MTRL.LEVKOD = "2" AND MTRL.ENR = MTRLDEP.ENR
   NO-LOCK NO-ERROR.
   IF AVAILABLE MTRL THEN DO:
      PUT MTRLDEP.ENR " " MTRLDEP.BENAMNING " " MTRLDEP.ENHET " " MTRLDEP.NPRIS " " MTRL.NPRIS "   J"SKIP.
   END.
   ELSE DO:
      PUT MTRLDEP.ENR " " MTRLDEP.BENAMNING " " MTRLDEP.ENHET " " MTRLDEP.NPRIS " " MTRLDEP.NPRIS "   N"SKIP.
   END.
   GET NEXT MQ NO-LOCK.
END.
CLOSE QUERY MQ.
OUTPUT CLOSE.
     */
/*
OPEN QUERY mq FOR EACH MTRLDEP WHERE MTRLDEP.IBDATUM = ? NO-LOCK.
DO TRANSACTION:
   GET FIRST mq NO-LOCK.
   FIND FIRST MTRL WHERE MTRL.KALKNR = 0 AND MTRL.LEVKOD = "41" AND MTRL.ENR = MTRLDEP.ENR
   EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE MTRL THEN DO:
      ASSIGN
      MTRL.NPRIS = MTRLDEP.NPRIS
      MTRL.BPRIS = MTRLDEP.NPRIS.
   END.
   ELSE DO:
      CREATE MTRL.
      ASSIGN
      MTRL.ENR = MTRLDEP.ENR
      MTRL.BENAMNING = MTRLDEP.BENAMNING
      MTRL.ENHET = MTRLDEP.ENHET
      MTRL.NPRIS = MTRLDEP.NPRIS
      MTRL.BPRIS = MTRLDEP.NPRIS
      MTRL.LEVKOD = "41"
      MTRL.KALKNR = 0.
   END.
END.
REPEAT:
   DO TRANSACTION:
      GET NEXT MQ NO-LOCK.
      IF AVAILABLE MTRLDEP THEN DO:
         FIND FIRST MTRL WHERE MTRL.KALKNR = 0 AND MTRL.LEVKOD = "41" AND MTRL.ENR = MTRLDEP.ENR
         EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE MTRL THEN DO:
            ASSIGN
            MTRL.NPRIS = MTRLDEP.NPRIS
            MTRL.BPRIS = MTRLDEP.NPRIS.
         END.
         ELSE DO:
            CREATE MTRL.
            ASSIGN
            MTRL.ENR = MTRLDEP.ENR
            MTRL.BENAMNING = MTRLDEP.BENAMNING
            MTRL.ENHET = MTRLDEP.ENHET
            MTRL.NPRIS = MTRLDEP.NPRIS
            MTRL.BPRIS = MTRLDEP.NPRIS
            MTRL.LEVKOD = "41"
            MTRL.KALKNR = 0.
         END.
      END.
      ELSE LEAVE.
   END.
END.
CLOSE QUERY MQ.
OPEN QUERY LQ FOR EACH MTRL WHERE MTRL.KALKNR = 0 AND MTRL.LEVKOD = "41" NO-LOCK.
DO TRANSACTION:
   GET FIRST LQ EXCLUSIVE-LOCK.
   FIND FIRST MTRLDEP WHERE MTRLDEP.IBDATUM = ? AND MTRLDEP.ENR = MTRL.ENR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MTRLDEP THEN DELETE MTRL.
END.
REPEAT:
   DO TRANSACTION:
      GET NEXT LQ EXCLUSIVE-LOCK.
      IF AVAILABLE MTRL THEN DO:
         FIND FIRST MTRLDEP WHERE MTRLDEP.IBDATUM = ? AND MTRLDEP.ENR = MTRL.ENR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE MTRLDEP THEN DELETE MTRL.
      END.
      ELSE LEAVE.
   END.
END.
CLOSE QUERY LQ.
*/
*/

