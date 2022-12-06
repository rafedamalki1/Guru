
   
   /*xdepsnattrum.p*/       
DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.



DEFINE VARIABLE gurubilder AS CHARACTER NO-UNDO.
/*{PROVAG.I} */
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER  NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE satsvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE enrvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE melvar AS INTEGER NO-UNDO.
DEFINE VARIABLE melvar2 AS INTEGER NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO.
DEFINE VARIABLE pos1 AS INTEGER NO-UNDO. 
DEFINE VARIABLE hjenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE vald_depa  AS INTEGER NO-UNDO.
DEFINE VARIABLE timehjalp AS INTEGER NO-UNDO.
DEFINE VARIABLE trnr AS INTEGER NO-UNDO.
DEFINE BUFFER trdepbuff FOR TRUMMADEP.

DEFINE BUFFER mtrlbuff FOR MTRL.
DEFINE BUFFER mtrldepbuff FOR MTRLDEP.



DEFINE TEMP-TABLE tidinah
   FIELD TRUMMANAMN           AS CHARACTER
   FIELD ANTAL              AS INTEGER
   FIELD ENR                AS CHARACTER       
   FIELD indat AS date      
   FIELD NPRIS              AS DECIMAL   
       
   INDEX TRUMMANAMN IS PRIMARY TRUMMANAMN.
   

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
   
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.   
DEFINE VARIABLE leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
/*{EXTRADATA.I}
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
EMPTY TEMP-TABLE inextradatatemp NO-ERROR.*/
FIND FIRST FORETAG NO-LOCK NO-ERROR.
{muswait.i} 
OUTPUT TO c:\feldeptrum.txt.
ASSIGN 
   vald_depa = 3
   leverant = "12"
   /*filnamn = "C:\delad\Pro10s\SNATBERGET\DEPÅ\Inläs Hsand fiber trumma.SKV".
   filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\Kabel i depå 20191016\FIX trummor\Inläs Hsand fiber trumma.SKV".*/    
   RUN in_UI.
   /*ASSIGN 
   vald_depa = 2
   leverant = "12"
   filnamn = "C:\delad\Pro10s\SNATBERGET\DEPÅ\INLÄS ELNÄT trumma.SKV".
   /*filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\Kabel i depå 20191016\FIX trummor\INLÄS ELNÄT trumma.SKV".*/
   RUN in_UI.
   assign 
   vald_depa = 2
   leverant = "12"
   filnamn = "C:\delad\Pro10s\SNATBERGET\DEPÅ\Inläs elnät fiber trumma.skv".
   /*filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\Kabel i depå 20191016\FIX trummor\Inläs elnät fiber trumma.skv".*/
   RUN in_UI.
   ASSIGN  
   vald_depa = 5
   leverant = "12"
   filnamn = "C:\delad\Pro10s\SNATBERGET\DEPÅ\INLÄS Strömsund fiber trumma.SKV".
   /*filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\Kabel i depå 20191016\FIX trummor\INLÄS Strömsund fiber trumma.SKV".*/ 
   RUN in_UI.
   ASSIGN 
   vald_depa = 6
   leverant = "12"
   filnamn = "C:\delad\Pro10s\SNATBERGET\DEPÅ\Inläs Ånge fiber trumma.skv".
   /*filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\Kabel i depå 20191016\FIX trummor\Inläs Ånge fiber trumma.skv".*/    
   RUN in_UI.*/
   OUTPUT CLOSE.
   /*IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
*/   
PROCEDURE in_UI: 
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
   FOR EACH tidinah :
      IF SUBSTRING(tidinah.TRUMMANAMN,1,1) = "E" OR SUBSTRING(tidinah.TRUMMANAMN,1,1) = "D" OR SUBSTRING(tidinah.TRUMMANAMN,1,1) = "F"  OR SUBSTRING(tidinah.TRUMMANAMN,1,1) = "G" THEN.
      ELSE tidinah.TRUMMANAMN = "E" + tidinah.TRUMMANAMN.
   END.     
   

   RUN skapaenr_UI.           

   
END PROCEDURE.

PROCEDURE skapaenr_UI:   


   /*måste vara unik på enr datum och tid. Det går för fort med inläsning= sama tid pluusa på 1*/
   timehjalp = TIME.
   FIND LAST trdepbuff WHERE trdepbuff.DEPNR = vald_depa USE-INDEX TRUMMANR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE trdepbuff THEN DO:
      trnr = 1.
   END.
   ELSE DO:
      trnr = trdepbuff.TRUMMANR + 1.
   END.
   FOR EACH tidinah NO-LOCK:                                   
      DO TRANSACTION: 
         hjenr = tidinah.ENR.
         
         IF FORETAG.FORETAG = "SNAT" THEN DO: 
            IF SUBSTRING(hjenr,1,1) NE  "E" THEN hjenr = "E" + hjenr.
         END.      
         FIND FIRST mtrlbuff WHERE mtrlbuff.ENR =  hjenr AND
         mtrlbuff.LEVKOD = leverant AND mtrlbuff.KALKNR = 0 AND mtrlbuff.ENHET = "m" USE-INDEX LEV EXCLUSIVE-LOCK NO-ERROR.   
         IF AVAILABLE mtrlbuff THEN DO:
            FIND FIRST MTRLDEP WHERE MTRLDEP.DEPNR = vald_depa AND MTRLDEP.ENR = hjenr AND MTRLDEP.IBDATUM = ? EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE MTRLDEP THEN DO:            
              FIND FIRST TRUMMADEP WHERE TRUMMADEP.DEPNR = vald_depa AND TRUMMADEP.TRUMMAPLATS = tidinah.TRUMMANAMN   NO-LOCK NO-ERROR.   
              IF AVAILABLE TRUMMADEP THEN DO:
                 PUT UNFORMATTED "trumma finns redan" " " tidinah.ENR "  "  tidinah.TRUMMANAMN " " tidinah.antal " " vald_depa SKIP.    
              END.                                  
              ELSE DO:
                
                 CREATE TRUMMADEP.
                 ASSIGN 
                 TRUMMADEP.TRUMMANR = trnr
                 TRUMMADEP.TRUMMA = "S"                  
                 TRUMMADEP.TRUMMAPLATS = tidinah.TRUMMANAMN
                 TRUMMADEP.DEPNR = vald_depa                
                 TRUMMADEP.ENR = hjenr
                 TRUMMADEP.BENAMNING = mtrlbuff.Benamning
                 TRUMMADEP.ENHET = mtrlbuff.ENHET
                 TRUMMADEP.STMETER = tidinah.antal
                 TRUMMADEP.LAGMETER = tidinah.antal
                 TRUMMADEP.DATUM = tidinah.indat.
                 IF tidinah.NPRIS > 0 THEN TRUMMADEP.PRIS = tidinah.NPRIS / 100.
                 ELSE TRUMMADEP.PRIS = mtrlbuff.NPRIS.
                 
                 
                 trnr = trnr + 1.
              END.   
            END.
            ELSE DO:
               PUT UNFORMATTED "MTRLDEP finns inte" " " tidinah.ENR "  "  tidinah.TRUMMANAMN " " tidinah.antal " " vald_depa SKIP.              
            END.         
         END.
         ELSE DO:                                        
            PUT UNFORMATTED "MTRL finns inte" " " tidinah.ENR "  "  tidinah.TRUMMANAMN " " tidinah.antal " " vald_depa SKIP.            
         END.   
      END.            
   END.
/*   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).*/
              

/*   OS-DELETE VALUE(kommando).   
    OUTPUT CLOSE. */
END PROCEDURE.   

                

   

=======
   
   /*xdepsnattrum.p*/       
DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.



DEFINE VARIABLE gurubilder AS CHARACTER NO-UNDO.
/*{PROVAG.I} */
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER  NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE satsvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE enrvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE melvar AS INTEGER NO-UNDO.
DEFINE VARIABLE melvar2 AS INTEGER NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO.
DEFINE VARIABLE pos1 AS INTEGER NO-UNDO. 
DEFINE VARIABLE hjenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE vald_depa  AS INTEGER NO-UNDO.
DEFINE VARIABLE timehjalp AS INTEGER NO-UNDO.
DEFINE VARIABLE trnr AS INTEGER NO-UNDO.
DEFINE BUFFER trdepbuff FOR TRUMMADEP.

DEFINE BUFFER mtrlbuff FOR MTRL.
DEFINE BUFFER mtrldepbuff FOR MTRLDEP.



DEFINE TEMP-TABLE tidinah
   FIELD TRUMMANAMN           AS CHARACTER
   FIELD ANTAL              AS INTEGER
   FIELD ENR                AS CHARACTER       
   FIELD indat AS date      
   FIELD NPRIS              AS DECIMAL   
       
   INDEX TRUMMANAMN IS PRIMARY TRUMMANAMN.
   

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
   
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.   
DEFINE VARIABLE leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
/*{EXTRADATA.I}
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
EMPTY TEMP-TABLE inextradatatemp NO-ERROR.*/
FIND FIRST FORETAG NO-LOCK NO-ERROR.
{muswait.i} 
OUTPUT TO c:\feldeptrum.txt.
ASSIGN 
   vald_depa = 3
   leverant = "12"
   /*filnamn = "C:\delad\Pro10s\SNATBERGET\DEPÅ\Inläs Hsand fiber trumma.SKV".
   filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\Kabel i depå 20191016\FIX trummor\Inläs Hsand fiber trumma.SKV".*/    
   RUN in_UI.
   /*ASSIGN 
   vald_depa = 2
   leverant = "12"
   filnamn = "C:\delad\Pro10s\SNATBERGET\DEPÅ\INLÄS ELNÄT trumma.SKV".
   /*filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\Kabel i depå 20191016\FIX trummor\INLÄS ELNÄT trumma.SKV".*/
   RUN in_UI.
   assign 
   vald_depa = 2
   leverant = "12"
   filnamn = "C:\delad\Pro10s\SNATBERGET\DEPÅ\Inläs elnät fiber trumma.skv".
   /*filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\Kabel i depå 20191016\FIX trummor\Inläs elnät fiber trumma.skv".*/
   RUN in_UI.
   ASSIGN  
   vald_depa = 5
   leverant = "12"
   filnamn = "C:\delad\Pro10s\SNATBERGET\DEPÅ\INLÄS Strömsund fiber trumma.SKV".
   /*filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\Kabel i depå 20191016\FIX trummor\INLÄS Strömsund fiber trumma.SKV".*/ 
   RUN in_UI.
   ASSIGN 
   vald_depa = 6
   leverant = "12"
   filnamn = "C:\delad\Pro10s\SNATBERGET\DEPÅ\Inläs Ånge fiber trumma.skv".
   /*filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\Kabel i depå 20191016\FIX trummor\Inläs Ånge fiber trumma.skv".*/    
   RUN in_UI.*/
   OUTPUT CLOSE.
   /*IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
*/   
PROCEDURE in_UI: 
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
   FOR EACH tidinah :
      IF SUBSTRING(tidinah.TRUMMANAMN,1,1) = "E" OR SUBSTRING(tidinah.TRUMMANAMN,1,1) = "D" OR SUBSTRING(tidinah.TRUMMANAMN,1,1) = "F"  OR SUBSTRING(tidinah.TRUMMANAMN,1,1) = "G" THEN.
      ELSE tidinah.TRUMMANAMN = "E" + tidinah.TRUMMANAMN.
   END.     
   

   RUN skapaenr_UI.           

   
END PROCEDURE.

PROCEDURE skapaenr_UI:   


   /*måste vara unik på enr datum och tid. Det går för fort med inläsning= sama tid pluusa på 1*/
   timehjalp = TIME.
   FIND LAST trdepbuff WHERE trdepbuff.DEPNR = vald_depa USE-INDEX TRUMMANR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE trdepbuff THEN DO:
      trnr = 1.
   END.
   ELSE DO:
      trnr = trdepbuff.TRUMMANR + 1.
   END.
   FOR EACH tidinah NO-LOCK:                                   
      DO TRANSACTION: 
         hjenr = tidinah.ENR.
         
         IF FORETAG.FORETAG = "SNAT" THEN DO: 
            IF SUBSTRING(hjenr,1,1) NE  "E" THEN hjenr = "E" + hjenr.
         END.      
         FIND FIRST mtrlbuff WHERE mtrlbuff.ENR =  hjenr AND
         mtrlbuff.LEVKOD = leverant AND mtrlbuff.KALKNR = 0 AND mtrlbuff.ENHET = "m" USE-INDEX LEV EXCLUSIVE-LOCK NO-ERROR.   
         IF AVAILABLE mtrlbuff THEN DO:
            FIND FIRST MTRLDEP WHERE MTRLDEP.DEPNR = vald_depa AND MTRLDEP.ENR = hjenr AND MTRLDEP.IBDATUM = ? EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE MTRLDEP THEN DO:            
              FIND FIRST TRUMMADEP WHERE TRUMMADEP.DEPNR = vald_depa AND TRUMMADEP.TRUMMAPLATS = tidinah.TRUMMANAMN   NO-LOCK NO-ERROR.   
              IF AVAILABLE TRUMMADEP THEN DO:
                 PUT UNFORMATTED "trumma finns redan" " " tidinah.ENR "  "  tidinah.TRUMMANAMN " " tidinah.antal " " vald_depa SKIP.    
              END.                                  
              ELSE DO:
                
                 CREATE TRUMMADEP.
                 ASSIGN 
                 TRUMMADEP.TRUMMANR = trnr
                 TRUMMADEP.TRUMMA = "S"                  
                 TRUMMADEP.TRUMMAPLATS = tidinah.TRUMMANAMN
                 TRUMMADEP.DEPNR = vald_depa                
                 TRUMMADEP.ENR = hjenr
                 TRUMMADEP.BENAMNING = mtrlbuff.Benamning
                 TRUMMADEP.ENHET = mtrlbuff.ENHET
                 TRUMMADEP.STMETER = tidinah.antal
                 TRUMMADEP.LAGMETER = tidinah.antal
                 TRUMMADEP.DATUM = tidinah.indat.
                 IF tidinah.NPRIS > 0 THEN TRUMMADEP.PRIS = tidinah.NPRIS / 100.
                 ELSE TRUMMADEP.PRIS = mtrlbuff.NPRIS.
                 
                 
                 trnr = trnr + 1.
              END.   
            END.
            ELSE DO:
               PUT UNFORMATTED "MTRLDEP finns inte" " " tidinah.ENR "  "  tidinah.TRUMMANAMN " " tidinah.antal " " vald_depa SKIP.              
            END.         
         END.
         ELSE DO:                                        
            PUT UNFORMATTED "MTRL finns inte" " " tidinah.ENR "  "  tidinah.TRUMMANAMN " " tidinah.antal " " vald_depa SKIP.            
         END.   
      END.            
   END.
/*   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).*/
              

/*   OS-DELETE VALUE(kommando).   
    OUTPUT CLOSE. */
END PROCEDURE.   

                

   

>>>>>>> branch 'master' of file:///\\server05\delad\REMOTEGURU\GuruRemote.git
