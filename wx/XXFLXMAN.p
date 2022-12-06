/*XXFLXMAN.P.  FLEXKORNING*/
DEFINE VARIABLE kordatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.       
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.

DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.  
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE VARIABLE pflex AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE perflex AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE pover AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE ptot AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE personal LIKE PERSONALTAB.PERSONALKOD NO-UNDO.
DEFINE VARIABLE sdatum AS DATE NO-UNDO.
DEFINE VARIABLE totsal AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE diff AS DECIMAL FORMAT "99.99" NO-UNDO.
kordatum = 03/31/2015.
vkdatum = kordatum.
RUN flex_UI.
PROCEDURE flex_UI.
    /*FLEXSALDO.BACFLEX  -  Föregående månads FLEXSALDO.PERIODFLEX
   FLEXSALDO.PERIODFLEX -  Denna körnings saldo
   FLEXSALDO.ACCFLEX    -  Ingående saldo
   FLEXSALDO.EJKORDFLEX -  Ej körd flex tom igår
   FLEXSALDO.EJKFLSISTA -  Ej körd flex tom förra månaden*/
   
   
   personal = "".  /* ALLA FELMED MÅSTE VARA KONTROLLERADE  VID KÖRNING TAS DE MED*/
   pflex = 0.
   OPEN QUERY flxman FOR EACH FLEXDAG WHERE /*FLEXDAG.PERSONALKOD = "SETED" and*/ FLEXDAG.FELOK = TRUE AND FLEXDAG.KORD = 01/01/97
   USE-INDEX KONTROLL NO-LOCK BY PERSONALKOD BY DATUM.
   GET FIRST flxman NO-LOCK.
   DO WHILE AVAILABLE(FLEXDAG):     
     IF FLEXDAG.DATUM > vkdatum THEN musz = musz.    
     ELSE DO:               
        IF personal = "" THEN musz = musz.
        ELSE IF personal = FLEXDAG.PERSONALKOD THEN musz = musz.
        ELSE DO TRANSACTION:
           FIND FIRST FLEXSALDO WHERE FLEXSALDO.PERSONALKOD = personal
           USE-INDEX PKOD EXCLUSIVE-LOCK NO-ERROR.
           IF AVAILABLE FLEXSALDO THEN DO:             
              ASSIGN
              FLEXSALDO.BACFLEX = FLEXSALDO.PERIODFLEX           
              nytid = FLEXSALDO.PERIODFLEX.
              RUN TIMSEK.P.
              ASSIGN
              perflex = sekunder              
              sekunder = pflex.   /* ejkordflsista har räknats fram igen */
              RUN FSEKTIM.P.
              ASSIGN FLEXSALDO.PERIODFLEX = fnytid.            
              ASSIGN FLEXSALDO.EJKORDFLEX = 0
              FLEXSALDO.EJKFLSISTA = 0.
              nytid = FLEXSALDO.ACCFLEX.
              RUN TIMSEK.P.
              sekunder = sekunder + perflex.  
              RUN FSEKTIM.P.
              ASSIGN FLEXSALDO.ACCFLEX = fnytid.  /* ingående accflex + ingående periodflex*/    
              ASSIGN sekunder = sekunder + pflex.
              RUN FSEKTIM.P.
              totsal = fnytid.
              IF totsal > 40 THEN DO:
                 diff = totsal - 40.
                 nytid = diff.
                 RUN TIMSEK.P.                 
                 ASSIGN  sekunder = pflex - sekunder.
                 RUN FSEKTIM.P.
                 ASSIGN FLEXSALDO.PERIODFLEX = fnytid.
                 CREATE FLBET.
                 ASSIGN
                 FLBET.ACCFORE = totsal
                 FLBET.ACCEFTER = 40
                 FLBET.PERSONALKOD = FLEXSALDO.PERSONALKOD
                 FLBET.ANVANDARE = "FLEXKÖRNING"
                 FLBET.DATUM = vkdatum
                 FLBET.TIMMAR = 0 - diff .
              END.
              IF totsal < -10 THEN DO:
                 DEBUGGER:SET-BREAK().
                 diff = totsal + 10.
                 nytid = diff.
                 RUN TIMSEK.P.                 
                 ASSIGN  sekunder = pflex - sekunder.
                 RUN FSEKTIM.P.
                 ASSIGN FLEXSALDO.PERIODFLEX = fnytid.
                 CREATE FLBET.
                 ASSIGN
                 FLBET.ACCFORE = totsal
                 FLBET.ACCEFTER = -10
                 FLBET.PERSONALKOD = FLEXSALDO.PERSONALKOD
                 FLBET.ANVANDARE = "FLEXKÖRNING"
                 FLBET.DATUM = vkdatum
                 FLBET.TIMMAR = 0 - diff.

              END.
           END.
           ELSE DO:
              sekunder = pflex.
              RUN SEKTIM.P.
              CREATE FLEXSALDO.              
              ASSIGN
              FLEXSALDO.PERSONALKOD = personal
              FLEXSALDO.PERIODFLEX = nytid            
              FLEXSALDO.ACCFLEX = FLEXSALDO.PERIODFLEX.       
           END.              
           DO :
              CREATE FSALDMAN.
              ASSIGN 
              FSALDMAN.PERSONALKOD = FLEXSALDO.PERSONALKOD
              FSALDMAN.DATUM = vkdatum
              FSALDMAN.PERIODFLEX = FLEXSALDO.PERIODFLEX
              FSALDMAN.ACCFLEX = FLEXSALDO.ACCFLEX.           
              pflex = 0.
           END.   
        END.   
        /*IF personal = "" OR personal = FLEXDAG.PERSONALKOD THEN DO:*/
        DO:
           nytid = FLEXDAG.PLUS.
           RUN TIMSEK.P.      
           ASSIGN
           pflex = pflex + sekunder
           nytid = FLEXDAG.FLARB.
           RUN TIMSEK.P.      
           ASSIGN
           pflex = pflex + sekunder
           nytid = FLEXDAG.OVUTPLUS.
           RUN TIMSEK.P.      
           ASSIGN
           pflex = pflex + sekunder
           nytid = FLEXDAG.TOTALT.
           RUN TIMSEK.P.
           ASSIGN
           ptot = ptot + sekunder
           nytid = FLEXDAG.OVINPLUS.
           RUN TIMSEK.P.
           ASSIGN
           pover = pover + sekunder
           nytid = FLEXDAG.OVUTPLUS.
           RUN TIMSEK.P.
           pover = pover + sekunder.
        
        END.   
        personal = FLEXDAG.PERSONALKOD.
     END.        
     GET NEXT flxman NO-LOCK. 
   END.
   IF pflex NE 0 THEN DO TRANSACTION :  /* SISTA PERSONEN*/      
      FIND FIRST FLEXSALDO WHERE FLEXSALDO.PERSONALKOD = personal
      USE-INDEX PKOD EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE FLEXSALDO THEN DO :
         ASSIGN
         FLEXSALDO.BACFLEX = FLEXSALDO.PERIODFLEX           
         nytid = FLEXSALDO.PERIODFLEX.
         RUN TIMSEK.P.
         ASSIGN
         perflex = sekunder
         sekunder = pflex.
         RUN FSEKTIM.P.
         ASSIGN FLEXSALDO.PERIODFLEX = fnytid.            
         nytid = FLEXSALDO.ACCFLEX.
         RUN TIMSEK.P.
         sekunder = sekunder + perflex.  
         RUN FSEKTIM.P.
         ASSIGN FLEXSALDO.ACCFLEX = fnytid.  /* ingående accflex + ingående periodflex*/    
         ASSIGN sekunder = sekunder + pflex.
         RUN FSEKTIM.P.
         totsal = fnytid.
         IF totsal > 40 THEN DO:
            diff = totsal - 40.
            nytid = diff.
            RUN TIMSEK.P.                 
            ASSIGN  sekunder = pflex - sekunder.
            RUN FSEKTIM.P.
            ASSIGN FLEXSALDO.PERIODFLEX = fnytid.
            CREATE FLBET.
            ASSIGN
            FLBET.ACCFORE = totsal
            FLBET.ACCEFTER = 40
            FLBET.PERSONALKOD = FLEXSALDO.PERSONALKOD
            FLBET.ANVANDARE = "FLEXKÖRNING"
            FLBET.DATUM = vkdatum
            FLBET.TIMMAR = 0 - diff .
         END.
         IF totsal < -10 THEN DO:
            DEBUGGER:SET-BREAK().
            diff = totsal + 10.
            nytid = diff.
            RUN TIMSEK.P.                 
            ASSIGN  sekunder = pflex + sekunder.
            RUN FSEKTIM.P.
            ASSIGN FLEXSALDO.PERIODFLEX = fnytid.
            CREATE FLBET.
            ASSIGN
            FLBET.ACCFORE = totsal
            FLBET.ACCEFTER = -10
            FLBET.PERSONALKOD = FLEXSALDO.PERSONALKOD
            FLBET.ANVANDARE = "FLEXKÖRNING"
            FLBET.DATUM = vkdatum
            FLBET.TIMMAR = 0 - diff.
         END.
      END.
      ELSE DO :
         sekunder = pflex.
         RUN SEKTIM.P.
         CREATE FLEXSALDO.              
         ASSIGN
         FLEXSALDO.PERSONALKOD = personal
         FLEXSALDO.PERIODFLEX = nytid            
         FLEXSALDO.ACCFLEX = FLEXSALDO.PERIODFLEX.       
      END.              
      DO :
         CREATE FSALDMAN.
         ASSIGN 
         FSALDMAN.PERSONALKOD = FLEXSALDO.PERSONALKOD
         FSALDMAN.DATUM = vkdatum
         FSALDMAN.PERIODFLEX = FLEXSALDO.PERIODFLEX
         FSALDMAN.ACCFLEX = FLEXSALDO.ACCFLEX.           
         pflex = 0.      
      END.   
   END.
   
/*   OPEN QUERY fltman FOR EACH FLEXDAG WHERE FLEXDAG.FELOK = TRUE AND FLEXDAG.KORD = 01/01/97
   AND FLEXDAG.DATUM LE vkdatum USE-INDEX KONTROLL NO-LOCK,
   EACH FLEXTID WHERE FLEXTID.PERSONALKOD = FLEXDAG.PERSONALKOD AND
   FLEXTID.DATUM = FLEXDAG.DATUM USE-INDEX FLEX NO-LOCK.   
   GET FIRST fltman NO-LOCK.
   DO WHILE AVAILABLE(FLEXDAG):
      DO TRANSACTION:
         GET CURRENT fltman EXCLUSIVE-LOCK.
         ASSIGN  
         FLEXDAG.KORD = vkdatum
         FLEXTID.KORD = vkdatum.
      END. 
      GET NEXT fltman NO-LOCK.
   END.    
   OPEN QUERY fldman FOR EACH FLEXDAG WHERE FLEXDAG.FELOK = TRUE AND FLEXDAG.KORD = 01/01/97
   AND FLEXDAG.DATUM LE vkdatum USE-INDEX KONTROLL NO-LOCK.
   GET FIRST fldman NO-LOCK.
   DO WHILE AVAILABLE(FLEXDAG):
      DO TRANSACTION:
         GET CURRENT fldman EXCLUSIVE-LOCK.
         ASSIGN  
         FLEXDAG.KORD = vkdatum.
      END. 
      GET NEXT fldman NO-LOCK.
   END.             
   FOR EACH FLEXREG EXCLUSIVE-LOCK:
      ASSIGN FLEXREG.SALDOKORD = vkdatum.
   END.   */
END PROCEDURE.

