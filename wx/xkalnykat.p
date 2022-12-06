/*xkalnykat.p*/

FOR EACH LOP1 WHERE LOP1.KATAR = EBRPRIS.ARTAL NO-LOCK:
   
   FIND FIRST KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGID = KALKYLKATALOG.KLOGID AND 
   KALKYLKATALOGSUB.BENAMNING = "EBR " + STRING(EBRPRIS.ARTAL - minskaartal)
   NO-LOCK NO-ERROR.
   FIND FIRST KALKYLLOPPOSTER WHERE KALKYLLOPPOSTER.KLOGSUBID =  KALKYLKATALOGSUB.KLOGSUBI AND
   KALKYLLOPPOSTER.ARBKOD = LOP1.ARBKOD AND KALKYLLOPPOSTER.LOPNR = LOP1.LOPNR       
   NO-LOCK NO-ERROR.                         
   IF NOT AVAILABLE KALKYLLOPPOSTER THEN DO:
      FIND FIRST KALKYLARBKODER WHERE KALKYLARBKODER.KLOGSUBID =  KALKYLKATALOGSUB.KLOGSUBI AND
      KALKYLARBKODER.ARBKOD = LOP1.ARBKOD        
      NO-LOCK NO-ERROR. 
      DO TRANSACTION:
         CREATE KALKYLLOPPOSTER.
         ASSIGN 
         KALKYLLOPPOSTER.MARKNING = KALKYLARBKODER.MARKNING
         KALKYLLOPPOSTER.TYPKALK = 1
         KALKYLLOPPOSTER.KLOGSUBID =  KALKYLKATALOGSUB.KLOGSUBI
         KALKYLLOPPOSTER.ARBKOD = LOP1.ARBKOD 
         KALKYLLOPPOSTER.LOPNR = LOP1.LOPNR
         KALKYLLOPPOSTER.BENAMNING = SUBSTRING(LOP1.BENAMNING,1,49)
         KALKYLLOPPOSTER.KOMMENTAR = SUBSTRING(LOP1.BENAMNING,60)
         KALKYLLOPPOSTER.ENHET = LOP1.ENHET
         KALKYLLOPPOSTER.EAMANGD = LOP1.EA.     
      END.
      deckatfix = 1. 
      IF KALKYLKATALOGSUB.BENAMNING = "EBR " + STRING(EBRPRIS.ARTAL - minskaartal) THEN DO: 
         IF LOP1.F9 = 0 THEN deckatfix = 1.  
         ELSE deckatfix = 100.  
      END.
      
      IF LENGTH(TRIM(SUBSTRING(LOP1.ARBKOD,2))) < 3 THEN DO:
         FIND FIRST KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND
         KALKYLPRISER.SOKBENAMNING = "BEREDARE REGION" NO-LOCK NO-ERROR.
      END.    
      ELSE DO:
         FIND FIRST KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND
         KALKYLPRISER.SOKBENAMNING = "BEREDARE" NO-LOCK NO-ERROR.
      END.              
      /*beredare*/
      DO TRANSACTION:
         RUN katlopsub_UI (INPUT LOP1.F1 / deckatfix, INPUT 0).
      END.            
      /*mont�r*/
      FIND FIRST KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND
      KALKYLPRISER.SOKBENAMNING = "MONT�R" NO-LOCK NO-ERROR.
      DO TRANSACTION:
         RUN katlopsub_UI (INPUT LOP1.F2 / deckatfix, INPUT 0).
      END.   
       /*MASKIN*/
      
       
      FIND FIRST KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND
      KALKYLPRISER.SOKBENAMNING = "MASKIN1" NO-LOCK NO-ERROR.
      DO TRANSACTION:
         FIND FIRST EXTRADATA WHERE EXTRADATA.PROGRAM = "EBRNYPRIS" AND EXTRADATA.HUVUDINT = EBRPRIS.ARTAL NO-LOCK NO-ERROR.
               
         IF (LOP1.MASKINKOST / LOP1.F3 - EBRPRIS.MASK1) < (LOP1.MASKINKOST / LOP1.F3 - EBRPRIS.MASK2) AND
            (LOP1.MASKINKOST / LOP1.F3 - EBRPRIS.MASK1 < (LOP1.MASKINKOST / LOP1.F3 - EXTRADATA.SOKDEC[4]) THEN DO:             
            RUN katlopsub_UI (INPUT LOP1.F3 / deckatfix, INPUT 0).
         END.
         ELSE DO:            
            IF (LOP1.MASKINKOST / LOP1.F3 - EBRPRIS.MASK2) < (LOP1.MASKINKOST / LOP1.F3 - EBRPRIS.MASK1) AND
            (LOP1.MASKINKOST / LOP1.F3 - EBRPRIS.MASK2 < (LOP1.MASKINKOST / LOP1.F3 - EXTRADATA.SOKDEC[4]) THEN DO:   
               FIND FIRST KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND
               KALKYLPRISER.SOKBENAMNING = "MASKIN2" NO-LOCK NO-ERROR.          
               RUN katlopsub_UI (INPUT LOP1.F3 / deckatfix, INPUT 0).
            END.
            
            ELSE IF (LOP1.MASKINKOST / LOP1.F3 - EXTRADATA.SOKDEC[4]) < (LOP1.MASKINKOST / LOP1.F3 - EBRPRIS.MASK1) AND
            (LOP1.MASKINKOST / LOP1.F3 - EXTRADATA.SOKDEC[4] < (LOP1.MASKINKOST / LOP1.F3 - EBRPRIS.MASK2) THEN DO:
               FIND FIRST KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND
               KALKYLPRISER.SOKBENAMNING = "MASKIN3" NO-LOCK NO-ERROR.          
               RUN katlopsub_UI (INPUT LOP1.F3 / deckatfix, INPUT 0).
            END.
         END.               
      END.
       /*MASKIN*/
      FIND FIRST KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND
      KALKYLPRISER.SOKBENAMNING = "MASKIN2" NO-LOCK NO-ERROR.
      DO TRANSACTION:
         RUN katlopsub_UI (INPUT LOP1.F4 / deckatfix, INPUT 0).
      END.   
       /*MASKIN*/
      FIND FIRST KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND
      KALKYLPRISER.SOKBENAMNING = "MASKIN4" NO-LOCK NO-ERROR.
      DO TRANSACTION:
         RUN katlopsub_UI (INPUT LOP1.F5 / deckatfix, INPUT 0).
      END.   
          /*MASKIN*/
      FIND FIRST KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND
      KALKYLPRISER.SOKBENAMNING = "MASKIN5" NO-LOCK NO-ERROR.
      DO TRANSACTION:
         RUN katlopsub_UI (INPUT LOP1.F6 / deckatfix, INPUT 0).
      END.  
      /*MASKIN*/
      FIND FIRST KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND
      KALKYLPRISER.SOKBENAMNING = "MASKIN3" NO-LOCK NO-ERROR.
      DO TRANSACTION:
         RUN katlopsub_UI (INPUT LOP1.F7 / deckatfix, INPUT 0).               
      END.
      /*MATERIEL*/
      FIND FIRST KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND
      KALKYLPRISER.SOKBENAMNING = "MATERIEL" NO-LOCK NO-ERROR.
      DO TRANSACTION:
         RUN katlopsub_UI (INPUT 0, INPUT LOP1.MATERIEL).
      END.            
      /*UTRUSTNING*/
      FIND FIRST KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND
      KALKYLPRISER.SOKBENAMNING = "UTRUSTNING" NO-LOCK NO-ERROR.
      DO TRANSACTION:
         
         RUN katlopsub_UI (INPUT LOP1.UTRUST / deckatfix, INPUT LOP1.UTRUSTKOST).
      END.   
      /*�VRIGKOSTNAD*/
      FIND FIRST KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND
      KALKYLPRISER.SOKBENAMNING = "�VRIGKOSTNAD" NO-LOCK NO-ERROR.
      DO TRANSACTION:
         RUN katlopsub_UI (INPUT 0, INPUT LOP1.OVRIGT).
      END.   
      /*SAMBYGGNADSPRIS  ?????*/
      FIND FIRST KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND
      KALKYLPRISER.SOKBENAMNING = "SAMBYGGNADSPRIS" NO-LOCK NO-ERROR.
      IF AVAILABLE KALKYLPRISER THEN DO TRANSACTION:
         RUN katlopsub_UI (INPUT 0, INPUT KALKYLPRISER.PRIS).
      END.   
      
      /*R�RLIGKOSTNAD EA  ?????*/
      FIND FIRST KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND
      KALKYLPRISER.SOKBENAMNING = "R�RLIGKOSTNAD EA" NO-LOCK NO-ERROR.
      IF AVAILABLE KALKYLPRISER THEN DO TRANSACTION:
         RUN katlopsub_UI (INPUT 0, INPUT KALKYLPRISER.PRIS).
      END.            
   END.                  
END.      
      
PROCEDURE katlopsub_UI :
   DEFINE INPUT  PARAMETER timvar AS DECIMAL NO-UNDO.
   DEFINE INPUT  PARAMETER kostvar AS DECIMAL NO-UNDO.
   IF AVAILABLE KALKYLPRISER THEN DO:
      CREATE KALKYLLOPSUB.
      ASSIGN
      KALKYLLOPSUB.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID
      KALKYLLOPSUB.ARBKOD = KALKYLLOPPOSTER.ARBKOD
      KALKYLLOPSUB.LOPNR = KALKYLLOPPOSTER.LOPNR
      KALKYLLOPSUB.KPID = KALKYLPRISER.KPID  
      KALKYLLOPSUB.TIMMAR = timvar
      KALKYLLOPSUB.KOSTNAD =  kostvar.
   END.      
END PROCEDURE.
      