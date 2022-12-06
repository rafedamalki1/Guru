/*BOLAGSEK.P.*/
&Scoped-define NEW NEW
{OMRTEMPW.I}
{GLOBVAR2DEL1.I}
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
{FORESTYR.I}
{BOLAGSEKTEMP.I}
{BOLAGSEKSTART.I}

RUN grundinstall_UI.
PROCEDURE grundinstall_UI :
   IF Guru.Konstanter:varforetypval[18] = 0 THEN RETURN.
   
   FIND FIRST BOLAGSEK NO-LOCK NO-ERROR.
   IF AVAILABLE BOLAGSEK THEN RETURN.
   FOR EACH ANVANDARE NO-LOCK:
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = ANVANDARE.PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE PERSONALTAB THEN DO:
         FIND FIRST omvtemp WHERE omvtemp.OMRADE  = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
         IF AVAILABLE omvtemp THEN DO:
            CREATE BOLAGSEK.
            ASSIGN
            BOLAGSEK.ANVANDARE =  ANVANDARE.ANVANDARE
            BOLAGSEK.OMRADE =  omvtemp.JUDID.                
         END.   
      END.   
   END.   
   
   IF Guru.Konstanter:globforetag = "elpa" THEN DO:
      RUN bextra_UI (INPUT  "rofo").
   END.
   IF Guru.Konstanter:globforetag = "SUND" THEN DO:       
      RUN bextra_UI (INPUT  "SEJKT").     
      RUN bextra_UI (INPUT  "SEKWM").      
   END.
   IF Guru.Konstanter:globforetag = "SNAT" THEN DO:       
      
      RUN bextra_UI (INPUT  "SEBHN").                  
      RUN bextra_UI (INPUT  "SEGSL").
      RUN bextra_UI (INPUT  "SEJSA").          
      RUN bextra_UI (INPUT  "SELNG").
      RUN bextra_UI (INPUT  "SEVRN").      
   END.    
    
   IF Guru.Konstanter:globforetag = "LULE" THEN DO: 
      RUN bextra_UI (INPUT  "sgjn").
      RUN bextra_UI (INPUT  "afs").
      RUN bextra_UI (INPUT  "ged").
      RUN bextra_UI (INPUT  "sög").
      RUN bextra_UI (INPUT  "LNT").
      RUN bextra_UI (INPUT  "mbl").
      RUN bextra_UI (INPUT  "FNN").
      RUN bextra_UI (INPUT  "KFK").
      RUN bextra_UI (INPUT  "awg").
   END. 
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO: 
      RUN bextra_UI (INPUT  "").
   END.
   
END PROCEDURE.
 
PROCEDURE bextra_UI :
   DEFINE INPUT PARAMETER vem AS CHARACTER NO-UNDO. 
   IF vem NE "" THEN DO:
      FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = vem NO-LOCK NO-ERROR.  
      FOR EACH omvtemp NO-LOCK:
         FIND FIRST BOLAGSEK WHERE BOLAGSEK.OMRADE = omvtemp.JUDID AND BOLAGSEK.ANVANDARE =  ANVANDARE.ANVANDARE EXCLUSIVE-LOCK NO-ERROR. 
         IF NOT AVAILABLE BOLAGSEK THEN CREATE BOLAGSEK.
         ASSIGN
         BOLAGSEK.ANVANDARE =  ANVANDARE.ANVANDARE
         BOLAGSEK.OMRADE =  omvtemp.JUDID.          
      END.
   END.   
   IF vem = "" THEN DO:      
      FOR EACH ANVANDARE NO-LOCK,
      EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = ANVANDARE.PERSONALKOD NO-LOCK:
         FIND FIRST omvtemp WHERE omvtemp.OMRADE  = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
         IF AVAILABLE omvtemp THEN DO:
            IF omvtemp.JUDID = "SEAB" THEN DO:
               FIND FIRST BOLAGSEK WHERE BOLAGSEK.OMRADE = omvtemp.JUDID AND BOLAGSEK.ANVANDARE =  ANVANDARE.ANVANDARE EXCLUSIVE-LOCK NO-ERROR. 
               IF NOT AVAILABLE BOLAGSEK THEN CREATE BOLAGSEK.
               ASSIGN
               BOLAGSEK.ANVANDARE =  ANVANDARE.ANVANDARE
               BOLAGSEK.OMRADE =  omvtemp.JUDID.                                  
            END. 
            ELSE DO:
               FOR EACH omvtemp:
                  IF omvtemp.JUDID NE "SEAB" THEN DO:
                     FIND FIRST BOLAGSEK WHERE BOLAGSEK.OMRADE = omvtemp.JUDID AND BOLAGSEK.ANVANDARE =  ANVANDARE.ANVANDARE EXCLUSIVE-LOCK NO-ERROR. 
                     IF NOT AVAILABLE BOLAGSEK THEN CREATE BOLAGSEK.
                     ASSIGN
                     BOLAGSEK.ANVANDARE =  ANVANDARE.ANVANDARE
                     BOLAGSEK.OMRADE =  omvtemp.JUDID.
                      
                  END.
               END.   
            END.        
         END.   
      END.
   END.
   
END PROCEDURE.
