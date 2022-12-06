/*ANDTIDBTNR.I*/
musz = FALSE.   
   ASSIGN
   FILL-IN-DATUM = INPUT FRAME {&FRAME-NAME} FILL-IN-DATUM.         
   IF MONTH(extratidallt.DATUM) = 12 THEN datkoll = DATE(12,31,YEAR(extratidallt.DATUM)).      
   ELSE datkoll = DATE((MONTH(extratidallt.DATUM) + 1),01,YEAR(extratidallt.DATUM)) - 1.      
   IF FILL-IN-DATUM > DAY(datkoll) THEN DO:
      MESSAGE "Felaktigt angivet datum. Denna månad har bara" DAY(datkoll) "dagar."
      VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      RETURN.
   END.            
   IF FILL-IN-DATUM <= 0 THEN DO:
      MESSAGE "Felaktigt angivet datum. Datum kan ej vara mindre än 1." 
      VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      RETURN.
   
   END.            
   IF tillochmeddatum NE ? THEN DO:
      IF DAY(tillochmeddatum) >= FILL-IN-DATUM THEN DO:
         MESSAGE "Felaktigt angivet datum. Tidsedeln är godkänd till och med"
         tillochmeddatum VIEW-AS ALERT-BOX.
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.   
      END.            
   END.                                 
   DEFINE VARIABLE rsthj AS DECIMAL NO-UNDO. 
   DEFINE VARIABLE rslhj AS DECIMAL NO-UNDO.      
   IF FILL-IN-SLUT <  FILL-IN-START THEN DO:
      /*övertid söndag till måndag kontrollera att det inte är under arbetstid Lena 20200716*/
      regdatum = DATE((regmnr),FILL-IN-DATUM ,regar) + 1.      
      RUN REGVEC.P.
      RUN REGDAG.P.
      {SLUTARBW.I}
      rsthj = regstart.
      rslhj = regslut.
   END.   
   regdatum = DATE((regmnr),FILL-IN-DATUM,regar).
   extratidallt.DATUM = regdatum.
   RUN REGVEC.P.
   RUN REGDAG.P.
   {SLUTARBW.I}
   ASSIGN
   CMB_DAG = regdagnamn
   FILL-IN-VECKO = regvnr   
   musz = FALSE   
   CMB_PRISTYP = INPUT CMB_PRISTYP  
   FILL-IN-PRISTYP = CMB_PRISTYP.
   /*PRISFOR*/
   IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
     
   END.
   ELSE DO:
      FILL-IN-PRIS = INPUT FILL-IN-PRIS. 
   END.   
   IF utryckningtemp.NODF = TRUE THEN FILL-IN_NODF = INPUT FILL-IN_NODF.   
   IF Guru.Konstanter:globforetag = "cLULE"  THEN FILL-IN_VECKOVILA = INPUT FILL-IN_VECKOVILA.                    
                       
   ASSIGN
   FILL-IN-AONR = INPUT FILL-IN-AONR
   FILL-IN-DELNR = INPUT FILL-IN-DELNR.     
   TOG_FORSTA = INPUT TOG_FORSTA.     
   CMB_OVERUT = INPUT CMB_OVERUT.
   IF CMB_OVERUT = "Komp" THEN FILL-IN-OVER = "K".
   IF CMB_OVERUT = "Över" THEN FILL-IN-OVER = "Ö".
   IF CMB_OVERUT = "Flex" THEN FILL-IN-OVER = "F". 
   IF CMB_OVERUT = "Ejöv" THEN FILL-IN-OVER = "I". 
   IF CMB_OVERUT = "Ledi" THEN FILL-IN-OVER = "L".
   IF CMB_OVERUT = "Komb" THEN FILL-IN-OVER = "L".
   ASSIGN
   FILL-IN-SLUT = INPUT FILL-IN-SLUT
   FILL-IN-START = INPUT FILL-IN-START
   FILL-IN-TRAKT = INPUT CMB_TRAK
   FILL-IN-UTRYCK = INPUT FILL-IN-UTRYCK     
   FILL-IN-LUNCH = INPUT FILL-IN-LUNCH
   regdatumspar = regdatum.  
   FILL-IN_RESMAL = INPUT FILL-IN_RESMAL NO-ERROR. 
   IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      musz = FALSE.
      RUN ejerscheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         ASSIGN CMB_OVERUT = "Ejöv" FILL-IN-OVER = "I". 
      END.
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      musz = FALSE.
      RUN ejerscheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         ASSIGN CMB_OVERUT = "Ejöv" FILL-IN-OVER = "I". 
      END.
   END.
      
   IF FILL-IN-PRISTYP = "FRÅNVARO." AND FILL-IN-SLUT > regslut THEN DO:
      MESSAGE "Frånvaro kan  bara användas inom ordinarie arbetstid" VIEW-AS ALERT-BOX.    
      status-mus2 = SESSION:SET-WAIT-STATE("").
      RETURN.      
   END.
   ELSE IF FILL-IN-PRISTYP = "FRÅNVARO." AND FILL-IN-START < regstart THEN DO:
      MESSAGE "Frånvaro kan  bara användas inom ordinarie arbetstid" VIEW-AS ALERT-BOX.    
      status-mus2 = SESSION:SET-WAIT-STATE("").
      RETURN.      
   END.
   
   
   FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = personaltemp.PERSONALKOD 
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   IF AVAILABLE flexavttemp THEN DO:
      IF CMB_OVERUT = "Flex" AND flexavttemp.FLEXTID = FALSE THEN DO:
         MESSAGE "Personen har INTE flextidsavtal." VIEW-AS ALERT-BOX.    
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.      
      END.
      ELSE IF flexavttemp.FLEXTID = FALSE AND FILL-IN-AONR = "155" THEN DO:
         MESSAGE "Personen har INTE flextidsavtal." VIEW-AS ALERT-BOX.    
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.      
      END.
      ELSE IF FILL-IN-AONR = "155" AND FILL-IN-SLUT > regslut THEN DO:
         MESSAGE "Flexuttag kan  bara användas inom ordinarie arbetstid" VIEW-AS ALERT-BOX.    
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.      
      END.
      ELSE IF FILL-IN-AONR = "155" AND FILL-IN-START < regstart THEN DO:
         MESSAGE "Flexuttag kan  bara användas inom ordinarie arbetstid" VIEW-AS ALERT-BOX.    
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.      
      END.
      ELSE IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "ELPA"  THEN DO:
         IF flexavttemp.FLEXTID = TRUE AND CMB_OVERUT NE "Flex" THEN DO:
            IF FILL-IN-START GE regstart AND FILL-IN-START < regslut   THEN DO: 
               MESSAGE "Personen har flextidsavtal." VIEW-AS ALERT-BOX.    
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.      
            END.
            IF FILL-IN-SLUT LE regslut AND FILL-IN-SLUT > regstart THEN DO: 
               MESSAGE "Personen har flextidsavtal." VIEW-AS ALERT-BOX.    
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.      
            END.
            IF FILL-IN-START < regstart AND FILL-IN-SLUT > regslut AND regstart NE regslut THEN DO:
               MESSAGE "Personen har flextidsavtal." VIEW-AS ALERT-BOX.    
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.      
            END.
            IF FILL-IN-SLUT <  FILL-IN-START THEN DO:
               IF FILL-IN-START GE rsthj AND FILL-IN-START < rslhj   THEN DO: 
                  MESSAGE "Personen har flextidsavtal." VIEW-AS ALERT-BOX.    
                  status-mus2 = SESSION:SET-WAIT-STATE("").
                  RETURN.      
               END.
               IF FILL-IN-SLUT LE rslhj AND FILL-IN-SLUT > rsthj THEN DO: 
                  MESSAGE "Personen har flextidsavtal." VIEW-AS ALERT-BOX.    
                  status-mus2 = SESSION:SET-WAIT-STATE("").
                  RETURN.      
               END.
               IF FILL-IN-START < rsthj AND FILL-IN-SLUT > rslhj AND rsthj NE rslhj THEN DO:
                  MESSAGE "Personen har flextidsavtal." VIEW-AS ALERT-BOX.    
                  status-mus2 = SESSION:SET-WAIT-STATE("").
                  RETURN.      
               END.
            END.    
         END.         
      END.
   END.
   IF Guru.Konstanter:globforetag = "LULE" THEN DO:   
      IF CMB_OVERUT = "Komp" OR CMB_OVERUT = "Över" THEN DO:      
         FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = personaltemp.PERSONALKOD 
         USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
         IF AVAILABLE flexavttemp AND flexavttemp.FLEXTID = TRUE THEN DO:
            ASSIGN
            sok1 = personaltemp.PERSONALKOD
            sok4 = STRING(extratidallt.DATUM)      
            sok5 = regtotalt.
            IF Guru.Konstanter:appcon THEN DO: 
               RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT 46,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
               INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
            END.
            ELSE DO:
               RUN FLEXTIDH.P 
               (INPUT 46,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
               INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
            END.
            IF sok2 = 1 THEN DO:
               MESSAGE "Full arbetstid måste vara registrerad innan övertid accepteras." SKIP 
               "Förläng flexen med " + string(sok5,">9.99") + " timmar innan du registrerar övertiden."
                VIEW-AS ALERT-BOX.                
               status-mus2 = SESSION:SET-WAIT-STATE(""). 
               RETURN.                              
            END.
         END.
      END.
   END.
   IF Guru.Konstanter:globforetag = "MISV" THEN DO:
      /*Kontrollera max 48 tim övertid i löpnande 4-veckorsperiod from 1/12 2013*/
      nytid = FILL-IN-START.
      RUN TIMSEK.P.
      ASSIGN
      regstartsek = sekunder
      nytid = FILL-IN-SLUT.
      RUN TIMSEK.P.          
      ASSIGN
      regdatumspar = regdatum
      regslutsek = sekunder.        
      RUN TOTTIDW.P (INPUT pkod).
      nytid = FILL-IN-START.
      RUN TIMSEK.P.
      ASSIGN
      regstartsek = sekunder
      nytid = FILL-IN-SLUT.
      RUN TIMSEK.P.          
      ASSIGN
      regdatumspar = regdatum
      regslutsek = sekunder.        
      RUN TOTTIDW.P (INPUT pkod).    
      IF extratidallt.DATUM GE 12/01/2013 THEN DO:         
         IF FILL-IN-OVER = "F" OR FILL-IN-OVER = "I" THEN.
         ELSE IF FILL-IN-START > regslut OR FILL-IN-SLUT LE regstart THEN DO:       
            RUN ovmaxcheck_UI IN otbeordapph (INPUT personaltemp.PERSONALKOD,INPUT nytid,input vart,INPUT TABLE extratidallt,OUTPUT maxovcheck).      
            dispens48 = FALSE.
            RUN hdispens48 IN asfaktapph (INPUT personaltemp.PERSONALKOD, OUTPUT dispens48, OUTPUT d48datum, OUTPUT d48godk).
            IF  dispens48 = FALSE THEN DO:    
               IF maxovcheck > 48 THEN DO:
                  MESSAGE "Du har registrerar mer än 48 timmar övertid inom en fyra-veckorsperiod. Kontakta ansvarig chef. (" + STRING(maxovcheck) + " tim )" VIEW-AS ALERT-BOX.                
                  status-mus2 = SESSION:SET-WAIT-STATE(""). 
                  RETURN.            
               END.
            END.
         END.         
      END.   
    
   END.   
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "cELPA" THEN FILL-IN-TRAKT = 0.   
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      IF vart = "AND" THEN DO:
         /*Om man byter projekt på en endagstjänsteresa ska inte traktzon försvinna lena 20100809*/
         IF extratidallt.TRAKTAMENTE = 1 AND extratidallt.AONR NE FILL-IN-AONR THEN DO:
            FILL-IN-TRAKT = extratidallt.TRAKTAMENTE.            
         END.
      END.
   END.
   IF Guru.Konstanter:globforetag = "SNAT"  THEN DO:      
      IF extratidallt.LAGBAS = TRUE  THEN DO:
         MESSAGE "Vid ändring av godkänd övertid tas godkännande bort. Vill du detta?"  
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val9 AS LOGICAL.
         IF val9 = TRUE THEN DO:
            ASSIGN 
            extratidallt.LAGBAS = FALSE
            SUBSTRING(extratidallt.PROGRAM,159) = "".               
         END.
         ELSE DO:
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
   END.      
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      IF vart = "AND" THEN DO:
         IF FILL-IN-START NE extratidallt.START THEN DO:         
            ASSIGN
            sok1 = personaltemp.PERSONALKOD
            sok4 = STRING(extratidallt.DATUM)
            sok5 = extratidallt.START.
            IF Guru.Konstanter:appcon THEN DO: 
               RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT 17,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
               INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
            END.
            ELSE DO:
               RUN FLEXTIDH.P 
               (INPUT 17,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
               INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
            END.
            IF sok2 = 1 THEN DO:
               MESSAGE "Tiden redan registrerad via flex." VIEW-AS ALERT-BOX.    
               ASSIGN FILL-IN-START = extratidallt.START.
               DISPLAY FILL-IN-START WITH FRAME {&FRAME-NAME}.
               status-mus2 = SESSION:SET-WAIT-STATE(""). 
               RETURN.                              
            END.
         END.
         IF FILL-IN-SLUT NE extratidallt.SLUT THEN DO:         
            ASSIGN
            sok1 = personaltemp.PERSONALKOD
            sok4 = STRING(extratidallt.DATUM)
            sok5 = extratidallt.SLUT.
            IF sok2 = 1 THEN DO:
               MESSAGE "Tiden redan registrerad via flex." VIEW-AS ALERT-BOX.    
               ASSIGN FILL-IN-SLUT = extratidallt.SLUT.
               DISPLAY FILL-IN-SLUT WITH FRAME {&FRAME-NAME}.
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.               
            END.
         END.         
         IF spaonr = "155" AND FILL-IN-AONR NE "155" THEN DO:
            MESSAGE "Det går ej att ändra flex i tidskrivningen" VIEW-AS ALERT-BOX.                 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.               
         END.
         ELSE IF spaonr NE "155" AND FILL-IN-AONR = "155" THEN DO:
            MESSAGE "Det går ej att ändra flex i tidskrivningen" VIEW-AS ALERT-BOX.                 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.               
         END.
      END.         
      IF vart = "DEL" THEN DO:
         IF spaonr = "155" AND FILL-IN-AONR NE "155" THEN DO:
            MESSAGE "Det går ej att ändra flex i tidskrivningen" VIEW-AS ALERT-BOX.                 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.               
         END.
         ELSE IF spaonr NE "155" AND FILL-IN-AONR = "155" THEN DO:
            MESSAGE "Det går ej att ändra flex i tidskrivningen" VIEW-AS ALERT-BOX.                 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.               
         END.
      END.
   END.
   ASSIGN FILL-IN-PLUS = INPUT FILL-IN-PLUS.
   IF FILL-IN-PLUS > 0 THEN DO:
      IF vart = "DEL" THEN DO:
         nytid = extratidallt.START.
         RUN TIMSEK.P.
         ASSIGN regstartsek = sekunder
         nytid = FILL-IN-PLUS.
         RUN TIMSEK.P.
         RUN PLUSTIDW.P (INPUT pkod).
         ASSIGN FILL-IN-START = nytid.
      END.
      ELSE DO:
         nytid = FILL-IN-START.
         RUN TIMSEK.P.
         ASSIGN 
         regstartsek = sekunder
         nytid = FILL-IN-PLUS.
         RUN TIMSEK.P.
         RUN PLUSTIDW.P (INPUT pkod).
         ASSIGN FILL-IN-SLUT = nytid.
      END.   
   END.       
   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:
      IF regstart = regslut AND FILL-IN-OVER = "F"  THEN DO:
         IF AVAILABLE personaltemp AND  personaltemp.PERSONALKOD = "16101" THEN musz = musz.
         ELSE DO:        
            MESSAGE "Flextid skall ej registreras på helg" VIEW-AS ALERT-BOX.    
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.    
         END.
      END.
      IF Guru.Konstanter:globforetag = "cSUND" OR Guru.Konstanter:globforetag = "gkal" OR Guru.Konstanter:globforetag = "ELPA" THEN DO: /*gkal får flexa efter 18 Tina 8/12 2003, tillbaka 5/7 2004 enligt Tina*/
         IF FILL-IN-SLUT > 18 AND FILL-IN-OVER = "F" AND regslut LE 18  THEN DO:
            MESSAGE "Flextid skall ej registreras efter 18" VIEW-AS ALERT-BOX.    
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.      
         END. 
      END.
   END.
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
      /*Ändra till 6 2006-01-01*/
      IF FILL-IN-START < 6 AND FILL-IN-OVER = "F"  THEN DO:
         MESSAGE "Flextid skall ej registreras före 6" VIEW-AS ALERT-BOX.    
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.      
      END.  
      IF FILL-IN-START >  FILL-IN-SLUT AND FILL-IN-OVER = "F"  THEN DO:
         MESSAGE "Det går inte att registrera flextid över dygnsbrytning" VIEW-AS ALERT-BOX.    
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.
      END.       
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
      IF vart = "nya" AND FILL-IN-OVER = "F"  THEN DO:
         MESSAGE "Flextid registreras via flexrutinen" VIEW-AS ALERT-BOX.    
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.      
      END.
      IF FILL-IN-START < 6 AND FILL-IN-OVER = "F"  THEN DO:
         MESSAGE "Flextid skall ej registreras före 6" VIEW-AS ALERT-BOX.    
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.      
      END.       
   END.   
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:
      FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = personaltemp.PERSONALKOD 
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE flexavttemp THEN DO:
         IF flexavttemp.FLEXTID = TRUE THEN DO:
            IF regstart = regslut THEN DO:
               IF FILL-IN-LUNCH > 0 THEN DO:
                  MESSAGE "Lunch registreras bara arbetsdagar" VIEW-AS ALERT-BOX.    
                  status-mus2 = SESSION:SET-WAIT-STATE("").
                  RETURN.      
               END.
            END.
            ELSE IF regstart GE 7.30 AND regslut LE 11.30 THEN DO:              
               IF FILL-IN-LUNCH > 0 THEN DO:
                  MESSAGE "Lunch registreras inte halvdagar" VIEW-AS ALERT-BOX.    
                  status-mus2 = SESSION:SET-WAIT-STATE("").
                  RETURN.      
               END.
            END.    
            ELSE IF lunchslutet = lunchstarten THEN DO:
               /*ett schema som ej har lunch, tex deltidare 9-12 = 3 tim*/
               IF FILL-IN-LUNCH > 0 THEN DO:
                  MESSAGE "Detta schema har inte lunch" VIEW-AS ALERT-BOX.    
                  status-mus2 = SESSION:SET-WAIT-STATE("").
                  RETURN.      
               END.
            END.           
            ELSE DO:
               IF FILL-IN-LUNCH < 30 THEN DO:
                  MESSAGE "Minimilunch är 30 minuter" VIEW-AS ALERT-BOX.    
                  status-mus2 = SESSION:SET-WAIT-STATE("").
                  RETURN.      
               END.
               IF FILL-IN-LUNCH > 90 THEN DO:
                  MESSAGE "Maxlunch är 1 timme och 30 minuter (90min)" VIEW-AS ALERT-BOX.    
                  status-mus2 = SESSION:SET-WAIT-STATE("").
                  RETURN.      
               END.
            END.
            IF regstart = regslut AND FILL-IN-OVER = "F"  THEN DO:              
               MESSAGE "Skall verkligen flextid registreras på helg?"  
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val5 AS LOGICAL.
               IF val5 = TRUE THEN DO:
                  
               END.
               ELSE DO:
                  status-mus2 = SESSION:SET-WAIT-STATE("").
                  RETURN.
               END.
            END.
            
            FIND FIRST flexregtemp WHERE flexregtemp.KOD = flexavttemp.FLEXKOD USE-INDEX FLEXREG NO-LOCK NO-ERROR.                        
            IF AVAILABLE flexregtemp THEN DO:               
               flexkvsl = flexregtemp.KVSLUT.
               IF MONTH(TODAY) > MONTH(flexregtemp.SOMMARST) AND 
               MONTH(TODAY) < MONTH(flexregtemp.SOMMARSL) THEN DO:               
                  flexkvsl = flexregtemp.KVSOSL.
               END.
               ELSE IF MONTH(TODAY) = MONTH(flexregtemp.SOMMARSL) AND 
               DAY(TODAY) <= DAY(flexregtemp.SOMMARSL) THEN DO:                  
                  flexkvsl = flexregtemp.KVSOSL.
               END.
               ELSE IF MONTH(TODAY) = MONTH(flexregtemp.SOMMARST) AND 
               DAY(TODAY) >= DAY(flexregtemp.SOMMARST) THEN DO:                   
                  flexkvsl = flexregtemp.KVSOSL.
               END.
            END.           
         END.
      END.      
   END.
  
   
   FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND 
   utsokaonr.DELNR = FILL-IN-DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
   IF NOT AVAILABLE utsokaonr THEN DO:   
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 47
      soktemp.SOKCHAR[1] = FILL-IN-AONR
      soktemp.SOKINT[1] = FILL-IN-DELNR.
      {SOKANROP.I}      
      IF soktemp.SOKCHAR[2] = ? THEN DO:
         MESSAGE Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "finns inte." VIEW-AS ALERT-BOX.      
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.      
      END.
      ELSE IF soktemp.SOKDATE[1] = 01/01/1991 THEN musz = musz.
      ELSE IF soktemp.SOKDATE[1] < regdatum  THEN DO:
         MESSAGE Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är avslutat." VIEW-AS ALERT-BOX.      
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.      
      END.      
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN AOVALK.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT regdatum,INPUT ?,INPUT personaltemp.PERSONALKOD,
          INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,
          OUTPUT TABLE felmeddtemp).            
      END.
      ELSE DO:
         RUN AOVALK.P 
         (INPUT regdatum,INPUT ? ,INPUT personaltemp.PERSONALKOD,
          INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,
          OUTPUT TABLE felmeddtemp).            
      END.        
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.
         RETURN NO-APPLY.      
      END.
      /*PRISFOR*/
      IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
         ASSIGN
         CMB_PRISTYP = soktemp.SOKCHAR[3]  
         FILL-IN-PRISTYP = soktemp.SOKCHAR[3].
         ASSIGN  CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL-IN-PRISTYP. 
         DISPLAY CMB_PRISTYP WITH FRAME {&FRAME-NAME}.
      END.
   END.
   ELSE DO:      
      /*PRISFOR*/
      IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
         ASSIGN
         CMB_PRISTYP = utsokaonr.PRISTYP  
         FILL-IN-PRISTYP = utsokaonr.PRISTYP.
         ASSIGN  CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL-IN-PRISTYP. 
         DISPLAY CMB_PRISTYP WITH FRAME {&FRAME-NAME}.
      END.
      {AOKOLLERS.I}
      IF utsokaonr.AONRAVDATUM = 01/01/1991 OR
      utsokaonr.AONRAVDATUM >= regdatum THEN FILL-IN-DELNR = FILL-IN-DELNR.
      ELSE DO:
         MESSAGE Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är redan avslutat." VIEW-AS ALERT-BOX.         
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.
      END.
      /* arbetstidsförkortning*/
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
         nytid = FILL-IN-START.
         RUN TIMSEK.P.
         ASSIGN
         regstartsek = sekunder
         nytid = FILL-IN-SLUT.
         RUN TIMSEK.P.          
         ASSIGN
         regdatumspar = regdatum
         regslutsek = sekunder.        
         RUN TOTTIDW.P (INPUT pkod).                
         IF TOG_FORSTA = TRUE THEN DO:
            /*annars kollas fel registrering !! Lena 20191104*/
            nytid = klock60(klock100(extratidallt.TOTALT) - klock100(nytid)).            
         END.   
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN AOTIDKOLL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT regdatum,INPUT ?,INPUT personaltemp.PERSONALKOD,
            INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,INPUT vart ,INPUT nytid,
            OUTPUT TABLE felmeddtemp ,INPUT TABLE extratidallt ).            
         END.
         ELSE DO:
            RUN AOTIDKOLL.P 
            (INPUT regdatum,INPUT ?,INPUT personaltemp.PERSONALKOD,
            INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,INPUT vart,INPUT nytid,
            OUTPUT TABLE felmeddtemp ,INPUT TABLE extratidallt).            
         END. 
      END.
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.
         APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.      
      END.            
   END.  
   
   IF Guru.Konstanter:globforetag = "LULE" THEN DO:
      musz = FALSE.
      IF FILL-IN-AONR = "003005" AND FILL-IN-DELNR = 1 THEN musz = TRUE.
      IF FILL-IN-AONR = "100293" AND FILL-IN-DELNR = 0 THEN musz = TRUE.
      IF FILL-IN-AONR = "241" AND FILL-IN-DELNR = 0 THEN musz = TRUE.
      IF FILL-IN-AONR = "245" AND FILL-IN-DELNR = 0 THEN musz = TRUE.         
      IF musz = TRUE THEN DO:
         musz = FALSE.
         MESSAGE Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) " används bara för beredskap." VIEW-AS ALERT-BOX.         
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.
      END.
      
   END.            
   RUN REGVEC.P.
   {SLUTARBW.I}      
   IF FILL-IN-PRISTYP = "FRÅNVARO." THEN DO:      
      IF FILL-IN-OVER = "F" THEN musz = musz.            
      ELSE IF FILL-IN-START GE regslut OR FILL-IN-SLUT LE regstart 
      OR FILL-IN-SLUT > regslut OR FILL-IN-START < regstart THEN DO:
         IF skregslut = 24 THEN .
         ELSE DO:         
            MESSAGE "Övertid kan inte registreras på frånvaro " VIEW-AS ALERT-BOX.             
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.    

   END.   
   IF Guru.Konstanter:globforetag = "Csund" THEN DO:
      IF ftro = TRUE THEN DO:
         IF FILL-IN-PRISTYP = "FRÅNVARO." THEN.
         ELSE DO:
            MESSAGE "Enbart frånvaro skall registreras på personer med förtroendetid" VIEW-AS ALERT-BOX.             
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.   
   END.         
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "CELPA"  THEN DO:
      IF FILL-IN-PRISTYP = "FRÅNVARO." THEN DO:                    
         IF FILL-IN-START GE regslut OR FILL-IN-SLUT LE regstart 
         OR FILL-IN-SLUT > regslut OR FILL-IN-START < regstart THEN DO:
            IF skregslut = 24 THEN .
            ELSE DO: 
               MESSAGE "Övertid/Flex kan inte registreras på frånvaro " VIEW-AS ALERT-BOX.             
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
         END.
      END.          
      RUN kommentarutbcheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Kommentar om vad utbildningen avser är obligatorisk."  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
      musz = FALSE.
      RUN kommoblkom_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz, OUTPUT valdkom ).      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE valdkom VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
      musz = FALSE.
      RUN kommentaroblcheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Obligatorisk kommentar"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
      musz = FALSE.           
      RUN tvbarncheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = TRUE THEN DO:
         FILL-IN_RESMAL:FORMAT = "XXXXXX-XXXX".
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Fyll i barnets personnummer format 999999-9999"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         musz = FALSE.
         RUN pnrkoll_UI (OUTPUT musz).
         IF musz = TRUE THEN DO:
            musz = FALSE.
            MESSAGE "Barnets personnummer felaktigt angivet. Skall vara format 999999-9999"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         bpnr = DATE(SUBSTRING(FILL-IN_RESMAL,1,6)).
         balder = (( regdatum - bpnr ) / 365 ).         
         IF balder  > 12 THEN DO:
            MESSAGE "Om barnet är äldre än 12 år krävs intyg för " Guru.Konstanter:gaok " 132. Finns intyg?"  
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val3 AS LOGICAL.
            IF val3 = TRUE THEN DO:
               SUBSTRING(FILL-IN_RESMAL,11,50) = "Intygkrävs".
            END.
            ELSE DO:
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
         END.
      END.            
      musz = FALSE.      
      RUN fpbarncheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).           
      IF musz = ? THEN DO:
         /*ok utan personnummer 60 dagar nnnan barns födelse*/
         FILL-IN_RESMAL:FORMAT = "XXXXXX-XXXX".
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Barnets personnummer skall vara ifyllt ( format 999999-9999)." SKIP 
                    "Endast om det är frågan om föräldrapenning innan barnets födelse kan personnummer vara blankt." skip 
                    "Är det föräldrapenning före barnets födelse?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val6 AS LOGICAL.
            IF val6 = TRUE THEN DO:
               SUBSTRING(FILL-IN_RESMAL,12,50) = "Innan barns födelse".
            END.
            ELSE DO:
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.                         
         END.
         ELSE musz = TRUE.                  
      END.
      IF musz = TRUE THEN DO:
         FILL-IN_RESMAL:FORMAT = "XXXXXX-XXXX".
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Fyll i barnets personnummer format 999999-9999"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         musz = FALSE.
         RUN pnrkoll_UI (OUTPUT musz).
         IF musz = TRUE THEN DO:
            musz = FALSE.
            MESSAGE "Barnets personnummer felaktigt angivet. Skall vara format 999999-9999"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         bpnr = DATE(SUBSTRING(FILL-IN_RESMAL,1,6)).
         balder = (( regdatum - bpnr ) / 365 ).
         IF bpnr < 01/01/2014 THEN DO:
            IF balder  GE 9  THEN DO:
               MESSAGE "Barnet är äldre än 8 år " Guru.Konstanter:gaok " 131 får inte användas."  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
            ELSE IF balder  > 8 AND balder  < 9 AND MONTH(bpnr) > 7 THEN.            
            ELSE IF balder  > 8 AND balder  < 9 AND MONTH(regdatum) > 7 THEN DO:
               MESSAGE "Barnet är äldre än 8 år " Guru.Konstanter:gaok " 131 får inte användas."  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
         END.
         ELSE DO:
            IF balder  > 12 THEN DO:
               MESSAGE "Barnet är äldre än 12 år " Guru.Konstanter:gaok " 131 får inte användas."  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
               APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
            END.
         END.      
      END.            
   END.      
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:      
      musz = FALSE.
      RUN kommentarutbcheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            IF Guru.Konstanter:globforetag = "MISV"  THEN DO:      
               MESSAGE "Det är obligatoriskt att ange kursens namn."  VIEW-AS ALERT-BOX. 
            END.
            ELSE IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  THEN DO:      
               MESSAGE "Ange utbildningens och utbildningsföretagets namn."  VIEW-AS ALERT-BOX. 
            END.
            ELSE DO:            
               MESSAGE "Kommentar om vad utbildningen avser är obligatorisk."  VIEW-AS ALERT-BOX. 
            END.
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.      
      FILL-IN_RESMAL:FORMAT = "X(158)".
      
      musz = FALSE.      
      RUN tvbarncheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = TRUE THEN DO:
         FILL-IN_RESMAL:FORMAT = "XXXXXX-XXXX".
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Fyll i barnets personnummer format 999999-9999"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         musz = FALSE.
         RUN pnrkoll_UI (OUTPUT musz).
         IF musz = TRUE THEN DO:
            musz = FALSE.
            MESSAGE "Barnets personnummer felaktigt angivet. Skall vara format 999999-9999"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         bpnr = DATE(SUBSTRING(FILL-IN_RESMAL,1,6)).
         balder = (( regdatum - bpnr ) / 365 ).
         IF balder  > 12 THEN DO:
            MESSAGE "Om barnet är äldre än 12 år krävs intyg för " Guru.Konstanter:gaok " 118. Finns intyg?"  
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val2 AS LOGICAL.
            IF val2 = TRUE THEN DO:
               SUBSTRING(FILL-IN_RESMAL,12,50) = "Intyg krävs".
            END.
            ELSE DO:
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
         END.
      END.
                  
      musz = FALSE.
      RUN fpbarncheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = ? THEN DO:
         /*ok utan personnummer 60 dagar nnnan barns födelse*/
         FILL-IN_RESMAL:FORMAT = "XXXXXX-XXXX".
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Barnets personnummer skall vara ifyllt ( format 999999-9999)." SKIP 
                    "Endast om det är frågan om föräldrapenning innan barnets födelse kan personnummer vara blankt." skip 
                    "Är det föräldrapenning före barnets födelse?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val7 AS LOGICAL.
            IF val7 = TRUE THEN DO:
               SUBSTRING(FILL-IN_RESMAL,12,50) = "Innan barns födelse".
            END.
            ELSE DO:
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.                         
         END.
         ELSE musz = TRUE.                  
      END.
      IF musz = TRUE THEN DO:         
         FILL-IN_RESMAL:FORMAT = "XXXXXX-XXXX".
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Fyll i barnets personnummer format 999999-9999"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         musz = FALSE.
         RUN pnrkoll_UI (OUTPUT musz).
         IF musz = TRUE THEN DO:
            musz = FALSE.
            MESSAGE "Barnets personnummer felaktigt angivet. Skall vara format 999999-9999"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         bpnr = DATE(SUBSTRING(FILL-IN_RESMAL,1,6)).
         balder = (( regdatum - bpnr ) / 365 ).
         IF bpnr < 01/01/2014 THEN DO:
            IF balder  GE 9 THEN DO:
               MESSAGE "Barnet är äldre än 8 år " Guru.Konstanter:gaok " 119 eller 117 får inte användas."  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
            ELSE IF balder  > 8 AND balder  < 9 AND MONTH(bpnr) > 7 THEN.            
            ELSE IF balder  > 8 AND balder  < 9 AND MONTH(regdatum) > 7 THEN DO:
               MESSAGE "Barnet är äldre än 8 år " Guru.Konstanter:gaok " 119 eller 117 får inte användas."  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
         END.
         ELSE DO:
            IF balder  > 12 THEN DO:
               MESSAGE "Barnet är äldre än 12 år " Guru.Konstanter:gaok " 119 eller 117 får inte användas."  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
               APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
            END.
         END.      
      END.                  
      musz = FALSE.
      RUN kommoblkom_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz, OUTPUT valdkom ).      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE valdkom VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
      musz = FALSE.      
      RUN kommentaroblcheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Obligatorisk kommentar"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
      musz = FALSE.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         musz = FALSE.      
         RUN kommobllangd_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).         
         IF musz = TRUE THEN DO:
            musz = FALSE.
            IF LENGTH(FILL-IN_RESMAL) < 10 THEN DO:
               MESSAGE "Obligatorisk kommentar måste vara längre än 10 tecken."  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
         END.
      END.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:
         FILL-IN-OTBRD = INPUT FILL-IN-OTBRD.
         IF FILL-IN-OTBRD NE ""  THEN DO:      
            FIND FIRST otidbeordtemp WHERE otidbeordtemp.PERSONALKOD = FILL-IN-OTBRD AND otidbeordtemp.AKTIV = TRUE  NO-LOCK NO-ERROR.
            IF NOT AVAILABLE otidbeordtemp  THEN DO:
               MESSAGE "Angiven övertidsbeordrare finns inte"  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
            IF FILL-IN-OTBRD = personaltemp.PERSONALKOD THEN DO:                  
               MESSAGE "Man kan inte berodra sig själv"  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
               
            END.
         END.                          
         IF FILL-IN-OVER = "F" /*OR personaltemp.OVERTIDUTTAG = "I"*/ THEN DO:
            IF FILL-IN-OTBRD NE "" THEN DO:
               MESSAGE "Övertidsbeordrare skall skall bara användas för övertid"  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.      
         END.   
         ELSE IF personaltemp.OVERTIDUTTAG = "I" THEN DO:
            /*driftledare på snat med inläst övertid som får övertid vid beredskap ska också registrera övertidsbeordare  Lena 20201127*/
            IF  FILL-IN-OVER = "Ö" OR  FILL-IN-OVER = "K" THEN DO:
               IF FILL-IN-OTBRD = "" THEN DO:
                  MESSAGE "Övertidsbeordrare måste anges vid övertid. Välj från vallista!"  VIEW-AS ALERT-BOX. 
                  status-mus2 = SESSION:SET-WAIT-STATE("").
                  RETURN.
               END.
               IF FILL-IN_RESMAL = "" THEN DO:
                  MESSAGE "Kommentar är obligatorisk vid övertid"  VIEW-AS ALERT-BOX. 
                  status-mus2 = SESSION:SET-WAIT-STATE("").
                  RETURN.
               END.                          
            END.
            ELSE DO:
               IF FILL-IN-OTBRD NE "" THEN DO:
                  MESSAGE "Övertidsbeordrare skall skall bara användas för övertid"  VIEW-AS ALERT-BOX. 
                  status-mus2 = SESSION:SET-WAIT-STATE("").
                  RETURN.
               END.                          
            END.
                    
         END.   
         ELSE DO:                              
            IF FILL-IN-START > FILL-IN-SLUT AND regslut = 24.00 AND skregstart = 00.00 THEN DO:                
               IF skregstart = 0 AND skregslut = 24 THEN DO:            
                  IF FILL-IN-START GE lunchslutet AND FILL-IN-SLUT LE sklunchstarten  THEN DO:
                     IF FILL-IN-OTBRD NE "" THEN DO:
                        MESSAGE "Övertidsbeordrare skall skall bara användas för övertid"  VIEW-AS ALERT-BOX. 
                        status-mus2 = SESSION:SET-WAIT-STATE("").
                        RETURN.
                     END.      
                  END.
                  ELSE IF FILL-IN-START GE lunchslutet AND FILL-IN-SLUT > sklunchstarten  THEN DO:
                     IF FILL-IN-OTBRD = "" THEN DO:
                        MESSAGE "Övertidsbeordrare måste anges vid övertid. Välj från vallista!"  VIEW-AS ALERT-BOX. 
                        status-mus2 = SESSION:SET-WAIT-STATE("").
                        RETURN.
                     END.      
                     IF FILL-IN_RESMAL = "" THEN DO:
                        MESSAGE "Kommentar är obligatorisk vid övertid"  VIEW-AS ALERT-BOX. 
                        status-mus2 = SESSION:SET-WAIT-STATE("").
                        RETURN.
                     END.
                  END.
               END.
               ELSE DO:
                  IF FILL-IN-START GE lunchslutet AND FILL-IN-SLUT LE skregslut  THEN DO:
                     IF FILL-IN-OTBRD NE "" THEN DO:
                        MESSAGE "Övertidsbeordrare skall skall bara användas för övertid"  VIEW-AS ALERT-BOX. 
                        status-mus2 = SESSION:SET-WAIT-STATE("").
                        RETURN.
                     END.      
                  END.
                  ELSE IF FILL-IN-START GE lunchslutet AND FILL-IN-SLUT > skregslut  THEN DO:
                     IF FILL-IN-OTBRD = "" THEN DO:
                        MESSAGE "Övertidsbeordrare måste anges vid övertid. Välj från vallista!"  VIEW-AS ALERT-BOX. 
                        status-mus2 = SESSION:SET-WAIT-STATE("").
                        RETURN.
                     END.      
                     IF FILL-IN_RESMAL = "" THEN DO:
                        MESSAGE "Kommentar är obligatorisk vid övertid"  VIEW-AS ALERT-BOX. 
                        status-mus2 = SESSION:SET-WAIT-STATE("").
                        RETURN.
                     END.
                  END.
               END.
            END.
            ELSE IF FILL-IN-START < regstart OR FILL-IN-SLUT > regslut OR FILL-IN-SLUT LE regstart  THEN DO:                       
               IF FILL-IN-OVER = "I" OR FILL-IN-OVER = "F" THEN musz = musz.
               ELSE DO:               
                  IF FILL-IN-OTBRD = "" THEN DO:
                     MESSAGE "Övertidsbeordrare måste anges vid övertid. Välj från vallista!"  VIEW-AS ALERT-BOX. 
                     status-mus2 = SESSION:SET-WAIT-STATE("").
                     RETURN.
                  END.      
                  IF FILL-IN_RESMAL = "" THEN DO:
                     MESSAGE "Kommentar är obligatorisk vid övertid"  VIEW-AS ALERT-BOX. 
                     status-mus2 = SESSION:SET-WAIT-STATE("").
                     RETURN.
                  END.
               END.
            END.
            ELSE IF FILL-IN-START GE regstart AND FILL-IN-SLUT LE regslut  THEN DO:                                      
               IF regstart = 0 AND regslut = 24 THEN DO:
                  /*skift - övertid på lunchen*/
                  IF FILL-IN-START < lunchstarten AND FILL-IN-SLUT > lunchslutet THEN DO:
                     IF FILL-IN-OTBRD NE "" THEN DO:
                        MESSAGE "Övertidsbeordrare skall skall bara användas för övertid"  VIEW-AS ALERT-BOX. 
                        status-mus2 = SESSION:SET-WAIT-STATE("").
                        RETURN.
                     END.      
                  END.
                  
               END.
               ELSE DO:               
                  IF FILL-IN-OTBRD NE "" THEN DO:
                     MESSAGE "Övertidsbeordrare skall skall bara användas för övertid"  VIEW-AS ALERT-BOX. 
                     status-mus2 = SESSION:SET-WAIT-STATE("").
                     RETURN.
                  END.    
               END.
            END.            
         END.             
         IF Guru.Konstanter:globforetag = "csund" OR Guru.Konstanter:globforetag = "elpa" THEN DO:   
            /*krav på full arbetstid inna övertid utom om det sklijer mer än 30 miunter efter utflex*/
            IF CMB_OVERUT = "Komp" OR CMB_OVERUT = "Över" THEN DO:      
               FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = personaltemp.PERSONALKOD 
               USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
               IF AVAILABLE flexavttemp AND flexavttemp.FLEXTID = TRUE THEN DO:                  
                  ASSIGN
                  sok1 = personaltemp.PERSONALKOD
                  sok4 = STRING(extratidallt.DATUM)      
                  sok5 = regtotalt
                  sok2 = regslut
                  sok3 = "".
                  
                  /*46 kollar bara de som inte har beredskap  49 kollar alla*/
                  IF Guru.Konstanter:appcon THEN DO: 
                     RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
                     (INPUT 49,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
                     INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
                  END.
                  ELSE DO:
                     RUN FLEXTIDH.P 
                     (INPUT 49,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
                     INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
                  END.                                                      
                  IF sok3 = "I" THEN DO:
                     /*utan beredskapsavtal måste full arbetstid vara reg innan övertid tillåts*/
                     IF sok5 = 1 THEN DO:
                        MESSAGE "Full arbetstid måste vara registrerad innan övertid accepteras" VIEW-AS ALERT-BOX.                
                        status-mus2 = SESSION:SET-WAIT-STATE(""). 
                        RETURN.                              
                     
                     END.
                  END.
                  ELSE DO:                  
                     /*med beredskapsavtal måste full arbetstid vara reg innan övertid tillåts inom 30 minuter efter utreg*/
                     ograns = 0.
                     IF sok2 > 0 THEN DO:
                         ograns = sok2 / 100.
                        IF regslut > sok2 THEN ograns = regslut.
                        ograns =  klock60(klock100(ograns) + 0.5 ).                                                          
                        IF FILL-IN-START GE regslut AND FILL-IN-START LE ograns THEN DO:              
                           IF sok5 = 1 THEN DO:
                              MESSAGE "Full arbetstid måste vara registrerad innan övertid accepteras" VIEW-AS ALERT-BOX.                
                              status-mus2 = SESSION:SET-WAIT-STATE(""). 
                              RETURN.                              
                           
                           END.
                        END.
                     END.
                  END.
               END.
            END.
         END.
      END.
      
   END.
   IF Guru.Konstanter:globforetag = "tecm"  THEN DO:
      musz = FALSE.      
      RUN kommentaroblcheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Obligatorisk kommentar"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
   END.   
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:      
      IF FILL-IN-PRISTYP = "FRÅNVARO." THEN DO:      
         IF FILL-IN-START GE regslut OR FILL-IN-SLUT LE regstart 
         OR FILL-IN-SLUT > regslut OR FILL-IN-START < regstart THEN DO:
            MESSAGE "Övertid/Flex kan inte registreras på frånvaro " VIEW-AS ALERT-BOX.             
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
      musz = FALSE.
      RUN kommentarutbcheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Kommentar om vad utbildningen avser är obligatorisk."  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
      musz = FALSE.
      RUN fpbarncheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = ? THEN DO:
         /*ok utan personnummer 60 dagar nnnan barns födelse*/
         FILL-IN_RESMAL:FORMAT = "XXXXXX-XXXX".
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Barnets personnummer skall vara ifyllt ( format 999999-9999)." SKIP 
                    "Endast om det är frågan om föräldrapenning innan barnets födelse kan personnummer vara blankt." skip 
                    "Är det föräldrapenning före barnets födelse?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val8 AS LOGICAL.
            IF val8 = TRUE THEN DO:
               SUBSTRING(FILL-IN_RESMAL,12,50) = "Innan barns födelse".
            END.
            ELSE DO:
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.                         
         END.
         ELSE musz = TRUE.                  
      END.
      IF musz = TRUE THEN DO:         
         FILL-IN_RESMAL:FORMAT = "XXXXXX".         
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:                                    
            MESSAGE "Fyll i barnets personnummer format ÅÅMMDD"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.      
         IF INTEGER(SUBSTRING(FILL-IN_RESMAL,3,2)) > 12 OR INTEGER(SUBSTRING(FILL-IN_RESMAL,5,2)) > 31 THEN DO:
            MESSAGE "Felaktigt personnummer format skall vara ÅÅMMDD"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.               
         bpnr = DATE(SUBSTRING(FILL-IN_RESMAL,1,6)).
         balder = (( regdatum - bpnr ) / 365 ).
         IF bpnr < 01/01/2014 THEN DO:
            IF balder  GE 9 THEN DO:
               MESSAGE "Barnet är äldre än 8 år " Guru.Konstanter:gaok " 130 får inte användas."  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
            ELSE IF balder  > 8 AND balder  < 9 AND MONTH(bpnr) > 7 THEN.            
            ELSE IF balder  > 8 AND balder  < 9 AND MONTH(regdatum) > 7 THEN DO:
               MESSAGE "Barnet är äldre än 8 år " Guru.Konstanter:gaok " 130 får inte användas."  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
         END.
         ELSE DO:
            IF balder  > 12 THEN DO:
               MESSAGE "Barnet är äldre än 12 år " Guru.Konstanter:gaok " 130 får inte användas."  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
               APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
            END.
         END.      
      END.            
      musz = FALSE.
      RUN tvbarncheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = TRUE THEN DO:      
         FILL-IN_RESMAL:FORMAT = "XXXXXX".
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:            
            MESSAGE "Fyll i barnets personnummer format 999999"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         IF INTEGER(SUBSTRING(FILL-IN_RESMAL,3,2)) > 12 OR INTEGER(SUBSTRING(FILL-IN_RESMAL,5,2)) > 31 THEN DO:
            MESSAGE "Felaktigt personnummer format skall vara ÅÅMMDD"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.               
         bpnr = DATE(SUBSTRING(FILL-IN_RESMAL,1,6)).
         balder = (( regdatum - bpnr ) / 365 ).         
         IF balder  > 12 THEN DO:
            MESSAGE "Om barnet är äldre än 12 år krävs intyg för " Guru.Konstanter:gaok " 131. Finns intyg?"  
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val4 AS LOGICAL.
            IF val4 = TRUE THEN DO:
               SUBSTRING(FILL-IN_RESMAL,12,50) = "Intyg krävs".
            END.
            ELSE DO:
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
         END.
      END.                  
      musz = FALSE.
      RUN kommoblkom_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz, OUTPUT valdkom ).      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE valdkom VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
      musz = FALSE.
      RUN kommentaroblcheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Obligatorisk kommentar"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
   END.
   
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:      
      IF FILL-IN-OVER = "F" THEN musz = musz.
      ELSE DO:               
         IF regstart = 0 AND regslut = 24 AND FILL-IN-START < FILL-IN-SLUT THEN DO:
            /*skift - övertid på lunchen*/
            IF FILL-IN-START GE lunchstarten AND FILL-IN-SLUT LE lunchslutet THEN DO:
               IF FILL-IN_RESMAL = "" THEN DO:
                  MESSAGE "Kommentar är obligatorik vid övertid"  VIEW-AS ALERT-BOX. 
                  status-mus2 = SESSION:SET-WAIT-STATE("").
                  RETURN.
               END.
            END.
         END.
         ELSE IF regstart = 0 AND regslut = 24 AND FILL-IN-START > FILL-IN-SLUT THEN DO:
            /*skift - övertid på lunchen*/
            IF FILL-IN-START < lunchslutet AND FILL-IN-SLUT LE lunchslutet THEN DO:
               IF FILL-IN_RESMAL = "" THEN DO:
                  MESSAGE "Kommentar är obligatorik vid övertid"  VIEW-AS ALERT-BOX. 
                  status-mus2 = SESSION:SET-WAIT-STATE("").
                  RETURN.
               END.
            END.
         END.
         ELSE IF regslut = 24 AND FILL-IN-START > FILL-IN-SLUT THEN DO:
            /*skift - startdygn 22-24 */
            IF FILL-IN-START < regstart  THEN DO:
               IF FILL-IN_RESMAL = "" THEN DO:
                  MESSAGE "Kommentar är obligatorik vid övertid"  VIEW-AS ALERT-BOX. 
                  status-mus2 = SESSION:SET-WAIT-STATE("").
                  RETURN.
               END.
            END.
            ELSE IF skregstart = 0 AND skregslut = 24 THEN DO:            
               IF FILL-IN-SLUT > sklunchstarten AND FILL-IN-SLUT < sklunchslutet  THEN DO:
                  IF FILL-IN_RESMAL = "" THEN DO:
                     MESSAGE "Kommentar är obligatorik vid övertid"  VIEW-AS ALERT-BOX. 
                     status-mus2 = SESSION:SET-WAIT-STATE("").
                     RETURN.
                  END.
               END.
            END.
            ELSE IF skregstart = 0 AND skregslut > 0 THEN DO:            
               IF FILL-IN-SLUT > skregslut  THEN DO:
                  IF FILL-IN_RESMAL = "" THEN DO:
                     MESSAGE "Kommentar är obligatorik vid övertid"  VIEW-AS ALERT-BOX. 
                     status-mus2 = SESSION:SET-WAIT-STATE("").
                     RETURN.
                  END.
               END.   
            END.

         END.
         ELSE IF FILL-IN-START < regstart OR FILL-IN-SLUT > regslut OR FILL-IN-SLUT LE regstart THEN DO:
            
            IF FILL-IN-OVER = "I" THEN .
            ELSE IF FILL-IN_RESMAL = "" THEN DO:
               MESSAGE "Kommentar är obligatorik vid övertid"  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
         END.
         ELSE IF FILL-IN-START > FILL-IN-SLUT THEN DO:
            IF FILL-IN_RESMAL = "" THEN DO:
               MESSAGE "Kommentar är obligatorik vid övertid"  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
         END.
         
      END.      
   END.
   IF Guru.Konstanter:globforetag = "GRAN"   
   OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA"  THEN DO:
      IF FILL-IN-AONR = "05" OR FILL-IN-AONR = "150" THEN DO:
         nytid = FILL-IN-START.
         RUN TIMSEK.P.
         ASSIGN
         regstartsek = sekunder
         nytid = FILL-IN-SLUT.
         RUN TIMSEK.P.          
         ASSIGN
         regdatumspar = regdatum
         regslutsek = sekunder.        
         RUN TOTTIDW.P (INPUT pkod).
         IF regstart > 20 THEN musz = musz.
         ELSE IF regstart = 00 THEN musz = musz.
         ELSE IF nytid < regtotalt  THEN DO:
            MESSAGE "Semester kan bara registreras hela dagar " VIEW-AS ALERT-BOX.             
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.   
      END.    
   END.       
   IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA"  THEN DO:
      IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)   THEN .
      ELSE IF FILL-IN-AONR = "150" THEN DO:        
         IF regstart = 0 OR regslut = 24 THEN musz = musz.
         ELSE IF FILL-IN-START NE regstart OR FILL-IN-SLUT NE regslut THEN DO:         
            MESSAGE "Semester kan bara registreras hela dagar " VIEW-AS ALERT-BOX.             
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.   
      END.    
   END.       
   IF FILL-IN_VECKOVILA = TRUE THEN DO:
      IF FILL-IN-START GE regslut OR FILL-IN-SLUT LE regstart OR 
         FILL-IN-SLUT > regslut OR FILL-IN-START < regstart THEN DO:
         MESSAGE "Veckovila kan inte registreras utanför ordinarie arbetstid " VIEW-AS ALERT-BOX.             
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.
      END. 
      IF AVAILABLE utsokaonr THEN DO:
         IF utsokaonr.PRISTYP = "FRÅNVARO." THEN DO:
            MESSAGE "Veckovila kan inte registreras på frånvaro " VIEW-AS ALERT-BOX.             
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.      
      END.
   END. 
   IF personaltemp.OVERTIDUTTAG = "I" THEN DO:
      IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "CSNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
         /*snat behöver ingen stopp här. Kompsaldot styr om det kan ta ut komp istället. Ibland delar de ut komptimmar även till de som har inläst övertid tex 6 juni*/
         IF FILL-IN-AONR = "170" THEN DO:
            /*170 = kompledig*/
            MESSAGE "Personen har inte övertidersättning " VIEW-AS ALERT-BOX.             
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.     
         END.
      END.
      IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "elpa" THEN DO:         
         IF FILL-IN-AONR = "171" THEN DO:
            IF tillit = TRUE THEN.
            ELSE DO:
               /*171 = kompledig*/
               MESSAGE "Personen har inte övertidersättning " VIEW-AS ALERT-BOX.             
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.     
         END.
      END.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
         pcheck = FALSE.
         RUN pcheck_UI IN otbeordapph (INPUT personaltemp.PERSONALKOD,OUTPUT pcheck).
         FIND FIRST tidallt WHERE tidallt.PERSONALKOD = personaltemp.PERSONALKOD AND
         tidallt.DATUM = regdatum AND tidallt.BEREDSKAPSTART LE FILL-IN-SLUT AND
         tidallt.BEREDSKAPSLUT > FILL-IN-START NO-LOCK NO-ERROR. 
         IF AVAILABLE tidallt THEN DO:            
           /* PERSONALTAB.OVERTIUTTAG = "I" men övertid vid beredskap*/ 
         END.                 
         ELSE IF pcheck = TRUE THEN.
         ELSE IF FILL-IN-OVER = "I" OR FILL-IN-OVER = "F" THEN musz = musz.
         ELSE DO:
            MESSAGE "Personen har inte övertidersättning eller restidsersättning" VIEW-AS ALERT-BOX.             
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.     
         END.    
      END.
      ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
         pcheck = FALSE.
         RUN pcheck_UI IN otbeordapph (INPUT personaltemp.PERSONALKOD,OUTPUT pcheck).
         FIND FIRST tidallt WHERE tidallt.PERSONALKOD = personaltemp.PERSONALKOD AND
         tidallt.DATUM = regdatum AND tidallt.BEREDSKAPSTART LE FILL-IN-SLUT AND
         tidallt.BEREDSKAPSLUT > FILL-IN-START NO-LOCK NO-ERROR. 
         IF AVAILABLE tidallt THEN DO:            
           /* PERSONALTAB.OVERTIUTTAG = "I" men övertid vid beredskap*/ 
         END.          
         ELSE IF pcheck = TRUE THEN.    
         ELSE IF tillit = TRUE THEN.        
         ELSE IF FILL-IN-OVER = "I" OR FILL-IN-OVER = "F" THEN musz = musz.
         ELSE DO:
            MESSAGE "Personen har inte övertidersättning eller restidsersättning" VIEW-AS ALERT-BOX.             
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.     
         END.    
      END.
      ELSE IF  FILL-IN-OVER = "I" OR FILL-IN-OVER = "F" THEN musz = musz.
      ELSE DO:
         MESSAGE "Personen har inte övertidersättning eller restidsersättning" VIEW-AS ALERT-BOX.             
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.      
      END.    
   END.
   regdatum = regdatumspar.  
   IF FILL-IN-START > 24.00 THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.      
      status-mus2 = SESSION:SET-WAIT-STATE("").
      RETURN.
   END.      
   IF SUBSTRING(STRING(FILL-IN-START,"99.99"),4 ,2) > "59" THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.      
      status-mus2 = SESSION:SET-WAIT-STATE("").
      RETURN.   
   END.
   IF vart = "DEL" THEN DO:
      IF kontrollstart >= FILL-IN-START THEN DO:
         MESSAGE "Denna registrering kan inte börja före den du delar upp." VIEW-AS ALERT-BOX.         
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.
      END.
      IF mellanslutvar < FILL-IN-SLUT THEN DO:
         MESSAGE "Denna registrering kan inte sluta efter den du delar upp." VIEW-AS ALERT-BOX.         
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.
      END.
      IF FILL-IN-START GE FILL-IN-SLUT THEN DO:
         MESSAGE "Start kan inte vara större än slut." VIEW-AS ALERT-BOX.      
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.
      END.
   END.
   ELSE DO:         
      ASSIGN
      regstart = FILL-IN-START
      vart = "AND".
      RUN TIDSTARTW.P (INPUT pkod,INPUT extratidallt.RECTIDVIS).
      IF musz = TRUE THEN DO:
         musz = FALSE.
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.
      END.      
      IF FILL-IN-SLUT > 24.00 THEN DO:
         MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.         
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.
      END. 
      IF SUBSTRING(STRING(FILL-IN-SLUT,"99.99"),4 ,2) > "59" THEN DO:
         MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.         
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.
      END.    
      IF FILL-IN-SLUT = FILL-IN-START THEN DO:
         MESSAGE "Start och slut kan ej vara lika." VIEW-AS ALERT-BOX.         
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.
      END.
      IF FILL-IN-SLUT = 00.00 THEN DO:
         MESSAGE "Slut kan ej vara 00.00, ange 24.00 istället." VIEW-AS ALERT-BOX.         
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.
      END.
      IF Guru.Konstanter:globforetag = "elpa" OR Guru.Konstanter:globforetag = "lule" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL" THEN musz = musz.
      ELSE DO:   
         IF FILL-IN-START > FILL-IN-SLUT THEN DO:
            MESSAGE "Start kan inte vara större än slut." VIEW-AS ALERT-BOX.         
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.       
      IF FILL-IN-SLUT > FILL-IN-START  THEN DO:      
         ASSIGN
         regstart = FILL-IN-START
         regslut = FILL-IN-SLUT.        
         
         RUN TIDSLUTW.P (INPUT pkod,INPUT extratidallt.RECTIDVIS).
         IF musz = TRUE THEN DO:
            musz = FALSE.         
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
      ELSE DO:         
         ASSIGN
         regstart = FILL-IN-START
         regslut = 24.        
         RUN TIDSLUTW.P (INPUT pkod,INPUT extratidallt.RECTIDVIS).
         IF musz = TRUE THEN DO:
            musz = FALSE.         
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         IF MONTH(regdatum) = MONTH(regdatum + 1) THEN DO:         
            ASSIGN
            regdatum = regdatum + 1
            regstart = 00
            regslut = FILL-IN-SLUT.         
            RUN TIDSLUTW.P (INPUT pkod,INPUT extratidallt.RECTIDVIS).
            regdatum = regdatum - 1.
            IF musz = TRUE THEN DO:
               musz = FALSE.         
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
         END.
         ELSE DO:
            /*ny månad inga tialltar att kolla mot gör appad koll*/
            ASSIGN
            regdatum = regdatum + 1
            regstart = 00
            regslut = FILL-IN-SLUT.         
            ASSIGN
            sok1 = pkod
            sok4 = STRING(regdatum)      
            sok5 = regslut
            sok2 = 0.
            IF Guru.Konstanter:appcon THEN DO: 
               RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT 48,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
               INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
            END.
            ELSE DO:
               RUN FLEXTIDH.P 
               (INPUT 48,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
               INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
            END.
            IF sok2 = 1 THEN DO:
               MESSAGE "Det finns redan en registrering under denna tid" VIEW-AS ALERT-BOX.                
               status-mus2 = SESSION:SET-WAIT-STATE(""). 
               RETURN.                              
            END.
         
         END.
      END.
      
   END.
   IF Guru.Konstanter:globforetag = "GRAN"   OR 
      Guru.Konstanter:globforetag = "ELPA"  THEN DO:
      /*värme kan inte ha automatik på detta!*/
      IF ansttemp.KOD BEGINS "K" THEN DO:
         OPEN QUERY ttq FOR EACH tidallt WHERE 
         tidallt.PERSONAL = personaltemp.PERSONALKOD AND
         tidallt.DATUM = regdatum USE-INDEX PSTART NO-LOCK.
         GET FIRST ttq NO-LOCK.
         DO WHILE AVAILABLE(tidallt): 
            IF FILL-IN-START GE tidallt.BEREDSKAPSTART AND
            FILL-IN-START < tidallt.BEREDSKAPSLUT THEN DO: 
               ASSIGN FILL-IN-UTRYCK = TRUE.
               DISPLAY FILL-IN-UTRYCK WITH FRAME {&FRAME-NAME}.
            END.    
            GET NEXT ttq NO-LOCK.
         END.
      END.   
   END.   
  IF utryckningtemp.HALV = TRUE THEN DO:
      FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = personaltemp.PERSONALKOD 
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE flexavttemp AND flexavttemp.FLEXTID = TRUE AND CMB_OVERUT = "FLEX" THEN musz = musz.                         
      ELSE IF CMB_OVERUT = "EJÖV"  THEN musz = musz.
      ELSE DO:
         RUN REGVEC.P.
         {SLUTARBW.I}
         IF FILL-IN-START < regstart AND FILL-IN-SLUT > regslut THEN DO:
            ASSIGN nytid = FILL-IN-START.
            RUN TIMSEK.P.
            ASSIGN overant = sekunder
            nytid = regstart.
            RUN TIMSEK.P.
            ASSIGN overant = sekunder - overant
            nytid = FILL-IN-SLUT.
            RUN TIMSEK.P.
            ASSIGN seku = sekunder
            nytid = regslut.
            RUN TIMSEK.P.
            overant = overant + seku - sekunder.            
         END.
         ELSE DO: 
            IF FILL-IN-SLUT > regslut AND  FILL-IN-START < regslut THEN nytid = regslut.
            ELSE nytid = FILL-IN-START.
            RUN TIMSEK.P.
            overant = sekunder. 
            IF FILL-IN-START < regstart AND  FILL-IN-SLUT > regstart THEN nytid = regstart.
            ELSE nytid = FILL-IN-SLUT.
            RUN TIMSEK.P.     
            overant = sekunder - overant.    
         END.   
         halvkvart = 1800.         
         ASSIGN          
         otim = TRUNCATE(overant / halvkvart ,0)
         otim2 = overant - (otim * halvkvart).
         IF FILL-IN-UTRYCK = TRUE AND overant > utryckningtemp.UTRYCKNBER AND
         otim2 > 0 AND FILL-IN-SLUT > regslut THEN DO:
            MESSAGE "Endast jämna halvtimmar får registreras .5" VIEW-AS ALERT-BOX.           
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.   
         IF FILL-IN-UTRYCK = TRUE AND overant > utryckningtemp.UTRYCKNBER AND
         otim2 > 0 AND FILL-IN-START < regstart THEN DO:
            MESSAGE "Endast jämna halvtimmar får registreras.6" VIEW-AS ALERT-BOX.
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         IF FILL-IN-UTRYCK = FALSE AND otim2 > 0 AND FILL-IN-SLUT > regslut THEN DO:
            MESSAGE "Endast jämna halvtimmar får registreras.7" VIEW-AS ALERT-BOX.
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         IF FILL-IN-UTRYCK = FALSE AND otim2 > 0 AND FILL-IN-START < regstart THEN DO:
            MESSAGE "Endast jämna halvtimmar får registreras.8" VIEW-AS ALERT-BOX.
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.   
   END.
    
   IF Guru.Konstanter:globforetag = "snat" THEN DO:
      /*veckovila 36 TIM HELG  20180515 Ingrid Eriksson 
      1.       Den som registrerar övertid mellan kl. 19.00 lördag och kl. 04.00 söndag (grundschema 7-16)
               eller mellan kl. 20.00 lördag och 05.00 söndag (grundschema 8-17)ska få upp ett meddelande: ”Kom ihåg att du är berättigad till veckovila nästa fredag”
       2.       GURU ska då på samma sätt som när man registrerar beredskap förutsätta att nästkommande fredag är veckovila 
                så att om personen ändå registrerar tid den fredagen får upp påminnelsen om att även registrera löneart 190, Arbete under veckovila.            
      */
      vilach = FALSE.
      IF ansttemp.KOD BEGINS  "K" THEN DO:
         IF WEEKDAY(regdatum) = 7 AND FILL-IN-SLUT > 19 THEN vilach = TRUE.   
         IF WEEKDAY(regdatum) = 1 AND FILL-IN-START < 4 THEN vilach = TRUE.
      END.   
      IF ansttemp.KOD begins "T" THEN DO:
         IF WEEKDAY(regdatum) = 7 AND FILL-IN-SLUT > 20 THEN vilach = TRUE.   
         IF WEEKDAY(regdatum) = 1 AND FILL-IN-START < 5 THEN vilach = TRUE.
      END.   
      IF vilach = TRUE THEN DO:    
         MESSAGE "Kom ihåg att du är berättigad till veckovila nästa fredag" VIEW-AS ALERT-BOX.
            
      END.
   END.   
   /*PRISFOR*/
   IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
   END.
   ELSE DO:     
      IF AVAILABLE utsokaonr THEN DO:      
         ASSIGN
         sok1 = utsokaonr.AONR      
         sok2 = utsokaonr.DELNR
         sok4 = pkod.       
         RUN nyupp_UI (INPUT 16).      
         IF FILL-IN-PRISTYP = sok1 AND FILL-IN-PRIS = sok5 THEN musz = musz.
         ELSE DO:
            MESSAGE Guru.Konstanter:gdebk " och/eller priset är ändrat i"
               skip 
               "jämförelse mot " + LC(Guru.Konstanter:gaok) + " ! "
               skip
               LC(Guru.Konstanter:gaok) + " har            :" sok1 "och" sok5 
               skip
               "tidregistreringen har :" FILL-IN-PRISTYP " och" FILL-IN-PRIS
               skip
               "vill du ändra tillbaka priset till" 
               skip
               sok1 "och" sok5  "?"
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
            CASE val:
               WHEN TRUE THEN DO:
                  ASSIGN  CMB_PRISTYP:SCREEN-VALUE = FILL-IN-PRISTYP.             
                  status-mus2 = SESSION:SET-WAIT-STATE("").
                  RETURN.
               END.
               WHEN FALSE THEN DO:
                  FILL-IN-PRISTYP = FILL-IN-PRISTYP. 
                  FILL-IN-PRIS = FILL-IN-PRIS.
               END.
            END CASE. 
         END.
      END.
   END.
