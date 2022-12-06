
/*------------------------------------------------------------------------
    File        : k3
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Jan 04 16:25:20 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

PROCEDURE hmtafri_UI :
   DEFINE VARIABLE antalvar AS DECIMAL NO-UNDO.
   DEFINE VARIABLE kalkfil AS CHARACTER NO-UNDO.
   DEFINE VARIABLE minskaartal AS INTEGER NO-UNDO.
  DEBUGGER:SET-BREAK().
   FIND FIRST KALKYLKATALOG NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KALKYLKATALOG THEN DO:
      RETURN.
   END.
   /*
   FIND FIRST KALKHUV WHERE KALKHUV.TYPKALK = 6 NO-LOCK NO-ERROR.
   IF AVAILABLE KALKHUV THEN RETURN. 
   */
   FOR EACH EXTRADATA WHERE EXTRADATA.PROGRAM = "FRIKALK2" AND EXTRADATA.HUVUDINT = konvkalknr NO-LOCK:
      OUTPUT TO VALUE(filut) APPEND.
      PUT "NU STARTAR FRIKALKYL " EXTRADATA.HUVUDINT SKIP.
      OUTPUT CLOSE.
      EMPTY TEMP-TABLE kalkkostnad NO-ERROR.
      EMPTY TEMP-TABLE kalkantal NO-ERROR.
      EMPTY TEMP-TABLE efastkalktemp NO-ERROR. 
      EMPTY TEMP-TABLE tidutrubrik NO-ERROR. 
      /*OBS! globforetag = "GRAN" OCH ÅRTAL*/
      IF globforetag = "GRAN" THEN minskaartal = 2.
      ELSE minskaartal = 1.   
      FIND LAST KALKYLKATALOG WHERE KALKYLKATALOG.BENAMNING BEGINS "EBR" USE-INDEX VISARTAL NO-LOCK NO-ERROR.           
      FIND LAST KALKBEF WHERE KALKBEF.KALKNR = 0 NO-LOCK NO-ERROR.
      kalkfil = SESSION:TEMP-DIR + STRING(EXTRADATA.HUVUDINT) + ".txt".
      OUTPUT TO VALUE(kalkfil).
      FOR EACH KALKYL WHERE KALKYL.RECKALKYL = EXTRADATA.HUVUDINT AND KALKYL.TYP = "".
         PUT UNFORMATTED KALKYL.BEFATTNING KALKYL.OMRADE SKIP.
      END.
      OUTPUT CLOSE.
      INPUT FROM VALUE(kalkfil) NO-ECHO.
      REPEAT:
         CREATE efastkalktemp.
         ASSIGN.
         IMPORT DELIMITER "£" efastkalktemp NO-ERROR.                  
      END.
      INPUT CLOSE.
      FOR EACH efastkalktemp WHERE efastkalktemp.ARBKOD = "":
         DELETE efastkalktemp.
      END.
      OUTPUT TO VALUE(kalkfil).
      FOR EACH KALKYL WHERE KALKYL.RECKALKYL = EXTRADATA.HUVUDINT AND KALKYL.TYP NE "".
         PUT UNFORMATTED KALKYL.TYP SKIP.   
      END.
      OUTPUT CLOSE.
      INPUT FROM VALUE(kalkfil) NO-ECHO.
      REPEAT:
         CREATE tidutrubrik.
         ASSIGN.
         IMPORT DELIMITER "£" tidutrubrik   NO-ERROR.                  
      END.
      INPUT CLOSE.
      FOR EACH tidutrubrik WHERE tidutrubrik.ORDNING = 0:
         DELETE tidutrubrik.
      END.
      
      FIND FIRST KALKHUV  WHERE KALKHUV.KALKNR = EXTRADATA.HUVUDINT NO-LOCK NO-ERROR.
      IF AVAILABLE KALKHUV THEN DO:
         OUTPUT TO VALUE(filut) APPEND.
         PUT "fanns redan! " EXTRADATA.HUVUDINT SKIP.
         OUTPUT CLOSE.
         
      END.
      ELSE DO:
      
         FIND FIRST KALKSPEC WHERE KALKSPEC.KALKNR = EXTRADATA.HUVUDINT NO-LOCK NO-ERROR.
         DO TRANSACTION:
            CREATE KALKHUV.  
            ASSIGN
            KALKHUV.KALKNR = KALKSPEC.KALKNR                    
            KALKHUV.OMRADE = KALKSPEC.OMRADE                      
            KALKHUV.BENAMNING = KALKSPEC.KALKTEXT                   
            KALKHUV.KLOGID  = KALKYLKATALOG.KLOGID                    
            KALKHUV.TYPKALK  = 6                   
            /*
            KALKHUV.EGETMTRL  = FALSE                  
            KALKHUV.EGNAPRISER = FALSE                 
            KALKHUV.FAKTORER   = FALSE                 
            */
            KALKHUV.ANMARKNING  = KALKSPEC.STARTDAG                
            KALKHUV.BESTID = KALKSPEC.BESTID                      
            KALKHUV.KALKANV = KALKSPEC.KALKANV                    
            KALKHUV.ANVANDARE = KALKSPEC.ANVANDARE                  
            KALKHUV.AKTIV = KALKSPEC.AKTIV
            KALKHUV.UTYP  = 1.                            
         END.
         
         FIND FIRST efastkalktemp WHERE efastkalktemp.FAKTOR = TRUE NO-LOCK NO-ERROR.
         IF AVAILABLE efastkalktemp THEN DO:
            FOR EACH KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KALKYLKATALOG.HKLOGSUBID AND KALKYLPRISER.EGENKODUPP = TRUE NO-LOCK:
               CREATE KALKFAKTORER.  
               ASSIGN
               KALKFAKTORER.KLOGSUBID = KALKYLPRISER.KLOGSUBID
               KALKFAKTORER.BENAMNING = KALKYLPRISER.BENAMNING
               KALKFAKTORER.KALKNR = KALKHUV.KALKNR
               KALKFAKTORER.OMRADE = KALKHUV.OMRADE
               KALKFAKTORER.KPID = KALKYLPRISER.KPID.
               IF KALKYLPRISER.SOKBENAMNING = "BEREDARE" THEN KALKFAKTORER.FAKTOR = 1.
               ELSE IF KALKYLPRISER.SOKBENAMNING = "MONTÖR" THEN KALKFAKTORER.FAKTOR = efastkalktemp.ARBFAKTOR.
               ELSE IF KALKYLPRISER.SOKBENAMNING = "MASKIN1" THEN KALKFAKTORER.FAKTOR = efastkalktemp.MASKFAKTOR.
               ELSE IF KALKYLPRISER.SOKBENAMNING = "MASKIN2" THEN KALKFAKTORER.FAKTOR = 1.
               ELSE IF KALKYLPRISER.SOKBENAMNING = "MASKIN3" THEN KALKFAKTORER.FAKTOR = 1.
               ELSE IF KALKYLPRISER.SOKBENAMNING = "MASKIN4" THEN KALKFAKTORER.FAKTOR = 1.
               ELSE IF KALKYLPRISER.SOKBENAMNING = "MASKIN5" THEN KALKFAKTORER.FAKTOR = 1.
               ELSE IF KALKYLPRISER.SOKBENAMNING = "MATERIEL" THEN KALKFAKTORER.FAKTOR = efastkalktemp.MTRLFAKTOR.
               ELSE IF KALKYLPRISER.SOKBENAMNING = "ÖVRIGKOSTNAD" THEN KALKFAKTORER.FAKTOR = efastkalktemp.OVRFAKTOR.
               ELSE IF KALKYLPRISER.SOKBENAMNING = "ÖVRIGKOSTNADEA" THEN KALKFAKTORER.FAKTOR = efastkalktemp.UTRFAKTOR.
               ELSE IF KALKYLPRISER.SOKBENAMNING = "UTRUSTNING" THEN KALKFAKTORER.FAKTOR =  efastkalktemp.UTRFAKTOR.              
            END.
         END.
         
         FIND LAST KALKBEF WHERE KALKBEF.KALKNR = 0 NO-LOCK NO-ERROR.
         IF AVAILABLE KALKBEF THEN DO:    
            FOR EACH KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KALKYLKATALOG.HKLOGSUBID AND KALKYLPRISER.EGENPRISUPP = TRUE NO-LOCK:
               CREATE KALKEGNAPRISER.  
               BUFFER-COPY KALKYLPRISER TO KALKEGNAPRISER. 
               ASSIGN
               KALKEGNAPRISER.KLOGSUBID = KALKYLPRISER.KLOGSUBID
               KALKEGNAPRISER.KALKNR = KALKHUV.KALKNR
               KALKEGNAPRISER.OMRADE = KALKHUV.OMRADE.       
               IF KALKYLPRISER.SOKBENAMNING = "BEREDARE" THEN KALKEGNAPRISER.PRIS = KALKBEF.PRIS1.
               ELSE IF KALKYLPRISER.SOKBENAMNING = "MONTÖR" THEN KALKEGNAPRISER.PRIS = KALKBEF.PRIS2.
               ELSE IF KALKYLPRISER.SOKBENAMNING = "MASKIN1" THEN KALKEGNAPRISER.PRIS = KALKBEF.PRIS3.
               ELSE IF KALKYLPRISER.SOKBENAMNING = "MASKIN2" THEN KALKEGNAPRISER.PRIS = KALKBEF.PRIS4.
               ELSE IF KALKYLPRISER.SOKBENAMNING = "MASKIN3" THEN KALKEGNAPRISER.PRIS = KALKBEF.PRIS7.
               ELSE IF KALKYLPRISER.SOKBENAMNING = "MASKIN4" THEN KALKEGNAPRISER.PRIS = KALKBEF.PRIS5.
               ELSE IF KALKYLPRISER.SOKBENAMNING = "MASKIN5" THEN KALKEGNAPRISER.PRIS = KALKBEF.PRIS6.           
               /*UTRUSTNING FAKTOR.FAKMASKINKOST FAKTOR.FAKUTRUSTKOST ????*/           
            END.   
         END.
         
         FOR EACH efastkalktemp NO-LOCK: 
            EMPTY TEMP-TABLE kalknumsubtt NO-ERROR.
            DO TRANSACTION:   
               CREATE KALKNUM.
               BUFFER-COPY efastkalktemp TO KALKNUM.
               KALKNUM.BENAMNING = "".
               ASSIGN
               KALKNUM.BENAMNING = SUBSTRING(efastkalktemp.BENAMNING,1,40)
               KALKNUM.ANMARK = SUBSTRING(efastkalktemp.BENAMNING,50)
               KALKNUM.KLOGSUBID = KALKYLKATALOG.HKLOGSUBID           
               KALKNUM.KALKNR = KALKHUV.KALKNR
               KALKNUM.OMRADE = KALKHUV.OMRADE
               KALKNUM.MATRIS = efastkalktemp.F10   
               KALKNUM.TYPKALK = 6.
               IF KALKNUM.MATRIS = 0 THEN KALKNUM.MATRIS = 1.
               RUN sistanum_UI (OUTPUT KALKNUM.NUM).
            END. 
            FOR EACH KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KALKYLKATALOG.HKLOGSUBID AND KALKYLPRISER.EGENKODUPP = TRUE NO-LOCK:
               
               CREATE KALKNUMSUB.            
               BUFFER-COPY KALKYLPRISER TO KALKNUMSUB. 
               ASSIGN 
               KALKNUMSUB.KALKNR = KALKNUM.KALKNR
               KALKNUMSUB.OMRADE = KALKHUV.OMRADE
               KALKNUMSUB.NUM = KALKNUM.NUM.
               RUN sistanumsubid_UI (OUTPUT KALKNUMSUB.NUMSUBID).
               IF KALKNUM.ANTAL = 0 THEN antalvar = 1.           
               ELSE antalvar = KALKNUM.ANTAL.
               IF KALKYLPRISER.SOKBENAMNING = "BEREDARE" THEN DO:
                  IF efastkalktemp.F1 + efastkalktemp.F2 = 0 THEN.
                  ELSE DO:
                     ASSIGN
                     KALKNUMSUB.FRITIMMAR = efastkalktemp.F1 / antalvar
                     KALKNUMSUB.FRIPRIS = efastkalktemp.ARBETE /(efastkalktemp.F1 + efastkalktemp.F2).
                  END.   
               END.   
               ELSE IF KALKYLPRISER.SOKBENAMNING = "MONTÖR" THEN DO:
                  IF efastkalktemp.F1 + efastkalktemp.F2 = 0 THEN.
                  ELSE DO:
                     ASSIGN
                     KALKNUMSUB.FRITIMMAR = efastkalktemp.F2 / antalvar
                     KALKNUMSUB.FRIPRIS = efastkalktemp.ARBETE /(efastkalktemp.F1 + efastkalktemp.F2).
                  END.   
               END.   
               ELSE IF KALKYLPRISER.SOKBENAMNING = "MASKIN1" THEN DO:
                  IF efastkalktemp.MASKINTIMMAR = 0 THEN.
                  ELSE DO: 
                     ASSIGN
                     KALKNUMSUB.FRITIMMAR = efastkalktemp.MASKINTIMMAR / antalvar
                     KALKNUMSUB.FRIPRIS = efastkalktemp.MASKINKOST /(efastkalktemp.MASKINTIMMAR).
                  END.   
               END.  
                  
               ELSE IF KALKYLPRISER.SOKBENAMNING = "MATERIEL" THEN DO: 
                  ASSIGN
                  KALKNUMSUB.FRIKOSTNAD  = efastkalktemp.MATERIEL / antalvar.                  
               END.   
               ELSE IF KALKYLPRISER.SOKBENAMNING = "ÖVRIGKOSTNAD" THEN DO:
                  ASSIGN
                  KALKNUMSUB.FRIKOSTNAD = efastkalktemp.OVRIGT / antalvar.             
               END.
               ELSE IF KALKYLPRISER.SOKBENAMNING = "ÖVRIGKOSTNADEA" THEN DO:
                  IF efastkalktemp.UTRUST = 0 THEN DO:  
                     ASSIGN 
                     KALKNUMSUB.FRIKOSTNAD = efastkalktemp.UTRUSTKOST / antalvar.
                  END.
                  ELSE DO:
                     ASSIGN                    
                     KALKNUMSUB.FRITIMMAR = efastkalktemp.UTRUST / antalvar.
                     KALKNUMSUB.FRIPRIS = efastkalktemp.UTRUSTKOST /(efastkalktemp.UTRUST * antalvar).
                  END.    
                     
               END. 
               ELSE IF KALKYLPRISER.SOKBENAMNING = "UTRUSTNING" THEN DO:
                  IF efastkalktemp.UTRUST = 0 THEN DO:
                     ASSIGN 
                     KALKNUMSUB.FRIKOSTNAD = efastkalktemp.UTRUSTKOST / antalvar.
                  END.
                  ELSE DO:
                     ASSIGN                    
                     KALKNUMSUB.FRITIMMAR = efastkalktemp.UTRUST / antalvar.
                     KALKNUMSUB.FRIPRIS = efastkalktemp.UTRUSTKOST /(efastkalktemp.UTRUST * antalvar).
                  END.                         
               END.
               IF KALKNUMSUB.FRIPRIS = ? OR KALKNUMSUB.FRITIMMAR = ? THEN 
               MESSAGE "STOPP"
               VIEW-AS ALERT-BOX.
               IF efastkalktemp.ARBKOD = "EGEN" THEN DO:
                  ASSIGN 
                  KALKNUMSUB.FRIBENAMNING = KALKNUMSUB.BENAMNING         
                  KALKNUMSUB.KOSTNAD   = KALKNUMSUB.FRIKOSTNAD
                  KALKNUMSUB.PRIS      = KALKNUMSUB.FRIPRIS
                  KALKNUMSUB.TIMMAR    = KALKNUMSUB.FRITIMMAR.
                  CREATE kalknumsubtt.  
                  BUFFER-COPY KALKNUMSUB TO kalknumsubtt.
               END.
               ELSE DO:
                  ASSIGN 
                  KALKNUMSUB.FRIBENAMNING = KALKNUMSUB.BENAMNING.
                  FOR EACH KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGID = KALKHUV.KLOGID NO-LOCK:
                     FIND FIRST KALKYLLOPPOSTER WHERE KALKYLLOPPOSTER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND
                      KALKYLLOPPOSTER.ARBKOD = KALKNUM.ARBKOD AND KALKYLLOPPOSTER.LOPNR = KALKNUM.LOPNR NO-LOCK NO-ERROR.
                      IF AVAILABLE KALKYLLOPPOSTER THEN DO:
                         kpostid = ROWID(KALKYLLOPPOSTER) .
                      END.   
                   END.
                   FIND FIRST KALKYLLOPPOSTER WHERE ROWID(KALKYLLOPPOSTER) = kpostid NO-LOCK NO-ERROR.
                   BUFFER-COPY KALKYLLOPPOSTER TO KALKNUM.
                   FOR EACH KALKYLLOPSUB WHERE KALKYLLOPSUB.KLOGSUBID = KALKYLLOPPOSTER.KLOGSUBID AND 
                   KALKYLLOPSUB.ARBKOD = KALKNUM.ARBKOD AND KALKYLLOPSUB.LOPNR = KALKNUM.LOPNR AND  
                   KALKYLLOPSUB.KPID = KALKYLPRISER.KPID  NO-LOCK:                   
                     BUFFER-COPY KALKYLLOPSUB TO KALKNUMSUB.      
                     ASSIGN 
                     KALKNUMSUB.KALKNR = KALKNUM.KALKNR
                     KALKNUMSUB.OMRADE = KALKHUV.OMRADE
                     KALKNUMSUB.NUM = KALKNUM.NUM.
                     CREATE kalknumsubtt.  
                     BUFFER-COPY KALKNUMSUB TO kalknumsubtt.   
                  END.              
               END.  
            END.                  
            
            EMPTY TEMP-TABLE kalknumtt NO-ERROR.
            CREATE kalknumtt.
            BUFFER-COPY KALKNUM TO kalknumtt.
            kalknumtt.TTRECID = RECID(kalknumtt).          
            RUN raknaenkod_UI (INPUT FALSE).
            FOR EACH kalknumsubtt:
               FIND FIRST KALKNUMSUB WHERE KALKNUMSUB.KALKNR = kalknumsubtt.KALKNR AND
               KALKNUMSUB.OMRADE = kalknumsubtt.OMRADE AND
               KALKNUMSUB.NUM = kalknumsubtt.NUM AND
               KALKNUMSUB.KPID = kalknumsubtt.KPID 
               EXCLUSIVE-LOCK NO-ERROR.
               BUFFER-COPY kalknumsubtt TO KALKNUMSUB.
            END.
         
            DO TRANSACTION:
               FIND CURRENT KALKNUM EXCLUSIVE-LOCK. 
               BUFFER-COPY kalknumtt TO KALKNUM.
            END.
            
            RELEASE KALKNUM NO-ERROR.
            EMPTY TEMP-TABLE kalknumtt NO-ERROR. 
            EMPTY TEMP-TABLE kalknumsubtt NO-ERROR.
         END.
      END.            
   END.         
END PROCEDURE.