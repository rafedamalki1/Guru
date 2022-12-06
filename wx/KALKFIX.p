
/*------------------------------------------------------------------------
    File        : KALKFIX.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed May 13 12:56:08 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{KALKYLKAT.I}
DEFINE TEMP-TABLE ktt NO-UNDO
   FIELD KALKNR AS INTEGER
   FIELD ARBKOD AS CHARACTER
   FIELD LOPNR AS INTEGER
   INDEX KK KALKNR ARBKOD LOPNR.
DEFINE TEMP-TABLE ktt2 NO-UNDO  
   FIELD KALKNR AS INTEGER
   INDEX KK KALKNR. 
DEFINE TEMP-TABLE ktt3 NO-UNDO
   FIELD ARBKOD AS CHARACTER
   FIELD LOPNR AS INTEGER
   INDEX KK ARBKOD LOPNR.

FIND FIRST KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID =  15 AND  KALKYLPRISER.KPID = 13 NO-LOCK NO-ERROR.
FOR EACH KALKYLLOPSUB WHERE KALKYLLOPSUB.KLOGSUBID = KALKYLPRISER.KLOGSUBID AND KALKYLLOPSUB.KPID = KALKYLPRISER.KPID  NO-LOCK:
   IF KALKYLLOPSUB.TIMMAR = 0 THEN .
   ELSE DO:
      FIND FIRST ktt3 WHERE ktt3.ARBKOD = KALKYLLOPSUB.ARBKOD AND ktt3.LOPNR = KALKYLLOPSUB.LOPNR NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE ktt3 THEN DO:
         CREATE ktt3.
         ASSIGN 
         
         ktt3.ARBKOD = KALKYLLOPSUB.ARBKOD 
         ktt3.LOPNR  = KALKYLLOPSUB.LOPNR.
      END.   
   END.   
END.
OUTPUT TO c:\temp\rejkalk3.txt.
FOR EACH ktt3 WHERE NO-LOCK:
   PUT UNFORMATTED " arbkod " ktt3.ARBKOD " lopnr " ktt3.LOPNR SKIP.
END.
OUTPUT CLOSE.  
      
FOR EACH KALKHUV WHERE KALKHUV.KLOGID = 9 NO-LOCK:
   

   FOR EACH kalknum WHERE KALKNUM.KALKNR = KALKHUV.KALKNR AND KALKNUM.KLOGSUBID = 15 NO-LOCK:
      FIND FIRST KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID =  KALKNUM.KLOGSUBID AND  KALKYLPRISER.KPID = 13 NO-LOCK NO-ERROR.
      FOR EACH KALKNUMSUB WHERE KALKNUMSUB.KALKNR = KALKNUM.KALKNR AND KALKNUMSUB.NUM = KALKNUM.NUM AND KALKNUMSUB.KPID = KALKYLPRISER.KPID EXCLUSIVE-LOCK:
         IF KALKNUMSUB.TIMMAR = 0 THEN .
         ELSE DO:
            KALKNUMSUB.PRIS = KALKYLPRISER.PRIS.
            KALKNUMSUB.FRIPRIS = KALKYLPRISER.PRIS.
            FIND FIRST ktt WHERE ktt.KALKNR = KALKHUV.KALKNR AND ktt.ARBKOD = KALKNUM.ARBKOD AND ktt.LOPNR = KALKNUM.LOPNR NO-LOCK NO-ERROR. 
            IF NOT AVAILABLE ktt THEN DO:
                CREATE ktt.
                ASSIGN 
                ktt.KALKNR = KALKHUV.KALKNR 
                ktt.ARBKOD = KALKNUM.ARBKOD 
                ktt.LOPNR = KALKNUM.LOPNR.
            END.
            FIND FIRST ktt2 WHERE ktt2.KALKNR = KALKHUV.KALKNR NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ktt2 THEN DO:
                CREATE ktt2.
                ASSIGN 
                ktt2.KALKNR = KALKHUV.KALKNR. 
            END.
         END.   
      END.
   END.   
END.
OUTPUT TO c:\temp\rejkalk.txt.
FOR EACH ktt WHERE NO-LOCK:
   PUT UNFORMATTED "kalknr " ktt.KALKNR  " arbkod " ktt.ARBKOD " lopnr " ktt.LOPNR SKIP.
END.
OUTPUT CLOSE. 
OUTPUT TO c:\temp\rejkalk2.txt.
FOR EACH ktt2 WHERE NO-LOCK:
   PUT UNFORMATTED "kalknr " ktt2.KALKNR   SKIP.
END.
OUTPUT CLOSE.             

FOR EACH ktt2 WHERE NO-LOCK:
   FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = ktt2.KALKNR NO-LOCK NO-ERROR.
   FOR EACH KALKNUM WHERE KALKNUM.KALKNR = ktt2.KALKNR EXCLUSIVE-LOCK:
      EMPTY TEMP-TABLE kalknumtt NO-ERROR.
      CREATE kalknumtt.
      BUFFER-COPY KALKNUM TO kalknumtt.
      kalknumtt.TTRECID = RECID(kalknumtt).   
      FOR EACH KALKNUMSUB WHERE KALKNUMSUB.KALKNR = kalknumtt.KALKNR AND KALKNUMSUB.OMRADE = kalknumtt.OMRADE AND KALKNUMSUB.NUM = kalknumtt.NUM NO-LOCK:
         CREATE kalknumsubtt.  
         BUFFER-COPY KALKNUMSUB TO kalknumsubtt.  
      END.       
      RUN raknaenkod_UI (INPUT FALSE).
      FOR EACH kalknumsubtt:
         FIND FIRST KALKNUMSUB WHERE KALKNUMSUB.KALKNR = kalknumsubtt.KALKNR AND
         KALKNUMSUB.OMRADE = kalknumsubtt.OMRADE AND
         KALKNUMSUB.NUM = kalknumsubtt.NUM AND
         KALKNUMSUB.KPID = kalknumsubtt.KPID 
         EXCLUSIVE-LOCK NO-ERROR.
         BUFFER-COPY kalknumsubtt TO KALKNUMSUB.
      END.
      BUFFER-COPY kalknumtt TO KALKNUM.
      EMPTY TEMP-TABLE kalknumtt NO-ERROR. 
      EMPTY TEMP-TABLE kalknumsubtt NO-ERROR. 
   END.
END.   
/*beräknar kostnad för en kod ELLER ALLA KODER*/ 
PROCEDURE raknaenkod_UI :
   DEFINE INPUT  PARAMETER allakoder AS LOGICAL NO-UNDO.
   DEFINE VARIABLE totkostvar AS DECIMAL NO-UNDO.
   DEFINE VARIABLE fritotkostvar AS DECIMAL NO-UNDO.
   EMPTY TEMP-TABLE kalkantal NO-ERROR. 
   EMPTY TEMP-TABLE kalkkostnad NO-ERROR. 
   /*SAMBYGG EGNA PRISER FAKTORER */
   
   IF allakoder = FALSE THEN DO:
      OPEN QUERY ksq FOR EACH kalknumsubtt WHERE kalknumsubtt.KALKNR = kalknumtt.KALKNR AND kalknumsubtt.OMRADE = KALKHUV.OMRADE AND 
      kalknumsubtt.NUM = kalknumtt.NUM NO-LOCK.
      GET FIRST ksq NO-LOCK.
     
   END.
   ELSE DO:
      OPEN QUERY ksq FOR EACH kalknumsubtt WHERE kalknumsubtt.KALKNR = KALKHUV.KALKNR AND kalknumsubtt.OMRADE = KALKHUV.OMRADE NO-LOCK.
      GET FIRST ksq NO-LOCK.
   END.    
   DO WHILE AVAILABLE(kalknumsubtt):
      
      IF allakoder = TRUE THEN DO:
         FIND FIRST KALKNUM WHERE KALKNUM.KALKNR = KALKHUV.KALKNR AND KALKNUM.OMRADE = KALKHUV.OMRADE AND KALKNUM.NUM = kalknumsubtt.NUM NO-LOCK NO-ERROR.
         FIND FIRST kalknumtt WHERE kalknumtt.KALKNR = KALKHUV.KALKNR AND kalknumtt.OMRADE = KALKHUV.OMRADE AND kalknumtt.NUM = kalknumsubtt.NUM NO-ERROR.
         IF NOT AVAILABLE kalknumtt THEN DO:
            CREATE kalknumtt.
            BUFFER-COPY KALKNUM TO kalknumtt.
            kalknumtt.TTRECID = RECID(kalknumtt).
         END.         
      END.
      CREATE kalkantal.
      BUFFER-COPY kalknumtt TO kalkantal.
      CREATE kalkkostnad.
      BUFFER-COPY kalknumtt TO kalkkostnad.
      kalkkostnad.TOTKOST = 0.
      FIND FIRST KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = kalknumtt.KLOGSUBID AND 
      KALKYLPRISER.KPID = kalknumsubtt.KPID NO-LOCK NO-ERROR.
      FIND FIRST KALKVISNING WHERE KALKVISNING.KVID = KALKYLPRISER.KVID NO-LOCK NO-ERROR.
      /*)
      EMPTY TEMP-TABLE kalkEA NO-ERROR.
      CREATE kalkEA.
      */        
      IF AVAILABLE KALKVISNING THEN DO:   
         
         ASSIGN 
         kalkkostnad.KOSTTYP = KALKVISNING.KOSTTYP
         kalkkostnad.KPID = KALKYLPRISER.KPID
         kalkkostnad.KVID = KALKYLPRISER.KVID.
          
         kalkkostnad.TOTKOST = kalkkostnad.TOTKOST + (kalknumsubtt.TIMMAR * kalknumtt.ANTAL * KALKYLPRISER.PRIS)
         + kalknumtt.ANTAL * kalknumsubtt.KOSTNAD.
         kalkkostnad.FRITOTKOST = kalkkostnad.FRITOTKOST + (kalknumsubtt.FRITIMMAR * kalknumtt.ANTAL * kalknumsubtt.FRIPRIS)
         + kalknumtt.ANTAL * kalknumsubtt.FRIKOSTNAD.
         
         totkostvar = totkostvar + kalkkostnad.TOTKOST.
         fritotkostvar = fritotkostvar + kalkkostnad.FRITOTKOST.
         ASSIGN 
         kalkantal.TIMTYP = KALKVISNING.TIMTYP
         kalkantal.KPID = KALKYLPRISER.KPID
         kalkantal.KVID = KALKYLPRISER.KVID.
          
         kalkantal.SUMMA = kalkantal.SUMMA + kalknumsubtt.TIMMAR * kalknumtt.ANTAL.
         kalkantal.FRISUMMA = kalkantal.FRISUMMA + kalknumsubtt.FRITIMMAR * kalknumtt.ANTAL.
                         
      END.
      FIND FIRST KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = kalknumtt.KLOGSUBID AND KALKYLPRISER.KVID = 0 AND KALKYLPRISER.SOKBENAMNING = "RÖRLIGKOSTNAD EA"  NO-LOCK NO-ERROR.
      IF AVAILABLE KALKYLPRISER THEN DO:
         
         IF kalkantal.KVID = 2 THEN DO:
            kalkkostnad.EAMANGD = kalkantal.SUMMA.
         END.
         IF kalkantal.KVID = 3 THEN DO:
            kalkkostnad.EAMANGD = kalkkostnad.TOTKOST / KALKYLPRISER.PRIS.
         END.
         IF kalkantal.KVID = 6 THEN DO:
            kalkkostnad.EAMANGD = kalkkostnad.TOTKOST / KALKYLPRISER.PRIS.
         END.
         IF kalkantal.KVID = 7 THEN DO:
            kalkkostnad.EAMANGD = kalkkostnad.TOTKOST / KALKYLPRISER.PRIS.
         END.                   
      END.
      ELSE DO:
         ASSIGN
         kalkkostnad.EAMANGD  = 0
         kalkkostnad.FRIEAMANGD = 0.        
      END.
      
      GET NEXT ksq NO-LOCK.
   END.            
   IF allakoder = FALSE THEN DO:
      ASSIGN
      kalknumtt.FRITOTKOST = fritotkostvar
      kalknumtt.TOTKOST = totkostvar.   
      EMPTY TEMP-TABLE kalkantal NO-ERROR.
      EMPTY TEMP-TABLE kalkkostnad NO-ERROR.       
   END.    
END PROCEDURE.  
