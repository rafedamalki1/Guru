/*FLTRA2N.P FLERDYGNSTRAKTAMENTE */
/*DEFINE SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/
DEFINE SHARED VARIABLE regdatum LIKE TIDREGITAB.DATUM NO-UNDO.
DEFINE SHARED VARIABLE regvnr LIKE TIDREGITAB.VECKONUMMER NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID  NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE bdatum LIKE TIDREGITAB.DATUM NO-UNDO.
DEFINE SHARED VARIABLE avdatum LIKE TIDREGITAB.DATUM NO-UNDO.
DEFINE SHARED VARIABLE enflerdygns AS LOGICAL FORMAT "ENDAGS/FLERDYGNS" NO-UNDO.
DEFINE SHARED VARIABLE bilforare AS LOGICAL FORMAT "JA/NEJ" NO-UNDO.
DEFINE SHARED VARIABLE nattrakt AS LOGICAL FORMAT "JA/NEJ" NO-UNDO.
DEFINE SHARED VARIABLE resrec AS RECID NO-UNDO.  

DEFINE SHARED VARIABLE fostart AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE foslut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE restart AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE reslut AS DECIMAL NO-UNDO.
DEFINE VARIABLE hjrec AS RECID NO-UNDO.  
DEFINE VARIABLE skilln LIKE TIDREGITAB.START NO-UNDO.
DEFINE BUFFER tidbuff FOR TIDREGITAB.
&Scoped-define NEW
{RESDEF.I}
/*DEFINE SHARED TEMP-TABLE respers    
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD VECKONUMMER LIKE TIDREGITAB.VECKONUMMER
   FIELD DATUM LIKE TIDREGITAB.DATUM
   FIELD DAG LIKE TIDREGITAB.DAG
   FIELD START LIKE TIDREGITAB.START
   FIELD SLUT LIKE TIDREGITAB.SLUT 
   FIELD PRIS AS DECIMAL
   FIELD PRISTYP AS CHARACTER
   FIELD NATTRAKT AS LOGICAL FORMAT "JA/NEJ" LABEL "NATT TRAKT"
   FIELD OVERTIDUTTAG LIKE PERSONALTAB.OVERTIDUTTAG 
   FIELD BILFORARE LIKE TIDREGITAB.BILFORARE
   FIELD ENFLERDAGS LIKE TIDREGITAB.ENFLERDAGS  
   FIELD TIDREC AS RECID
   INDEX RESPERS IS PRIMARY DATUM START SLUT ASCENDING.*/
DEFINE SHARED VARIABLE reddatum LIKE TIDREGITAB.DATUM NO-UNDO.
DEFINE SHARED VARIABLE FILL-IN-REDTRAKT AS LOGICAL FORMAT "JA/NEJ":U INITIAL NO 
     LABEL "5. Reducerat traktamente                         ?" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1
       NO-UNDO.
DEFINE SHARED VARIABLE FILL-IN-3MAN AS LOGICAL FORMAT "JA/NEJ":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.
DEFINE SHARED VARIABLE FILL-IN-FRIMAT AS LOGICAL FORMAT "JA/NEJ":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
       NO-UNDO.      
FIND PERSONALTAB WHERE RECID(PERSONALTAB) = persrec NO-LOCK NO-ERROR.
FIND respers WHERE RECID(respers) = resrec NO-LOCK NO-ERROR.
IF foslut = TRUE THEN ASSIGN reslut = 24.
IF fostart = TRUE THEN ASSIGN restart = 00.
IF respers.DATUM = bdatum THEN DO:    
   IF fostart = TRUE AND bdatum = avdatum  THEN persrec = persrec.
   ELSE DO:   
      hjrec = RECID(respers).            
      IF FILL-IN-REDTRAKT = TRUE AND FILL-IN-3MAN = TRUE  AND  respers.DATUM GE reddatum THEN DO:
           /* 3 M�NADER l�ngtidsf�rr�ttning*/
          FIND FIRST TRAKTFLER WHERE TRAKTFLER.DAGTYP = "3MLANG" AND
          TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
          IF NOT AVAILABLE TRAKTFLER THEN DO:        
             FIND FIRST TRAKTFLER WHERE TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL AND
             TRAKTFLER.RESSTART LE restart AND
             TRAKTFLER.RESSTART2 GE restart AND  TRAKTFLER.DAGTYP = "UTRESA"
             USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
         END.  
      END.
      ELSE IF FILL-IN-REDTRAKT = TRUE AND FILL-IN-3MAN = TRUE
      AND  respers.DATUM < reddatum THEN DO:
          /* 3 M�NADER korttidsf�rr�ttning*/
          FIND FIRST TRAKTFLER WHERE TRAKTFLER.DAGTYP = "UTRESAR" AND
          TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL AND          
          TRAKTFLER.RESSTART LE restart AND TRAKTFLER.RESSTART2 GE restart 
          USE-INDEX FLTRAKT NO-LOCK NO-ERROR.   
      END.
      ELSE IF FILL-IN-REDTRAKT = TRUE AND  respers.DATUM GE reddatum THEN DO:
           /*l�ngtidsf�rr�ttning*/
          FIND FIRST TRAKTFLER WHERE TRAKTFLER.DAGTYP = "REDUCER" AND
          TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
          IF NOT AVAILABLE TRAKTFLER THEN DO:        
             FIND FIRST TRAKTFLER WHERE TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL AND
             TRAKTFLER.RESSTART LE restart AND
             TRAKTFLER.RESSTART2 GE restart AND  TRAKTFLER.DAGTYP = "UTRESA"
             USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
         END.  
      END.
      ELSE DO:
         IF FILL-IN-3MAN = TRUE THEN DO:
            /*3 M�NADER korttidsf�rr�ttning*/
            FIND FIRST TRAKTFLER WHERE TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL AND
            TRAKTFLER.RESSTART LE restart AND
            TRAKTFLER.RESSTART2 GE restart AND TRAKTFLER.DAGTYP = "UTRESAR"
            USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
         END.
         ELSE DO:
            FIND FIRST TRAKTFLER WHERE TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL AND
            TRAKTFLER.RESSTART LE restart AND
            TRAKTFLER.RESSTART2 GE restart AND TRAKTFLER.DAGTYP = "UTRESA"
            USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
         END.   
      END.   
      IF NOT AVAILABLE TRAKTFLER THEN persrec = persrec. 
      ELSE DO:  
         FIND respers WHERE RECID(respers) = hjrec NO-LOCK NO-ERROR.
         IF TRAKTFLER.TRAKTKOD NE '' THEN DO:
            CREATE TIDREGITAB.  
            ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
            SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "FLTRAKT2" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv            
            TIDREGITAB.DAG = respers.DAG
            TIDREGITAB.VECKONUMMER = respers.VECKONUMMER
            TIDREGITAB.START = 7.00
            TIDREGITAB.SLUT = 7.00
            TIDREGITAB.TIDLOG = FALSE
            TIDREGITAB.TRAKTAUTO = FALSE 
            TIDREGITAB.TRAKTKOD = TRAKTFLER.TRAKTKOD
            TIDREGITAB.TRAKTANTAL = TRAKTFLER.TRAKTANTAL 
            TIDREGITAB.AONR = respers.AONR
            TIDREGITAB.DELNR = respers.DELNR
            TIDREGITAB.BILFORARE = respers.BILFORARE
            TIDREGITAB.ENFLERDAGS = respers.ENFLERDAGS
            TIDREGITAB.DATUM = respers.DATUM.        
            FIND respers WHERE RECID(respers) = resrec NO-LOCK NO-ERROR.
         END.  
      END.
      IF respers.NATTRAKT = TRUE THEN DO:
         FIND FIRST TRAKTFLER WHERE TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL AND
         TRAKTFLER.DAGTYP = "NATT" AND TRAKTFLER.ARBTIDMIN = 0
         USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
         IF NOT AVAILABLE TRAKTFLER THEN persrec = persrec.
         ELSE DO:
            IF TRAKTFLER.TRAKTKOD NE '' THEN DO:
   	     CREATE TIDREGITAB.  
               ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
               SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "FLTRAKT2" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
               TIDREGITAB.DAG = respers.DAG
               TIDREGITAB.VECKONUMMER = respers.VECKONUMMER
               TIDREGITAB.START = 7.00
               TIDREGITAB.SLUT = 7.00
               TIDREGITAB.TIDLOG = FALSE
               TIDREGITAB.TRAKTAUTO = FALSE 
               TIDREGITAB.TRAKTKOD = TRAKTFLER.TRAKTKOD
               TIDREGITAB.TRAKTANTAL = TRAKTFLER.TRAKTANTAL 
               TIDREGITAB.AONR = respers.AONR
               TIDREGITAB.DELNR = respers.DELNR
               TIDREGITAB.BILFORARE = respers.BILFORARE
               TIDREGITAB.ENFLERDAGS = respers.ENFLERDAGS
               TIDREGITAB.DATUM = respers.DATUM.	    
            END. 
         END.
      END.
   END.
END.

IF respers.DATUM > bdatum AND respers.DATUM < avdatum THEN DO:
   /* MELLANDAGAR */   
   IF FILL-IN-REDTRAKT = TRUE AND FILL-IN-3MAN = TRUE AND  respers.DATUM GE reddatum THEN DO:
   /* 3 M�NADER l�ngtidsf�rr�ttning*/
      FIND FIRST TRAKTFLER WHERE TRAKTFLER.DAGTYP = "3MLANG" AND
      TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
      IF NOT AVAILABLE TRAKTFLER THEN DO:
         FIND FIRST TRAKTFLER WHERE TRAKTFLER.DAGTYP = "HELDAG" AND
         TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
      END.   
   END.  
   ELSE IF FILL-IN-REDTRAKT = TRUE AND FILL-IN-3MAN = TRUE
   AND  respers.DATUM < reddatum THEN DO:
      /* 3 M�NADER korttidsf�rr�ttning*/
      FIND FIRST TRAKTFLER WHERE TRAKTFLER.DAGTYP = "HELDAGR" AND
      TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
      IF NOT AVAILABLE TRAKTFLER THEN DO:
         FIND FIRST TRAKTFLER WHERE TRAKTFLER.DAGTYP = "HELDAG" AND
         TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
      END.   
   END.
   ELSE IF FILL-IN-REDTRAKT = TRUE AND FILL-IN-3MAN = FALSE
   AND  respers.DATUM GE reddatum THEN DO:
      /*l�ngtidsf�rr�ttning*/
      FIND FIRST TRAKTFLER WHERE TRAKTFLER.DAGTYP = "REDUCER" AND
      TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
      IF NOT AVAILABLE TRAKTFLER THEN DO:
         FIND FIRST TRAKTFLER WHERE TRAKTFLER.DAGTYP = "HELDAG" AND
         TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
      END.   
   END.      
   ELSE DO:
      IF FILL-IN-3MAN = TRUE THEN DO:
         FIND FIRST TRAKTFLER WHERE TRAKTFLER.DAGTYP = "HELDAGR" AND
         TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
      END.
      ELSE DO: 
         FIND FIRST TRAKTFLER WHERE TRAKTFLER.DAGTYP = "HELDAG" AND
         TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
      END.   
   END.   
   IF NOT AVAILABLE TRAKTFLER THEN persrec = persrec.
   ELSE DO:
      CREATE TIDREGITAB.     
      ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
      SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "FLTRAKT2" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
      TIDREGITAB.DAG = respers.DAG
      TIDREGITAB.VECKONUMMER = respers.VECKONUMMER
      TIDREGITAB.START = 7.00
      TIDREGITAB.SLUT = 7.00
      TIDREGITAB.TIDLOG = FALSE
      TIDREGITAB.TRAKTAUTO = FALSE 
      TIDREGITAB.TRAKTKOD = TRAKTFLER.TRAKTKOD
      TIDREGITAB.TRAKTANTAL = TRAKTFLER.TRAKTANTAL 
      TIDREGITAB.AONR = respers.AONR
      TIDREGITAB.DELNR = respers.DELNR
      TIDREGITAB.BILFORARE = respers.BILFORARE
      TIDREGITAB.ENFLERDAGS = respers.ENFLERDAGS
      TIDREGITAB.DATUM = respers.DATUM.      
   END.   
   IF respers.NATTRAKT = TRUE THEN DO:
      FIND FIRST TRAKTFLER WHERE TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL AND
      TRAKTFLER.DAGTYP = "NATT" AND TRAKTFLER.ARBTIDMIN = 0
      USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
      IF NOT AVAILABLE TRAKTFLER THEN persrec = persrec.
      ELSE DO:
         IF TRAKTFLER.TRAKTKOD NE '' THEN DO:
	    CREATE TIDREGITAB.
	    ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
            SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "FLTRAKT2" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
            TIDREGITAB.DAG = respers.DAG
            TIDREGITAB.VECKONUMMER = respers.VECKONUMMER
            TIDREGITAB.START = 7.00
            TIDREGITAB.SLUT = 7.00
            TIDREGITAB.TIDLOG = FALSE
            TIDREGITAB.TRAKTAUTO = FALSE 
            TIDREGITAB.TRAKTKOD = TRAKTFLER.TRAKTKOD
            TIDREGITAB.TRAKTANTAL = TRAKTFLER.TRAKTANTAL 
            TIDREGITAB.AONR = respers.AONR
            TIDREGITAB.DELNR = respers.DELNR
            TIDREGITAB.BILFORARE = respers.BILFORARE
            TIDREGITAB.ENFLERDAGS = respers.ENFLERDAGS
            TIDREGITAB.DATUM = respers.DATUM.	 	
	 END.
      END.
   END.
   IF respers.START < 5.00 AND respers.SLUT > respers.START THEN DO:
      skilln = respers.SLUT - respers.START.
      IF skilln > 1 THEN DO:
   	  FIND FIRST TRAKTFLER WHERE TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL AND
   	  TRAKTFLER.RESSTART LE respers.START AND
   	  TRAKTFLER.RESSTART2 GE respers.START AND
   	  TRAKTFLER.DAGTYP = "NATT" AND TRAKTFLER.ARBTIDMIN > 0
             USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
   	  IF NOT AVAILABLE TRAKTFLER THEN persrec = persrec.
   	  ELSE DO:
   	     IF TRAKTFLER.TRAKTKOD NE '' THEN DO:
   	        CREATE TIDREGITAB.
   	        ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
             SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "FLTRAKT2" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
             TIDREGITAB.DAG = respers.DAG
             TIDREGITAB.VECKONUMMER = respers.VECKONUMMER
             TIDREGITAB.START = 7.00
             TIDREGITAB.SLUT = 7.00
             TIDREGITAB.TIDLOG = FALSE
             TIDREGITAB.TRAKTAUTO = FALSE 
             TIDREGITAB.TRAKTKOD = TRAKTFLER.TRAKTKOD
             TIDREGITAB.TRAKTANTAL = TRAKTFLER.TRAKTANTAL 
             TIDREGITAB.AONR = respers.AONR
             TIDREGITAB.DELNR = respers.DELNR
             TIDREGITAB.BILFORARE = respers.BILFORARE
             TIDREGITAB.ENFLERDAGS = respers.ENFLERDAGS
             TIDREGITAB.DATUM = respers.DATUM. 		
   	     END.
    	   END.
      END.
   END.
END.
IF respers.DATUM = avdatum THEN DO:   
   IF foslut = TRUE AND bdatum = avdatum  THEN persrec = persrec.
   ELSE DO:         
      IF FILL-IN-REDTRAKT = TRUE AND FILL-IN-3MAN = TRUE AND   foslut = TRUE  THEN DO:
          /* 3 M�NADER l�ngtidsf�rr�ttning*/
          FIND FIRST TRAKTFLER WHERE TRAKTFLER.DAGTYP = "3MLANG" AND
          TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
          IF NOT AVAILABLE TRAKTFLER THEN DO: 
             FIND FIRST TRAKTFLER WHERE TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL AND            
             TRAKTFLER.RESSTART LE reslut AND
             TRAKTFLER.RESSTART2 GE reslut AND TRAKTFLER.DAGTYP = "HEMRESA"
             USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
         END.
      END.
      ELSE IF FILL-IN-REDTRAKT = TRUE AND FILL-IN-3MAN = FALSE AND 
      foslut = TRUE  THEN DO:
          /*l�ngtidsf�rr�ttning*/  
          FIND FIRST TRAKTFLER WHERE TRAKTFLER.DAGTYP = "REDUCER" AND
          TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
          IF NOT AVAILABLE TRAKTFLER THEN DO: 
             FIND FIRST TRAKTFLER WHERE TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL AND            
             TRAKTFLER.RESSTART LE reslut AND
             TRAKTFLER.RESSTART2 GE reslut AND TRAKTFLER.DAGTYP = "HEMRESA"
             USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
         END.
      END.
      ELSE IF FILL-IN-REDTRAKT = TRUE AND FILL-IN-3MAN = TRUE THEN DO:
         /*HEMRESA > 3 M�NADER INNAN 19?????????*/
         FIND FIRST TRAKTFLER WHERE TRAKTFLER.DAGTYP = "3MLANGH" AND
         TRAKTFLER.RESSTART LE reslut AND
         TRAKTFLER.RESSTART2 GE reslut AND         
         TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL USE-INDEX FLTRAKT NO-LOCK NO-ERROR.         
      END.
      ELSE IF FILL-IN-REDTRAKT = TRUE AND FILL-IN-3MAN = FALSE THEN DO:
         /*L�NGTIDSF�RR�TTNING*/
         FIND FIRST TRAKTFLER WHERE TRAKTFLER.DAGTYP = "REDHEM" AND
         TRAKTFLER.RESSTART LE reslut AND
         TRAKTFLER.RESSTART2 GE reslut AND         
         TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
         IF NOT AVAILABLE TRAKTFLER THEN DO: 
             FIND FIRST TRAKTFLER WHERE TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL AND
             TRAKTFLER.RESSTART LE respers.SLUT AND
             TRAKTFLER.RESSTART2 GE respers.SLUT AND TRAKTFLER.DAGTYP = "HEMRESA"
             USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
         END.
      END.
      ELSE DO:
         IF FILL-IN-3MAN = TRUE THEN DO:
            FIND FIRST TRAKTFLER WHERE TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL AND
            TRAKTFLER.RESSTART LE reslut AND
            TRAKTFLER.RESSTART2 GE reslut AND TRAKTFLER.DAGTYP = "HEMRESAR"
            USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
   
         END.
         ELSE DO: 
            FIND FIRST TRAKTFLER WHERE TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL AND
            TRAKTFLER.RESSTART LE reslut AND
            TRAKTFLER.RESSTART2 GE reslut AND TRAKTFLER.DAGTYP = "HEMRESA"
            USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
         END.   
      END.   
      IF NOT AVAILABLE TRAKTFLER THEN persrec = persrec.
      ELSE DO:
         IF TRAKTFLER.TRAKTKOD NE '' THEN DO:
            CREATE TIDREGITAB.
            ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
            SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "FLTRAKT2" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
            TIDREGITAB.DAG = respers.DAG
            TIDREGITAB.VECKONUMMER = respers.VECKONUMMER
            TIDREGITAB.START = 7.00
            TIDREGITAB.SLUT = 7.00
            TIDREGITAB.TIDLOG = FALSE
            TIDREGITAB.TRAKTAUTO = FALSE 
            TIDREGITAB.TRAKTKOD = TRAKTFLER.TRAKTKOD
            TIDREGITAB.TRAKTANTAL = TRAKTFLER.TRAKTANTAL 
            TIDREGITAB.AONR = respers.AONR
            TIDREGITAB.DELNR = respers.DELNR
            TIDREGITAB.BILFORARE = respers.BILFORARE
            TIDREGITAB.ENFLERDAGS = respers.ENFLERDAGS
            TIDREGITAB.DATUM = respers.DATUM.        
         END.
      END. 
      IF respers.NATTRAKT = TRUE  AND foslut = TRUE THEN DO:  
         FIND FIRST TRAKTFLER WHERE TRAKTFLER.TRAAVTAL = PERSONALTAB.TRAAVTAL AND
         TRAKTFLER.DAGTYP = "NATT" AND TRAKTFLER.ARBTIDMIN = 0
         USE-INDEX FLTRAKT NO-LOCK NO-ERROR.
         IF NOT AVAILABLE TRAKTFLER THEN persrec = persrec.
         ELSE DO:
            IF TRAKTFLER.TRAKTKOD NE '' THEN DO:
   	     CREATE TIDREGITAB.
   	     ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
               SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "FLTRAKT2" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
               TIDREGITAB.DAG = respers.DAG
               TIDREGITAB.VECKONUMMER = respers.VECKONUMMER
               TIDREGITAB.START = 7.00
               TIDREGITAB.SLUT = 7.00
               TIDREGITAB.TIDLOG = FALSE
               TIDREGITAB.TRAKTAUTO = FALSE 
               TIDREGITAB.TRAKTKOD = TRAKTFLER.TRAKTKOD
               TIDREGITAB.TRAKTANTAL = TRAKTFLER.TRAKTANTAL 
               TIDREGITAB.AONR = respers.AONR
               TIDREGITAB.DELNR = respers.DELNR
               TIDREGITAB.BILFORARE = respers.BILFORARE
               TIDREGITAB.ENFLERDAGS = respers.ENFLERDAGS
               TIDREGITAB.DATUM = respers.DATUM.	 		 
            END.
         END.
      END.
   END.
END.