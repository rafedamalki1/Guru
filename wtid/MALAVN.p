/*MALAVN.P MALTIDSAVDRAG */
DEFINE SHARED VARIABLE regdatum LIKE TIDREGITAB.DATUM NO-UNDO.
DEFINE SHARED VARIABLE regvnr LIKE TIDREGITAB.VECKONUMMER NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID  NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE bdatum LIKE TIDREGITAB.DATUM NO-UNDO.
DEFINE SHARED VARIABLE avdatum LIKE TIDREGITAB.DATUM NO-UNDO.

DEFINE SHARED VARIABLE enflerdygns AS LOGICAL FORMAT "ENDAGS/FLERDYGNS" NO-UNDO.
DEFINE SHARED VARIABLE bilforare AS LOGICAL FORMAT "JA/NEJ" NO-UNDO.
DEFINE SHARED VARIABLE nattrakt AS LOGICAL FORMAT "JA/NEJ" NO-UNDO.

/*DEFINE SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/
DEFINE SHARED VARIABLE fostart AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE foslut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE restart AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE reslut AS DECIMAL NO-UNDO.
DEFINE VARIABLE koll1 AS INTEGER NO-UNDO.
DEFINE VARIABLE mallart AS CHARACTER NO-UNDO.
DEFINE VARIABLE ejtolk AS LOGICAL NO-UNDO.
DEFINE VARIABLE ejtolkfr AS LOGICAL NO-UNDO.
&Scoped-define NEW
{RESDEF.I}
/*DEFINE SHARED TEMP-TABLE maltidfil
   FIELD MPERSONALKOD LIKE MALTIDTAB.PERSONALKOD
   FIELD MDAG LIKE MALTIDTAB.DAG
   FIELD MVECKONUMMER LIKE MALTIDTAB.VECKONUMMER
   FIELD MDATUM LIKE MALTIDTAB.DATUM
   FIELD MFRU LIKE MALTIDTAB.FRU
   FIELD MLUN LIKE MALTIDTAB.FRU
   FIELD MMID LIKE MALTIDTAB.FRU.*/    

DEFINE SHARED VARIABLE FILL-IN-FRIMAT AS LOGICAL FORMAT "JA/NEJ":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
       NO-UNDO.
DEFINE SHARED VARIABLE FILL-IN-3MAN AS LOGICAL FORMAT "JA/NEJ":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

       
DEFINE BUFFER tidbuff FOR TIDREGITAB.
DEFINE BUFFER tidbuff2 FOR TIDREGITAB.
DEFINE BUFFER mtab FOR MALTAB.

FIND tidbuff WHERE RECID(tidbuff) = tidtabrec NO-LOCK NO-ERROR.
FIND PERSONALTAB WHERE RECID(PERSONALTAB) = persrec NO-LOCK NO-ERROR.
FIND FIRST maltidfil WHERE maltidfil.MDATUM = tidbuff.DATUM NO-LOCK NO-ERROR.
IF NOT AVAILABLE maltidfil THEN RETURN.
IF enflerdygns = TRUE AND PERSONALTAB.TRAAVTAL = "I" THEN RETURN.
IF foslut = TRUE THEN ASSIGN reslut = 24.
IF fostart = TRUE THEN ASSIGN restart = 00.
IF tidbuff.DATUM = bdatum THEN DO:
   IF fostart = TRUE AND bdatum = avdatum  THEN persrec = persrec.
   ELSE DO:
      FIND FIRST AVDRAGMALTID WHERE AVDRAGMALTID.RESSTART LE restart AND
      AVDRAGMALTID.RESSTART2 GE restart AND 
      AVDRAGMALTID.DAGTYP = "UTRESA" AND AVDRAGMALTID.ENFLER = enflerdygns 
      USE-INDEX MALTID NO-LOCK NO-ERROR.
      IF AVAILABLE AVDRAGMALTID  THEN DO:
         IF AVDRAGMALTID.AVDRAGSTYP NE 0 THEN DO:
            maltid:
            REPEAT:
         	   FIND NEXT MALTAB WHERE MALTAB.TRAAVTAL = PERSONALTAB.TRAAVTAL AND
         	   MALTAB.FRUKOST = maltidfil.MFRU AND MALTAB.LUNCH = maltidfil.MLUN AND
         	   MALTAB.MIDDAG = maltidfil.MMID AND MALTAB.ENFLER = enflerdygns AND 
         	   MALTAB.HEL = AVDRAGMALTID.AVDRAGSTYP
         	   USE-INDEX MALTAB NO-LOCK NO-ERROR.
         	   IF NOT AVAILABLE MALTAB THEN LEAVE.
         	   IF FILL-IN-3MAN = FALSE AND MALTAB.AVDRAGSKOD BEGINS "R" THEN NEXT maltid. 
         	   IF FILL-IN-3MAN = TRUE  THEN DO:
         	      IF MALTAB.AVDRAGSKOD BEGINS "R" THEN.
         	      ELSE NEXT maltid.
         	   END.   
         	   ejtolk = FALSE.
         	   ejtolkfr = FALSE.         	   
         	   /*Nytt traktaavtal trakt13 KFS 2013/01/01*/
         	   IF (Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" ) THEN DO: 
         	      IF  MALTAB.AVDRAGSKOD = "775" OR MALTAB.AVDRAGSKOD = "777" THEN DO:
         	         /*Inga resetillägg om lunch eller middag har erhållits 8386 = 773 helt rese 8385 = 772 halvt rese*/
         	         FIND FIRST tidbuff2 WHERE tidbuff2.PERSONALKOD = tidbuff.PERSONALKOD AND tidbuff2.DATUM = tidbuff.DATUM AND tidbuff2.LONTILLAGG = "8385" EXCLUSIVE-LOCK NO-ERROR.
         	         IF AVAILABLE tidbuff2 THEN DO:
         	            DELETE tidbuff2.
         	         END.
         	         FIND FIRST tidbuff2 WHERE tidbuff2.PERSONALKOD = tidbuff.PERSONALKOD AND tidbuff2.DATUM = tidbuff.DATUM and tidbuff2.LONTILLAGG = "8386" EXCLUSIVE-LOCK NO-ERROR.
                     IF AVAILABLE tidbuff2 THEN DO:
                        DELETE tidbuff2.
                     END.   
         	         ejtolk = TRUE.         	          
                  END.
                  IF MALTAB.AVDRAGSKOD = "774" OR MALTAB.AVDRAGSKOD = "776" THEN DO:
                      IF maltidfil.MLUN = TRUE OR maltidfil.MMID = TRUE THEN ejtolkfr = TRUE.                       
                  END.                  
               END.
               IF Guru.Konstanter:globforetag = "lule" THEN DO:                 
                IF maltidfil.MLUN = TRUE OR maltidfil.MMID = TRUE THEN DO:
                   /*Inga resetillägg om lunch eller middag har erhållits 8386 = 773 helt rese 8385 = 772 halvt rese*/
                   FIND FIRST tidbuff2 WHERE tidbuff2.PERSONALKOD = PERSONALTAB.PERSONALKOD AND tidbuff2.DATUM = tidbuff.DATUM AND tidbuff2.LONTILLAGG = "837" EXCLUSIVE-LOCK NO-ERROR.
                   IF AVAILABLE tidbuff2 THEN DO:
                      DELETE tidbuff2.
                   END.
                   FIND FIRST tidbuff2 WHERE tidbuff2.PERSONALKOD = PERSONALTAB.PERSONALKOD AND tidbuff2.DATUM = tidbuff.DATUM and tidbuff2.LONTILLAGG = "838" EXCLUSIVE-LOCK NO-ERROR.
                   IF AVAILABLE tidbuff2 THEN DO:
                      DELETE tidbuff2.
                   END.
                END.                                                                             
             END.
               IF ejtolk = TRUE THEN ejtolk = FALSE.
               ELSE IF ejtolkfr = TRUE THEN.
               ELSE DO:                           	            	   
            	   CREATE TIDREGITAB.
            	   IF enflerdygns = TRUE THEN ASSIGN TIDREGITAB.ENFLERDAGS = "Endag".
            	   IF enflerdygns = FALSE THEN ASSIGN TIDREGITAB.ENFLERDAGS = "Flerdag".
            	   ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
            	   SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "MALAV" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
            	   TIDREGITAB.DAG = tidbuff.DAG
            	   TIDREGITAB.VECKONUMMER = tidbuff.VECKONUMMER
            	   TIDREGITAB.DATUM = tidbuff.DATUM
            	   TIDREGITAB.START = 7.00
            	   TIDREGITAB.SLUT = 7.00
            	   TIDREGITAB.TIDLOG = FALSE
            	   TIDREGITAB.LONTILLAGG = MALTAB.AVDRAGSKOD      
            	   TIDREGITAB.LONTILLANTAL = MALTAB.AVANTAL
            	   TIDREGITAB.AONR = tidbuff.AONR
            	   TIDREGITAB.DELNR = tidbuff.DELNR
            	   TIDREGITAB.BILFORARE = tidbuff.BILFORARE.	                 
                  IF (Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" ) AND MALTAB.AVDRAGSKOD BEGINS "77" 
                  AND AVDRAGMALTID.AVDRAGSTYP = 1 AND restart > 6 THEN DO:
                    /*Sundsvall har olika tider för traktamenten och resetillägg*/
                    mallart = "".
                    IF MALTAB.AVDRAGSKOD = "774" THEN mallart = "776".
                    IF MALTAB.AVDRAGSKOD = "775" THEN mallart = "777".
                    IF mallart NE "" THEN DO:           
                       FIND FIRST mtab WHERE mtab.TRAAVTAL = PERSONALTAB.TRAAVTAL AND
                       mtab.AVDRAGSKOD =  mallart AND
                       mtab.FRUKOST = maltidfil.MFRU AND
                       mtab.LUNCH = maltidfil.MLUN AND
                       mtab.MIDDAG = maltidfil.MMID AND
                       mtab.ENFLER = enflerdygns AND mtab.HEL = 0.5
                       USE-INDEX MALTAB NO-LOCK NO-ERROR.
                       IF AVAILABLE mtab THEN DO: 
                          ASSIGN 
                          TIDREGITAB.LONTILLAGG = mallart      
                          TIDREGITAB.LONTILLANTAL = mtab.AVANTAL.
                       END.
                    END.
                  END.                  
                  IF Guru.Konstanter:globforetag = "GKAL" AND MALTAB.AVDRAGSKOD = "XXXX" 
                  AND AVDRAGMALTID.AVDRAGSTYP = 1 AND restart > 6 THEN DO:
                  /*Kalmar har olika tider för traktamenten och resetillägg*/
                     FIND FIRST mtab WHERE mtab.TRAAVTAL = PERSONALTAB.TRAAVTAL AND
                     mtab.AVDRAGSKOD = "XXXX" AND
                  	mtab.FRUKOST = maltidfil.MFRU AND
                  	mtab.LUNCH = maltidfil.MLUN AND
                  	mtab.MIDDAG = maltidfil.MMID AND
                  	mtab.ENFLER = enflerdygns AND mtab.HEL = 0.5
                     USE-INDEX MALTAB NO-LOCK NO-ERROR.
                     IF AVAILABLE mtab THEN DO: 
                        ASSIGN TIDREGITAB.LONTILLANTAL = mtab.AVANTAL.
                     END.
                  END.
               END.
            END.   
         END.
      END.
   END.
END.
IF tidbuff.DATUM > bdatum AND tidbuff.DATUM < avdatum THEN DO:
   IF Guru.Konstanter:globforetag = "GKAL" AND FILL-IN-FRIMAT = TRUE THEN bdatum = bdatum.
   ELSE DO:
      koll1 = 1.
      /* MELLANDAGAR */   
      FIND FIRST maltidfil WHERE maltidfil.MDATUM = tidbuff.DATUM NO-LOCK NO-ERROR.
      IF NOT AVAILABLE maltidfil THEN persrec = persrec.
      ELSE DO:
         FIND FIRST AVDRAGMALTID WHERE AVDRAGMALTID.DAGTYP = "HELDAG" AND
         AVDRAGMALTID.ENFLER = enflerdygns USE-INDEX MALTID NO-LOCK NO-ERROR.
         IF AVDRAGMALTID.AVDRAGSTYP NE 0 THEN DO:
   	      block1:
	         REPEAT:
      	      IF koll1 = 1 THEN DO:
        	         FIND FIRST MALTAB WHERE MALTAB.TRAAVTAL = PERSONALTAB.TRAAVTAL AND
         		   MALTAB.FRUKOST = maltidfil.MFRU AND
          		   MALTAB.LUNCH = maltidfil.MLUN AND
         		   MALTAB.MIDDAG = maltidfil.MMID AND
         		   MALTAB.ENFLER = enflerdygns
         		   AND MALTAB.HEL = AVDRAGMALTID.AVDRAGSTYP
         		   USE-INDEX MALTAB NO-LOCK NO-ERROR.
         		   IF NOT AVAILABLE MALTAB THEN LEAVE block1.
         		   koll1 = 0.
      	      END.
      	      ELSE DO:
         		   FIND NEXT MALTAB WHERE MALTAB.TRAAVTAL = PERSONALTAB.TRAAVTAL AND
         		   MALTAB.FRUKOST = maltidfil.MFRU AND
         	     	MALTAB.LUNCH = maltidfil.MLUN AND
         		   MALTAB.MIDDAG = maltidfil.MMID AND
         	  	   MALTAB.ENFLER = enflerdygns
         		   AND MALTAB.HEL = AVDRAGMALTID.AVDRAGSTYP
         		   USE-INDEX MALTAB NO-LOCK NO-ERROR.
         		   IF NOT AVAILABLE MALTAB THEN LEAVE block1.
      	      END.
      	      IF FILL-IN-3MAN = FALSE AND MALTAB.AVDRAGSKOD BEGINS "R" THEN NEXT block1. 
               IF FILL-IN-3MAN = TRUE  THEN DO:
      	         IF MALTAB.AVDRAGSKOD BEGINS "R" THEN.
      	         ELSE NEXT block1.
      	      END.   
      	      
      	      ejtolk = FALSE.
      	      ejtolkfr = FALSE.
               /*Nytt traktaavtal trakt13 KFS 2013/01/01*/
               IF (Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" ) THEN DO: 
                  IF  MALTAB.AVDRAGSKOD = "775" OR MALTAB.AVDRAGSKOD = "777" THEN DO:
                     /*Inga resetillägg om lunch eller middag har erhållits*/
                     FIND FIRST tidbuff2 WHERE tidbuff2.PERSONALKOD = tidbuff.PERSONALKOD AND tidbuff2.DATUM = tidbuff.DATUM AND tidbuff2.LONTILLAGG = "8385" EXCLUSIVE-LOCK NO-ERROR.
                     IF AVAILABLE tidbuff2 THEN DO:
                        DELETE tidbuff2.
                     END.
                     FIND FIRST tidbuff2 WHERE tidbuff2.PERSONALKOD = tidbuff.PERSONALKOD AND tidbuff2.DATUM = tidbuff.DATUM and tidbuff2.LONTILLAGG = "8386" EXCLUSIVE-LOCK NO-ERROR.
                     IF AVAILABLE tidbuff2 THEN DO:
                        DELETE tidbuff2.
                     END.   
                     ejtolk = TRUE.                       
                  END.
                  IF MALTAB.AVDRAGSKOD = "774" OR MALTAB.AVDRAGSKOD = "776" THEN DO:
                      IF maltidfil.MLUN = TRUE OR maltidfil.MMID = TRUE THEN ejtolkfr = TRUE.                       
                  END.                  
               END.
               IF Guru.Konstanter:globforetag = "lule" THEN DO:                 
                 IF maltidfil.MLUN = TRUE OR maltidfil.MMID = TRUE THEN DO:
                   /*Inga resetillägg om lunch eller middag har erhållits 8386 = 773 helt rese 8385 = 772 halvt rese*/
                   FIND FIRST tidbuff2 WHERE tidbuff2.PERSONALKOD = PERSONALTAB.PERSONALKOD AND tidbuff2.DATUM = tidbuff.DATUM AND tidbuff2.LONTILLAGG = "837" EXCLUSIVE-LOCK NO-ERROR.
                   IF AVAILABLE tidbuff2 THEN DO:
                      DELETE tidbuff2.
                   END.
                   FIND FIRST tidbuff2 WHERE tidbuff2.PERSONALKOD = PERSONALTAB.PERSONALKOD AND tidbuff2.DATUM = tidbuff.DATUM and tidbuff2.LONTILLAGG = "838" EXCLUSIVE-LOCK NO-ERROR.
                   IF AVAILABLE tidbuff2 THEN DO:
                      DELETE tidbuff2.
                   END.
                 END.                                                                             
               END.
               IF ejtolk = TRUE THEN ejtolk = FALSE.
               ELSE IF ejtolkfr = TRUE THEN.
               ELSE DO:
         	      CREATE TIDREGITAB.
         	      IF enflerdygns = TRUE THEN ASSIGN TIDREGITAB.ENFLERDAGS = "Endag".
         	      IF enflerdygns = FALSE THEN ASSIGN TIDREGITAB.ENFLERDAGS = "Flerdag".
         	      ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
         	      SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "MALAV" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
         	      TIDREGITAB.DAG = tidbuff.DAG
         	      TIDREGITAB.VECKONUMMER = tidbuff.VECKONUMMER
         	      TIDREGITAB.DATUM = tidbuff.DATUM
         	      TIDREGITAB.START = 7.00
         	      TIDREGITAB.SLUT = 7.00
         	      TIDREGITAB.TIDLOG = FALSE
         	      TIDREGITAB.LONTILLAGG = MALTAB.AVDRAGSKOD      
         	      TIDREGITAB.LONTILLANTAL = MALTAB.AVANTAL
         	      TIDREGITAB.AONR = tidbuff.AONR
         	      TIDREGITAB.DELNR = tidbuff.DELNR
            	   TIDREGITAB.BILFORARE = tidbuff.BILFORARE.	     
            	END.   
      	   END. /*block1*/
      	   koll1 = 1.
         END.     
      END. 
   END.   
END.
koll1 = 1.
IF tidbuff.DATUM = avdatum AND avdatum > bdatum THEN DO:       
   FIND FIRST maltidfil WHERE maltidfil.MDATUM = tidbuff.DATUM NO-LOCK NO-ERROR.
   IF NOT AVAILABLE maltidfil THEN RETURN.
   FIND FIRST AVDRAGMALTID WHERE AVDRAGMALTID.RESSTART LE reslut AND
   AVDRAGMALTID.RESSTART2 GE reslut AND AVDRAGMALTID.DAGTYP = "HEMRESA" AND
   AVDRAGMALTID.ENFLER = enflerdygns USE-INDEX MALTID NO-LOCK NO-ERROR.
   IF NOT AVAILABLE AVDRAGMALTID THEN RETURN.
   IF AVDRAGMALTID.AVDRAGSTYP NE 0 THEN DO:   
      block2:
      REPEAT:
   	    IF koll1 = 1 THEN DO:
      	    FIND FIRST MALTAB WHERE MALTAB.TRAAVTAL = PERSONALTAB.TRAAVTAL AND
      	    MALTAB.FRUKOST = maltidfil.MFRU AND
      	    MALTAB.LUNCH = maltidfil.MLUN AND
      	    MALTAB.MIDDAG = maltidfil.MMID AND
      	    MALTAB.ENFLER = enflerdygns AND MALTAB.HEL = AVDRAGMALTID.AVDRAGSTYP
             USE-INDEX MALTAB NO-LOCK NO-ERROR.
      	    IF NOT AVAILABLE MALTAB THEN LEAVE.
      	    koll1 = 0.
      	 END.
      	 ELSE DO:
      	    FIND NEXT MALTAB WHERE MALTAB.TRAAVTAL = PERSONALTAB.TRAAVTAL AND
      	    MALTAB.FRUKOST = maltidfil.MFRU AND
      	    MALTAB.LUNCH = maltidfil.MLUN AND
      	    MALTAB.MIDDAG = maltidfil.MMID AND
      	    MALTAB.ENFLER = enflerdygns AND MALTAB.HEL = AVDRAGMALTID.AVDRAGSTYP
      	    USE-INDEX MALTAB NO-LOCK NO-ERROR.
      	    IF NOT AVAILABLE MALTAB THEN LEAVE.
      	 END.
      	 IF FILL-IN-3MAN = FALSE AND MALTAB.AVDRAGSKOD BEGINS "R" THEN NEXT block2. 
      	 IF FILL-IN-3MAN = TRUE  THEN DO:
      	    IF MALTAB.AVDRAGSKOD BEGINS "R" THEN.
      	    ELSE NEXT block2.
      	 END.     
      	 ejtolk = FALSE.
      	 ejtolkfr = FALSE.
          /*Nytt traktaavtal trakt13 KFS 2013/01/01*/
          IF (Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" ) THEN DO: 
            IF  MALTAB.AVDRAGSKOD = "775" OR MALTAB.AVDRAGSKOD = "777" THEN DO:
               /*Inga resetillägg om lunch eller middag har erhållits*/
               FIND FIRST tidbuff2 WHERE tidbuff2.PERSONALKOD = tidbuff.PERSONALKOD AND tidbuff2.DATUM = tidbuff.DATUM AND tidbuff2.LONTILLAGG = "8385" EXCLUSIVE-LOCK NO-ERROR.
               IF AVAILABLE tidbuff2 THEN DO:
                  DELETE tidbuff2.
               END.
               FIND FIRST tidbuff2 WHERE tidbuff2.PERSONALKOD = tidbuff.PERSONALKOD AND tidbuff2.DATUM = tidbuff.DATUM and tidbuff2.LONTILLAGG = "8386" EXCLUSIVE-LOCK NO-ERROR.
               IF AVAILABLE tidbuff2 THEN DO:
                  DELETE tidbuff2.
               END.   
               ejtolk = TRUE.                 
            END.
            IF MALTAB.AVDRAGSKOD = "774" OR MALTAB.AVDRAGSKOD = "776" THEN DO:
              IF maltidfil.MLUN = TRUE OR maltidfil.MMID = TRUE THEN ejtolkfr = TRUE.                       
            END.            
          END.
          IF Guru.Konstanter:globforetag = "lule" THEN DO:                 
             IF maltidfil.MLUN = TRUE OR maltidfil.MMID = TRUE THEN DO:
                /*Inga resetillägg om lunch eller middag har erhållits 8386 = 773 helt rese 8385 = 772 halvt rese*/
                FIND FIRST tidbuff2 WHERE tidbuff2.PERSONALKOD = PERSONALTAB.PERSONALKOD AND tidbuff2.DATUM = tidbuff.DATUM AND tidbuff2.LONTILLAGG = "837" EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE tidbuff2 THEN DO:
                   DELETE tidbuff2.
                END.
                FIND FIRST tidbuff2 WHERE tidbuff2.PERSONALKOD = PERSONALTAB.PERSONALKOD AND tidbuff2.DATUM = tidbuff.DATUM and tidbuff2.LONTILLAGG = "838" EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE tidbuff2 THEN DO:
                   DELETE tidbuff2.
                END.
             END.                                                                             
          END.
          IF ejtolk = TRUE THEN ejtolk = FALSE.
          ELSE IF ejtolkfr = TRUE THEN.
          ELSE DO:   
         	 CREATE TIDREGITAB.
         	 IF enflerdygns = TRUE THEN ASSIGN TIDREGITAB.ENFLERDAGS = "Endag".
             IF enflerdygns = FALSE THEN ASSIGN TIDREGITAB.ENFLERDAGS = "Flerdag".
         	 ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
         	 SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "MALAV" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
         	 TIDREGITAB.DAG = tidbuff.DAG
         	 TIDREGITAB.VECKONUMMER = tidbuff.VECKONUMMER
         	 TIDREGITAB.DATUM = tidbuff.DATUM
         	 TIDREGITAB.START = 7.00
         	 TIDREGITAB.SLUT = 7.00
         	 TIDREGITAB.TIDLOG = FALSE
         	 TIDREGITAB.LONTILLAGG = MALTAB.AVDRAGSKOD      
         	 TIDREGITAB.LONTILLANTAL = MALTAB.AVANTAL
         	 TIDREGITAB.AONR = tidbuff.AONR
         	 TIDREGITAB.DELNR = tidbuff.DELNR
         	 TIDREGITAB.BILFORARE = tidbuff.BILFORARE.	
             IF (Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" ) AND MALTAB.AVDRAGSKOD BEGINS "77" 
             AND AVDRAGMALTID.AVDRAGSTYP = 1  THEN DO:
                 /*Sundsvall har olika tider för traktamenten och resetillägg*/
                 mallart = "".
                 IF MALTAB.AVDRAGSKOD = "774" THEN mallart = "776".
                 IF MALTAB.AVDRAGSKOD = "775" THEN mallart = "777".
                 IF mallart NE "" THEN DO:           
                    FIND FIRST mtab WHERE mtab.TRAAVTAL = PERSONALTAB.TRAAVTAL AND
                    mtab.AVDRAGSKOD =  mallart AND
                    mtab.FRUKOST = maltidfil.MFRU AND
                    mtab.LUNCH = maltidfil.MLUN AND
                    mtab.MIDDAG = maltidfil.MMID AND
                    mtab.ENFLER = enflerdygns AND mtab.HEL = 0.5
                    USE-INDEX MALTAB NO-LOCK NO-ERROR.
                    IF AVAILABLE mtab THEN DO: 
                       ASSIGN 
                       TIDREGITAB.LONTILLAGG = mallart      
                       TIDREGITAB.LONTILLANTAL = mtab.AVANTAL.
                    END.
                 END.
             END.             
          END.
       END.
    END.   
END.
