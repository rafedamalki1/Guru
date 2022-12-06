/*GRANMAN.P*/
/*MÅNADSKÖRNING graninge*/
/* hämtar data från pasumma.d (skapas av EKO-LÖNE-SAMMANST.)
, omformar och lägger ut till pa90 ,filen som ska läsas till pa90*/
/*web*/
/*
DEFINE NEW SHARED VARIABLE man AS INTEGER FORMAT "99" NO-UNDO.
DEFINE /*NEW*/ SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE /*NEW*/ SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.

*/
{LESAMMAN.I}  
DEFINE INPUT PARAMETER invkdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ingvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER inglobforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE INPUT PARAMETER inman AS INTEGER FORMAT "99" NO-UNDO.
RUN sammut_UI (INPUT 1).
DEFINE NEW SHARED VARIABLE man AS INTEGER FORMAT "99" NO-UNDO.
DEFINE NEW SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.


ASSIGN
vkdatum     = invkdatum  
gvisatidpermanad   = ingvisatidpermanad 
Guru.Konstanter:globforetag = inglobforetag
man         = inman.
/*man endast globman = false.*/
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.

DEFINE VARIABLE personal LIKE PERSONALTAB.PERSONALKOD NO-UNDO.      /*LÖN*/
DEFINE VARIABLE losen AS CHARACTER FORMAT "X(6)" NO-UNDO.
DEFINE VARIABLE persnr LIKE PERSONALTAB.PERSONALKOD NO-UNDO.     /*LÖN*/
DEFINE VARIABLE pnummer LIKE PERSONALTAB.PERSONALKOD NO-UNDO.     /*LÖN*/
DEFINE VARIABLE sor LIKE LONTILL.ENHET NO-UNDO.    /*LÖN*/
DEFINE VARIABLE lon LIKE TIDREGITAB.LONTILLAGG NO-UNDO.     /*LÖN*/
DEFINE VARIABLE lone LIKE TIDREGITAB.LONTILLAGG NO-UNDO.     /*LÖN*/
DEFINE VARIABLE antal LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.   /*LÖN*/
DEFINE VARIABLE antal1 LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.   /*LÖN*/
DEFINE VARIABLE persnummer AS CHARACTER FORMAT "X(10)" NO-UNDO.      /*LÖN*/
DEFINE VARIABLE datum AS CHARACTER FORMAT "X(6)" NO-UNDO.      /*LÖN*/
DEFINE VARIABLE overrapp1 AS CHARACTER FORMAT "X(300)" NO-UNDO.      /*LÖN*/
DEFINE VARIABLE overrapp2 AS CHARACTER FORMAT "X(70)" NO-UNDO.      /*ÖVERTID*/
DEFINE VARIABLE overrapp3 AS CHARACTER FORMAT "X(70)" NO-UNDO.
DEFINE VARIABLE overrapp4 AS CHARACTER FORMAT "X(70)" NO-UNDO.
DEFINE VARIABLE rapphj3 AS CHARACTER FORMAT "X(6)" NO-UNDO.
DEFINE VARIABLE rapphj4 AS INTEGER FORMAT "9999999999" NO-UNDO.
DEFINE VARIABLE rapphj1 AS CHARACTER FORMAT "X(6)" NO-UNDO.      /*HJÄLP ANTAL*/
DEFINE VARIABLE rapphj2 AS CHARACTER FORMAT "X(9)" NO-UNDO.
DEFINE VARIABLE datrapphj1 AS DATE FORMAT "999999" NO-UNDO.      /*HJÄLP datum*/
DEFINE VARIABLE seku AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE beravt LIKE PERSONALTAB.BEREDSKAPSAVTAL NO-UNDO.   
DEFINE VARIABLE svar AS LOGICAL FORMAT "JA/NEJ" INITIAL FALSE NO-UNDO.
DEFINE VARIABLE rec AS RECID NO-UNDO.
DEFINE VARIABLE tal AS INTEGER NO-UNDO.
DEFINE VARIABLE tal2 AS INTEGER NO-UNDO.
DEFINE VARIABLE tal3 AS INTEGER NO-UNDO.
DEFINE VARIABLE tal4 AS INTEGER NO-UNDO.
DEFINE VARIABLE tal5 AS INTEGER NO-UNDO.
DEFINE VARIABLE tal6 AS INTEGER NO-UNDO.
DEFINE VARIABLE pkod LIKE ANSTFORMTAB.KOD NO-UNDO.
DEFINE VARIABLE akod LIKE ANSTFORMTAB.KOD NO-UNDO.
DEFINE VARIABLE aomr LIKE PERSONALTAB.OMRADE NO-UNDO.
DEFINE VARIABLE pomr LIKE PERSONALTAB.OMRADE NO-UNDO.
DEFINE VARIABLE reco AS RECID NO-UNDO.
DEFINE VARIABLE kolle AS CHARACTER NO-UNDO.
DEFINE VARIABLE ftag AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE pa90fil
   FIELD PPERSKOD LIKE PERSONALTAB.PERSONALKOD
   FIELD PLONTILLAGG LIKE TIDREGITAB.LONTILLAGG
   FIELD PLONTILLANTAL LIKE TIDREGITAB.LONTILLANTAL
   FIELD POVERTIDTILL LIKE TIDREGITAB.OVERTIDTILL
   FIELD POVERANTAL LIKE TIDREGITAB.OVERANTAL
   FIELD PTRAKTKOD LIKE TIDREGITAB.TRAKTKOD
   FIELD PTRAKTANTAL LIKE TIDREGITAB.TRAKTANTAL
   FIELD PBEREDSKAP LIKE TIDREGITAB.BEREDSKAP
   FIELD PBERANTAL LIKE TIDREGITAB.BERANTAL
   FIELD PDATUM LIKE TIDREGITAB.DATUM
   FIELD PVECKONUMMER LIKE TIDREGITAB.VECKONUMMER
   FIELD PSORT AS CHARACTER FORMAT "XX"
   FIELD PKOD LIKE ANSTFORMTAB.KOD
   FIELD POMR LIKE PERSONALTAB.OMRADE
   INDEX PPERSKOD IS PRIMARY PPERSKOD ASCENDING.
DEFINE TEMP-TABLE pa90filen
   FIELD PPERSKOD AS CHARACTER FORMAT "X(10)"
   FIELD PDATUMET AS CHARACTER FORMAT "X(6)"
   FIELD PTILLAGG LIKE TIDREGITAB.LONTILLAGG
   FIELD PTI  AS CHARACTER FORMAT "X(6)"
   FIELD PANTAL AS CHARACTER FORMAT "X(6)"
   FIELD PKR AS CHARACTER FORMAT "X(9)"
   FIELD PKOD LIKE ANSTFORMTAB.KOD
   FIELD POMR LIKE PERSONALTAB.OMRADE
   FIELD VIJUDID AS CHARACTER
   INDEX PPERSKOD IS PRIMARY PPERSKOD PTILLAGG.

FOR EACH pa90filen:
  DELETE pa90filen.
END.
FOR EACH pa90fil:
  DELETE pa90fil.
END.
{AMERICANEUROPEAN.I}
IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\gran\pasumma.d NO-ECHO.
END.  
ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\gadm\pasumma.d NO-ECHO.
END.  
ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\gsyd\pasumma.d NO-ECHO.
END.  
ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\grit\pasumma.d NO-ECHO.
END.
ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
   INPUT FROM d:\DELAD\server\PRO9S\gkal\pasumma.d NO-ECHO.
END.
ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\gkrva\pasumma.d NO-ECHO.
END.  
ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
  INPUT FROM \\pc112\delad\pro9s\korning\pasumma.d NO-ECHO.
END.    
REPEAT:
   DO TRANSACTION:
      CREATE pa90fil.
      ASSIGN.
      IMPORT pa90fil.
      IF gvisatidpermanad = FALSE THEN DO:
         IF man = 12 THEN DO:
            IF MONTH(pa90fil.PDATUM) < 6 THEN DELETE pa90fil.
         END.
         ELSE IF man = 1 THEN DO:
            IF MONTH(pa90fil.PDATUM) > 1 AND MONTH(pa90fil.PDATUM) < 4 THEN DELETE pa90fil.  
         END.   
         ELSE IF MONTH(pa90fil.PDATUM) > man THEN DELETE pa90fil.
      END.  
   END.
END.   
lon="".
antal = 0.
persnr = "".            
akod = " " .
FOR EACH pa90fil BY pa90fil.PPERSKOD BY pa90fil.PLONTILLAGG
BY pa90fil.PDATUM:
  rec = RECID(pa90fil).
  pnummer = pa90fil.PPERSKOD. 
  pkod = pa90fil.PKOD.
  IF pa90fil.PLONTILLAGG = "" THEN NEXT.
  lone = pa90fil.PLONTILLAGG.
  IF lon = lone AND persnr = pnummer THEN DO:
    FIND FIRST LONTILL WHERE LONTILL.LONTILLAGG = lon NO-LOCK NO-ERROR.
    IF NOT AVAILABLE LONTILL THEN DO:
      antal = antal + pa90fil.PLONTILLANTAL.
    END.
    ELSE DO:
      sor = LONTILL.ENHET.
      IF sor = "TI" THEN DO:
	nytid = antal.
	RUN TIMSEK.P.
	seku = sekunder.
	nytid = pa90fil.PLONTILLANTAL.
	RUN TIMSEK.P.
	sekunder = sekunder + seku.
	RUN SEKTIM.P.
	antal = nytid.
      END.
      ELSE antal = antal + pa90fil.PLONTILLANTAL.
    END.
  END.
  ELSE DO TRANSACTION:
    IF antal NE 0 THEN DO:
     FIND FIRST LONTILL WHERE LONTILL.LONTILLAGG = lon NO-LOCK NO-ERROR.
     IF NOT AVAILABLE LONTILL THEN DO:
      rapphj3 = "000000".
      rapphj1 = STRING(antal,"999999").
      rapphj2 = "000000000".
     END.
     ELSE DO:
       sor = LONTILL.ENHET.
       IF sor = "KR" THEN DO:
	  /*antal = antal * 100. */
	  rapphj3 = "000000".
	  rapphj1 = "000000".
	  rapphj2 =  STRING(antal,"999999999").
       END.
       ELSE IF sor = "TI" THEN DO:
	  nytid = antal.
	  RUN TIMSEK.P.
	  antal = sekunder / 36.     /* timmar i 100-delar*/
	  rapphj3 = STRING(antal,"999999").
	  rapphj1 = "000000".
	  rapphj2 = '000000000'.
	  
       END.
       ELSE DO:                   
          /*antal = antal * 100.            */
          rapphj3 = "000000".
	  rapphj1 = STRING(antal,"999999").
	  rapphj2 = '000000000'.
       END.
     END.
     persnummer = SUBSTRING(STRING(persnr),1 ,6) +
     SUBSTRING(STRING(persnr),7 ,4).
     datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
     SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
      CREATE pa90filen.
      ASSIGN pa90filen.PPERSKOD = persnummer
       pa90filen.PDATUMET = datum
       pa90filen.PTILLAGG = lon
       pa90filen.PTI = rapphj3
       pa90filen.PANTAL = rapphj1
       pa90filen.PKR = rapphj2
       pa90filen.PKOD = akod.
    END.
    antal = pa90fil.PLONTILLANTAL.
    lon = lone.
    persnr = pnummer.       
    akod = pkod.
  END.
END.
IF antal NE 0 THEN DO:
   FIND FIRST LONTILL WHERE LONTILL.LONTILLAGG = lon NO-LOCK NO-ERROR.
   IF NOT AVAILABLE LONTILL THEN DO:
      rapphj3 = "000000".
      rapphj1 = STRING(antal,"999999").
      rapphj2 = "000000000".    
   END.
   ELSE DO:
      sor = LONTILL.ENHET.
      IF sor = "KR" THEN DO:
         /*antal = antal * 100.            */
         rapphj3 = "000000".
	 rapphj1 = "000000".
	 rapphj2 =  STRING(antal,"999999999").
      END.
      ELSE IF sor = "TI" THEN DO:
	 nytid = antal.
	 RUN TIMSEK.P.
	 antal = sekunder / 36.     /* timmar i 100-delar*/
	 rapphj3 = STRING(antal,"999999").                  
	 rapphj1 = "000000".
	 rapphj2 = "000000000".
      END.
      ELSE DO:      
         /*antal = antal * 100.*/
         rapphj3 = "000000".
	 rapphj1 = STRING(antal,"999999").
	 rapphj2 = "000000000".
      END.
   END.
   FIND FIRST pa90fil WHERE RECID(pa90fil) = rec NO-LOCK NO-ERROR.
   IF NOT AVAILABLE pa90fil THEN personal = personal.
   ELSE DO TRANSACTION:
      persnummer = SUBSTRING(STRING(persnr),1 ,6) +
      SUBSTRING(STRING(persnr),7 ,4).
      datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
      SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
      CREATE pa90filen.
      ASSIGN pa90filen.PPERSKOD = persnummer
      pa90filen.PDATUMET = datum
      pa90filen.PTILLAGG = lon
      pa90filen.PTI = rapphj3
      pa90filen.PANTAL = rapphj1
      pa90filen.PKR = rapphj2
      pa90filen.PKOD = akod.
   END.
END.
lon="".
antal = 0.
persnr = "".               
akod = "".
FOR EACH pa90fil BY pa90fil.PPERSKOD BY pa90fil.POVERTIDTILL
BY pa90fil.PDATUM:
  rec = RECID(pa90fil).
  pnummer = pa90fil.PPERSKOD.
  pkod = pa90fil.PKOD.
  IF pa90fil.POVERTIDTILL = "" THEN NEXT.
  lone = pa90fil.POVERTIDTILL.
  IF lon = lone AND persnr = pnummer THEN DO:
      nytid = antal.
      RUN TIMSEK.P.
      seku = sekunder.
      nytid = pa90fil.POVERANTAL.
      RUN TIMSEK.P.
      sekunder = sekunder + seku.
      RUN SEKTIM.P.
      antal = nytid.
  END.
  ELSE DO TRANSACTION:
    IF antal > 0 THEN DO:
      nytid = antal.
      RUN TIMSEK.P.
      antal = sekunder / 36.     /* timmar i 100-delar*/
      rapphj3 = STRING(antal,"999999").
      rapphj1 = "000000".
      rapphj2 = "000000000".
      persnummer = SUBSTRING(STRING(persnr),1 ,6) +
      SUBSTRING(STRING(persnr),7 ,4).
      datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
      SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
      CREATE pa90filen.
      ASSIGN pa90filen.PPERSKOD = persnummer
       pa90filen.PDATUMET = datum
       pa90filen.PTILLAGG = lon
       pa90filen.PTI = rapphj3
       pa90filen.PANTAL = rapphj1
       pa90filen.PKR = rapphj2
       pa90filen.PKOD = akod.
    END.
    antal = pa90fil.POVERANTAL.
    lon = lone.
    persnr = pnummer.       
    akod = pkod.
  END.
END.
IF antal > 0 THEN DO:
  nytid = antal.
  RUN TIMSEK.P.
  antal = sekunder / 36.     /* timmar i 100-delar*/
  rapphj3 = STRING(antal,"999999").                  
  rapphj1 = "000000".
  rapphj2 = "000000000".
  persnummer = SUBSTRING(STRING(persnr),1 ,6) +
  SUBSTRING(STRING(persnr),7 ,4).
  FIND FIRST pa90fil WHERE RECID(pa90fil) = rec NO-LOCK NO-ERROR.
  IF NOT AVAILABLE pa90fil THEN personal = personal.
  ELSE DO TRANSACTION:
    datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
    SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
    CREATE pa90filen.
    ASSIGN pa90filen.PPERSKOD = persnummer
     pa90filen.PDATUMET = datum
     pa90filen.PTILLAGG = lon
     pa90filen.PTI = rapphj3
     pa90filen.PANTAL = rapphj1
     pa90filen.PKR = rapphj2
     pa90filen.PKOD = akod.
  END.
END.
lon="".
antal = 0.
persnr = "".              
akod = "".
FOR EACH pa90fil BY pa90fil.PPERSKOD BY pa90fil.PTRAKTKOD
BY pa90fil.PDATUM:
  rec = RECID(pa90fil).
  pnummer = pa90fil.PPERSKOD.
  pkod = pa90fil.PKOD.
  IF pa90fil.PTRAKTKOD = "" THEN NEXT.
  lone = pa90fil.PTRAKTKOD.
  IF lon = lone AND persnr = pnummer THEN antal = antal + pa90fil.PTRAKTANTAL.
  ELSE DO TRANSACTION:
    IF antal > 0 THEN DO:          
      /*antal = antal * 100.*/
      rapphj3 = "000000".
      rapphj1 = STRING(antal,"999999").
      rapphj2 = "000000000".
      persnummer = SUBSTRING(STRING(persnr),1 ,6) +
      SUBSTRING(STRING(persnr),7 ,4).
      datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
      SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
      CREATE pa90filen.
      ASSIGN pa90filen.PPERSKOD = persnummer
       pa90filen.PDATUMET = datum pa90filen.PTILLAGG = lon
       pa90filen.PANTAL = rapphj1 pa90filen.PTI = rapphj3 pa90filen.PKR = rapphj2 
       pa90filen.PKOD = akod.
    END.
    antal = pa90fil.PTRAKTANTAL.
    lon = lone.
    persnr = pnummer.
    akod = pkod.
  END.                                    
END.
IF antal > 0 THEN DO:
  DO TRANSACTION: 
    /*antal = antal * 100.*/
    rapphj3 = "000000".
    rapphj1 = STRING(antal,"999999").
    rapphj2 = "000000000".
    persnummer = SUBSTRING(STRING(persnr),1 ,6) +  
    SUBSTRING(STRING(persnr),7 ,4).
    FIND FIRST pa90fil WHERE RECID(pa90fil) = rec NO-LOCK NO-ERROR.
    IF NOT AVAILABLE pa90fil THEN DO:
      personal = personal.
    END.
    ELSE DO:
      datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
      SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
      CREATE pa90filen.
      ASSIGN pa90filen.PPERSKOD = persnummer
      pa90filen.PDATUMET = datum pa90filen.PTILLAGG = lon pa90filen.PTI = rapphj3
      pa90filen.PANTAL = rapphj1 pa90filen.PKR = rapphj2 pa90filen.PKOD = akod.
    END.
  END.  
END.
lon="".
antal = 0.
persnr = "".
akod = "".                                      
aomr = "".                                      
FOR EACH pa90fil BY pa90fil.PPERSKOD BY pa90fil.PBEREDSKAP
BY pa90fil.PDATUM:
  rec = RECID(pa90fil).
  pnummer = pa90fil.PPERSKOD.
  pkod = pa90fil.PKOD.
  pomr = pa90fil.POMR.
  lone = pa90fil.PBEREDSKAP.
  IF pa90fil.PBEREDSKAP = "" THEN NEXT.
  IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "GKAL"  THEN DO:
     FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pnummer
     USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
     beravt = PERSONALTAB.BEREDSKAPSAVTAL.
  END.
  ELSE DO:
     FIND FIRST PERSONALTAB WHERE PERSONALTAB.ANSTNR = pnummer
     USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
     beravt = PERSONALTAB.BEREDSKAPSAVTAL.
  END.   
  FIND FIRST BEREDSKAPTAB WHERE BEREDSKAPTAB.BEREDSKAPSAVTAL = beravt
  USE-INDEX BERED NO-LOCK NO-ERROR.    
  FIND FIRST BERKOD WHERE BERKOD.BEREDSKAPSAVTAL = beravt AND
  BERKOD.BEREDSKAP = lone USE-INDEX BERE NO-LOCK NO-ERROR.
  /*Ändrat pga lart 387 timmar kollektiv som annars har styck*/
  IF lon = lone AND persnr = pnummer THEN DO:
    IF BERKOD.ENHET = "TI" THEN DO:
      nytid = antal.
      RUN TIMSEK.P.
      seku = sekunder.
      nytid = pa90fil.PBERANTAL.
      RUN TIMSEK.P.
      sekunder = sekunder + seku.
      RUN SEKTIM.P.
      antal = nytid.
    END.
    ELSE antal = antal + pa90fil.PBERANTAL.
  END.
  ELSE DO TRANSACTION:
    IF antal > 0 THEN DO:
       IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "GKAL"  THEN DO:
          FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = persnr
          USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
          beravt = PERSONALTAB.BEREDSKAPSAVTAL.
      END.
      ELSE DO:
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.ANSTNR = persnr
         USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
         beravt = PERSONALTAB.BEREDSKAPSAVTAL.
      END.       
      FIND FIRST BEREDSKAPTAB WHERE BEREDSKAPTAB.BEREDSKAPSAVTAL = beravt
      USE-INDEX BERED NO-LOCK NO-ERROR.                        
      FIND FIRST BERKOD WHERE BERKOD.BEREDSKAPSAVTAL = beravt AND
      BERKOD.BEREDSKAP = lon USE-INDEX BERE NO-LOCK NO-ERROR.
      /*Ändrat pga lart 387 timmar kollektiv som annars har styck*/
      /*IF BEREDSKAPTAB.BERANTAL > 0 THEN DO: */
      IF BERKOD.ENHET NE "TI" THEN DO:
         rapphj3 = "000000".
         rapphj1 = "000000".   /*STRING(antal,"999999").*/
         FIND FIRST BERKOD WHERE BERKOD.BEREDSKAPSAVTAL = beravt AND
         BERKOD.BEREDSKAP = lon NO-LOCK NO-ERROR.
         antal1 = antal * BERKOD.ERSATTNING.
         rapphj2 = STRING(antal1,"999999999"). 
      END.
      ELSE DO:
         nytid = antal.
         RUN TIMSEK.P.
         antal = sekunder / 36.     /* timmar i 100-delar*/
         rapphj3 = STRING(antal,"999999").
         rapphj1 = "000000".
         rapphj2 = "000000000".
      END.   
      persnummer = SUBSTRING(STRING(persnr),1 ,6) +
      SUBSTRING(STRING(persnr),7 ,4).
      datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
      SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
      CREATE pa90filen.
      ASSIGN pa90filen.PPERSKOD = persnummer
       pa90filen.PDATUMET = datum pa90filen.PTILLAGG = lon pa90filen.PTI = rapphj3
       pa90filen.PANTAL = rapphj1 pa90filen.PKR = rapphj2 pa90filen.PKOD = akod
       pa90filen.POMR = aomr.
    END.
    antal = pa90fil.PBERANTAL.
    lon = lone.
    persnr = pnummer.
    akod = pkod.            
    aomr = pomr.               
  END.
END.
IF antal > 0 THEN DO:
  DO TRANSACTION:  
     IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "GKAL"  THEN DO:
        FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = persnr
        USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
        beravt = PERSONALTAB.BEREDSKAPSAVTAL.
     END.
     ELSE DO:
        FIND FIRST PERSONALTAB WHERE PERSONALTAB.ANSTNR = persnr
        USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
        beravt = PERSONALTAB.BEREDSKAPSAVTAL.
    END. 
    /*FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = persnr
    USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
    beravt = PERSONALTAB.BEREDSKAPSAVTAL.*/
    FIND FIRST BEREDSKAPTAB WHERE BEREDSKAPTAB.BEREDSKAPSAVTAL = beravt
    USE-INDEX BERED NO-LOCK NO-ERROR.   
    FIND FIRST BERKOD WHERE BERKOD.BEREDSKAPSAVTAL = beravt AND
    BERKOD.BEREDSKAP = lon USE-INDEX BERE NO-LOCK NO-ERROR.
    /*Ändrat pga lart 387 timmar kollektiv som annars har styck*/
    /*IF BEREDSKAPTAB.BERANTAL > 0 THEN DO: */
    IF BERKOD.ENHET NE "TI" THEN DO:    
         rapphj3 = "000000".
         rapphj1 = "000000".   /*STRING(antal,"999999").*/
         FIND FIRST BERKOD WHERE BERKOD.BEREDSKAPSAVTAL = beravt AND
         BERKOD.BEREDSKAP = lon NO-LOCK NO-ERROR.
         antal1 = antal * BERKOD.ERSATTNING.
         rapphj2 = STRING(antal1,"999999999"). 
    END.
    ELSE DO: 
       nytid = antal.
       RUN TIMSEK.P.
       antal = sekunder / 36.     /* timmar i 100-delar*/
       rapphj3 = STRING(antal,"999999").                  
       rapphj1 = "000000".
       rapphj2 = "000000000".
    END.   
    persnummer = SUBSTRING(STRING(persnr),1 ,6) +
    SUBSTRING(STRING(persnr),7 ,4).  
    FIND FIRST pa90fil WHERE RECID(pa90fil) = rec NO-LOCK NO-ERROR.
    IF NOT AVAILABLE pa90fil THEN personal = personal.
    ELSE DO:
      datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
      SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
      CREATE pa90filen.
      ASSIGN pa90filen.PPERSKOD = persnummer
       pa90filen.PDATUMET = datum pa90filen.PTILLAGG = lon pa90filen.PTI = rapphj3
       pa90filen.PANTAL = rapphj1 pa90filen.PKR = rapphj2 pa90filen.PKOD = akod
       pa90filen.POMR = aomr.
    END.
  END.
END.  
IF Guru.Konstanter:globforetag = "GKAL"    THEN DO:
   FOR EACH pa90filen:   
      IF Guru.Konstanter:globforetag = "GADM"  THEN DO:
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.ANSTNR = pa90filen.PPERSKOD NO-LOCK NO-ERROR.
      END.
      ELSE DO:      
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pa90filen.PPERSKOD NO-LOCK NO-ERROR.
      END.
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
      FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
      FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
      IF AVAILABLE JURPERS THEN DO:
         ASSIGN pa90filen.VIJUDID = JURPERS.VIJUDID.
      END.
   END.   
END.


lon="".
antal = 0.
persnr = "".
akod = "".                  
aomr = "".
/* filen pa90 sparas undan i pakopia.d innan ny pa90fil skapas */
IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gran\LOKOLL.D \\GRANGURU\guru_ser\server\PRO9S\gran\LOMAKO.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gran\LOTJAN.D \\GRANGURU\guru_ser\server\PRO9S\gran\LOMATJ.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gran\LOSYD.D \\GRANGURU\guru_ser\server\PRO9S\gran\LOMASY.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gran\FRKOLL.D \\GRANGURU\guru_ser\server\PRO9S\gran\FRMAKO.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gran\FRTJAN.D \\GRANGURU\guru_ser\server\PRO9S\gran\FRMATJ.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gran\FRSYD.D \\GRANGURU\guru_ser\server\PRO9S\gran\FRMASY.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gran\FRFELMA.D \\GRANGURU\guru_ser\server\PRO9S\gran\FRFELKO.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gran\FRANFEL.D \\GRANGURU\guru_ser\server\PRO9S\gran\FRFELMA.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gran\FRANFOR.D \\GRANGURU\guru_ser\server\PRO9S\gran\FRFORMA.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gran\TOM.D \\GRANGURU\guru_ser\server\PRO9S\gran\FRANFOR.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gran\TOM.D \\GRANGURU\guru_ser\server\PRO9S\gran\FRANFEL.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gran\pasumma.d \\GRANGURU\guru_ser\server\PRO9S\gran\paloko.d.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gran\fransu.d \\GRANGURU\guru_ser\server\PRO9S\gran\pafrko.d.

/*    DOS SILENT \\GRANGURU\guru_ser\server\PRO9S\gran\makop.bat.    /* I  \GURU\GSU.BAT*/*/
END.  
ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gadm\LOKOLL.D \\GRANGURU\guru_ser\server\PRO9S\gadm\LOMAKO.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gadm\LOTJAN.D \\GRANGURU\guru_ser\server\PRO9S\gadm\LOMATJ.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gadm\LOTRAD.D \\GRANGURU\guru_ser\server\PRO9S\gadm\LOMATR.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gadm\FRKOLL.D \\GRANGURU\guru_ser\server\PRO9S\gadm\FRMAKO.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gadm\FRTJAN.D \\GRANGURU\guru_ser\server\PRO9S\gadm\FRMATJ.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gadm\FRTRAD.D \\GRANGURU\guru_ser\server\PRO9S\gadm\FRMATR.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gadm\FRFELMA.D \\GRANGURU\guru_ser\server\PRO9S\gadm\FRFELKO.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gadm\FRANFEL.D \\GRANGURU\guru_ser\server\PRO9S\gadm\FRFELMA.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gadm\FRANFOR.D \\GRANGURU\guru_ser\server\PRO9S\gadm\FRFORMA.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gadm\TOM.D \\GRANGURU\guru_ser\server\PRO9S\gadm\FRANFOR.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gadm\TOM.D \\GRANGURU\guru_ser\server\PRO9S\gadm\FRANFEL.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gadm\pasumma.d \\GRANGURU\guru_ser\server\PRO9S\gadm\paloko.d.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gadm\fransu.d \\GRANGURU\guru_ser\server\PRO9S\gadm\pafrko.d.
   /* DOS SILENT \\GRANGURU\guru_ser\server\PRO9S\gadm\makop.bat.    /* I  \GURU\GSU.BAT*/*/
END.  
ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gsyd\LOKOLL.D \\GRANGURU\guru_ser\server\PRO9S\gsyd\LOMAKO.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gsyd\LOTJAN.D \\GRANGURU\guru_ser\server\PRO9S\gsyd\LOMATJ.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gsyd\FRKOLL.D \\GRANGURU\guru_ser\server\PRO9S\gsyd\FRMAKO.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gsyd\FRTJAN.D \\GRANGURU\guru_ser\server\PRO9S\gsyd\FRMATJ.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gsyd\FRFELMA.D \\GRANGURU\guru_ser\server\PRO9S\gsyd\FRFELKO.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gsyd\FRANFEL.D \\GRANGURU\guru_ser\server\PRO9S\gsyd\FRFELMA.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gsyd\TOM.D \\GRANGURU\guru_ser\server\PRO9S\gsyd\FRANFEL.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gsyd\pasumma.d \\GRANGURU\guru_ser\server\PRO9S\gsyd\paloko.d.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gsyd\fransu.d \\GRANGURU\guru_ser\server\PRO9S\gsyd\pafrko.d.

/*    DOS SILENT \\GRANGURU\guru_ser\server\PRO9S\gsyd\makop.bat.    /* I  \GURU\GSU.BAT*/*/
END. 
ELSE IF Guru.Konstanter:globforetag = "GRIT"  THEN DO:
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\grit\LOKOLL.D \\GRANGURU\guru_ser\server\PRO9S\grit\LOMAKO.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\grit\LOTJAN.D \\GRANGURU\guru_ser\server\PRO9S\grit\LOMATJ.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\grit\FRKOLL.D \\GRANGURU\guru_ser\server\PRO9S\grit\FRMAKO.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\grit\FRTJAN.D \\GRANGURU\guru_ser\server\PRO9S\grit\FRMATJ.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\grit\FRFELMA.D \\GRANGURU\guru_ser\server\PRO9S\grit\FRFELKO.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\grit\FRANFEL.D \\GRANGURU\guru_ser\server\PRO9S\grit\FRFELMA.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\grit\FRANFOR.D \\GRANGURU\guru_ser\server\PRO9S\grit\FRFORMA.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\grit\TOM.D \\GRANGURU\guru_ser\server\PRO9S\grit\FRANFOR.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\grit\TOM.D \\GRANGURU\guru_ser\server\PRO9S\grit\FRANFEL.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\grit\pasumma.d \\GRANGURU\guru_ser\server\PRO9S\grit\paloko.d.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\grit\fransu.d \\GRANGURU\guru_ser\server\PRO9S\grit\pafrko.d.

/*    DOS SILENT \\GRANGURU\guru_ser\server\PRO9S\grit\makop.bat.    /* I  \GURU\GSU.BAT*/*/
END. 
ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
   OS-COPY d:\DELAD\server\PRO9S\gkal\LOKOLL.D d:\DELAD\server\PRO9S\gkal\LOMAKO.D.
   OS-COPY d:\DELAD\server\PRO9S\gkal\LOTJAN.D d:\DELAD\server\PRO9S\gkal\LOMATJ.D.
   OS-COPY d:\DELAD\server\PRO9S\gkal\FRKOLL.D d:\DELAD\server\PRO9S\gkal\FRMAKO.D.
   OS-COPY d:\DELAD\server\PRO9S\gkal\FRTJAN.D d:\DELAD\server\PRO9S\gkal\FRMATJ.D.
   OS-COPY d:\DELAD\server\PRO9S\gkal\FRFELMA.D d:\DELAD\server\PRO9S\gkal\FRFELKO.D.
   OS-COPY d:\DELAD\server\PRO9S\gkal\FRANFEL.D d:\DELAD\server\PRO9S\gkal\FRFELMA.D.
   OS-COPY d:\DELAD\server\PRO9S\gkal\TOM.D d:\DELAD\server\PRO9S\gkal\FRANFEL.D.
   OS-COPY d:\DELAD\server\PRO9S\gkal\pasumma.d d:\DELAD\server\PRO9S\gkal\paloko.d.
   OS-COPY d:\DELAD\server\PRO9S\gkal\fransu.d d:\DELAD\server\PRO9S\gkal\pafrko.d.

/*    DOS SILENT \\GRANGURU\guru_ser\server\PRO9S\gren\makop.bat.    /* I  \GURU\GSU.BAT*/*/
END. 
ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gkrva\LOKOLL.D \\GRANGURU\guru_ser\server\PRO9S\gkrva\LOMAKO.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gkrva\LOTJAN.D \\GRANGURU\guru_ser\server\PRO9S\gkrva\LOMATJ.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gkrva\FRKOLL.D \\GRANGURU\guru_ser\server\PRO9S\gkrva\FRMAKO.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gkrva\FRTJAN.D \\GRANGURU\guru_ser\server\PRO9S\gkrva\FRMATJ.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gkrva\FRFELMA.D \\GRANGURU\guru_ser\server\PRO9S\gkrva\FRFELKO.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gkrva\FRANFEL.D \\GRANGURU\guru_ser\server\PRO9S\gkrva\FRFELMA.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gkrva\FRANFOR.D \\GRANGURU\guru_ser\server\PRO9S\gkrva\FRFORMA.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gkrva\TOM.D \\GRANGURU\guru_ser\server\PRO9S\gkrva\FRANFOR.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gkrva\TOM.D \\GRANGURU\guru_ser\server\PRO9S\gkrva\FRANFEL.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gkrva\pasumma.d \\GRANGURU\guru_ser\server\PRO9S\gkrva\paloko.d.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gkrva\fransu.d \\GRANGURU\guru_ser\server\PRO9S\gkrva\pafrko.d.
   
END.  
ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
   OS-COPY \\pc112\delad\pro9s\korning\LOKOLL.D \\pc112\delad\pro9s\korning\LOMAKO.D.
   OS-COPY \\pc112\delad\pro9s\korning\LOTJAN.D \\pc112\delad\pro9s\korning\LOMATJ.D.
   OS-COPY \\pc112\delad\pro9s\korning\LOSYD.D \\pc112\delad\pro9s\korning\LOMASY.D.
   OS-COPY \\pc112\delad\pro9s\korning\FRKOLL.D \\pc112\delad\pro9s\korning\FRMAKO.D.
   OS-COPY \\pc112\delad\pro9s\korning\FRTJAN.D \\pc112\delad\pro9s\korning\FRMATJ.D.
   OS-COPY \\pc112\delad\pro9s\korning\FRSYD.D \\pc112\delad\pro9s\korning\FRMASY.D.
   OS-COPY \\pc112\delad\pro9s\korning\FRFELMA.D \\pc112\delad\pro9s\korning\FRFELKO.D.
   OS-COPY \\pc112\delad\pro9s\korning\FRANFEL.D \\pc112\delad\pro9s\korning\FRFELMA.D.
   OS-COPY \\pc112\delad\pro9s\korning\TOM.D \\pc112\delad\pro9s\korning\FRANFEL.D.
   OS-COPY \\pc112\delad\pro9s\korning\pasumma.d \\pc112\delad\pro9s\korning\gran\paloko.d.
   OS-COPY \\pc112\delad\pro9s\korning\fransu.d \\pc112\delad\pro9s\korning\pafrko.d.
END.  


IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
  OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gran\pa2.d.
END.   
ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
  OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gadm\pa2.d.
END.
ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
  OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsyd\pa2.d.
END.   
ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
  OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\grit\pa2.d.
END.
ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
  OUTPUT TO d:\DELAD\server\PRO9S\gkal\pa2.d.
END.
ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
  OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\pa2.d.
END.
ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
   OUTPUT TO \\pc112\delad\pro9s\korning\pa2.d.
END.                                
FOR EACH pa90filen BY pa90filen.PPERSKOD BY pa90filen.PTILLAGG:
  EXPORT pa90filen.
END. 
/*hur skall gsyd funka här*/                          
IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:
   IF Guru.Konstanter:globforetag = "GRAN" THEN OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gran\lokoll.d.
   IF Guru.Konstanter:globforetag = "CELPA" THEN OUTPUT TO \\pc112\delad\pro9s\korning\lokoll.d.
   FOR EACH pa90filen WHERE pa90filen.PPERSKOD BEGINS "35" BY pa90filen.PPERSKOD BY pa90filen.PTILLAGG:
      IF pa90filen.POMR ="" THEN DO:
         /* TIDIGARE 2222 03 skall ej finnas några kvar*/
         overrapp1 = "8888" + " " + "05" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " " + 
         SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +  
         SUBSTRING(pa90filen.PTI,1 ,6) + " "  +   
         SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
         SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
         "000" + " " + "0000" + " " + "      " + " " + "EL".
      END.
      ELSE DO:
         overrapp1 = "8888" + " " + "05" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " " + 
         SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +  
         SUBSTRING(pa90filen.PTI,1 ,6) + " "  +   
         SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
         SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
         "0" + SUBSTRING(pa90filen.POMR,1,2) + " " + "3000" + " " + "      " + " " + "EL".
      END.   
      PUT overrapp1.   /*  pa90filen.P    */
      PUT SKIP.
   END.
END.  
ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gadm\lokoll.d.
   FOR EACH pa90filen WHERE pa90filen.VIJUDID = "GRA" BY pa90filen.PPERSKOD BY pa90filen.PTILLAGG:
      IF pa90filen.POMR ="" THEN DO:
         /*Tidigare 2000 04*/
         overrapp1 = "8888" + " " + "03" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " " + 
         SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +  
         SUBSTRING(pa90filen.PTI,1 ,6) + " "  +   
         SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
         SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
         "000" + " " + "0000" + " " + "      " + " " + "EL".
      END.
      ELSE DO:
         overrapp1 = "8888" + " " + "03" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " " + 
         SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +  
         SUBSTRING(pa90filen.PTI,1 ,6) + " "  +   
         SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
         SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
         "0" + SUBSTRING(pa90filen.POMR,1,2) + " " + "3000" + " " + "      " + " " + "EL".
      END.   
      PUT overrapp1.   /*  pa90filen.P    */
      PUT SKIP.
   END.
END.   
ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\lokoll.d.
   FOR EACH pa90filen WHERE pa90filen.VIJUDID = "GVAOST" BY pa90filen.PPERSKOD BY pa90filen.PTILLAGG:
      IF pa90filen.POMR ="" THEN DO:
         overrapp1 = "8888" + " " + "06" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " " + 
         SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +  
         SUBSTRING(pa90filen.PTI,1 ,6) + " "  +   
         SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
         SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
         "000" + " " + "0000" + " " + "      " + " " + "EL".
      END.
      ELSE DO:
         overrapp1 = "8888" + " " + "06" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " " + 
         SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +  
         SUBSTRING(pa90filen.PTI,1 ,6) + " "  +   
         SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
         SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
         "0" + SUBSTRING(pa90filen.POMR,1,2) + " " + "3000" + " " + "      " + " " + "EL".
      END.   
      PUT overrapp1.   /*  pa90filen.P    */
      PUT SKIP.
   END.
END.   
ELSE IF Guru.Konstanter:globforetag = "GRIT" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
   IF Guru.Konstanter:globforetag = "grit" THEN OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\grit\lokoll.d.
   IF Guru.Konstanter:globforetag = "ELPA" THEN OUTPUT TO \\pc112\delad\pro9s\korning\lokoll.d.
   FOR EACH pa90filen BY pa90filen.VIJUDID BY pa90filen.PPERSKOD BY pa90filen.PTILLAGG:
      IF pa90filen.VIJUDID = "Gra" THEN DO:
         ASSIGN
         ftag = "8888"
         kolle = "08".
      END.
      ELSE IF pa90filen.VIJUDID = "Fors" THEN DO: 
        ASSIGN
         ftag = "8888"
         kolle = "03".
      END.
      ELSE IF pa90filen.VIJUDID = "Serv" THEN DO: 
        ASSIGN
         ftag = "8888"
         kolle = "02".
      END.
      ELSE IF pa90filen.VIJUDID = "FEKA" THEN DO: 
        ASSIGN
         ftag = "8888"
         kolle = "09".
      END.
      ELSE DO:
         ASSIGN
         ftag = "8888"
         kolle = "08".
      END.
      IF pa90filen.POMR ="" THEN DO:
         /* ridigare 2000 04*/
         overrapp1 = ftag + " " + kolle + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " " + 
         SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +  
         SUBSTRING(pa90filen.PTI,1 ,6) + " "  +   
         SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
         SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
         "000" + " " + "0000" + " " + "      " + " " + "EL".
      END.
      ELSE DO:
         overrapp1 = ftag + " " + kolle + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " " + 
         SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +  
         SUBSTRING(pa90filen.PTI,1 ,6) + " "  +   
         SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
         SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
         "0" + SUBSTRING(pa90filen.POMR,1,2) + " " + "3000" + " " + "      " + " " + "EL".
      END.   
      PUT overrapp1.   /*  pa90filen.P    */
      PUT SKIP.
   END.
END.   
ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
   OUTPUT TO d:\DELAD\server\PRO9S\gkal\lokoll.d.
   FOR EACH pa90filen BY pa90filen.VIJUDID BY pa90filen.PPERSKOD BY pa90filen.PTILLAGG:
      /*IF pa90filen.POMR ="" THEN DO:*/
         IF pa90filen.VIJUDID = "GKEAB" THEN kolle = "01".
         ELSE IF pa90filen.VIJUDID = "GSEAB" THEN kolle = "02".
         overrapp1 = "2003" + " " + kolle + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " " + 
         SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +  
         SUBSTRING(pa90filen.PTI,1 ,6) + " "  +   
         SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
         SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
         "000" + " " + "0000" + " " + "      " + " " + "EL".
      /*END.
      ELSE DO:
         overrapp1 = "2003" + " " + kolle + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " " + 
         SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +  
         SUBSTRING(pa90filen.PTI,1 ,6) + " "  +   
         SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
         SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
         SUBSTRING(pa90filen.POMR,1,3) + " " + "3000" + " " + "      " + " " + "EL".
      END.   */
      PUT overrapp1.   /*  pa90filen.P    */
      PUT SKIP.
   END.
END.   
ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsyd\lokoll.d.
   FOR EACH pa90filen BY pa90filen.PPERSKOD BY pa90filen.PTILLAGG:
      IF pa90filen.POMR ="" THEN DO:
         /*TIDIGARE 4444 01*/
         overrapp1 = "8888" + " " + "04" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " " + 
         SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +  
         SUBSTRING(pa90filen.PTI,1 ,6) + " "  +   
         SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
         SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
         "000" + " " + "0000" + " " + "      " + " " + "EL".
      END.
      ELSE DO:
         overrapp1 = "8888" + " " + "04" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " " + 
         SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +  
         SUBSTRING(pa90filen.PTI,1 ,6) + " "  +   
         SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
         SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
         "0" + SUBSTRING(pa90filen.POMR,1,2) + " " + "3000" + " " + "      " + " " + "EL".
      END.   
      PUT overrapp1.   /*  pa90filen.P    */
      PUT SKIP.
   END.
END.       

OUTPUT CLOSE.
IF Guru.Konstanter:globforetag = "GRAN"   OR Guru.Konstanter:globforetag = "CELPA" THEN DO:
   IF Guru.Konstanter:globforetag = "GRAN" THEN  OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gran\lotjan.d.
   IF Guru.Konstanter:globforetag = "GADM" THEN  OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gadm\lotjan.d.
   IF Guru.Konstanter:globforetag = "GKRVA" THEN  OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\lotjan.d.
   IF Guru.Konstanter:globforetag = "CELPA" THEN  OUTPUT TO \\pc112\delad\pro9s\korning\lotjan.d.
   IF Guru.Konstanter:globforetag = "GRAN" THEN DO:   
      FOR EACH pa90filen WHERE pa90filen.PPERSKOD BEGINS "45" BY pa90filen.PPERSKOD BY pa90filen.PTILLAGG:
         IF pa90filen.PPERSKOD = "45086" THEN Guru.Konstanter:globforetag = Guru.Konstanter:globforetag.
         ELSE DO:
            IF pa90filen.POMR = "" THEN DO:
               /* TIDIGARE 2222 04*/
               overrapp1 = "8888" + " " + "05" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " "+ 
               SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +    
               SUBSTRING(pa90filen.PTI,1 ,6) + " "  +  
               SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
               SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
               "000" + " " + "0000" + " " + "      " + " " + "EL".
            END.
            ELSE DO: 
               overrapp1 = "8888" + " " + "05" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " " + 
               SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +  
               SUBSTRING(pa90filen.PTI,1 ,6) + " "  +  
               SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
               SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
               "0" + SUBSTRING(pa90filen.POMR,1,2) + " " + "3000" + " " + "      " + " " + "EL".
            END. 
            PUT overrapp1.   /*  pa90filen.P    */ 
            PUT SKIP.
         END.
      END.  
      OUTPUT CLOSE.
   END.
   IF Guru.Konstanter:globforetag = "GADM" THEN DO:   
      FOR EACH pa90filen WHERE pa90filen.VIJUDID = "GRAEM" BY pa90filen.PPERSKOD BY pa90filen.PTILLAGG:
         IF pa90filen.POMR = "" THEN DO:
            /* TIDIGARE 2000 03*/
            overrapp1 = "8888" + " " + "03" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " "+ 
            SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +    
            SUBSTRING(pa90filen.PTI,1 ,6) + " "  +  
            SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
            SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
            "000" + " " + "0000" + " " + "      " + " " + "EL".
         END.
         ELSE DO: 
            overrapp1 = "8888" + " " + "03" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " " + 
            SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +  
            SUBSTRING(pa90filen.PTI,1 ,6) + " "  +  
            SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
            SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
            "0" + SUBSTRING(pa90filen.POMR,1,2) + " " + "3000" + " " + "      " + " " + "EL".
         END. 
         PUT overrapp1.   /*  pa90filen.P    */ 
         PUT SKIP.
      END.  
      OUTPUT CLOSE.
   END.
   IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:   
      FOR EACH pa90filen WHERE pa90filen.VIJUDID = "GVARME" BY pa90filen.PPERSKOD BY pa90filen.PTILLAGG:
         IF pa90filen.POMR = "" THEN DO:
            /* tidigare 2000 02*/
            overrapp1 = "8888" + " " + "07" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " "+ 
            SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +    
            SUBSTRING(pa90filen.PTI,1 ,6) + " "  +  
            SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
            SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
            "000" + " " + "0000" + " " + "      " + " " + "EL".
         END.
         ELSE DO: 
            overrapp1 = "8888" + " " + "07" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " " + 
            SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +  
            SUBSTRING(pa90filen.PTI,1 ,6) + " "  +  
            SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
            SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
            "0" + SUBSTRING(pa90filen.POMR,1,2) + " " + "3000" + " " + "      " + " " + "EL".
         END. 
         PUT overrapp1.   /*  pa90filen.P    */ 
         PUT SKIP.
      END.  
      OUTPUT CLOSE.
   END.
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:
      IF Guru.Konstanter:globforetag = "GRAN" THEN OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gran\losyd.d.
      IF Guru.Konstanter:globforetag = "CELPA" THEN OUTPUT TO \\pc112\delad\pro9s\korning\losyd.d.
      FOR EACH pa90filen WHERE pa90filen.PPERSKOD BEGINS "31" BY pa90filen.PPERSKOD BY pa90filen.PTILLAGG:
         IF pa90filen.POMR = "" THEN DO:
            /*TIDIGARE 4444 01*/
            overrapp1 = "8888" + " " + "04" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " "+ 
            SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +    
            SUBSTRING(pa90filen.PTI,1 ,6) + " "  +  
            SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
            SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
            "000" + " " + "0000" + " " + "      " + " " + "EL".
         END.
         ELSE DO: 
            overrapp1 = "8888" + " " + "04" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " " + 
            SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +  
            SUBSTRING(pa90filen.PTI,1 ,6) + " "  +  
            SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
            SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
            "0" + SUBSTRING(pa90filen.POMR,1,2) + " " + "3000" + " " + "      " + " " + "EL".
         END. 
         PUT overrapp1.   /*  pa90filen.P    */ 
         PUT SKIP.
      END.  
      FOR EACH pa90filen WHERE pa90filen.PPERSKOD BEGINS "41" BY pa90filen.PPERSKOD BY pa90filen.PTILLAGG:
         IF pa90filen.POMR = "" THEN DO:
            /*TIDIGARE 4444 01*/
            overrapp1 = "8888" + " " + "04" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " "+ 
            SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +    
            SUBSTRING(pa90filen.PTI,1 ,6) + " "  +  
            SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
            SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
            "000" + " " + "0000" + " " + "      " + " " + "EL".
         END.
         ELSE DO: 
            overrapp1 = "8888" + " " + "04" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " " + 
            SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +  
            SUBSTRING(pa90filen.PTI,1 ,6) + " "  +  
            SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
            SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
            "0" + SUBSTRING(pa90filen.POMR,1,2) + " " + "3000" + " " + "      " + " " + "EL".
         END. 
         PUT overrapp1.   /*  pa90filen.P    */ 
         PUT SKIP.
      END.
      FOR EACH pa90filen WHERE pa90filen.PPERSKOD BEGINS "24" BY pa90filen.PPERSKOD BY pa90filen.PTILLAGG:
         IF pa90filen.POMR = "" THEN DO:
            /*TIDIGARE 4444 01*/
            overrapp1 = "8888" + " " + "04" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " "+ 
            SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +    
            SUBSTRING(pa90filen.PTI,1 ,6) + " "  +  
            SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
            SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
            "000" + " " + "0000" + " " + "      " + " " + "EL".
         END.
         ELSE DO: 
            overrapp1 = "8888" + " " + "04" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " " + 
            SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +  
            SUBSTRING(pa90filen.PTI,1 ,6) + " "  +  
            SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
            SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
            "0" + SUBSTRING(pa90filen.POMR,1,2) + " " + "3000" + " " + "      " + " " + "EL".
         END. 
         PUT overrapp1.   /*  pa90filen.P    */ 
         PUT SKIP.
      END.  
      FOR EACH pa90filen WHERE pa90filen.PPERSKOD = "45086" BY pa90filen.PPERSKOD BY pa90filen.PTILLAGG:
         IF pa90filen.POMR = "" THEN DO:
            /*TIDIGARE 4444 01*/
            overrapp1 = "8888" + " " + "04" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " "+ 
            SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +    
            SUBSTRING(pa90filen.PTI,1 ,6) + " "  +  
            SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
            SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
            "000" + " " + "0000" + " " + "      " + " " + "EL".
         END.
         ELSE DO: 
            overrapp1 = "8888" + " " + "04" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " " + 
            SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +  
            SUBSTRING(pa90filen.PTI,1 ,6) + " "  +  
            SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
            SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
            "0" + SUBSTRING(pa90filen.POMR,1,2) + " " + "3000" + " " + "      " + " " + "EL".
         END. 
         PUT overrapp1.   /*  pa90filen.P    */ 
         PUT SKIP.
      END.  
   END.
   IF Guru.Konstanter:globforetag = "GADM" THEN DO:   
      IF Guru.Konstanter:globforetag = "GADM" THEN OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gadm\lotrad.d.
      FOR EACH pa90filen WHERE pa90filen.VIJUDID = "SYDSP" BY pa90filen.PPERSKOD BY pa90filen.PTILLAGG:
         IF pa90filen.POMR = "" THEN DO:
            /* TIDIGARE 5555 01*/
            overrapp1 = "8888" + " " + "02" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " "+ 
            SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +    
            SUBSTRING(pa90filen.PTI,1 ,6) + " "  +  
            SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
            SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
            "000" + " " + "0000" + " " + "      " + " " + "EL".
         END.
         ELSE DO: 
            overrapp1 = "8888" + " " + "02" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " " + 
            SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +  
            SUBSTRING(pa90filen.PTI,1 ,6) + " "  +  
            SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
            SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
            "0" + SUBSTRING(pa90filen.POMR,1,2) + " " + "3000" + " " + "      " + " " + "EL".
         END. 
         PUT overrapp1.   /*  pa90filen.P    */ 
         PUT SKIP.
      END.  
      OUTPUT CLOSE.
   END.
   
END.  

OUTPUT CLOSE.
/* pa90 -filen summeras til pa90sum.d som innehåller alla månadskörningar */
/*IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
   OS-APPEND \\GRANGURU\guru_ser\server\PRO9S\GRAN\LOKOLL.D \\GRANGURU\guru_ser\server\PRO9S\GRAN\PA90SUM.D.
   OS-APPEND \\GRANGURU\guru_ser\server\PRO9S\GRAN\LOTJAN.D \\GRANGURU\guru_ser\server\PRO9S\GRAN\PA90SUM.D.
/*    DOS SILENT \\GRANGURU\guru_ser\server\PRO9S\gran\sum.bat.    /* I  \GURU\GSU.BAT*/*/    
END.
ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
    OS-APPEND \\GRANGURU\guru_ser\server\PRO9S\gadm\LOKOLL.D \\GRANGURU\guru_ser\server\PRO9S\gadm\PA90SUM.D.
    OS-APPEND \\GRANGURU\guru_ser\server\PRO9S\gadm\LOTJAN.D \\GRANGURU\guru_ser\server\PRO9S\gadm\PA90SUM.D.

   /* DOS SILENT \\GRANGURU\guru_ser\server\PRO9S\gadm\sum.bat.    /* I  \GURU\GSU.BAT*/    */
END.     
ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
    OS-APPEND \\GRANGURU\guru_ser\server\PRO9S\grit\LOKOLL.D \\GRANGURU\guru_ser\server\PRO9S\grit\PA90SUM.D.
    OS-APPEND \\GRANGURU\guru_ser\server\PRO9S\grit\LOTJAN.D \\GRANGURU\guru_ser\server\PRO9S\grit\PA90SUM.D.

   /* DOS SILENT \\GRANGURU\guru_ser\server\PRO9S\grit\sum.bat.    /* I  \GURU\GSU.BAT*/    */
END.
ELSE IF Guru.Konstanter:globforetag = "GREN" THEN DO:
    OS-APPEND \\GRANGURU\guru_ser\server\PRO9S\gren\LOKOLL.D \\GRANGURU\guru_ser\server\PRO9S\gren\PA90SUM.D.
    OS-APPEND \\GRANGURU\guru_ser\server\PRO9S\gren\LOTJAN.D \\GRANGURU\guru_ser\server\PRO9S\gren\PA90SUM.D.

   /* DOS SILENT \\GRANGURU\guru_ser\server\PRO9S\gren\sum.bat.    /* I  \GURU\GSU.BAT*/    */
END.  
ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
   OS-APPEND \\GRANGURU\guru_ser\server\PRO9S\gsyd\LOKOLL.D \\GRANGURU\guru_ser\server\PRO9S\gsyd\PA90SUM.D.
   OS-APPEND \\GRANGURU\guru_ser\server\PRO9S\gsyd\LOTJAN.D \\GRANGURU\guru_ser\server\PRO9S\gsyd\PA90SUM.D.

   /* DOS SILENT \\GRANGURU\guru_ser\server\PRO9S\gsyd\sum.bat.    /* I  \GURU\GSU.BAT*/*/
END.*/
FOR EACH pa90fil:
  DELETE pa90fil.
END.
/* ALLT SOM ÄR EKO-LÖNE-SAMMANST. SEDAN FÖRRA MÅNADSKÖRNING LÄGGS I ARBETSFIL */                 
IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\gran\pasumma.d NO-ECHO.
END.   
ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\gadm\pasumma.d NO-ECHO.
END. 
ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\grit\pasumma.d NO-ECHO.
END. 
ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
   INPUT FROM d:\DELAD\server\PRO9S\gkal\pasumma.d NO-ECHO.
END. 
ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\gsyd\pasumma.d NO-ECHO.
END.
ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\gkrva\pasumma.d NO-ECHO.
END. 
ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
   INPUT FROM \\pc112\delad\pro9s\korning\pasumma.d NO-ECHO.
END.   
REPEAT TRANSACTION:
  CREATE pa90fil.
  ASSIGN.
  IMPORT pa90fil.
END.
/* KOPIA SKAPAS*/
IF gvisatidpermanad = TRUE THEN DO:
   IF MONTH(vkdatum) = 1 THEN DO:
      IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gran\pakop01.d.
      END.   
      ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gadm\pakop01.d.
      END.
      ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsyd\pakop01.d.
      END. 
      ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\grit\pakop01.d.
      END.
      ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
         OUTPUT TO d:\DELAD\server\PRO9S\gkal\pakop01.d.
      END.         
      ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\pakop01.d.
      END.         
      ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
         OUTPUT TO \\pc112\delad\pro9s\korning\pakop01.d.
      END.      
      PUT "JANUARI MÅNAD" AT 20.
   END.
   IF MONTH(vkdatum) = 2 THEN DO:
      IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gran\pakop02.d.
      END. 
      ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gadm\pakop02.d.
      END.   
      ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\grit\pakop02.d.
      END. 
      ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
         OUTPUT TO d:\DELAD\server\PRO9S\gkal\pakop02.d.
      END. 
      ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsyd\pakop02.d.
      END.            
      ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\pakop02.d.
      END.         
      ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN  OUTPUT TO \\pc112\delad\pro9s\korning\pakop02.d.     
      PUT "FEBRUARI MÅNAD" AT 20.
   END.
   IF MONTH(vkdatum) = 3 THEN DO:
      IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gran\pakop03.d.
      END. 
      ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gadm\pakop03.d.
      END.  
      ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\grit\pakop03.d.
      END.
      ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
         OUTPUT TO d:\DELAD\server\PRO9S\gkal\pakop03.d.
      END.                                              
      ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsyd\pakop03.d.
      END. 
      ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\pakop03.d.
      END.         
      ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN OUTPUT TO \\pc112\delad\pro9s\korning\pakop03.d.    
      PUT "MARS MÅNAD" AT 20.
   END.
   IF MONTH(vkdatum) = 4 THEN DO:
      IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gran\pakop04.d.
      END.                                                       
      ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gadm\pakop04.d.
      END. 
      ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\grit\pakop04.d.
      END.
      ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
         OUTPUT TO d:\DELAD\server\PRO9S\gkal\pakop04.d.
      END.
      ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsyd\pakop04.d.
      END. 
      ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\pakop04.d.
      END.         
      ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN OUTPUT TO \\pc112\delad\pro9s\korning\pakop04.d.    
      PUT "APRIL MÅNAD" AT 20.
   END.
   IF MONTH(vkdatum) = 5 THEN DO:
      IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gran\pakop05.d.
      END.                   
      ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gadm\pakop05.d.
      END.   
      ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\grit\pakop05.d.
      END.
      ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
         OUTPUT TO d:\DELAD\server\PRO9S\gkal\pakop05.d.
      END.                           
      ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsyd\pakop05.d.
      END. 
      ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\pakop05.d.
      END.         
      ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN  OUTPUT TO \\pc112\delad\pro9s\korning\pakop05.d.    
      PUT "MAJ MÅNAD" AT 20.
   END.
   IF MONTH(vkdatum) = 6 THEN DO:
     IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gran\pakop06.d.
      END.                   
      ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gadm\pakop06.d.
      END.     
      ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\grit\pakop06.d.
      END.
      ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
         OUTPUT TO d:\DELAD\server\PRO9S\gkal\pakop06.d.
      END.                         
      ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsyd\pakop06.d.
      END. 
      ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\pakop06.d.
      END.         
      ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN OUTPUT TO \\pc112\delad\pro9s\korning\pakop06.d.
      PUT "JUNI MÅNAD" AT 20.
   END.
   IF MONTH(vkdatum) = 7 THEN DO:
      IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gran\pakop07.d.
      END.                   
      ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gadm\pakop07.d.
      END.   
      ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\grit\pakop07.d.
      END.
      ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
         OUTPUT TO d:\DELAD\server\PRO9S\gkal\pakop07.d.
      END.                           
      ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsyd\pakop07.d.
      END. 
      ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\pakop07.d.
      END.         
      ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN OUTPUT TO \\pc112\delad\pro9s\korning\pakop07.d. 
      PUT "JULI MÅNAD" AT 20.
   END.
   IF MONTH(vkdatum) = 8 THEN DO:
      IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gran\pakop08.d.
      END.                   
      ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gadm\pakop08.d.
      END.   
      ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\grit\pakop08.d.
      END.
      ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
         OUTPUT TO d:\DELAD\server\PRO9S\gkal\pakop08.d.
      END.                           
      ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsyd\pakop08.d.
      END. 
      ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\pakop08.d.
      END.         
      ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN OUTPUT TO \\pc112\delad\pro9s\korning\pakop08.d.
      PUT "AUGUSTI MÅNAD" AT 20.
   END.
   IF MONTH(vkdatum) = 9 THEN DO:
      IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gran\pakop09.d.
      END.                   
      ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gadm\pakop09.d.
      END.        
      ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\grit\pakop09.d.
      END.
      ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
         OUTPUT TO d:\DELAD\server\PRO9S\gkal\pakop09.d.
      END.                      
      ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsyd\pakop09.d.
      END. 
      ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\pakop09.d.
      END.         
      ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN OUTPUT TO \\pc112\delad\pro9s\korning\pakop09.d.  
      PUT "SEPTEMBER MÅNAD" AT 20.
   END.
   IF MONTH(vkdatum) = 10 THEN DO:
      IF Guru.Konstanter:globforetag = "GRAN" THEN DO:  
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gran\pakop10.d.
      END.                   
      ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gadm\pakop10.d.
      END. 
      ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\grit\pakop10.d.
      END.
      ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
         OUTPUT TO d:\DELAD\server\PRO9S\gkal\pakop10.d.
      END.                             
      ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsyd\pakop10.d.
      END. 
      ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\pakop10.d.
      END.         
      ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN OUTPUT TO \\pc112\delad\pro9s\korning\pakop10.d.
      PUT "OKTOBER MÅNAD" AT 20.
   END.
   IF MONTH(vkdatum) = 11 THEN DO:
      IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gran\pakop11.d.
      END.                   
      ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gadm\pakop11.d.
      END.   
      ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\grit\pakop11.d.
      END. 
      ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
         OUTPUT TO d:\DELAD\server\PRO9S\gkal\pakop11.d.
      END.                            
      ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsyd\pakop11.d.
      END. 
      ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\pakop11.d.
      END.         
      ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN OUTPUT TO \\pc112\delad\pro9s\korning\pakop11.d.
      PUT "NOVEMBER MÅNAD" AT 20. 
   END.
   IF MONTH(vkdatum) = 12 THEN DO:
      IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gran\pakop12.d.
      END.                   
      ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
        OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gadm\pakop12.d.
      END.      
      ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
        OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\grit\pakop12.d.
      END.
      ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
        OUTPUT TO d:\DELAD\server\PRO9S\gkal\pakop12.d.
      END.                       
      ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsyd\pakop12.d.
      END. 
      ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\pakop12.d.
      END.         
      ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN OUTPUT TO \\pc112\delad\pro9s\korning\pakop12.d.
      PUT "DECEMBER MÅNAD" AT 20.
   END.
END.   

FOR EACH pa90filen:
  DELETE pa90filen.
END.
FOR EACH pa90fil BY pa90fil.PPERSKOD BY pa90fil.PLONTILLAGG
BY POVERTIDTILL BY PTRAKTKOD BY PBEREDSKAP:
  IF pa90fil.PPERSKOD = " " THEN DELETE pa90fil.
  ELSE IF gvisatidpermanad = FALSE THEN DO:
     IF MONTH(pa90fil.PDATUM) LE man THEN EXPORT pa90fil.
  END.
  ELSE EXPORT pa90fil.  
END.

IF Guru.Konstanter:globforetag = "GADM" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gadm\felpers.d.
   FOR EACH pa90filen:
      IF pa90filen.VIJUDID = "GRA" THEN man = man.
      ELSE IF pa90filen.VIJUDID = "SYDSP" THEN man = man.
      /*ELSE IF pa90filen.VIJUDID = "GRAEM" THEN man = man.
      ELSE IF pa90filen.VIJUDID = "GRATR" THEN man = man.  */
      ELSE DO:
         EXPORT pa90filen.
      END.
   END.
   OUTPUT CLOSE.
END.

IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\grit\felpers.d.
   FOR EACH pa90filen:
      IF pa90filen.VIJUDID = "GRA" THEN man = man.
      ELSE IF pa90filen.VIJUDID = "Fors" THEN man = man.
      ELSE IF pa90filen.VIJUDID = "Serv" THEN man = man.      
      ELSE DO:
         EXPORT pa90filen.
      END.
   END.
   OUTPUT CLOSE.
END.

IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
   OUTPUT TO d:\DELAD\server\PRO9S\gkal\felpers.d APPEND.
   FOR EACH pa90filen:
      IF pa90filen.VIJUDID = "GKEAB" THEN man = man.   
      ELSE IF pa90filen.VIJUDID = "GSEAB" THEN man = man.
      ELSE DO:
         EXPORT pa90filen.
      END.
   END.
   OUTPUT CLOSE.
END.
IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\felpers.d APPEND.
   FOR EACH pa90filen:
      IF pa90filen.VIJUDID = "GVARME" THEN man = man.         
      ELSE DO:
         EXPORT pa90filen.
      END.
   END.
   OUTPUT CLOSE.
END.
IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gran\felpers.d APPEND.
   FOR EACH pa90filen:
      IF pa90filen.PPERSKOD BEGINS "35" THEN man = man.         
      ELSE IF pa90filen.PPERSKOD BEGINS "45" THEN man = man.         
      ELSE IF pa90filen.PPERSKOD BEGINS "31" THEN man = man.                     
      ELSE IF pa90filen.PPERSKOD BEGINS "41" THEN man = man.               
      ELSE IF pa90filen.PPERSKOD BEGINS "24" THEN man = man.                     
      ELSE DO:
         EXPORT pa90filen.
      END.
   END.
   OUTPUT CLOSE.
END.
   



IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gran\pasumma.d.
END.   
ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gadm\pasumma.d.
END.  
ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\grit\pasumma.d.
END.
ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
   OUTPUT TO d:\DELAD\server\PRO9S\gkal\pasumma.d.
END.
ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsyd\pasumma.d.
END.  
ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\pasumma.d.
END.  
ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN OUTPUT TO \\pc112\delad\pro9s\korning\pasumma.d.
IF gvisatidpermanad = FALSE THEN DO:
   /* TA BORT INNEVARANDE MÅNAD OCH FÖREGÅENDE MÅNADER */
   FOR EACH pa90fil:
     IF man = 12 THEN DO:
        IF MONTH(pa90fil.PDATUM) > 6 THEN DELETE pa90fil.
     END.
     ELSE IF man = 1 THEN DO:
        IF MONTH(pa90fil.PDATUM) = 1 THEN DELETE pa90fil.
        ELSE IF MONTH(pa90fil.PDATUM) > 10 THEN DELETE pa90fil.  
     END.  
     ELSE IF MONTH(pa90fil.PDATUM) LE man THEN DELETE pa90fil.
     ELSE IF pa90fil.PPERSKOD = " " THEN DELETE pa90fil.
   END.
   /*KVAR I EKO-LÖNE-SAMMANSTÄLLNINGEN. ÄR BARA DET SOM HÖR TILL NÄSTA MÅNADSKÖRNING */
   FOR EACH pa90fil BY pa90fil.PPERSKOD BY pa90fil.PLONTILLAGG
   BY POVERTIDTILL BY PTRAKTKOD BY PBEREDSKAP:
     EXPORT pa90fil.
   END.
END.   
ELSE DO:   
   PUT " ".
END.   

FOR EACH pa90fil:
  DELETE pa90fil.
END.
RUN sammut_UI (INPUT 2).
{EUROPEANAMERICAN.I}