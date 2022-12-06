/*GSOLMAN.P*/
/*MÅNADSKÖRNING graninge*/
/* hämtar data från pasumma.d (skapas av EKO-LÖNE-SAMMANST.)
, omformar och lägger ut till pa90 ,filen som ska läsas till pa90*/
DEFINE SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.

DEFINE SHARED VARIABLE nytid AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE SHARED VARIABLE man AS INTEGER FORMAT "99" NO-UNDO.
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
DEFINE VARIABLE overrapp1 AS CHARACTER FORMAT "X(82)" NO-UNDO.      /*LÖN*/
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
   INDEX PPERSKOD IS PRIMARY PPERSKOD PTILLAGG.
{AMERICANEUROPEAN.I}
FOR EACH pa90filen:
  DELETE pa90filen.
END.
FOR EACH pa90fil:
  DELETE pa90fil.
END.
IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\gsol\pasumma.d NO-ECHO.
END.
ELSE DO:
  INPUT FROM C:\guru\pasumma.d NO-ECHO.
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
IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
   FIND FIRST pa90fil NO-LOCK NO-ERROR.
   REPEAT:
      DO TRANSACTION:
         FIND NEXT pa90fil WHERE pa90fil.PLONTILLAGG = "754" EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE pa90fil THEN LEAVE.
         ELSE DELETE pa90fil.
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
  FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pnummer
  USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
  beravt = PERSONALTAB.BEREDSKAPSAVTAL.
  FIND FIRST BEREDSKAPTAB WHERE BEREDSKAPTAB.BEREDSKAPSAVTAL = beravt
  USE-INDEX BERED NO-LOCK NO-ERROR.    
  IF lon = lone AND persnr = pnummer THEN DO:
    IF BEREDSKAPTAB.BERANTAL = 0 THEN DO:
      nytid = antal.
      RUN TIMSEK.P.
      seku = sekunder.
      nytid = pa90fil.PBERANTAL.
      RUN TIMSEK.P.
      sekunder = sekunder + seku.
      RUN SEKTIM.P.
      antal = nytid.
    END.
    ELSE IF BEREDSKAPTAB.BERANTAL > 0 THEN antal = antal + pa90fil.PBERANTAL.
  END.
  ELSE DO TRANSACTION:
    IF antal > 0 THEN DO:
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = persnr
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      beravt = PERSONALTAB.BEREDSKAPSAVTAL.
      FIND FIRST BEREDSKAPTAB WHERE BEREDSKAPTAB.BEREDSKAPSAVTAL = beravt
      USE-INDEX BERED NO-LOCK NO-ERROR.                        
      IF BEREDSKAPTAB.BERANTAL > 0 THEN DO: 
         rapphj3 = "000000".
         rapphj1 = "000000".   /*STRING(antal,"999999").*/
         FIND FIRST BERKOD WHERE BERKOD.BEREDSKAP = lon NO-LOCK NO-ERROR.
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
    FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = persnr
    USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
    beravt = PERSONALTAB.BEREDSKAPSAVTAL.
    FIND FIRST BEREDSKAPTAB WHERE BEREDSKAPTAB.BEREDSKAPSAVTAL = beravt
    USE-INDEX BERED NO-LOCK NO-ERROR.   
    IF BEREDSKAPTAB.BERANTAL > 0 THEN DO: 
         rapphj3 = "000000".
         rapphj1 = "000000".   /*STRING(antal,"999999").*/
         FIND FIRST BERKOD WHERE BERKOD.BEREDSKAP = lon NO-LOCK NO-ERROR.
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
lon="".
antal = 0.
persnr = "".
akod = "".                  
aomr = "".
/* filen pa90 sparas undan i pakopia.d innan ny pa90fil skapas */
IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gsol\LOKOLL.D \\GRANGURU\guru_ser\server\PRO9S\gsol\LOMAKO.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gsol\LOTJAN.D \\GRANGURU\guru_ser\server\PRO9S\gsol\LOMATJ.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gsol\FRKOLL.D \\GRANGURU\guru_ser\server\PRO9S\gsol\FRMAKO.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gsol\FRTJAN.D \\GRANGURU\guru_ser\server\PRO9S\gsol\FRMATJ.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gsol\LOVARM.D \\GRANGURU\guru_ser\server\PRO9S\gsol\LOMAVA.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gsol\FRVARM.D \\GRANGURU\guru_ser\server\PRO9S\gsol\FRMAVA.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gsol\FRFELMA.D \\GRANGURU\guru_ser\server\PRO9S\gsol\FRFELKO.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gsol\FRANFEL.D \\GRANGURU\guru_ser\server\PRO9S\gsol\FRFELMA.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gsol\TOM.D \\GRANGURU\guru_ser\server\PRO9S\gsol\FRANFEL.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gsol\pasumma.d \\GRANGURU\guru_ser\server\PRO9S\gsol\paloko.d.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gsol\fransu.d \\GRANGURU\guru_ser\server\PRO9S\gsol\pafrko.d.

/*    DOS SILENT \\GRANGURU\guru_ser\server\PRO9S\gren\makop.bat.    /* I  \GURU\GSU.BAT*/*/
END.
IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
  OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pa2.d.
END.

ELSE DO:
   OUTPUT TO C:\GURU\pa2.d.
END.                                
FOR EACH pa90filen BY pa90filen.PPERSKOD BY pa90filen.PTILLAGG:
  EXPORT pa90filen.
END. 
/*hur skall gsyd funka här*/                          
IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\lokoll.d.
   FOR EACH pa90filen WHERE pa90filen.PPERSKOD BEGINS "35" BY pa90filen.PPERSKOD BY pa90filen.PTILLAGG:
      IF pa90filen.POMR ="" THEN DO:
         overrapp1 = "2222" + " " + "03" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " " + 
         SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +  
         SUBSTRING(pa90filen.PTI,1 ,6) + " "  +   
         SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
         SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
         "000" + " " + "0000" + " " + "      " + " " + "EL".
      END.
      ELSE DO:
         overrapp1 = "2222" + " " + "03" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " " + 
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
ELSE DO:
  OUTPUT TO C:\GURU\lokoll.d.
END.  
IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\lotjan.d.
   FOR EACH pa90filen WHERE pa90filen.PPERSKOD BEGINS "45" BY pa90filen.PPERSKOD BY pa90filen.PTILLAGG:
      IF pa90filen.POMR = "" THEN DO:
         overrapp1 = "2222" + " " + "04" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " "+ 
         SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +    
         SUBSTRING(pa90filen.PTI,1 ,6) + " "  +  
         SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
         SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
         "000" + " " + "0000" + " " + "      " + " " + "EL".
      END.
      ELSE DO: 
         overrapp1 = "2222" + " " + "04" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " " + 
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
ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
   OUTPUT TO C:\GURU\lotjan.d.
END.
IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\lovarm.d.
   FOR EACH pa90filen WHERE pa90filen.PPERSKOD BEGINS "32" BY pa90filen.PPERSKOD BY pa90filen.PTILLAGG:
      IF pa90filen.POMR = "" THEN DO:
         overrapp1 = "3333" + " " + "02" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " "+ 
         SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +    
         SUBSTRING(pa90filen.PTI,1 ,6) + " "  +  
         SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
         SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
         "000" + " " + "0000" + " " + "      " + " " + "EL".
      END.
      ELSE DO: 
         overrapp1 = "3333" + " " + "02" + " " + SUBSTRING(pa90filen.PPERSKOD,1 ,5) + " " + 
         SUBSTRING(pa90filen.PTILLAGG,1,3) + " " +  
         SUBSTRING(pa90filen.PTI,1 ,6) + " "  +  
         SUBSTRING(pa90filen.PANTAL,1 ,6) + " "  + "0000000" + " " +
         SUBSTRING(pa90filen.PKR,1 ,9) + " " + "      " + " " + "      " + " " +
         "300" + " " + "3000" + " " + "      " + " " + "EL".
      END. 
      PUT overrapp1.   /*  pa90filen.P    */ 
      PUT SKIP.
   END.  
END.  

/* pa90 -filen summeras til pa90sum.d som innehåller alla månadskörningar */
IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
    OS-APPEND \\GRANGURU\guru_ser\server\PRO9S\gsol\LOKOLL.D \\GRANGURU\guru_ser\server\PRO9S\gsol\PA90SUM.D.
    OS-APPEND \\GRANGURU\guru_ser\server\PRO9S\gsol\LOTJAN.D \\GRANGURU\guru_ser\server\PRO9S\gsol\PA90SUM.D.
    OS-APPEND \\GRANGURU\guru_ser\server\PRO9S\gsol\LOVARM.D \\GRANGURU\guru_ser\server\PRO9S\gsol\PA90SUM.D.
   /* DOS SILENT \\GRANGURU\guru_ser\server\PRO9S\gren\sum.bat.    /* I  \GURU\GSU.BAT*/    */
END. 
FOR EACH pa90fil:
  DELETE pa90fil.
END.
/* ALLT SOM ÄR EKO-LÖNE-SAMMANST. SEDAN FÖRRA MÅNADSKÖRNING LÄGGS I ARBETSFIL */                 
IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\gsol\pasumma.d NO-ECHO.
END.
ELSE DO:
   INPUT FROM C:\GURU\pasumma.d NO-ECHO.
END.   
REPEAT TRANSACTION:
  CREATE pa90fil.
  ASSIGN.
  IMPORT pa90fil.
END.
/* KOPIA SKAPAS*/
IF gvisatidpermanad = TRUE THEN DO:
   IF MONTH(vkdatum) = 1 THEN DO:
      IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pakop01.d.
      END.          
      ELSE DO:
         OUTPUT TO C:\GURU\pakop01.d.
      END.      
      PUT "JANUARI MÅNAD" AT 20.
   END.
   IF MONTH(vkdatum) = 2 THEN DO:
      IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pakop02.d.
      END.         
      ELSE  OUTPUT TO C:\GURU\pakop02.d.     
      PUT "FEBRUARI MÅNAD" AT 20.
   END.
   IF MONTH(vkdatum) = 3 THEN DO:
      IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pakop03.d.
      END.                    
      IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsyd\pakop03.d.
      END. 
      ELSE OUTPUT TO C:\GURU\pakop03.d.    
      PUT "MARS MÅNAD" AT 20.
   END.
   IF MONTH(vkdatum) = 4 THEN DO:
      IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pakop04.d.
      END.         
      ELSE OUTPUT TO C:\GURU\pakop04.d.    
      PUT "APRIL MÅNAD" AT 20.
   END.
   IF MONTH(vkdatum) = 5 THEN DO:      
      IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pakop05.d.
      END.         
      ELSE  OUTPUT TO C:\GURU\pakop05.d.    
      PUT "MAJ MÅNAD" AT 20.
   END.
   IF MONTH(vkdatum) = 6 THEN DO:
      IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pakop06.d.
      END.         
      ELSE OUTPUT TO C:\GURU\pakop06.d.
      PUT "JUNI MÅNAD" AT 20.
   END.
   IF MONTH(vkdatum) = 7 THEN DO:
      IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pakop07.d.
      END.         
      ELSE OUTPUT TO C:\GURU\pakop07.d. 
      PUT "JULI MÅNAD" AT 20.
   END.
   IF MONTH(vkdatum) = 8 THEN DO:
      IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pakop08.d.
      END.        
      ELSE OUTPUT TO C:\GURU\pakop08.d.
      PUT "AUGUSTI MÅNAD" AT 20.
   END.
   IF MONTH(vkdatum) = 9 THEN DO:
      IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pakop09.d.
      END.         
      ELSE OUTPUT TO C:\GURU\pakop09.d.  
      PUT "SEPTEMBER MÅNAD" AT 20.
   END.
   IF MONTH(vkdatum) = 10 THEN DO:     
      IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pakop10.d.
      END.         
      ELSE OUTPUT TO C:\GURU\pakop10.d.
      PUT "OKTOBER MÅNAD" AT 20.
   END.
   IF MONTH(vkdatum) = 11 THEN DO:
      IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pakop11.d.
      END.         
      ELSE OUTPUT TO C:\GURU\pakop11.d.
      PUT "NOVEMBER MÅNAD" AT 20. 
   END.
   IF MONTH(vkdatum) = 12 THEN DO:
      IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pakop12.d.
      END.         
      ELSE OUTPUT TO C:\GURU\pakop12.d.
      PUT "DECEMBER MÅNAD" AT 20.
   END.
END.   
ELSE DO:
   IF man = 1 THEN DO:
      IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pakop01.d.
      END.         
      ELSE DO:
         OUTPUT TO C:\GURU\pakop01.d.
      END.      
      PUT "JANUARI MÅNAD" AT 20.
   END.
   IF man = 2 THEN DO:
      IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pakop02.d.
      END.         
      ELSE  OUTPUT TO C:\GURU\pakop02.d.     
      PUT "FEBRUARI MÅNAD" AT 20.
   END.
   IF man = 3 THEN DO:      
      IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pakop03.d.
      END.   
      ELSE OUTPUT TO C:\GURU\pakop03.d.    
      PUT "MARS MÅNAD" AT 20.
   END.
   IF man = 4 THEN DO:      
      IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pakop04.d.
      END.   
      ELSE OUTPUT TO C:\GURU\pakop04.d.    
      PUT "APRIL MÅNAD" AT 20.
   END.
   IF man = 5 THEN DO:      
      IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pakop05.d.
      END.   
      ELSE  OUTPUT TO C:\GURU\pakop05.d.    
      PUT "MAJ MÅNAD" AT 20.
   END.
   IF man = 6 THEN DO:     
      IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pakop06.d.
      END.   
      ELSE OUTPUT TO C:\GURU\pakop06.d.
      PUT "JUNI MÅNAD" AT 20.
   END.
   IF man = 7 THEN DO:      
      IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pakop07.d.
      END.   
      ELSE OUTPUT TO C:\GURU\pakop07.d. 
      PUT "JULI MÅNAD" AT 20.
   END.
   IF man = 8 THEN DO:      
      IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pakop08.d.
      END.   
      ELSE OUTPUT TO C:\GURU\pakop08.d.
      PUT "AUGUSTI MÅNAD" AT 20.
   END.
   IF man = 9 THEN DO:      
      IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pakop09.d.
      END.   
      ELSE OUTPUT TO C:\GURU\pakop09.d.  
      PUT "SEPTEMBER MÅNAD" AT 20.
   END.
   IF man = 10 THEN DO:      
      IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pakop10.d.
      END.   
      ELSE OUTPUT TO C:\GURU\pakop10.d.
      PUT "OKTOBER MÅNAD" AT 20.
   END.
   IF man = 11 THEN DO:      
      IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pakop11.d.
      END.   
      ELSE OUTPUT TO C:\GURU\pakop11.d.
      PUT "NOVEMBER MÅNAD" AT 20. 
   END.
   IF man = 12 THEN DO:      
      iF Guru.Konstanter:globforetag = "GSOL" THEN DO:
         OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pakop12.d.
      END.   
      ELSE OUTPUT TO C:\GURU\pakop12.d.
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
IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pasumma.d.
END.
ELSE OUTPUT TO C:\GURU\pasumma.d.
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
{EUROPEANAMERICAN.I}
