 /*GRANVE2.P*/
 /*TEMP-TABLES*/
/*SUMMERAR VECKANS KORNING + foregaende korningar i pasumma.d    */

DEFINE VARIABLE losen AS CHARACTER FORMAT "X(6)" NO-UNDO.
DEFINE VARIABLE persnr LIKE PERSONALTAB.PERSONALKOD NO-UNDO.     /*LÖN*/
DEFINE VARIABLE pnummer LIKE PERSONALTAB.PERSONALKOD NO-UNDO.     /*LÖN*/
DEFINE VARIABLE sor LIKE LONTILL.ENHET NO-UNDO.    /*LÖN*/
DEFINE VARIABLE lon LIKE TIDREGITAB.LONTILLAGG NO-UNDO.     /*LÖN*/
DEFINE VARIABLE lone LIKE TIDREGITAB.LONTILLAGG NO-UNDO.     /*LÖN*/
DEFINE VARIABLE antal LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.   /*LÖN*/
DEFINE VARIABLE datum AS CHARACTER FORMAT "X(6)" NO-UNDO.      /*LÖN*/
DEFINE VARIABLE regdat AS DATE NO-UNDO.      /*LÖN*/
DEFINE VARIABLE regdat1 AS DATE NO-UNDO.      /*LÖN*/
DEFINE SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE seku AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE man AS INTEGER FORMAT "99" NO-UNDO.
DEFINE VARIABLE manad AS INTEGER FORMAT "99" NO-UNDO.
DEFINE VARIABLE beravt LIKE PERSONALTAB.BEREDSKAPSAVTAL NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL FORMAT "JA/NEJ" INITIAL FALSE NO-UNDO.
DEFINE VARIABLE rec AS RECID NO-UNDO.
DEFINE VARIABLE pkod LIKE ANSTFORMTAB.KOD NO-UNDO.
DEFINE VARIABLE akod LIKE ANSTFORMTAB.KOD NO-UNDO.
DEFINE VARIABLE aomr LIKE PERSONALTAB.OMRADE NO-UNDO.
DEFINE VARIABLE pomr LIKE PERSONALTAB.OMRADE NO-UNDO.
DEFINE SHARED TEMP-TABLE pa90fil
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
DEFINE TEMP-TABLE pa90sum
   FIELD PPERSKOD LIKE PERSONALTAB.PERSONALKOD
   FIELD PLONTILLAGG LIKE TIDREGITAB.LONTILLAGG
   FIELD PLONTILLANTAL LIKE TIDREGITAB.LONTILLANTAL
   FIELD POVERTIDTILL LIKE TIDREGITAB.OVERTIDTILL
   FIELD POVERANTAL LIKE TIDREGITAB.OVERANTAL
   FIELD PTRAKTKOD LIKE TIDREGITAB.TRAKTKOD
   FIELD PTRAKTANTAL LIKE TIDREGITAB.TRAKTANTAL
   FIELD PBEREDSKAP LIKE TIDREGITAB.BEREDSKAP
   FIELD PBERANTAL AS DECIMAL FORMAT "999.99"
   FIELD PDATUM LIKE TIDREGITAB.DATUM
   FIELD PVECKONUMMER LIKE TIDREGITAB.VECKONUMMER
   FIELD PSORT AS CHARACTER FORMAT "XX"
   FIELD PKOD LIKE ANSTFORMTAB.KOD
   FIELD POMR LIKE PERSONALTAB.OMRADE.
   DEFINE VARIABLE ST AS INTEGER.
{AMERICANEUROPEAN.I}
/* filen pasumma.d sparas undan i pakopsum.d innan ny pasumma skapas */ 
IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gran\PASUMMA.D \\GRANGURU\guru_ser\server\PRO9S\gran\PAKOPSUM.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gran\FRANSU.D \\GRANGURU\guru_ser\server\PRO9S\gran\PAKOPFR.D.
   /* DOS SILENT \\GRANGURU\guru_ser\server\PRO9S\gran\vekop.bat.    /* I  \GURU\GSU.BAT*/*/
END.  
ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gadm\PASUMMA.D \\GRANGURU\guru_ser\server\PRO9S\gadm\PAKOPSUM.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gadm\FRANSU.D \\GRANGURU\guru_ser\server\PRO9S\gadm\PAKOPFR.D.
 /*   DOS SILENT \\GRANGURU\guru_ser\server\PRO9S\gadm\vekop.bat.    /* I  \GURU\GSU.BAT*/*/
END.  
ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\grit\PASUMMA.D \\GRANGURU\guru_ser\server\PRO9S\grit\PAKOPSUM.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\grit\FRANSU.D \\GRANGURU\guru_ser\server\PRO9S\grit\PAKOPFR.D.
 /*   DOS SILENT \\GRANGURU\guru_ser\server\PRO9S\grit\vekop.bat.    /* I  \GURU\GSU.BAT*/*/
END.  
ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
   OS-COPY d:\DELAD\server\PRO9S\gkal\PASUMMA.D d:\DELAD\server\PRO9S\gkal\PAKOPSUM.D.
   OS-COPY d:\DELAD\server\PRO9S\gkal\FRANSU.D d:\DELAD\server\PRO9S\gkal\PAKOPFR.D.
 /*   DOS SILENT \\GRANGURU\guru_ser\server\PRO9S\gren\vekop.bat.    /* I  \GURU\GSU.BAT*/*/
END.  
ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gkrva\PASUMMA.D \\GRANGURU\guru_ser\server\PRO9S\gkrva\PAKOPSUM.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gkrva\FRANSU.D \\GRANGURU\guru_ser\server\PRO9S\gkrva\PAKOPFR.D.
 /*   DOS SILENT \\GRANGURU\guru_ser\server\PRO9S\gadm\vekop.bat.    /* I  \GURU\GSU.BAT*/*/
END.  
ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gsyd\PASUMMA.D \\GRANGURU\guru_ser\server\PRO9S\gsyd\PAKOPSUM.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gsyd\FRANSU.D \\GRANGURU\guru_ser\server\PRO9S\gsyd\PAKOPFR.D.
   /* DOS SILENT \\GRANGURU\guru_ser\server\PRO9S\gsyd\vekop.bat.    /* I  \GURU\GSU.BAT*/*/
END. 
ELSE IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gsol\PASUMMA.D \\GRANGURU\guru_ser\server\PRO9S\gsol\PAKOPSUM.D.
   OS-COPY \\GRANGURU\guru_ser\server\PRO9S\gsol\FRANSU.D \\GRANGURU\guru_ser\server\PRO9S\gsol\PAKOPFR.D.
   /* DOS SILENT \\GRANGURU\guru_ser\server\PRO9S\gsyd\vekop.bat.    /* I  \GURU\GSU.BAT*/*/
END.  
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
ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\gkrva\pasumma.d NO-ECHO.
END.  
ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\gsyd\pasumma.d NO-ECHO.
END. 
ELSE IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
   INPUT FROM \\GRANGURU\guru_ser\server\PRO9S\gsol\pasumma.d NO-ECHO.
END. 
ELSE DO:
   INPUT FROM \\pc112\delad\pro9s\korning\pasumma.d NO-ECHO.
END.   
REPEAT TRANSACTION:
  CREATE pa90fil.
  ASSIGN.
  IMPORT pa90fil.
END.
lon="".
antal = 0.
persnr = "".    
akod = " ".
manad = 0.
regdat1 = 01/01/93.
FOR EACH pa90fil BY pa90fil.PPERSKOD BY pa90fil.PLONTILLAGG
BY pa90fil.PDATUM:
  rec = RECID(pa90fil).
  man = MONTH(pa90fil.PDATUM).
  pnummer = pa90fil.PPERSKOD.
  regdat = pa90fil.PDATUM.
  pkod = pa90fil.PKOD.
  IF pa90fil.PLONTILLAGG = "" THEN NEXT.
  lone = pa90fil.PLONTILLAGG.
  IF lon = lone AND persnr = pnummer AND manad = man THEN DO:
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
      ELSE DO:
	      antal = antal + pa90fil.PLONTILLANTAL.
      END.
      
    END.
  END.
  ELSE DO:
    IF antal NE 0 THEN DO TRANSACTION:
      CREATE pa90sum.
      ASSIGN pa90sum.PPERSKOD = persnr pa90sum.PDATUM = regdat1
      pa90sum.PLONTILLAGG = lon pa90sum.PLONTILLANTAL = antal
      pa90sum.PKOD = akod. 
      FIND FIRST LONTILL WHERE LONTILL.LONTILLAGG = lon NO-LOCK NO-ERROR.
      IF NOT AVAILABLE LONTILL THEN rec = rec.
      ELSE IF LONTILL.ENHET = "TI" THEN DO:
	      ASSIGN pa90sum.PSORT = "TI".
      END.
      ELSE IF LONTILL.ENHET = "KR" THEN DO:
	      ASSIGN pa90sum.PSORT = "KR".
      END.
    END.
    manad = man.
    antal = pa90fil.PLONTILLANTAL.
    lon = lone.
    persnr = pnummer.
    akod = pkod.
    regdat1 =regdat.
  END.
END.
IF antal NE 0 THEN DO TRANSACTION:
    CREATE pa90sum.
    ASSIGN pa90sum.PPERSKOD = persnr pa90sum.PDATUM = regdat1
    pa90sum.PLONTILLAGG = lon pa90sum.PLONTILLANTAL = antal pa90sum.PKOD = akod.
    FIND FIRST LONTILL WHERE LONTILL.LONTILLAGG = lon NO-LOCK NO-ERROR.
    IF NOT AVAILABLE LONTILL THEN rec = rec.
    ELSE IF LONTILL.ENHET = "TI" THEN DO:
      ASSIGN pa90sum.PSORT = "TI".
    END.
    ELSE IF LONTILL.ENHET = "KR" THEN DO:
	    ASSIGN pa90sum.PSORT = "KR".
    END.
END.
lon="".
antal = 0.
persnr = "".
akod = " ".
manad = 0.
regdat1 = 01/01/93.
FOR EACH pa90fil BY pa90fil.PPERSKOD BY pa90fil.POVERTIDTILL
BY pa90fil.PDATUM:
  rec = RECID(pa90fil).
  man = MONTH(pa90fil.PDATUM).
  pnummer = pa90fil.PPERSKOD.
  pkod = pa90fil.PKOD.
  regdat =pa90fil.PDATUM.
  IF pa90fil.POVERTIDTILL = "" THEN NEXT.
  lone = pa90fil.POVERTIDTILL.
  IF lon = lone AND persnr = pnummer AND manad = man THEN DO:
      nytid = antal.
      RUN TIMSEK.P.
      seku = sekunder.
      nytid = pa90fil.POVERANTAL.
      RUN TIMSEK.P.
      sekunder = sekunder + seku.
      RUN SEKTIM.P.
      antal = nytid.
  END.
  ELSE DO:
    IF antal > 0 THEN DO TRANSACTION:
      CREATE pa90sum.
      ASSIGN pa90sum.PPERSKOD = persnr pa90sum.PDATUM = regdat1
      pa90sum.POVERTIDTILL = lon pa90sum.POVERANTAL = antal
      pa90sum.PSORT = "TI"
      pa90sum.PKOD = akod.
    END.
    manad = man.
    antal = pa90fil.POVERANTAL.
    lon = lone.
    persnr = pnummer.   
    akod = pkod.
    regdat1 =regdat.
  END.
END.
IF antal > 0 THEN DO TRANSACTION:
    CREATE pa90sum.
    ASSIGN pa90sum.PPERSKOD = persnr pa90sum.PDATUM = regdat1
    pa90sum.POVERTIDTILL = lon pa90sum.POVERANTAL = antal
    pa90sum.PSORT = "TI" pa90sum.PKOD = akod.
END.
lon="".
antal = 0.
persnr = "".
akod = " ".
regdat1 = 01/01/93.
manad = 0.
FOR EACH pa90fil BY pa90fil.PPERSKOD BY pa90fil.PTRAKTKOD
BY pa90fil.PDATUM:
  rec = RECID(pa90fil).
  man = MONTH(pa90fil.PDATUM).
  pnummer = pa90fil.PPERSKOD.
  pkod = pa90fil.PKOD.
  regdat = pa90fil.PDATUM.
  IF pa90fil.PTRAKTKOD = "" THEN NEXT.
  lone = pa90fil.PTRAKTKOD.
  IF lon = lone AND persnr = pnummer AND manad = man THEN DO:
    antal = antal + pa90fil.PTRAKTANTAL.
  END.
  ELSE DO:
    IF antal > 0 THEN DO TRANSACTION:
      CREATE pa90sum.
      ASSIGN pa90sum.PPERSKOD = persnr pa90sum.PDATUM = regdat1
      pa90sum.PTRAKTKOD = lon pa90sum.PTRAKTANTAL = antal pa90sum.PKOD = akod.
    END.
    manad = man.
    antal = pa90fil.PTRAKTANTAL.
    lon = lone.
    regdat1 = regdat.
    persnr = pnummer.
    akod = pkod.
  END.
END.
IF antal > 0 THEN DO TRANSACTION:
     CREATE pa90sum.
     ASSIGN pa90sum.PPERSKOD = persnr pa90sum.PDATUM = regdat1
     pa90sum.PTRAKTKOD = lon pa90sum.PTRAKTANTAL = antal pa90sum.PKOD = akod.
END.
lon="".
antal = 0.
persnr = "".
akod = "".                       
aomr = "".
regdat1 = 01/01/93.
manad = 0.
FOR EACH pa90fil BY pa90fil.PPERSKOD BY pa90fil.PBEREDSKAP
BY pa90fil.PDATUM:
  rec = RECID(pa90fil).
  man = MONTH(pa90fil.PDATUM).
  pnummer = pa90fil.PPERSKOD.
  pkod = pa90fil.PKOD.
  pomr = pa90fil.POMR.
  regdat = pa90fil.PDATUM.
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
  IF lon = lone AND persnr = pnummer AND manad = man THEN DO:
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
    ELSE IF BEREDSKAPTAB.BERANTAL > 0 THEN DO:
      antal = antal + pa90fil.PBERANTAL.
    END.
  END.
  ELSE DO:
    IF antal > 0 THEN DO TRANSACTION:
      CREATE pa90sum.
      ASSIGN pa90sum.PPERSKOD = persnr pa90sum.PDATUM = regdat1
      pa90sum.PBEREDSKAP = lon pa90sum.PBERANTAL = antal pa90sum.PKOD = akod
      pa90sum.POMR = aomr.
      IF BEREDSKAPTAB.BERANTAL = 0 THEN DO:
	ASSIGN pa90sum.PSORT = "TI".
      END.
    END.
    manad = man.
    antal = pa90fil.PBERANTAL.
    lon = lone.
    persnr = pnummer.
    akod = pkod.         
    aomr = pomr.
    regdat1 = regdat.
  END.
END.
IF antal > 0 THEN DO TRANSACTION:
    CREATE pa90sum.
    ASSIGN pa90sum.PPERSKOD = persnr pa90sum.PDATUM = regdat1
    pa90sum.PBEREDSKAP = lon pa90sum.PBERANTAL = antal pa90sum.PKOD = akod
    pa90sum.POMR = aomr.
    IF BEREDSKAPTAB.BERANTAL = 0 THEN DO:
      ASSIGN pa90sum.PSORT = "TI".
    END.
END.
FOR EACH pa90fil:
  DELETE pa90fil.
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
ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gkrva\pasumma.d.
END.  
ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsyd\pasumma.d.
END. 
ELSE IF Guru.Konstanter:globforetag = "GSOL" THEN DO:
   OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\gsol\pasumma.d.
END.
ELSE DO:
   OUTPUT TO \\pc112\delad\pro9s\korning\pasumma.d.
END.
FOR EACH pa90sum
BY pa90sum.PPERSKOD BY pa90sum.PLONTILLAGG BY pa90sum.POVERTIDTILL
BY pa90sum.PTRAKTKOD BY pa90sum.PBEREDSKAP:
  EXPORT pa90sum.
END.
FOR EACH pa90sum:
  DELETE pa90sum.
END.
{EUROPEANAMERICAN.I}