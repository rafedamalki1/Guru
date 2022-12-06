 /*GKALVE2.P*/
 /*TEMP-TABLES*/
/*SUMMERAR VECKANS KORNING + foregaende korningar i pasumma.d    */

DEFINE SHARED VARIABLE korvar LIKE FORETAG.FORETAG NO-UNDO.
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
DEFINE VARIABLE prognamn2 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE prognamn3 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE prognamn4 AS CHARACTER  NO-UNDO.
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
   FIELD MANAD AS INTEGER
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
   FIELD MANAD AS INTEGER
   FIELD PVECKONUMMER LIKE TIDREGITAB.VECKONUMMER
   FIELD PSORT AS CHARACTER FORMAT "XX"
   FIELD PKOD LIKE ANSTFORMTAB.KOD
   FIELD POMR LIKE PERSONALTAB.OMRADE.
   DEFINE VARIABLE ST AS INTEGER.
{AMERICANEUROPEAN.I}
IF Guru.Konstanter:globforetag = "GKAL" THEN prognamn2 = "d:\DELAD\server\PRO9S\gkal\".
/*IF Guru.Konstanter:globforetag = "GKAL" THEN prognamn2 = "d:\DELAD\server\PRO9S\gkal\".      */
ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN prognamn2 = "\\pc112\delad\pro9s\korning\".
/* filen pasumma.d sparas undan i pakopsum.d innan ny pasumma skapas */ 
IF korvar = "" THEN DO:
   
   prognamn3 = prognamn2 + "pasumma.d".
   prognamn4 = prognamn2 + "PAKOPSUM.D".
   OS-COPY VALUE(prognamn3) VALUE(prognamn4).      
   prognamn3 = prognamn2 + "FRANSU.D".
   prognamn4 = prognamn2 + "PAKOPFR.D".
   OS-COPY VALUE(prognamn3) VALUE(prognamn4).      
   /*IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
      OS-COPY \\extraguru\DELAD\server\PRO9S\gkal\PASUMMA.D \\extraguru\DELAD\server\PRO9S\gkal\PAKOPSUM.D.
      OS-COPY \\extraguru\DELAD\server\PRO9S\gkal\FRANSU.D \\extraguru\DELAD\server\PRO9S\gkal\PAKOPFR.D.   
   END. 
   ELSE DO:
      OS-COPY \\pc112\delad\pro9s\korning\PASUMMA.D \\pc112\delad\pro9s\korning\PAKOPSUM.D.
      OS-COPY \\pc112\delad\pro9s\korning\FRANSU.D \\pc112\delad\pro9s\korning\PAKOPFR.D.   
   END. */
END.
IF korvar = "" THEN DO:
   prognamn3 = prognamn2 + "pasumma.d".
   INPUT FROM VALUE(prognamn3) NO-ECHO.
   /*IF Guru.Konstanter:globforetag = "GKAL" THEN DO:   
      INPUT FROM \\extraguru\DELAD\server\PRO9S\gkal\pasumma.d NO-ECHO.
   END. 
   ELSE DO:
      INPUT FROM \\pc112\delad\pro9s\korning\pasumma.d NO-ECHO.
   END.   */
   REPEAT TRANSACTION:
     CREATE pa90fil.
     ASSIGN.
     IMPORT pa90fil.
   END.
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
    /*FIND FIRST LONTILL WHERE LONTILL.VILART = lon USE-INDEX VILART NO-LOCK NO-ERROR.*/
    FIND FIRST LONTILL WHERE LONTILL.LONTILLAGG = lon USE-INDEX LON NO-LOCK NO-ERROR.
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
      /*FIND FIRST LONTILL WHERE LONTILL.VILART = lon USE-INDEX VILART NO-LOCK NO-ERROR.*/
      FIND FIRST LONTILL WHERE LONTILL.LONTILLAGG = lon USE-INDEX LON NO-LOCK NO-ERROR.
      IF NOT AVAILABLE LONTILL THEN rec = rec.
      ELSE IF LONTILL.ENHET = "TI" THEN DO:
	      ASSIGN pa90sum.PSORT = "TI".
      END.
      ELSE IF LONTILL.ENHET = "KR" THEN DO:
	      ASSIGN pa90sum.PSORT = "KR".
      END.
      ELSE IF pa90sum.PLONTILLAGG = "ERSS" THEN DO: 
         /*hårdkodning , registrerat som st i LONTILL. se även GKALVE1.P */
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
    /*FIND FIRST LONTILL WHERE LONTILL.LONTILLAGG = lon NO-LOCK NO-ERROR.*/
    /*FIND FIRST LONTILL WHERE LONTILL.VILART = lon USE-INDEX VILART NO-LOCK NO-ERROR.*/
    FIND FIRST LONTILL WHERE LONTILL.LONTILLAGG = lon USE-INDEX LON NO-LOCK NO-ERROR.
    IF NOT AVAILABLE LONTILL THEN rec = rec.
    ELSE IF LONTILL.ENHET = "TI" THEN DO:
      ASSIGN pa90sum.PSORT = "TI".
    END.
    ELSE IF LONTILL.ENHET = "KR" THEN DO:
	    ASSIGN pa90sum.PSORT = "KR".
    END.
    ELSE IF pa90sum.PLONTILLAGG = "ERSS" THEN DO: 
       /*hårdkodning , registrerat som st i LONTILL. se även GKALVE1.P */
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
  IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
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
EMPTY TEMP-TABLE pa90fil NO-ERROR. 


/*flyttad översättning till VILART 20111201 lena*/
FOR EACH pa90sum where pa90sum.PLONTILLAGG NE "":
   /*FIND FIRST LONTILL WHERE LONTILL.LONTILLAGG = pa90fil.PLONTILLAGG
   AND LONTILL.KOD = pa90fil.PKOD USE-INDEX LON NO-LOCK NO-ERROR.
   IF AVAILABLE LONTILL THEN ASSIGN pa90fil.PLONTILLAGG = LONTILL.VILART.*/
   /*översättning till Personec fast vilart översatt till Hogia redan 20111201 lena*/   
   IF pa90sum.PLONTILLAGG =  "262" THEN pa90sum.PLONTILLAGG = "FÄRH".
   IF pa90sum.PLONTILLAGG = "261"  THEN pa90sum.PLONTILLAGG = "FÄRL".
   IF pa90sum.PLONTILLAGG =  "781" THEN pa90sum.PLONTILLAGG = "KLUM".
   IF pa90sum.PLONTILLAGG =  "753" THEN pa90sum.PLONTILLAGG = "KMER".
   IF pa90sum.PLONTILLAGG =  "727" THEN pa90sum.PLONTILLAGG = "MEND".
   IF pa90sum.PLONTILLAGG =  "790" THEN pa90sum.PLONTILLAGG = "MOMS".   
   IF pa90sum.PLONTILLAGG = "711"  THEN pa90sum.PLONTILLAGG = "REHA".
   IF pa90sum.PLONTILLAGG = "710"  THEN pa90sum.PLONTILLAGG = "REHE" .
   IF pa90sum.PLONTILLAGG = "764"  THEN pa90sum.PLONTILLAGG = "RKOS".
   IF pa90sum.PLONTILLAGG = "723"  THEN pa90sum.PLONTILLAGG = "TFHA".
   IF pa90sum.PLONTILLAGG = "720"  THEN pa90sum.PLONTILLAGG = "TFHE".
   IF pa90sum.PLONTILLAGG = "724"  THEN pa90sum.PLONTILLAGG = "TLHA".
   IF pa90sum.PLONTILLAGG = "721"  THEN pa90sum.PLONTILLAGG = "TLHE".
   IF pa90sum.PLONTILLAGG = "742"  THEN pa90sum.PLONTILLAGG = "UTLT".      
END.
FOR EACH pa90sum where pa90sum.POVERTIDTILL NE "":
   /*FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = pa90fil.POVERTIDTILL
   /*AND OVERKOD.KOD = pa90fil.PAVTAL*/ USE-INDEX OVER NO-LOCK NO-ERROR.
   IF AVAILABLE OVERKOD THEN ASSIGN pa90fil.POVERTIDTILL = OVERKOD.VILART.*/
   /*IF pa90sum.POVERTIDTILL = ""  THEN pa90sum.POVERTIDTILL = "".
   IF pa90sum.POVERTIDTILL = ""  THEN pa90sum.POVERTIDTILL = "".
   IF pa90sum.POVERTIDTILL = ""  THEN pa90sum.POVERTIDTILL = "".
   IF pa90sum.POVERTIDTILL = ""  THEN pa90sum.POVERTIDTILL = "".
   IF pa90sum.POVERTIDTILL = ""  THEN pa90sum.POVERTIDTILL = "".
   IF pa90sum.POVERTIDTILL = ""  THEN pa90sum.POVERTIDTILL = "".*/
END.
FOR EACH pa90sum where pa90sum.PBEREDSKAP NE "":
   /*FIND FIRST BERKOD WHERE BERKOD.BEREDSKAP = pa90fil.PBEREDSKAP 
   /*AND BERKOD.BEREDSKAPSAVTAL = pa90fil.PAVTAL*/ USE-INDEX BERE NO-LOCK NO-ERROR.
   IF AVAILABLE BERKOD THEN ASSIGN pa90fil.PBEREDSKAP = BERKOD.VILART.*/
   /*IF pa90sum.PBEREDSKAP = ""  THEN pa90sum.PBEREDSKAP = "".
   IF pa90sum.PBEREDSKAP = ""  THEN pa90sum.PBEREDSKAP = "".
   IF pa90sum.PBEREDSKAP = ""  THEN pa90sum.PBEREDSKAP = "".
   IF pa90sum.PBEREDSKAP = ""  THEN pa90sum.PBEREDSKAP = "".
   IF pa90sum.PBEREDSKAP = ""  THEN pa90sum.PBEREDSKAP = "".
   IF pa90sum.PBEREDSKAP = ""  THEN pa90sum.PBEREDSKAP = "".*/
END.
FOR EACH pa90sum where pa90sum.PTRAKTKOD NE "":
   /*FIND FIRST TRAKTATAB WHERE TRAKTATAB.TRAKTKOD = pa90fil.PTRAKTKOD
   /*AND TRAKTATAB.TRAAVTAL = pa90fil.PAVTAL*/ USE-INDEX TRAAV NO-LOCK NO-ERROR.
   IF AVAILABLE TRAKTATAB THEN ASSIGN pa90fil.PTRAKTKOD = TRAKTATAB.VILART.*/
   IF pa90sum.PTRAKTKOD = "700"  THEN pa90sum.PTRAKTKOD = "END6".
   IF pa90sum.PTRAKTKOD = "701"  THEN pa90sum.PTRAKTKOD = "EN10".
   IF pa90sum.PTRAKTKOD = "712"  THEN pa90sum.PTRAKTKOD = "TRHE".
   IF pa90sum.PTRAKTKOD = "713"  THEN pa90sum.PTRAKTKOD = "TRHA".
   IF pa90sum.PTRAKTKOD = "716"  THEN pa90sum.PTRAKTKOD = "NATT".
END.



IF korvar = "" THEN DO:
   prognamn3 = prognamn2 + "pasumma.d".
   OUTPUT TO VALUE(prognamn3).
   /*IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
      OUTPUT TO \\extraguru\DELAD\server\PRO9S\gkal\pasumma.d.
   END.
   ELSE DO:
      OUTPUT TO \\pc112\delad\pro9s\korning\pasumma.d.
   END.*/
END.
ELSE DO:
   prognamn3 = prognamn2 + "pasummaomk.d".
   OUTPUT TO VALUE(prognamn3).
   /*IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
      OUTPUT TO \\extraguru\DELAD\server\PRO9S\gkal\pasummaomk.d.
   END.
   ELSE DO:
      OUTPUT TO \\pc112\delad\pro9s\korning\pasummaomk.d.
   END.*/
END.
FOR EACH pa90sum
BY pa90sum.PPERSKOD BY pa90sum.PLONTILLAGG BY pa90sum.POVERTIDTILL
BY pa90sum.PTRAKTKOD BY pa90sum.PBEREDSKAP:
  EXPORT pa90sum.
END.
EMPTY TEMP-TABLE pa90sum NO-ERROR. 
{EUROPEANAMERICAN.I}