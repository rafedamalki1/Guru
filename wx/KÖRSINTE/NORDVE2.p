 /*NORDVE2.P*/
 /*TEMP-TABLES*/
/*SUMMERAR VECKANS KORNING + foregaende korningar i pasumma.d    */

DEFINE VARIABLE personal LIKE PERSONALTAB.PERSONALKOD NO-UNDO.      /*LÖN*/
DEFINE VARIABLE losen AS CHARACTER FORMAT "X(6)" NO-UNDO.
DEFINE VARIABLE persnr LIKE PERSONALTAB.PERSONNUMMER NO-UNDO.     /*LÖN*/
DEFINE VARIABLE pnummer LIKE PERSONALTAB.PERSONNUMMER NO-UNDO.     /*LÖN*/
DEFINE VARIABLE sor LIKE LONTILL.ENHET NO-UNDO.    /*LÖN*/
DEFINE VARIABLE lon LIKE TIDREGITAB.LONTILLAGG NO-UNDO.     /*LÖN*/
DEFINE VARIABLE lone LIKE TIDREGITAB.LONTILLAGG NO-UNDO.     /*LÖN*/
DEFINE VARIABLE antal LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.   /*LÖN*/
DEFINE VARIABLE persnummer AS CHARACTER FORMAT "X(10)" NO-UNDO.      /*LÖN*/
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
DEFINE VARIABLE anr LIKE TIDREGITAB.AONR NO-UNDO.
DEFINE VARIABLE arnr LIKE TIDREGITAB.AONR NO-UNDO.
DEFINE VARIABLE dnr LIKE TIDREGITAB.DELNR NO-UNDO.
DEFINE VARIABLE drnr LIKE TIDREGITAB.DELNR NO-UNDO.
DEFINE VARIABLE vrnr LIKE TIDREGITAB.VECKONUMMER NO-UNDO.
DEFINE VARIABLE venr LIKE TIDREGITAB.VECKONUMMER NO-UNDO.
DEFINE SHARED TEMP-TABLE pa90fil
   FIELD PPERSONNUMMER LIKE PERSONALTAB.PERSONNUMMER
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
   FIELD PPAR8 AS DECIMAL FORMAT "-99.99"
   FIELD PANSTNR LIKE PERSONALTAB.ANSTNR
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD PAVTAL AS CHARACTER
   INDEX PPERSONNUMMER IS PRIMARY PPERSONNUMMER ASCENDING.
  /* FIELD PVECKONUMMER LIKE TIDREGITAB.VECKONUMMER
   FIELD PPAR8 AS DECIMAL FORMAT "-99.99".*/
DEFINE TEMP-TABLE pa90sum
   FIELD PPERSONNUMMER LIKE PERSONALTAB.PERSONNUMMER
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
   FIELD PPAR8 AS DECIMAL
   FIELD PANSTNR LIKE PERSONALTAB.ANSTNR
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR.
  /* FIELD PVECKONUMMER LIKE TIDREGITAB.VECKONUMMER
   FIELD PPAR8 AS DECIMAL.*/
   DEFINE VARIABLE ST AS INTEGER.
/* filen pasumma.d sparas undan i pakopsum.d innan ny pasumma skapas */
{AMERICANEUROPEAN.I}
IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:   
   IF globforetag = "NORD" THEN DO:
     /* DOS SILENT P:\progress\vekop.bat.*/     
      OS-COPY P:\progress\PASUMMA.D P:\progress\PAKOPSUM.D.
      OS-COPY P:\progress\FRANSU.D P:\progress\PAKOPFR.D.     
   END.
   ELSE DO:       
      DOS SILENT copy C:\GURU\pasumma.d c:\GURU\pakopsum.d.
   END.   
END.   
IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:   
   IF globforetag = "NORD" THEN DO:
      INPUT FROM P:\progress\pasumma.d NO-ECHO.
   END.   
   ELSE DO:
      INPUT FROM C:\GURU\pasumma.d NO-ECHO.
   END.   
END.
REPEAT TRANSACTION:
   CREATE pa90fil.
   ASSIGN.
   IMPORT pa90fil.
END.
ASSIGN
lon=""
antal = 0
persnr = ""
manad = 0
regdat1 = 01/01/93
arnr = ""
drnr = 0
vrnr = 0.
FOR EACH pa90fil BY pa90fil.PPERSONNUMMER BY pa90fil.PLONTILLAGG 
BY pa90fil.AONR  BY pa90fil.DELNR BY pa90fil.PVECKONUMMER:
   rec = RECID(pa90fil).
   man = MONTH(pa90fil.PDATUM).
   pnummer = pa90fil.PPERSONNUMMER.
   regdat = pa90fil.PDATUM.
   IF pa90fil.PLONTILLAGG = "" THEN NEXT.
   ASSIGN
   lone = pa90fil.PLONTILLAGG
   anr = pa90fil.AONR
   dnr = pa90fil.DELNR
   venr = pa90fil.PVECKONUMMER.
   IF lon = lone AND persnr = pnummer AND manad = man AND arnr = anr 
   AND drnr = dnr AND vrnr = venr THEN DO:
      FIND FIRST LONTILL WHERE LONTILL.VILART = lon USE-INDEX VILART NO-LOCK NO-ERROR.
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
         ELSE  antal = antal + pa90fil.PLONTILLANTAL.
      END.
   END.
   ELSE DO:
      IF antal NE 0 THEN DO TRANSACTION:
         CREATE pa90sum.
         ASSIGN pa90sum.PPERSONNUMMER = persnr pa90sum.PDATUM = regdat1
         pa90sum.PLONTILLAGG = lon pa90sum.PLONTILLANTAL = antal
         pa90sum.AONR = arnr pa90sum.DELNR = drnr pa90sum.PVECKONUMMER = vrnr. 
         FIND FIRST LONTILL WHERE LONTILL.VILART = lon USE-INDEX VILART NO-LOCK NO-ERROR.
         IF NOT AVAILABLE LONTILL THEN personal = personal.
         ELSE IF LONTILL.ENHET = "TI" THEN ASSIGN pa90sum.PSORT = "TI".
      END.
      ASSIGN
      manad = man
      antal = pa90fil.PLONTILLANTAL
      lon = lone
      persnr = pnummer
      regdat1 =regdat
      arnr = anr
      drnr = dnr
      vrnr = venr.
   END.
END.
IF antal NE 0 THEN DO TRANSACTION:
   CREATE pa90sum.
   ASSIGN pa90sum.PPERSONNUMMER = persnr pa90sum.PDATUM = regdat1
   pa90sum.PLONTILLAGG = lon pa90sum.PLONTILLANTAL = antal
   pa90sum.AONR = arnr pa90sum.DELNR = drnr pa90sum.PVECKONUMMER = vrnr.
   FIND FIRST LONTILL WHERE LONTILL.VILART = lon USE-INDEX VILART NO-LOCK NO-ERROR.
   IF NOT AVAILABLE LONTILL THEN personal = personal.
   ELSE IF LONTILL.ENHET = "TI" THEN ASSIGN pa90sum.PSORT = "TI".
END.
ASSIGN
lon=""
antal = 0
persnr = ""
manad = 0
regdat1 = 01/01/93.
FOR EACH pa90fil BY pa90fil.PPERSONNUMMER BY pa90fil.POVERTIDTILL
BY pa90fil.AONR  BY pa90fil.DELNR BY pa90fil.PVECKONUMMER:
   ASSIGN
   rec = RECID(pa90fil)
   man = MONTH(pa90fil.PDATUM)
   pnummer = pa90fil.PPERSONNUMMER
   regdat =pa90fil.PDATUM.
   IF pa90fil.POVERTIDTILL = "" THEN NEXT.
   ASSIGN
   lone = pa90fil.POVERTIDTILL
   anr = pa90fil.AONR
   dnr = pa90fil.DELNR
   venr = pa90fil.PVECKONUMMER.
   IF lon = lone AND persnr = pnummer AND manad = man AND arnr = anr 
   AND drnr = dnr AND vrnr = venr THEN DO:
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
        ASSIGN pa90sum.PPERSONNUMMER = persnr pa90sum.PDATUM = regdat1
        pa90sum.POVERTIDTILL = lon pa90sum.POVERANTAL = antal
        pa90sum.PSORT = "TI" pa90sum.AONR = arnr pa90sum.DELNR = drnr pa90sum.PVECKONUMMER = vrnr.
     END.
     ASSIGN
     manad = man
     antal = pa90fil.POVERANTAL
     lon = lone
     persnr = pnummer
     regdat1 =regdat
     arnr = anr
     drnr = dnr
     vrnr = venr.
  END.
END.
IF antal > 0 THEN DO TRANSACTION:
   CREATE pa90sum.
   ASSIGN pa90sum.PPERSONNUMMER = persnr pa90sum.PDATUM = regdat1
   pa90sum.POVERTIDTILL = lon pa90sum.POVERANTAL = antal
   pa90sum.PSORT = "TI" pa90sum.AONR = arnr pa90sum.DELNR = drnr pa90sum.PVECKONUMMER = vrnr.
END.
ASSIGN
lon=""
antal = 0
persnr = ""
regdat1 = 01/01/93
manad = 0.
FOR EACH pa90fil BY pa90fil.PPERSONNUMMER BY pa90fil.PTRAKTKOD
BY pa90fil.AONR  BY pa90fil.DELNR BY pa90fil.PVECKONUMMER:
   ASSIGN
   rec = RECID(pa90fil)
   man = MONTH(pa90fil.PDATUM)
   pnummer = pa90fil.PPERSONNUMMER
   regdat = pa90fil.PDATUM.
   IF pa90fil.PTRAKTKOD = "" THEN NEXT.
   ASSIGN
   lone = pa90fil.PTRAKTKOD
   anr = pa90fil.AONR
   dnr = pa90fil.DELNR
   venr = pa90fil.PVECKONUMMER.
   IF lon = lone AND persnr = pnummer AND manad = man AND arnr = anr 
   AND drnr = dnr AND vrnr = venr THEN DO:
      antal = antal + pa90fil.PTRAKTANTAL.
   END.
   ELSE DO:
      IF antal > 0 THEN DO TRANSACTION:
         CREATE pa90sum.
         ASSIGN pa90sum.PPERSONNUMMER = persnr pa90sum.PDATUM = regdat1
         pa90sum.PTRAKTKOD = lon pa90sum.PTRAKTANTAL = antal
         pa90sum.AONR = arnr pa90sum.DELNR = drnr pa90sum.PVECKONUMMER = vrnr.
      END.
      ASSIGN
      manad = man
      antal = pa90fil.PTRAKTANTAL
      lon = lone
      regdat1 = regdat
      persnr = pnummer
      arnr = anr
      drnr = dnr
      vrnr = venr.
   END.
END.
IF antal > 0 THEN DO TRANSACTION:
   CREATE pa90sum.
   ASSIGN pa90sum.PPERSONNUMMER = persnr pa90sum.PDATUM = regdat1
   pa90sum.PTRAKTKOD = lon pa90sum.PTRAKTANTAL = antal 
   pa90sum.AONR = arnr pa90sum.DELNR = drnr pa90sum.PVECKONUMMER = vrnr.
END.
ASSIGN
lon=""
antal = 0
persnr = ""
regdat1 = 01/01/93
manad = 0.
FOR EACH pa90fil BY pa90fil.PPERSONNUMMER BY pa90fil.PBEREDSKAP
BY pa90fil.AONR  BY pa90fil.DELNR BY pa90fil.PVECKONUMMER:
   ASSIGN
   rec = RECID(pa90fil)
   man = MONTH(pa90fil.PDATUM)
   pnummer = pa90fil.PPERSONNUMMER
   regdat = pa90fil.PDATUM
   lone = pa90fil.PBEREDSKAP
   anr = pa90fil.AONR
   dnr = pa90fil.DELNR
   venr = pa90fil.PVECKONUMMER.
   IF pa90fil.PBEREDSKAP = "" THEN NEXT.
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONNUMMER = pnummer
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   beravt = PERSONALTAB.BEREDSKAPSAVTAL.
   FIND FIRST BEREDSKAPTAB WHERE BEREDSKAPTAB.BEREDSKAPSAVTAL = beravt
   USE-INDEX BERED NO-LOCK NO-ERROR.
   IF lon = lone AND persnr = pnummer AND manad = man AND arnr = anr 
   AND drnr = dnr AND vrnr = venr THEN DO:
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
         ASSIGN pa90sum.PPERSONNUMMER = persnr pa90sum.PDATUM = regdat1
         pa90sum.PBEREDSKAP = lon pa90sum.PBERANTAL = antal
         pa90sum.AONR = arnr pa90sum.DELNR = drnr pa90sum.PVECKONUMMER = vrnr.
         IF BEREDSKAPTAB.BERANTAL = 0 THEN ASSIGN pa90sum.PSORT = "TI".
      END.
      ASSIGN
      manad = man
      antal = pa90fil.PBERANTAL
      lon = lone
      persnr = pnummer
      regdat1 = regdat
      arnr = anr
      drnr = dnr
      vrnr = venr.
   END.
END.
IF antal > 0 THEN DO TRANSACTION:
   CREATE pa90sum.
   ASSIGN pa90sum.PPERSONNUMMER = persnr pa90sum.PDATUM = regdat1
   pa90sum.PBEREDSKAP = lon pa90sum.PBERANTAL = antal
   pa90sum.AONR = arnr pa90sum.DELNR = drnr pa90sum.PVECKONUMMER = vrnr.
   IF BEREDSKAPTAB.BERANTAL = 0 THEN ASSIGN pa90sum.PSORT = "TI".
END.
FOR EACH pa90fil:
   DELETE pa90fil.
END.

IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:   
   IF globforetag = "NORD" THEN DO:
      OUTPUT TO P:\progress\pasumma.d.
   END.
   ELSE DO:
      OUTPUT TO C:\GURU\pasumma.d.
   END.      
END.
FOR EACH pa90sum
BY pa90sum.PPERSONNUMMER BY pa90sum.PLONTILLAGG BY pa90sum.POVERTIDTILL
BY pa90sum.PTRAKTKOD BY pa90sum.PBEREDSKAP:
   EXPORT pa90sum.
END.
FOR EACH pa90sum:
   DELETE pa90sum.
END.
{EUROPEANAMERICAN.I}