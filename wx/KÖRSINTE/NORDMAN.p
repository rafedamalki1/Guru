/*NORDMAN.P*/
/*UNIX TEMP-TABLES*/
/*MÅNADSKÖRNING nordkraft*/
/* hämtar data från pasumma.d (skapas av EKONOMI- OCH LÖNESAMMANST.)
, omformar och lägger ut till pa90 ,filen som ska läsas till pa90*/
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.


DEFINE SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE nytid AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE SHARED VARIABLE man AS INTEGER FORMAT "99" NO-UNDO.
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.

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
DEFINE VARIABLE dtum AS DATE NO-UNDO.
DEFINE VARIABLE overrapp1 AS CHARACTER FORMAT "X(100)" NO-UNDO.      /*LÖN*/
DEFINE VARIABLE rapphj1 AS CHARACTER FORMAT "X(5)" NO-UNDO.      /*HJÄLP ANTAL*/
DEFINE VARIABLE rapphj2 AS CHARACTER FORMAT "X(9)" NO-UNDO.
DEFINE VARIABLE seku AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE beravt LIKE PERSONALTAB.BEREDSKAPSAVTAL NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL FORMAT "JA/NEJ" INITIAL FALSE NO-UNDO.
DEFINE VARIABLE rec AS RECID NO-UNDO.
DEFINE VARIABLE anr LIKE TIDREGITAB.AONR NO-UNDO.
DEFINE VARIABLE arnr LIKE TIDREGITAB.AONR NO-UNDO.
DEFINE VARIABLE dnr LIKE TIDREGITAB.DELNR NO-UNDO.
DEFINE VARIABLE drnr LIKE TIDREGITAB.DELNR NO-UNDO.
DEFINE VARIABLE vrnr LIKE TIDREGITAB.VECKONUMMER NO-UNDO.
DEFINE VARIABLE venr LIKE TIDREGITAB.VECKONUMMER NO-UNDO.
DEFINE VARIABLE typdatum AS CHARACTER FORMAT "999999" NO-UNDO. 
DEFINE VARIABLE tperiod AS INTEGER FORMAT "999" NO-UNDO.
DEFINE TEMP-TABLE pa90fil
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
   FIELD DELNR LIKE TIDREGITAB.DELNR
   INDEX PPERSONNUMMER IS PRIMARY PPERSONNUMMER ASCENDING.
DEFINE TEMP-TABLE pa90filen
   FIELD PPERSONNUMMER AS CHARACTER FORMAT "X(10)"
   FIELD PDATUMET AS CHARACTER FORMAT "X(6)"
   FIELD PDATUM LIKE TIDREGITAB.DATUM
   FIELD PTILLAGG LIKE TIDREGITAB.LONTILLAGG
   FIELD PANTAL AS CHARACTER FORMAT "X(5)"
   FIELD PKR AS CHARACTER FORMAT "X(9)"
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD PVECKONUMMER LIKE TIDREGITAB.VECKONUMMER
   INDEX PPERSONNUMMER IS PRIMARY PPERSONNUMMER ASCENDING.
DEFINE TEMP-TABLE lonfel
   FIELD PPERSONNUMMER AS CHARACTER FORMAT "X(10)"
   FIELD PDATUMET AS CHARACTER FORMAT "X(6)"
   FIELD PTILLAGG LIKE TIDREGITAB.LONTILLAGG
   FIELD PANTAL AS CHARACTER FORMAT "X(5)"
   FIELD PKR AS CHARACTER FORMAT "X(9)"
   INDEX PPERSONNUMMER IS PRIMARY PPERSONNUMMER ASCENDING.
   

{AMERICANEUROPEAN.I}
FOR EACH pa90filen:
  DELETE pa90filen.
END.
FOR EACH pa90fil:
  DELETE pa90fil.
END.
IF Guru.Konstanter:globforetag = "NORD" THEN DO:      
   OS-COPY P:\progress\frfelma.d P:\progress\frfelko.d.
   OS-COPY P:\progress\franfel.d P:\progress\frfelma.d.
/*   OS-COPY P:\progress\tom.d P:\progress\franfel.d.*/
   OS-copy P:\progress\fransu.d P:\progress\kofrsu.d.
   OS-copy P:\progress\fran1.d P:\progress\frankop.d.
   OS-copy P:\progress\pasumma.d P:\progress\kopasu.d.
   OS-copy P:\progress\pa90.reg P:\progress\pakopia.d.
END.    
IF Guru.Konstanter:globforetag = "ELPA" THEN DO:      
   OS-COPY C:\guru\frfelma.d C:\guru\frfelko.d.
   OS-COPY C:\guru\franfel.d C:\guru\frfelma.d.
  /* OS-COPY C:\guru\tom.d C:\guru\franfel.d.*/
   OS-copy C:\guru\fransu.d C:\guru\kofrsu.d.
   OS-copy C:\guru\fran1.d C:\guru\frankop.d.
   OS-copy C:\guru\pasumma.d C:\guru\kopasu.d.
   OS-copy C:\guru\pa90.reg C:\guru\pakopia.d.
END. 
IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
   IF Guru.Konstanter:globforetag = "NORD" THEN DO:
      INPUT FROM P:\progress\pasumma.d NO-ECHO.
   END.  
   ELSE DO:
      INPUT FROM C:\guru\pasumma.d NO-ECHO.
   END.    
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
FOR EACH pa90fil BY pa90fil.PPERSONNUMMER BY PLONTILLAGG:
   IF PLONTILLAGG = "" THEN NEXT.
   IF PLONTILLAGG = "FRUK" THEN DO TRANSACTION:
      FIND FIRST LONTILL WHERE LONTILL.VILART = pa90fil.PLONTILLAGG
      USE-INDEX VILART NO-LOCK NO-ERROR.
      ASSIGN pa90fil.PLONTILLAGG = "847"
      pa90fil.PLONTILLANTAL = -1 * LONTILL.ERSATTNING * pa90fil.PLONTILLANTAL.
   END.
   IF PLONTILLAGG = "LUNC" THEN DO TRANSACTION:
      FIND FIRST LONTILL WHERE LONTILL.VILART = pa90fil.PLONTILLAGG
      USE-INDEX VILART NO-LOCK NO-ERROR.
      ASSIGN pa90fil.PLONTILLAGG = "847"
      pa90fil.PLONTILLANTAL = -1 * LONTILL.ERSATTNING * pa90fil.PLONTILLANTAL.
   END.
   IF PLONTILLAGG = "MIDD" THEN DO TRANSACTION:
      FIND FIRST LONTILL WHERE LONTILL.VILART = pa90fil.PLONTILLAGG
      USE-INDEX VILART NO-LOCK NO-ERROR.
      ASSIGN pa90fil.PLONTILLAGG = "847"
      pa90fil.PLONTILLANTAL = -1 * LONTILL.ERSATTNING * pa90fil.PLONTILLANTAL.
   END.
   IF PLONTILLAGG = "FRLU" THEN DO TRANSACTION:
      FIND FIRST LONTILL WHERE LONTILL.VILART = pa90fil.PLONTILLAGG
      USE-INDEX VILART NO-LOCK NO-ERROR.
      ASSIGN pa90fil.PLONTILLAGG = "847"
      pa90fil.PLONTILLANTAL = -1 * LONTILL.ERSATTNING * pa90fil.PLONTILLANTAL.
   END.
   IF PLONTILLAGG = "FRMI" THEN DO TRANSACTION:
      FIND FIRST LONTILL WHERE LONTILL.VILART = pa90fil.PLONTILLAGG
      USE-INDEX VILART NO-LOCK NO-ERROR.
      ASSIGN pa90fil.PLONTILLAGG = "847"
      pa90fil.PLONTILLANTAL = -1 * LONTILL.ERSATTNING * pa90fil.PLONTILLANTAL.
   END.
   IF PLONTILLAGG = "LUMI" THEN DO TRANSACTION:
      FIND FIRST LONTILL WHERE LONTILL.VILART = pa90fil.PLONTILLAGG
      USE-INDEX VILART NO-LOCK NO-ERROR.
      ASSIGN pa90fil.PLONTILLAGG = "847"
      pa90fil.PLONTILLANTAL = -1 * LONTILL.ERSATTNING * pa90fil.PLONTILLANTAL.
   END.
   IF PLONTILLAGG = "FLMI" THEN DO TRANSACTION:
      FIND FIRST LONTILL WHERE LONTILL.VILART = pa90fil.PLONTILLAGG
      USE-INDEX VILART NO-LOCK NO-ERROR.
      ASSIGN pa90fil.PLONTILLAGG = "847"
      pa90fil.PLONTILLANTAL = -1 * LONTILL.ERSATTNING * pa90fil.PLONTILLANTAL.
   END.   
   IF PLONTILLAGG = "KFRU" THEN DO TRANSACTION:
      FIND FIRST LONTILL WHERE LONTILL.VILART = pa90fil.PLONTILLAGG
      USE-INDEX VILART NO-LOCK NO-ERROR.
      ASSIGN pa90fil.PLONTILLAGG = "808"
      pa90fil.PLONTILLANTAL = LONTILL.ERSATTNING * pa90fil.PLONTILLANTAL.
   END.
   IF PLONTILLAGG = "KLUN" THEN DO TRANSACTION:
      FIND FIRST LONTILL WHERE LONTILL.VILART = pa90fil.PLONTILLAGG
      USE-INDEX VILART NO-LOCK NO-ERROR.
      ASSIGN pa90fil.PLONTILLAGG = "808"
      pa90fil.PLONTILLANTAL = LONTILL.ERSATTNING * pa90fil.PLONTILLANTAL.
   END.
   IF PLONTILLAGG = "KMID" THEN DO TRANSACTION:
      FIND FIRST LONTILL WHERE LONTILL.VILART = pa90fil.PLONTILLAGG
      USE-INDEX VILART NO-LOCK NO-ERROR.
      ASSIGN pa90fil.PLONTILLAGG = "808"
      pa90fil.PLONTILLANTAL = LONTILL.ERSATTNING * pa90fil.PLONTILLANTAL.
   END.


END.    
FIND FIRST pa90fil NO-LOCK NO-ERROR.
REPEAT:
   DO TRANSACTION:
      FIND NEXT pa90fil WHERE pa90fil.PLONTILLAGG = "882" EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE pa90fil THEN LEAVE.
      ELSE DELETE pa90fil.
   END. 
END.   
FIND FIRST pa90fil NO-LOCK NO-ERROR.
REPEAT:
   DO TRANSACTION:
      FIND NEXT pa90fil WHERE pa90fil.PLONTILLAGG = "881" EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE pa90fil THEN LEAVE.
      ELSE DELETE pa90fil.
   END. 
END. 
FIND FIRST pa90fil NO-LOCK NO-ERROR.
REPEAT:
   DO TRANSACTION:
      FIND NEXT pa90fil WHERE pa90fil.PLONTILLAGG = "884" EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE pa90fil THEN LEAVE.
      ELSE DELETE pa90fil.
   END. 
END.   
  
regdatum = TODAY.
RUN REGVEC.P.
ASSIGN
tperiod = regvnr + 19.

ASSIGN
lon=""
antal = 0
persnr = ""
arnr = ""
drnr = 0
vrnr = 0.
FOR EACH pa90fil BY pa90fil.PPERSONNUMMER BY pa90fil.PLONTILLAGG
BY pa90fil.AONR  BY pa90fil.DELNR BY pa90fil.PVECKONUMMER:
   rec = RECID(pa90fil).
   pnummer = pa90fil.PPERSONNUMMER.
   IF pa90fil.PLONTILLAGG = "" THEN NEXT.
   ASSIGN
   lone = pa90fil.PLONTILLAGG
   anr = pa90fil.AONR
   dnr = pa90fil.DELNR
   venr = pa90fil.PVECKONUMMER.
   IF lon = lone AND persnr = pnummer AND arnr = anr 
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
	     RUN FSEKTIM.P.
            antal = fnytid.
         END.
         ELSE antal = antal + pa90fil.PLONTILLANTAL.
      END.
   END.
   ELSE DO TRANSACTION:
      IF antal NE 0 THEN DO:
         FIND FIRST LONTILL WHERE LONTILL.VILART = lon USE-INDEX VILART NO-LOCK NO-ERROR.
         IF NOT AVAILABLE LONTILL THEN DO:
            rapphj1 = STRING(antal,">>>99").
            rapphj2 = '         '.
         END.
         ELSE DO:
            sor = LONTILL.ENHET.
            IF sor = "KR" THEN DO:
	        antal = antal * 100.
	        rapphj1 = '     '.
	        rapphj2 =  STRING(antal,"->>>>>>99").
            END.
            ELSE IF sor = "TI" THEN DO:
	        nytid = antal.
	        RUN TIMSEK.P.
	        antal = sekunder / 36.     /* timmar i 100-delar*/
	        IF antal < 0 THEN DO:
	           rapphj1 = STRING(antal,"->>99").
	           rapphj2 = '         '.	  
	        END.
	        ELSE DO:
	           rapphj1 = STRING(antal,">>>99").
	           rapphj2 = '         '.	  
	        END.   
            END.
            ELSE DO:                   
               antal = antal * 100.
               IF antal > 99999 THEN DO:               
                  CREATE lonfel.
                  ASSIGN lonfel.PPERSONNUMMER = persnummer
                  lonfel.PDATUMET = datum
                  lonfel.PTILLAGG = lon
                  lonfel.PANTAL = STRING(antal,">>>>>99").
                  lonfel.PKR = '         '.
                  rapphj1 = '     '.
	           rapphj2 = '         '.
               END.     
               ELSE DO:
	           rapphj1 = STRING(antal,">>>99").
	           rapphj2 = '         '.
	        END.   
            END.
         END.
         persnummer = SUBSTRING(STRING(persnr),1 ,6) +
         SUBSTRING(STRING(persnr),7 ,4).
         datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
         SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".               
         dtum = pa90fil.PDATUM.
         CREATE pa90filen.
         ASSIGN pa90filen.PPERSONNUMMER = persnummer
         pa90filen.PDATUMET = datum
         pa90filen.PDATUM = dtum
         pa90filen.PTILLAGG = lon
         pa90filen.AONR = arnr
         pa90filen.DELNR = drnr
         pa90filen.PVECKONUMMER = vrnr
         pa90filen.PANTAL = rapphj1
         pa90filen.PKR = rapphj2.           
      END.
      ASSIGN
      antal = pa90fil.PLONTILLANTAL
      lon = lone
      persnr = pnummer
      arnr = anr
      drnr = dnr
      vrnr = venr.
   END.
END.
IF antal NE 0 THEN DO:
   FIND FIRST LONTILL WHERE LONTILL.VILART = lon USE-INDEX VILART NO-LOCK NO-ERROR.
   IF NOT AVAILABLE LONTILL THEN DO:
      rapphj1 = STRING(antal,">>>99").
      rapphj2 = ''.     
   END.
   ELSE DO:
      sor = LONTILL.ENHET.
      IF sor = "KR" THEN DO:
         antal = antal * 100.
	  rapphj1 = '     '.
	  rapphj2 =  STRING(antal,"->>>>>>99").
      END.
      ELSE IF sor = "TI" THEN DO:
         nytid = antal.
	  RUN TIMSEK.P.
	  antal = sekunder / 36.     /* timmar i 100-delar*/
	  IF antal < 0 THEN DO:
	     rapphj1 = STRING(antal,"->>99").
	     rapphj2 = '         '.	  
	  END.
	  ELSE DO:
	     rapphj1 = STRING(antal,">>>99").
	     rapphj2 = '         '.	  
	  END.   
      END.
      ELSE DO:      
         antal = antal * 100.
         IF antal > 99999 THEN DO:               
            CREATE lonfel.
            ASSIGN lonfel.PPERSONNUMMER = persnummer
            lonfel.PDATUMET = datum
            lonfel.PTILLAGG = lon
            lonfel.PANTAL = STRING(antal,">>>>>99").
            lonfel.PKR = '         '.
            rapphj1 = '     '.
	     rapphj2 = '         '.
         END.     
         ELSE DO:
	     rapphj1 = STRING(antal,">>>99").
	     rapphj2 = ''.
	  END.   
      END.
   END.
   FIND FIRST pa90fil WHERE RECID(pa90fil) = rec NO-LOCK NO-ERROR.
   IF NOT AVAILABLE pa90fil THEN personal = personal.
   ELSE DO TRANSACTION:
      persnummer = SUBSTRING(STRING(persnr),1 ,6) +
      SUBSTRING(STRING(persnr),7 ,4).
      datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
      SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".      
      dtum = pa90fil.PDATUM.
      CREATE pa90filen.
      ASSIGN pa90filen.PPERSONNUMMER = persnummer
      pa90filen.PDATUMET = datum
      pa90filen.PDATUM = dtum
      pa90filen.PTILLAGG = lon
      pa90filen.AONR = arnr
      pa90filen.DELNR = drnr
      pa90filen.PVECKONUMMER = vrnr
      pa90filen.PANTAL = rapphj1
      pa90filen.PKR = rapphj2.   
   END.
END.
ASSIGN
lon=""
antal = 0
persnr = ""
arnr = ""
drnr = 0
vrnr = 0.
FOR EACH pa90fil BY pa90fil.PPERSONNUMMER BY pa90fil.POVERTIDTILL
BY pa90fil.AONR  BY pa90fil.DELNR BY pa90fil.PVECKONUMMER:
   rec = RECID(pa90fil).
   pnummer = pa90fil.PPERSONNUMMER.
   IF pa90fil.POVERTIDTILL = "" THEN NEXT.
   ASSIGN
   lone = pa90fil.POVERTIDTILL
   anr = pa90fil.AONR
   dnr = pa90fil.DELNR
   venr = pa90fil.PVECKONUMMER.
   IF lon = lone AND persnr = pnummer AND arnr = anr 
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
   ELSE DO TRANSACTION:
      IF antal > 0 THEN DO:
         nytid = antal.
         RUN TIMSEK.P.
         antal = sekunder / 36.     /* timmar i 100-delar*/
         rapphj1 = STRING(antal,">>>99").
         rapphj2 = ''.
         persnummer = SUBSTRING(STRING(persnr),1 ,6) +
         SUBSTRING(STRING(persnr),7 ,4).
         datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
         SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
         dtum = pa90fil.PDATUM.
         CREATE pa90filen.
         ASSIGN pa90filen.PPERSONNUMMER = persnummer
         pa90filen.PDATUMET = datum
         pa90filen.PDATUM = dtum
         pa90filen.PTILLAGG = lon
         pa90filen.AONR = arnr
         pa90filen.DELNR = drnr
         pa90filen.PVECKONUMMER = vrnr
         pa90filen.PANTAL = rapphj1
         pa90filen.PKR = rapphj2.
      END.
      ASSIGN
      antal = pa90fil.POVERANTAL
      lon = lone
      persnr = pnummer
      arnr = anr
      drnr = dnr
      vrnr = venr.
   END.
END.
IF antal > 0 THEN DO:
   nytid = antal.
   RUN TIMSEK.P.
   antal = sekunder / 36.     /* timmar i 100-delar*/
   rapphj1 = STRING(antal,">>>99").
   rapphj2 = '         '.
   persnummer = SUBSTRING(STRING(persnr),1 ,6) +
   SUBSTRING(STRING(persnr),7 ,4).
   FIND FIRST pa90fil WHERE RECID(pa90fil) = rec NO-LOCK NO-ERROR.
   IF NOT AVAILABLE pa90fil THEN personal = personal.
   ELSE DO TRANSACTION:
      datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
      SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
      dtum = pa90fil.PDATUM.
      CREATE pa90filen.
      ASSIGN pa90filen.PPERSONNUMMER = persnummer
      pa90filen.PDATUMET = datum
      pa90filen.PDATUM = dtum
      pa90filen.PTILLAGG = lon
      pa90filen.AONR = arnr
      pa90filen.DELNR = drnr
      pa90filen.PVECKONUMMER = vrnr
      pa90filen.PANTAL = rapphj1
      pa90filen.PKR = rapphj2.
   END.
END.
ASSIGN
lon=""
antal = 0
persnr = ""
arnr = ""
drnr = 0
vrnr = 0.
FOR EACH pa90fil BY pa90fil.PPERSONNUMMER BY pa90fil.PTRAKTKOD
BY pa90fil.AONR  BY pa90fil.DELNR BY pa90fil.PVECKONUMMER:
   rec = RECID(pa90fil).
   pnummer = pa90fil.PPERSONNUMMER.
   IF pa90fil.PTRAKTKOD = "" THEN NEXT.
   ASSIGN
   lone = pa90fil.PTRAKTKOD
   anr = pa90fil.AONR
   dnr = pa90fil.DELNR
   venr = pa90fil.PVECKONUMMER.
   IF lon = lone AND persnr = pnummer 
   AND arnr = anr AND drnr = dnr AND vrnr = venr 
   THEN antal = antal + pa90fil.PTRAKTANTAL.
   ELSE DO TRANSACTION:
      IF antal > 0 THEN DO:          
         antal = antal * 100.
         rapphj1 = STRING(antal,">>>99").
         rapphj2 = ''.
         persnummer = SUBSTRING(STRING(persnr),1 ,6) +
         SUBSTRING(STRING(persnr),7 ,4).
         datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
         SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
         dtum = pa90fil.PDATUM.
         CREATE pa90filen.
         ASSIGN pa90filen.PPERSONNUMMER = persnummer
         pa90filen.PDATUMET = datum
         pa90filen.PDATUM = dtum pa90filen.PTILLAGG = lon
         pa90filen.AONR = arnr
         pa90filen.DELNR = drnr
         pa90filen.PVECKONUMMER = vrnr
         pa90filen.PANTAL = rapphj1 pa90filen.PKR = rapphj2.
      END.
      ASSIGN
      antal = pa90fil.PTRAKTANTAL
      lon = lone
      persnr = pnummer
      arnr = anr
      drnr = dnr
      vrnr = venr.
   END.
END.
IF antal > 0 THEN DO:
   DO TRANSACTION: 
      antal = antal * 100.
      rapphj1 = STRING(antal,">>>99").
      rapphj2 = ''.
      persnummer = SUBSTRING(STRING(persnr),1 ,6) +  
      SUBSTRING(STRING(persnr),7 ,4).
      FIND FIRST pa90fil WHERE RECID(pa90fil) = rec NO-LOCK NO-ERROR.
      IF NOT AVAILABLE pa90fil THEN DO:
         personal = personal.
      END.
      ELSE DO:
         datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
         SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
         dtum = pa90fil.PDATUM.
         CREATE pa90filen.
         ASSIGN pa90filen.PPERSONNUMMER = persnummer
         pa90filen.PDATUMET = datum
         pa90filen.PDATUM = dtum pa90filen.PTILLAGG = lon
         pa90filen.AONR = arnr
         pa90filen.DELNR = drnr
         pa90filen.PVECKONUMMER = vrnr
         pa90filen.PANTAL = rapphj1 pa90filen.PKR = rapphj2.
      END.
   END.  
END.
ASSIGN
lon=""
antal = 0
persnr = ""
arnr = ""
drnr = 0
vrnr = 0.
FOR EACH pa90fil BY pa90fil.PPERSONNUMMER BY pa90fil.PBEREDSKAP
BY pa90fil.AONR  BY pa90fil.DELNR BY pa90fil.PVECKONUMMER:
   ASSIGN
   rec = RECID(pa90fil)
   pnummer = pa90fil.PPERSONNUMMER
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
   IF lon = lone AND persnr = pnummer AND arnr = anr 
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
      ELSE IF BEREDSKAPTAB.BERANTAL > 0 THEN antal = antal + pa90fil.PBERANTAL.
   END.
   ELSE DO TRANSACTION:
      IF antal > 0 THEN DO:
         IF BEREDSKAPTAB.BERANTAL = 0 THEN DO:
            nytid = antal.
            RUN TIMSEK.P.
            antal = sekunder / 36.     /* timmar i 100-delar*/
         END.
         ELSE DO:
            antal = antal * 100.
         END.      
         rapphj1 = STRING(antal,">>>99").
         rapphj2 = ''.
         persnummer = SUBSTRING(STRING(persnr),1 ,6) +
         SUBSTRING(STRING(persnr),7 ,4).
         datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
         SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
         dtum = pa90fil.PDATUM.
         CREATE pa90filen.
         ASSIGN pa90filen.PPERSONNUMMER = persnummer
         pa90filen.PDATUMET = datum pa90filen.PDATUM = dtum
         pa90filen.PTILLAGG = lon
         pa90filen.AONR = arnr
         pa90filen.DELNR = drnr
         pa90filen.PVECKONUMMER = vrnr
         pa90filen.PANTAL = rapphj1 pa90filen.PKR = rapphj2.
      END.
      ASSIGN
      antal = pa90fil.PBERANTAL
      lon = lone
      persnr = pnummer
      arnr = anr
      drnr = dnr
      vrnr = venr.
   END.
END.
IF antal > 0 THEN DO:
   DO TRANSACTION:
      IF BEREDSKAPTAB.BERANTAL = 0 THEN DO:
         nytid = antal.
         RUN TIMSEK.P.
         antal = sekunder / 36.     /* timmar i 100-delar*/
      END.
      ELSE antal = antal * 100.    
      rapphj1 = STRING(antal,">>>99").
      rapphj2 = ''.
      persnummer = SUBSTRING(STRING(persnr),1 ,6) +
      SUBSTRING(STRING(persnr),7 ,4).  
      FIND FIRST pa90fil WHERE RECID(pa90fil) = rec NO-LOCK NO-ERROR.
      IF NOT AVAILABLE pa90fil THEN personal = personal.
      ELSE DO:
         datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
         SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
         dtum = pa90fil.PDATUM.
         CREATE pa90filen.
         ASSIGN pa90filen.PPERSONNUMMER = persnummer
         pa90filen.PDATUMET = datum pa90filen.PDATUM = dtum
         pa90filen.PTILLAGG = lon
         pa90filen.AONR = arnr
         pa90filen.DELNR = drnr
         pa90filen.PVECKONUMMER = vrnr
         pa90filen.PANTAL = rapphj1 pa90filen.PKR = rapphj2.
      END.
   END.
END.
ASSIGN  
lon=""
antal = 0
persnr = ""
arnr = ""
drnr = 0
vrnr = 0.
/* filen pa90 sparas undan i pakopia.d innan ny pa90fil skapas */
IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:                          
   IF Guru.Konstanter:globforetag = "NORD" THEN DO:
      OUTPUT TO P:\progress\pa2.d.
   END.
   ELSE DO:
      OUTPUT TO C:\GURU\pa2.d.
   END.
END.                                
FOR EACH pa90filen BY pa90filen.PPERSONNUMMER BY pa90filen.PTILLAGG:
   EXPORT pa90filen.
END.  
IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:                          
   IF Guru.Konstanter:globforetag = "NORD" THEN DO:
      OUTPUT TO P:\progress\pa90.reg.
   END.
   ELSE DO:
      OUTPUT TO C:\GURU\pa90.reg.
   END.
END.
FOR EACH pa90filen BY pa90filen.PPERSONNUMMER BY pa90filen.PTILLAGG:         
   typdatum = SUBSTRING(string(YEAR(pa90filen.PDATUM),"9999"),3,1) 
   + STRING(pa90filen.PVECKONUMMER,"999").
   overrapp1 = "4852" + STRING(tperiod)
   + SUBSTRING(pa90filen.PPERSONNUMMER,1 ,10) + 
   SUBSTRING(pa90filen.PTILLAGG,1,3) + " " + typdatum + "                " +  
   SUBSTRING(pa90filen.PANTAL,1 ,5) + "     "  +
   SUBSTRING(pa90filen.PKR,1 ,9). 
   ASSIGN
   SUBSTRING(overrapp1,81,8) = pa90filen.AONR + SUBSTRING(STRING(pa90filen.DELNR,"999"),2,2).
   PUT overrapp1.   /*  pa90filen.P    */
   PUT SKIP.
END.
OUTPUT CLOSE.
IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:                          
   IF Guru.Konstanter:globforetag = "NORD" THEN DO:
      OUTPUT TO P:\progress\franfel.d APPEND NO-ECHO.      
   END.
   ELSE DO:
      OUTPUT TO C:\GURU\franfel.d APPEND NO-ECHO.
   END.
END.
FOR EACH lonfel BY lonfel.PPERSONNUMMER BY lonfel.PTILLAGG:   
   overrapp1 = "4852000" + SUBSTRING(lonfel.PPERSONNUMMER,1 ,10) + 
   SUBSTRING(lonfel.PTILLAGG,1,3) + "                     " +  
   SUBSTRING(lonfel.PANTAL,1 ,5) + "     "  +
   SUBSTRING(lonfel.PKR,1 ,9). 
   PUT overrapp1.   /*  pa90filen.P    */
   PUT SKIP.
END.
OUTPUT CLOSE.
/* pa90 -filen summeras til pa90sum.d som innehåller alla månadskörningar */
FOR EACH pa90fil:
   DELETE pa90fil.
END.
/* ALLT SOM ÄR EKONOMI- OCH LÖNESAMMANSTÄLLT SEDAN FÖRRA MÅNADSKÖRNING LÄÄGGS I ARBETSFIL */
IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:                     
   IF Guru.Konstanter:globforetag = "NORD" THEN DO:
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
/* KOPIA SKAPAS*/
IF gvisatidpermanad = TRUE THEN DO:
    IF MONTH(vkdatum) = 1 THEN DO:       
      IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            OUTPUT TO P:\progress\pakop01.d.
         END.
         ELSE DO:
            OUTPUT TO C:\GURU\pakop01.d.
         END. 
      END.     
      IF OPSYS = "UNIX" THEN OUTPUT TO /ekonomi/atr/pakop01.d.
      PUT "JANUARI MÅNAD" AT 20.
    END.
    IF MONTH(vkdatum) = 2 THEN DO:
      IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            OUTPUT TO P:\progress\pakop02.d.
         END.
         ELSE DO:
            OUTPUT TO C:\GURU\pakop02.d.
         END.
      END.      
      IF OPSYS = "UNIX" THEN OUTPUT TO /ekonomi/atr/pakop02.d.
      PUT "FEBRUARI MÅNAD" AT 20.
    END.
    IF MONTH(vkdatum) = 3 THEN DO:
      IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            OUTPUT TO P:\progress\pakop03.d.
         END.
         ELSE DO:
            OUTPUT TO C:\GURU\pakop03.d.
         END.   
      END.   
      IF OPSYS = "UNIX" THEN OUTPUT TO /ekonomi/atr/pakop03.d.
      PUT "MARS MÅNAD" AT 20.
    END.
    IF MONTH(vkdatum) = 4 THEN DO:
      IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            OUTPUT TO P:\progress\pakop04.d.
         END.
         ELSE DO:
            OUTPUT TO C:\GURU\pakop04.d.
         END.
      END.      
      IF OPSYS = "UNIX" THEN OUTPUT TO /ekonomi/atr/pakop04.d.
      PUT "APRIL MÅNAD" AT 20.
    END.
    IF MONTH(vkdatum) = 5 THEN DO:
      IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            OUTPUT TO P:\progress\pakop05.d.
         END.
         ELSE DO:
            OUTPUT TO C:\GURU\pakop05.d.
         END.   
      END.   
      IF OPSYS = "UNIX" THEN OUTPUT TO /ekonomi/atr/pakop05.d.
      PUT "MAJ MÅNAD" AT 20.
    END.
    IF MONTH(vkdatum) = 6 THEN DO:
      IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            OUTPUT TO P:\progress\pakop06.d.
         END.
         ELSE DO:
            OUTPUT TO C:\GURU\pakop06.d.
         END.   
      END.   
      IF OPSYS = "UNIX" THEN OUTPUT TO /ekonomi/atr/pakop06.d.
      PUT "JUNI MÅNAD" AT 20.
    END.
    IF MONTH(vkdatum) = 7 THEN DO:
      IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            OUTPUT TO P:\progress\pakop07.d.
         END.
         ELSE DO:
            OUTPUT TO C:\GURU\pakop07.d.
         END.   
      END.   
      IF OPSYS = "UNIX" THEN OUTPUT TO /ekonomi/atr/pakop07.d.
      PUT "JULI MÅNAD" AT 20.
    END.
    IF MONTH(vkdatum) = 8 THEN DO:
      IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            OUTPUT TO P:\progress\pakop08.d.
         END.
         ELSE DO:
            OUTPUT TO C:\GURU\pakop08.d.
         END.   
      END.   
      IF OPSYS = "UNIX" THEN OUTPUT TO /ekonomi/atr/pakop08.d.
      PUT "AUGUSTI MÅNAD" AT 20.
    END.
    IF MONTH(vkdatum) = 9 THEN DO:
      IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            OUTPUT TO P:\progress\pakop09.d.
         END.
         ELSE DO:
            OUTPUT TO C:\GURU\pakop09.d.
         END.   
      END.   
      IF OPSYS = "UNIX" THEN OUTPUT TO /ekonomi/atr/pakop09.d.
      PUT "SEPTEMBER MÅNAD" AT 20.
    END.
    IF MONTH(vkdatum) = 10 THEN DO:
      IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            OUTPUT TO P:\progress\pakop10.d.
         END.
         ELSE DO:
            OUTPUT TO C:\GURU\pakop10.d.
         END.   
      END.   
      IF OPSYS = "UNIX" THEN OUTPUT TO /ekonomi/atr/pakop10.d.
      PUT "OKTOBER MÅNAD" AT 20.
    END.
    IF MONTH(vkdatum) = 11 THEN DO:
      IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            OUTPUT TO P:\progress\pakop11.d.
         END.
         ELSE DO:
            OUTPUT TO C:\GURU\pakop11.d.
         END.   
      END.   
      IF OPSYS = "UNIX" THEN OUTPUT TO /ekonomi/atr/pakop11.d.
      PUT "NOVEMBER MÅNAD" AT 20.
    END.
    IF MONTH(vkdatum) = 12 THEN DO:
      IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            OUTPUT TO P:\progress\pakop12.d.
         END.
         ELSE DO:
            OUTPUT TO C:\GURU\pakop12.d.
         END.   
      END.   
      IF OPSYS = "UNIX" THEN OUTPUT TO /ekonomi/atr/pakop12.d.
      PUT "DECEMBER MÅNAD" AT 20.
    END.
END.    
ELSE DO:
    IF man = 1 THEN DO:
      IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            OUTPUT TO P:\progress\pakop01.d.
         END.
         ELSE DO:
            OUTPUT TO C:\GURU\pakop01.d.
         END. 
      END.     
      IF OPSYS = "UNIX" THEN OUTPUT TO /ekonomi/atr/pakop01.d.
      PUT "JANUARI MÅNAD" AT 20.
    END.
    IF man = 2 THEN DO:
      IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            OUTPUT TO P:\progress\pakop02.d.
         END.
         ELSE DO:
            OUTPUT TO C:\GURU\pakop02.d.
         END.
      END.      
      IF OPSYS = "UNIX" THEN OUTPUT TO /ekonomi/atr/pakop02.d.
      PUT "FEBRUARI MÅNAD" AT 20.
    END.
    IF man = 3 THEN DO:
      IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            OUTPUT TO P:\progress\pakop03.d.
         END.
         ELSE DO:
            OUTPUT TO C:\GURU\pakop03.d.
         END.   
      END.   
      IF OPSYS = "UNIX" THEN OUTPUT TO /ekonomi/atr/pakop03.d.
      PUT "MARS MÅNAD" AT 20.
    END.
    IF man = 4 THEN DO:
      IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            OUTPUT TO P:\progress\pakop04.d.
         END.
         ELSE DO:
            OUTPUT TO C:\GURU\pakop04.d.
         END.
      END.      
      IF OPSYS = "UNIX" THEN OUTPUT TO /ekonomi/atr/pakop04.d.
      PUT "APRIL MÅNAD" AT 20.
    END.
    IF man = 5 THEN DO:
      IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            OUTPUT TO P:\progress\pakop05.d.
         END.
         ELSE DO:
            OUTPUT TO C:\GURU\pakop05.d.
         END.   
      END.   
      IF OPSYS = "UNIX" THEN OUTPUT TO /ekonomi/atr/pakop05.d.
      PUT "MAJ MÅNAD" AT 20.
    END.
    IF man = 6 THEN DO:
      IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            OUTPUT TO P:\progress\pakop06.d.
         END.
         ELSE DO:
            OUTPUT TO C:\GURU\pakop06.d.
         END.   
      END.   
      IF OPSYS = "UNIX" THEN OUTPUT TO /ekonomi/atr/pakop06.d.
      PUT "JUNI MÅNAD" AT 20.
    END.
    IF man = 7 THEN DO:
      IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            OUTPUT TO P:\progress\pakop07.d.
         END.
         ELSE DO:
            OUTPUT TO C:\GURU\pakop07.d.
         END.   
      END.   
      IF OPSYS = "UNIX" THEN OUTPUT TO /ekonomi/atr/pakop07.d.
      PUT "JULI MÅNAD" AT 20.
    END.
    IF man = 8 THEN DO:
      IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            OUTPUT TO P:\progress\pakop08.d.
         END.
         ELSE DO:
            OUTPUT TO C:\GURU\pakop08.d.
         END.   
      END.   
      IF OPSYS = "UNIX" THEN OUTPUT TO /ekonomi/atr/pakop08.d.
      PUT "AUGUSTI MÅNAD" AT 20.
    END.
    IF man = 9 THEN DO:
      IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            OUTPUT TO P:\progress\pakop09.d.
         END.
         ELSE DO:
            OUTPUT TO C:\GURU\pakop09.d.
         END.   
      END.   
      IF OPSYS = "UNIX" THEN OUTPUT TO /ekonomi/atr/pakop09.d.
      PUT "SEPTEMBER MÅNAD" AT 20.
    END.
    IF man = 10 THEN DO:
      IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            OUTPUT TO P:\progress\pakop10.d.
         END.
         ELSE DO:
            OUTPUT TO C:\GURU\pakop10.d.
         END.   
      END.   
      IF OPSYS = "UNIX" THEN OUTPUT TO /ekonomi/atr/pakop10.d.
      PUT "OKTOBER MÅNAD" AT 20.
    END.
    IF man = 11 THEN DO:
      IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            OUTPUT TO P:\progress\pakop11.d.
         END.
         ELSE DO:
            OUTPUT TO C:\GURU\pakop11.d.
         END.   
      END.   
      IF OPSYS = "UNIX" THEN OUTPUT TO /ekonomi/atr/pakop11.d.
      PUT "NOVEMBER MÅNAD" AT 20.
    END.
    IF man = 12 THEN DO:
      IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            OUTPUT TO P:\progress\pakop12.d.
         END.
         ELSE DO:
            OUTPUT TO C:\GURU\pakop12.d.
         END.   
      END.   
      IF OPSYS = "UNIX" THEN OUTPUT TO /ekonomi/atr/pakop12.d.
      PUT "DECEMBER MÅNAD" AT 20.
    END.
END.    
   

FOR EACH pa90filen:
  DELETE pa90filen.
END.
FOR EACH pa90fil BY pa90fil.PPERSONNUMMER BY pa90fil.PLONTILLAGG
BY POVERTIDTILL BY PTRAKTKOD BY PBEREDSKAP:
  IF pa90fil.PPERSONNUMMER = " " THEN DELETE pa90fil. 
  ELSE IF gvisatidpermanad = FALSE THEN DO:
     IF MONTH(pa90fil.PDATUM) LE man THEN EXPORT pa90fil.
  END.
  ELSE EXPORT pa90fil.
END.
IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO:
  IF Guru.Konstanter:globforetag = "NORD" THEN DO:
     OUTPUT TO P:\progress\pasumma.d.
  END.
  ELSE DO:
     OUTPUT TO C:\GURU\pasumma.d.
  END.
END.
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
      ELSE IF pa90fil.PPERSONNUMMER = " " THEN DELETE pa90fil.
    END.
    /*KVAR I EKONOMI- OCH LÖNESAMMANSTÄLLNINGEN ÄR BARA DET SOM HÖR TILL NÄSTA MÅNADSKÖRNING */
    FOR EACH pa90fil BY pa90fil.PPERSONNUMMER BY pa90fil.PLONTILLAGG
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