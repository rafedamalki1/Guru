/*ESVE3.P*/
/*UNIX TEMP-TABLES*/
/*MÅNADSKÖRNING nordkraft*/
/* hämtar data från pasumma.d (skapas av EKONOMI- OCH LÖNESAMMANST.)
, omformar och lägger ut till pa90 ,filen som ska läsas till pa90*/
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.

DEFINE SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE nytid AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE SHARED VARIABLE man AS INTEGER FORMAT "99" NO-UNDO.
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.

DEFINE VARIABLE personal LIKE PERSONALTAB.PERSONALKOD NO-UNDO.      /*LÖN*/
DEFINE VARIABLE losen AS CHARACTER FORMAT "X(6)" NO-UNDO.
DEFINE VARIABLE persnr LIKE PERSONALTAB.ANSTNR NO-UNDO.     /*LÖN*/
DEFINE VARIABLE pnummer LIKE PERSONALTAB.ANSTNR NO-UNDO.     /*LÖN*/
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
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(41)" NO-UNDO.
DEFINE VARIABLE anstall AS CHARACTER FORMAT "X(10)" NO-UNDO.
DEFINE VARIABLE blag AS CHARACTER FORMAT "X(2)" NO-UNDO.
DEFINE VARIABLE blg AS CHARACTER FORMAT "X(2)" NO-UNDO.
DEFINE SHARED TEMP-TABLE pa90fil
   FIELD PPERSONNUMMER LIKE PERSONALTAB.PERSONNUMMER
   FIELD PLONTILLAGG LIKE TIDREGITAB.LONTILLAGG
   FIELD PLONTILLANTAL AS DECIMAL FORMAT "-999.99"
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
   FIELD BOLAG AS CHARACTER FORMAT "X(2)"
   INDEX PANSTNR IS PRIMARY PANSTNR ASCENDING.
DEFINE TEMP-TABLE pa90filen
   FIELD PANSTNR AS CHARACTER FORMAT "X(10)"
   FIELD PDATUMET AS CHARACTER FORMAT "X(6)"
   FIELD PDATUM LIKE TIDREGITAB.DATUM
   FIELD PTILLAGG LIKE TIDREGITAB.LONTILLAGG
   FIELD PANTAL AS CHARACTER FORMAT "X(5)"
   FIELD PKR AS CHARACTER FORMAT "X(9)"
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD PVECKONUMMER LIKE TIDREGITAB.VECKONUMMER
   FIELD BOLAG AS CHARACTER FORMAT "X(2)"
   INDEX PANSTNR IS PRIMARY PANSTNR ASCENDING.
DEFINE TEMP-TABLE lonfel
   FIELD PANSTNR AS CHARACTER FORMAT "X(10)"
   FIELD PDATUMET AS CHARACTER FORMAT "X(6)"
   FIELD PTILLAGG LIKE TIDREGITAB.LONTILLAGG
   FIELD PANTAL AS CHARACTER FORMAT "X(5)"
   FIELD PKR AS CHARACTER FORMAT "X(9)"
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   INDEX PANSTNR IS PRIMARY PANSTNR ASCENDING.
   


FOR EACH pa90filen:
  DELETE pa90filen.
END.

/*FOR EACH pa90fil BY pa90fil.PANSTNR BY PLONTILLAGG:
   IF PLONTILLAGG = "" THEN NEXT.
   IF PLONTILLAGG = "FRUK" THEN DO TRANSACTION:
      FIND FIRST LONTILL WHERE LONTILL.VILART = pa90fil.PLONTILLAGG
      USE-INDEX VILART NO-LOCK NO-ERROR.
      ASSIGN pa90fil.PLONTILLAGG = "847"
      pa90fil.PLONTILLANTAL = LONTILL.ERSATTNING * pa90fil.PLONTILLANTAL.
   END.
   IF PLONTILLAGG = "LUNC" THEN DO TRANSACTION:
      FIND FIRST LONTILL WHERE LONTILL.VILART = pa90fil.PLONTILLAGG
      USE-INDEX VILART NO-LOCK NO-ERROR.
      ASSIGN pa90fil.PLONTILLAGG = "847"
      pa90fil.PLONTILLANTAL = LONTILL.ERSATTNING * pa90fil.PLONTILLANTAL.
   END.
   IF PLONTILLAGG = "MIDD" THEN DO TRANSACTION:
      FIND FIRST LONTILL WHERE LONTILL.VILART = pa90fil.PLONTILLAGG
      USE-INDEX VILART NO-LOCK NO-ERROR.
      ASSIGN pa90fil.PLONTILLAGG = "847"
      pa90fil.PLONTILLANTAL = LONTILL.ERSATTNING * pa90fil.PLONTILLANTAL.
   END.
   IF PLONTILLAGG = "FRLU" THEN DO TRANSACTION:
      FIND FIRST LONTILL WHERE LONTILL.VILART = pa90fil.PLONTILLAGG
      USE-INDEX VILART NO-LOCK NO-ERROR.
      ASSIGN pa90fil.PLONTILLAGG = "847"
      pa90fil.PLONTILLANTAL = LONTILL.ERSATTNING * pa90fil.PLONTILLANTAL.
   END.
   IF PLONTILLAGG = "FRMI" THEN DO TRANSACTION:
      FIND FIRST LONTILL WHERE LONTILL.VILART = pa90fil.PLONTILLAGG
      USE-INDEX VILART NO-LOCK NO-ERROR.
      ASSIGN pa90fil.PLONTILLAGG = "847"
      pa90fil.PLONTILLANTAL = LONTILL.ERSATTNING * pa90fil.PLONTILLANTAL.
   END.
   IF PLONTILLAGG = "LUMI" THEN DO TRANSACTION:
      FIND FIRST LONTILL WHERE LONTILL.VILART = pa90fil.PLONTILLAGG
      USE-INDEX VILART NO-LOCK NO-ERROR.
      ASSIGN pa90fil.PLONTILLAGG = "847"
      pa90fil.PLONTILLANTAL = LONTILL.ERSATTNING * pa90fil.PLONTILLANTAL.
   END.
   IF PLONTILLAGG = "FLMI" THEN DO TRANSACTION:
      FIND FIRST LONTILL WHERE LONTILL.VILART = pa90fil.PLONTILLAGG
      USE-INDEX VILART NO-LOCK NO-ERROR.
      ASSIGN pa90fil.PLONTILLAGG = "847"
      pa90fil.PLONTILLANTAL = LONTILL.ERSATTNING * pa90fil.PLONTILLANTAL.
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

IF globforetag = "ETA" THEN globforetag = globforetag.
ELSE DO:
   OPEN QUERY p90q FOR EACH pa90fil WHERE pa90fil.PLONTILLAGG = "882" NO-LOCK.
   DO TRANSACTION:
      GET FIRST p90q EXCLUSIVE-LOCK.
      IF AVAILABLE pa90fil THEN DO:
         IF globforetag = "ESAN" AND pa90fil.PAVTAL = "TA" THEN globforetag = globforetag.
         ELSE DELETE pa90fil.      
      END.         
   END.   
   REPEAT:
      DO TRANSACTION:
         GET NEXT p90q EXCLUSIVE-LOCK.
         IF AVAILABLE pa90fil THEN DO:
            IF globforetag = "ESAN" AND pa90fil.PAVTAL = "TA" THEN globforetag = globforetag.
            ELSE DELETE pa90fil.      
         END.       
         ELSE LEAVE.
      END.        
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
END.   */
  
regdatum = TODAY.
RUN REGVEC.P.
ASSIGN
tperiod = 922.

ASSIGN
lon=""
antal = 0
persnr = ""
arnr = ""
drnr = 0
vrnr = 0
blag = "".
FOR EACH pa90fil BY pa90fil.PANSTNR BY pa90fil.PLONTILLAGG
BY pa90fil.AONR  BY pa90fil.DELNR BY pa90fil.PVECKONUMMER:
   rec = RECID(pa90fil).
   pnummer = pa90fil.PANSTNR.
   IF pa90fil.PLONTILLAGG = "" THEN NEXT.
   ASSIGN
   lone = pa90fil.PLONTILLAGG
   anr = pa90fil.AONR
   dnr = pa90fil.DELNR
   venr = pa90fil.PVECKONUMMER
   blg = pa90fil.BOLAG.
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
                  ASSIGN lonfel.PANSTNR = persnr
                  lonfel.PDATUMET = datum
                  lonfel.PTILLAGG = lon
                  lonfel.PANTAL = STRING(antal,">>>>>99")
                  lonfel.PKR = '         '
                  lonfel.AONR = arnr
                  lonfel.DELNR = drnr.
                  rapphj1 = '     '.
	           rapphj2 = '         '.
               END.     
               ELSE DO:
	           rapphj1 = STRING(antal,">>>99").
	           rapphj2 = '         '.
	        END.   
            END.
         END.
         datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
         SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".               
         dtum = pa90fil.PDATUM.
         CREATE pa90filen.
         ASSIGN pa90filen.PANSTNR = persnr
         pa90filen.PDATUMET = datum
         pa90filen.PDATUM = dtum
         pa90filen.PTILLAGG = lon
         pa90filen.AONR = arnr
         pa90filen.DELNR = drnr
         pa90filen.PVECKONUMMER = vrnr
         pa90filen.PANTAL = rapphj1
         pa90filen.PKR = rapphj2
         pa90filen.BOLAG = blag.           
      END.
      ASSIGN
      antal = pa90fil.PLONTILLANTAL
      lon = lone
      persnr = pnummer
      arnr = anr
      drnr = dnr
      vrnr = venr
      blag = blg.
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
            ASSIGN lonfel.PANSTNR = persnr
            lonfel.PDATUMET = datum
            lonfel.PTILLAGG = lon
            lonfel.PANTAL = STRING(antal,">>>>>99")
            lonfel.PKR = '         '
            lonfel.AONR = arnr
            lonfel.DELNR = drnr.
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
      datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
      SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".      
      dtum = pa90fil.PDATUM.
      CREATE pa90filen.
      ASSIGN pa90filen.PANSTNR = persnr
      pa90filen.PDATUMET = datum
      pa90filen.PDATUM = dtum
      pa90filen.PTILLAGG = lon
      pa90filen.AONR = arnr
      pa90filen.DELNR = drnr
      pa90filen.PVECKONUMMER = vrnr
      pa90filen.PANTAL = rapphj1
      pa90filen.PKR = rapphj2
      pa90filen.BOLAG = blag.   
   END.
END.
ASSIGN
lon=""
antal = 0
persnr = ""
arnr = ""
drnr = 0
vrnr = 0
blg = "".
FOR EACH pa90fil BY pa90fil.PANSTNR BY pa90fil.POVERTIDTILL
BY pa90fil.AONR  BY pa90fil.DELNR BY pa90fil.PVECKONUMMER:
   rec = RECID(pa90fil).
   pnummer = pa90fil.PANSTNR.
   IF pa90fil.POVERTIDTILL = "" THEN NEXT.
   ASSIGN
   lone = pa90fil.POVERTIDTILL
   anr = pa90fil.AONR
   dnr = pa90fil.DELNR
   venr = pa90fil.PVECKONUMMER
   blg = pa90fil.BOLAG.
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
         datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
         SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
         dtum = pa90fil.PDATUM.
         CREATE pa90filen.
         ASSIGN pa90filen.PANSTNR = persnr
         pa90filen.PDATUMET = datum
         pa90filen.PDATUM = dtum
         pa90filen.PTILLAGG = lon
         pa90filen.AONR = arnr
         pa90filen.DELNR = drnr
         pa90filen.PVECKONUMMER = vrnr
         pa90filen.PANTAL = rapphj1
         pa90filen.PKR = rapphj2
         pa90filen.BOLAG = blag.
      END.
      ASSIGN
      antal = pa90fil.POVERANTAL
      lon = lone
      persnr = pnummer
      arnr = anr
      drnr = dnr
      vrnr = venr
      blag = blg.
   END.
END.
IF antal > 0 THEN DO:
   nytid = antal.
   RUN TIMSEK.P.
   antal = sekunder / 36.     /* timmar i 100-delar*/
   rapphj1 = STRING(antal,">>>99").
   rapphj2 = '         '.
   FIND FIRST pa90fil WHERE RECID(pa90fil) = rec NO-LOCK NO-ERROR.
   IF NOT AVAILABLE pa90fil THEN personal = personal.
   ELSE DO TRANSACTION:
      datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
      SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
      dtum = pa90fil.PDATUM.
      CREATE pa90filen.
      ASSIGN pa90filen.PANSTNR = persnr
      pa90filen.PDATUMET = datum
      pa90filen.PDATUM = dtum
      pa90filen.PTILLAGG = lon
      pa90filen.AONR = arnr
      pa90filen.DELNR = drnr
      pa90filen.PVECKONUMMER = vrnr
      pa90filen.PANTAL = rapphj1
      pa90filen.PKR = rapphj2
      pa90filen.BOLAG = blag.
   END.
END.
/*ASSIGN
lon=""
antal = 0
persnr = ""
arnr = ""
drnr = 0
vrnr = 0
blag = "".
FOR EACH pa90fil BY pa90fil.PANSTNR BY pa90fil.PTRAKTKOD
BY pa90fil.AONR  BY pa90fil.DELNR BY pa90fil.PVECKONUMMER:
   rec = RECID(pa90fil).
   pnummer = pa90fil.PANSTNR.
   IF pa90fil.PTRAKTKOD = "" THEN NEXT.
   ASSIGN
   lone = pa90fil.PTRAKTKOD
   anr = pa90fil.AONR
   dnr = pa90fil.DELNR
   venr = pa90fil.PVECKONUMMER
   blg = pa90fil.BOLAG.
   IF lon = lone AND persnr = pnummer 
   AND arnr = anr AND drnr = dnr AND vrnr = venr 
   THEN antal = antal + pa90fil.PTRAKTANTAL.
   ELSE DO TRANSACTION:
      IF antal > 0 THEN DO:          
         antal = antal * 100.
         rapphj1 = STRING(antal,">>>99").
         rapphj2 = ''.
         datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
         SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
         dtum = pa90fil.PDATUM.
         CREATE pa90filen.
         ASSIGN pa90filen.PANSTNR = persnr
         pa90filen.PDATUMET = datum
         pa90filen.PDATUM = dtum pa90filen.PTILLAGG = lon
         pa90filen.AONR = arnr
         pa90filen.DELNR = drnr
         pa90filen.PVECKONUMMER = vrnr
         pa90filen.PANTAL = rapphj1 pa90filen.PKR = rapphj2
         pa90filen.BOLAG = blag.
      END.
      ASSIGN
      antal = pa90fil.PTRAKTANTAL
      lon = lone
      persnr = pnummer
      arnr = anr
      drnr = dnr
      vrnr = venr
      blag = blg.
   END.
END.
IF antal > 0 THEN DO:
   DO TRANSACTION: 
      antal = antal * 100.
      rapphj1 = STRING(antal,">>>99").
      rapphj2 = ''.
      FIND FIRST pa90fil WHERE RECID(pa90fil) = rec NO-LOCK NO-ERROR.
      IF NOT AVAILABLE pa90fil THEN DO:
         personal = personal.
      END.
      ELSE DO:
         datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
         SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
         dtum = pa90fil.PDATUM.
         CREATE pa90filen.
         ASSIGN pa90filen.PANSTNR = persnr
         pa90filen.PDATUMET = datum
         pa90filen.PDATUM = dtum pa90filen.PTILLAGG = lon
         pa90filen.AONR = arnr
         pa90filen.DELNR = drnr
         pa90filen.PVECKONUMMER = vrnr
         pa90filen.PANTAL = rapphj1 pa90filen.PKR = rapphj2
         pa90filen.BOLAG = blag.
      END.
   END.  
END.
ASSIGN
lon=""
antal = 0
persnr = ""
arnr = ""
drnr = 0
vrnr = 0
blag = "".
FOR EACH pa90fil BY pa90fil.PANSTNR BY pa90fil.PBEREDSKAP
BY pa90fil.AONR  BY pa90fil.DELNR BY pa90fil.PVECKONUMMER:
   ASSIGN
   rec = RECID(pa90fil)
   pnummer = pa90fil.PANSTNR
   lone = pa90fil.PBEREDSKAP
   anr = pa90fil.AONR
   dnr = pa90fil.DELNR
   venr = pa90fil.PVECKONUMMER
   blg = pa90fil.BOLAG.
   IF pa90fil.PBEREDSKAP = "" THEN NEXT.
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.ANSTNR = pnummer
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
         ELSE ASSIGN antal = antal * 100.   
         rapphj1 = STRING(antal,">>>99").
         rapphj2 = ''.
         datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
         SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
         dtum = pa90fil.PDATUM.
         CREATE pa90filen.
         ASSIGN pa90filen.PANSTNR = persnr
         pa90filen.PDATUMET = datum pa90filen.PDATUM = dtum
         pa90filen.PTILLAGG = lon
         pa90filen.AONR = arnr
         pa90filen.DELNR = drnr
         pa90filen.PVECKONUMMER = vrnr
         pa90filen.PANTAL = rapphj1 pa90filen.PKR = rapphj2
         pa90filen.BOLAG = blag.
      END.
      ASSIGN
      antal = pa90fil.PBERANTAL
      lon = lone
      persnr = pnummer
      arnr = anr
      drnr = dnr
      vrnr = venr
      blag = blg.
   END.
END.
IF antal > 0 THEN DO:
   DO TRANSACTION:
      IF BEREDSKAPTAB.BERANTAL = 0 THEN DO:
         nytid = antal.
         RUN TIMSEK.P.
         antal = sekunder / 36.     /* timmar i 100-delar*/
      END.
      ELSE ASSIGN antal = antal * 100.         
      rapphj1 = STRING(antal,">>>99").
      rapphj2 = ''.
      FIND FIRST pa90fil WHERE RECID(pa90fil) = rec NO-LOCK NO-ERROR.
      IF NOT AVAILABLE pa90fil THEN personal = personal.
      ELSE DO:
         datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
         SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
         dtum = pa90fil.PDATUM.
         CREATE pa90filen.
         ASSIGN pa90filen.PANSTNR = persnr
         pa90filen.PDATUMET = datum pa90filen.PDATUM = dtum
         pa90filen.PTILLAGG = lon
         pa90filen.AONR = arnr
         pa90filen.DELNR = drnr
         pa90filen.PVECKONUMMER = vrnr
         pa90filen.PANTAL = rapphj1 pa90filen.PKR = rapphj2
         pa90filen.BOLAG = blag.
      END.
   END.
END.
ASSIGN  
lon=""
antal = 0
persnr = ""
arnr = ""
drnr = 0
vrnr = 0
blag = "".*/
/* filen pa90 sparas undan i pakopia.d innan ny pa90fil skapas */

IF globforetag = "ESAN" THEN DO:
   OUTPUT TO /u10/guru/export/esan/eaf0601.d.
END.
FOR EACH pa90filen BY pa90filen.PANSTNR BY pa90filen.PTILLAGG:         
   IF pa90filen.PVECKONUMMER = 952 AND YEAR(pa90filen.PDATUM) = 2000 THEN DO: /*1 OCH 2 JANUARI 2000*/
      typdatum = "9" + STRING(pa90filen.PVECKONUMMER,"999").  
   END.
   ELSE DO:
      typdatum = SUBSTRING(string(YEAR(pa90filen.PDATUM),"9999"),3,1) 
      + STRING(pa90filen.PVECKONUMMER,"999").  
   END.
   anstall = STRING(INTEGER(pa90filen.PANSTNR),"9999999999").
   ASSIGN overrapp1 = "".
   ASSIGN
   SUBSTRING(overrapp1,1,7) = pa90filen.BOLAG + "52" + STRING(tperiod,"999")
   SUBSTRING(overrapp1,8,10) = anstall
   SUBSTRING(overrapp1,18,3) = SUBSTRING(pa90filen.PTILLAGG,1,3)
   SUBSTRING(overrapp1,22,4) = typdatum
   SUBSTRING(overrapp1,42,5) = SUBSTRING(pa90filen.PANTAL,1 ,5)
   SUBSTRING(overrapp1,52,9) = SUBSTRING(pa90filen.PKR,1 ,9)
   SUBSTRING(overrapp1,81,8) = pa90filen.AONR + SUBSTRING(STRING(pa90filen.DELNR,"999"),2,2).
   PUT overrapp1.   /*  pa90filen.P    */
   PUT SKIP.
END.
OUTPUT CLOSE.



