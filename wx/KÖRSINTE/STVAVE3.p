/*STVAVE3.P*/
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.

 
DEFINE SHARED VARIABLE vkdatum AS DATE NO-UNDO.
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
DEFINE VARIABLE antal AS DECIMAL FORMAT "99999.99" NO-UNDO.   /*LÖN*/
DEFINE VARIABLE persnummer AS CHARACTER FORMAT "X(10)" NO-UNDO.      /*LÖN*/
DEFINE VARIABLE datum AS CHARACTER FORMAT "X(6)" NO-UNDO.      /*LÖN*/
DEFINE VARIABLE dtum AS DATE NO-UNDO.
DEFINE VARIABLE overrapp1 AS CHARACTER FORMAT "X(100)" NO-UNDO.      /*LÖN*/
DEFINE VARIABLE rapphj1 AS DECIMAL FORMAT "99999.99" NO-UNDO.      /*HJÄLP ANTAL*/
DEFINE VARIABLE rapphj2 AS DECIMAL FORMAT "9999999.99" NO-UNDO.
/*DEFINE VARIABLE rapphj1 AS CHARACTER FORMAT "X(8)" NO-UNDO.      /*HJÄLP ANTAL*/
DEFINE VARIABLE rapphj2 AS CHARACTER FORMAT "X(10)" NO-UNDO.*/
DEFINE VARIABLE seku AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE beravt LIKE PERSONALTAB.BEREDSKAPSAVTAL NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL FORMAT "JA/NEJ" INITIAL FALSE NO-UNDO.
DEFINE VARIABLE rec AS RECID NO-UNDO.
DEFINE VARIABLE mnr AS INTEGER NO-UNDO.
DEFINE VARIABLE mrnr AS INTEGER NO-UNDO.
/*DEFINE VARIABLE dnr LIKE TIDREGITAB.DELNR NO-UNDO.
DEFINE VARIABLE drnr LIKE TIDREGITAB.DELNR NO-UNDO.*/
/*DEFINE VARIABLE vrnr LIKE TIDREGITAB.VECKONUMMER NO-UNDO.
DEFINE VARIABLE venr LIKE TIDREGITAB.VECKONUMMER NO-UNDO.*/
DEFINE VARIABLE typdatum AS CHARACTER FORMAT "999999" NO-UNDO. 
DEFINE VARIABLE tperiod AS INTEGER FORMAT "999" NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(41)" NO-UNDO.
DEFINE VARIABLE anstall AS CHARACTER FORMAT "X(10)" NO-UNDO.
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
   FIELD MANAD AS INTEGER
   FIELD PSORT AS CHARACTER FORMAT "XX"
   FIELD PPAR8 AS DECIMAL FORMAT "-99.99"
   FIELD PANSTNR LIKE PERSONALTAB.ANSTNR
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD PAVTAL AS CHARACTER
   INDEX PPERSONNUMMER IS PRIMARY PPERSONNUMMER ASCENDING.
DEFINE TEMP-TABLE pa90filen
   FIELD PPERSONNUMMER AS CHARACTER FORMAT "X(10)"
   FIELD PDATUMET AS CHARACTER FORMAT "X(6)"
   FIELD PDATUM LIKE TIDREGITAB.DATUM
   FIELD PTILLAGG LIKE TIDREGITAB.LONTILLAGG
   FIELD PANTAL AS DECIMAL FORMAT "99999.99"
   FIELD PKR AS DECIMAL FORMAT "9999999.99"
/*   FIELD PANTAL AS CHARACTER FORMAT "X(8)"
   FIELD PKR AS CHARACTER FORMAT "X(10)"*/
   FIELD MANAD AS INTEGER FORMAT "99"
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD PVECKONUMMER LIKE TIDREGITAB.VECKONUMMER
   INDEX PPERSONNUMMER IS PRIMARY PPERSONNUMMER ASCENDING.
DEFINE TEMP-TABLE lonfel
   FIELD PPERSONNUMMER AS CHARACTER FORMAT "X(10)"
   FIELD PDATUMET AS CHARACTER FORMAT "X(6)"
   FIELD PTILLAGG LIKE TIDREGITAB.LONTILLAGG
   FIELD PANTAL AS CHARACTER FORMAT "X(8)"
   FIELD PKR AS CHARACTER FORMAT "X(10)"
   /*FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR*/
   INDEX PPERSONNUMMER IS PRIMARY PPERSONNUMMER ASCENDING.
   
{AMERICANEUROPEAN.I}
FOR EACH pa90filen:
  DELETE pa90filen.
END.

ASSIGN
lon=""
antal = 0
persnr = ""
mrnr = 0.
/*drnr = 0.*/
FOR EACH pa90fil BY pa90fil.PPERSONNUMMER BY pa90fil.PLONTILLAGG BY pa90fil.MANAD:
   rec = RECID(pa90fil).
   pnummer = pa90fil.PPERSONNUMMER.
   IF pa90fil.PLONTILLAGG = "" THEN NEXT.
   ASSIGN
   lone = pa90fil.PLONTILLAGG
   mnr = pa90fil.MANAD.
   /*dnr = pa90fil.DELNR.*/
   IF lon = lone AND persnr = pnummer AND mrnr = mnr 
   /*AND drnr = dnr*/ THEN DO:
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
            rapphj1 = antal.
            rapphj2 = 0.
         END.
         ELSE DO:
            sor = LONTILL.ENHET.
            IF sor = "KR" THEN DO:
	        /*antal = antal * 100.*/	        
	        rapphj1 = 0.
	        rapphj2 =  antal.
            END.
            ELSE IF sor = "TI" THEN DO:
	       /* nytid = antal.
	        RUN TIMSEK.P.
	        antal = sekunder / 36.  */   /* timmar i 100-delar*/
	        IF antal < 0 THEN DO:
	           rapphj1 = antal.
	           rapphj2 = 0.	  
	        END.
	        ELSE DO:
	           rapphj1 = antal.
	           rapphj2 = 0.	  
	        END.   
            END.
            ELSE DO:                   
               /*antal = antal * 100.*/               
               rapphj1 = antal.
	        rapphj2 = 0.
	     END.
         END.
         datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
         SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".               
         dtum = pa90fil.PDATUM.
         CREATE pa90filen.
         ASSIGN pa90filen.PPERSONNUMMER = persnr
         pa90filen.PDATUMET = datum
         pa90filen.PDATUM = dtum
         pa90filen.PTILLAGG = lon
         pa90filen.MANAD = mrnr
         /*pa90filen.DELNR = drnr*/
         pa90filen.PANTAL = rapphj1
         pa90filen.PKR = rapphj2.         
      END.
      ASSIGN
      antal = pa90fil.PLONTILLANTAL
      lon = lone
      persnr = pnummer
      mrnr = mnr.
/*      drnr = dnr.*/
   END.
END.
IF antal NE 0 THEN DO:
   FIND FIRST LONTILL WHERE LONTILL.VILART = lon USE-INDEX VILART NO-LOCK NO-ERROR.
   IF NOT AVAILABLE LONTILL THEN DO:
      rapphj1 = antal.
      rapphj2 = 0.     
   END.
   ELSE DO:
      sor = LONTILL.ENHET.
      IF sor = "KR" THEN DO:
         /*antal = antal * 100.*/
	  rapphj1 = 0.
	  rapphj2 =  antal.
      END.
      ELSE IF sor = "TI" THEN DO:
         /*nytid = antal.
	  RUN TIMSEK.P.
	  antal = sekunder / 36.*/     /* timmar i 100-delar*/
	  IF antal < 0 THEN DO:
	     rapphj1 = antal.
	     rapphj2 = 0.	  
	  END.
	  ELSE DO:
	     rapphj1 = antal.
	     rapphj2 = 0.	  
	  END.   
      END.
      ELSE DO:      
         /*antal = antal * 100.*/
         rapphj1 = antal.
	  rapphj2 = 0.	     
      END.
   END.
   FIND FIRST pa90fil WHERE RECID(pa90fil) = rec NO-LOCK NO-ERROR.
   IF NOT AVAILABLE pa90fil THEN personal = personal.
   ELSE DO TRANSACTION:
      datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
      SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".      
      dtum = pa90fil.PDATUM.
      CREATE pa90filen.
      ASSIGN pa90filen.PPERSONNUMMER = persnr
      pa90filen.PDATUMET = datum
      pa90filen.PDATUM = dtum
      pa90filen.PTILLAGG = lon
      pa90filen.MANAD = mrnr
      /*pa90filen.DELNR = drnr*/
      pa90filen.PANTAL = rapphj1
      pa90filen.PKR = rapphj2. 
   END.
END.
ASSIGN
lon=""
antal = 0
persnr = ""
mrnr = 0.
/*drnr = 0.*/
FOR EACH pa90fil BY pa90fil.PPERSONNUMMER BY pa90fil.POVERTIDTILL BY pa90fil.MANAD:
   rec = RECID(pa90fil).
   pnummer = pa90fil.PPERSONNUMMER.
   IF pa90fil.POVERTIDTILL = "" THEN NEXT.
   ASSIGN
   lone = pa90fil.POVERTIDTILL
   mnr = pa90fil.MANAD.
/*   dnr = pa90fil.DELNR.*/
   IF lon = lone AND persnr = pnummer AND mrnr = mnr 
/*   AND drnr = dnr*/ THEN DO:
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
         /*nytid = antal.
         RUN TIMSEK.P.
         antal = sekunder / 36.     /* timmar i 100-delar*/*/
         rapphj1 = antal.
         rapphj2 = 0.
         datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
         SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
         dtum = pa90fil.PDATUM.
         CREATE pa90filen.
         ASSIGN pa90filen.PPERSONNUMMER = persnr
         pa90filen.PDATUMET = datum
         pa90filen.PDATUM = dtum
         pa90filen.PTILLAGG = lon
         pa90filen.MANAD = mrnr
/*         pa90filen.DELNR = drnr*/
         pa90filen.PANTAL = rapphj1
         pa90filen.PKR = rapphj2.
      END.
      ASSIGN
      antal = pa90fil.POVERANTAL
      lon = lone
      persnr = pnummer
      mrnr = mnr.
      /*drnr = dnr.*/
   END.
END.
IF antal > 0 THEN DO:
   /*nytid = antal.
   RUN TIMSEK.P.
   antal = sekunder / 36.     /* timmar i 100-delar*/*/
   rapphj1 = antal.
   rapphj2 = 0.
   FIND FIRST pa90fil WHERE RECID(pa90fil) = rec NO-LOCK NO-ERROR.
   IF NOT AVAILABLE pa90fil THEN personal = personal.
   ELSE DO TRANSACTION:
      datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
      SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
      dtum = pa90fil.PDATUM.
      CREATE pa90filen.
      ASSIGN pa90filen.PPERSONNUMMER = persnr
      pa90filen.PDATUMET = datum
      pa90filen.PDATUM = dtum
      pa90filen.PTILLAGG = lon
      pa90filen.MANAD = mrnr
/*      pa90filen.DELNR = drnr*/
      pa90filen.PANTAL = rapphj1
      pa90filen.PKR = rapphj2.
   END.
END.
ASSIGN
lon=""
antal = 0
persnr = ""
mrnr = 0.
/*drnr = 0.*/
FOR EACH pa90fil BY pa90fil.PPERSONNUMMER BY pa90fil.PTRAKTKOD BY pa90fil.MANAD:
   rec = RECID(pa90fil).
   pnummer = pa90fil.PPERSONNUMMER.
   IF pa90fil.PTRAKTKOD = "" THEN NEXT.
   ASSIGN
   lone = pa90fil.PTRAKTKOD
   mnr = pa90fil.MANAD.
/*   dnr = pa90fil.DELNR.*/
   IF lon = lone AND persnr = pnummer 
   AND mrnr = mnr /*AND drnr = dnr*/ THEN antal = antal + pa90fil.PTRAKTANTAL.
   ELSE DO TRANSACTION:
      IF antal > 0 THEN DO:          
         /*antal = antal * 100.*/
         rapphj1 = antal.
         rapphj2 = 0.
         datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
         SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
         dtum = pa90fil.PDATUM.
         CREATE pa90filen.
         ASSIGN pa90filen.PPERSONNUMMER = persnr
         pa90filen.PDATUMET = datum
         pa90filen.PDATUM = dtum pa90filen.PTILLAGG = lon
         pa90filen.MANAD = mrnr
/*         pa90filen.DELNR = drnr*/
         pa90filen.PANTAL = rapphj1 pa90filen.PKR = rapphj2.
      END.
      ASSIGN
      antal = pa90fil.PTRAKTANTAL
      lon = lone
      persnr = pnummer
      mrnr = mnr.
/*      drnr = dnr.*/
   END.
END.
IF antal > 0 THEN DO:
   DO TRANSACTION: 
/*      antal = antal * 100.*/
      rapphj1 = antal.
      rapphj2 = 0.
      FIND FIRST pa90fil WHERE RECID(pa90fil) = rec NO-LOCK NO-ERROR.
      IF NOT AVAILABLE pa90fil THEN DO:
         personal = personal.
      END.
      ELSE DO:
         datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
         SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
         dtum = pa90fil.PDATUM.
         CREATE pa90filen.
         ASSIGN pa90filen.PPERSONNUMMER = persnr
         pa90filen.PDATUMET = datum
         pa90filen.PDATUM = dtum pa90filen.PTILLAGG = lon
         pa90filen.MANAD = mrnr
 /*        pa90filen.DELNR = drnr*/
         pa90filen.PANTAL = rapphj1 pa90filen.PKR = rapphj2.
      END.
   END.  
END.
ASSIGN
lon=""
antal = 0
persnr = ""
mrnr = 0.
/*drnr = 0.*/
FOR EACH pa90fil BY pa90fil.PPERSONNUMMER BY pa90fil.PBEREDSKAP BY pa90fil.MANAD:
   ASSIGN
   rec = RECID(pa90fil)
   pnummer = pa90fil.PPERSONNUMMER
   lone = pa90fil.PBEREDSKAP
   mnr = pa90fil.MANAD.
/*   dnr = pa90fil.DELNR.*/
   IF pa90fil.PBEREDSKAP = "" THEN NEXT.
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONNUMMER = pnummer
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   beravt = PERSONALTAB.BEREDSKAPSAVTAL.
   FIND FIRST BEREDSKAPTAB WHERE BEREDSKAPTAB.BEREDSKAPSAVTAL = beravt
   USE-INDEX BERED NO-LOCK NO-ERROR.
   IF lon = lone AND persnr = pnummer AND mrnr = mnr 
/*   AND drnr = dnr*/  THEN DO:
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
         /*IF BEREDSKAPTAB.BERANTAL = 0 THEN DO:
            nytid = antal.
            RUN TIMSEK.P.
            antal = sekunder / 36.     /* timmar i 100-delar*/
         END.
         ELSE ASSIGN antal = antal * 100.   */
         rapphj1 = antal.
         rapphj2 = 0.
         datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
         SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
         dtum = pa90fil.PDATUM.
         CREATE pa90filen.
         ASSIGN pa90filen.PPERSONNUMMER = persnr
         pa90filen.PDATUMET = datum pa90filen.PDATUM = dtum
         pa90filen.PTILLAGG = lon
         pa90filen.MANAD = mrnr
/*         pa90filen.DELNR = drnr*/
         pa90filen.PANTAL = rapphj1 pa90filen.PKR = rapphj2.
      END.
      ASSIGN
      antal = pa90fil.PBERANTAL
      lon = lone
      persnr = pnummer
      mrnr = mnr.
/*      drnr = dnr.*/
   END.
END.
IF antal > 0 THEN DO:
   DO TRANSACTION:
/*      IF BEREDSKAPTAB.BERANTAL = 0 THEN DO:
         nytid = antal.
         RUN TIMSEK.P.
         antal = sekunder / 36.     /* timmar i 100-delar*/
      END.
      ELSE ASSIGN antal = antal * 100.         */
      rapphj1 = antal.
      rapphj2 = 0.
      FIND FIRST pa90fil WHERE RECID(pa90fil) = rec NO-LOCK NO-ERROR.
      IF NOT AVAILABLE pa90fil THEN personal = personal.
      ELSE DO:
         datum = SUBSTRING(STRING(pa90fil.PDATUM),1 ,2) +
         SUBSTRING(STRING(pa90fil.PDATUM),4 ,2) + "01".
         dtum = pa90fil.PDATUM.
         CREATE pa90filen.
         ASSIGN pa90filen.PPERSONNUMMER = persnr
         pa90filen.PDATUMET = datum pa90filen.PDATUM = dtum
         pa90filen.PTILLAGG = lon
         pa90filen.MANAD = mrnr
/*         pa90filen.DELNR = drnr*/
         pa90filen.PANTAL = rapphj1 pa90filen.PKR = rapphj2.
      END.
   END.
END.
ASSIGN  
lon=""
antal = 0
persnr = ""
mrnr = 0.
/*drnr = 0.*/
IF Guru.Konstanter:globforetag = "elpa" then DO:
   OUTPUT TO D:\delad\pro8\GURU\apptemp\pa90filen.d.
   for each pa90filen BY pa90filen.Ppersonnummer BY pa90filen.PTILLAGG:
     display pa90filen.
   end.  
END.
/* filen pa90 sparas undan i pakopia.d innan ny pa90fil skapas */
IF Guru.Konstanter:globforetag = "VATT" THEN DO:
   OUTPUT TO /guru/export/lon/lon.d.
END.
ELSE DO:
   OUTPUT TO D:\delad\pro8\GURU\apptemp\lon.d.
END.
FOR EACH pa90filen BY pa90filen.PPERSONNUMMER BY pa90filen.PTILLAGG:         
   ASSIGN overrapp1 = "".
   ASSIGN typdatum = SUBSTRING(string(YEAR(pa90filen.PDATUM),"9999"),3,1) 
   + STRING(pa90filen.PVECKONUMMER,"999").
  
  
   ASSIGN anstall = SUBSTRING(STRING(pa90filen.PPERSONNUMMER),1,6) +
                    SUBSTRING(STRING(pa90filen.PPERSONNUMMER),7,4).
   ASSIGN
   SUBSTRING(overrapp1,2,10) = anstall
   SUBSTRING(overrapp1,15,4) = SUBSTRING(pa90filen.PTILLAGG,1,4) 
   SUBSTRING(overrapp1,22,6) = SUBSTRING(STRING(pa90filen.PDATUM),1 ,2) +
                               STRING(pa90filen.MANAD,"99") + "01".  
/*   SUBSTRING(overrapp1,22,6) = SUBSTRING(STRING(pa90filen.PDATUM),1 ,2) +
                               SUBSTRING(STRING(pa90filen.PDATUM),4 ,2) + "01".*/
   IF pa90filen.PTILLAGG = "9008" THEN ASSIGN SUBSTRING(overrapp1,30,1) = "-".
   ELSE ASSIGN SUBSTRING(overrapp1,30,1) = "+".
   ASSIGN
   SUBSTRING(overrapp1,31,8) = STRING(pa90filen.PANTAL,"99999.99")
   SUBSTRING(overrapp1,40,1) = "+"
   SUBSTRING(overrapp1,41,10) = STRING(pa90filen.PKR,"9999999.99").
   PUT overrapp1.   /*  pa90filen.P    */
   PUT SKIP.
END.
OUTPUT CLOSE.
IF Guru.Konstanter:globforetag = "VATT" THEN DO:
   OUTPUT TO /guru/export/lon/frfelma.d NO-ECHO.      
END.
ELSE DO:
   OUTPUT TO D:\delad\pro8\GURU\apptemp\frfelma.d NO-ECHO.
END.
FOR EACH lonfel BY lonfel.PPERSONNUMMER BY lonfel.PTILLAGG:   
   EXPORT lonfel.
END.
OUTPUT CLOSE.
{EUROPEANAMERICAN.I}



