/*OTO2PR.p  */   
&Scoped-define NEW 
{TIDALLT.I}
DEFINE NEW SHARED VARIABLE krav2 AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE ber1 LIKE UTRYCKNING.UTRYCKNBER NO-UNDO.

/*DEFINE SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/
DEFINE SHARED VARIABLE tidtabrec AS RECID  NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID  NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO. 
DEFINE SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO.  
DEFINE SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE bustart3 LIKE TIDREGITAB.START NO-UNDO.
DEFINE SHARED VARIABLE slut4 AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE bslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE SHARED VARIABLE bstart LIKE TIDREGITAB.START NO-UNDO.
DEFINE SHARED VARIABLE avslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE SHARED VARIABLE avstart LIKE TIDREGITAB.START NO-UNDO.
DEFINE SHARED VARIABLE nytid AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE sta AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE slu AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE avsta AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE avslu AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE bilforare AS LOGICAL FORMAT "JA/NEJ" NO-UNDO.
DEFINE SHARED VARIABLE reco AS RECID NO-UNDO.
DEFINE SHARED VARIABLE reco2 AS RECID NO-UNDO.    
DEFINE VARIABLE redag AS INTEGER FORMAT "9" NO-UNDO.
DEFINE VARIABLE onr LIKE TIDREGITAB.AONR NO-UNDO.
DEFINE VARIABLE dnr LIKE TIDREGITAB.DELNR NO-UNDO.
DEFINE VARIABLE idagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE utryck LIKE TIDREGITAB.UTRYCKNING NO-UNDO.
DEFINE VARIABLE krav4 AS LOGICAL NO-UNDO.   
DEFINE VARIABLE sekun AS INTEGER NO-UNDO.   
DEFINE VARIABLE seku AS INTEGER NO-UNDO.                 /*NY*/
DEFINE VARIABLE hjadatum LIKE TIDREGITAB.DATUM NO-UNDO.
DEFINE VARIABLE hjbdatum LIKE TIDREGITAB.DATUM NO-UNDO.  
DEFINE VARIABLE res LIKE AUTOMREG.PRISTYP NO-UNDO.
DEFINE VARIABLE utrber LIKE UTRYCKNING.UTRYCKNBER NO-UNDO.
DEFINE VARIABLE utrejber LIKE UTRYCKNING.UTRYCKNEJBER NO-UNDO.
DEFINE VARIABLE regstartspar LIKE TIDREGITAB.START NO-UNDO.  
DEFINE VARIABLE regslutspar LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE VARIABLE energiavt AS LOGICAL NO-UNDO.
DEFINE VARIABLE berhj AS DECIMAL NO-UNDO.
DEFINE VARIABLE avberrec AS RECID NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE bantal AS DECIMAL NO-UNDO.
DEFINE VARIABLE hjsttid AS DECIMAL NO-UNDO.
DEFINE SHARED TEMP-TABLE ovavtab
   FIELD KOD LIKE OVERAVTAB.KOD 
   FIELD OVERTIDTILL LIKE OVERAVTAB.OVERTIDTILL
   FIELD START1 LIKE OVERAVTAB.START1
   FIELD STOPP1 LIKE OVERAVTAB.STOPP1
   FIELD START2 LIKE OVERAVTAB.START2
   FIELD STOPP2 LIKE OVERAVTAB.STOPP2
   FIELD DATUM LIKE OVERAVTAB.DATUM
   FIELD OVERTIDUTTAG LIKE OVERAVTAB.OVERTIDUTTAG
   FIELD DAGEQ LIKE OVERAVTAB.DAGEQ
   FIELD EQDAG LIKE OVERAVTAB.EQDAG 
   INDEX ODATUM IS PRIMARY DATUM ASCENDING 
   INDEX OSTART1 KOD DATUM START1 STOPP1 ASCENDING
   INDEX OSTART2 KOD DATUM START2 STOPP2 ASCENDING.
DEFINE SHARED TEMP-TABLE otidtab
   FIELD KOD LIKE OVERTIDTAB.KOD 
   FIELD OVERTIDTILL LIKE OVERTIDTAB.OVERTIDTILL
   FIELD START1 LIKE OVERTIDTAB.START1
   FIELD STOPP1 LIKE OVERTIDTAB.STOPP1
   FIELD START2 LIKE OVERTIDTAB.START2
   FIELD STOPP2 LIKE OVERTIDTAB.STOPP2
   FIELD DAGNR LIKE OVERTIDTAB.DAGNR
   FIELD OVERTIDUTTAG LIKE OVERTIDTAB.OVERTIDUTTAG
   FIELD FORKL LIKE OVERTIDTAB.FORKL
   FIELD ARBSLUT LIKE OVERTIDTAB.ARBSLUT
   FIELD ARBSTART LIKE OVERTIDTAB.ARBSTART
   INDEX OVERTILL IS PRIMARY DAGNR OVERTIDTILL ASCENDING    
   INDEX OVERT KOD DAGNR ARBSLUT ASCENDING   
   INDEX OVERSTART KOD DAGNR START1 STOPP1 ASCENDING 
   INDEX OVERKOD OVERTIDTILL ASCENDING
   INDEX OVER KOD DAGNR OVERTIDTILL OVERTIDUTTAG ASCENDING
   INDEX OSTART2 KOD DAGNR START2 STOPP2 ASCENDING.      

DEFINE NEW SHARED TEMP-TABLE ohjalp
 FIELD ODATUM LIKE TIDREGITAB.DATUM FIELD OSTART LIKE TIDREGITAB.START
 FIELD OSLUT LIKE TIDREGITAB.SLUT /*FIELD OOVERTIDTILL LIKE TIDREGITAB.OVERTIDTILL*/
 FIELD OKOD1 LIKE TIDREGITAB.OKOD1 FIELD OANT1 LIKE TIDREGITAB.OANT1
 FIELD OST1 LIKE TIDREGITAB.START FIELD OSL1 LIKE TIDREGITAB.SLUT
 FIELD OKOD2 LIKE TIDREGITAB.OKOD2 FIELD OANT2 LIKE TIDREGITAB.OANT2
 FIELD OST2 LIKE TIDREGITAB.START FIELD OSL2 LIKE TIDREGITAB.SLUT
 FIELD OKOD3 LIKE TIDREGITAB.OKOD3 FIELD OANT3 LIKE TIDREGITAB.OANT3
 FIELD OST3 LIKE TIDREGITAB.START FIELD OSL3 LIKE TIDREGITAB.SLUT
 FIELD OTOTALT LIKE TIDREGITAB.TOTALT 
 FIELD OOVERAUTO LIKE TIDREGITAB.OVERAUTO 
 FIELD OENKE LIKE OVERKOD.ENKEL
 FIELD OAONR LIKE TIDREGITAB.AONR 
 FIELD ODELNR LIKE TIDREGITAB.DELNR 
 FIELD OETOT LIKE TIDREGITAB.TOTALT 
 FIELD OUTR LIKE TIDREGITAB.UTRYCKNING 
 FIELD RECTIDVIS AS RECID 
 FIELD OVERTIDUTTAG LIKE TIDREGITAB.OVERTIDUTTAG
 FIELD OAVBE LIKE TIDREGITAB.LAGANTAL
 INDEX ODATUM IS PRIMARY ODATUM ASCENDING OSTART ASCENDING.

DEFINE QUERY traktq FOR TIDREGITAB.
DEFINE BUFFER exbuff FOR ohjalp.
DEFINE BUFFER tidbuff FOR TIDREGITAB.
/* LAGG TILLBAKA */
FIND PERSONALTAB WHERE RECID(PERSONALTAB) = persrec NO-LOCK NO-ERROR.
FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
USE-INDEX ANSTF NO-LOCK NO-ERROR.
FIND FIRST UTRYCKNING WHERE UTRYCKNING.KOD = ANSTFORMTAB.KOD
USE-INDEX UT NO-LOCK NO-ERROR.
ASSIGN 
utrber = UTRYCKNING.UTRYCKNBER
utrejber = UTRYCKNING.UTRYCKNEJBER.
FIND FIRST AUTOMREG WHERE AUTOMREG.RESTIDREG = TRUE
USE-INDEX PRISTYPER NO-LOCK NO-ERROR.
res = AUTOMREG.PRISTYP.
IF bilforare = TRUE THEN  res = ' '.
krav4 = FALSE.
energiavt = FALSE.   
IF (Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  OR Guru.Konstanter:globforetag = "MISV") THEN ASSIGN energiavt = TRUE.
IF Guru.Konstanter:globforetag = "GKAL" THEN ASSIGN energiavt = TRUE.
IF Guru.Konstanter:globforetag = "LULE" THEN ASSIGN energiavt = TRUE.
EMPTY TEMP-TABLE ohjalp NO-ERROR. 
/*hjsttid ist�llet f�r bustart3 - eftersom reg 23-04 har startdatum 23 och bustart3 00*/
FIND TIDREGITAB WHERE RECID(TIDREGITAB) = reco NO-LOCK NO-ERROR.
hjsttid = TIDREGITAB.START.
FIND FIRST BEREDSKAPTAB WHERE BEREDSKAPTAB.BEREDSKAPSAVTAL =
PERSONALTAB.BEREDSKAPSAVTAL USE-INDEX BERED NO-LOCK NO-ERROR.
IF AVAILABLE BEREDSKAPTAB THEN DO:
   IF BEREDSKAPTAB.BERANTAL > 0 THEN DO:                 
      bantal = 0.
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum USE-INDEX PSTART NO-LOCK:
         bantal = bantal + TIDREGITAB.BERANTAL.
         IF bantal = 1 THEN krav4 = TRUE.
      END.   
   END.
END.   
FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD AND
TIDREGITAB.DATUM = regdatum USE-INDEX PSTART NO-LOCK:
   IF hjsttid GE TIDREGITAB.BEREDSKAPSTART AND
   hjsttid < TIDREGITAB.BEREDSKAPSLUT THEN krav4 = TRUE.
END.

FIND TIDREGITAB WHERE RECID(TIDREGITAB) = reco NO-LOCK NO-ERROR.
IF Guru.Konstanter:globforetag = "VATT" THEN DO: /*F�RBEREDD F�R DRIFTLEDNING VATTENFALL*/
   FIND FIRST AODRIFT WHERE AODRIFT.AONR = TIDREGITAB.AONR AND AODRIFT.DELNR = 
   TIDREGITAB.DELNR NO-LOCK NO-ERROR.
   IF AVAILABLE AODRIFT THEN DO:
      ASSIGN 
      utrber = UTRYCKNING.BER2
      utrejber = UTRYCKNING.EJBER2.
   END.  
   ELSE DO:
      ASSIGN 
      utrber = UTRYCKNING.UTRYCKNBER
      utrejber = UTRYCKNING.UTRYCKNEJBER.
   END.         
END.   
IF krav4 = true THEN ber1 = utrber.
ELSE ber1 = utrejber.
sekunder = ber1.
RUN SEKTIM.P.       
utryck = TIDREGITAB.UTRYCKNING.                                 
IF Guru.Konstanter:globforetag = "GRAN"  AND res NE "" THEN DO: 
   ASSIGN onr = TIDREGITAB.AONR  dnr = TIDREGITAB.DELNR
   idagnamn = TIDREGITAB.DAG.                    
   redag = WEEKDAY(regdatum).   
   IF krav4 = FALSE AND utryck = TRUE THEN DO:
      FIND FIRST ovavtab WHERE ovavtab.DATUM = regdatum AND 
      ovavtab.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
      IF AVAILABLE ovavtab THEN redag = ovavtab.EQDAG.
      FIND FIRST UTRTAB WHERE UTRTAB.DAGNR = redag AND
      UTRTAB.KOD = ANSTFORMTAB.KOD AND sta GE  UTRTAB.START1 AND
      sta < UTRTAB.STOPP1  AND sta NE UTRTAB.STOPP1  AND
      UTRTAB.START1 NE UTRTAB.STOPP1 USE-INDEX UTRSTART NO-LOCK NO-ERROR.
      IF NOT AVAILABLE UTRTAB THEN DO:
         FIND FIRST UTRTAB WHERE UTRTAB.DAGNR = redag AND
         UTRTAB.KOD = ANSTFORMTAB.KOD AND sta GE  UTRTAB.START2 AND
         sta < UTRTAB.STOPP2  AND sta NE UTRTAB.STOPP2 AND
         UTRTAB.START2 NE UTRTAB.STOPP2 USE-INDEX UTRSTART2 NO-LOCK NO-ERROR.
         IF NOT AVAILABLE UTRTAB THEN regdatum = regdatum.
      END.
      IF AVAILABLE UTRTAB THEN DO:
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD =
         PERSONALTAB.PERSONALKOD AND TIDREGITAB.DATUM = bdatum AND
         TIDREGITAB.SLUT = bstart USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF NOT AVAILABLE TIDREGITAB THEN DO TRANSACTION:
            FIND TIDREGITAB WHERE RECID(TIDREGITAB) = reco EXCLUSIVE-LOCK NO-ERROR.
            IF TIDREGITAB.RECTIDVIS = ? THEN TIDREGITAB.RECTIDVIS = reco.
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.RECTIDVIS = reco AND TIDREGITAB.TIDLOG = FALSE
            USE-INDEX RECTIDVIS EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE TIDREGITAB THEN DO:   
               regdatum =  bdatum.
               RUN REGVEC.P.
               CREATE TIDREGITAB.
               CREATE tidallt.
               tidallt.RECTIDVIS = RECID(TIDREGITAB).
               ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
               TIDREGITAB.PROGRAM  = 'OTO2PR' + STRING(TODAY) + Guru.Konstanter:globanv
               TIDREGITAB.VECKONUMMER = regvnr
               TIDREGITAB.DATUM = bdatum
               TIDREGITAB.DAG = idagnamn
               TIDREGITAB.AONR = onr
               TIDREGITAB.DELNR = dnr
               TIDREGITAB.START = 7.00
               TIDREGITAB.SLUT = 7.00
               TIDREGITAB.LONTILLAGG = UTRTAB.LONTILLAGG
               TIDREGITAB.LONTILLANTAL = 1
               TIDREGITAB.RECTIDVIS = reco
               TIDREGITAB.TIDLOG = FALSE.	        
            END.
            ELSE DO:
               ASSIGN TIDREGITAB.LONTILLAGG = UTRTAB.LONTILLAGG.
            END.
         END.
      END.

   END.
   ELSE DO TRANSACTION:
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.RECTIDVIS = reco USE-INDEX
      RECTIDVIS EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE TIDREGITAB THEN DELETE TIDREGITAB.
   END.   
END.
ASSIGN hjadatum = bdatum
hjbdatum = avdatum.                

IF bustart3 > regstart  THEN DO:
   FIND LAST TIDREGITAB WHERE 
   TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND 
   TIDREGITAB.DATUM = avdatum AND TIDREGITAB.PRISTYP NE res AND
   TIDREGITAB.UTRYCKNING = TRUE AND
   TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.OANT1 > 0 
   USE-INDEX PSTART NO-LOCK NO-ERROR.
   IF AVAILABLE TIDREGITAB AND TIDREGITAB.SLUT > 21 THEN hjbdatum = avdatum + 1.
END. 
IF regstart = regslut THEN DO:
   FIND FIRST TIDREGITAB WHERE 
   TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND 
   TIDREGITAB.DATUM = avdatum AND TIDREGITAB.PRISTYP NE res AND
   TIDREGITAB.UTRYCKNING = TRUE AND
   TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.OANT1 > 0 
   USE-INDEX PSTART NO-LOCK NO-ERROR.
   IF AVAILABLE TIDREGITAB AND TIDREGITAB.START < 3 THEN hjadatum = bdatum - 1.
   FIND LAST TIDREGITAB WHERE 
   TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND 
   TIDREGITAB.DATUM = avdatum AND TIDREGITAB.PRISTYP NE res AND
   TIDREGITAB.UTRYCKNING = TRUE AND
   TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.OANT1 > 0 
   USE-INDEX PSTART NO-LOCK NO-ERROR.
   IF AVAILABLE TIDREGITAB AND TIDREGITAB.SLUT > 21 THEN hjbdatum = avdatum + 1.
END. 
ELSE IF bustart3 < regstart AND bustart3 > 0 THEN hjadatum = bdatum - 1.
OPEN QUERY traktq FOR EACH TIDREGITAB WHERE 
TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
TIDREGITAB.DATUM >= hjadatum AND TIDREGITAB.DATUM <= hjbdatum AND 
TIDREGITAB.PRISTYP NE res AND TIDREGITAB.TIDLOG = TRUE AND 
TIDREGITAB.OANT1 > 0 USE-INDEX PSTART NO-LOCK.              
GET FIRST traktq NO-LOCK.   
DO WHILE AVAILABLE(TIDREGITAB):
   DO TRANSACTION:
      CREATE ohjalp. 
      ASSIGN 
      ohjalp.ODATUM = TIDREGITAB.DATUM ohjalp.OSLUT = TIDREGITAB.SLUT
      ohjalp.OSTART = TIDREGITAB.START   
      ohjalp.OKOD1 = TIDREGITAB.OKOD1 ohjalp.OANT1 = TIDREGITAB.OANT1
      ohjalp.OST1 = TIDREGITAB.OST1 ohjalp.OSL1 = TIDREGITAB.OSL1
      ohjalp.OKOD2 = TIDREGITAB.OKOD2 ohjalp.OANT2 = TIDREGITAB.OANT2
      ohjalp.OST2 = TIDREGITAB.OST2 ohjalp.OSL2 = TIDREGITAB.OSL2
      ohjalp.OKOD3 = TIDREGITAB.OKOD3 ohjalp.OANT3 = TIDREGITAB.OANT3
      ohjalp.OST3 = TIDREGITAB.OST3 ohjalp.OSL3 = TIDREGITAB.OSL3
      ohjalp.OTOTALT = TIDREGITAB.TOTALT ohjalp.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG
      ohjalp.OOVERAUTO = TIDREGITAB.OVERAUTO
      ohjalp.OAONR = TIDREGITAB.AONR ohjalp.ODELNR = TIDREGITAB.DELNR
      ohjalp.OUTR = TIDREGITAB.UTRYCKNING ohjalp.RECTIDVIS = RECID(TIDREGITAB)
      ohjalp.OAVBE = TIDREGITAB.LAGANTAL.
   END.                     
   GET NEXT traktq NO-LOCK.   
END.       
CLOSE QUERY traktq.

regstartspar = regstart.
regslutspar = regslut.
IF energiavt = TRUE THEN bdatum = bdatum.
ELSE DO:
   regdatum = bdatum.
   RUN REGVEC.P.
   RUN SLUTARB.P.
   IF hjadatum = bdatum AND bustart3 GE regstart AND regstart NE regslut THEN DO TRANSACTION:
      FIND FIRST ohjalp WHERE ohjalp.ODATUM = bdatum AND 
      ohjalp.OSTART < regstart NO-ERROR.
      IF AVAILABLE ohjalp THEN DELETE ohjalp.
      bort:
      REPEAT:
         FIND NEXT ohjalp WHERE ohjalp.ODATUM = bdatum AND 
         ohjalp.OSTART LE regstart  NO-ERROR.
         IF AVAILABLE ohjalp THEN DELETE ohjalp.
         ELSE LEAVE bort.
      END.
   END. 
   regdatum = avdatum.
   RUN REGVEC.P.
   RUN SLUTARB.P.
   IF hjbdatum = avdatum AND bdatum = avdatum AND bustart3 LE regstart 
   AND regstart NE regslut THEN DO TRANSACTION:  
      FIND FIRST ohjalp WHERE ohjalp.ODATUM = avdatum AND ohjalp.OSTART >
      regstart NO-ERROR.
      IF AVAILABLE ohjalp THEN DELETE ohjalp.
      bort:
      REPEAT:
         FIND NEXT ohjalp WHERE ohjalp.ODATUM = avdatum AND ohjalp.OSTART >
         regstart NO-ERROR.
         IF AVAILABLE ohjalp THEN DELETE ohjalp.
         ELSE LEAVE bort.
      END.
   END.   
END.   
IF bdatum NE avdatum THEN DO TRANSACTION:
   regdatum = avdatum.
   RUN REGVEC.P.
   RUN SLUTARB.P.
   IF regstart NE regslut THEN DO:
      FIND FIRST ohjalp WHERE ohjalp.ODATUM = avdatum AND ohjalp.OSTART >
      regstart NO-ERROR.
      IF AVAILABLE ohjalp THEN DELETE ohjalp.
      bort:
      REPEAT:
         FIND NEXT ohjalp WHERE ohjalp.ODATUM = avdatum AND ohjalp.OSTART >
         regstart NO-ERROR.
         IF AVAILABLE ohjalp THEN DELETE ohjalp.
         ELSE LEAVE bort.
      END.
   END.   
END.
IF hjadatum < bdatum THEN DO TRANSACTION:
   regdatum = hjadatum.
   RUN REGVEC.P.
   RUN SLUTARB.P.   
   IF regstart NE regslut THEN DO:  
      FIND FIRST ohjalp WHERE ohjalp.ODATUM = hjadatum AND ohjalp.OSTART <
      regstart NO-ERROR.
      IF AVAILABLE ohjalp THEN DELETE ohjalp.
      bort:
      REPEAT:
         FIND NEXT ohjalp WHERE ohjalp.ODATUM = hjadatum AND ohjalp.OSTART LE
         regstart  NO-ERROR.
         IF AVAILABLE ohjalp THEN DELETE ohjalp.
         ELSE LEAVE bort.
      END.
   END.
END.   
IF hjbdatum > avdatum  THEN DO TRANSACTION:
   regdatum = hjbdatum.
   RUN REGVEC.P.
   RUN SLUTARB.P.
   IF regstart NE regslut THEN DO:
      FIND FIRST ohjalp WHERE ohjalp.ODATUM = hjbdatum AND ohjalp.OSTART >
      regstart NO-ERROR.
      IF AVAILABLE ohjalp THEN DELETE ohjalp.
      bort:
      REPEAT:
         FIND NEXT ohjalp WHERE ohjalp.ODATUM = hjbdatum AND ohjalp.OSTART >
         regstart NO-ERROR.
         IF AVAILABLE ohjalp THEN DELETE ohjalp.
         ELSE LEAVE bort.      
      END.
   END.   
END.
regstart = regstartspar.
regslut = regslutspar.
IF energiavt = TRUE THEN DO:   
   RUN OVERKOM.P.
END. 
IF utryck = TRUE THEN DO:
   RUN OVERUTR.P.
END. 
IF UTRYCKNING.NODF = TRUE THEN DO TRANSACTION:
   IF UTRYCKNING.LAGOVER = TRUE THEN regstart = regstart.
   /* ES AB De som skickar hela l�nearten p� b�de p� 381 + 099. Allts� ej �vers�ttning med NFALL*/
   ELSE DO:
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = reco EXCLUSIVE-LOCK NO-ERROR.
      IF res = "" THEN ASSIGN TIDREGITAB.NODF = TRUE.
      IF TIDREGITAB.NODF = TRUE THEN DO: 
         IF TIDREGITAB.OKOD1 NE '' THEN DO: 
            FIND FIRST NFALL WHERE NFALL.KOD = TIDREGITAB.OKOD1 
            USE-INDEX KOD NO-LOCK NO-ERROR.
            IF AVAILABLE NFALL THEN  ASSIGN TIDREGITAB.OKOD1 = NFALL.OBYT.
         END.     
         IF TIDREGITAB.OKOD2 NE '' THEN DO: 
            FIND FIRST NFALL WHERE NFALL.KOD = TIDREGITAB.OKOD2 
            USE-INDEX KOD NO-LOCK NO-ERROR.
            IF AVAILABLE NFALL THEN  ASSIGN TIDREGITAB.OKOD2 = NFALL.OBYT.
         END.    
         IF TIDREGITAB.OKOD3 NE '' THEN DO: 
            FIND FIRST NFALL WHERE NFALL.KOD = TIDREGITAB.OKOD3 
            USE-INDEX KOD NO-LOCK NO-ERROR.
            IF AVAILABLE NFALL THEN  ASSIGN TIDREGITAB.OKOD3 = NFALL.OBYT.
         END.                                              
         IF bslut = 24.00 AND avstart = 00.00 THEN DO: 
            FIND TIDREGITAB WHERE RECID(TIDREGITAB) = reco EXCLUSIVE-LOCK 
            NO-ERROR.
            IF TIDREGITAB.NODF = TRUE THEN DO: 
               IF TIDREGITAB.OKOD1 NE '' THEN DO: 
                  FIND FIRST NFALL WHERE NFALL.KOD = TIDREGITAB.OKOD1 
                  USE-INDEX KOD NO-LOCK NO-ERROR.
                  IF AVAILABLE NFALL THEN  ASSIGN TIDREGITAB.OKOD1 = NFALL.OBYT.
               END.     
               IF TIDREGITAB.OKOD2 NE '' THEN DO: 
                  FIND FIRST NFALL WHERE NFALL.KOD = TIDREGITAB.OKOD2 
                  USE-INDEX KOD NO-LOCK NO-ERROR.
                  IF AVAILABLE NFALL THEN  ASSIGN TIDREGITAB.OKOD2 = NFALL.OBYT.
               END.    
               IF TIDREGITAB.OKOD3 NE '' THEN DO: 
                  FIND FIRST NFALL WHERE NFALL.KOD = TIDREGITAB.OKOD3
                  USE-INDEX KOD NO-LOCK NO-ERROR.
                  IF AVAILABLE NFALL THEN  ASSIGN TIDREGITAB.OKOD3 = NFALL.OBYT.
               END.
            END.
         END.   
      END.   
   END.
END.     
FIND FIRST BEREDSKAPTAB WHERE BEREDSKAPTAB.BEREDSKAPSAVTAL =
PERSONALTAB.BEREDSKAPSAVTAL USE-INDEX BERED NO-LOCK NO-ERROR.
IF AVAILABLE BEREDSKAPTAB AND  BEREDSKAPTAB.BERANTAL > 0 THEN regdatum = regdatum.                  
ELSE IF utryck = FALSE AND UTRYCKNING.AVBE = TRUE THEN DO TRANSACTION:
   FOR EACH ohjalp NO-LOCK:  
      /*Om ohjalp g�r �ver dygnsbrytning och det ej �r denna som ska tolkas
      (bustart3 ne ohjalp.ost1) tolka ej om det nya dygnet*/           
  
      IF ohjalp.OSL1 = 24.00 AND ohjalp.OSL2 > 0 THEN DO:      
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONAL =
         PERSONALTAB.PERSONALKOD AND TIDREGITAB.DATUM = ohjalp.ODATUM + 1 AND
         TIDREGITAB.BEREDSKAPSTART = 00.00 AND TIDREGITAB.BEREDSKAPSLUT > 0
         USE-INDEX PSTART EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:
            nytid = TIDREGITAB.BEREDSKAPSLUT.
            RUN TIMSEK.P.
            ASSIGN sekun = sekunder
            nytid = TIDREGITAB.BEREDSKAPSTART.
            RUN TIMSEK.P.
            ASSIGN seku = sekunder
            sekunder = sekun - seku.
            RUN SEKTIM.P.
            ASSIGN TIDREGITAB.BERANTAL = nytid.
         END.
      END.      
      IF ohjalp.OST1 = 0 AND ohjalp.OSL1 > 0 THEN DO:
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONAL =
         PERSONALTAB.PERSONALKOD AND TIDREGITAB.DATUM = ohjalp.ODATUM AND
         TIDREGITAB.BEREDSKAPSTART = 00.00 AND TIDREGITAB.BEREDSKAPSLUT > 0
         USE-INDEX PSTART EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:
            nytid = TIDREGITAB.BEREDSKAPSLUT.
            RUN TIMSEK.P.
            ASSIGN sekun = sekunder
            nytid = TIDREGITAB.BEREDSKAPSTART.
            RUN TIMSEK.P.
            ASSIGN seku = sekunder
            sekunder = sekun - seku.
            RUN SEKTIM.P.
            ASSIGN TIDREGITAB.BERANTAL = nytid.
         END.
      END.      
      IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONAL =
         PERSONALTAB.PERSONALKOD AND TIDREGITAB.DATUM = ohjalp.ODATUM AND
         TIDREGITAB.BEREDSKAPSTART LE ohjalp.OSTART AND
         TIDREGITAB.BEREDSKAPSLUT GE ohjalp.OSTART AND TIDREGITAB.BEREDSKAP NE "260" USE-INDEX PSTART
         EXCLUSIVE-LOCK NO-ERROR.
      END.
      ELSE DO:      
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONAL =
         PERSONALTAB.PERSONALKOD AND TIDREGITAB.DATUM = ohjalp.ODATUM AND
         TIDREGITAB.BEREDSKAPSTART LE ohjalp.OSTART AND
         TIDREGITAB.BEREDSKAPSLUT GE ohjalp.OSTART USE-INDEX PSTART
         EXCLUSIVE-LOCK NO-ERROR.
      END.
      IF AVAILABLE TIDREGITAB THEN DO:
         nytid = TIDREGITAB.BEREDSKAPSLUT.
         RUN TIMSEK.P.
         ASSIGN sekun = sekunder
         nytid = TIDREGITAB.BEREDSKAPSTART.
         RUN TIMSEK.P.
         ASSIGN seku = sekunder
         sekunder = sekun - seku.
         RUN SEKTIM.P.
         ASSIGN TIDREGITAB.BERANTAL = nytid.
         /*om �vertids-registreringen t�cker flera beredskapsregistreringar
         -nollst�ll b�da*/
         berhj = ohjalp.OSLUT.
         IF ohjalp.OSL1 > berhj THEN berhj = ohjalp.OSL1.
         IF ohjalp.OSL2 > berhj THEN berhj = ohjalp.OSL2.
         IF ohjalp.OSL3 > berhj THEN berhj = ohjalp.OSL3.
         IF berhj > TIDREGITAB.BEREDSKAPSLUT THEN DO:
            FIND FIRST tidbuff WHERE tidbuff.PERSONAL =
            PERSONALTAB.PERSONALKOD AND tidbuff.DATUM = ohjalp.ODATUM AND
            tidbuff.BEREDSKAPSTART = TIDREGITAB.BEREDSKAPSLUT 
            USE-INDEX PSTART NO-LOCK NO-ERROR.
            IF AVAILABLE tidbuff THEN DO:   
               avberrec = RECID(tidbuff).
               FIND tidbuff WHERE RECID(tidbuff) = avberrec EXCLUSIVE-LOCK NO-ERROR.
               nytid = tidbuff.BEREDSKAPSLUT.
               RUN TIMSEK.P.
               ASSIGN sekun = sekunder
               nytid = tidbuff.BEREDSKAPSTART.
               RUN TIMSEK.P.
               ASSIGN seku = sekunder
               sekunder = sekun - seku.
               RUN SEKTIM.P.
               ASSIGN tidbuff.BERANTAL = nytid.
            END.
         END.
         
      END.      
   END.   
   
   FIND FIRST ohjalp NO-LOCK NO-ERROR.
   IF AVAILABLE ohjalp THEN DO:   
      reco = ohjalp.RECTIDVIS.   
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD AND 
      TIDREGITAB.DATUM = ohjalp.ODATUM USE-INDEX PSTART NO-LOCK:
         IF ohjalp.OSTART GE TIDREGITAB.BEREDSKAPSTART AND
         ohjalp.OSTART < TIDREGITAB.BEREDSKAPSLUT THEN krav2 = TRUE.
         /* Beredskap 23-06 men �vertid kan vara 22-24 och skall dras*/
         IF ohjalp.OSTART < TIDREGITAB.BEREDSKAPSTART AND
         ohjalp.OSLUT > TIDREGITAB.BEREDSKAPSTART THEN krav2 = TRUE.
      END.
      IF krav2 = true THEN ber1 = utrber.
      ELSE ber1 = utrejber.      
      IF krav2 = TRUE THEN DO:             
         RUN AVBER.P.
      END.
      avdra:
      REPEAT:
         FIND NEXT ohjalp NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ohjalp THEN LEAVE avdra.
         reco = ohjalp.RECTIDVIS.    
         FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD
         AND TIDREGITAB.DATUM = ohjalp.ODATUM USE-INDEX PSTART NO-LOCK:
            IF ohjalp.OSTART GE TIDREGITAB.BEREDSKAPSTART AND
            ohjalp.OSTART < TIDREGITAB.BEREDSKAPSLUT THEN krav2 = TRUE.
            /* Beredskap 23-06 men �vertid kan vara 22-24 och skall dras*/
            IF ohjalp.OSTART < TIDREGITAB.BEREDSKAPSTART AND
            ohjalp.OSLUT > TIDREGITAB.BEREDSKAPSTART THEN krav2 = TRUE.
         END.
         IF krav2 = true THEN ber1 = utrber.
         ELSE ber1 = utrejber.
         IF krav2 = TRUE THEN DO: 
            RUN AVBER.P.
         END.
      END.
   END.
END.

FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD AND
TIDREGITAB.DATUM = bdatum AND TIDREGITAB.OVERAUTO = FALSE AND
( TIDREGITAB.OANT1 > 0  OR TIDREGITAB.OANT2 > 0 OR TIDREGITAB.OANT3 > 0 )
USE-INDEX PSTART NO-LOCK NO-ERROR.
IF AVAILABLE TIDREGITAB THEN DO:   
   OPEN QUERY traktq
   FOR EACH TIDREGITAB WHERE
   TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD AND TIDREGITAB.DATUM = bdatum AND 
   TIDREGITAB.OVERAUTO = TRUE  
   USE-INDEX PSTART NO-LOCK.
   DO TRANSACTION:
      GET FIRST traktq EXCLUSIVE-LOCK.   
      DO WHILE AVAILABLE(TIDREGITAB):                               
         ASSIGN TIDREGITAB.OKOD1 = ''  TIDREGITAB.OKOD2 = '' TIDREGITAB.OKOD3 = ''
         TIDREGITAB.OANT1 = 0 TIDREGITAB.OANT2 = 0 TIDREGITAB.OANT3 = 0.
         IF TIDREGITAB.TIDLOG = FALSE THEN DO:
            TIDREGITAB.OVERAUTO = FALSE.	                    
         END.   
         GET NEXT traktq EXCLUSIVE-LOCK.   
      END.
   END.
END.   
CLOSE QUERY traktq.
IF avdatum > bdatum THEN DO: 
   FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD
   AND TIDREGITAB.DATUM = avdatum AND TIDREGITAB.OVERAUTO = FALSE AND
   ( TIDREGITAB.OANT1 > 0  OR TIDREGITAB.OANT2 > 0 OR TIDREGITAB.OANT3 > 0 )
   USE-INDEX PSTART NO-LOCK NO-ERROR. 
   IF AVAILABLE TIDREGITAB THEN DO:
      OPEN QUERY traktq
      FOR EACH TIDREGITAB WHERE
      TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD AND TIDREGITAB.DATUM = avdatum AND 
      TIDREGITAB.OVERAUTO = TRUE 
      USE-INDEX PSTART NO-LOCK.
      DO TRANSACTION:
         GET FIRST traktq EXCLUSIVE-LOCK.   
         DO WHILE AVAILABLE(TIDREGITAB):                                                     
            ASSIGN TIDREGITAB.OKOD1 = ''  TIDREGITAB.OKOD2 = '' TIDREGITAB.OKOD3 = ''
            TIDREGITAB.OANT1 = 0 TIDREGITAB.OANT2 = 0 TIDREGITAB.OANT3 = 0.
            IF TIDREGITAB.TIDLOG = FALSE THEN DO:
               TIDREGITAB.OVERAUTO = FALSE.	               
            END.   
            GET NEXT traktq EXCLUSIVE-LOCK.   
         END.   
      END.
   END.
   CLOSE QUERY traktq.
END.