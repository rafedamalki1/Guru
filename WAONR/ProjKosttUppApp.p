
/*------------------------------------------------------------------------
    File        : ProjKosttUppApp.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri May 23 13:37:42 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/
{DIRDEF.I}
&Scoped-define NEW NEW 
{REGVAR.I}

DEFINE VARIABLE ovkod LIKE TIDREGITAB.OVERTIDTILL NO-UNDO.
DEFINE VARIABLE okodtext AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ovantal LIKE TIDREGITAB.OVERANTAL NO-UNDO.
DEFINE VARIABLE ovbelopp LIKE EKRAPPRESULT.EOVERBELOPP NO-UNDO. 

&Scoped-define NEW NEW 
{FAKTTEMP.I}
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}
FUNCTION klockan100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):
  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.

FUNCTION klockan60 RETURNS DECIMAL
  ( INPUT ber100 AS DECIMAL ):
  RETURN TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) / 100) * 60 . 
END FUNCTION.


FUNCTION runda RETURNS DECIMAL
  ( INPUT varedin AS DECIMAL) :
  RETURN ROUND(varedin,0).   /* Function return value. */
END FUNCTION.

DEFINE TEMP-TABLE tidertemp NO-UNDO LIKE TIDFEL
   FIELD KOD AS CHARACTER
   INDEX KOD KOD.
DEFINE TEMP-TABLE kundbeftemp NO-UNDO LIKE FAKTBEF
   FIELD VIBEFATTNING AS CHARACTER.

DEFINE INPUT PARAMETER aonrvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER delnrvar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR uppvaltemp.
DEFINE OUTPUT PARAMETER TABLE FOR sumpers.
DEFINE OUTPUT PARAMETER TABLE FOR egfaktemp.

FIND FIRST FORETAG WHERE NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
EMPTY TEMP-TABLE sumtidtemp NO-ERROR. 
EMPTY TEMP-TABLE egfaktemp NO-ERROR. 

RUN STYRFORE.P (INPUT Guru.Konstanter:globforetag).

FOR EACH BEFATTNINGSTAB WHERE NO-LOCK:
   CREATE kundbeftemp.
   ASSIGN
   kundbeftemp.BEFATTNING =  BEFATTNINGSTAB.BEFATTNING  
   kundbeftemp.VIBEFATTNING = BEFATTNINGSTAB.NAMN.      
END.      

FIND FIRST uppvaltemp WHERE NO-LOCK NO-ERROR.   
RUN openfalt_UI.
RUN kostreg_UI.
RUN summa_UI.
{GDPRLOGGCLIENT.I}
FIND FIRST egfaktemp WHERE NO-LOCK NO-ERROR.
egfaktemp.TOTALT = egfaktemp.MTRL + egfaktemp.OVRIG + egfaktemp.ARBKOST + 
                        egfaktemp.OBELOPP + egfaktemp.TRAKT + egfaktemp.RES + 
                        egfaktemp.LONTILL + egfaktemp.KBELOPP.
 /*
FOR EACH egfaktemp WHERE NO-LOCK:
   DISPLAY egfaktemp WITH FRAME cc 2 columns.
END.
FOR EACH sumpers  BY sumpers.ORDNING BY sumpers.AONR BY sumpers.DELNR:
   DISPLAY sumpers.VIBEFATTNING FORMAT "X(22)" sumpers.AONR
 sumpers.TIMMAR sumpers.OTIMMAR sumpers.BELOPP WITH FRAME cc2 down.
 
END.
 */
PROCEDURE openfalt_UI :
   DEFINE VARIABLE pkod AS CHARACTER NO-UNDO.
   DEFINE VARIABLE sumanstf AS CHARACTER NO-UNDO.
   DEFINE VARIABLE sumanstraf AS CHARACTER NO-UNDO.
   
   FIND FIRST egfaktemp WHERE egfaktemp.ORDNING = 3 NO-ERROR.
   IF NOT AVAILABLE  egfaktemp THEN CREATE egfaktemp.
   ASSIGN
   egfaktemp.AONR = aonrvar
   egfaktemp.DELNR = delnrvar 
   egfaktemp.TYPTEXT = "Summerade kostnader" 
   egfaktemp.ORDNING = 3.
   RUN overegna_UI.

   RUN tladda_UI (INPUT 1).                       
   RUN tidreg_UI.
   REPEAT:
      FIND FIRST sumtidtemp WHERE sumtidtemp.ANSF = "" NO-LOCK NO-ERROR.
      IF NOT AVAILABLE sumtidtemp THEN DO:
         LEAVE.
      END.
      pkod = sumtidtemp.PERSONALKOD.
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod NO-LOCK NO-ERROR.

      FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING NO-LOCK NO-ERROR.
      sumanstf = ANSTFORMTAB.KOD.
      sumanstraf = PERSONALTAB.TRAAVTAL.
      FOR EACH sumtidtemp WHERE sumtidtemp.PERSONALKOD = pkod:
         ASSIGN
         sumtidtemp.ANSF     = sumanstf  
         sumtidtemp.TRAAVTAL = sumanstraf.  
      END.
   END.
   
   RUN befat_UI.   
END PROCEDURE.



PROCEDURE overegna_UI :
   RUN utan_UI.
   
END PROCEDURE.

PROCEDURE utan_UI:
   /*obs inga tidfelar */
   IF uppvaltemp.VISPERAR = ? THEN DO:
      OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = aonrvar AND TIDREGITAB.DELNR = delnrvar NO-LOCK.
   END.
   ELSE DO:
      OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = aonrvar AND TIDREGITAB.DELNR = delnrvar AND TIDREGITAB.DATUM >= uppvaltemp.STARTDATUM AND 
      TIDREGITAB.DATUM <= uppvaltemp.SLUTDATUM NO-LOCK.
   END.      
   GET FIRST tidq NO-LOCK. 
   DO WHILE AVAILABLE(TIDREGITAB):         
      IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN.
      ELSE DO:
        IF uppvaltemp.VISGODKANDA = TRUE AND TIDREGITAB.VECKOKORD = "" THEN.
         ELSE DO:
            CREATE tidertemp.
            BUFFER-COPY TIDREGITAB TO tidertemp.
            tidertemp.DEBET = TRUE.
         END.
      END.   
      GET NEXT tidq NO-LOCK. 
   END.
   RUN anst_UI.
   FOR EACH tidertemp WHERE tidertemp.AONR = aonrvar AND tidertemp.DELNR = delnrvar:
      RUN nytid_UI.                             
   END.        
   RUN bef_UI.
END PROCEDURE.

PROCEDURE anst_UI :
   DEFINE VARIABLE pkod AS CHARACTER NO-UNDO.
   REPEAT:
      FIND FIRST tidertemp WHERE tidertemp.KOD = "" NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidertemp THEN LEAVE.
      pkod = tidertemp.PERSONALKOD.
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod 
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE PERSONALTAB THEN DO:
         FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
         USE-INDEX ANSTF NO-LOCK NO-ERROR.  
         IF NOT AVAILABLE ANSTFORMTAB THEN FIND FIRST ANSTFORMTAB WHERE NO-LOCK NO-ERROR.
      END.
      ELSE FIND FIRST ANSTFORMTAB WHERE NO-LOCK NO-ERROR.
      FOR EACH tidertemp WHERE tidertemp.KOD = "" AND tidertemp.PERSONALKOD = pkod:
         tidertemp.KOD = ANSTFORMTAB.KOD.
      END.
   END.
END PROCEDURE.

PROCEDURE bef_UI :
   FIND FIRST tidtemp WHERE tidtemp.NYBEF = FALSE USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   REPEAT:      
      IF NOT AVAILABLE tidtemp THEN LEAVE.                 
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = tidtemp.PERSONALKOD
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE PERSONALTAB THEN DO:
         FIND FIRST ANSTFORMTAB WHERE 
         ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
         USE-INDEX ANSTF NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ANSTFORMTAB THEN FIND FIRST ANSTFORMTAB NO-LOCK NO-ERROR.
         FOR EACH tidtemp WHERE tidtemp.PERSONALKOD = PERSONALTAB.PERSONALKOD 
         USE-INDEX PERSONALKOD:
            ASSIGN 
            tidtemp.NYBEF = TRUE
            tidtemp.KOD = ANSTFORMTAB.KOD.
            IF tidtemp.BEFATTNING = "" THEN DO:
               ASSIGN
               tidtemp.PERSMASK = PERSONALTAB.PERSMASK
               tidtemp.BEFATTNING = PERSONALTAB.BEFATTNING.                                    
            END.
            ELSE DO:
               FIND FIRST BEFATTNINGSTAB WHERE BEFATTNINGSTAB.BEFATTNING = tidtemp.BEFATTNING NO-LOCK NO-ERROR.
               IF AVAILABLE BEFATTNINGSTAB THEN tidtemp.PERSMASK = BEFATTNINGSTAB.PERSMASK.
            END.
            ASSIGN
            tidtemp.NAMN = SUBSTRING(PERSONALTAB.FORNAMN,1,1) + "." + PERSONALTAB.EFTERNAMN.   
            tidtemp.TRAAVTAL =  PERSONALTAB.TRAAVTAL.
         END.         
      END.             
      ELSE DO:
         FIND FIRST ANSTFORMTAB NO-LOCK NO-ERROR.
         ASSIGN
         tidtemp.NYBEF = TRUE
         tidtemp.KOD = ANSTFORMTAB.KOD.
         FIND FIRST BEFATTNINGSTAB NO-LOCK NO-ERROR.
         IF tidtemp.BEFATTNING = "" THEN DO:
            ASSIGN
            tidtemp.PERSMASK = TRUE
            tidtemp.BEFATTNING = BEFATTNINGSTAB.BEFATTNING.                                    
         END.
         ASSIGN
         tidtemp.NAMN = "Saknas".         
      END.
      FIND FIRST tidtemp WHERE tidtemp.NYBEF = FALSE USE-INDEX PERSONALKOD NO-ERROR.
   END.   
   REPEAT: 
      FIND FIRST extrasum NO-ERROR.
      IF NOT AVAILABLE extrasum THEN LEAVE.
      
      CREATE tidtemp.
      BUFFER-COPY extrasum TO tidtemp.
      ASSIGN
      tidtemp.NYBEF = TRUE
      tidtemp.PRIS = extrasum.PRISA  
      tidtemp.START = extrasum.START
      tidtemp.SLUT = extrasum.SLUT      
      tidtemp.OKOST = extrasum.OBELOPP
      tidtemp.OTIMMAR = extrasum.OTIMMAR    
      tidtemp.OANT1 = extrasum.OANT1.
      DELETE extrasum.
      FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = tidtemp.DATUM AND 
      OVERAVTAB.KOD = tidtemp.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
      IF AVAILABLE OVERAVTAB THEN DO:
         ASSIGN tidtemp.EQDAG = OVERAVTAB.EQDAG.
      END. 
      ELSE DO:
         ASSIGN tidtemp.EQDAG = WEEKDAY(tidtemp.DATUM).
      END.            
   END.      
END PROCEDURE.


PROCEDURE nytid_UI :
   
   IF tidertemp.TIDLOG = TRUE THEN DO:
      IF tidertemp.PRISTYP = "RESTID..." THEN DO:
         CREATE tidtemp.
         ASSIGN
         tidtemp.DEBET = tidertemp.DEBET
         tidtemp.NYBEF = FALSE
         tidtemp.PERSONALKOD = tidertemp.PERSONALKOD
         tidtemp.PRISTYP = tidertemp.PRISTYP
         tidtemp.AONR = tidertemp.AONR
         tidtemp.DELNR = tidertemp.DELNR
         tidtemp.DATUM = tidertemp.DATUM
         tidtemp.RESPRIS = tidertemp.PRIS
         tidtemp.START = tidertemp.START      
         tidtemp.SLUT = tidertemp.SLUT
         tidtemp.BEFATTNING = tidertemp.OVERTIDTILL .
         tidtemp.TOTALT = klockan100(tidertemp.TOTALT).
         ASSIGN  
         tidtemp.RESTIM = klockan100(tidtemp.SLUT) - klockan100(tidtemp.START).
         tidtemp.LUNCH = (klockan100(tidtemp.SLUT) - klockan100(tidtemp.START)) - tidtemp.TOTALT.
         ASSIGN
         tidtemp.RESKOSTDEC = tidtemp.RESTIM * tidtemp.RESPRIS.
         tidtemp.TOTALT = 0.     
      END.
      ELSE DO:
         CREATE tidtemp.
         ASSIGN
         tidtemp.DEBET = tidertemp.DEBET
         tidtemp.NYBEF = FALSE
         tidtemp.PERSONALKOD = tidertemp.PERSONALKOD
         tidtemp.PRISTYP = tidertemp.PRISTYP
         tidtemp.AONR = tidertemp.AONR
         tidtemp.DELNR = tidertemp.DELNR
         tidtemp.DATUM = tidertemp.DATUM
         tidtemp.START = tidertemp.START
         tidtemp.SLUT = tidertemp.SLUT
         tidtemp.BEFATTNING = tidertemp.OVERTIDTILL .
         tidtemp.TOTALT = klockan100(tidertemp.TOTALT).
         ASSIGN
         tidtemp.PRIS = tidertemp.PRIS      
         tidtemp.OBER = FALSE.
         ASSIGN 
         tidtemp.KOST = tidtemp.TOTALT * tidtemp.PRIS
         tidtemp.LUNCH = (klockan100(tidtemp.SLUT) - klockan100(tidtemp.START)) - tidtemp.TOTALT.       
         IF tidertemp.OKOD1 NE " " THEN DO: 
            ASSIGN                     
            ovkod = tidertemp.OKOD1
            nytid = tidertemp.OANT1.
            RUN TIMSEK.P.
            ovantal = (sekunder / 3600).
            RUN over_UI.
            tidtemp.BOKKONTO = okodtext.
            tidtemp.OKOST = ovbelopp.
            tidtemp.OANT1 = (DECIMAL(SUBSTRING(STRING(tidertemp.OANT1,"99.99"),1,2)) + DECIMAL(SUBSTRING(STRING(tidertemp.OANT1,"99.99"),4,2)) / 60).
                         
            IF tidtemp.OANT1 NE 0 THEN DO: 
               ASSIGN
               tidtemp.OTIMMAR = tidtemp.OANT1
               tidtemp.KOST = 0
               tidtemp.TOTALT = 0
               tidtemp.OPRIS = tidtemp.OKOST /  tidtemp.OANT1.
            END.
         END.    
         RUN noll_UI.
         IF tidertemp.OKOD2 NE " " THEN DO:
            CREATE tidtempbuff.
            ASSIGN
            tidtempbuff.DEBET = tidertemp.DEBET
            tidtempbuff.NYBEF = FALSE
            tidtempbuff.PERSONALKOD = tidertemp.PERSONALKOD
            tidtempbuff.PRISTYP = tidertemp.PRISTYP
            tidtempbuff.AONR = tidertemp.AONR
            tidtempbuff.DELNR = tidertemp.DELNR
            tidtempbuff.DATUM = tidertemp.DATUM
            tidtempbuff.BEFATTNING = tidertemp.OVERTIDTILL. 
            ASSIGN                     
            ovkod = tidertemp.OKOD2
            nytid = tidertemp.OANT2.
            RUN TIMSEK.P.
            ovantal = (sekunder / 3600).
            RUN over_UI.
            tidtempbuff.BOKKONTO = okodtext.                       
            tidtempbuff.OKOST = ovbelopp.
            tidtempbuff.OANT1 = (DECIMAL(SUBSTRING(STRING(tidertemp.OANT2,"99.99"),1,2)) + DECIMAL(SUBSTRING(STRING(tidertemp.OANT2,"99.99"),4,2)) / 60).
                         
            IF tidtempbuff.OANT1 NE 0 THEN DO: 
               ASSIGN
               tidtempbuff.OTIMMAR = tidtempbuff.OANT1
               tidtempbuff.KOST = 0
               tidtempbuff.TOTALT = 0
               tidtempbuff.OPRIS = tidtempbuff.OKOST /  tidtempbuff.OANT1.
            END.
         END.    
         RUN noll_UI.
         IF tidertemp.OKOD3 NE " " THEN DO:
            CREATE tidtempbuff.
            ASSIGN
            tidtempbuff.DEBET = tidertemp.DEBET
            tidtempbuff.NYBEF = FALSE
            tidtempbuff.PERSONALKOD = tidertemp.PERSONALKOD
            tidtempbuff.PRISTYP = tidertemp.PRISTYP
            tidtempbuff.AONR = tidertemp.AONR
            tidtempbuff.DELNR = tidertemp.DELNR
            tidtempbuff.DATUM = tidertemp.DATUM
            tidtempbuff.BEFATTNING = tidertemp.OVERTIDTILL.  
            ASSIGN                     
            ovkod = tidertemp.OKOD3
            nytid = tidertemp.OANT3.
            RUN TIMSEK.P.
            ovantal = (sekunder / 3600).
            RUN over_UI. 
            tidtempbuff.BOKKONTO = okodtext.                      
            tidtempbuff.OKOST = ovbelopp.
            tidtempbuff.OANT1 = (DECIMAL(SUBSTRING(STRING(tidertemp.OANT3,"99.99"),1,2)) + DECIMAL(SUBSTRING(STRING(tidertemp.OANT3,"99.99"),4,2)) / 60).
                         
            IF tidtempbuff.OANT1 NE 0 THEN DO: 
               ASSIGN
               tidtempbuff.OTIMMAR = tidtempbuff.OANT1
               tidtempbuff.KOST = 0
               tidtempbuff.TOTALT = 0
               tidtempbuff.OPRIS = tidtempbuff.OKOST /  tidtempbuff.OANT1.
            END.
         END.    
         RUN noll_UI.  
         /*               
         tidtemp.OANT1 = ((DECIMAL(SUBSTRING(STRING(tidertemp.OANT1,"99.99"),1,2)) + 
                          DECIMAL(SUBSTRING(STRING(tidertemp.OANT1,"99.99"),4,2)) / 60) +
                         (DECIMAL(SUBSTRING(STRING(tidertemp.OANT2,"99.99"),1,2)) + 
                          DECIMAL(SUBSTRING(STRING(tidertemp.OANT2,"99.99"),4,2)) / 60) +
                         (DECIMAL(SUBSTRING(STRING(tidertemp.OANT3,"99.99"),1,2)) + 
                          DECIMAL(SUBSTRING(STRING(tidertemp.OANT3,"99.99"),4,2)) / 60)).
         IF tidtemp.OANT1 NE 0 THEN DO: 
            ASSIGN
            tidtemp.OTIMMAR = tidtemp.OANT1
            tidtemp.KOST = 0
            tidtemp.TOTALT = 0
            tidtemp.OPRIS = tidtemp.OKOST /  tidtemp.OANT1.
         END.
   */
      END.
   END.
      
END PROCEDURE.

PROCEDURE noll_UI:
   ASSIGN    
   ovbelopp = 0
   okodtext = "".                    
   ovkod = " ".            
END PROCEDURE. 


PROCEDURE over_UI:
   DEFINE VARIABLE multi LIKE OVERKOD.MULTIP NO-UNDO.
   multi = 0.
   FIND FIRST OVERKOD WHERE OVERKOD.KOD = tidertemp.KOD AND
   OVERKOD.OVERTIDTILL = ovkod 
   USE-INDEX OVER NO-LOCK NO-ERROR.
   IF AVAILABLE OVERKOD THEN DO:
      okodtext = OVERKOD.LONKODTEXT. 
      multi = OVERKOD.MULTIP.      
   END.  
   
   IF okodtext = "Kompledig 1.5" THEN okodtext = "Övertid enkel".
   IF okodtext = "Kompledig 2" THEN okodtext = "Övertid Kval".
   
   ovbelopp = (tidtemp.PRIS + (tidtemp.PRIS * multi)) * ovantal.    
   
END.

PROCEDURE tladda_UI :
   DEFINE INPUT PARAMETER visatotvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE pkod AS CHARACTER NO-UNDO.
   DEFINE VARIABLE sumanstf AS CHARACTER NO-UNDO.
   DEFINE VARIABLE sumanstraf AS CHARACTER NO-UNDO.
   pkod = "".
   FOR EACH tidtemp USE-INDEX PKOD:         
      CREATE sumtidtemp.
      IF pkod NE tidtemp.PERSONALKOD THEN DO:               
         ASSIGN
         sumanstf   = tidtemp.KOD
         sumanstraf = tidtemp.TRAAVTAL.
         pkod = tidtemp.PERSONALKOD.
      END.
      ASSIGN
      sumtidtemp.FELKORD = tidtemp.FELKORD
      sumtidtemp.VECKOKORD = tidtemp.VECKOKORD
      sumtidtemp.ANSF     = sumanstf  
      sumtidtemp.TRAAVTAL = sumanstraf
      sumtidtemp.PERSMASK = tidtemp.PERSMASK
      sumtidtemp.BEFATTNING = tidtemp.BEFATTNING               
      sumtidtemp.NAMN = tidtemp.NAMN.   
      IF tidtemp.PRISTYP = "RESTID..." THEN DO:
         ASSIGN
         sumtidtemp.BOKKONTO = tidtemp.BOKKONTO
         sumtidtemp.TRAKTKOD     = tidtemp.TRAKTKOD    
         sumtidtemp.TRAKTANTAL   = tidtemp.TRAKTANTAL  
         sumtidtemp.LONTILLAGG   = tidtemp.LONTILLAGG  
         sumtidtemp.LONTILLANTAL = tidtemp.LONTILLANTAL
         sumtidtemp.OTEXTID = tidtemp.OTEXTID
         sumtidtemp.DATUM = tidtemp.DATUM
         sumtidtemp.GSTART = tidtemp.START
         sumtidtemp.GSLUT = tidtemp.SLUT
         sumtidtemp.START = tidtemp.START
         sumtidtemp.SLUT = tidtemp.SLUT
         sumtidtemp.PERSONALKOD = tidtemp.PERSONALKOD
         sumtidtemp.AONR = tidtemp.AONR
         sumtidtemp.DELNR = tidtemp.DELNR                  
         sumtidtemp.MED = TRUE 
         sumtidtemp.RESPRIS = tidtemp.RESPRIS 
         sumtidtemp.RESKOSTDEC = sumtidtemp.RESKOSTDEC + tidtemp.RESKOSTDEC
         sumtidtemp.RESTIM = sumtidtemp.RESTIM + tidtemp.RESTIM.
      END.
      ELSE DO:      
         ASSIGN
         sumtidtemp.BOKKONTO = tidtemp.BOKKONTO
         sumtidtemp.TRAKTKOD     = tidtemp.TRAKTKOD    
         sumtidtemp.TRAKTANTAL   = tidtemp.TRAKTANTAL  
         sumtidtemp.LONTILLAGG   = tidtemp.LONTILLAGG  
         sumtidtemp.LONTILLANTAL = tidtemp.LONTILLANTAL
         sumtidtemp.START = tidtemp.START
         sumtidtemp.SLUT = tidtemp.SLUT
         sumtidtemp.GSTART = tidtemp.START 
         sumtidtemp.GSLUT = tidtemp.SLUT         
         sumtidtemp.OTEXTID = tidtemp.OTEXTID
         sumtidtemp.DATUM = tidtemp.DATUM
         sumtidtemp.PERSONALKOD = tidtemp.PERSONALKOD
         sumtidtemp.AONR = tidtemp.AONR
         sumtidtemp.DELNR = tidtemp.DELNR         
         sumtidtemp.TIMMAR = tidtemp.TOTALT
         sumtidtemp.BELOPP = tidtemp.KOST 
         sumtidtemp.OBELOPP = tidtemp.OKOST
         sumtidtemp.OTIMMAR = tidtemp.OTIMMAR                                        
         sumtidtemp.PRISA = tidtemp.PRIS
         sumtidtemp.OPRIS = tidtemp.OPRIS
         sumtidtemp.LUNCH = tidtemp.LUNCH
         sumtidtemp.MED = TRUE.   
      END.
      IF visatotvar = 1 THEN DO:
         ASSIGN
         egfaktemp.OBELOPP = egfaktemp.OBELOPP + tidtemp.OKOST
         egfaktemp.ARBKOST = egfaktemp.ARBKOST + tidtemp.KOST.                        
         IF tidtemp.PRISTYP = "RESTID..." THEN DO:
            egfaktemp.RES = egfaktemp.RES + tidtemp.RESKOSTDEC.
         END.
      END.
   END.      
   pkod = "".  
END PROCEDURE.

PROCEDURE tidreg_UI :
   RUN utantra_UI.
  
END PROCEDURE.
PROCEDURE utantra_UI:
   DEFINE VARIABLE lonantal AS DECIMAL NO-UNDO.
   DEFINE VARIABLE antalvar AS DECIMAL NO-UNDO.
   DEFINE VARIABLE kodvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE tillvar AS CHARACTER NO-UNDO.
   REPEAT:
      FIND FIRST sumtidtemp WHERE sumtidtemp.LONTILLAGG NE "" AND sumtidtemp.LONTAGEN = FALSE
      NO-ERROR.
      IF AVAILABLE sumtidtemp THEN DO:        
         ASSIGN
         kodvar = sumtidtemp.ANSF
         tillvar = sumtidtemp.LONTILLAGG.
         FIND FIRST LONTILL WHERE LONTILL.KOD = kodvar AND
         LONTILL.LONTILLAGG = sumtidtemp.LONTILLAGG
         USE-INDEX LON NO-LOCK NO-ERROR.
         IF NOT AVAILABLE LONTILL THEN DO:
            FOR EACH sumtidtemp WHERE sumtidtemp.ANSF = kodvar AND 
            sumtidtemp.LONTILLAGG = tillvar:
               ASSIGN
               sumtidtemp.LONTAGEN = TRUE
               sumtidtemp.LONTILLAGG = ""
               sumtidtemp.LONTILLANTAL = 0.
            END.
            NEXT.
         END.
         ELSE IF SUBSTRING(LONTILL.TYPKOD,5,3) NE "FAK" THEN DO:
            FOR EACH sumtidtemp WHERE sumtidtemp.ANSF = kodvar AND
            sumtidtemp.LONTILLAGG = tillvar:  
               ASSIGN
               sumtidtemp.LONTAGEN = TRUE
               sumtidtemp.LONTILLAGG = ""
               sumtidtemp.LONTILLANTAL = 0.
            END.
            NEXT.
         END.
         ELSE DO:                    
            FOR EACH sumtidtemp WHERE sumtidtemp.ANSF = kodvar AND 
            sumtidtemp.LONTILLAGG = tillvar:  
               ASSIGN
               sumtidtemp.LONTAGEN = TRUE
               sumtidtemp.VILART = LONTILL.VILART.
               IF LONTILL.ENHET = "Km" THEN antalvar = 0.1.
               ELSE antalvar = 1. 

               IF LONTILL.ENHET = "Km" OR LONTILL.ENHET = "MIL" THEN DO:
                  ASSIGN
                  sumtidtemp.LONKOST = sumtidtemp.LONTILLANTAL * antalvar * FAKTREGLER.MIL.                  
                  sumtidtemp.LONTILLANTAL = sumtidtemp.LONTILLANTAL * antalvar.                             
               END.
               ELSE DO:
                  lonantal = sumtidtemp.LONTILLANTAL.
                  IF LONTILL.ENHET = "TI" THEN DO:
                     nytid = lonantal.
                     RUN TIMSEK.P.
                     lonantal = (sekunder / 3600). 
                  END.
                  sumtidtemp.LONKOST = (lonantal * LONTILL.ERSATTNING).                          
                  sumtidtemp.LONTILLANTAL = sumtidtemp.LONTILLANTAL.                             
               END.                             
            END.            
         END.
      END.
      ELSE LEAVE.
   END.
   REPEAT:
      FIND FIRST sumtidtemp WHERE sumtidtemp.TRAKTKOD NE "" AND sumtidtemp.TRATAGEN = FALSE
      NO-ERROR.
      IF AVAILABLE sumtidtemp THEN DO:        
         ASSIGN
         kodvar = sumtidtemp.TRAAVTAL
         tillvar = sumtidtemp.TRAKTKOD.
         FIND FIRST TRAKTATAB WHERE TRAKTATAB.TRAAVTAL = kodvar AND 
         TRAKTATAB.TRAKTKOD = tillvar  
         NO-LOCK NO-ERROR.         
         IF NOT AVAILABLE TRAKTATAB THEN DO:
            FOR EACH sumtidtemp WHERE sumtidtemp.TRAAVTAL = kodvar AND 
            sumtidtemp.TRAKTKOD = tillvar:  
               ASSIGN
               sumtidtemp.TRATAGEN = TRUE
               sumtidtemp.TRAKTKOD = ""
               sumtidtemp.TRAKTANTAL = 0.
            END.
            NEXT.
         END.
         ELSE DO:            
            FIND FIRST TRAKTFLER WHERE TRAKTFLER.TRAAVTAL = kodvar AND 
            TRAKTFLER.TRAKTKOD = tillvar  
            USE-INDEX FLTRAKT NO-LOCK NO-ERROR.        
            FOR EACH sumtidtemp WHERE sumtidtemp.TRAAVTAL = kodvar AND 
            sumtidtemp.TRAKTKOD = tillvar:  
               ASSIGN
               sumtidtemp.TRATAGEN = TRUE
               sumtidtemp.VITRAKT = TRAKTATAB.VILART.
               sumtidtemp.TBELOPP = sumtidtemp.TRAKTANTAL * TRAKTATAB.ERSATTNING.                                                        
            END.            
         END.
      END.
      ELSE LEAVE.
   END.

END PROCEDURE.
PROCEDURE befat_UI :
   OPEN QUERY sq FOR EACH sumtidtemp NO-LOCK,
   EACH OVERTEXTFAKT WHERE OVERTEXTFAKT.OTEXTID = sumtidtemp.OTEXTID NO-LOCK.
   GET FIRST sq EXCLUSIVE-LOCK.
   DO WHILE AVAILABLE(sumtidtemp):
      ASSIGN sumtidtemp.VIOBEFATTNING = OVERTEXTFAKT.OTEXT. 
      GET NEXT sq EXCLUSIVE-LOCK.
   END.
   OPEN QUERY sbq FOR EACH sumtidtemp NO-LOCK,
   EACH BEFATTNINGSTAB WHERE BEFATTNINGSTAB.BEFATTNING = sumtidtemp.BEFATTNING NO-LOCK.
   GET FIRST sbq EXCLUSIVE-LOCK.
   DO WHILE AVAILABLE(sumtidtemp):
      ASSIGN 
      sumtidtemp.PERSBEF = sumtidtemp.PERSONALKOD + " " + BEFATTNINGSTAB.NAMN
      sumtidtemp.VIBEFATTNING = BEFATTNINGSTAB.NAMN. 
      GET NEXT sbq EXCLUSIVE-LOCK.
   END.
   OPEN QUERY slq FOR EACH sumtidtemp NO-LOCK,
   EACH LONTILL WHERE LONTILL.LONTILLAGG = sumtidtemp.LONTILLAGG AND
   LONTILL.KOD = sumtidtemp.ANSF NO-LOCK.
   GET FIRST slq EXCLUSIVE-LOCK.
   DO WHILE AVAILABLE(LONTILL):
      ASSIGN sumtidtemp.VILART = LONTILL.VILART. 
      GET NEXT slq EXCLUSIVE-LOCK.
   END.
   OPEN QUERY stq FOR EACH sumtidtemp NO-LOCK,
   EACH TRAKTATAB WHERE TRAKTATAB.TRAAVTAL = sumtidtemp.TRAAVTAL AND
   TRAKTATAB.TRAKTKOD = sumtidtemp.TRAKTKOD NO-LOCK.
   GET FIRST stq EXCLUSIVE-LOCK.
   DO WHILE AVAILABLE(TRAKTATAB):
      ASSIGN sumtidtemp.VITRAKT = TRAKTATAB.VILART. 
      GET NEXT stq EXCLUSIVE-LOCK.
   END.
END PROCEDURE.




PROCEDURE summa_UI :
   DEFINE VARIABLE ohj AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE sumpers NO-ERROR. 
   ohj = ohj + 1.
   
   
   ohj = ohj + 1.
   FOR EACH sumtidtemp WHERE sumtidtemp.MED = TRUE  BREAK 
   BY sumtidtemp.RESPRIS BY sumtidtemp.AONR BY sumtidtemp.DELNR:         
      ACCUMULATE 
      sumtidtemp.RESKOSTDEC (TOTAL BY sumtidtemp.RESPRIS BY sumtidtemp.AONR BY sumtidtemp.DELNR).
      ACCUMULATE 
      sumtidtemp.RESTIM (TOTAL BY sumtidtemp.AONR BY sumtidtemp.RESPRIS BY sumtidtemp.AONR BY sumtidtemp.DELNR).
      IF LAST-OF(sumtidtemp.DELNR) THEN DO:
         IF (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.RESTIM) NE 0 THEN DO:
            CREATE sumpers.
            ASSIGN                
            sumpers.VIBEFATTNING = "Restidskostnad"            
            sumpers.ORDNING = ohj
            sumpers.AONR = sumtidtemp.AONR
            sumpers.DELNR = sumtidtemp.DELNR               
            sumpers.OTIMMAR = sumtidtemp.RESPRIS 
            sumpers.TIMMAR = 
            (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.RESTIM)
            sumpers.BELOPP = 
            (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.RESKOSTDEC).      
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + sumpers.PERSONALKOD.
                      
         END.
      END.
   END.
   ohj = ohj + 1.
   FOR EACH sumtidtemp WHERE sumtidtemp.MED = TRUE  BREAK 
   BY sumtidtemp.TRAKTKOD BY sumtidtemp.AONR BY sumtidtemp.DELNR:         
      ACCUMULATE 
      sumtidtemp.TBELOPP (TOTAL BY sumtidtemp.TRAKTKOD BY sumtidtemp.AONR BY sumtidtemp.DELNR).
      ACCUMULATE 
      sumtidtemp.TRAKTANTAL (TOTAL BY sumtidtemp.TRAKTKOD BY sumtidtemp.AONR BY sumtidtemp.DELNR).
      IF LAST-OF(sumtidtemp.DELNR) THEN DO:
         IF (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.TRAKTANTAL) NE 0 THEN DO:
            CREATE sumpers.
            ASSIGN                
            sumpers.VIBEFATTNING = "Trakt.kostnad " + sumtidtemp.VITRAKT            
            sumpers.ORDNING = ohj
            sumpers.AONR = sumtidtemp.AONR
            sumpers.DELNR = sumtidtemp.DELNR                                  
            sumpers.TIMMAR = 
            (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.TRAKTANTAL)
            sumpers.BELOPP =                
            (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.TBELOPP).
            IF sumpers.TIMMAR > 0 THEN 
            sumpers.OTIMMAR = sumpers.BELOPP / sumpers.TIMMAR. 
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + sumpers.PERSONALKOD.                 
         END.
      END.
   END.        
   ohj = ohj + 1.
   FOR EACH sumtidtemp WHERE sumtidtemp.MED = TRUE  BREAK 
   BY sumtidtemp.LONTILLAGG BY sumtidtemp.AONR BY sumtidtemp.DELNR:         
      ACCUMULATE 
      sumtidtemp.LONKOST (TOTAL BY sumtidtemp.LONTILLAGG BY sumtidtemp.AONR BY sumtidtemp.DELNR).
      ACCUMULATE 
      sumtidtemp.LONTILLANTAL (TOTAL BY sumtidtemp.LONTILLAGG BY sumtidtemp.AONR BY sumtidtemp.DELNR).
      IF LAST-OF(sumtidtemp.DELNR) THEN DO:
         IF (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.LONTILLANTAL) NE 0 THEN DO:
            CREATE sumpers.
            ASSIGN                
            sumpers.VIBEFATTNING = "Lart.kostnad " + sumtidtemp.VILART            
            sumpers.ORDNING = ohj
            sumpers.AONR = sumtidtemp.AONR
            sumpers.DELNR = sumtidtemp.DELNR                                  
            sumpers.TIMMAR = 
            (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.LONTILLANTAL)
            sumpers.BELOPP = 
            (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.LONKOST).
            IF sumpers.TIMMAR > 0 THEN 
            sumpers.OTIMMAR = sumpers.BELOPP / sumpers.TIMMAR.    
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + sumpers.PERSONALKOD.              
         END.
      END.
   END.
   DEFINE VARIABLE ohj2 AS INTEGER NO-UNDO.
   ohj = ohj + 1.
   FOR EACH kosttemp WHERE kosttemp.MED = TRUE  BREAK 
   BY kosttemp.AONR BY kosttemp.DELNR BY kosttemp.BOKKONTO:         
      ACCUMULATE 
      kosttemp.MASKKOST (TOTAL BY kosttemp.AONR BY kosttemp.DELNR BY kosttemp.BOKKONTO).
      ACCUMULATE 
      kosttemp.FRTJPAKR (TOTAL BY kosttemp.AONR BY kosttemp.DELNR BY kosttemp.BOKKONTO).
      ACCUMULATE 
      kosttemp.MTRL (TOTAL BY kosttemp.AONR BY kosttemp.DELNR BY kosttemp.BOKKONTO). 
      ACCUMULATE 
      kosttemp.MTRLPAKR (TOTAL BY kosttemp.AONR BY kosttemp.DELNR BY kosttemp.BOKKONTO).
      ACCUMULATE 
      kosttemp.OVRKR (TOTAL BY kosttemp.AONR BY kosttemp.DELNR BY kosttemp.BOKKONTO).
      ACCUMULATE 
      kosttemp.PERSKOST (TOTAL BY kosttemp.AONR BY kosttemp.DELNR BY kosttemp.BOKKONTO).
      ACCUMULATE 
      kosttemp.TRAKTKOST (TOTAL BY kosttemp.AONR BY kosttemp.DELNR BY kosttemp.BOKKONTO).         
      IF LAST-OF(kosttemp.BOKKONTO) THEN DO:
         ohj2 = ohj2 + 1.
         IF (ACCUM TOTAL BY kosttemp.BOKKONTO kosttemp.PERSKOST) NE 0 THEN DO:
            ohj2 = ohj2 + 1.
            CREATE sumpers.
            ASSIGN                
            sumpers.VIBEFATTNING = "Konterad personalkostnad " + kosttemp.BOKKONTO 
            sumpers.BOKKONTO = kosttemp.BOKKONTO           
            sumpers.ORDNING = ohj
            sumpers.AONR = kosttemp.AONR
            sumpers.DELNR = kosttemp.DELNR                                  
            sumpers.OTIMMAR = 0 
            sumpers.TIMMAR = 1
            sumpers.BELOPP = (ACCUM TOTAL BY kosttemp.BOKKONTO kosttemp.PERSKOST).
            egfaktemp.ARBKOST = egfaktemp.ARBKOST + sumpers.BELOPP.
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + sumpers.PERSONALKOD.
                            
         END.
         IF (ACCUM TOTAL BY kosttemp.BOKKONTO kosttemp.TRAKTKOST) NE 0 THEN DO:
            ohj2 = ohj2 + 1. 
            CREATE sumpers.
            ASSIGN                
            sumpers.VIBEFATTNING = "Konterad trakt.kostnad " + kosttemp.BOKKONTO 
            sumpers.BOKKONTO = kosttemp.BOKKONTO           
            sumpers.ORDNING = ohj + 1
            sumpers.AONR = kosttemp.AONR
            sumpers.DELNR = kosttemp.DELNR                                  
            sumpers.OTIMMAR = 0 
            sumpers.TIMMAR = 1
            sumpers.BELOPP = (ACCUM TOTAL BY kosttemp.BOKKONTO kosttemp.TRAKTKOST).
            egfaktemp.TRAKT = egfaktemp.TRAKT + sumpers.BELOPP.
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + sumpers.PERSONALKOD.                 
         END.
         IF (ACCUM TOTAL BY kosttemp.BOKKONTO kosttemp.MASKKOST) +  
         (ACCUM TOTAL BY kosttemp.BOKKONTO kosttemp.FRTJPAKR) NE 0 THEN DO:
            ohj2 = ohj2 + 1.
            CREATE sumpers.
            ASSIGN                
            sumpers.VIBEFATTNING = "Främandetj " + kosttemp.BOKKONTO
            sumpers.BOKKONTO = kosttemp.BOKKONTO           
            sumpers.ORDNING = ohj + 2
            sumpers.AONR = kosttemp.AONR
            sumpers.DELNR = kosttemp.DELNR                                  
            sumpers.OTIMMAR = 0 
            sumpers.TIMMAR = 1
            sumpers.BELOPP = (ACCUM TOTAL BY kosttemp.BOKKONTO kosttemp.MASKKOST) +  
                             (ACCUM TOTAL BY kosttemp.BOKKONTO kosttemp.FRTJPAKR). 
            egfaktemp.KBELOPP = egfaktemp.KBELOPP + sumpers.BELOPP.                 
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + sumpers.PERSONALKOD.                        
                             
              
         END.
         IF (ACCUM TOTAL BY kosttemp.BOKKONTO kosttemp.MTRL) +  
            (ACCUM TOTAL BY kosttemp.BOKKONTO kosttemp.MTRLPAKR) NE 0 THEN DO:
            ohj2 = ohj2 + 1.   
            CREATE sumpers.
            ASSIGN                
            sumpers.VIBEFATTNING = "Materielkostnad " + kosttemp.BOKKONTO           
            sumpers.BOKKONTO = kosttemp.BOKKONTO
            sumpers.ORDNING = ohj + 3
            sumpers.AONR = kosttemp.AONR
            sumpers.DELNR = kosttemp.DELNR                                  
            sumpers.OTIMMAR = 0 
            sumpers.TIMMAR = 1
            sumpers.BELOPP = (ACCUM TOTAL BY kosttemp.BOKKONTO kosttemp.MTRL) +  
                             (ACCUM TOTAL BY kosttemp.BOKKONTO kosttemp.MTRLPAKR). 
            egfaktemp.MTRL = egfaktemp.MTRL + sumpers.BELOPP.
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + sumpers.PERSONALKOD.
                         
         END.
         IF (ACCUM TOTAL BY kosttemp.BOKKONTO kosttemp.OVRKR) NE 0 THEN DO:
            ohj2 = ohj2 + 1.
            CREATE sumpers.
            ASSIGN                
            sumpers.VIBEFATTNING = "Övrig kostnad " + kosttemp.BOKKONTO 
            sumpers.BOKKONTO = kosttemp.BOKKONTO           
            sumpers.ORDNING = ohj + 4
            sumpers.AONR = kosttemp.AONR
            sumpers.DELNR = kosttemp.DELNR                                  
            sumpers.OTIMMAR = 0 
            sumpers.TIMMAR = 1
            sumpers.BELOPP = (ACCUM TOTAL BY kosttemp.BOKKONTO kosttemp.OVRKR).
            egfaktemp.OVRIG = egfaktemp.OVRIG + sumpers.BELOPP.    
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + sumpers.PERSONALKOD.               
         END.               
      END.
   END.
   ohj = ohj + 1 + ohj2.
   FIND FIRST sumtidtemp WHERE sumtidtemp.MED = TRUE NO-LOCK NO-ERROR.
   IF AVAILABLE sumtidtemp  THEN DO:
      CREATE sumpers.
      ASSIGN                
      sumpers.ORDNING = ohj
      sumpers.AONR = aonrvar
      sumpers.DELNR = delnrvar
      
      sumpers.VIBEFATTNING = "Arbetskostnad:".
      /* 
      sumpers.VIBEFATTNING = "Var av dessa befattningar:".
      */
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + sumpers.PERSONALKOD.
   END.       
   ohj = ohj + 1.
   FOR EACH sumtidtemp WHERE sumtidtemp.MED = TRUE  BREAK 
   BY sumtidtemp.VIBEFATTNING BY sumtidtemp.PRISA BY sumtidtemp.AONR BY sumtidtemp.DELNR:         
      ACCUMULATE 
      sumtidtemp.BELOPP (TOTAL BY sumtidtemp.VIBEFATTNING BY sumtidtemp.PRISA BY sumtidtemp.AONR BY sumtidtemp.DELNR).
      ACCUMULATE 
      sumtidtemp.TIMMAR (TOTAL BY sumtidtemp.VIBEFATTNING BY sumtidtemp.PRISA BY sumtidtemp.AONR BY sumtidtemp.DELNR).
      IF LAST-OF(sumtidtemp.DELNR) THEN DO:
         IF (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.TIMMAR) NE 0 THEN DO:
            ohj = ohj + 1.
            CREATE sumpers.
            ASSIGN                
            sumpers.VIBEFATTNING = sumtidtemp.VIBEFATTNING 
            sumpers.ORDNING = ohj
            sumpers.AONR = sumtidtemp.AONR
            sumpers.DELNR = sumtidtemp.DELNR               
            sumpers.OTIMMAR = sumtidtemp.PRISA 
            sumpers.TIMMAR = 
            (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.TIMMAR)
            sumpers.BELOPP = 
            (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.BELOPP).  
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + sumpers.PERSONALKOD.                             
         END.
      END.
   END.
   ohj = ohj + 1.
   FOR EACH sumtidtemp WHERE sumtidtemp.MED = TRUE  BREAK 
   BY sumtidtemp.OPRIS BY sumtidtemp.AONR BY sumtidtemp.DELNR BY sumtidtemp.BOKKONTO:         
      ACCUMULATE 
      sumtidtemp.OBELOPP (TOTAL BY sumtidtemp.OPRIS BY sumtidtemp.AONR BY sumtidtemp.DELNR BY sumtidtemp.BOKKONTO).
      ACCUMULATE 
      sumtidtemp.OTIMMAR (TOTAL BY sumtidtemp.OPRIS BY sumtidtemp.AONR BY sumtidtemp.DELNR BY sumtidtemp.BOKKONTO).
      IF LAST-OF(sumtidtemp.BOKKONTO) THEN DO:
         IF (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.OTIMMAR) NE 0 THEN DO:
            CREATE sumpers.
            ASSIGN                
            sumpers.VIBEFATTNING = /*"Övertidskostnad " +*/ sumtidtemp.BOKKONTO            
            sumpers.ORDNING = ohj
            sumpers.AONR = sumtidtemp.AONR
            sumpers.DELNR = sumtidtemp.DELNR               
            sumpers.OTIMMAR = sumtidtemp.OPRIS 
            sumpers.TIMMAR = 
            (ACCUM TOTAL BY sumtidtemp.BOKKONTO sumtidtemp.OTIMMAR)
            sumpers.BELOPP = 
            (ACCUM TOTAL BY sumtidtemp.BOKKONTO sumtidtemp.OBELOPP).   
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + sumpers.PERSONALKOD.                           
         END.
      END.
   END.
END PROCEDURE.

PROCEDURE kostreg_UI :  
   IF uppvaltemp.VISPERAR = TRUE  THEN DO: 
      OPEN QUERY kostq FOR EACH KOSTREG WHERE KOSTREG.AONR = aonrvar AND
      KOSTREG.DELNR = delnrvar USE-INDEX KOST NO-LOCK.         
   END.
   ELSE DO:
      OPEN QUERY kostq FOR EACH KOSTREG WHERE KOSTREG.AONR = aonrvar AND
      KOSTREG.DELNR = delnrvar AND KOSTREG.REGDATUM >= uppvaltemp.STARTDATUM AND KOSTREG.REGDATUM <= uppvaltemp.SLUTDATUM USE-INDEX KOST NO-LOCK. 
   END.   
   GET FIRST kostq NO-LOCK.
   DO WHILE AVAILABLE(KOSTREG):
      IF KOSTREG.INKOMST = 0 THEN DO:
         RUN skapkost_UI. 
      END.
      
      GET NEXT kostq NO-LOCK.
   END.                   

END PROCEDURE.   
PROCEDURE skapkost_UI :
   DO TRANSACTION:
      CREATE kosttemp.
      ASSIGN 
      kosttemp.FAKTNR = KOSTREG.FAKTNR
      kosttemp.AONR = KOSTREG.AONR    
      kosttemp.DELNR = KOSTREG.DELNR 
      kosttemp.RADNR = KOSTREG.RADNR
      kosttemp.BENAMNING = KOSTREG.BENAMNING   
      kosttemp.BOKKONTO = KOSTREG.BOKKONTO 
      kosttemp.PERSKOST = ROUND(KOSTREG.PERSKOST,0)
      kosttemp.TRAKTKOST = ROUND(KOSTREG.TRAKTKOST,0)
      kosttemp.MASKKOST = ROUND(KOSTREG.MASKKOST,0)
      kosttemp.MTRL = ROUND(KOSTREG.MTRL,0) 
      kosttemp.OVRKR = ROUND(KOSTREG.OVRKR + -1 * KOSTREG.INKOMST,0)
      kosttemp.MED = TRUE   
      /* 
      kosttemp.FRTJPAKR = ROUND(KOSTREG.MASKKOST * FAKTREGLER.FRTJPA / 100,0)
      kosttemp.MTRLPAKR = ROUND(KOSTREG.MTRL * FAKTREGLER.MTRLPA / 100,0)
      */
      kosttemp.RADNR = KOSTREG.RADNR.
      /* KOSTREG.INKOMST */
   
   END.   
END PROCEDURE.




      
