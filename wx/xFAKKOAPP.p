/*xFAKKOAPP.P GURUKUND TIMMAR GURUHBOK*/
DEFINE TEMP-TABLE strokund
   FIELD FORETAG LIKE AVDELNING.AVDELNINGNR
   FIELD ORDNING AS INTEGER
   FIELD ORDNINGNUM AS INTEGER 
   FIELD TRANSKOD AS CHARACTER  
   FIELD KLIENTNR LIKE AVDELNING.AVDELNINGNR 
   FIELD KUNDNR LIKE BESTTAB.VIBESTID        
   FIELD DELKUND AS INTEGER
   FIELD NAMNKUND LIKE FAKTNAMN.BESTNAMN
   FIELD ADRESS LIKE FAKTNAMN.FAKADRESS         
   FIELD POSTNR LIKE FAKTNAMN.FAKPNR
   FIELD FAKTNR LIKE FAKTSKARP.LOPNR                                 
   FIELD DATUMFAKT AS DATE
   FIELD DATUMFF AS DATE
   FIELD MBELOPP AS DECIMAL 
   FIELD BELOPP AS DECIMAL
   FIELD DEBKRED AS CHARACTER
   FIELD POSTADRESS LIKE FAKTNAMN.FAKORT
   FIELD KUNDTYPFAKT AS CHARACTER
   FIELD BOKDATUM AS DATE                                      
   FIELD KUNDKONTO LIKE KUNDFODRAN.KUNDKONTO
   FIELD REF2 AS CHARACTER                                                      
   FIELD AONR LIKE FAKTKUNDKONTO.AONR
   FIELD DELNR LIKE FAKTKUNDKONTO.DELNR
   INDEX SORT IS PRIMARY FORETAG ORDNING ORDNINGNUM KLIENTNR DELKUND.

DEFINE TEMP-TABLE viskonttemp NO-UNDO  
   FIELD KONTO AS CHARACTER 
   FIELD MOTPART AS CHARACTER
   FIELD OMRADE LIKE OMRADETAB.OMRADE
   FIELD AONR LIKE AONRTAB.AONR
   FIELD DELNR LIKE AONRTAB.DELNR
   FIELD VDELNR AS CHARACTER
   FIELD DEBET AS DECIMAL
   FIELD KREDIT AS DECIMAL
   FIELD ORDNING AS INTEGER
   INDEX ORDNING IS PRIMARY ORDNING AONR DELNR KONTO MOTPART.

DEFINE TEMP-TABLE vismomstemp   NO-UNDO   
   FIELD AONR LIKE AONRTAB.AONR
   FIELD DELNR LIKE AONRTAB.DELNR
   FIELD MOMS AS DECIMAL
   INDEX AONR IS PRIMARY AONR DELNR.


DEFINE TEMP-TABLE huvudtemp   NO-UNDO
   FIELD ORDNING AS INTEGER
   FIELD TRANSKOD AS CHARACTER
   FIELD KUND AS CHARACTER
   INDEX SORT IS PRIMARY ORDNING.

DEFINE TEMP-TABLE sumkont
   FIELD AONR LIKE AONRKONTKOD.AONR 
   FIELD DELNR LIKE AONRKONTKOD.DELNR
   FIELD K1 LIKE AONRKONTKOD.K1 
   FIELD K2 LIKE AONRKONTKOD.K2 
   FIELD K3 LIKE AONRKONTKOD.K3 
   FIELD K4 LIKE AONRKONTKOD.K4 
   FIELD K5 LIKE AONRKONTKOD.K5 
   FIELD BELOPP AS DECIMAL
   INDEX AONR IS PRIMARY AONR DELNR K1 K2.

DEFINE VARIABLE debkred AS LOGICAL NO-UNDO.   
debkred = FALSE.
DEFINE VARIABLE fplanr LIKE FAKTPLAN.FAKTNR NO-UNDO.
fplanr = 30.
DEFINE VARIABLE fpdelnr LIKE FAKTPLAN.FDELNR NO-UNDO.
FIND FIRST FAKTPLAN WHERE FAKTPLAN.faktnr = 30 NO-LOCK NO-ERROR.
fpdelnr = FAKTPLAN.FDELNR.
DEFINE VARIABLE skarpvar LIKE FAKTSKARP.LOPNR NO-UNDO.
FIND FIRST FAKTURERAD WHERE FAKTNR = 30  NO-LOCK NO-ERROR.
skarpvar = FAKTURERAD.VFAKTNR.
DEFINE VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE VARIABLE orevar AS LOGICAL NO-UNDO.
DEFINE VARIABLE varfakturd AS DATE NO-UNDO.
DEFINE VARIABLE varforfalld AS DATE NO-UNDO.
DEFINE VARIABLE strovar AS LOGICAL NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE varbelopp AS DECIMAL NO-UNDO.
DEFINE VARIABLE varmbelopp AS DECIMAL NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(140)" NO-UNDO.
DEFINE VARIABLE str2 AS CHARACTER FORMAT "X(140)" NO-UNDO.
DEFINE VARIABLE progrest AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnold AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnold2 AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnk AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnIBT AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnkIBT AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnIBTI AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnkIBTI AS CHARACTER NO-UNDO.
DEFINE VARIABLE utnr AS INTEGER EXTENT 55 NO-UNDO.
DEFINE VARIABLE bredd AS INTEGER EXTENT 55 NO-UNDO.
DEFINE VARIABLE nrcol AS INTEGER EXTENT 55 NO-UNDO.
DEFINE VARIABLE breddantal AS INTEGER NO-UNDO.
DEFINE VARIABLE breddantalIBT AS INTEGER NO-UNDO.
DEFINE VARIABLE utnrIBT AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE breddIBT AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE nrcolIBT AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE BUFFER viskonttempbuff FOR viskonttemp.
DEFINE BUFFER faktureradbuff FOR FAKTURERAD.
FIND FIRST FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
strovar = FALSE.
FIND FIRST FAKTPLAN WHERE FAKTPLAN.FAKTNR = fplanr NO-LOCK NO-ERROR.
FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = FAKTPLAN.OMRADE NO-LOCK NO-ERROR.
FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR 
NO-LOCK NO-ERROR.
FIND FIRST BESTTAB WHERE BESTTAB.BESTID = FAKTPLAN.BESTID 
USE-INDEX BEST NO-LOCK NO-ERROR.
FIND FIRST FAKTREGLER WHERE FAKTREGLER.FAKTNR = FAKTPLAN.FAKTNR 
USE-INDEX FAKTREGLER NO-LOCK NO-ERROR.
FIND FIRST KUNDTYP WHERE KUNDTYP.KUNDID = FAKTREGLER.KUNDID NO-LOCK NO-ERROR.
/*FAKTFOR*/
IF globforetag = "GRAN"  OR globforetag = "GKAL"
OR globforetag = "ELPA"  THEN DO:
   IF debkred = FALSE THEN DO:         
       FIND FIRST FAKTURERAD WHERE FAKTURERAD.FAKTNR = FAKTPLAN.FAKTNR AND
      FAKTURERAD.FDELNR = fpdelnr NO-LOCK NO-ERROR.  
      RUN grkundres_UI.
      RUN grbokfor_UI.
      /* RUN grintkonto_UI.*/
      RUN faktimgran_UI.
   END.
   ELSE DO:
      FIND FIRST FAKTKRED WHERE FAKTKRED.FAKTNR = FAKTPLAN.FAKTNR AND
      FAKTKRED.FDELNR = fpdelnr NO-LOCK NO-ERROR.  
      RUN grkundreskre_UI.
      RUN grbokforkre_UI.
      RUN faktimkregran_UI.
   END.
   RUN utgr_UI. 
END.
ELSE IF globforetag = "ESAN" THEN DO:  
   RUN huvud_UI.
   IF debkred = FALSE THEN DO:   
      FIND FIRST FAKTURERAD WHERE FAKTURERAD.FAKTNR = FAKTPLAN.FAKTNR AND
      FAKTURERAD.FDELNR = fpdelnr NO-LOCK NO-ERROR.  
      FIND FIRST FAKTNAMN WHERE FAKTNAMN.FAKTURNR = fplanr AND 
      FAKTNAMN.FDELNR = fpdelnr NO-LOCK NO-ERROR.
      IF KUNDTYP.KUNDTYP = "Str�kund" THEN DO:
         strovar = TRUE.
         RUN strokud_UI.
      END.
      RUN eskundres_UI.
      RUN esbokfor_UI.
      RUN faktimesab_UI.
   END.
   ELSE DO:
      FIND FIRST FAKTKRED WHERE FAKTKRED.FAKTNR = FAKTPLAN.FAKTNR AND
      FAKTKRED.FDELNR = fpdelnr NO-LOCK NO-ERROR.  
      FIND FIRST FAKTNAMNKRE WHERE FAKTNAMNKRE.FAKTURNR = fplanr AND 
      FAKTNAMNKRE.FDELNR = fpdelnr NO-LOCK NO-ERROR.
      IF KUNDTYP.KUNDTYP = "Str�kund" THEN DO:
         strovar = TRUE.
         RUN strokud_UI.
      END.
      RUN eskundreskre_UI.
      RUN eskundreskre_UI.
      RUN faktimkreesab_UI.
   END.
   RUN utesab_UI. 
END. 
{GRFAKKONTO.I}
{ESFAKKONTO.I}

PROCEDURE huvud_UI:
   CREATE huvudtemp.
   ASSIGN
   huvudtemp.ORDNING = 0
   huvudtemp.TRANSKOD = "00"
   huvudtemp.KUND = "KUND".
END PROCEDURE.


