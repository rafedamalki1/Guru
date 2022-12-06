/*AOKAAPPC.P*/
{KALKYLUPP.I}
{EXTRATAB.I}
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
RUN STYRFORE.P (INPUT Guru.Konstanter:globforetag).
&Scoped-define NEW NEW
{FAKTTYPDEF.I}
&Scoped-define NEW 
{FAKTTYPSKAP.I}
FUNCTION klockan100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):
  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.
END FUNCTION.
FUNCTION klockan60 RETURNS DECIMAL
  ( INPUT ber100 AS DECIMAL ):
  RETURN TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) / 100) * 60 . 
END FUNCTION.
DEFINE QUERY fq FOR FAKTINTAKTKONT,FAKTURERAD.
DEFINE QUERY fkq FOR FAKTINTAKTKONTKRED,FAKTKRED.
DEFINE QUERY kq FOR KOSTREG.
DEFINE QUERY sq FOR SUMTID.
DEFINE QUERY stq FOR SUMTIDDAG.
{DIRDEF.I}
{KALKTEMP.I}
{EXTRADATA.I}
DEFINE TEMP-TABLE kosttemp     
   FIELD AONR LIKE KOSTREG.AONR 
   FIELD DELNR LIKE KOSTREG.DELNR         
   FIELD BOKKONTO LIKE KOSTREG.BOKKONTO   
   FIELD FAKTNR LIKE KOSTREG.FAKTNR    
   FIELD REGDATUM LIKE KOSTREG.REGDATUM    
   FIELD BENAMNING LIKE KOSTREG.BENAMNING
   FIELD BELOPP LIKE KOSTREG.MTRL   
   FIELD MBELOPP LIKE KOSTREG.MTRL   
   FIELD TBELOPP LIKE KOSTREG.MTRL   
   FIELD MTRL LIKE KOSTREG.MTRL   
   FIELD OVRKR LIKE KOSTREG.MTRL 
   FIELD INKOMST LIKE KOSTREG.MTRL   
   INDEX BOK IS PRIMARY BOKKONTO REGDATUM
   INDEX AONR AONR DELNR.
DEFINE TEMP-TABLE kosttemp2
   FIELD ANVANDARE LIKE KOSTREG.ANVANDARE
   FIELD AONR LIKE KOSTREG.AONR 
   FIELD DELNR LIKE KOSTREG.DELNR
   FIELD BENAMNING LIKE KOSTREG.BENAMNING 
   FIELD BETDATUM LIKE KOSTREG.BETDATUM 
   FIELD BOKKONTO LIKE KOSTREG.BOKKONTO    
   FIELD FAKBES LIKE KOSTREG.FAKBES 
   FIELD FAKTNR LIKE KOSTREG.FAKTNR 
  /*FIELD FAKTURERAD LIKE KOSTREG.FAKTURERAD */
   FIELD INKOMST LIKE KOSTREG.INKOMST  
  /* FIELD KOSTAUTO LIKE KOSTREG.KOSTAUTO */
   FIELD LEVKOD LIKE KOSTREG.LEVKOD  
   FIELD MASKKOST LIKE KOSTREG.MASKKOST  
   FIELD MOMS LIKE KOSTREG.MOMS 
   FIELD MTRL LIKE KOSTREG.MTRL 
   FIELD OVRKR LIKE KOSTREG.OVRKR 
   FIELD PERSKOST LIKE KOSTREG.PERSKOST 
   FIELD RADNR LIKE KOSTREG.RADNR 
   FIELD REGDATUM LIKE KOSTREG.REGDATUM 
   FIELD TRAKTKOST LIKE KOSTREG.TRAKTKOST
   INDEX AONR IS PRIMARY AONR DELNR BOKKONTO REGDATUM.

DEFINE TEMP-TABLE slutsum           
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD PRIS LIKE TIDREGITAB.PRIS     
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP 
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR 
   FIELD TIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"      
   FIELD OTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "OTIMMAR"         
   FIELD BELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "ARBKOSTNAD"           
   FIELD OBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "�-KOSTNAD"  
   FIELD MBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "�-KOSTNAD"  
   FIELD OANTAL LIKE EKRAPPRESULT.EANTAL  LABEL "�-ANTAL"         
   FIELD TBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "T-KOSTNAD"
   FIELD TANTAL LIKE EKRAPPRESULT.EANTAL  LABEL "T-ANTAL"       
   FIELD LONKOST LIKE EKRAPPRESULT.EBELOPP LABEL "L-KOSTNAD"    
   FIELD MTRL LIKE KOSTREG.MTRL    
   FIELD OVRKR LIKE KOSTREG.OVRKR    
   FIELD INKOMST LIKE KOSTREG.INKOMST   
   INDEX AONR IS PRIMARY AONR DELNR ASCENDING.    
DEFINE TEMP-TABLE sumsum           
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD PRIS LIKE TIDREGITAB.PRIS     
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP 
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR 
   FIELD TIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"      
   FIELD OTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "OTIMMAR"         
   FIELD BELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "ARBKOSTNAD"           
   FIELD OBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "�-KOSTNAD"  
   FIELD MBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "�-KOSTNAD"  
   FIELD OANTAL LIKE EKRAPPRESULT.EANTAL  LABEL "�-ANTAL"         
   FIELD TBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "T-KOSTNAD"
   FIELD TANTAL LIKE EKRAPPRESULT.EANTAL  LABEL "T-ANTAL"       
   FIELD LONKOST LIKE EKRAPPRESULT.EBELOPP LABEL "L-KOSTNAD"    
   FIELD MTRL LIKE KOSTREG.MTRL    
   FIELD OVRKR LIKE KOSTREG.OVRKR    
   FIELD INKOMST LIKE KOSTREG.INKOMST
   FIELD MBLOPP AS DECIMAL
   FIELD TINKOMST AS DECIMAL
   FIELD RESTIM AS DECIMAL
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD ASCENDING.    
 {BOLAGSEKSTART.I}  
DEFINE INPUT  PARAMETER ganv AS CHARACTER NO-UNDO.

DEFINE INPUT PARAMETER TABLE FOR uppvaltemp.
DEFINE INPUT PARAMETER TABLE FOR valdaao.
DEFINE OUTPUT PARAMETER TABLE FOR tidut.
DEFINE OUTPUT PARAMETER str AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE OUTPUT PARAMETER str2 AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE OUTPUT PARAMETER str3 AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE NEW SHARED VARIABLE fastrec AS RECID NO-UNDO.
DEFINE VARIABLE offert AS LOGICAL NO-UNDO.
DEFINE VARIABLE xtypmtrl AS INTEGER NO-UNDO.
DEFINE VARIABLE kalkvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE utnr AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE ingakostver AS LOGICAL NO-UNDO.
DEFINE VARIABLE stim AS DECIMAL NO-UNDO.   
DEFINE VARIABLE o50 AS DECIMAL NO-UNDO. 
DEFINE VARIABLE o75 AS DECIMAL NO-UNDO. 
DEFINE VARIABLE o100 AS DECIMAL NO-UNDO. 
DEFINE VARIABLE stimk AS DECIMAL NO-UNDO.   
DEFINE VARIABLE o50k AS DECIMAL NO-UNDO. 
DEFINE VARIABLE o75k AS DECIMAL NO-UNDO. 
DEFINE VARIABLE o100k AS DECIMAL NO-UNDO.
DEFINE VARIABLE monpris LIKE EBRPRIS.MONT NO-UNDO.
DEFINE VARIABLE berpris AS DECIMAL NO-UNDO.
DEFINE VARIABLE region AS LOGICAL NO-UNDO.
DEFINE VARIABLE baonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE bdelnr AS INTEGER NO-UNDO.
DEFINE VARIABLE emask3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
/*BEREDNING*/
{LISTDEF.I} 
{BYTAO.I} 
DEFINE NEW SHARED TEMP-TABLE mtrl_temp2   
   {MTRLTEMP2TT.I}
/*SLUT BEREDNING*/


FIND FIRST uppvaltemp NO-ERROR.
OPEN QUERY aq FOR EACH valdaao NO-LOCK. 
GET FIRST aq NO-LOCK.
GET NEXT aq  NO-LOCK.
IF AVAILABLE valdaao THEN ingakostver = TRUE.
ELSE ingakostver = FALSE.
GET FIRST aq NO-LOCK.
DO WHILE AVAILABLE(valdaao):      
   {SUMOPEN.I}              
   RUN kalkyler_UI.
   GET NEXT aq NO-LOCK.
END.      
{DAGTEMPBOLAG.I}
FOR EACH kalksumsum:
   kalksumsum.TIMMAR = kalksumsum.TIMMAR + kalksumsum.BTIMMAR.
   kalksumsum.MBELOPP = kalksumsum.MASKGBELOPP. 
END.
RUN summa_UI.
RUN huvud_UI.
{AOKAPPP.I}

{KALKYLUPPSUMMA.I}

PROCEDURE kalkyler_UI :   
   FIND FIRST AONRTAB WHERE AONRTAB.AONR = valdaao.AONR AND
   AONRTAB.DELNR = valdaao.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.
   /*KALKRUTIN*/
   FIND FIRST KALKAONR WHERE KALKAONR.AONR = valdaao.AONR AND
   KALKAONR.DELNR = valdaao.DELNR AND KALKAONR.STATUSNIV = "UF"
   USE-INDEX AONR NO-LOCK NO-ERROR.
   IF AVAILABLE KALKAONR THEN DO:
      RUN kalkupp_UI (INPUT KALKAONR.KALKNR,INPUT KALKAONR.OMRADE).  
      RUN summeringkalk_UI.   
   END.
   
   
END PROCEDURE.
   

