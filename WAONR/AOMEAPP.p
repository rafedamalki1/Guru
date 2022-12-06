/*AOMEAPP.P*/
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}

DEFINE TEMP-TABLE intakttemp NO-UNDO
   FIELD AONR         LIKE KOSTREG.AONR
   FIELD DELNR        LIKE KOSTREG.DELNR   
   FIELD BOKKONTO     LIKE KOSTREG.BOKKONTO
   FIELD INTINTAKT    AS DECIMAL
   FIELD EXTERNINTAKT AS DECIMAL
   FIELD RESULTAT     AS DECIMAL
   INDEX AONR IS PRIMARY AONR DELNR BOKKONTO.
DEFINE TEMP-TABLE inkomsttemp NO-UNDO
   FIELD AONR         LIKE KOSTREG.AONR
   FIELD DELNR        LIKE KOSTREG.DELNR   
   FIELD BOKKONTO     LIKE KOSTREG.BOKKONTO
   FIELD INKOMST    AS DECIMAL   
   INDEX AONR IS PRIMARY AONR DELNR BOKKONTO.

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
DEFINE VARIABLE superhandle AS HANDLE.
DEFINE VARIABLE vartvar AS CHARACTER NO-UNDO. 
DEFINE VARIABLE kalktotvar AS DECIMAL NO-UNDO. 
DEFINE VARIABLE intkalktotvar AS DECIMAL NO-UNDO. 
DEFINE VARIABLE exkalktotvar AS DECIMAL NO-UNDO. 
DEFINE VARIABLE summakostvar AS DECIMAL NO-UNDO. 
DEFINE VARIABLE hjalpvar AS DECIMAL NO-UNDO. 
DEFINE QUERY fq FOR FAKTINTAKTKONT,FAKTURERAD.
DEFINE QUERY fkq FOR FAKTINTAKTKONTKRED,FAKTKRED.
DEFINE QUERY kq FOR KOSTREG.
DEFINE QUERY sq FOR SUMTID.
DEFINE QUERY stq FOR SUMTIDDAG.
DEFINE QUERY tq FOR TIDREGITAB.
{DIRDEF.I}
{KALKTEMP.I}
FIND FIRST FORETAG NO-LOCK NO-ERROR.
IF FORETAG.FORETAG = "GRIT" THEN DO:
   RUN GRITIN.P PERSISTENT SET superhandle (INPUT 1).
   THIS-PROCEDURE:ADD-SUPER-PROCEDURE (superhandle).
END.
{TIDUTTT.I}

DEFINE TEMP-TABLE kosttemp     
   FIELD AONR LIKE KOSTREG.AONR 
   FIELD DELNR LIKE KOSTREG.DELNR         
   FIELD BOKKONTO LIKE KOSTREG.BOKKONTO   
   FIELD FAKTNR LIKE KOSTREG.FAKTNR    
   FIELD REGDATUM LIKE KOSTREG.REGDATUM    
   FIELD BENAMNING LIKE KOSTREG.BENAMNING
   FIELD BELOPP LIKE KOSTREG.MTRL   
   FIELD BELOPP1P LIKE KOSTREG.MTRL   
   FIELD BELOPP2P LIKE KOSTREG.MTRL   
   FIELD GRAVARE LIKE KOSTREG.MASKKOST  
   FIELD MASKOVRIG LIKE KOSTREG.MASKKOST     
   FIELD TBELOPP LIKE KOSTREG.MTRL   
   FIELD MTRL LIKE KOSTREG.MTRL   
   FIELD OVRKR LIKE KOSTREG.MTRL 
   FIELD INKOMST LIKE KOSTREG.MTRL   
   INDEX BOK IS PRIMARY BOKKONTO REGDATUM
   INDEX AONR AONR DELNR.

DEFINE TEMP-TABLE kalksumsum   
   FIELD TYP AS INTEGER
   FIELD MASKGBELOPP AS DECIMAL
   FIELD MONTBELOPP  AS DECIMAL
   FIELD MONTTIMMAR  AS DECIMAL
   FIELD BERBELOPP   AS DECIMAL
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
   FIELD TIMMAR LIKE KOSTREG.MTRL 
   FIELD BTIMMAR LIKE KOSTREG.MTRL
   FIELD OTIMMAR LIKE KOSTREG.MTRL
   FIELD OBELOPP LIKE KOSTREG.MTRL
   FIELD LONKOST LIKE KOSTREG.MTRL
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
   FIELD GRAVARE LIKE KOSTREG.MASKKOST  
   FIELD MASKOVRIG LIKE KOSTREG.MASKKOST  
   FIELD MOMS LIKE KOSTREG.MOMS 
   FIELD MTRL LIKE KOSTREG.MTRL 
   FIELD OVRKR LIKE KOSTREG.OVRKR 
   FIELD PERSKOST LIKE KOSTREG.PERSKOST 
   FIELD PERSKOST2 LIKE KOSTREG.PERSKOST 
   FIELD RADNR LIKE KOSTREG.RADNR 
   FIELD REGDATUM LIKE KOSTREG.REGDATUM 
   FIELD TRAKTKOST LIKE KOSTREG.TRAKTKOST
   INDEX AONR IS PRIMARY AONR DELNR BOKKONTO REGDATUM.

DEFINE TEMP-TABLE dagtemp
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR 
   FIELD ORT LIKE AONRTAB.ORT
   FIELD PERSMASK LIKE SUMTIDDAG.PERSMASK
   FIELD BELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "ARBKOSTNAD"           
   FIELD TIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"         
   FIELD NTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"         
   FIELD OATIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "OTIMMAR"
   FIELD ABELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "ARBKOSTNAD"           
   FIELD BTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "OTIMMAR" 
   FIELD NBTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"
   FIELD OBTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "OTIMMAR"
   FIELD BBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "ARBKOSTNAD"           
   FIELD OTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "OTIMMAR"  
   FIELD NOTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"
   FIELD OOTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "OTIMMAR"
   FIELD OBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "ARBKOSTNAD"           
   FIELD BEFATTNING AS CHARACTER           
   FIELD LONKOST AS DECIMAL
   FIELD OMRADE AS CHARACTER
   FIELD GEOMRADE AS CHARACTER
   INDEX AONR IS PRIMARY AONR DELNR. 
/*
DEFINE TEMP-TABLE restid                  
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR 
   FIELD TIMMAR AS DECIMAL LABEL "RTIMMAR"                
   INDEX AONR IS PRIMARY AONR DELNR.  
   */
DEFINE TEMP-TABLE slutsum           
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD PRIS LIKE TIDREGITAB.PRIS     
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP 
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR 
   FIELD ORT LIKE AONRTAB.ORT
   FIELD TIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"      
   FIELD NTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"      
   FIELD OATIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"      
   FIELD OTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "OTIMMAR"
   FIELD NOTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"      
   FIELD OOTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"
   FIELD BTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "OTIMMAR"
   FIELD NBTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"      
   FIELD OBTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"
   FIELD BELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "ARBKOSTNAD"           
   FIELD ABELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "ARBKOSTNAD"           
   FIELD BBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "ARBKOSTNAD"           
   FIELD OBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "Ö-KOSTNAD"  
   FIELD MBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "Ö-KOSTNAD"  
   FIELD IBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "Ö-KOSTNAD"  
   FIELD FBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "Ö-KOSTNAD"  
   FIELD OANTAL LIKE EKRAPPRESULT.EANTAL  LABEL "Ö-ANTAL"         
   FIELD TBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "T-KOSTNAD"
   FIELD TANTAL LIKE EKRAPPRESULT.EANTAL  LABEL "T-ANTAL"       
   FIELD LONKOST LIKE EKRAPPRESULT.EBELOPP LABEL "L-KOSTNAD"    
   FIELD MTRL LIKE KOSTREG.MTRL    
   FIELD OVRKR LIKE KOSTREG.OVRKR    
   FIELD INKOMST LIKE KOSTREG.INKOMST   
   FIELD NY AS LOGICAL INITIAL FALSE  
   INDEX AONR IS PRIMARY AONR DELNR ASCENDING.    
DEFINE TEMP-TABLE sumsum           
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD MASKGBELOPP AS DECIMAL
   FIELD MONTTIMMAR  AS DECIMAL
   FIELD PRIS LIKE TIDREGITAB.PRIS     
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP 
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR 
   FIELD TIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"      
   FIELD NTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"      
   FIELD OATIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"   
   FIELD ABELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "ARBKOSTNAD"
   FIELD NBTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"      
   FIELD OBTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"   
   FIELD BBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "ARBKOSTNAD"
   FIELD OTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "OTIMMAR"   
   FIELD NOTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"      
   FIELD OOTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"   
   FIELD OBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "ARBKOSTNAD"
   FIELD MBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "ARBKOSTNAD"   
   FIELD BELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "ARBKOSTNAD"
   FIELD SKOSTNAD LIKE EKRAPPRESULT.EBELOPP LABEL "SUMMA KOSTNADER"
   FIELD BTIMMAR LIKE EKRAPPRESULT.EBELOPP LABEL "Ö-KOSTNAD"
   FIELD GRAVARE LIKE KOSTREG.MASKKOST  
   FIELD MASKOVRIG LIKE KOSTREG.MASKKOST
   FIELD FORDON LIKE KOSTREG.MASKKOST  
   FIELD OANTAL LIKE EKRAPPRESULT.EANTAL  LABEL "Ö-ANTAL"         
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
/*DEFINE OUTPUT PARAMETER str2 AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE OUTPUT PARAMETER str3 AS CHARACTER FORMAT "X(92)" NO-UNDO.*/
DEFINE NEW SHARED VARIABLE fastrec AS RECID NO-UNDO.
DEFINE VARIABLE offert AS LOGICAL NO-UNDO.
DEFINE VARIABLE xtypmtrl AS INTEGER NO-UNDO.
DEFINE VARIABLE monpris LIKE EBRPRIS.MONT NO-UNDO.
DEFINE VARIABLE berpris AS DECIMAL NO-UNDO.
DEFINE VARIABLE kalkvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE breddantal AS INTEGER NO-UNDO.
DEFINE VARIABLE utnr AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE bredd AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE nrcol AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE ingakostver AS LOGICAL NO-UNDO.
DEFINE VARIABLE stim AS DECIMAL NO-UNDO.   
DEFINE VARIABLE o50 AS DECIMAL NO-UNDO. 
DEFINE VARIABLE o75 AS DECIMAL NO-UNDO. 
DEFINE VARIABLE o100 AS DECIMAL NO-UNDO. 
DEFINE VARIABLE stimk AS DECIMAL NO-UNDO.   
DEFINE VARIABLE o50k AS DECIMAL NO-UNDO. 
DEFINE VARIABLE o75k AS DECIMAL NO-UNDO. 
DEFINE VARIABLE o100k AS DECIMAL NO-UNDO.
DEFINE VARIABLE varin AS DECIMAL NO-UNDO.
DEFINE VARIABLE varkalk AS DECIMAL NO-UNDO.
DEFINE VARIABLE vartot AS DECIMAL NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(256)" NO-UNDO.

/*BEREDNING*/
{LISTDEF.I} 
DEFINE NEW SHARED TEMP-TABLE mtrl_temp2   
   {MTRLTEMP2TT.I}
/*SLUT BEREDNING*/
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR. 
Guru.Konstanter:globforetag = FORETAG.FORETAG.

FIND FIRST uppvaltemp NO-ERROR.
OPEN QUERY aq FOR EACH valdaao NO-LOCK. 
GET FIRST aq NO-LOCK.
GET NEXT aq  NO-LOCK.
IF AVAILABLE valdaao THEN ingakostver = TRUE.
ELSE ingakostver = FALSE.
IF uppvaltemp.DELNRKOLL = FALSE THEN ingakostver = TRUE.
GET FIRST aq NO-LOCK.
DO WHILE AVAILABLE(valdaao):      
   {SUMOPEN.I}
   /*RUN kalkyler_UI.*/
   GET NEXT aq NO-LOCK.
END.   
{DAGTEMPBOLAG.I}   
RUN summa_UI.
RUN huvud_UI.

{GDPRLOGGCLIENT.I}
PROCEDURE huvud_UI :  
   CREATE tidut. 
   SUBSTRING(tidut.UT,60) = STRING(TODAY) + " " + STRING(TIME,"HH:MM").
   CREATE tidut.
   CREATE tidut.
   tidut.UT = uppvaltemp.VALDLISTA. 
   IF uppvaltemp.VISPERAR = TRUE THEN DO: 
      SUBSTRING(tidut.UT,64) = "ÅR " + STRING(YEAR(uppvaltemp.STARTDATUM),"9999").
   END.                        
   ELSE IF uppvaltemp.VISPERAR = FALSE THEN DO:
      SUBSTRING(tidut.UT,64) = "PERIOD " +  STRING(uppvaltemp.STARTDATUM) + 
      " - " + STRING(uppvaltemp.SLUTDATUM).     
   END.
   ELSE DO:
      SUBSTRING(tidut.UT,64) = "VISNING AV ALLT".
   END.
   CREATE tidut.
   {KUURV.I}
   /*CREATE tidut.
   ASSIGN 
   tidut.UT = "UF=KOSTNADER FRÅN TIDREDOVISNING OCH KOSTNADSREGISTRERING, KA=KALKYLERING".      */
   ASSIGN
   nrcol[1] = 1
   nrcol[2] = 2
   nrcol[3] = 27 /*ANVÄNDS EJ*/ 
   nrcol[4] = 9
   nrcol[5] = 12
   nrcol[6] = 15
   nrcol[7] = 11
   nrcol[8] = 19
   nrcol[9] = 20
   nrcol[10] = 21
   nrcol[11] = 22
   nrcol[12] = 25
   nrcol[13] = 28 /*ANVÄNDS EJ*/
   nrcol[14] = 28 /*ANVÄNDS EJ*/
   nrcol[15] = 24    /*ny kolumn*/
   nrcol[16] = 3
   nrcol[17] = 4
   nrcol[18] = 5
   nrcol[19] = 6
   nrcol[20] = 7
   nrcol[21] = 8
   nrcol[22] = 10
   nrcol[23] = 13
   nrcol[24] = 14
   nrcol[25] = 16
   nrcol[26] = 17
   nrcol[27] = 23           
   nrcol[28] = 18      
   breddantal = 28   /*antal kolumner*/
   bredd[1] = 9
   bredd[2] = 40
   bredd[3] = 6
   bredd[4] = 6
   bredd[5] = 6
   bredd[6] = 6
   bredd[7] = 7
   bredd[8] = 7
   bredd[9] = 8
   bredd[10] = 8
   bredd[11] = 7
   bredd[12] = 8
   bredd[13] = 8
   bredd[14] = 8
   bredd[15] = 8
   bredd[16] = 8
   bredd[17] = 8
   bredd[18] = 8
   bredd[19] = 8
   bredd[20] = 8
   bredd[21] = 8
   bredd[22] = 8
   bredd[23] = 8
   bredd[24] = 8
   bredd[25] = 8
   bredd[26] = 8
   bredd[27] = 8
   bredd[28] = 8.

   ASSIGN
   i = 2.     
   utnr[nrcol[1]] = 1.
   DO WHILE i <= breddantal:             
      utnr[i] = utnr[i - 1] + bredd[i - 1] + 1.            
      i = i + 1.
   END.   
   ASSIGN
   str = "".  
   i = 1.
   DO WHILE i <= utnr[breddantal] + bredd[breddantal] - 1:
      str = str + "=".     
      i = i + 1.
   END.   
   i = 2.      
   DO WHILE i <= breddantal:             
      SUBSTRING(str,(utnr[i] - 1),1) = ".".      
      i = i + 1.
   END.          
   CREATE tidut.  
   CREATE tidut.   
   CREATE tidut.             
   FIND FIRST KBENAMNING NO-LOCK NO-ERROR.
   ASSIGN                                                                                                       
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "MONTÖR"             
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "BERED."         
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "ÖVRIGA"  
   /*SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "ARBETS"                                          */
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "Främmande tj."
   SUBSTRING(tidut.UT,utnr[nrcol[28]]) = "Löne-"   
   SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "Materiel"
   SUBSTRING(tidut.UT,utnr[nrcol[11]]) = "Övrig"
   SUBSTRING(tidut.UT,utnr[nrcol[15]]) = "Summa".
   IF Guru.Konstanter:globforetag = "GKAL" THEN SUBSTRING(tidut.UT,utnr[nrcol[27]]) = "Skylift".
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN SUBSTRING(tidut.UT,utnr[nrcol[27]]) = "Fordon".
   IF Guru.Konstanter:varforetypval[10] = 1 THEN DO:
      SUBSTRING(tidut.UT,utnr[nrcol[14]]) = "      ".
   END.
 /*  str2 = tidut.UT.                 */
   IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
      ASSIGN                                                                                                       
      SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "UTRUST"             
      SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "UTLÄGG"     
      SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "INLEJDA  TJ."
      SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "TELEFON".      
   END.
   CREATE tidut.      
   ASSIGN                               
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = CAPS(Guru.Konstanter:gaok)      
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = CAPS(Guru.Konstanter:gaonamnk)
   SUBSTRING(tidut.UT,utnr[nrcol[16]]) = KBENAMNING.K1             
   SUBSTRING(tidut.UT,utnr[nrcol[17]]) = KBENAMNING.K2         
   SUBSTRING(tidut.UT,utnr[nrcol[18]]) = KBENAMNING.K3
   SUBSTRING(tidut.UT,utnr[nrcol[19]]) = KBENAMNING.K4                                          
   SUBSTRING(tidut.UT,utnr[nrcol[20]]) = KBENAMNING.K5
   SUBSTRING(tidut.UT,utnr[nrcol[21]]) = "%"
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "Normaltid"     
   SUBSTRING(tidut.UT,utnr[nrcol[22]]) = "Övertid"             
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "Kostnad"             
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "Normaltid"         
   SUBSTRING(tidut.UT,utnr[nrcol[23]]) = "Övertid"             
   SUBSTRING(tidut.UT,utnr[nrcol[24]]) = "Kostnad"             
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "Normaltid"  
   SUBSTRING(tidut.UT,utnr[nrcol[25]]) = "Övertid"             
   SUBSTRING(tidut.UT,utnr[nrcol[26]]) = "Kostnad"   
   SUBSTRING(tidut.UT,utnr[nrcol[28]]) = "tillägg"   
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "Grävare"  
   SUBSTRING(tidut.UT,utnr[nrcol[9]]) = "Övriga" 
   SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "Kostnad"
   SUBSTRING(tidut.UT,utnr[nrcol[11]]) = "Kostnad"
   SUBSTRING(tidut.UT,utnr[nrcol[27]]) = "Kostnad"
   SUBSTRING(tidut.UT,utnr[nrcol[15]]) = "Kostnad"
   SUBSTRING(tidut.UT,utnr[nrcol[12]]) = "Intäkt"
   SUBSTRING(tidut.UT,300) = CAPS(Guru.Konstanter:garbal)
   SUBSTRING(tidut.UT,320) = CAPS(Guru.Konstanter:gberel)
   SUBSTRING(tidut.UT,340) = CAPS(Guru.Konstanter:gprojl).
   
   IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "KOSTN."             
      SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "KOSTN."
      SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "KONSULT"                                            
      SUBSTRING(tidut.UT,utnr[nrcol[9]]) = "ÖVRIGT". 
   END.
   /*str3 = tidut.UT.                */
   CREATE tidut.      
   tidut.UT = str.
   CREATE sumsum.
   ASSIGN sumsum.PERSONALKOD = "UF".
   /*CREATE sumsum.
   ASSIGN sumsum.PERSONALKOD = "KA".   */
   FOR EACH valdaao: 
      FIND FIRST slutsum WHERE slutsum.AONR = valdaao.AONR AND
      slutsum.DELNR = valdaao.DELNR NO-ERROR.
      IF NOT AVAILABLE slutsum THEN DO:
         FIND FIRST kosttemp WHERE kosttemp.AONR = valdaao.AONR AND
         kosttemp.DELNR = valdaao.DELNR USE-INDEX AONR NO-ERROR.
         FIND FIRST kalksumsum WHERE kalksumsum.AONR = valdaao.AONR AND
         kalksumsum.DELNR = kalksumsum.DELNR USE-INDEX AONR NO-ERROR.
         IF AVAILABLE kosttemp THEN musz = musz.                  
         ELSE IF AVAILABLE kalksumsum THEN musz = musz.
         ELSE NEXT.         
         CREATE slutsum.
         ASSIGN
         slutsum.AONR  = valdaao.AONR 
         slutsum.DELNR = valdaao.DELNR
         slutsum.ORT   = valdaao.ORT.           
      END.   
   END.
   FOR EACH slutsum USE-INDEX AONR:      
      ASSIGN
      varin = 0
      varkalk = 0.
      FIND FIRST valdaao WHERE valdaao.AONR = slutsum.AONR AND valdaao.DELNR = slutsum.DELNR NO-ERROR.
      /*FIND FIRST kalksumsum WHERE kalksumsum.AONR = slutsum.AONR AND
      kalksumsum.DELNR = slutsum.DELNR USE-INDEX AONR NO-ERROR.
      IF AVAILABLE kalksumsum THEN DO:
         varkalk = kalksumsum.BELOPP + kalksumsum.MBELOPP + 
         kalksumsum.MTRL + kalksumsum.OVRKR +  
         kalksumsum.MASKGBELOPP + kalksumsum.MONTBELOPP + kalksumsum.BERBELOPP.         
      END.*/
      FIND FIRST kosttemp WHERE kosttemp.AONR = slutsum.AONR AND
      kosttemp.DELNR = slutsum.DELNR USE-INDEX AONR NO-ERROR.
      IF AVAILABLE kosttemp THEN DO:
         varin = kosttemp.INKOMST.
      END.
      vartot = vartot + (varin - varkalk).      
      IF Guru.Konstanter:globforetag = "misv" THEN DO:
         CREATE tidut.                                   
         ASSIGN 
         SUBSTRING(tidut.UT,utnr[nrcol[1]]) = slutsum.AONR + STRING(slutsum.DELNR,Guru.Konstanter:varforetypchar[1])
         SUBSTRING(tidut.UT,utnr[nrcol[2]]) = SUBSTRING(slutsum.ORT,1,bredd[2])              
         /*SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "UF"        */
         /*SUBSTRING(tidut.UT,utnr[nrcol[16]]) = AONRKONTKOD.K1
         SUBSTRING(tidut.UT,utnr[nrcol[17]]) = AONRKONTKOD.K2
         SUBSTRING(tidut.UT,utnr[nrcol[18]]) = AONRKONTKOD.K3
         SUBSTRING(tidut.UT,utnr[nrcol[19]]) = AONRKONTKOD.K4
         SUBSTRING(tidut.UT,utnr[nrcol[20]]) = AONRKONTKOD.K5
         SUBSTRING(tidut.UT,utnr[nrcol[21]]) = STRING(AONRKONTKOD.SATS%,">>9") */
         SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(slutsum.NTIMMAR,">>>>>9.99")
         SUBSTRING(tidut.UT,utnr[nrcol[22]]) = STRING(slutsum.OATIMMAR,">>>>>9.99")                                                          
         SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(slutsum.ABELOPP,">>>>>>>9")                                                          
         SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(slutsum.NBTIMMAR,">>>>>9.99")
         SUBSTRING(tidut.UT,utnr[nrcol[23]]) = STRING(slutsum.OBTIMMAR,">>>>>9.99")                                                          
         SUBSTRING(tidut.UT,utnr[nrcol[24]]) = STRING(slutsum.BBELOPP,">>>>>>>9")                                                          
         SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING( slutsum.NOTIMMAR,">>>>>9.99")  
         SUBSTRING(tidut.UT,utnr[nrcol[25]]) = STRING(slutsum.OOTIMMAR,">>>>>9.99")                                                          
         SUBSTRING(tidut.UT,utnr[nrcol[26]]) = STRING(slutsum.OBELOPP,">>>>>>>9")                                                                               
         SUBSTRING(tidut.UT,utnr[nrcol[28]]) = STRING(slutsum.LONKOST,">>>>>>>9") 
         SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(slutsum.BELOPP,">>>>>>>9")
         SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(0)
         SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(0)
         SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(0)                        
         SUBSTRING(tidut.UT,utnr[nrcol[27]]) = STRING(slutsum.FBELOPP,">>>>>>9")
         SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING((slutsum.BELOPP + slutsum.FBELOPP),">>>>>>>9").
         IF AVAILABLE valdaao THEN DO:
            RUN pnamn_UI.                       
         END. 
         
         FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "UF" NO-ERROR.                   
         ASSIGN  
         sumsum.TIMMAR =  sumsum.TIMMAR  + ( slutsum.TIMMAR )  
         sumsum.NTIMMAR =  sumsum.NTIMMAR  + ( slutsum.NTIMMAR)  
         sumsum.OATIMMAR =  sumsum.OATIMMAR  + ( slutsum.OATIMMAR)  
         sumsum.ABELOPP =  sumsum.ABELOPP + ( slutsum.ABELOPP)  
         sumsum.BTIMMAR = sumsum.BTIMMAR + ( slutsum.BTIMMAR)  
         sumsum.NBTIMMAR =  sumsum.NBTIMMAR  + ( slutsum.NBTIMMAR)  
         sumsum.OBTIMMAR =  sumsum.OBTIMMAR  + ( slutsum.OBTIMMAR)  
         sumsum.BBELOPP =  sumsum.BBELOPP + ( slutsum.BBELOPP)  
         sumsum.OTIMMAR = sumsum.OTIMMAR + ( slutsum.OTIMMAR)  
         sumsum.NOTIMMAR =  sumsum.NOTIMMAR  + ( slutsum.NOTIMMAR)  
         sumsum.OOTIMMAR =  sumsum.OOTIMMAR  + ( slutsum.OOTIMMAR)  
         sumsum.OBELOPP =  sumsum.OBELOPP + ( slutsum.OBELOPP)  
         sumsum.BELOPP =  sumsum.BELOPP + ( slutsum.BELOPP)  
         sumsum.SKOSTNAD = sumsum.SKOSTNAD + ( slutsum.BELOPP)  
         sumsum.TINKOMST = sumsum.TINKOMST + (0 - ( slutsum.BELOPP))
         sumsum.LONKOST = sumsum.LONKOST + ( slutsum.LONKOST)                             
         sumsum.FORDON =  sumsum.FORDON + ( slutsum.FBELOPP).
      END.
      ELSE DO:      
         FOR EACH AONRKONTKOD WHERE AONRKONTKOD.AONR = slutsum.AONR AND AONRKONTKOD.DELNR = slutsum.DELNR NO-LOCK:
            CREATE tidut.                       
            IF NOT AVAILABLE kosttemp THEN DO:                        
               ASSIGN 
               SUBSTRING(tidut.UT,utnr[nrcol[1]]) = slutsum.AONR + STRING(slutsum.DELNR,Guru.Konstanter:varforetypchar[1])
               SUBSTRING(tidut.UT,utnr[nrcol[2]]) = SUBSTRING(slutsum.ORT,1,bredd[2])              
               /*SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "UF"        */
               SUBSTRING(tidut.UT,utnr[nrcol[16]]) = AONRKONTKOD.K1
               SUBSTRING(tidut.UT,utnr[nrcol[17]]) = AONRKONTKOD.K2
               SUBSTRING(tidut.UT,utnr[nrcol[18]]) = AONRKONTKOD.K3
               SUBSTRING(tidut.UT,utnr[nrcol[19]]) = AONRKONTKOD.K4
               SUBSTRING(tidut.UT,utnr[nrcol[20]]) = AONRKONTKOD.K5
               SUBSTRING(tidut.UT,utnr[nrcol[21]]) = STRING(AONRKONTKOD.SATS%,">>9")
               SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.NTIMMAR,">>>>>9.99")
               SUBSTRING(tidut.UT,utnr[nrcol[22]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.OATIMMAR,">>>>>9.99")                                                          
               SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.ABELOPP,">>>>>>>9")                                                          
               SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.NBTIMMAR,">>>>>9.99")
               SUBSTRING(tidut.UT,utnr[nrcol[23]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.OBTIMMAR,">>>>>9.99")                                                          
               SUBSTRING(tidut.UT,utnr[nrcol[24]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.BBELOPP,">>>>>>>9")                                                          
               SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.NOTIMMAR,">>>>>9.99")  
               SUBSTRING(tidut.UT,utnr[nrcol[25]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.OOTIMMAR,">>>>>9.99")                                                          
               SUBSTRING(tidut.UT,utnr[nrcol[26]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.OBELOPP,">>>>>>>9")                                                                               
               SUBSTRING(tidut.UT,utnr[nrcol[28]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.LONKOST,">>>>>>>9") 
               SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.BELOPP,">>>>>>>9")
               SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(0)
               SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(0)
               SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(0).
               IF AVAILABLE valdaao THEN DO:
                  RUN pnamn_UI.                       
               END. 
               /*SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(AONRKONTKOD.SATS% * (0 - slutsum.BELOPP),"->>>>>>9").*/
               IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:            
                  ASSIGN
                  SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.MBELOPP,">>>>>>9")
                  SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.IBELOPP,">>>>>>9")
                  SUBSTRING(tidut.UT,utnr[nrcol[27]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.FBELOPP,">>>>>>9")
                  SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(AONRKONTKOD.SATS% / 100 * (slutsum.BELOPP + slutsum.MBELOPP + slutsum.IBELOPP + slutsum.FBELOPP + slutsum.LONKOST),">>>>>>>9").               
                  /*SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(0 - AONRKONTKOD.SATS% / 100 * (slutsum.BELOPP + slutsum.MBELOPP + slutsum.IBELOPP),"->>>>>>9").*/
               END.
               IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:            
                  ASSIGN               
                  SUBSTRING(tidut.UT,utnr[nrcol[27]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.FBELOPP,">>>>>>9")
                  SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(AONRKONTKOD.SATS% / 100 * (slutsum.BELOPP + slutsum.FBELOPP),">>>>>>>9").
                  /*SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(0 - AONRKONTKOD.SATS% / 100 * (slutsum.BELOPP + slutsum.MBELOPP + slutsum.IBELOPP),"->>>>>>9").*/
               END.
               
               FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "UF" NO-ERROR.                   
               ASSIGN  
               sumsum.TIMMAR =  sumsum.TIMMAR  + ( slutsum.TIMMAR * AONRKONTKOD.SATS% / 100)  
               sumsum.NTIMMAR =  sumsum.NTIMMAR  + ( slutsum.NTIMMAR  * AONRKONTKOD.SATS% / 100)  
               sumsum.OATIMMAR =  sumsum.OATIMMAR  + ( slutsum.OATIMMAR  * AONRKONTKOD.SATS% / 100)  
               sumsum.ABELOPP =  sumsum.ABELOPP + ( slutsum.ABELOPP  * AONRKONTKOD.SATS% / 100)  
               sumsum.BTIMMAR = sumsum.BTIMMAR + ( slutsum.BTIMMAR * AONRKONTKOD.SATS% / 100)  
               sumsum.NBTIMMAR =  sumsum.NBTIMMAR  + ( slutsum.NBTIMMAR  * AONRKONTKOD.SATS% / 100)  
               sumsum.OBTIMMAR =  sumsum.OBTIMMAR  + ( slutsum.OBTIMMAR  * AONRKONTKOD.SATS% / 100)  
               sumsum.BBELOPP =  sumsum.BBELOPP + ( slutsum.BBELOPP  * AONRKONTKOD.SATS% / 100)  
               sumsum.OTIMMAR = sumsum.OTIMMAR + ( slutsum.OTIMMAR * AONRKONTKOD.SATS% / 100)  
               sumsum.NOTIMMAR =  sumsum.NOTIMMAR  + ( slutsum.NOTIMMAR  * AONRKONTKOD.SATS% / 100)  
               sumsum.OOTIMMAR =  sumsum.OOTIMMAR  + ( slutsum.OOTIMMAR  * AONRKONTKOD.SATS% / 100)  
               sumsum.OBELOPP =  sumsum.OBELOPP + ( slutsum.OBELOPP  * AONRKONTKOD.SATS% / 100)  
               sumsum.BELOPP =  sumsum.BELOPP + ( slutsum.BELOPP  * AONRKONTKOD.SATS% / 100)  
               sumsum.SKOSTNAD = sumsum.SKOSTNAD + ( slutsum.BELOPP * AONRKONTKOD.SATS% / 100)  
               sumsum.TINKOMST = sumsum.TINKOMST + (0 - ( slutsum.BELOPP * AONRKONTKOD.SATS% / 100))
               sumsum.LONKOST = sumsum.LONKOST + ( slutsum.LONKOST * AONRKONTKOD.SATS% / 100)     .
               IF Guru.Konstanter:globforetag = "gkal" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
                  ASSIGN
                  sumsum.GRAVARE =  sumsum.GRAVARE + ( slutsum.MBELOPP  * AONRKONTKOD.SATS% / 100)  
                  sumsum.MASKOVRIG =  sumsum.MASKOVRIG + ( slutsum.IBELOPP * AONRKONTKOD.SATS% / 100)  
                  sumsum.FORDON =  sumsum.FORDON + ( slutsum.FBELOPP * AONRKONTKOD.SATS% / 100).            
                  /*IF sumsum.LONKOST > sumsum.FORDON THEN DO:               
                     sumsum.LONKOST = sumsum.LONKOST - sumsum.FORDON.
                  END.
                  ELSE sumsum.LONKOST = 0. */
               END. 
               IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
                  ASSIGN               
                  sumsum.FORDON =  sumsum.FORDON + ( slutsum.FBELOPP * AONRKONTKOD.SATS% / 100).            
               END.
            END.
            ELSE DO:                        
               ASSIGN
               SUBSTRING(tidut.UT,utnr[nrcol[1]]) = slutsum.AONR + STRING(slutsum.DELNR,Guru.Konstanter:varforetypchar[1])
               SUBSTRING(tidut.UT,utnr[nrcol[2]]) = SUBSTRING(slutsum.ORT,1,bredd[2])      
               /*SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "UF"*/
               SUBSTRING(tidut.UT,utnr[nrcol[16]]) = AONRKONTKOD.K1
               SUBSTRING(tidut.UT,utnr[nrcol[17]]) = AONRKONTKOD.K2
               SUBSTRING(tidut.UT,utnr[nrcol[18]]) = AONRKONTKOD.K3
               SUBSTRING(tidut.UT,utnr[nrcol[19]]) = AONRKONTKOD.K4
               SUBSTRING(tidut.UT,utnr[nrcol[20]]) = AONRKONTKOD.K5
               SUBSTRING(tidut.UT,utnr[nrcol[21]]) = STRING(AONRKONTKOD.SATS%,">>9")
               SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.NOTIMMAR,">>>>>9.99")
               SUBSTRING(tidut.UT,utnr[nrcol[25]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.OOTIMMAR,">>>>>9.99").
               /*SUBSTRING(tidut.UT,150) = slutsum.ORT.  */
               IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
                  ASSIGN
                  SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(AONRKONTKOD.SATS% / 100 * kosttemp.BELOPP1P,">>>>>9")                                                          
                  SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(AONRKONTKOD.SATS% / 100 * kosttemp.BELOPP2P,">>>>>9").                        
               END.
               ELSE DO:            
                  ASSIGN
                  SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.NTIMMAR,">>>>>9.99")                                                          
                  SUBSTRING(tidut.UT,utnr[nrcol[22]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.OATIMMAR,">>>>>9.99")                                                                         
                  SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.NBTIMMAR,">>>>>9.99")
                  SUBSTRING(tidut.UT,utnr[nrcol[23]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.OBTIMMAR,">>>>>9.99").            
               END.
               IF kosttemp.BELOPP < 0 
               THEN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(AONRKONTKOD.SATS% / 100 * (slutsum.ABELOPP + kosttemp.BELOPP),"->>>>>>9").
               ELSE SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(AONRKONTKOD.SATS% / 100 * (slutsum.ABELOPP + kosttemp.BELOPP),">>>>>>>9").
               SUBSTRING(tidut.UT,utnr[nrcol[24]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.BBELOPP,">>>>>>>9").
               SUBSTRING(tidut.UT,utnr[nrcol[26]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.OBELOPP,">>>>>>>9").
               SUBSTRING(tidut.UT,utnr[nrcol[28]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.LONKOST,">>>>>>>9").
   
               IF kosttemp.GRAVARE < 0 
               THEN SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(AONRKONTKOD.SATS% / 100 * kosttemp.GRAVARE,"->>>>>9").
               ELSE SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(AONRKONTKOD.SATS% / 100 * kosttemp.GRAVARE,">>>>>>9").
               IF kosttemp.MASKOVRIG < 0 
               THEN SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(AONRKONTKOD.SATS% / 100 * kosttemp.MASKOVRIG,"->>>>>9").
               ELSE SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(AONRKONTKOD.SATS% / 100 * kosttemp.MASKOVRIG,">>>>>>9").
               IF kosttemp.MTRL < 0 
               THEN SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(AONRKONTKOD.SATS% / 100 * kosttemp.MTRL,"->>>>>>9").
               ELSE SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(AONRKONTKOD.SATS% / 100 * kosttemp.MTRL,">>>>>>>9").            
               summakostvar = slutsum.BELOPP + kosttemp.MTRL + kosttemp.GRAVARE + 
               kosttemp.MASKOVRIG + kosttemp.OVRKR + slutsum.LONKOST.            
               IF summakostvar > 0 THEN DO:
                  SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(AONRKONTKOD.SATS% / 100 * summakostvar,">>>>>>>9").                       
               END.
               ELSE DO:           
                  SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(AONRKONTKOD.SATS% / 100 * summakostvar,"->>>>>>9").            
               END.
               ASSIGN
               SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(AONRKONTKOD.SATS% / 100 * kosttemp.OVRKR,"->>>>>9").
               IF kosttemp.INKOMST > 0 THEN
               SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(AONRKONTKOD.SATS% / 100 * kosttemp.INKOMST,">>>>>>>9").
               ELSE SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(AONRKONTKOD.SATS% / 100 * kosttemp.INKOMST,"->>>>>>9").
               /*hjalpvar = kosttemp.INKOMST - (slutsum.BELOPP + kosttemp.MTRL + kosttemp.GRAVARE + 
               kosttemp.MASKOVRIG + kosttemp.OVRKR).
               IF hjalpvar > 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(AONRKONTKOD.SATS% * hjalpvar,">>>>>>>9").
               ELSE SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(AONRKONTKOD.SATS% * hjalpvar,"->>>>>>9").*/
      
               IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
                  /* 65922 KOSTREG istället/också för MASKINENTREP förslag Wenche 20181108
                  byt till 65923 i övergång för att inte få dubbelt  Wenche 20181127*/
                  IF kosttemp.GRAVARE > 0 THEN 
                  SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING( AONRKONTKOD.SATS% / 100 * (kosttemp.GRAVARE + slutsum.MBELOPP),">>>>>>9").
                  ELSE SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.MBELOPP,">>>>>>9").                               
                  ASSIGN 
                  SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.IBELOPP,">>>>>>9").
                  SUBSTRING(tidut.UT,utnr[nrcol[27]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.FBELOPP,">>>>>>9").
                  DEBUGGER:SET-BREAK().
                  summakostvar = summakostvar + slutsum.MBELOPP + slutsum.IBELOPP + slutsum.FBELOPP.               
                  IF summakostvar > 0 THEN DO:
                     SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(AONRKONTKOD.SATS% / 100 * summakostvar,">>>>>>>9").                       
                  END.
                  ELSE DO:           
                     SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(AONRKONTKOD.SATS% / 100 * summakostvar,"->>>>>>9").            
                  END.
                  /*hjalpvar = hjalpvar - ( slutsum.MBELOPP + slutsum.IBELOPP).
                  IF hjalpvar > 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(AONRKONTKOD.SATS% * hjalpvar,">>>>>>>9").
                  ELSE SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(AONRKONTKOD.SATS% * hjalpvar,"->>>>>>9").*/
               END.
               IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:            
                  ASSIGN               
                  SUBSTRING(tidut.UT,utnr[nrcol[27]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.FBELOPP,">>>>>>9").
                  summakostvar = summakostvar  + slutsum.FBELOPP.
                  IF summakostvar > 0 THEN DO:
                     SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(AONRKONTKOD.SATS% / 100 * summakostvar,">>>>>>>9").                       
                  END.
                  ELSE DO:           
                     SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(AONRKONTKOD.SATS% / 100 * summakostvar,"->>>>>>9").            
                  END.
                  /*hjalpvar = hjalpvar - ( slutsum.MBELOPP + slutsum.IBELOPP).
                  IF hjalpvar > 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(AONRKONTKOD.SATS% * hjalpvar,">>>>>>>9").
                  ELSE SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(AONRKONTKOD.SATS% * hjalpvar,"->>>>>>9").*/
               END.
               IF AVAILABLE valdaao THEN DO:
                  RUN pnamn_UI.                       
               END. 
               FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "UF" NO-ERROR.
               ASSIGN
               sumsum.TIMMAR =  sumsum.TIMMAR  + ( slutsum.TIMMAR * AONRKONTKOD.SATS% / 100)    
               sumsum.NTIMMAR =  sumsum.NTIMMAR  + ( slutsum.NTIMMAR  * AONRKONTKOD.SATS% / 100)  
               sumsum.OATIMMAR =  sumsum.OATIMMAR  + ( slutsum.OATIMMAR  * AONRKONTKOD.SATS% / 100)  
               sumsum.ABELOPP =  sumsum.ABELOPP + ( slutsum.ABELOPP  * AONRKONTKOD.SATS% / 100)  
               sumsum.BTIMMAR = sumsum.BTIMMAR + ( slutsum.BTIMMAR * AONRKONTKOD.SATS% / 100)  
               sumsum.NBTIMMAR =  sumsum.NBTIMMAR  + ( slutsum.NBTIMMAR  * AONRKONTKOD.SATS% / 100)  
               sumsum.OBTIMMAR =  sumsum.OBTIMMAR  + ( slutsum.OBTIMMAR  * AONRKONTKOD.SATS% / 100)  
               sumsum.BBELOPP =  sumsum.BBELOPP + ( slutsum.BBELOPP  * AONRKONTKOD.SATS% / 100)  
               sumsum.OTIMMAR = sumsum.OTIMMAR + ( slutsum.OTIMMAR * AONRKONTKOD.SATS% / 100)  
               sumsum.NOTIMMAR =  sumsum.NOTIMMAR  + ( slutsum.NOTIMMAR  * AONRKONTKOD.SATS% / 100)  
               sumsum.OOTIMMAR =  sumsum.OOTIMMAR  + ( slutsum.OOTIMMAR  * AONRKONTKOD.SATS% / 100)  
               sumsum.OBELOPP =  sumsum.OBELOPP + ( slutsum.OBELOPP  * AONRKONTKOD.SATS% / 100)  
               /*sumsum.BELOPP =  sumsum.BELOPP + slutsum.BELOPP 
               sumsum.SKOSTNAD = sumsum.SKOSTNAD + slutsum.BELOPP
               sumsum.TINKOMST = sumsum.TINKOMST + (0 - slutsum.BELOPP)*/
   
               /*ASSIGN
               sumsum.TIMMAR =  sumsum.TIMMAR  + slutsum.TIMMAR                                                      
               sumsum.BTIMMAR = sumsum.BTIMMAR + slutsum.BTIMMAR
               sumsum.OTIMMAR = sumsum.OTIMMAR + slutsum.OTIMMAR*/
               sumsum.TINKOMST = sumsum.TINKOMST + ( AONRKONTKOD.SATS% / 100)  * (kosttemp.INKOMST - 
               (slutsum.BELOPP + kosttemp.MTRL + kosttemp.GRAVARE + kosttemp.MASKOVRIG + 
               kosttemp.OVRKR))
               sumsum.BELOPP = sumsum.BELOPP + ( AONRKONTKOD.SATS% / 100)  * ( slutsum.BELOPP + kosttemp.BELOPP)
               sumsum.SKOSTNAD = sumsum.SKOSTNAD + ( AONRKONTKOD.SATS% / 100)  * ( slutsum.BELOPP + kosttemp.MTRL + kosttemp.GRAVARE + kosttemp.MASKOVRIG + 
               kosttemp.OVRKR)
               sumsum.MTRL = sumsum.MTRL + ( AONRKONTKOD.SATS% / 100) * kosttemp.MTRL
               sumsum.GRAVARE = sumsum.GRAVARE + ( AONRKONTKOD.SATS% / 100) * kosttemp.GRAVARE
               sumsum.MASKOVRIG = sumsum.MASKOVRIG + ( AONRKONTKOD.SATS% / 100) * kosttemp.MASKOVRIG
               sumsum.OVRKR = sumsum.OVRKR + ( AONRKONTKOD.SATS% / 100) * kosttemp.OVRKR                                                     
               sumsum.INKOMST = sumsum.INKOMST + ( AONRKONTKOD.SATS% / 100) * kosttemp.INKOMST.
               IF Guru.Konstanter:globforetag = "gkal" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
                  ASSIGN
                  sumsum.GRAVARE =  sumsum.GRAVARE + ( AONRKONTKOD.SATS% / 100) * slutsum.MBELOPP 
                  sumsum.MASKOVRIG =  sumsum.MASKOVRIG + ( AONRKONTKOD.SATS% / 100) * slutsum.IBELOPP. 
                  sumsum.FORDON =  sumsum.FORDON + ( AONRKONTKOD.SATS% / 100) * slutsum.FBELOPP. 
               END.
               IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
                  ASSIGN               
                  sumsum.FORDON =  sumsum.FORDON + ( AONRKONTKOD.SATS% / 100) * slutsum.FBELOPP. 
               END.
            END.
         END.
      END.
               

   END.
   IF ingakostver = FALSE THEN  DO:     
   END.
   ELSE DO:
      FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "UF" NO-ERROR.
      CREATE tidut.
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "SUMMA".
      IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
         /*GRAVARE tillagt redan, detta blir dubbelt  20190815*/
         ASSIGN
         sumsum.SKOSTNAD = sumsum.SKOSTNAD + sumsum.MASKOVRIG /*+ sumsum.GRAVARE*/ + sumsum.FORDON
         sumsum.TINKOMST = sumsum.TINKOMST + (0 - (sumsum.MASKOVRIG /*+ sumsum.GRAVARE*/ + sumsum.FORDON)).
      END.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
         ASSIGN
         sumsum.SKOSTNAD = sumsum.SKOSTNAD + sumsum.FORDON
         sumsum.TINKOMST = sumsum.TINKOMST + (0 - (sumsum.FORDON)).
      END.

      ASSIGN       
      /*SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "UF"                     */
      SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(sumsum.NTIMMAR,">>>>>9")
      SUBSTRING(tidut.UT,utnr[nrcol[22]]) = STRING(sumsum.OATIMMAR,">>>>>9")                                                          
      SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(sumsum.ABELOPP,">>>>>>>9")                                                          
      SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(sumsum.NBTIMMAR,">>>>>9")
      SUBSTRING(tidut.UT,utnr[nrcol[23]]) = STRING(sumsum.OBTIMMAR,">>>>>9")                                                          
      SUBSTRING(tidut.UT,utnr[nrcol[24]]) = STRING(sumsum.BBELOPP,">>>>>>>9")                                                          
      SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(sumsum.NOTIMMAR,">>>>>9")  
      SUBSTRING(tidut.UT,utnr[nrcol[25]]) = STRING(sumsum.OOTIMMAR,">>>>>9")                                                          
      SUBSTRING(tidut.UT,utnr[nrcol[26]]) = STRING(sumsum.OBELOPP,">>>>>>>9")                                                                               
      SUBSTRING(tidut.UT,utnr[nrcol[28]]) = STRING(sumsum.LONKOST,">>>>>>>9")
      SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(sumsum.BELOPP,">>>>>>>9").

      /*SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(sumsum.TINKOMST,"->>>>>>9").*/
      /*IF vartot > 0 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING(vartot,">>>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING(vartot,"->>>>>>9").*/
   /*   IF sumsum.BELOPP < 0 
      THEN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(sumsum.BELOPP,"->>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(sumsum.BELOPP,">>>>>>9").*/
      IF sumsum.GRAVARE < 0 
      THEN SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(sumsum.GRAVARE,"->>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(sumsum.GRAVARE,">>>>>>9").
      IF sumsum.MASKOVRIG < 0 
      THEN SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(sumsum.MASKOVRIG,"->>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(sumsum.MASKOVRIG,">>>>>>9").
      IF sumsum.FORDON < 0 
      THEN SUBSTRING(tidut.UT,utnr[nrcol[27]]) = STRING(sumsum.FORDON,"->>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[27]]) = STRING(sumsum.FORDON,">>>>>>9").
      IF sumsum.MTRL < 0 
      THEN SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(sumsum.MTRL,"->>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(sumsum.MTRL,">>>>>>>9").
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(sumsum.OVRKR,"->>>>>9")
      SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(sumsum.SKOSTNAD,">>>>>>>9").
      IF sumsum.INKOMST > 0 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(sumsum.INKOMST,">>>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(sumsum.INKOMST,"->>>>>>9").      
      
   END.
END PROCEDURE.

PROCEDURE kostreg_UI :   
   GET FIRST kq NO-LOCK.
   DO WHILE AVAILABLE(KOSTREG):     
      CREATE kosttemp2.
      ASSIGN            
      kosttemp2.ANVANDARE = SUBSTRING(KOSTREG.ANVANDARE,1,12) 
      kosttemp2.AONR = KOSTREG.AONR 
      kosttemp2.DELNR = valdaao.DELNR
      kosttemp2.BENAMNING = KOSTREG.BENAMNING 
      kosttemp2.BETDATUM = KOSTREG.BETDATUM 
      kosttemp2.BOKKONTO = KOSTREG.BOKKONTO 
      kosttemp2.DELNR = KOSTREG.DELNR 
      kosttemp2.FAKBES = KOSTREG.FAKBES 
      kosttemp2.FAKTNR = KOSTREG.FAKTNR 
     /* kosttemp2.FAKTURERAD = KOSTREG.FAKTURERAD */
      kosttemp2.INKOMST = KOSTREG.INKOMST  
     /* kosttemp2.KOSTAUTO = KOSTREG.KOSTAUTO */
      kosttemp2.LEVKOD = KOSTREG.LEVKOD  
      kosttemp2.MOMS = KOSTREG.MOMS 
      kosttemp2.RADNR = KOSTREG.RADNR 
      kosttemp2.REGDATUM = KOSTREG.REGDATUM 
      kosttemp2.TRAKTKOST = KOSTREG.TRAKTKOST.
      
      /*IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
         IF SUBSTRING(KOSTREG.BOKKONTO,1,1)= "6" THEN DO:      
            IF KOSTREG.BOKKONTO = "65920" THEN kosttemp2.GRAVARE = KOSTREG.MASKKOST.
            ELSE IF KOSTREG.BOKKONTO > "65920" THEN kosttemp2.MASKOVRIG = KOSTREG.MASKKOST.      
            ELSE kosttemp2.MASKOVRIG = KOSTREG.MASKKOST.
            ASSIGN
            kosttemp2.MTRL = KOSTREG.MTRL 
            kosttemp2.OVRKR = KOSTREG.OVRKR 
            kosttemp2.PERSKOST = KOSTREG.PERSKOST 
            kosttemp2.PERSKOST2 = 0.
         END.
         ELSE DO:
            ASSIGN
            kosttemp2.MTRL = KOSTREG.MTRL 
            kosttemp2.OVRKR = KOSTREG.OVRKR 
            kosttemp2.PERSKOST = KOSTREG.PERSKOST 
            kosttemp2.PERSKOST2 = 0.
         END.
      END.*/
      IF Guru.Konstanter:globforetag = "gkal" THEN DO:         
         /* 65922 KOSTREG istället för MASKINENTREP förslag Wenche 20181108
         byt till 65923 i övergång för att inte få dubbelt  Wenche 20181127*/     
         IF KOSTREG.BOKKONTO = "65923" THEN kosttemp2.GRAVARE = KOSTREG.MASKKOST.                     
         ASSIGN
         kosttemp2.MTRL = KOSTREG.MTRL 
         kosttemp2.OVRKR = KOSTREG.OVRKR 
         kosttemp2.PERSKOST = KOSTREG.PERSKOST 
         kosttemp2.PERSKOST2 = 0.         
      END.
      ELSE IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         ASSIGN
         kosttemp2.MASKOVRIG = KOSTREG.MASKKOST
         kosttemp2.MTRL = KOSTREG.MTRL 
         kosttemp2.OVRKR = KOSTREG.OVRKR 
         kosttemp2.PERSKOST = KOSTREG.PERSKOST 
         kosttemp2.PERSKOST2 = 0.
      END.
      ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
         vartvar = "".
         RUN kontokoll_UI (INPUT KOSTREG.BOKKONTO,OUTPUT vartvar).
         IF vartvar = "OVRIGT" THEN kosttemp2.OVRKR = KOSTREG.OVRKR.
         IF vartvar = "MASK1" THEN kosttemp2.GRAVARE = KOSTREG.MASKKOST.
         IF vartvar = "MASK2" THEN kosttemp2.MASKOVRIG = KOSTREG.MASKKOST.
         IF vartvar = "MASK3" THEN kosttemp2.MTRL = KOSTREG.MASKKOST. 
         IF vartvar = "MTRL2" THEN kosttemp2.PERSKOST = KOSTREG.MTRL.
         IF vartvar = "PERS2" THEN kosttemp2.PERSKOST2 = KOSTREG.PERSKOST.            
      END.
      ELSE DO:
         ASSIGN
         kosttemp2.MTRL = KOSTREG.MTRL 
         kosttemp2.OVRKR = KOSTREG.OVRKR 
         kosttemp2.PERSKOST = KOSTREG.PERSKOST 
         kosttemp2.PERSKOST2 = 0.
      END.
      GET NEXT kq NO-LOCK.      
   END.
END PROCEDURE.
PROCEDURE kontokoll_UI:
   DEFINE INPUT PARAMETER varkont LIKE KOSTREG.BOKKONTO NO-UNDO.
   DEFINE OUTPUT PARAMETER varvart AS CHARACTER NO-UNDO.
   RUN SUPER (INPUT varkont,OUTPUT varvart).
END PROCEDURE.

PROCEDURE skapadagdag_UI :
   GET FIRST stq NO-LOCK.
   DO WHILE AVAILABLE(SUMTIDDAG):  
      IF Guru.Konstanter:globforetag = "GKAL" AND SUMTIDDAG.PERSMASK = FALSE THEN DO:
         /*Kalmar vill ha med tiskrivning maskiner -ej kostnadsregistreringar*/    
              
         CREATE dagtemp.
         ASSIGN   
         dagtemp.OMRADE = SUMTIDDAG.OMRADE       
         dagtemp.AONR = SUMTIDDAG.AONR
         dagtemp.DELNR = valdaao.DELNR 
         dagtemp.ORT = SUMTIDDAG.ORT
         dagtemp.PERSMASK = SUMTIDDAG.PERSMASK
         dagtemp.BELOPP = SUMTIDDAG.BELOPP + SUMTIDDAG.OBELOPP
         dagtemp.BEFATTNING = SUBSTRING(SUMTIDDAG.BEFATTNING,1,20)
         dagtemp.LONKOST = SUMTIDDAG.LONKOST.         
      END.
      ELSE IF SUMTIDDAG.PERSMASK = FALSE THEN musz = musz.
      ELSE IF SUMTIDDAG.PRISTYP = "RESTID..." THEN DO: 
         /*
         FIND FIRST restid WHERE restid.AONR = SUMTIDDAG.AONR AND
         restid.DELNR = SUMTIDDAG.DELNR USE-INDEX AONR EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE restid THEN CREATE restid.
         ASSIGN          
         restid.AONR = SUMTIDDAG.AONR
         restid.DELNR = SUMTIDDAG.DELNR 
         restid.TIMMAR = restid.TIMMAR + SUMTIDDAG.TIMMAR.           
         CREATE dagtemp.
         ASSIGN          
         dagtemp.AONR = SUMTIDDAG.AONR
         dagtemp.DELNR = SUMTIDDAG.DELNR 
         dagtemp.PERSMASK = SUMTIDDAG.PERSMASK             
         dagtemp.OTIMMAR = SUMTIDDAG.OTIMMAR 
         dagtemp.BELOPP = SUMTIDDAG.BELOPP 
         dagtemp.OBELOPP = SUMTIDDAG.OBELOPP 
         dagtemp.TBELOPP = SUMTIDDAG.TBELOPP
         dagtemp.LONKOST = SUMTIDDAG.LONKOST.
         */
      END.
      ELSE DO:
         CREATE dagtemp.
         ASSIGN        
         dagtemp.OMRADE = SUMTIDDAG.OMRADE  
         dagtemp.AONR = SUMTIDDAG.AONR
         dagtemp.DELNR = valdaao.DELNR 
         dagtemp.ORT = SUMTIDDAG.ORT
         dagtemp.PERSMASK = SUMTIDDAG.PERSMASK
         dagtemp.BELOPP = SUMTIDDAG.BELOPP + SUMTIDDAG.OBELOPP
         dagtemp.LONKOST = SUMTIDDAG.LONKOST.                          
         IF SUMTIDDAG.PRIS = 0 THEN DO:
            dagtemp.OTIMMAR = SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
            dagtemp.NOTIMMAR = SUMTIDDAG.TIMMAR. 
            dagtemp.OOTIMMAR = SUMTIDDAG.OTIMMAR.    
            dagtemp.OBELOPP = dagtemp.BELOPP.            
         END.
         ELSE DO:
            /*"EJ DEBI.."*/
            IF SUMTIDDAG.PRISTYP = "BEREDARE." THEN DO:              
               dagtemp.BTIMMAR = SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR. 
               dagtemp.NBTIMMAR = SUMTIDDAG.TIMMAR. 
               dagtemp.OBTIMMAR = SUMTIDDAG.OTIMMAR.    
               dagtemp.BBELOPP = dagtemp.BELOPP.
            END.
            ELSE IF SUMTIDDAG.BEFATTNING BEGINS "BEREDARE" THEN DO:
               dagtemp.BTIMMAR = SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
               dagtemp.NBTIMMAR = SUMTIDDAG.TIMMAR. 
               dagtemp.OBTIMMAR = SUMTIDDAG.OTIMMAR.    
               dagtemp.BBELOPP = dagtemp.BELOPP.
            END.
            ELSE IF SUMTIDDAG.BEFATTNING BEGINS "MONTÖR" THEN DO:
               dagtemp.TIMMAR = SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
               dagtemp.NTIMMAR = SUMTIDDAG.TIMMAR. 
               dagtemp.OATIMMAR = SUMTIDDAG.OTIMMAR.    
               dagtemp.ABELOPP = dagtemp.BELOPP.               
            END.
            ELSE IF Guru.Konstanter:globforetag = "gkal" AND SUMTIDDAG.BEFATTNING BEGINS "TEKNIKER(+)" THEN DO:
               /*montör kalmar  efter 20150101*/
               dagtemp.TIMMAR = SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
               dagtemp.NTIMMAR = SUMTIDDAG.TIMMAR. 
               dagtemp.OATIMMAR = SUMTIDDAG.OTIMMAR.    
               dagtemp.ABELOPP = dagtemp.BELOPP.               
            END.
            
            ELSE DO:
               dagtemp.OTIMMAR = SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
               dagtemp.NOTIMMAR = SUMTIDDAG.TIMMAR. 
               dagtemp.OOTIMMAR = SUMTIDDAG.OTIMMAR.    
               dagtemp.OBELOPP = dagtemp.BELOPP.
            END.
         END.
      END.
      GET NEXT stq NO-LOCK.      
   END.
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
      GET FIRST tq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):
         FIND FIRST LONTILL WHERE LONTILL.LONTILLAGG = TIDREGITAB.LONTILLAGG NO-LOCK NO-ERROR.
         IF AVAILABLE LONTILL AND SUBSTRING(LONTILL.TYPKOD,1,3) = "BIL" THEN DO: 
            FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = TIDREGITAB.PERSONALKOD NO-LOCK NO-ERROR.        
            CREATE dagtemp.
            ASSIGN   
            dagtemp.OMRADE = PERSONALTAB.OMRADE       
            dagtemp.AONR = TIDREGITAB.AONR
            dagtemp.DELNR = TIDREGITAB.DELNR 
            /*dagtemp.ORT = SUMTID.ORT */
            dagtemp.PERSMASK = FALSE.
            dagtemp.BELOPP =  LONTILL.ERSATTNING * klockan100(TIDREGITAB.LONTILLANTAL).
            dagtemp.BEFATTNING = "FORDON".                            
            FIND FIRST AONRTAB WHERE AONRTAB.AONR = TIDREGITAB.AONR AND AONRTAB.DELNR = TIDREGITAB.DELNR NO-LOCK NO-ERROR.
            IF AVAILABLE AONRTAB THEN DO: 
               dagtemp.ORT = AONRTAB.ORT. 
            END.            
         END.
         GET NEXT tq NO-LOCK.      
      END.
   END.
END PROCEDURE.
PROCEDURE summa_UI.
   /*PERSONER*/
   FOR EACH dagtemp WHERE dagtemp.PERSMASK = TRUE NO-LOCK 
   BREAK BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK: 
      ACCUMULATE dagtemp.BELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.TIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.NTIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.OATIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.ABELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.OTIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.NOTIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.OOTIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.OBELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.BTIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK).        
      ACCUMULATE dagtemp.NBTIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.OBTIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.BBELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.LONKOST (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      IF LAST-OF(dagtemp.PERSMASK) THEN DO:         
         CREATE slutsum.
         ASSIGN 
         slutsum.AONR = dagtemp.AONR
         slutsum.DELNR = dagtemp.DELNR
         slutsum.ORT = dagtemp.ORT          
         slutsum.BELOPP = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.BELOPP)                      
         slutsum.TIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.TIMMAR) 
         slutsum.NTIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.NTIMMAR) 
         slutsum.OATIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.OATIMMAR) 
         slutsum.ABELOPP = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.ABELOPP)                      
         slutsum.OTIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.OTIMMAR)
         slutsum.NOTIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.NOTIMMAR) 
         slutsum.OOTIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.OOTIMMAR)
         slutsum.OBELOPP = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.OBELOPP)                      
         slutsum.BTIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.BTIMMAR)             
         slutsum.NBTIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.NBTIMMAR) 
         slutsum.OBTIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.OBTIMMAR)
         slutsum.BBELOPP = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.BBELOPP).                               
         slutsum.LONKOST = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.LONKOST).                                        
      END.     
   END.
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
      /*MASKINER  för in 65922 KOSTREG istället för MASKINENTREP förslag Wenche 20181108*/      
      FOR EACH dagtemp WHERE dagtemp.PERSMASK = FALSE NO-LOCK 
      BREAK BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK BY dagtemp.BEFATTNING: 
         ACCUMULATE dagtemp.BELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK BY dagtemp.BEFATTNING).           
         IF LAST-OF(dagtemp.BEFATTNING) THEN DO:
            FIND FIRST slutsum WHERE slutsum.AONR = dagtemp.AONR AND
            slutsum.DELNR = dagtemp.DELNR NO-LOCK NO-ERROR.
            IF NOT AVAILABLE slutsum THEN CREATE slutsum.
            ASSIGN slutsum.ORT = dagtemp.ORT.
            IF dagtemp.BEFATTNING = "MASKINENTREP" THEN DO:           
               ASSIGN 
               slutsum.AONR = dagtemp.AONR
               slutsum.DELNR = dagtemp.DELNR  
               slutsum.MBELOPP = slutsum.MBELOPP + 
               (ACCUM TOTAL BY dagtemp.BEFATTNING dagtemp.BELOPP).                              
            END.
            ELSE IF dagtemp.BEFATTNING = "FORDON" THEN DO:           
               ASSIGN 
               slutsum.AONR = dagtemp.AONR
               slutsum.DELNR = dagtemp.DELNR  
               slutsum.FBELOPP = slutsum.FBELOPP + 
               (ACCUM TOTAL BY dagtemp.BEFATTNING dagtemp.BELOPP).                              
            END.
            ELSE DO:
               /*ÖVRIGA*/
               ASSIGN 
               slutsum.AONR = dagtemp.AONR
               slutsum.DELNR = dagtemp.DELNR  
               slutsum.IBELOPP = slutsum.IBELOPP + 
               (ACCUM TOTAL BY dagtemp.BEFATTNING dagtemp.BELOPP).                  
            END.
         END.
      END.

   END.

   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      /*MASKINER*/      
      FOR EACH dagtemp WHERE dagtemp.PERSMASK = FALSE NO-LOCK 
      BREAK BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK BY dagtemp.BEFATTNING: 
         ACCUMULATE dagtemp.BELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK BY dagtemp.BEFATTNING).           
         IF LAST-OF(dagtemp.BEFATTNING) THEN DO:
            FIND FIRST slutsum WHERE slutsum.AONR = dagtemp.AONR AND
            slutsum.DELNR = dagtemp.DELNR NO-LOCK NO-ERROR.
            IF NOT AVAILABLE slutsum THEN CREATE slutsum.            
            ASSIGN slutsum.ORT = dagtemp.ORT.
            IF dagtemp.BEFATTNING = "FORDON" THEN DO:           
               ASSIGN 
               slutsum.AONR = dagtemp.AONR
               slutsum.DELNR = dagtemp.DELNR  
               slutsum.FBELOPP = slutsum.FBELOPP + 
               (ACCUM TOTAL BY dagtemp.BEFATTNING dagtemp.BELOPP).                              
            END.            
         END.
      END.

   END.
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO: 
      /*BÅDE OB OCH FORDON LIGGER SOM LÖNETILLÄGG*/      
      FOR EACH slutsum WHERE slutsum.FBELOPP > 0 :        
         IF slutsum.LONKOST > slutsum.FBELOPP THEN DO: 
            slutsum.LONKOST = slutsum.LONKOST - slutsum.FBELOPP.
         END.
         ELSE slutsum.LONKOST = 0.        
      END.
   END.
   FOR EACH kosttemp2 BREAK BY kosttemp2.AONR BY kosttemp2.DELNR:     
      ACCUMULATE kosttemp2.PERSKOST2 (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR). 
      ACCUMULATE kosttemp2.PERSKOST (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR). 
      ACCUMULATE kosttemp2.GRAVARE (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR). 
      ACCUMULATE kosttemp2.MASKOVRIG (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR). 
      ACCUMULATE kosttemp2.TRAKTKOST (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR). 
      ACCUMULATE kosttemp2.MTRL (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR).  
      ACCUMULATE kosttemp2.OVRKR (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR).  
      ACCUMULATE kosttemp2.INKOMST (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR). 
      IF LAST-OF(kosttemp2.DELNR) THEN DO:
         CREATE kosttemp.
         ASSIGN 
         kosttemp.AONR = kosttemp2.AONR
         kosttemp.DELNR = kosttemp2.DELNR 
         kosttemp.MASKOVRIG = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.MASKOVRIG) 
         kosttemp.GRAVARE = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.GRAVARE) 
         kosttemp.TBELOPP = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.TRAKTKOST)
         kosttemp.MTRL = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.MTRL)    
         kosttemp.OVRKR = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.OVRKR)
         kosttemp.INKOMST = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.INKOMST).         
         IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
            ASSIGN
            kosttemp.BELOPP1P = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.PERSKOST)
            kosttemp.BELOPP2P = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.PERSKOST2).
         END.
         ELSE DO:
            kosttemp.BELOPP = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.PERSKOST). 
         END.
         IF Guru.Konstanter:varforetypval[10] = 1 THEN DO:
            kosttemp.INKOMST = 0.
         END.
      END.       
   END. 
   FOR EACH kosttemp2 WHERE kosttemp2.INKOMST NE 0 
      BREAK BY kosttemp2.AONR BY kosttemp2.DELNR BY kosttemp2.BOKKONTO:     
      ACCUMULATE kosttemp2.INKOMST (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR BY kosttemp2.BOKKONTO). 
      IF LAST-OF(kosttemp2.BOKKONTO) THEN DO:
         CREATE inkomsttemp.
         ASSIGN 
         inkomsttemp.AONR = kosttemp2.AONR
         inkomsttemp.DELNR = kosttemp2.DELNR 
         inkomsttemp.BOKKONTO = kosttemp2.BOKKONTO
         inkomsttemp.INKOMST = (ACCUM TOTAL BY kosttemp2.BOKKONTO kosttemp2.INKOMST).                  
      END.       
   END.
   FOR EACH inkomsttemp:
      FIND FIRST intakttemp WHERE intakttemp.AONR = inkomsttemp.AONR AND 
      intakttemp.DELNR = inkomsttemp.DELNR NO-ERROR.
      IF NOT AVAILABLE intakttemp THEN CREATE intakttemp.
      ASSIGN
      intakttemp.AONR  = inkomsttemp.AONR 
      intakttemp.DELNR = inkomsttemp.DELNR.             
      IF inkomsttemp.BOKKONTO = "36680" THEN intakttemp.INTINTAKT = intakttemp.INTINTAKT + inkomsttemp.INKOMST.   
      ELSE intakttemp.EXTERNINTAKT = intakttemp.EXTERNINTAKT + inkomsttemp.INKOMST.
   END.
END PROCEDURE.   

PROCEDURE pnamn_UI:
   ASSIGN
   SUBSTRING(tidut.UT,300) = valdaao.ARBANSVARIG
   SUBSTRING(tidut.UT,320) = valdaao.BEREDARE   
   SUBSTRING(tidut.UT,340) = valdaao.PROJEKTOR.
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valdaao.ARBANSVARIG NO-LOCK NO-ERROR.
   IF AVAILABLE PERSONALTAB THEN DO:
      SUBSTRING(tidut.UT,300) = CAPS(SUBSTRING(PERSONALTAB.FORNAMN,1,1)) + " " + CAPS(PERSONALTAB.EFTERNAMN).
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
   END.   
   ELSE SUBSTRING(tidut.UT,300) = valdaao.ARBANSVARIG.
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valdaao.BEREDARE NO-LOCK NO-ERROR.
   IF AVAILABLE PERSONALTAB THEN DO:
      SUBSTRING(tidut.UT,320) = CAPS(SUBSTRING(PERSONALTAB.FORNAMN,1,1)) + " " + CAPS(PERSONALTAB.EFTERNAMN).
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
   END.
   ELSE SUBSTRING(tidut.UT,320) = valdaao.BEREDARE.
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valdaao.PROJEKTOR NO-LOCK NO-ERROR.
   IF AVAILABLE PERSONALTAB THEN DO:
      SUBSTRING(tidut.UT,340) = CAPS(SUBSTRING(PERSONALTAB.FORNAMN,1,1)) + " " + CAPS(PERSONALTAB.EFTERNAMN).
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
   END.
   ELSE SUBSTRING(tidut.UT,340) = valdaao.PROJEKTOR.
END PROCEDURE.
