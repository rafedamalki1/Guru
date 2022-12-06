/*AOMBAPPLULE.P*/
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}
{EXTRATAB.I}


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
DEFINE VARIABLE emask3 AS DECIMAL NO-UNDO.
FIND FIRST FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
RUN STYRFORE.P (INPUT globforetag).
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

{DAGTEMP.I}
       
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
   FIELD OTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "OTIMMAR"         
   FIELD BTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "OTIMMAR"
   FIELD BELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "ARBKOSTNAD"           
   FIELD OBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "Ö-KOSTNAD"  
   FIELD MBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "Ö-KOSTNAD"  
   FIELD IBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "Ö-KOSTNAD"  
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
   FIELD OTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "OTIMMAR"   
   FIELD MBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "ARBKOSTNAD"   
   FIELD BELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "ARBKOSTNAD"
   FIELD SKOSTNAD LIKE EKRAPPRESULT.EBELOPP LABEL "SUMMA KOSTNADER"
   FIELD BTIMMAR LIKE EKRAPPRESULT.EBELOPP LABEL "Ö-KOSTNAD"
   FIELD GRAVARE LIKE KOSTREG.MASKKOST  
   FIELD MASKOVRIG LIKE KOSTREG.MASKKOST  
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

{DIRDEF.I}
{KALKTEMP.I}
{EXTRADATA.I}

FIND FIRST FORETAG NO-LOCK NO-ERROR.
{BOLAGSEKSTART.I}
DEFINE INPUT  PARAMETER ganv AS CHARACTER NO-UNDO.
globanv = ganv.
DEFINE INPUT PARAMETER TABLE FOR uppvaltemp.
DEFINE INPUT PARAMETER TABLE FOR valdaao.
DEFINE OUTPUT PARAMETER TABLE FOR tidut.
DEFINE OUTPUT PARAMETER str AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE OUTPUT PARAMETER str2 AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE OUTPUT PARAMETER str3 AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE NEW SHARED VARIABLE fastrec AS RECID NO-UNDO.
DEFINE VARIABLE offert AS LOGICAL NO-UNDO.
DEFINE VARIABLE xtypmtrl AS INTEGER NO-UNDO.
DEFINE VARIABLE monpris LIKE EBRPRIS.MONT NO-UNDO.
DEFINE VARIABLE berpris AS DECIMAL NO-UNDO.
DEFINE VARIABLE region AS LOGICAL NO-UNDO.
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
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
/*BEREDNING*/
{LISTDEF.I} 
DEFINE NEW SHARED TEMP-TABLE mtrl_temp2   
   FIELD ENR LIKE MTRLBER.ENR
   FIELD BENAMNING LIKE MTRLBER.BENAMNING
   FIELD ENHET LIKE MTRLBER.ENHET
   FIELD ANTAL LIKE MTRLBER.ANTAL
   FIELD PRIS LIKE MTRLBER.PRIS 
   FIELD TOTPRIS LIKE MTRLBER.PRIS
   FIELD LEVKOD LIKE MTRLBER.LEVKOD      
   INDEX ENR IS PRIMARY ENR ASCENDING. 
/*SLUT BEREDNING*/

FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR. 
globforetag = FORETAG.FORETAG.

FIND FIRST uppvaltemp NO-ERROR.

OPEN QUERY aq FOR EACH valdaao NO-LOCK. 
GET FIRST aq NO-LOCK.
GET NEXT aq  NO-LOCK.
IF AVAILABLE valdaao THEN ingakostver = TRUE.
ELSE DO:
   /*IF uppvaltemp.DELNRKOLL = FALSE THEN ingakostver = FALSE.*/
   ingakostver = FALSE.
END.
IF uppvaltemp.MANUPPDEL = TRUE THEN ingakostver = FALSE.
ELSE ingakostver = TRUE.
GET FIRST aq NO-LOCK.
DO WHILE AVAILABLE(valdaao):      
   {SUMOPEN.I}
   RUN kalkyler_UI. /*ccc*/
   GET NEXT aq NO-LOCK.
END.      
{FRIKALKKOLL2.I}
{DAGTEMPBOLAG.I}
RUN summa_UI.
RUN huvud_UI.
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
   CREATE tidut.
   ASSIGN 
   tidut.UT = "UF=KOSTNADER FRÅN TIDREDOVISNING OCH KOSTNADSREGISTRERING, KA=KALKYLERING".      
   ASSIGN
   nrcol[1] = 1
   nrcol[2] = 2
   nrcol[3] = 3
   nrcol[4] = 4
   nrcol[5] = 5
   nrcol[6] = 6
   nrcol[7] = 7
   nrcol[8] = 8
   nrcol[9] = 9
   nrcol[10] = 10
   nrcol[11] = 11
   nrcol[12] = 13
   nrcol[13] = 14
   nrcol[14] = 15 
   nrcol[15] = 12    /*ny kolumn*/

   nrcol[16] = 16 
   nrcol[17] = 17 
   nrcol[18] = 18 
   nrcol[19] = 19 

   breddantal = 19   /*antal kolumner*/
   bredd[1] = 9
   bredd[2] = 15
   bredd[3] = 2
   bredd[4] = 6
   bredd[5] = 6
   bredd[6] = 8
   bredd[7] = 7
   bredd[8] = 8
   bredd[9] = 8
   bredd[10] = 7
   bredd[11] = 8
   bredd[12] = 8
   bredd[13] = 9
   bredd[14] = 9
   bredd[15] = 9.
   
   /* old
   bredd[4] = 6
   bredd[5] = 6
   bredd[6] = 6
   bredd[7] = 8
   bredd[8] = 7
   bredd[9] = 7
   bredd[10] = 8
   bredd[11] = 7
   bredd[12] = 8
   bredd[13] = 8
   bredd[14] = 9
   bredd[15] = 9.
   */
   
   /*
   bredd[16] = 8
   bredd[17] = 8
   bredd[18] = 8.
   */
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
   ASSIGN                                                                                                       
   /*SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "MONTÖR"   TILL ÖVRIG*/              
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "BERED." /* LÄGG TILL TJÄNSTEMÄN */        
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "ÖVRIGA"  
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "ARBETS"                                          
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "FRÄMMANDE TJ."
   SUBSTRING(tidut.UT,utnr[nrcol[9]]) = "MATERIEL"
   SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "ÖVRIG"
   SUBSTRING(tidut.UT,utnr[nrcol[14]]) = "SUMMA"
   SUBSTRING(tidut.UT,utnr[nrcol[13]]) = "INTÄKT".
   
   str2 = tidut.UT.                 
   
   CREATE tidut.      
   ASSIGN                               
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = CAPS(Guru.Konstanter:gaok)      
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = CAPS(Guru.Konstanter:gaonamnk) 
   /*SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "TIMMAR"*/             
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "TIMMAR" /* BERED+TJÄNSTEMÄN */        
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "TIMMAR"  /* ÖVRIGA + MONTÖR */
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "KOSTNAD" 
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "ANL.ARB"/*"GRÄVARE"*/  
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "ANL.ENTR"/*"ÖVRIGA"*/ 
   SUBSTRING(tidut.UT,utnr[nrcol[9]]) = "KOSTNAD"
   SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "KOSTNAD"
   SUBSTRING(tidut.UT,utnr[nrcol[14]]) = "KOSTNAD"
   SUBSTRING(tidut.UT,utnr[nrcol[11]]) = "INTÄKT"   
   SUBSTRING(tidut.UT,utnr[nrcol[12]]) = "RESULTAT"
   SUBSTRING(tidut.UT,utnr[nrcol[13]]) = "PLAN"
   SUBSTRING(tidut.UT,200) = CAPS(Guru.Konstanter:garbal)
   SUBSTRING(tidut.UT,220) = CAPS(Guru.Konstanter:gberel)
   SUBSTRING(tidut.UT,240) = CAPS(Guru.Konstanter:gprojl).   

   str3 = tidut.UT.                
   CREATE tidut.      
   tidut.UT = str.
   CREATE sumsum.
   ASSIGN sumsum.PERSONALKOD = "UF".
   CREATE sumsum.
   ASSIGN sumsum.PERSONALKOD = "KA".   
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
      ELSE slutsum.ORT   = valdaao.ORT.
   END.
   FOR EACH slutsum USE-INDEX AONR:      
      ASSIGN
      varin = 0
      varkalk = 0.
      FIND FIRST kalksumsum WHERE kalksumsum.AONR = slutsum.AONR AND
      kalksumsum.DELNR = slutsum.DELNR USE-INDEX AONR NO-ERROR.
      IF AVAILABLE kalksumsum THEN DO:
         varkalk = kalksumsum.BELOPP + kalksumsum.MBELOPP + 
         kalksumsum.MTRL + kalksumsum.OVRKR +  
         kalksumsum.MASKGBELOPP + kalksumsum.MONTBELOPP + kalksumsum.BERBELOPP.         
      END.
      FIND FIRST kosttemp WHERE kosttemp.AONR = slutsum.AONR AND
      kosttemp.DELNR = slutsum.DELNR USE-INDEX AONR NO-ERROR.
      IF AVAILABLE kosttemp THEN DO:
         varin = kosttemp.INKOMST.
      END.
      vartot = vartot + (varin - varkalk).
      FIND FIRST valdaao WHERE valdaao.AONR = slutsum.AONR AND valdaao.DELNR = slutsum.DELNR NO-ERROR.
      CREATE tidut.                 
      IF NOT AVAILABLE kosttemp THEN DO:
         ASSIGN 
         SUBSTRING(tidut.UT,utnr[nrcol[1]]) = slutsum.AONR + STRING(slutsum.DELNR,Guru.Konstanter:varforetypchar[1])
         SUBSTRING(tidut.UT,utnr[nrcol[2]]) = SUBSTRING(slutsum.ORT,1,bredd[2])              
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "UF"                  
         SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(slutsum.TIMMAR,">>>>>9")                                                          
         SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(slutsum.BTIMMAR,">>>>>9")
         SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(slutsum.OTIMMAR,">>>>>9")  
         SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(slutsum.BELOPP,">>>>>>>9")  
         SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(slutsum.BELOPP,">>>>>>>9")
         SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(0 - slutsum.BELOPP,"->>>>>>9").                  
         IF AVAILABLE valdaao THEN DO:
            RUN pnamn_UI.                       
         END. 
         
         IF varin - varkalk > 0 THEN                                   
         SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING(varin - varkalk,"->>>>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING(varin - varkalk,"->>>>>>>9").
         
         SUBSTRING(tidut.UT,150) = slutsum.ORT.
         FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "UF" NO-ERROR.                   
         ASSIGN  
         sumsum.TIMMAR =  sumsum.TIMMAR  + slutsum.TIMMAR                                                      
         sumsum.BTIMMAR = sumsum.BTIMMAR + slutsum.BTIMMAR
         sumsum.OTIMMAR = sumsum.OTIMMAR + slutsum.OTIMMAR
         sumsum.BELOPP =  sumsum.BELOPP + slutsum.BELOPP 
         sumsum.SKOSTNAD = sumsum.SKOSTNAD + slutsum.BELOPP
         sumsum.TINKOMST = sumsum.TINKOMST + (0 - slutsum.BELOPP).                  
      END.
      ELSE DO:
         ASSIGN
         SUBSTRING(tidut.UT,utnr[nrcol[1]]) = slutsum.AONR + STRING(slutsum.DELNR,Guru.Konstanter:varforetypchar[1])
         SUBSTRING(tidut.UT,utnr[nrcol[2]]) = SUBSTRING(slutsum.ORT,1,bredd[2])      
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "UF"
         SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(slutsum.OTIMMAR,">>>>>9").
         SUBSTRING(tidut.UT,150) = slutsum.ORT.                      
         ASSIGN
         SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(slutsum.TIMMAR,">>>>>9")                                                          
         SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(slutsum.BTIMMAR,">>>>>9").                     
         IF kosttemp.BELOPP < 0                                                             
         THEN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(slutsum.BELOPP + kosttemp.BELOPP,"->>>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(slutsum.BELOPP + kosttemp.BELOPP,">>>>>>>9").
         IF kosttemp.GRAVARE < 0 
         THEN SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(kosttemp.GRAVARE,"->>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(kosttemp.GRAVARE,">>>>>>9").
         IF kosttemp.MASKOVRIG < 0 
         THEN SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(kosttemp.MASKOVRIG,"->>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(kosttemp.MASKOVRIG,">>>>>>9").
         IF kosttemp.MTRL < 0 
         THEN SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(kosttemp.MTRL,"->>>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(kosttemp.MTRL,">>>>>>>9").
         summakostvar = slutsum.BELOPP + kosttemp.MTRL + kosttemp.GRAVARE + 
         kosttemp.MASKOVRIG + kosttemp.OVRKR + kosttemp.BELOPP.
         IF summakostvar > 0 THEN DO:
            SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(summakostvar,">>>>>>>9").                       
         END.
         ELSE DO:           
            SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(summakostvar,"->>>>>>9").            
         END.
         ASSIGN
         SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(kosttemp.OVRKR,"->>>>>9").
         IF kosttemp.INKOMST > 0 THEN
         SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(kosttemp.INKOMST,">>>>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(kosttemp.INKOMST,"->>>>>>9").
         hjalpvar = kosttemp.INKOMST - (slutsum.BELOPP + kosttemp.MTRL + kosttemp.GRAVARE + 
         kosttemp.MASKOVRIG + kosttemp.OVRKR + kosttemp.BELOPP).
         IF hjalpvar > 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(hjalpvar,">>>>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(hjalpvar,"->>>>>>9").         
         IF varin - varkalk > 0 THEN
         SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING(varin - varkalk,"->>>>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING(varin - varkalk,"->>>>>>>9").         
         IF AVAILABLE valdaao THEN DO:
            RUN pnamn_UI.                       
         END.         
         FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "UF" NO-ERROR.
         ASSIGN
         sumsum.TIMMAR =  sumsum.TIMMAR  + slutsum.TIMMAR                                                      
         sumsum.BTIMMAR = sumsum.BTIMMAR + slutsum.BTIMMAR
         sumsum.OTIMMAR = sumsum.OTIMMAR + slutsum.OTIMMAR
         sumsum.TINKOMST = sumsum.TINKOMST + (kosttemp.INKOMST - 
         (slutsum.BELOPP + kosttemp.MTRL + kosttemp.GRAVARE + kosttemp.MASKOVRIG + 
         kosttemp.OVRKR + kosttemp.BELOPP))
         sumsum.BELOPP = sumsum.BELOPP + slutsum.BELOPP + kosttemp.BELOPP
         sumsum.SKOSTNAD = sumsum.SKOSTNAD + slutsum.BELOPP + kosttemp.MTRL + kosttemp.GRAVARE + kosttemp.MASKOVRIG + 
         kosttemp.OVRKR + kosttemp.BELOPP
         sumsum.MTRL = sumsum.MTRL + kosttemp.MTRL
         sumsum.GRAVARE = sumsum.GRAVARE + kosttemp.GRAVARE
         sumsum.MASKOVRIG = sumsum.MASKOVRIG + kosttemp.MASKOVRIG
         sumsum.OVRKR = sumsum.OVRKR + kosttemp.OVRKR                                                     
         sumsum.INKOMST = sumsum.INKOMST + kosttemp.INKOMST.         
      END.
      FIND FIRST kalksumsum WHERE kalksumsum.AONR = slutsum.AONR AND
      kalksumsum.DELNR = slutsum.DELNR USE-INDEX AONR NO-ERROR.
      IF NOT AVAILABLE kalksumsum THEN DO:
         CREATE kalksumsum.
         ASSIGN
         kalksumsum.AONR = slutsum.AONR 
         kalksumsum.DELNR = slutsum.DELNR.
      END.
      IF AVAILABLE kalksumsum THEN DO:
         /*ANTAL MINUS*/
         CREATE tidut.                        
         kalktotvar = kalksumsum.BELOPP + kalksumsum.MBELOPP + 
         kalksumsum.MTRL + kalksumsum.OVRKR + kalksumsum.MASKGBELOPP + kalksumsum.MONTBELOPP + kalksumsum.BERBELOPP.
         ASSIGN                                              
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "KA".                    
         IF kalksumsum.MONTTIMMAR >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(kalksumsum.MONTTIMMAR,">>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(kalksumsum.MONTTIMMAR,"->>>>9").
         IF kalksumsum.BTIMMAR >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(kalksumsum.BTIMMAR,">>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(kalksumsum.BTIMMAR,"->>>>9").
         IF kalksumsum.BELOPP + kalksumsum.MONTBELOPP + kalksumsum.BERBELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(kalksumsum.BELOPP + kalksumsum.MONTBELOPP + kalksumsum.BERBELOPP,">>>>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(kalksumsum.BELOPP + kalksumsum.MONTBELOPP + kalksumsum.BERBELOPP,"->>>>>>9").
         IF kalksumsum.MASKGBELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(kalksumsum.MASKGBELOPP,">>>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(kalksumsum.MASKGBELOPP,"->>>>>9").
         IF kalksumsum.MBELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(kalksumsum.MBELOPP,">>>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(kalksumsum.MBELOPP,"->>>>>9").
         SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(kalksumsum.MTRL,"->>>>>>9").
         SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(kalksumsum.OVRKR,"->>>>>9").
         SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(kalktotvar,"->>>>>>9").         
         
         /*ANTAL MINUS*/
         IF kalksumsum.TYP = 6 THEN DO:
            IF kalksumsum.TIMMAR >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(kalksumsum.TIMMAR,">>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(kalksumsum.TIMMAR,"->>>>9").
         
         END.
         ELSE DO:
            ASSIGN
            SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(0,">>>>>9").
            IF kalksumsum.TIMMAR >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(kalksumsum.TIMMAR,">>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(kalksumsum.TIMMAR,"->>>>9").
            kalksumsum.MONTTIMMAR = kalksumsum.TIMMAR.
            kalksumsum.TIMMAR = 0.
         END.
         
         FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "KA" NO-ERROR.
         ASSIGN
         sumsum.MONTTIMMAR = sumsum.MONTTIMMAR + kalksumsum.MONTTIMMAR
         sumsum.TIMMAR = sumsum.TIMMAR + kalksumsum.TIMMAR
         sumsum.BTIMMAR = sumsum.BTIMMAR + kalksumsum.BTIMMAR
         sumsum.BELOPP = sumsum.BELOPP + kalksumsum.BELOPP + kalksumsum.MONTBELOPP + kalksumsum.BERBELOPP
         sumsum.MASKGBELOPP = sumsum.MASKGBELOPP + kalksumsum.MASKGBELOPP
         sumsum.MBELOPP = sumsum.MBELOPP + kalksumsum.MBELOPP
         sumsum.MTRL = sumsum.MTRL + kalksumsum.MTRL
         sumsum.OVRKR = sumsum.OVRKR + kalksumsum.OVRKR
         sumsum.SKOSTNAD = sumsum.SKOSTNAD + 
         (kalksumsum.BELOPP + kalksumsum.MONTBELOPP + kalksumsum.BERBELOPP + 
         kalksumsum.MBELOPP + kalksumsum.MASKGBELOPP +
         kalksumsum.MTRL + kalksumsum.OVRKR).                  
      END.
      CREATE tidut.            
   END.   
   IF ingakostver = FALSE THEN  DO:     
      CREATE tidut. 
      CREATE tidut.      
      CREATE tidut. 
      CREATE tidut.
      ASSIGN tidut.UT = "VERIFIKAT FRÅN KOSTNADSREGISTRERING".              
      CREATE tidut.         
      CREATE tidut.
      ASSIGN              
      SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok)         
      SUBSTRING(tidut.UT,12) = "KONTO"                           
      SUBSTRING(tidut.UT,18) = "VER-NR"         
      SUBSTRING(tidut.UT,34) = "DATUM"  
      SUBSTRING(tidut.UT,43) = "BENÄMNING"
      SUBSTRING(tidut.UT,86) = "BELOPP".  
      CREATE tidut.             
      ASSIGN  
      tidut.UT =                                    
"==========.=====.===============.========.==========================================.==========".        
      FOR EACH kosttemp2 NO-LOCK:         
         CREATE tidut.
         ASSIGN                   
         SUBSTRING(tidut.UT,1) = kosttemp2.AONR + STRING(kosttemp2.DELNR,Guru.Konstanter:varforetypchar[1])
         SUBSTRING(tidut.UT,12,5) = SUBSTRING(kosttemp2.BOKKONTO,1,5) 
         SUBSTRING(tidut.UT,18) = kosttemp2.FAKTNR         
         SUBSTRING(tidut.UT,34) = STRING(kosttemp2.REGDATUM)
         SUBSTRING(tidut.UT,43) = kosttemp2.BENAMNING
         SUBSTRING(tidut.UT,86) = STRING((kosttemp2.MTRL + kosttemp2.OVRKR + kosttemp2.GRAVARE +
         kosttemp2.MASKOVRIG + 
         kosttemp2.PERSKOST + kosttemp2.PERSKOST2 + kosttemp2.TRAKTKOST) - kosttemp2.INKOMST,"->>>>>>>>9").                           
      END.
   END.
   ELSE DO:
      FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "UF" NO-ERROR.
      CREATE tidut.
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "SUMMA".      
      ASSIGN       
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "UF"                     
      SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(sumsum.TIMMAR,">>>>>9")    
      SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(sumsum.BTIMMAR,">>>>>9") 
      SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(sumsum.OTIMMAR,">>>>>9")   
      SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(sumsum.TINKOMST,"->>>>>>9").
      IF vartot > 0 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING(vartot,"->>>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING(vartot,"->>>>>>>9").
      IF sumsum.BELOPP < 0                                            
      THEN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(sumsum.BELOPP,"->>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(sumsum.BELOPP,">>>>>>>9").
      IF sumsum.GRAVARE < 0 
      THEN SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(sumsum.GRAVARE,"->>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(sumsum.GRAVARE,">>>>>>9").
      IF sumsum.MASKOVRIG < 0 
      THEN SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(sumsum.MASKOVRIG,"->>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(sumsum.MASKOVRIG,">>>>>>9").
      IF sumsum.MTRL < 0 
      THEN SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(sumsum.MTRL,"->>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(sumsum.MTRL,">>>>>>>9").
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(sumsum.OVRKR,"->>>>>9")                                                                  
      SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(sumsum.SKOSTNAD,"->>>>>>9").
      IF sumsum.INKOMST > 0 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(sumsum.INKOMST,">>>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(sumsum.INKOMST,"->>>>>>9").                  
      /*ANTAL MINUS*/
      FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "KA" NO-ERROR.
      CREATE tidut.
      ASSIGN                 
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "KA".            
      /*ccc*/
      IF sumsum.MONTTIMMAR >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(sumsum.MONTTIMMAR,">>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(sumsum.MONTTIMMAR,"->>>>9").
      IF sumsum.BTIMMAR >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(sumsum.BTIMMAR,">>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(sumsum.BTIMMAR,"->>>>9").
      IF sumsum.TIMMAR >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(sumsum.TIMMAR,">>>>>9").                                                                 
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(sumsum.TIMMAR,"->>>>9").                                                                 
      IF sumsum.BELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(sumsum.BELOPP,">>>>>>>9").                                                                       
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(sumsum.BELOPP,"->>>>>>9").                                                                       
      IF sumsum.MASKGBELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(sumsum.MASKGBELOPP,">>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(sumsum.MASKGBELOPP,"->>>>>9").
      IF sumsum.MBELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(sumsum.MBELOPP,">>>>>>9").             
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(sumsum.MBELOPP,"->>>>>9").             
      
      SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(sumsum.OVRKR,"->>>>>9").
      SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(sumsum.SKOSTNAD,"->>>>>>9").

      IF sumsum.MTRL > 0 THEN                                        
      SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(sumsum.MTRL,">>>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(sumsum.MTRL,"->>>>>>9").               
   END.
END PROCEDURE.

PROCEDURE kalkstart_UI :
    
END PROCEDURE.

PROCEDURE kalkyler_UI :   
   DEFINE VARIABLE dk AS LOGICAL NO-UNDO.
   dk = uppvaltemp.DELNRKOLL.   
   IF dk = TRUE THEN DO:
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = valdaao.AONR AND
      AONRTAB.DELNR = valdaao.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.
      /*KALKRUTIN*/
      FIND FIRST KALKAONR WHERE KALKAONR.AONR = valdaao.AONR AND
      KALKAONR.DELNR = valdaao.DELNR AND KALKAONR.STATUSNIV = "UF"
      USE-INDEX AONR NO-LOCK NO-ERROR.
      IF AVAILABLE KALKAONR THEN DO:
         /*KALKÅR*/
         RUN kalkarstart_UI (INPUT KALKAONR.KALKNR).
         IF KALKAONR.TYP = 6 THEN RUN frikalk_UI.
         ELSE RUN fastkalk_UI.
      END.
      ELSE DO:
         FIND FIRST KALKAONR WHERE KALKAONR.AONR = valdaao.AONR AND
         KALKAONR.DELNR = valdaao.DELNR 
         USE-INDEX AONR NO-LOCK NO-ERROR.
         IF AVAILABLE KALKAONR THEN DO:
            /*KALKÅR*/
            RUN kalkarstart_UI (INPUT KALKAONR.KALKNR).
            IF KALKAONR.TYP = 6 THEN RUN frikalk_UI.
            ELSE RUN fastkalk_UI.
         END.
         
      END.
   END.
   ELSE DO:
      FOR EACH AONRTAB WHERE AONRTAB.AONR = valdaao.AONR 
      USE-INDEX AONR NO-LOCK:
      /*KALKRUTIN*/
         FIND FIRST KALKAONR WHERE KALKAONR.AONR = AONRTAB.AONR AND
         KALKAONR.DELNR = AONRTAB.DELNR AND KALKAONR.STATUSNIV = "UF"
         USE-INDEX AONR NO-LOCK NO-ERROR.
         IF AVAILABLE KALKAONR THEN DO:
            /*KALKÅR*/
            RUN kalkarstart_UI (INPUT KALKAONR.KALKNR).
            IF KALKAONR.TYP = 6 THEN RUN frikalk_UI.
            ELSE RUN fastkalk_UI.
         END.
         ELSE DO:
            FIND FIRST KALKAONR WHERE KALKAONR.AONR = AONRTAB.AONR AND
            KALKAONR.DELNR = AONRTAB.DELNR 
            USE-INDEX AONR NO-LOCK NO-ERROR.
            IF AVAILABLE KALKAONR THEN DO:
               /*KALKÅR*/
               RUN kalkarstart_UI (INPUT KALKAONR.KALKNR).
               IF KALKAONR.TYP = 6 THEN RUN frikalk_UI.
               ELSE RUN fastkalk_UI.
            END.
         END.
      END.
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
            
      DO:
         ASSIGN
         /*kosttemp2.MASKOVRIG = KOSTREG.MASKKOST*/
         kosttemp2.MTRL = KOSTREG.MTRL 
         kosttemp2.OVRKR = KOSTREG.OVRKR + KOSTREG.MASKKOST 
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
      IF SUMTIDDAG.PERSMASK = FALSE THEN musz = musz.
      ELSE IF SUMTIDDAG.PRISTYP = "RESTID..." THEN DO:          
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
         END.
         ELSE DO:
            /*"EJ DEBI.."*/
            IF SUMTIDDAG.PRISTYP = "BEREDARE." THEN DO: 
               dagtemp.BTIMMAR = SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.    
            END.
            ELSE IF SUBSTRING(SUMTIDDAG.BEFATTNING,1,20) = "BEREDARE" OR SUBSTRING(SUMTIDDAG.BEFATTNING,1,20) = "TJÄNSTEMAN" OR SUBSTRING(SUMTIDDAG.BEFATTNING,1,20) = "TJÄNSTEMÄN" THEN DO:
               dagtemp.BTIMMAR = SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
            END.
            ELSE IF SUBSTRING(SUMTIDDAG.BEFATTNING,1,20) = "MONTÖR" THEN DO:
               dagtemp.TIMMAR = SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
            END.
            ELSE IF SUBSTRING(SUMTIDDAG.BEFATTNING,1,20) = "AVLÄSARE" THEN DO:
               dagtemp.TIMMAR = SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
            END.
            ELSE DO:
               dagtemp.OTIMMAR = SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
            END.
         END.
      END.
      GET NEXT stq NO-LOCK.      
   END.
END PROCEDURE.
PROCEDURE summa_UI.
   /*PERSONER*/
   FOR EACH dagtemp WHERE dagtemp.PERSMASK = TRUE NO-LOCK 
   BREAK BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK: 
      ACCUMULATE dagtemp.BELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.TIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.OTIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.BTIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK).        
      ACCUMULATE dagtemp.LONKOST (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK).        
      IF LAST-OF(dagtemp.PERSMASK) THEN DO:
         CREATE slutsum.
         ASSIGN 
         slutsum.AONR = dagtemp.AONR
         slutsum.DELNR = dagtemp.DELNR
         slutsum.ORT = dagtemp.ORT
         slutsum.BELOPP = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.BELOPP)                      
         slutsum.TIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.TIMMAR) 
         slutsum.OTIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.OTIMMAR)
         slutsum.BTIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.BTIMMAR).             
         slutsum.LONKOST = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.LONKOST).             
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
         kosttemp.INKOMST = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.INKOMST)         
         kosttemp.BELOPP = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.PERSKOST). 
                  
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
PROCEDURE frikalk_UI:
   {FRIKALKKOLL.I}
   OPEN QUERY kalkspecq FOR EACH KALKSPEC WHERE KALKSPEC.KALKNR = KALKAONR.KALKNR NO-LOCK. 
   GET FIRST kalkspecq NO-LOCK.
   DO WHILE AVAILABLE(KALKSPEC):
      FIND FIRST kalksumsum WHERE kalksumsum.AONR = valdaao.AONR AND
      kalksumsum.DELNR = valdaao.DELNR NO-ERROR.
      IF NOT AVAILABLE kalksumsum THEN DO:
         CREATE kalksumsum.  
         ASSIGN
         kalksumsum.TYP = 6
         kalksumsum.AONR = valdaao.AONR 
         kalksumsum.DELNR = valdaao.DELNR.
      END.
      OPEN QUERY kalkq
      FOR EACH KALKYL WHERE KALKYL.RECKALKYL = KALKSPEC.KALKNR AND 
      KALKYL.TYP BEGINS "PERS"
      NO-LOCK.  
      GET FIRST kalkq NO-LOCK.
      DO WHILE AVAILABLE(KALKYL):
         /*CCC*/
         IF KALKYL.TYP = "PERS-M" THEN DO:
            ASSIGN
            kalksumsum.MONTBELOPP = kalksumsum.MONTBELOPP + 
            (KALKYL.TIMMAR * KALKYL.PRIS) + (KALKYL.OT50 * KALKYL.OT100)          
            kalksumsum.MONTTIMMAR = kalksumsum.MONTTIMMAR + 
            KALKYL.TIMMAR + KALKYL.OT50.
         END.
         ELSE IF KALKYL.TYP = "PERS-B" THEN DO:             
            ASSIGN
            kalksumsum.BERBELOPP = kalksumsum.BERBELOPP + 
            KALKYL.TIMMAR * KALKYL.PRIS +  KALKYL.OT50 * KALKYL.OT100          
            kalksumsum.BTIMMAR = kalksumsum.BTIMMAR + 
            KALKYL.TIMMAR + KALKYL.OT50.           
         END.
         ELSE DO: 
            ASSIGN
            kalksumsum.BELOPP = kalksumsum.BELOPP + 
            KALKYL.TIMMAR * KALKYL.PRIS +  KALKYL.OT50 * KALKYL.OT100          
            kalksumsum.TIMMAR = kalksumsum.TIMMAR + 
            KALKYL.TIMMAR + KALKYL.OT50.           
         END.
         GET NEXT kalkq NO-LOCK.
      END.              
      CLOSE QUERY kalkq.           
      
      OPEN QUERY kalkq   
      FOR EACH KALKYL WHERE KALKYL.RECKALKYL = KALKSPEC.KALKNR AND 
      KALKYL.TYP = "TRAKT"
      USE-INDEX RADNR NO-LOCK.   
      GET FIRST kalkq NO-LOCK.
      DO WHILE AVAILABLE(KALKYL):
          kalksumsum.TBELOPP = kalksumsum.TBELOPP + 1.0 * KALKYL.PRIS * KALKYL.TIMMAR.                                      
         GET NEXT kalkq NO-LOCK.   
      END.       
      OPEN QUERY kalkq   
      FOR EACH KALKYL WHERE KALKYL.RECKALKYL = KALKSPEC.KALKNR AND 
      KALKYL.TYP = "MIL"
      USE-INDEX RADNR NO-LOCK.   
      GET FIRST kalkq NO-LOCK.
      DO WHILE AVAILABLE(KALKYL):
         kalksumsum.LONKOST = kalksumsum.LONKOST + 1.0 * KALKYL.PRIS * KALKYL.TIMMAR.                                      
         GET NEXT kalkq NO-LOCK.   
      END.       
      OPEN QUERY kalkq
      FOR EACH KALKYL WHERE KALKYL.RECKALKYL = KALKSPEC.KALKNR AND 
      KALKYL.TYP BEGINS "MASK"
      USE-INDEX RECKALKYL NO-LOCK.
      GET FIRST kalkq NO-LOCK.            
      DO WHILE AVAILABLE(KALKYL):                               
         IF KALKYL.TYP = "MASK-G" THEN DO:
            ASSIGN
            kalksumsum.MASKGBELOPP = kalksumsum.MASKGBELOPP + 
            KALKYL.OT75.          
         END.
         ELSE DO:
            ASSIGN
            kalksumsum.MBELOPP = kalksumsum.MBELOPP + 
            KALKYL.OT75.                  
         END.
         GET NEXT kalkq NO-LOCK.
      END.                                 
      CLOSE QUERY kalkq.
      OPEN QUERY kalkq 
      FOR EACH KALKYL WHERE KALKYL.RECKALKYL = KALKSPEC.KALKNR AND 
      KALKYL.TYP = "MATRL"
      USE-INDEX RADNR NO-LOCK.
      GET FIRST kalkq NO-LOCK.
      DO WHILE AVAILABLE(KALKYL): 
         kalksumsum.MTRL = kalksumsum.MTRL + KALKYL.PRIS * KALKYL.TIMMAR. 
         GET NEXT kalkq NO-LOCK.         
      END.                                  
      CLOSE QUERY kalkq.      
      OPEN QUERY kalkq
      FOR EACH KALKYL WHERE KALKYL.RECKALKYL = KALKSPEC.KALKNR AND 
      KALKYL.TYP = "OVER"
      USE-INDEX RADNR NO-LOCK.  
      GET FIRST kalkq NO-LOCK.
      DO WHILE AVAILABLE(KALKYL):
         kalksumsum.OVRKR = kalksumsum.OVRKR + KALKYL.PRIS * KALKYL.TIMMAR. 
         GET NEXT kalkq NO-LOCK.         
      END.
      GET NEXT kalkspecq NO-LOCK.
   END.                     
END PROCEDURE.
PROCEDURE fastkalk_UI :
   EMPTY TEMP-TABLE kalk_temp NO-ERROR.    
   FIND FIRST FASTSPEC WHERE FASTSPEC.KALKNR = KALKAONR.KALKNR AND
   FASTSPEC.TYP = 3 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FASTSPEC THEN DO: 
      musz = musz.
   END. 
   ELSE DO:      
      IF globforetag = "ELPA" OR globforetag = "GRAN" OR globforetag = "GKAL" OR globforetag = "LULE" THEN DO:
         FIND FIRST KALKUPP WHERE KALKUPP.KALKNR = FASTSPEC.KALKNR
         USE-INDEX KALKNR NO-LOCK NO-ERROR.
         IF AVAILABLE KALKUPP THEN xtypmtrl = KALKUPP.TYP.
      END.
      ELSE DO:
         IF FASTSPEC.EGETMTRL = TRUE THEN xtypmtrl = 2.
         ELSE xtypmtrl = 1.
      END.
      RUN rakna_UI.
      RUN klar_UI.
      FIND FIRST kalksumsum WHERE kalksumsum.AONR = valdaao.AONR AND
      kalksumsum.DELNR = valdaao.DELNR NO-ERROR.
      IF NOT AVAILABLE kalksumsum THEN DO:
         CREATE kalksumsum.  
         ASSIGN
         kalksumsum.AONR = valdaao.AONR 
         kalksumsum.DELNR = valdaao.DELNR.
      END.
      ASSIGN   
      kalksumsum.TIMMAR = kalksumsum.TIMMAR + arbtim - bertim
      kalksumsum.BELOPP = kalksumsum.BELOPP + totarb                       
      kalksumsum.BTIMMAR = kalksumsum.BTIMMAR + bertim
      kalksumsum.MASKGBELOPP = kalksumsum.MASKGBELOPP + totmask + totutr
      /*kalksumsum.MBELOPP = kalksumsum.MBELOPP + totmask + totutr*/
      kalksumsum.MTRL = kalksumsum.MTRL + totmtrl 
      kalksumsum.OVRKR = kalksumsum.OVRKR + totovr.
      
      RETURN.
   END.   
   FIND FIRST FASTSPEC WHERE FASTSPEC.KALKNR = KALKAONR.KALKNR AND 
   FASTSPEC.TYP = 2 
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FASTSPEC THEN DO: 
      FIND FIRST FASTSPEC WHERE FASTSPEC.KALKNR = KALKAONR.KALKNR AND
      FASTSPEC.TYP = 1 NO-LOCK NO-ERROR.
      IF AVAILABLE FASTSPEC THEN DO:
         IF globforetag = "ELPA" OR globforetag = "GRAN" OR globforetag = "GKAL" OR globforetag = "LULE" THEN DO:
            FIND FIRST KALKUPP WHERE KALKUPP.KALKNR = FASTSPEC.KALKNR
            USE-INDEX KALKNR NO-LOCK NO-ERROR.
            IF AVAILABLE KALKUPP THEN xtypmtrl = KALKUPP.TYP.            
         END.
         ELSE DO:
            IF FASTSPEC.EGETMTRL = TRUE THEN xtypmtrl = 2.
            ELSE xtypmtrl = 1.
         END.
         RUN rakna_UI.
         RUN klar_UI.
      END.
   END. 
   ELSE DO:  
      IF globforetag = "ELPA" OR globforetag = "GRAN" OR globforetag = "GKAL" OR globforetag = "LULE" THEN DO:
         FIND FIRST KALKUPP WHERE KALKUPP.KALKNR = FASTSPEC.KALKNR
         USE-INDEX KALKNR NO-LOCK NO-ERROR.
         IF AVAILABLE KALKUPP THEN xtypmtrl = KALKUPP.TYP.
      END.
      ELSE DO:
         IF FASTSPEC.EGETMTRL = TRUE THEN xtypmtrl = 2.
         ELSE xtypmtrl = 1.
      END.
      RUN rakna_UI.
      RUN klar_UI.
   END.  
   FIND FIRST kalksumsum WHERE kalksumsum.AONR = valdaao.AONR AND
   kalksumsum.DELNR = valdaao.DELNR NO-ERROR.
   IF NOT AVAILABLE kalksumsum THEN DO:
      CREATE kalksumsum.  
      ASSIGN
      kalksumsum.AONR = valdaao.AONR 
      kalksumsum.DELNR = valdaao.DELNR.
   END.
   ASSIGN   
   kalksumsum.TIMMAR = kalksumsum.TIMMAR + arbtim - bertim
   kalksumsum.BELOPP = kalksumsum.BELOPP + totarb                       
   kalksumsum.BTIMMAR = kalksumsum.BTIMMAR + bertim
   kalksumsum.MASKGBELOPP = kalksumsum.MASKGBELOPP + totmask + totutr
   /*kalksumsum.MBELOPP = kalksumsum.MBELOPP + totmask + totutr*/
   kalksumsum.MTRL = kalksumsum.MTRL + totmtrl 
   kalksumsum.OVRKR = kalksumsum.OVRKR + totovr. 
         
END PROCEDURE.


PROCEDURE klar_UI :
   {KLAR.I}      
   kontroll = FALSE.
   IF xtypmtrl = 2 THEN DO:   
      IF FASTSPEC.EGETMTRL = TRUE THEN DO:
         {MTRLKAL.I}                           
      END.         
   END.
   IF xtypmtrl = 3 THEN DO:      
      {BERKALKSTART1.I}
      {BERKALK2.I}                  
   END.
   {BERKALKSTART2.I}
   {BERKALK2.I}
   {BERKALKSLUT.I}
END PROCEDURE.
PROCEDURE rakna_UI :
   /*GG 2007-10-30 Om blandtyp, hämta katalogår för båda så att monpris laddas rätt vid uträkning*/
   DEFINE VARIABLE artalnum AS INTEGER NO-UNDO.
   DEFINE VARIABLE artalsam AS INTEGER NO-UNDO.
   DEFINE VARIABLE ebrmonpris AS DECIMAL NO-UNDO.
   DEFINE VARIABLE sebrmonpris AS DECIMAL NO-UNDO.
   
   /*KALKÅR*/
   IF varforetypval[35] = 1 THEN DO:
      RUN raknakalar_UI.
      RETURN.
   END.
   RUN procset_UI.   
   /*RUN EXTRADATAHMT.P PERSISTENT SET edataapph.*/
   /*KALKÅR*/
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "DOKALKYL"                   
   inextradatatemp.HUVUDCH = FASTSPEC.OMRADE
   inextradatatemp.HUVUDINT = FASTSPEC.KALKNR.        
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph NO-ERROR.
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
   IF AVAILABLE extradatatemp THEN DO:
      ASSIGN
      artalnum = extradatatemp.SOKINT[1]
      artalsam = extradatatemp.SOKINT[2].
   END.
   ELSE DO:
      ASSIGN
      artalnum = ?
      artalsam = ?.
   END.
   IF artalnum = ? THEN FIND FIRST EBRPRIS WHERE EBRPRIS.ARTAL = FASTSPEC.KATAR USE-INDEX AR NO-LOCK NO-ERROR.
   ELSE FIND FIRST EBRPRIS WHERE EBRPRIS.ARTAL = artalnum USE-INDEX AR NO-LOCK NO-ERROR.
   IF AVAILABLE EBRPRIS THEN ebrmonpris = EBRPRIS.MONT.
   IF artalsam = ? THEN FIND FIRST SEBRPRIS WHERE SEBRPRIS.ARTAL = FASTSPEC.KATAR USE-INDEX AR NO-LOCK NO-ERROR.
   ELSE FIND FIRST SEBRPRIS WHERE SEBRPRIS.ARTAL = artalsam USE-INDEX AR NO-LOCK NO-ERROR.
   IF AVAILABLE SEBRPRIS THEN sebrmonpris = SEBRPRIS.MONT.
   /*Slut blandtyp */
/*    FIND FIRST EBRPRIS WHERE EBRPRIS.ARTAL = FASTSPEC.KATAR      */
/*    USE-INDEX AR NO-LOCK NO-ERROR.                               */
/*    monpris = EBRPRIS.MONT.                                      */
/*    IF AVAILABLE FASTSPEC AND FASTSPEC.TYP = 5 THEN DO:          */
/*       FIND FIRST SEBRPRIS WHERE SEBRPRIS.ARTAL = FASTSPEC.KATAR */
/*       USE-INDEX AR NO-LOCK NO-ERROR.                            */
/*       monpris = SEBRPRIS.MONT.                                  */
/*    END.                                                         */
   IF FASTSPEC.FAKTOR = TRUE THEN DO:
      FIND FIRST FAKTOR WHERE FAKTOR.KALKNR = FASTSPEC.KALKNR AND
      FAKTOR.OMRADE = FASTSPEC.OMRADE USE-INDEX OMRADE NO-LOCK NO-ERROR.               
   END.
   IF FASTSPEC.EGETPRIS = TRUE THEN DO:
      FIND FIRST KALKBEF WHERE KALKBEF.KALKNR = FASTSPEC.KALKNR AND
      KALKBEF.OMRADE = FASTSPEC.OMRADE AND KALKBEF.ARBKOD = "" AND
      KALKBEF.LOPNR = 0 USE-INDEX OMRADE NO-LOCK NO-ERROR.          
   END.
   ELSE DO:
      FIND FIRST KALKBEF WHERE KALKBEF.KALKNR = 0 AND 
      KALKBEF.KATAR = FASTSPEC.KATAR
      USE-INDEX ARTAL NO-LOCK NO-ERROR.
   END.
   IF NOT AVAILABLE KALKBEF THEN DO:
      FIND FIRST KALKBEF WHERE KALKBEF.KALKNR = 0 AND 
      KALKBEF.KATAR = YEAR(TODAY)
      USE-INDEX ARTAL NO-LOCK NO-ERROR.      
   END.
   fastrec = RECID(FASTSPEC).
  /* RUN SUCCKALK.P.*/
   OPEN QUERY fastq FOR EACH FASTKALK WHERE FASTKALK.KALKNR = FASTSPEC.KALKNR 
   AND FASTKALK.OMRADE = FASTSPEC.OMRADE USE-INDEX OMRADE NO-LOCK.
   GET FIRST fastq NO-LOCK.
   DO WHILE AVAILABLE(FASTKALK):
      IF FASTKALK.TYP = 5 THEN monpris = sebrmonpris. 
      ELSE monpris = ebrmonpris.
      IF FASTKALK.ARBKOD = "EGEN" THEN DO:
         FIND FIRST befbuff WHERE befbuff.KALKNR = FASTSPEC.KALKNR AND
         befbuff.OMRADE = FASTSPEC.OMRADE AND befbuff.ARBKOD = FASTKALK.ARBKOD AND
         befbuff.LOPNR = FASTKALK.LOPNR USE-INDEX OMRADE NO-LOCK NO-ERROR. 
         RUN skapa2_UI.
      END.
      ELSE IF FASTKALK.ARBKOD = "SUCC" THEN DO:
         RUN skapa3_UI.
      END.
      ELSE DO:
         IF FASTKALK.FAST = FALSE THEN RUN skapa_UI.
         ELSE RUN skapa4_UI.
      END.
      GET NEXT fastq NO-LOCK.
   END.
   CLOSE QUERY fastq.   
END PROCEDURE.
PROCEDURE raknakalar_UI :
   IF FASTSPEC.FAKTOR = TRUE THEN DO:
      FIND FIRST FAKTOR WHERE FAKTOR.KALKNR = FASTSPEC.KALKNR AND
      FAKTOR.OMRADE = FASTSPEC.OMRADE USE-INDEX OMRADE NO-LOCK NO-ERROR.               
   END.
   IF FASTSPEC.EGETPRIS = TRUE THEN DO:
      FIND FIRST KALKBEF WHERE KALKBEF.KALKNR = FASTSPEC.KALKNR AND
      KALKBEF.OMRADE = FASTSPEC.OMRADE AND KALKBEF.ARBKOD = "" AND
      KALKBEF.LOPNR = 0 USE-INDEX OMRADE NO-LOCK NO-ERROR.          
   END.
   ELSE DO:
      FIND FIRST KALKBEF WHERE KALKBEF.KALKNR = 0 AND KALKBEF.KATAR = hjkatvar[1]
      USE-INDEX ARTAL NO-LOCK NO-ERROR.
   END.
   IF NOT AVAILABLE KALKBEF THEN DO:
      FIND FIRST KALKBEF WHERE KALKBEF.KALKNR = 0 AND 
      KALKBEF.KATAR = YEAR(TODAY)
      USE-INDEX ARTAL NO-LOCK NO-ERROR.      
   END.
   fastrec = RECID(FASTSPEC).
   OPEN QUERY fastq FOR EACH FASTKALK WHERE FASTKALK.KALKNR = FASTSPEC.KALKNR 
   AND FASTKALK.OMRADE = FASTSPEC.OMRADE USE-INDEX OMRADE NO-LOCK.
   GET FIRST fastq NO-LOCK.
   DO WHILE AVAILABLE(FASTKALK):
      IF FASTKALK.ARBKOD = "EGEN" THEN DO:
         FIND FIRST befbuff WHERE befbuff.KALKNR = FASTSPEC.KALKNR AND
         befbuff.OMRADE = FASTSPEC.OMRADE AND befbuff.ARBKOD = FASTKALK.ARBKOD AND
         befbuff.LOPNR = FASTKALK.LOPNR USE-INDEX OMRADE NO-LOCK NO-ERROR. 
         RUN skapa2artal_UI.
      END.
      ELSE DO:
         IF FASTKALK.FAST = FALSE THEN RUN skapaartal_UI.
         ELSE RUN skapa4artal_UI.
      END.
      GET NEXT fastq NO-LOCK.
   END.
   CLOSE QUERY fastq.   
END PROCEDURE.
PROCEDURE skapa2_UI :
   {SKAPA2.I}    
END PROCEDURE.

PROCEDURE skapa3_UI :
   {SKAPA3.I}        
END PROCEDURE.

PROCEDURE skapa4_UI :
   {SKAPA4.I}        
END PROCEDURE.
PROCEDURE skapa_UI :
   {SKAPA.I}   
END PROCEDURE.
PROCEDURE skapa2artal_UI : 
                               
END PROCEDURE.

PROCEDURE  :
    
END PROCEDURE.

PROCEDURE skapaartal_UI :
    
END PROCEDURE.
PROCEDURE pnamn_UI:
   ASSIGN
   SUBSTRING(tidut.UT,200) = valdaao.ARBANSVARIG
   SUBSTRING(tidut.UT,220) = valdaao.BEREDARE   
   SUBSTRING(tidut.UT,240) = valdaao.PROJEKTOR.
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valdaao.ARBANSVARIG NO-LOCK NO-ERROR.
   IF AVAILABLE PERSONALTAB THEN SUBSTRING(tidut.UT,200) = CAPS(SUBSTRING(PERSONALTAB.FORNAMN,1,1)) + " " + CAPS(PERSONALTAB.EFTERNAMN).
   ELSE SUBSTRING(tidut.UT,200) = valdaao.ARBANSVARIG.
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valdaao.BEREDARE NO-LOCK NO-ERROR.
   IF AVAILABLE PERSONALTAB THEN SUBSTRING(tidut.UT,220) = CAPS(SUBSTRING(PERSONALTAB.FORNAMN,1,1)) + " " + CAPS(PERSONALTAB.EFTERNAMN).
   ELSE SUBSTRING(tidut.UT,220) = valdaao.BEREDARE.
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valdaao.PROJEKTOR NO-LOCK NO-ERROR.
   IF AVAILABLE PERSONALTAB THEN SUBSTRING(tidut.UT,240) = CAPS(SUBSTRING(PERSONALTAB.FORNAMN,1,1)) + " " + CAPS(PERSONALTAB.EFTERNAMN).
   ELSE SUBSTRING(tidut.UT,240) = valdaao.PROJEKTOR.
END PROCEDURE.

PROCEDURE procset_UI:  
  IF NOT VALID-HANDLE(edataapph) THEN RUN EXTRADATAHMT.P PERSISTENT SET edataapph.            
END PROCEDURE .
