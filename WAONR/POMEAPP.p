/*POMEAPP.P*/
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
{PHMT.I}
{KALKTEMP.I}
FIND FIRST FORETAG NO-LOCK NO-ERROR.
IF FORETAG.FORETAG = "GRIT" THEN DO:
   RUN GRITIN.P PERSISTENT SET superhandle (INPUT 1).
   THIS-PROCEDURE:ADD-SUPER-PROCEDURE (superhandle).
END.
{TIDUTTT.I}


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


DEFINE TEMP-TABLE dagtemp
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD
   FIELD NAMN AS CHARACTER 
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
   FIELD NAMN AS CHARACTER 
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
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD AONR DELNR ASCENDING.    
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
   
DEFINE INPUT PARAMETER TABLE FOR uppvaltemp.
DEFINE INPUT PARAMETER TABLE FOR valperstemp.
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

DEFINE VARIABLE str AS CHARACTER FORMAT "X(256)" NO-UNDO.
DEFINE VARIABLE pekod AS CHARACTER NO-UNDO.
DEFINE VARIABLE aokod AS CHARACTER NO-UNDO.
DEFINE VARIABLE delkod AS INTEGER NO-UNDO.

/*BEREDNING*/
{LISTDEF.I} 
DEFINE NEW SHARED TEMP-TABLE mtrl_temp2   
   {MTRLTEMP2TT.I}
/*SLUT BEREDNING*/

FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR. 
Guru.Konstanter:globforetag = FORETAG.FORETAG.

EMPTY TEMP-TABLE dagtemp  NO-ERROR. 
EMPTY TEMP-TABLE slutsum NO-ERROR. 
EMPTY TEMP-TABLE sumsum NO-ERROR. 
EMPTY TEMP-TABLE tidut NO-ERROR. 

FIND FIRST uppvaltemp NO-ERROR.

OPEN QUERY aq FOR EACH valperstemp NO-LOCK. 
GET FIRST aq NO-LOCK.
GET NEXT aq  NO-LOCK.
IF AVAILABLE valperstemp THEN ingakostver = TRUE.
ELSE ingakostver = FALSE.
IF uppvaltemp.DELNRKOLL = FALSE THEN ingakostver = TRUE.
GET FIRST aq NO-LOCK.
DO WHILE AVAILABLE(valperstemp):      
   {SUPEOPEN.I}   
   GET NEXT aq NO-LOCK.
END.      
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
   nrcol[1] = 2
   nrcol[2] = 3   
   nrcol[4] = 8
   nrcol[7] = 10   
   nrcol[8] = 11      
   nrcol[16] = 4
   nrcol[17] = 5
   nrcol[18] = 6
   nrcol[21] = 7
   nrcol[22] = 9   
   nrcol[28] = 1         
   breddantal = 11   /*antal kolumner*/
   bredd[1] = 20
   bredd[2] = 9
   bredd[3] = 40
   bredd[4] = 6
   bredd[5] = 6
   bredd[6] = 6
   bredd[7] = 6
   bredd[8] = 9
   bredd[9] = 9
   bredd[10] = 10
   bredd[11] = 10. 
   
   ASSIGN
   i = 2.     
   utnr[nrcol[28]] = 1.
   
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
   
   IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
      ASSIGN                                                                                                       
      SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "UTRUST".             
      
   END.
   CREATE tidut.      
   ASSIGN                                  
   SUBSTRING(tidut.UT,utnr[nrcol[28]]) = "Enhet/Sign"
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = CAPS(Guru.Konstanter:gaok)      
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = CAPS(Guru.Konstanter:gaonamnk) 
   SUBSTRING(tidut.UT,utnr[nrcol[16]]) = KBENAMNING.K1             
   SUBSTRING(tidut.UT,utnr[nrcol[17]]) = KBENAMNING.K2         
   SUBSTRING(tidut.UT,utnr[nrcol[18]]) = KBENAMNING.K3   
   SUBSTRING(tidut.UT,utnr[nrcol[21]]) = "%"
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "Normaltid"     
   SUBSTRING(tidut.UT,utnr[nrcol[22]]) = "Övertid"             
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "Kostnad"
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "L-tillägg".                
   IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "KOSTN.".             
      /*SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "KOSTN.".
      SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "KONSULT"                                            
      SUBSTRING(tidut.UT,utnr[nrcol[9]]) = "ÖVRIGT". */
   END.
   /*str3 = tidut.UT.                */
   CREATE tidut.      
   tidut.UT = str.
   CREATE sumsum.
   ASSIGN sumsum.PERSONALKOD = "UF".
   ASSIGN
   pekod = ""
   aokod = ""
   delkod = 0.
   FOR EACH slutsum USE-INDEX PERSONALKOD:
      IF Guru.Konstanter:globforetag = "misv" THEN DO:
         CREATE tidut.                                       
         ASSIGN SUBSTRING(tidut.UT,utnr[nrcol[28]]) = slutsum.PERSONALKOD + " " + SUBSTRING(slutsum.NAMN,1,30).         
         ASSIGN SUBSTRING(tidut.UT,utnr[nrcol[1]]) = slutsum.AONR + STRING(slutsum.DELNR,Guru.Konstanter:varforetypchar[1]).
         ASSIGN
         SUBSTRING(tidut.UT,utnr[nrcol[2]]) = SUBSTRING(slutsum.ORT,1,bredd[3])                                         
         SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(slutsum.NTIMMAR,">>>>>9.99")
         SUBSTRING(tidut.UT,utnr[nrcol[22]]) = STRING(slutsum.OATIMMAR,">>>>>9.99")                                                          
         SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(slutsum.ABELOPP,">>>>>>>9"). 
         SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(slutsum.LONKOST,">>>>>>>9"). 
         ASSIGN
         pekod = slutsum.PERSONALKOD
         aokod = slutsum.AONR
         delkod = slutsum.DELNR.
         FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "UF" NO-ERROR.                   
         ASSIGN  
         sumsum.TIMMAR =  sumsum.TIMMAR  + ( slutsum.TIMMAR)  
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
         sumsum.TINKOMST = sumsum.TINKOMST + (0 - ( slutsum.BELOPP)).
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + slutsum.PERSONALKOD.
      END.
      ELSE DO:    
         FIND FIRST AONRKONTKOD WHERE AONRKONTKOD.AONR = slutsum.AONR AND AONRKONTKOD.DELNR = slutsum.DELNR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE AONRKONTKOD THEN DO:
             CREATE tidut.                                       
             ASSIGN SUBSTRING(tidut.UT,utnr[nrcol[28]]) = slutsum.PERSONALKOD + " " + SUBSTRING(slutsum.NAMN,1,30).         
             ASSIGN SUBSTRING(tidut.UT,utnr[nrcol[1]]) = slutsum.AONR + STRING(slutsum.DELNR,Guru.Konstanter:varforetypchar[1]).
             ASSIGN
             SUBSTRING(tidut.UT,utnr[nrcol[2]]) = SUBSTRING(slutsum.ORT,1,bredd[3])                                         
             SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(slutsum.NTIMMAR,">>>>>9.99")
             SUBSTRING(tidut.UT,utnr[nrcol[22]]) = STRING(slutsum.OATIMMAR,">>>>>9.99")                                                          
             SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(slutsum.ABELOPP,">>>>>>>9"). 
             SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(slutsum.LONKOST,">>>>>>>9"). 
              Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + slutsum.PERSONALKOD.
             ASSIGN
             pekod = slutsum.PERSONALKOD
             aokod = slutsum.AONR
             delkod = slutsum.DELNR.
             FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "UF" NO-ERROR.                   
             ASSIGN  
             sumsum.TIMMAR =  sumsum.TIMMAR  + ( slutsum.TIMMAR)  
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
             sumsum.TINKOMST = sumsum.TINKOMST + (0 - ( slutsum.BELOPP)).
         END.                                                   
         FOR EACH AONRKONTKOD WHERE AONRKONTKOD.AONR = slutsum.AONR AND AONRKONTKOD.DELNR = slutsum.DELNR NO-LOCK:
            CREATE tidut.                                       
            ASSIGN SUBSTRING(tidut.UT,utnr[nrcol[28]]) = slutsum.PERSONALKOD + " " + SUBSTRING(slutsum.NAMN,1,30).         
            ASSIGN SUBSTRING(tidut.UT,utnr[nrcol[1]]) = slutsum.AONR + STRING(slutsum.DELNR,Guru.Konstanter:varforetypchar[1]).
            ASSIGN
            SUBSTRING(tidut.UT,utnr[nrcol[2]]) = SUBSTRING(slutsum.ORT,1,bredd[3])               
            SUBSTRING(tidut.UT,utnr[nrcol[16]]) = AONRKONTKOD.K1
            SUBSTRING(tidut.UT,utnr[nrcol[17]]) = AONRKONTKOD.K2
            SUBSTRING(tidut.UT,utnr[nrcol[18]]) = AONRKONTKOD.K3         
            SUBSTRING(tidut.UT,utnr[nrcol[21]]) = STRING(AONRKONTKOD.SATS%,">>9")
            SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.NTIMMAR,">>>>>9.99")
            SUBSTRING(tidut.UT,utnr[nrcol[22]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.OATIMMAR,">>>>>9.99")                                                          
            SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.ABELOPP,">>>>>>>9"). 
            SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(AONRKONTKOD.SATS% / 100 * slutsum.LONKOST,">>>>>>>9"). 
             Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + slutsum.PERSONALKOD.
            ASSIGN
            pekod = slutsum.PERSONALKOD
            aokod = slutsum.AONR
            delkod = slutsum.DELNR.
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
            sumsum.TINKOMST = sumsum.TINKOMST + (0 - ( slutsum.BELOPP * AONRKONTKOD.SATS% / 100)).
            
         END.
      END.         
   END.

END PROCEDURE.


PROCEDURE kontokoll_UI:
   DEFINE INPUT PARAMETER varkont LIKE KOSTREG.BOKKONTO NO-UNDO.
   DEFINE OUTPUT PARAMETER varvart AS CHARACTER NO-UNDO.
   RUN SUPER (INPUT varkont,OUTPUT varvart).
END PROCEDURE.
PROCEDURE skapadag_UI :   
   GET FIRST sq NO-LOCK.
   DO WHILE AVAILABLE(SUMTID):     
      IF Guru.Konstanter:globforetag = "GKAL" AND SUMTID.PERSMASK = FALSE THEN DO:
         /*Kalmar vill ha med tiskrivning maskiner -ej kostnadsregistreringar*/         
         CREATE dagtemp.
         ASSIGN       
         dagtemp.PERSONALKOD = SUMTID.PERSONALKOD
         dagtemp.NAMN = SUBSTRING(SUMTID.FORNAMN,1,1) + "." + 
         SUBSTRING(SUMTID.EFTERNAMN,1,30)
         dagtemp.AONR = SUMTID.AONR
         dagtemp.DELNR = SUMTID.DELNR 
         dagtemp.ORT = SUMTID.ORT
         dagtemp.PERSMASK = SUMTID.PERSMASK
         dagtemp.BELOPP = SUMTID.BELOPP + SUMTID.OBELOPP
         dagtemp.TIMMAR = SUMTID.TIMMAR + SUMTID.OTIMMAR.
         dagtemp.NTIMMAR = SUMTID.TIMMAR. 
         dagtemp.OATIMMAR = SUMTID.OTIMMAR. 
         dagtemp.ABELOPP = dagtemp.BELOPP.
         dagtemp.BEFATTNING = SUBSTRING(SUMTID.BEFATTNING,1,20).
         dagtemp.LONKOST = SUMTID.LONKOST.
                  
      END.
      ELSE IF SUMTID.PERSMASK = FALSE THEN musz = musz.
      ELSE IF SUMTID.PRISTYP = "RESTID..." THEN DO: 
       
      END.
      ELSE DO:
         CREATE dagtemp.
         ASSIGN         
         dagtemp.PERSONALKOD = SUMTID.PERSONALKOD
         dagtemp.NAMN = SUBSTRING(SUMTID.FORNAMN,1,1) + "." + 
         SUBSTRING(SUMTID.EFTERNAMN,1,30)
         dagtemp.AONR = SUMTID.AONR
         dagtemp.DELNR = SUMTID.DELNR 
         dagtemp.ORT = SUMTID.ORT
         dagtemp.PERSMASK = SUMTID.PERSMASK
         dagtemp.BELOPP = SUMTID.BELOPP + SUMTID.OBELOPP
         dagtemp.TIMMAR = SUMTID.TIMMAR + SUMTID.OTIMMAR.
         dagtemp.NTIMMAR = SUMTID.TIMMAR. 
         dagtemp.OATIMMAR = SUMTID.OTIMMAR. 
         dagtemp.ABELOPP = dagtemp.BELOPP.
         dagtemp.LONKOST = SUMTID.LONKOST.
       
      END.                               
      GET NEXT sq NO-LOCK.      
   END.
   
      
END PROCEDURE.

PROCEDURE skapadagdag_UI :
   GET FIRST stq NO-LOCK.
   DO WHILE AVAILABLE(SUMTIDDAG):  
      IF Guru.Konstanter:globforetag = "GKAL" AND SUMTIDDAG.PERSMASK = FALSE THEN DO:
         /*Kalmar vill ha med tiskrivning maskiner -ej kostnadsregistreringar*/         
         CREATE dagtemp.
         ASSIGN          
         dagtemp.PERSONALKOD = SUMTIDDAG.PERSONALKOD
         dagtemp.NAMN = SUBSTRING(SUMTIDDAG.FORNAMN,1,1) + "." + 
         SUBSTRING(SUMTIDDAG.EFTERNAMN,1,30)
         dagtemp.AONR = SUMTIDDAG.AONR
         dagtemp.DELNR = SUMTIDDAG.DELNR 
         dagtemp.ORT = SUMTIDDAG.ORT
         dagtemp.PERSMASK = SUMTIDDAG.PERSMASK         
         dagtemp.BELOPP = SUMTIDDAG.BELOPP + SUMTIDDAG.OBELOPP
         dagtemp.TIMMAR = SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         dagtemp.NTIMMAR = SUMTIDDAG.TIMMAR. 
         dagtemp.OATIMMAR = SUMTIDDAG.OTIMMAR.    
         dagtemp.ABELOPP = dagtemp.BELOPP.
         dagtemp.BEFATTNING = SUBSTRING(SUMTIDDAG.BEFATTNING,1,20).
         dagtemp.LONKOST = SUMTIDDAG.LONKOST.
                  
      END.
      ELSE IF SUMTIDDAG.PERSMASK = FALSE THEN musz = musz.
      ELSE IF SUMTIDDAG.PRISTYP = "RESTID..." THEN musz = musz.               
      ELSE DO:
         CREATE dagtemp.
         ASSIGN         
         dagtemp.PERSONALKOD = SUMTIDDAG.PERSONALKOD
         dagtemp.NAMN = SUBSTRING(SUMTIDDAG.FORNAMN,1,1) + "." + 
         SUBSTRING(SUMTIDDAG.EFTERNAMN,1,30)
         dagtemp.AONR = SUMTIDDAG.AONR
         dagtemp.DELNR = SUMTIDDAG.DELNR 
         dagtemp.ORT = SUMTIDDAG.ORT
         dagtemp.PERSMASK = SUMTIDDAG.PERSMASK
         dagtemp.BELOPP = SUMTIDDAG.BELOPP + SUMTIDDAG.OBELOPP
         dagtemp.TIMMAR = SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         dagtemp.NTIMMAR = SUMTIDDAG.TIMMAR. 
         dagtemp.OATIMMAR = SUMTIDDAG.OTIMMAR.    
         dagtemp.ABELOPP = dagtemp.BELOPP.
         dagtemp.LONKOST = SUMTIDDAG.LONKOST.
   
      END.
      GET NEXT stq NO-LOCK.      
   END.
   /* arbetstidsförkortning */
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      GET FIRST tq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):        
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD =  TIDREGITAB.PERSONALKOD NO-LOCK NO-ERROR.
         CREATE dagtemp.
         ASSIGN          
         dagtemp.PERSONALKOD = TIDREGITAB.PERSONALKOD
         dagtemp.NAMN = SUBSTRING(PERSONALTAB.FORNAMN,1,1) + "." + 
         SUBSTRING(PERSONALTAB.EFTERNAMN,1,30)
         dagtemp.AONR = "160"
         dagtemp.DELNR = 0            
         dagtemp.PERSMASK = TRUE.
         dagtemp.BELOPP =  0.                  
         dagtemp.TIMMAR = klockan100(TIDREGITAB.LONTILLANTAL).
         dagtemp.NTIMMAR = klockan100(TIDREGITAB.LONTILLANTAL).          
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = TIDREGITAB.AONR AND AONRTAB.DELNR = TIDREGITAB.DELNR NO-LOCK NO-ERROR.
         IF AVAILABLE AONRTAB THEN DO: 
            dagtemp.ORT = AONRTAB.ORT. 
         END.                     
         GET NEXT tq NO-LOCK.      
      END.
   END.
   /*FOR EACH dagtemp WHERE dagtemp.aonr = "160":
      MESSAGE dagtemp.personalkod dagtemp.timmar dagtemp.PERSMASK VIEW-AS ALERT-BOX.
   END.*/
END PROCEDURE.
PROCEDURE summa_UI.
   /*PERSONER*/
   FOR EACH dagtemp NO-LOCK 
   BREAK BY dagtemp.PERSONALKOD BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK: 
      ACCUMULATE dagtemp.BELOPP (TOTAL BY dagtemp.PERSONALKOD BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.TIMMAR (TOTAL BY dagtemp.PERSONALKOD BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.NTIMMAR (TOTAL BY dagtemp.PERSONALKOD BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.OATIMMAR (TOTAL BY dagtemp.PERSONALKOD BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.ABELOPP (TOTAL BY dagtemp.PERSONALKOD BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.OTIMMAR (TOTAL BY dagtemp.PERSONALKOD BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.NOTIMMAR (TOTAL BY dagtemp.PERSONALKOD BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.OOTIMMAR (TOTAL BY dagtemp.PERSONALKOD BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.OBELOPP (TOTAL BY dagtemp.PERSONALKOD BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.BTIMMAR (TOTAL BY dagtemp.PERSONALKOD BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK).        
      ACCUMULATE dagtemp.NBTIMMAR (TOTAL BY dagtemp.PERSONALKOD BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.OBTIMMAR (TOTAL BY dagtemp.PERSONALKOD BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.BBELOPP (TOTAL BY dagtemp.PERSONALKOD BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.LONKOST (TOTAL BY dagtemp.PERSONALKOD BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      IF LAST-OF(dagtemp.PERSMASK) THEN DO:
         CREATE slutsum.
         ASSIGN 
         slutsum.PERSONALKOD = dagtemp.PERSONALKOD
         slutsum.NAMN = dagtemp.NAMN
         slutsum.AONR = dagtemp.AONR
         slutsum.DELNR = dagtemp.DELNR
         slutsum.ORT = dagtemp.ORT          
         slutsum.BELOPP = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.BELOPP)                      
         slutsum.TIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.TIMMAR) 
         slutsum.NTIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.NTIMMAR) 
         slutsum.OATIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.OATIMMAR) 
         slutsum.ABELOPP = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.ABELOPP).                      
         slutsum.LONKOST = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.LONKOST).                      
   
      END.     
   END.
   
END PROCEDURE.

   

