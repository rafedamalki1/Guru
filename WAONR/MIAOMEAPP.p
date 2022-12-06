/*MIAOMEAPP.P*/
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}


DEFINE QUERY stq FOR TIDREGITAB.
DEFINE QUERY tq FOR TIDREGITAB.
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
RUN STYRFORE.P (INPUT Guru.Konstanter:globforetag).
&Scoped-define NEW NEW
{FAKTTYPDEF.I}
&Scoped-define NEW 
{FAKTTYPSKAP.I}

FUNCTION klock100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):

  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.


FUNCTION klock60 RETURNS DECIMAL
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
{DIRDEF.I}
{KALKTEMP.I}
FIND FIRST FORETAG NO-LOCK NO-ERROR.
IF FORETAG.FORETAG = "GRIT" THEN DO:
   RUN GRITIN.P PERSISTENT SET superhandle (INPUT 1).
   THIS-PROCEDURE:ADD-SUPER-PROCEDURE (superhandle).
END.
{TIDUTTT.I}

DEFINE TEMP-TABLE dagtemp
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR 
   FIELD ORT LIKE AONRTAB.ORT
   FIELD PERSMASK LIKE SUMTIDDAG.PERSMASK
   /*FIELD BELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "ARBKOSTNAD"*/           
   FIELD TIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"
   FIELD EOTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "OTIMMAR"
   FIELD KOTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "OTIMMAR"
   FIELD KTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"
   FIELD KEOTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "OTIMMAR"
   FIELD KKOTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "OTIMMAR"
   FIELD FTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"                      
   FIELD BEFATTNING AS CHARACTER              
   FIELD OMRADE AS CHARACTER
   FIELD GEOMRADE AS CHARACTER
   INDEX AONR IS PRIMARY AONR DELNR. 

DEFINE TEMP-TABLE slutsum           
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD PRIS LIKE TIDREGITAB.PRIS     
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP 
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR 
   FIELD ORT LIKE AONRTAB.ORT
   FIELD TIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"
   FIELD EOTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"
   FIELD KOTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"
   FIELD KTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"
   FIELD KEOTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"
   FIELD KKOTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"
   FIELD FTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"         
   INDEX AONR IS PRIMARY AONR DELNR ASCENDING.    
DEFINE TEMP-TABLE sumsum           
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR 
   FIELD TIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"
   FIELD EOTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"
   FIELD KOTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"
   FIELD KTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"
   FIELD KEOTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"
   FIELD KKOTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"
   FIELD FTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"         
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD ASCENDING.
{BOLAGSEKSTART.I}
DEFINE INPUT  PARAMETER ganv AS CHARACTER NO-UNDO.
   
DEFINE INPUT PARAMETER TABLE FOR uppvaltemp.
DEFINE INPUT PARAMETER TABLE FOR valdaao.
DEFINE OUTPUT PARAMETER TABLE FOR tidut.
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
   {MISUMOPEN.I}
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
   ASSIGN
   nrcol[1] = 1
   nrcol[2] = 2
   nrcol[3] = 3   /*27*/  
   nrcol[4] = 4   /*9*/
   nrcol[5] = 5   /*12*/
   nrcol[6] = 6 /*15*/
   nrcol[7] = 7  /*11*/
   nrcol[8] = 8  /*19*/
   nrcol[9] = 9  /*20.*/
   nrcol[10] = 10.
   assign      
   breddantal = 10   /*antal kolumner*/
   bredd[1] = 9
   bredd[2] = 40
   bredd[3] = 13
   bredd[4] = 13
   bredd[5] = 13
   bredd[6] = 13
   bredd[7] = 13
   bredd[8] = 13
   bredd[9] = 13.
   
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
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "TJM"             
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "KOLL".            
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN SUBSTRING(tidut.UT,utnr[nrcol[9]]) = "Fordon".
      
   CREATE tidut.      
   ASSIGN                               
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = CAPS(Guru.Konstanter:gaok)      
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = CAPS(Guru.Konstanter:gaonamnk)
   
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "Normaltid"     
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "Enkel Övertid"             
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "Kval övertid"             
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "Normaltid"         
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "Enkel övertid"             
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "Kval övertid"
   SUBSTRING(tidut.UT,utnr[nrcol[9]]) = "Timmar"
   SUBSTRING(tidut.UT,260) = CAPS(Guru.Konstanter:gprojl).             
   CREATE tidut.      
   tidut.UT = str.
   CREATE sumsum.
   ASSIGN sumsum.PERSONALKOD = "UF".
         
   FOR EACH slutsum USE-INDEX AONR:      
      ASSIGN
      varin = 0
      varkalk = 0.
      FIND FIRST valdaao WHERE valdaao.AONR = slutsum.AONR AND valdaao.DELNR = slutsum.DELNR NO-ERROR.
      
      
      vartot = vartot + (varin - varkalk).      
      IF Guru.Konstanter:globforetag = "misv" THEN DO:
         CREATE tidut.                                   
         ASSIGN 
         SUBSTRING(tidut.UT,utnr[nrcol[1]]) = slutsum.AONR + STRING(slutsum.DELNR,Guru.Konstanter:varforetypchar[1])
         SUBSTRING(tidut.UT,utnr[nrcol[2]]) = SUBSTRING(slutsum.ORT,1,bredd[2])                       
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(slutsum.TIMMAR,">>>>>9.99")
         SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(slutsum.EOTIMMAR,">>>>>9.99")                                                          
         SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(slutsum.KOTIMMAR,">>>>>9.99")                                                          
         SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(slutsum.KTIMMAR,">>>>>9.99")
         SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(slutsum.KEOTIMMAR,">>>>>9.99")                                                          
         SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(slutsum.KKOTIMMAR,">>>>>9.99")
         SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(slutsum.FTIMMAR,">>>>>9.99").    
                                                              
         IF AVAILABLE valdaao THEN DO:
            RUN pnamn_UI.                       
         END. 
         
         FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "UF" NO-ERROR.                   
         ASSIGN  
         sumsum.TIMMAR =  sumsum.TIMMAR  + ( slutsum.TIMMAR )
         sumsum.EOTIMMAR =  sumsum.EOTIMMAR  + ( slutsum.EOTIMMAR )
         sumsum.KOTIMMAR =  sumsum.KOTIMMAR  + ( slutsum.KOTIMMAR )
         sumsum.KTIMMAR =  sumsum.KTIMMAR  + ( slutsum.KTIMMAR )
         sumsum.KEOTIMMAR =  sumsum.KEOTIMMAR  + ( slutsum.KEOTIMMAR )
         sumsum.KKOTIMMAR =  sumsum.KKOTIMMAR  + ( slutsum.KKOTIMMAR )
         sumsum.FTIMMAR =  sumsum.FTIMMAR  + ( slutsum.FTIMMAR ).           
      END.       
               
   END.
   IF ingakostver = FALSE THEN  DO:     
   END.
   ELSE DO:
      FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "UF" NO-ERROR.
      CREATE tidut.
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "SUMMA".                             
   END.
END PROCEDURE.



PROCEDURE skapadagdag_UI :
   GET FIRST stq NO-LOCK.
   DO WHILE AVAILABLE(TIDREGITAB):        
      IF TIDREGITAB.PRISTYP = "RESTID..." THEN.          
      ELSE DO:
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = TIDREGITAB.PERSONALKOD NO-LOCK NO-ERROR.
         FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING USE-INDEX ANSTF NO-LOCK NO-ERROR.
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = TIDREGITAB.AONR AND AONRTAB.DELNR = TIDREGITAB.DELNR NO-LOCK NO-ERROR.
            
         CREATE dagtemp.
         ASSIGN                   
         dagtemp.AONR = TIDREGITAB.AONR.
         dagtemp.DELNR = TIDREGITAB.DELNR. 
         dagtemp.TIMMAR = klock100(TIDREGITAB.TOTALT).
         dagtemp.BEFATTNING = PERSONALTAB.BEFATTNING.
         dagtemp.PERSMASK = TRUE.
         IF AVAILABLE AONRTAB THEN DO: 
            dagtemp.ORT = AONRTAB.ORT. 
         END.
                  
         IF TIDREGITAB.OKOD1 NE " " THEN DO: 
            FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND
            OVERKOD.OVERTIDTILL = TIDREGITAB.OKOD1 USE-INDEX OVER NO-LOCK NO-ERROR.
            IF AVAILABLE OVERKOD THEN DO:
               IF OVERKOD.OVERTIDUTTAG = "M" THEN.
               ELSE DO:  
                  dagtemp.TIMMAR = 0.          
                  IF TIDREGITAB.TOTALT < TIDREGITAB.OANT1 THEN DO:
                     IF OVERKOD.ENKEL = "ENKE" THEN dagtemp.EOTIMMAR = klock100(TIDREGITAB.TOTALT).
                     IF OVERKOD.ENKEL = "KVAL" THEN dagtemp.KOTIMMAR = klock100(TIDREGITAB.TOTALT).                    
                  END.
                  ELSE DO:
                     IF OVERKOD.ENKEL = "ENKE" THEN dagtemp.EOTIMMAR = klock100(TIDREGITAB.OANT1).
                     IF OVERKOD.ENKEL = "KVAL" THEN dagtemp.KOTIMMAR = klock100(TIDREGITAB.OANT1).
                  END.                                     
               END.
            END.            
         END.          
         IF TIDREGITAB.OKOD2 NE " " THEN DO: 
            FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND
            OVERKOD.OVERTIDTILL = TIDREGITAB.OKOD2 USE-INDEX OVER NO-LOCK NO-ERROR.
            IF AVAILABLE OVERKOD THEN DO:
               IF OVERKOD.OVERTIDUTTAG = "M" THEN.
               ELSE DO:            
                  dagtemp.TIMMAR = 0.                                  
                  IF  klock100(TIDREGITAB.TOTALT) < ( klock100(TIDREGITAB.OANT1) +  klock100(TIDREGITAB.OANT2))  THEN DO:
                     IF OVERKOD.ENKEL = "ENKE" THEN dagtemp.EOTIMMAR = dagtemp.EOTIMMAR + klock100(TIDREGITAB.TOTALT) - klock100(TIDREGITAB.OANT1).
                     IF OVERKOD.ENKEL = "KVAL" THEN dagtemp.KOTIMMAR = dagtemp.KOTIMMAR + klock100(TIDREGITAB.TOTALT) - klock100(TIDREGITAB.OANT1).                     
                  END.
                  ELSE DO:
                     IF OVERKOD.ENKEL = "ENKE" THEN dagtemp.EOTIMMAR = dagtemp.EOTIMMAR + klock100(TIDREGITAB.OANT2).
                     IF OVERKOD.ENKEL = "KVAL" THEN dagtemp.KOTIMMAR = dagtemp.KOTIMMAR + klock100(TIDREGITAB.OANT2).
                  END.                      
               END.
            END.
         
         END.                
         IF TIDREGITAB.OKOD3 NE " " THEN DO: 
            FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND
            OVERKOD.OVERTIDTILL = TIDREGITAB.OKOD3 USE-INDEX OVER NO-LOCK NO-ERROR.
            IF AVAILABLE OVERKOD THEN DO:
               IF OVERKOD.OVERTIDUTTAG = "M" THEN.
               ELSE DO:    
                  dagtemp.TIMMAR = 0.                            
                  IF  klock100(TIDREGITAB.TOTALT) < ( klock100(TIDREGITAB.OANT1) +  klock100(TIDREGITAB.OANT2) + klock100(TIDREGITAB.OANT3))  THEN DO:
                     IF OVERKOD.ENKEL = "ENKE" THEN dagtemp.EOTIMMAR = dagtemp.EOTIMMAR + klock100(TIDREGITAB.TOTALT) - klock100(TIDREGITAB.OANT1) - klock100(TIDREGITAB.OANT2).
                     IF OVERKOD.ENKEL = "KVAL" THEN dagtemp.KOTIMMAR = dagtemp.KOTIMMAR + klock100(TIDREGITAB.TOTALT) - klock100(TIDREGITAB.OANT1) - klock100(TIDREGITAB.OANT2).
                    
                  END.
                  ELSE DO:
                     IF OVERKOD.ENKEL = "ENKE" THEN dagtemp.EOTIMMAR = dagtemp.EOTIMMAR + klock100(TIDREGITAB.OANT3).
                     IF OVERKOD.ENKEL = "KVAL" THEN dagtemp.KOTIMMAR = dagtemp.KOTIMMAR + klock100(TIDREGITAB.OANT3).                     
                  END.                     
               END.
            END.
            
         END.
         IF dagtemp.BEFATTNING NE "MSV" THEN DO:
            ASSIGN
            dagtemp.KTIMMAR = dagtemp.TIMMAR
            dagtemp.KEOTIMMAR = dagtemp.EOTIMMAR
            dagtemp.KKOTIMMAR = dagtemp.KOTIMMAR
            dagtemp.TIMMAR = 0
            dagtemp.EOTIMMAR = 0
            dagtemp.KOTIMMAR = 0.
         END.            
      END.
      GET NEXT stq NO-LOCK.      
   END.   
   IF Guru.Konstanter:globforetag = "MISV" THEN DO:
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
            dagtemp.PERSMASK = TRUE.
            dagtemp.FTIMMAR =  klock100(TIDREGITAB.LONTILLANTAL).
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
      /*ACCUMULATE dagtemp.BELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK).*/ 
      ACCUMULATE dagtemp.TIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK).
      ACCUMULATE dagtemp.EOTIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK).
      ACCUMULATE dagtemp.KOTIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK).
      ACCUMULATE dagtemp.KTIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK).
      ACCUMULATE dagtemp.KEOTIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK).
      ACCUMULATE dagtemp.KKOTIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK).
      ACCUMULATE dagtemp.FTIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK).       
      IF LAST-OF(dagtemp.PERSMASK) THEN DO:         
         CREATE slutsum.
         ASSIGN 
         slutsum.AONR = dagtemp.AONR
         slutsum.DELNR = dagtemp.DELNR
         slutsum.ORT = dagtemp.ORT                                        
         slutsum.TIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.TIMMAR)
         slutsum.EOTIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.EOTIMMAR)
         slutsum.KOTIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.KOTIMMAR)
         slutsum.KTIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.KTIMMAR)
         slutsum.KEOTIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.KEOTIMMAR)
         slutsum.KKOTIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.KKOTIMMAR)
         slutsum.FTIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.FTIMMAR). 
                                                
      END.     
   END.
       
END PROCEDURE.   

PROCEDURE pnamn_UI:
   ASSIGN
   SUBSTRING(tidut.UT,260) = valdaao.PROJEKTOR.   
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valdaao.PROJEKTOR NO-LOCK NO-ERROR.
   IF AVAILABLE PERSONALTAB THEN DO:
      SUBSTRING(tidut.UT,260) = CAPS(SUBSTRING(PERSONALTAB.FORNAMN,1,1)) + " " + CAPS(PERSONALTAB.EFTERNAMN).
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
   END.   
   ELSE SUBSTRING(tidut.UT,260) = valdaao.PROJEKTOR.
   

END PROCEDURE.
