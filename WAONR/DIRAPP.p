/*DIRAPP.P*/
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
{TIDUTTT.I}

DEFINE TEMP-TABLE sumaotemp
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR   
   FIELD ORT LIKE AONRTAB.ORT 
   FIELD TIDSTART LIKE AONRTAB.STARTDATUM 
   FIELD AVSLUTDATUM LIKE AONRTAB.AONRAVDATUM         
   FIELD FAKTTYP LIKE AONRTAB.FAKTTYP
   FIELD TOTKOST AS DECIMAL
   FIELD FAKTKOST AS DECIMAL
   FIELD OPRIS AS DECIMAL
   FIELD FASTTAK AS LOGICAL
   FIELD BESTID LIKE BESTTAB.BESTID
   INDEX AONR IS PRIMARY AONR DELNR BESTID FAKTTYP
   INDEX OPRIS OPRIS.

DEFINE TEMP-TABLE sumtot
   FIELD TOTKOST AS DECIMAL
   FIELD FAKTKOST AS DECIMAL
   FIELD OPRIS AS DECIMAL.
DEFINE TEMP-TABLE kosttemp2
   FIELD AONR LIKE KOSTREG.AONR 
   FIELD DELNR LIKE KOSTREG.DELNR
   FIELD INKOMST LIKE KOSTREG.INKOMST  
   INDEX AONR IS PRIMARY AONR DELNR.
DEFINE VARIABLE globanv AS CHARACTER NO-UNDO.
{BOLAGSEKSTART.I}
DEFINE INPUT  PARAMETER ganv AS CHARACTER NO-UNDO.
globanv = ganv.      
DEFINE INPUT PARAMETER TABLE FOR uppvaltemp.
DEFINE INPUT PARAMETER TABLE FOR valdaao.
DEFINE OUTPUT PARAMETER TABLE FOR tidut.
DEFINE OUTPUT PARAMETER str AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE OUTPUT PARAMETER str2 AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE OUTPUT PARAMETER str3 AS CHARACTER FORMAT "X(92)" NO-UNDO.

DEFINE VARIABLE utnr AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE resultvar AS INTEGER NO-UNDO.
DEFINE VARIABLE resultvar2 AS INTEGER NO-UNDO.
DEFINE VARIABLE intaktvar AS INTEGER NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.


FIND FIRST uppvaltemp NO-ERROR.

OPEN QUERY aq FOR EACH valdaao NO-LOCK. 
GET FIRST aq NO-LOCK.
DO WHILE AVAILABLE(valdaao):      
   {SUMOPEN.I}
   {FAKOPEN.I}
   GET NEXT aq NO-LOCK.
END.    
/*
{DAGTEMPBOLAG.I}
*/  
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
   ASSIGN
   utnr[1] = 1
   utnr[2] = 12
   utnr[3] = 30
   utnr[4] = 39
   utnr[5] = 48
   utnr[6] = 64
   utnr[7] = 75
   utnr[8] = 85
   utnr[9] = 95
   utnr[10] = 105
   utnr[11] = 116
   str = "".      
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   DO WHILE i <= 124:
      i = i + 1.
      str = str + "=".      
   END.   
   i = 2.      
   DO WHILE i <= 11:             
      SUBSTRING(str,(utnr[i] - 1),1) = ".".      
      i = i + 1.
   END.          
   CREATE tidut.  
   CREATE tidut.   
   ASSIGN                         
   SUBSTRING(tidut.UT,utnr[3]) = "TIDSKRI."
   SUBSTRING(tidut.UT,utnr[4]) = "AVSLUT."
   SUBSTRING(tidut.UT,utnr[5]) = "AVIKANDE"
   SUBSTRING(tidut.UT,utnr[6]) = "AVIKANDE"
   SUBSTRING(tidut.UT,utnr[7]) = "UPPARB."
   SUBSTRING(tidut.UT,utnr[11]) = "TAKPRIS/".
   str2 = tidut.UT.                  
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[1]) = CAPS(Guru.Konstanter:gaok)
   SUBSTRING(tidut.UT,utnr[2]) = CAPS(Guru.Konstanter:gaonamnk)
   SUBSTRING(tidut.UT,utnr[3]) = "STARTAT"  
   SUBSTRING(tidut.UT,utnr[4]) = "DATUM"      
   SUBSTRING(tidut.UT,utnr[5]) = "FAKTURATYP"                                         
   SUBSTRING(tidut.UT,utnr[6]) = CAPS(RIGHT-TRIM(SUBSTRIN(Guru.Konstanter:gbestk,1,10)))
   SUBSTRING(tidut.UT,utnr[7]) = "KOSTNADER"
   SUBSTRING(tidut.UT,utnr[8]) = "INTÄKT"
   SUBSTRING(tidut.UT,utnr[9]) = "RESULTAT"
   SUBSTRING(tidut.UT,utnr[10]) = "FAKTURERAT"
   SUBSTRING(tidut.UT,utnr[11]) = "FASTPRIS".
   /*SUBSTRING(tidut.UT,150) = CAPS(Guru.Konstanter:gaonamnk). */
   str3 = tidut.UT.                
   CREATE tidut.
   tidut.UT = str.
   FOR EACH valdaao:
      FIND FIRST sumaotemp WHERE sumaotemp.AONR = valdaao.AONR AND sumaotemp.DELNR = valdaao.DELNR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE sumaotemp THEN DO:
         CREATE sumaotemp.
         ASSIGN      
         sumaotemp.AONR = valdaao.AONR
         sumaotemp.DELNR = valdaao.DELNR         
         sumaotemp.ORT = valdaao.ORT 
         sumaotemp.TIDSTART = valdaao.STARTDATUM 
         sumaotemp.AVSLUTDATUM = valdaao.AONRAVDATUM.         
      END.
   END.
   FOR EACH sumaotemp:
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,utnr[1]) = STRING(sumaotemp.AONR,"X(6)") + STRING(sumaotemp.DELNR,Guru.Konstanter:varforetypchar[1])
      SUBSTRING(tidut.UT,utnr[2]) = SUBSTRING(sumaotemp.ORT,1,17)
      SUBSTRING(tidut.UT,150) = sumaotemp.ORT.
      IF sumaotemp.TIDSTART NE ? THEN DO:
         SUBSTRING(tidut.UT,utnr[3]) = STRING(sumaotemp.TIDSTART,"99/99/99"). 
      END.
      IF sumaotemp.AVSLUTDATUM NE 01/01/91 THEN DO:
         SUBSTRING(tidut.UT,utnr[4]) = STRING(sumaotemp.AVSLUTDATUM,"99/99/99").  
      END.
      IF sumaotemp.FAKTTYP NE uppvaltemp.FAKTTYP THEN DO:
         SUBSTRING(tidut.UT,utnr[5]) = SUBSTRING(faktyp(sumaotemp.FAKTTYP),1,15).
      END.
      IF sumaotemp.BESTID NE uppvaltemp.BESTID THEN DO:
         SUBSTRING(tidut.UT,utnr[6]) = sumaotemp.BESTID.
      END.
      resultvar = -1 * sumaotemp.TOTKOST.
      IF sumaotemp.TOTKOST NE 0 THEN DO:                      
         SUBSTRING(tidut.UT,utnr[7]) = STRING(sumaotemp.TOTKOST,"->>>>>>>9").
      END.   
      FIND FIRST kosttemp2 WHERE kosttemp2.AONR = sumaotemp.AONR AND
      kosttemp2.DELNR = sumaotemp.DELNR NO-ERROR.
      IF AVAILABLE kosttemp2 THEN DO:
         SUBSTRING(tidut.UT,utnr[8]) = STRING(kosttemp2.INKOMST,"->>>>>>>9").
         ASSIGN
         intaktvar = intaktvar + kosttemp2.INKOMST
         resultvar = kosttemp2.INKOMST + resultvar.
      END.
      IF resultvar NE 0 THEN DO:                         
         SUBSTRING(tidut.UT,utnr[9]) = STRING(resultvar,"->>>>>>>9"). 
      END.
      IF sumaotemp.FAKTKOST NE 0 THEN DO:                         
         SUBSTRING(tidut.UT,utnr[10]) = STRING(sumaotemp.FAKTKOST,"->>>>>>>>9"). 
      END. 
      IF sumaotemp.OPRIS NE ? THEN DO:                         
         SUBSTRING(tidut.UT,utnr[11]) = STRING(sumaotemp.OPRIS,"->>>>>>>>9"). 
      END.
      resultvar2 = resultvar2 + resultvar.
   END.     
   FIND FIRST sumtot NO-ERROR.
   IF AVAILABLE sumtot THEN DO:
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,utnr[5]) = "SUMMA " 
      SUBSTRING(tidut.UT,utnr[7]) = STRING(sumtot.TOTKOST,">>>>>>>>9")
      SUBSTRING(tidut.UT,utnr[10]) = STRING(sumtot.FAKTKOST,">>>>>>>>>9")
      SUBSTRING(tidut.UT,utnr[11]) = STRING(sumtot.OPRIS,">>>>>>>>>9").                       
      IF intaktvar < 0 THEN SUBSTRING(tidut.UT,utnr[8]) = STRING(intaktvar,"->>>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[8]) = STRING(intaktvar,">>>>>>>>9").
      IF resultvar2 < 0 THEN SUBSTRING(tidut.UT,utnr[9]) = STRING(resultvar2,"->>>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[9]) = STRING(resultvar2,">>>>>>>>9").
   END.   
END PROCEDURE.
PROCEDURE hitta_UI :
   DEFINE INPUT PARAMETER varbes LIKE BESTTAB.BESTID NO-UNDO.
   DEFINE INPUT PARAMETER varftyp LIKE FAKTPLAN.FAKTTYP NO-UNDO.   
   DEFINE INPUT PARAMETER varfaktnr LIKE FAKTPLAN.FAKTNR NO-UNDO.   
   FIND FIRST sumaotemp WHERE sumaotemp.AONR = valdaao.AONR AND sumaotemp.DELNR = valdaao.DELNR AND
   sumaotemp.BESTID = varbes AND sumaotemp.FAKTTYP = varftyp NO-ERROR.
   IF NOT AVAILABLE sumaotemp THEN DO:
      CREATE sumaotemp.           
   END.
   ASSIGN      
   sumaotemp.AONR = valdaao.AONR
   sumaotemp.DELNR = valdaao.DELNR         
   sumaotemp.ORT = valdaao.ORT 
   sumaotemp.TIDSTART = valdaao.STARTDATUM 
   sumaotemp.AVSLUTDATUM = valdaao.AONRAVDATUM         
   sumaotemp.FAKTTYP = varftyp
   sumaotemp.BESTID = varbes.         
   RUN fasttak_UI (INPUT varbes, INPUT varftyp, INPUT varfaktnr).       
END PROCEDURE.
PROCEDURE fasttak_UI :   
   DEFINE INPUT PARAMETER varbes LIKE BESTTAB.BESTID NO-UNDO.
   DEFINE INPUT PARAMETER varftyp LIKE FAKTPLAN.FAKTTYP NO-UNDO.   
   DEFINE INPUT PARAMETER varfaktnr LIKE FAKTPLAN.FAKTNR NO-UNDO.
   IF varfaktnr = 0 THEN DO:
      ASSIGN
      sumaotemp.OPRIS = ?
      sumaotemp.FASTTAK = FALSE.
      RETURN.
   END.
   IF sumaotemp.FASTTAK = TRUE THEN RETURN.  
  
   IF varftyp = "Fastpris" OR varftyp = "Takprisfakt." THEN DO:
      /*
      IF uppvaltemp.DELNRKOLL = TRUE THEN DO:
         FIND FIRST FAKTAONR WHERE FAKTAONR.FAKTNR = varfaktnr AND
         FAKTAONR.AONR = valdaao.AONR AND FAKTAONR.DELNR = valdaao.DELNR
         NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST FAKTAONR WHERE FAKTAONR.FAKTNR = varfaktnr AND
         FAKTAONR.AONR = valdaao.AONR 
         NO-LOCK NO-ERROR.
      END.
      */
      FIND FIRST FAKTAONR WHERE FAKTAONR.FAKTNR = varfaktnr AND
      FAKTAONR.AONR = valdaao.AONR AND FAKTAONR.DELNR = valdaao.DELNR
      NO-LOCK NO-ERROR.
      IF AVAILABLE FAKTAONR THEN DO:
         ASSIGN
         sumaotemp.FASTTAK = TRUE
         sumaotemp.OPRIS = FAKTAONR.OPRIS.
         IF varftyp = "Fastpris" THEN DO:
            IF YEAR(uppvaltemp.STARTDATUM) NE YEAR(TODAY) THEN sumaotemp.OPRIS = 0.
         END.
      END.      
   END.
   ELSE DO:
      ASSIGN
      sumaotemp.OPRIS = ?
      sumaotemp.FASTTAK = FALSE.
      RETURN.
   END.
END PROCEDURE.
PROCEDURE fakt_UI :   
   GET FIRST fq NO-LOCK.
   DO WHILE AVAILABLE(FAKTINTAKTKONT): 
      RUN hitta_UI (INPUT valdaao.BESTID, INPUT valdaao.FAKTTYP, INPUT FAKTINTAKTKONT.FAKTNR).   
      IF FAKTINTAKTKONT.FAKTNR = valdaao.FAKTNR THEN sumaotemp.FAKTKOST = sumaotemp.FAKTKOST + FAKTINTAKTKONT.BELOPP.
      ELSE DO:
         FIND FIRST FAKTPLAN WHERE FAKTPLAN.FAKTNR = FAKTINTAKTKONT.FAKTNR NO-LOCK NO-ERROR.
         RUN hitta_UI (INPUT FAKTPLAN.BESTID, INPUT FAKTPLAN.FAKTTYP, INPUT FAKTINTAKTKONT.FAKTNR).         
         sumaotemp.FAKTKOST = sumaotemp.FAKTKOST + FAKTINTAKTKONT.BELOPP.      
      END.             
      GET NEXT fq NO-LOCK.      
   END.
END PROCEDURE.
PROCEDURE faktk_UI :   
   GET FIRST fkq NO-LOCK.
   DO WHILE AVAILABLE(FAKTINTAKTKONTKRED):     
      RUN hitta_UI (INPUT valdaao.BESTID, INPUT valdaao.FAKTTYP, INPUT FAKTINTAKTKONTKRED.FAKTNR).   
      IF FAKTINTAKTKONTKRED.FAKTNR = valdaao.FAKTNR THEN sumaotemp.FAKTKOST = sumaotemp.FAKTKOST - FAKTINTAKTKONTKRED.BELOPP.
      ELSE DO:
         FIND FIRST FAKTPLAN WHERE FAKTPLAN.FAKTNR = FAKTINTAKTKONTKRED.FAKTNR NO-LOCK NO-ERROR.
         RUN hitta_UI (INPUT FAKTPLAN.BESTID, INPUT FAKTPLAN.FAKTTYP, INPUT FAKTINTAKTKONTKRED.FAKTNR).         
         sumaotemp.FAKTKOST = sumaotemp.FAKTKOST - FAKTINTAKTKONTKRED.BELOPP.      
      END.        
      GET NEXT fkq NO-LOCK.      
   END.
END PROCEDURE.
PROCEDURE kostreg_UI :   
   GET FIRST kq NO-LOCK.
   DO WHILE AVAILABLE(KOSTREG):     
      RUN hitta_UI (INPUT valdaao.BESTID, INPUT valdaao.FAKTTYP, INPUT 0).   
      sumaotemp.TOTKOST = sumaotemp.TOTKOST + KOSTREG.MASKKOST + KOSTREG.MTRL + 
      KOSTREG.OVRKR + KOSTREG.PERSKOST + KOSTREG.TRAKTKOST.             
      IF KOSTREG.INKOMST NE 0 THEN DO:
         FIND FIRST kosttemp2 WHERE kosttemp2.AONR = KOSTREG.AONR AND
         kosttemp2.DELNR = KOSTREG.DELNR NO-ERROR.
         IF NOT AVAILABLE kosttemp2 THEN CREATE kosttemp2.
         ASSIGN            
         kosttemp2.AONR = KOSTREG.AONR 
         kosttemp2.DELNR = KOSTREG.DELNR
         kosttemp2.INKOMST = kosttemp2.INKOMST + KOSTREG.INKOMST.  
      END.
      GET NEXT kq NO-LOCK.      
   END.
END PROCEDURE.
PROCEDURE skapadag_UI :   
   GET FIRST sq NO-LOCK.
   DO WHILE AVAILABLE(SUMTID):     
      RUN hitta_UI (INPUT valdaao.BESTID, INPUT valdaao.FAKTTYP, INPUT 0).   
      /*kalmar vill ha med gurua maskiner i uppföljning- ej kostnadsregistreringar */
      IF (Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "xGKAL") AND SUMTID.PERSMASK = FALSE THEN DO:
         musz = musz.
      END.
      ELSE DO:         
         IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
            sumaotemp.TOTKOST = sumaotemp.TOTKOST + SUMTID.BELOPP + SUMTID.OBELOPP.             
         END.
         ELSE DO:
            sumaotemp.TOTKOST = sumaotemp.TOTKOST + SUMTID.BELOPP + SUMTID.OBELOPP
            + SUMTID.LONKOST + SUMTID.TBELOPP.             
         END.        
      END.
      
      GET NEXT sq NO-LOCK.      
   END.
  END PROCEDURE.

PROCEDURE skapadagdag_UI :
   GET FIRST stq NO-LOCK.
   DO WHILE AVAILABLE(SUMTIDDAG):     
      RUN hitta_UI (INPUT valdaao.BESTID, INPUT valdaao.FAKTTYP, INPUT 0).   
      IF (Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "XGKAL") AND SUMTIDDAG.PERSMASK = FALSE THEN DO:
         musz = musz.
      END.
      ELSE DO:
         IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
            sumaotemp.TOTKOST = sumaotemp.TOTKOST + SUMTIDDAG.BELOPP + SUMTIDDAG.OBELOPP.
         END.
         ELSE DO:
            sumaotemp.TOTKOST = sumaotemp.TOTKOST + SUMTIDDAG.BELOPP + SUMTIDDAG.OBELOPP + 
            SUMTIDDAG.LONKOST  + SUMTIDDAG.TBELOPP.             
         END.
         
      END.
      GET NEXT stq NO-LOCK.      
   END.
END PROCEDURE.
PROCEDURE summa_UI.
   FOR EACH sumaotemp BREAK BY sumaotemp.PERSONALKOD: 
      ACCUMULATE sumaotemp.TOTKOST (TOTAL BY sumaotemp.PERSONALKOD).
      ACCUMULATE sumaotemp.FAKTKOST (TOTAL BY sumaotemp.PERSONALKOD). 
      ACCUMULATE sumaotemp.OPRIS (TOTAL BY sumaotemp.PERSONALKOD).
      IF LAST-OF(sumaotemp.PERSONALKOD) THEN DO:
         CREATE sumtot.         
         ASSIGN
         sumtot.TOTKOST = (ACCUM TOTAL BY sumaotemp.PERSONALKOD sumaotemp.TOTKOST)
         sumtot.FAKTKOST = (ACCUM TOTAL BY sumaotemp.PERSONALKOD sumaotemp.FAKTKOST).
      END.   
   END.
   FOR EACH sumaotemp WHERE sumaotemp.OPRIS = ?:
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = sumaotemp.AONR AND 
      AONRTAB.DELNR = sumaotemp.DELNR NO-LOCK NO-ERROR.
      IF AVAILABLE AONRTAB THEN DO:
         IF AONRTAB.BETNR > 0 THEN sumaotemp.OPRIS = AONRTAB.BETNR.
         FIND FIRST FAKTAONR WHERE FAKTAONR.FAKTNR = AONRTAB.FAKTNR AND
         FAKTAONR.AONR = AONRTAB.AONR AND FAKTAONR.DELNR = AONRTAB.DELNR
         NO-LOCK NO-ERROR.
         IF AVAILABLE FAKTAONR THEN DO:
            IF FAKTAONR.OPRIS > 0 THEN sumaotemp.OPRIS = FAKTAONR.OPRIS.
         END.
      END.
   END.
   FOR EACH sumaotemp WHERE sumaotemp.OPRIS NE ? BREAK BY sumaotemp.PERSONALKOD:    
      ACCUMULATE sumaotemp.OPRIS (TOTAL BY sumaotemp.PERSONALKOD).
      IF LAST-OF(sumaotemp.PERSONALKOD) THEN DO:
         FIND FIRST sumtot NO-ERROR.         
         IF NOT AVAILABLE sumtot THEN CREATE sumtot.
         ASSIGN
         sumtot.OPRIS = (ACCUM TOTAL BY sumaotemp.PERSONALKOD sumaotemp.OPRIS).                                 
      END.   
   END.
   
   
END PROCEDURE.   
