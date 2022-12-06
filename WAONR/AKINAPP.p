/*AKINAPP.P*/
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
{DIRDEF.I}
{TIDUTTT.I}

DEFINE TEMP-TABLE sumaotemp   
   FIELD FAKTNR LIKE FAKTINTAKTKONT.FAKTNR 
   FIELD VFAKTNR LIKE FAKTINTAKTKONT.VFAKTNR 
   FIELD VKREDIT LIKE FAKTINTAKTKONTKRED.VKREDIT
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR   
   FIELD ORT LIKE AONRTAB.ORT 
   FIELD TIDSTART LIKE AONRTAB.STARTDATUM 
   FIELD AVSLUTDATUM LIKE AONRTAB.AONRAVDATUM         
   FIELD FAKTTYP LIKE AONRTAB.FAKTTYP
   FIELD TOTKOST AS DECIMAL
   FIELD FAKTKOST AS DECIMAL
   FIELD BESTID LIKE BESTTAB.BESTID
   FIELD SATS% AS DECIMAL
   FIELD KONTOBELOPP AS DECIMAL EXTENT 50 
   FIELD ARTAL AS INTEGER
   FIELD MANAD AS INTEGER
   INDEX DEB IS PRIMARY FAKTNR VFAKTNR AONR DELNR
   INDEX KREDIT FAKTNR VKREDIT AONR DELNR
   INDEX VKREDIT VKREDIT
   INDEX VFAKTNR VFAKTNR
   INDEX MANAD ARTAL MANAD.
   
   
DEFINE TEMP-TABLE sumaotemp2
   FIELD FAKTNR LIKE FAKTINTAKTKONT.FAKTNR 
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR   
   FIELD ORT LIKE AONRTAB.ORT 
   FIELD TIDSTART LIKE AONRTAB.STARTDATUM 
   FIELD AVSLUTDATUM LIKE AONRTAB.AONRAVDATUM         
   FIELD FAKTTYP LIKE AONRTAB.FAKTTYP
   FIELD TOTKOST AS DECIMAL
   FIELD FAKTKOST AS DECIMAL
   FIELD BESTID LIKE BESTTAB.BESTID
   FIELD SATS% AS DECIMAL
   FIELD KONTOBELOPP AS DECIMAL EXTENT 50 
   FIELD ARTAL AS INTEGER
   FIELD MANAD AS INTEGER
   INDEX AONR IS PRIMARY AONR DELNR
   INDEX MANAD ARTAL MANAD.


DEFINE TEMP-TABLE sumkonto 
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD ORDNING LIKE TIDREGITAB.DELNR   
   FIELD ORT LIKE AONRTAB.ORT 
   FIELD FAKTTYP LIKE AONRTAB.FAKTTYP
   FIELD BELOPP AS DECIMAL  
   FIELD BESTID LIKE BESTTAB.BESTID
   FIELD SATS% AS DECIMAL
   FIELD KONTO LIKE AONRKONTKOD.K2
   FIELD ARTAL AS INTEGER
   FIELD MANAD AS INTEGER
   INDEX AONR IS PRIMARY AONR DELNR ORDNING BESTID FAKTTYP
   INDEX MANAD ARTAL MANAD.

DEFINE TEMP-TABLE sumkonto2 
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD ORDNING LIKE TIDREGITAB.DELNR   
   FIELD ORT LIKE AONRTAB.ORT 
   FIELD FAKTTYP LIKE AONRTAB.FAKTTYP
   FIELD BELOPP AS DECIMAL  
   FIELD BESTID LIKE BESTTAB.BESTID
   FIELD SATS% AS DECIMAL
   FIELD KONTO LIKE AONRKONTKOD.K2
   FIELD ARTAL AS INTEGER
   FIELD MANAD AS INTEGER
   INDEX AONR IS PRIMARY AONR DELNR ORDNING BESTID FAKTTYP
   INDEX MANAD ARTAL MANAD.

      

DEFINE INPUT PARAMETER TABLE FOR uppvaltemp.
DEFINE INPUT PARAMETER TABLE FOR valdaao.
DEFINE OUTPUT PARAMETER TABLE FOR tidut.
DEFINE OUTPUT PARAMETER str AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE OUTPUT PARAMETER str2 AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE OUTPUT PARAMETER str3 AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE QUERY akdq FOR sumaotemp ,FAKTAONRKONTO.
DEFINE QUERY akkq FOR sumaotemp ,FAKTAONRKONTOKRED.
DEFINE QUERY fq FOR FAKTINTAKTKONT,FAKTURERAD.
DEFINE QUERY fkq FOR FAKTINTAKTKONTKRED,FAKTKRED.

DEFINE VARIABLE utnr AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE kontovar AS CHARACTER EXTENT 50 NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE manadvar AS LOGICAL NO-UNDO. 

FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.   
FIND FIRST uppvaltemp NO-ERROR.
manadvar = uppvaltemp.MANUPPDEL.
OPEN QUERY aq FOR EACH valdaao NO-LOCK. 
GET FIRST aq NO-LOCK.
IF AVAILABLE valdaao THEN DO:
   DO WHILE AVAILABLE(valdaao):      
      {FAKOPEN.I}
      GET NEXT aq NO-LOCK.
   END.      
END.
ELSE DO:
   OPEN QUERY ffq FOR EACH FAKTURERAD WHERE 
   FAKTURERAD.VFAKTNR NE 0 AND 
   FAKTURERAD.BOKDATUM >= uppvaltemp.STARTDATUM AND FAKTURERAD.BOKDATUM <= uppvaltemp.SLUTDATUM NO-LOCK,
   EACH FAKTINTAKTKONT WHERE 
   FAKTINTAKTKONT.VFAKTNR = FAKTURERAD.VFAKTNR NO-LOCK.
   GET FIRST ffq NO-LOCK.
   DO WHILE AVAILABLE(FAKTINTAKTKONT):     
      FIND FIRST valdaao WHERE valdaao.AONR = FAKTINTAKTKONT.AONR AND valdaao.DELNR = FAKTINTAKTKONT.DELNR
      NO-ERROR.
      IF NOT AVAILABLE valdaao THEN DO:
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = FAKTINTAKTKONT.AONR AND AONRTAB.DELNR = FAKTINTAKTKONT.DELNR
         NO-LOCK NO-ERROR.
         IF AVAILABLE AONRTAB THEN DO:
            CREATE valdaao.          
            BUFFER-COPY AONRTAB TO valdaao
            ASSIGN
            valdaao.PROJEKTOR = AONRTAB.STARTDAG
            valdaao.AONRREC = RECID(AONRTAB)
            valdaao.TABORT = TRUE.
         END.        
      END.
      IF AVAILABLE valdaao THEN DO:
         RUN hitta_UI (INPUT valdaao.BESTID, INPUT valdaao.FAKTTYP).   
         IF FAKTINTAKTKONT.FAKTNR = valdaao.FAKTNR THEN 
            sumaotemp.FAKTKOST = sumaotemp.FAKTKOST + FAKTINTAKTKONT.BELOPP.
         ELSE DO:
            FIND FIRST FAKTPLAN WHERE FAKTPLAN.FAKTNR = FAKTINTAKTKONT.FAKTNR NO-LOCK NO-ERROR.
            RUN hitta_UI (INPUT FAKTPLAN.BESTID, INPUT FAKTPLAN.FAKTTYP).         
            sumaotemp.FAKTKOST = sumaotemp.FAKTKOST + FAKTINTAKTKONT.BELOPP.
         END.             
      END.
      GET NEXT ffq NO-LOCK.      
   END.
   OPEN QUERY ffkq FOR EACH FAKTKRED WHERE FAKTKRED.VKREDIT NE 0 AND 
   FAKTKRED.BOKDATUM >= uppvaltemp.STARTDATUM AND FAKTKRED.BOKDATUM <= uppvaltemp.SLUTDATUM NO-LOCK,
   EACH FAKTINTAKTKONTKRED WHERE 
   FAKTINTAKTKONTKRED.VKREDIT = FAKTKRED.VKREDIT NO-LOCK.
   GET FIRST ffkq NO-LOCK.
   DO WHILE AVAILABLE(FAKTINTAKTKONTKRED): 
      FIND FIRST valdaao WHERE valdaao.AONR = FAKTINTAKTKONTKRED.AONR AND valdaao.DELNR = FAKTINTAKTKONTKRED.DELNR
      NO-ERROR.
      IF NOT AVAILABLE valdaao THEN DO:
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = FAKTINTAKTKONTKRED.AONR AND AONRTAB.DELNR = FAKTINTAKTKONTKRED.DELNR
         NO-LOCK NO-ERROR.
         IF AVAILABLE AONRTAB THEN DO:
            CREATE valdaao.          
            BUFFER-COPY AONRTAB TO valdaao
            ASSIGN
            valdaao.PROJEKTOR = AONRTAB.STARTDAG
            valdaao.AONRREC = RECID(AONRTAB)
            valdaao.TABORT = TRUE.
         END.        
      END.
      IF AVAILABLE valdaao THEN DO:
         RUN hittak_UI (INPUT valdaao.BESTID, INPUT valdaao.FAKTTYP).   
         IF FAKTINTAKTKONTKRED.FAKTNR = valdaao.FAKTNR THEN sumaotemp.FAKTKOST = sumaotemp.FAKTKOST - FAKTINTAKTKONTKRED.BELOPP.
         ELSE DO:
            FIND FIRST FAKTPLAN WHERE FAKTPLAN.FAKTNR = FAKTINTAKTKONTKRED.FAKTNR NO-LOCK NO-ERROR.
            RUN hittak_UI (INPUT FAKTPLAN.BESTID, INPUT FAKTPLAN.FAKTTYP).         
            sumaotemp.FAKTKOST = sumaotemp.FAKTKOST - FAKTINTAKTKONTKRED.BELOPP.
         END.             
      END.
      GET NEXT ffkq NO-LOCK.      
   END.
END.
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
   RUN forkl_UI.
   ASSIGN
   utnr[1] = 1
   utnr[2] = 12
   utnr[3] = 30
   utnr[4] = 39
   utnr[5] = 48
   utnr[6] = 57
   utnr[7] = 66
   utnr[8] = 75
   utnr[9] = 84
   utnr[10] = 93
   utnr[11] = 102
   utnr[12] = 111
   utnr[13] = 120
   utnr[14] = 126.
   i = 1. 
   DO WHILE i <= 130:      
      str = str + "=".      
      i = i + 1.
   END.   
   i = 2.      
   DO WHILE i <= 14:             
      SUBSTRING(str,(utnr[i] - 1),1) = ".".      
      i = i + 1.
   END.           
   CREATE tidut.  
   SUBSTRING(tidut.UT,utnr[11]) = kontovar[18].
   CREATE tidut.   
   ASSIGN                         
   SUBSTRING(tidut.UT,utnr[3]) = kontovar[1]  
   SUBSTRING(tidut.UT,utnr[4]) = kontovar[3]      
   SUBSTRING(tidut.UT,utnr[5]) = kontovar[5]      
   SUBSTRING(tidut.UT,utnr[6]) = kontovar[7]
   SUBSTRING(tidut.UT,utnr[7]) = kontovar[9]
   SUBSTRING(tidut.UT,utnr[8]) = kontovar[11]
   SUBSTRING(tidut.UT,utnr[9]) = kontovar[13]
   SUBSTRING(tidut.UT,utnr[11]) = kontovar[16]
   SUBSTRING(tidut.UT,utnr[12]) = "FAKT."
   SUBSTRING(tidut.UT,utnr[13]) = "FAKT."
   SUBSTRING(tidut.UT,utnr[14]) = "AVIK."
   
   str2 = tidut.UT.                  
   CREATE tidut.
   IF manadvar = TRUE THEN SUBSTRING(tidut.UT,utnr[1]) = "MÅN-" + CAPS(Guru.Konstanter:gaok).
   ELSE SUBSTRING(tidut.UT,utnr[1]) = CAPS(Guru.Konstanter:gaok).
   ASSIGN
   
   SUBSTRING(tidut.UT,utnr[2]) = CAPS(Guru.Konstanter:gaonamnk)
   SUBSTRING(tidut.UT,utnr[3]) = kontovar[2]  
   SUBSTRING(tidut.UT,utnr[4]) = kontovar[4]      
   SUBSTRING(tidut.UT,utnr[5]) = kontovar[6]      
   SUBSTRING(tidut.UT,utnr[6]) = kontovar[8]
   SUBSTRING(tidut.UT,utnr[7]) = kontovar[10]
   SUBSTRING(tidut.UT,utnr[8]) = kontovar[12]
   SUBSTRING(tidut.UT,utnr[9]) = kontovar[14]
   SUBSTRING(tidut.UT,utnr[10]) = kontovar[15]
   SUBSTRING(tidut.UT,utnr[11]) = kontovar[17]
   SUBSTRING(tidut.UT,utnr[12]) = "SUMMA"
   SUBSTRING(tidut.UT,utnr[13]) = "PLAN"      
   SUBSTRING(tidut.UT,utnr[14]) = CAPS(SUBSTRING(Guru.Konstanter:gbestk,1,5)).
                                    
   str3 = tidut.UT.                
   CREATE tidut.
   tidut.UT = str.     
   FOR EACH sumaotemp2 BREAK BY sumaotemp2.ARTAL BY sumaotemp2.MANAD:      
      musz = TRUE.
      ACCUMULATE sumaotemp2.KONTOBELOPP[1] (TOTAL).
      ACCUMULATE sumaotemp2.KONTOBELOPP[2] (TOTAL).
      ACCUMULATE sumaotemp2.KONTOBELOPP[3] (TOTAL).
      ACCUMULATE sumaotemp2.KONTOBELOPP[4] (TOTAL).
      ACCUMULATE sumaotemp2.KONTOBELOPP[5] (TOTAL).
      ACCUMULATE sumaotemp2.KONTOBELOPP[6] (TOTAL).
      ACCUMULATE sumaotemp2.KONTOBELOPP[7] (TOTAL).
      ACCUMULATE sumaotemp2.KONTOBELOPP[8] (TOTAL).
      ACCUMULATE sumaotemp2.KONTOBELOPP[9] (TOTAL).
      ACCUMULATE sumaotemp2.FAKTKOST (TOTAL).  

      ACCUMULATE sumaotemp2.KONTOBELOPP[1] (TOTAL BY sumaotemp2.MANAD).
      ACCUMULATE sumaotemp2.KONTOBELOPP[2] (TOTAL BY sumaotemp2.MANAD).
      ACCUMULATE sumaotemp2.KONTOBELOPP[3] (TOTAL BY sumaotemp2.MANAD).
      ACCUMULATE sumaotemp2.KONTOBELOPP[4] (TOTAL BY sumaotemp2.MANAD).
      ACCUMULATE sumaotemp2.KONTOBELOPP[5] (TOTAL BY sumaotemp2.MANAD).
      ACCUMULATE sumaotemp2.KONTOBELOPP[6] (TOTAL BY sumaotemp2.MANAD).
      ACCUMULATE sumaotemp2.KONTOBELOPP[7] (TOTAL BY sumaotemp2.MANAD).
      ACCUMULATE sumaotemp2.KONTOBELOPP[8] (TOTAL BY sumaotemp2.MANAD).
      ACCUMULATE sumaotemp2.KONTOBELOPP[9] (TOTAL BY sumaotemp2.MANAD).
      ACCUMULATE sumaotemp2.FAKTKOST (TOTAL BY sumaotemp2.MANAD). 

      CREATE tidut.
      IF manadvar = TRUE THEN DO:
         SUBSTRING(tidut.UT,utnr[1]) = STRING(sumaotemp2.MANAD,">9") + " " + sumaotemp2.AONR. 
      END.
      ELSE DO:
         SUBSTRING(tidut.UT,utnr[1]) = sumaotemp2.AONR + " " + STRING(sumaotemp2.DELNR,Guru.Konstanter:varforetypchar[1]).
      END.
      ASSIGN      
      SUBSTRING(tidut.UT,utnr[2]) = SUBSTRING(sumaotemp2.ORT,1,17).
      IF sumaotemp2.KONTOBELOPP[1] NE 0 THEN DO:                                         
         musz = FALSE.
         SUBSTRING(tidut.UT,utnr[3]) = STRING(sumaotemp2.KONTOBELOPP[1],"->>>>>>9"). 
      END.
      IF sumaotemp2.KONTOBELOPP[2] NE 0 THEN DO:                                
         musz = FALSE.
         SUBSTRING(tidut.UT,utnr[4]) = STRING(sumaotemp2.KONTOBELOPP[2],"->>>>>>9"). 
      END.
      IF sumaotemp2.KONTOBELOPP[3] NE 0 THEN DO:                                
         musz = FALSE.
         SUBSTRING(tidut.UT,utnr[5]) = STRING(sumaotemp2.KONTOBELOPP[3],"->>>>>>9"). 
      END.
      IF sumaotemp2.KONTOBELOPP[4] NE 0 THEN DO:                                
         musz = FALSE.
         SUBSTRING(tidut.UT,utnr[6]) = STRING(sumaotemp2.KONTOBELOPP[4],"->>>>>>9"). 
      END.
      IF sumaotemp2.KONTOBELOPP[5] NE 0 THEN DO:                                
         musz = FALSE.
         SUBSTRING(tidut.UT,utnr[7]) = STRING(sumaotemp2.KONTOBELOPP[5],"->>>>>>9"). 
      END.
      IF sumaotemp2.KONTOBELOPP[6] NE 0 THEN DO:                                
         musz = FALSE.
         SUBSTRING(tidut.UT,utnr[8]) = STRING(sumaotemp2.KONTOBELOPP[6],"->>>>>>9"). 
      END.
      IF sumaotemp2.KONTOBELOPP[7] NE 0 THEN DO:                                
         musz = FALSE.
         SUBSTRING(tidut.UT,utnr[9]) = STRING(sumaotemp2.KONTOBELOPP[7],"->>>>>>9"). 
      END.
      IF sumaotemp2.KONTOBELOPP[8] NE 0 THEN DO:                                
         musz = FALSE.
         SUBSTRING(tidut.UT,utnr[10]) = STRING(sumaotemp2.KONTOBELOPP[8],"->>>>>>9"). 
      END.
      IF sumaotemp2.KONTOBELOPP[9] NE 0 THEN DO:                                
         musz = FALSE.                                                    
         SUBSTRING(tidut.UT,utnr[11]) = STRING(sumaotemp2.KONTOBELOPP[9],"->>>>>>9"). 
      END.
      IF musz = TRUE THEN DELETE tidut.
      ELSE DO:
         IF sumaotemp2.FAKTKOST NE 0 THEN DO:
            SUBSTRING(tidut.UT,utnr[12]) = STRING(sumaotemp2.FAKTKOST,"->>>>>>9"). 
         END.
         /*
         IF sumaotemp2.FAKTTYP NE uppvaltemp.FAKTTYP THEN DO:
            SUBSTRING(tidut.UT,utnr[13]) = SUBSTRING(faktyp(sumaotemp2.FAKTTYP),1,5).
            
         END.
         */
         SUBSTRING(tidut.UT,utnr[13]) = STRING(sumaotemp2.FAKTNR).
         IF sumaotemp2.BESTID NE uppvaltemp.BESTID THEN DO:
            SUBSTRING(tidut.UT,utnr[14]) = sumaotemp2.BESTID.
         END.
      END.
      IF manadvar = TRUE THEN DO:
         IF LAST-OF(sumaotemp2.MANAD) THEN DO:
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,utnr[1]) = "Månad " + STRING(sumaotemp2.MANAD). 
            IF (ACCUM TOTAL BY sumaotemp2.MANAD sumaotemp2.KONTOBELOPP[1]) NE 0 THEN
            SUBSTRING(tidut.UT,utnr[3]) = STRING((ACCUM TOTAL BY sumaotemp2.MANAD sumaotemp2.KONTOBELOPP[1]),"->>>>>>9").
            IF (ACCUM TOTAL BY sumaotemp2.MANAD sumaotemp2.KONTOBELOPP[2]) NE 0 THEN
            SUBSTRING(tidut.UT,utnr[4]) = STRING((ACCUM TOTAL BY sumaotemp2.MANAD sumaotemp2.KONTOBELOPP[2]),"->>>>>>9").
            IF (ACCUM TOTAL BY sumaotemp2.MANAD sumaotemp2.KONTOBELOPP[3]) NE 0 THEN
            SUBSTRING(tidut.UT,utnr[5]) = STRING((ACCUM TOTAL BY sumaotemp2.MANAD sumaotemp2.KONTOBELOPP[3]),"->>>>>>9").
            IF (ACCUM TOTAL BY sumaotemp2.MANAD sumaotemp2.KONTOBELOPP[4]) NE 0 THEN
            SUBSTRING(tidut.UT,utnr[6]) = STRING((ACCUM TOTAL BY sumaotemp2.MANAD sumaotemp2.KONTOBELOPP[4]),"->>>>>>9").
            IF (ACCUM TOTAL BY sumaotemp2.MANAD sumaotemp2.KONTOBELOPP[5]) NE 0 THEN
            SUBSTRING(tidut.UT,utnr[7]) = STRING((ACCUM TOTAL BY sumaotemp2.MANAD sumaotemp2.KONTOBELOPP[5]),"->>>>>>9").
            IF (ACCUM TOTAL BY sumaotemp2.MANAD sumaotemp2.KONTOBELOPP[6]) NE 0 THEN
            SUBSTRING(tidut.UT,utnr[8]) = STRING((ACCUM TOTAL BY sumaotemp2.MANAD sumaotemp2.KONTOBELOPP[6]),"->>>>>>9").
            IF (ACCUM TOTAL BY sumaotemp2.MANAD sumaotemp2.KONTOBELOPP[7]) NE 0 THEN
            SUBSTRING(tidut.UT,utnr[9]) = STRING((ACCUM TOTAL BY sumaotemp2.MANAD sumaotemp2.KONTOBELOPP[7]),"->>>>>>9"). 
            IF (ACCUM TOTAL BY sumaotemp2.MANAD sumaotemp2.KONTOBELOPP[8]) NE 0 THEN
            SUBSTRING(tidut.UT,utnr[10]) = STRING((ACCUM TOTAL BY sumaotemp2.MANAD sumaotemp2.KONTOBELOPP[8]),"->>>>>>9").
            IF (ACCUM TOTAL BY sumaotemp2.MANAD sumaotemp2.KONTOBELOPP[9]) NE 0 THEN
            SUBSTRING(tidut.UT,utnr[11]) = STRING((ACCUM TOTAL BY sumaotemp2.MANAD sumaotemp2.KONTOBELOPP[9]),"->>>>>>9").
         END.
      END.
   END.      
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[1]) = "SUMMA ". 
   IF (ACCUM TOTAL sumaotemp2.KONTOBELOPP[1]) NE 0 THEN
   SUBSTRING(tidut.UT,utnr[3]) = STRING((ACCUM TOTAL sumaotemp2.KONTOBELOPP[1]),"->>>>>>9").
   IF (ACCUM TOTAL sumaotemp2.KONTOBELOPP[2]) NE 0 THEN
   SUBSTRING(tidut.UT,utnr[4]) = STRING((ACCUM TOTAL sumaotemp2.KONTOBELOPP[2]),"->>>>>>9").
   IF (ACCUM TOTAL sumaotemp2.KONTOBELOPP[3]) NE 0 THEN
   SUBSTRING(tidut.UT,utnr[5]) = STRING((ACCUM TOTAL sumaotemp2.KONTOBELOPP[3]),"->>>>>>9").
   IF (ACCUM TOTAL sumaotemp2.KONTOBELOPP[4]) NE 0 THEN
   SUBSTRING(tidut.UT,utnr[6]) = STRING((ACCUM TOTAL sumaotemp2.KONTOBELOPP[4]),"->>>>>>9").
   IF (ACCUM TOTAL sumaotemp2.KONTOBELOPP[5]) NE 0 THEN
   SUBSTRING(tidut.UT,utnr[7]) = STRING((ACCUM TOTAL sumaotemp2.KONTOBELOPP[5]),"->>>>>>9").
   IF (ACCUM TOTAL sumaotemp2.KONTOBELOPP[6]) NE 0 THEN
   SUBSTRING(tidut.UT,utnr[8]) = STRING((ACCUM TOTAL sumaotemp2.KONTOBELOPP[6]),"->>>>>>9").
   IF (ACCUM TOTAL sumaotemp2.KONTOBELOPP[7]) NE 0 THEN
   SUBSTRING(tidut.UT,utnr[9]) = STRING((ACCUM TOTAL sumaotemp2.KONTOBELOPP[7]),"->>>>>>9"). 
   IF (ACCUM TOTAL sumaotemp2.KONTOBELOPP[8]) NE 0 THEN
   SUBSTRING(tidut.UT,utnr[10]) = STRING((ACCUM TOTAL sumaotemp2.KONTOBELOPP[8]),"->>>>>>9").
   IF (ACCUM TOTAL sumaotemp2.KONTOBELOPP[9]) NE 0 THEN
   SUBSTRING(tidut.UT,utnr[11]) = STRING((ACCUM TOTAL sumaotemp2.KONTOBELOPP[9]),"->>>>>>9").
   /*IF (ACCUM TOTAL sumaotemp2.FAKTKOST) NE 0 THEN
   SUBSTRING(tidut.UT,utnr[12]) = STRING((ACCUM TOTAL sumaotemp2.FAKTKOST),"->>>>>>9").      
   */
END PROCEDURE.

PROCEDURE hitta_UI :
   DEFINE INPUT PARAMETER varbes LIKE BESTTAB.BESTID NO-UNDO.
   DEFINE INPUT PARAMETER varftyp LIKE FAKTPLAN.FAKTTYP NO-UNDO.   
   IF manadvar = TRUE THEN DO:
      FIND FIRST sumaotemp WHERE sumaotemp.AONR = valdaao.AONR AND sumaotemp.DELNR = valdaao.DELNR AND
      sumaotemp.BESTID = varbes AND sumaotemp.FAKTNR = FAKTINTAKTKONT.FAKTNR AND
      sumaotemp.VFAKTNR = FAKTINTAKTKONT.VFAKTNR AND
      sumaotemp.FAKTTYP = varftyp AND sumaotemp.ARTAL = YEAR(FAKTURERAD.BOKDATUM) AND 
      sumaotemp.MANAD = MONTH(FAKTURERAD.BOKDATUM)
      NO-ERROR.
   END.
   ELSE DO:
      FIND FIRST sumaotemp WHERE sumaotemp.AONR = valdaao.AONR AND sumaotemp.DELNR = valdaao.DELNR AND
      sumaotemp.BESTID = varbes AND sumaotemp.FAKTNR = FAKTINTAKTKONT.FAKTNR AND
      sumaotemp.VFAKTNR = FAKTINTAKTKONT.VFAKTNR AND
      sumaotemp.FAKTTYP = varftyp NO-ERROR.
   END.

   IF NOT AVAILABLE sumaotemp THEN DO:
      CREATE sumaotemp.
      IF manadvar = TRUE THEN DO:
         ASSIGN
         sumaotemp.ARTAL = YEAR(FAKTURERAD.BOKDATUM)
         sumaotemp.MANAD = MONTH(FAKTURERAD.BOKDATUM).
      END.
      ELSE DO:
         ASSIGN
         sumaotemp.ARTAL = 1
         sumaotemp.MANAD = 1.
      END.
   END.
   ASSIGN      
   sumaotemp.FAKTNR = FAKTINTAKTKONT.FAKTNR 
   sumaotemp.VFAKTNR = FAKTINTAKTKONT.VFAKTNR
   sumaotemp.AONR = valdaao.AONR
   sumaotemp.DELNR = valdaao.DELNR         
   sumaotemp.ORT = valdaao.ORT 
   sumaotemp.TIDSTART = valdaao.STARTDATUM 
   sumaotemp.AVSLUTDATUM = valdaao.AONRAVDATUM         
   sumaotemp.FAKTTYP = varftyp
   sumaotemp.BESTID = varbes.         
END PROCEDURE.
PROCEDURE hittak_UI :
   DEFINE INPUT PARAMETER varbes LIKE BESTTAB.BESTID NO-UNDO.
   DEFINE INPUT PARAMETER varftyp LIKE FAKTPLAN.FAKTTYP NO-UNDO.   
   IF manadvar = TRUE THEN DO:
      FIND FIRST sumaotemp WHERE sumaotemp.AONR = valdaao.AONR AND sumaotemp.DELNR = valdaao.DELNR AND
      sumaotemp.BESTID = varbes AND sumaotemp.FAKTNR = FAKTINTAKTKONTKRED.FAKTNR AND
      sumaotemp.VKREDIT = FAKTINTAKTKONTKRED.VKREDIT AND 
      sumaotemp.FAKTTYP = varftyp AND sumaotemp.ARTAL = YEAR(FAKTKRED.BOKDATUM) AND 
      sumaotemp.MANAD = MONTH(FAKTKRED.BOKDATUM) NO-ERROR.
   END.
   ELSE DO:
      FIND FIRST sumaotemp WHERE sumaotemp.AONR = valdaao.AONR AND sumaotemp.DELNR = valdaao.DELNR AND
      sumaotemp.BESTID = varbes AND sumaotemp.FAKTNR = FAKTINTAKTKONTKRED.FAKTNR AND
      sumaotemp.VKREDIT = FAKTINTAKTKONTKRED.VKREDIT AND
      sumaotemp.FAKTTYP = varftyp NO-ERROR.
   END.
    IF NOT AVAILABLE sumaotemp THEN DO:
      CREATE sumaotemp.
      IF manadvar = TRUE THEN DO:
         ASSIGN
         sumaotemp.ARTAL = YEAR(FAKTKRED.BOKDATUM)
         sumaotemp.MANAD = MONTH(FAKTKRED.BOKDATUM).
      END.
      ELSE DO:
         ASSIGN
         sumaotemp.ARTAL = 1
         sumaotemp.MANAD = 1.
      END.
   END.
   ASSIGN      
   sumaotemp.FAKTNR = FAKTINTAKTKONTKRED.FAKTNR 
   sumaotemp.VKREDIT = FAKTINTAKTKONTKRED.VKREDIT
   sumaotemp.AONR = valdaao.AONR
   sumaotemp.DELNR = valdaao.DELNR         
   sumaotemp.ORT = valdaao.ORT 
   sumaotemp.TIDSTART = valdaao.STARTDATUM 
   sumaotemp.AVSLUTDATUM = valdaao.AONRAVDATUM         
   sumaotemp.FAKTTYP = varftyp
   sumaotemp.BESTID = varbes.         
END PROCEDURE.
PROCEDURE fakt_UI :   
   GET FIRST fq NO-LOCK.
   DO WHILE AVAILABLE(FAKTINTAKTKONT):     
      RUN hitta_UI (INPUT valdaao.BESTID, INPUT valdaao.FAKTTYP).   
      IF FAKTINTAKTKONT.FAKTNR = valdaao.FAKTNR THEN 
         sumaotemp.FAKTKOST = sumaotemp.FAKTKOST + FAKTINTAKTKONT.BELOPP.
      ELSE DO:
         FIND FIRST FAKTPLAN WHERE FAKTPLAN.FAKTNR = FAKTINTAKTKONT.FAKTNR NO-LOCK NO-ERROR.
         RUN hitta_UI (INPUT FAKTPLAN.BESTID, INPUT FAKTPLAN.FAKTTYP).         
         sumaotemp.FAKTKOST = sumaotemp.FAKTKOST + FAKTINTAKTKONT.BELOPP.
      END.             
      GET NEXT fq NO-LOCK.      
   END.
END PROCEDURE.
PROCEDURE faktk_UI :   
   GET FIRST fkq NO-LOCK.
   DO WHILE AVAILABLE(FAKTINTAKTKONTKRED):     
      RUN hittak_UI (INPUT valdaao.BESTID, INPUT valdaao.FAKTTYP).   
      IF FAKTINTAKTKONTKRED.FAKTNR = valdaao.FAKTNR THEN sumaotemp.FAKTKOST = sumaotemp.FAKTKOST - FAKTINTAKTKONTKRED.BELOPP.
      ELSE DO:
         FIND FIRST FAKTPLAN WHERE FAKTPLAN.FAKTNR = FAKTINTAKTKONTKRED.FAKTNR NO-LOCK NO-ERROR.
         RUN hittak_UI (INPUT FAKTPLAN.BESTID, INPUT FAKTPLAN.FAKTTYP).         
         sumaotemp.FAKTKOST = sumaotemp.FAKTKOST - FAKTINTAKTKONTKRED.BELOPP.
      END.             
      GET NEXT fkq NO-LOCK.      
   END.
END PROCEDURE.
  
PROCEDURE forkl_UI:
   CREATE tidut.   
   CREATE tidut.
   tidut.UT = "FÖRKLARINGAR  : ".
   i = 1.
   DO WHILE i <= 17:          
      FIND FIRST KONTO WHERE KONTO.KONTO = "K2" AND KONTO.KONTONR = kontovar[i] NO-LOCK NO-ERROR.
      IF AVAILABLE KONTO THEN DO:
         CREATE tidut.
         tidut.UT = KONTO.KONTONR + " " + KONTO.BENAMNING.
      END.
      i = i + 1.
   END.
END PROCEDURE.

PROCEDURE summa_UI:
   ASSIGN
   kontovar[1] = "3300" 
   kontovar[2] = "3200" 
   kontovar[3] = "3310" 
   kontovar[4] = "3210" 
   kontovar[5] = "3320" 
   kontovar[6] = "3220" 
   kontovar[7] = "3330" 
   kontovar[8] = "3230" 
   kontovar[9] = "3400" 
   kontovar[10] = "3500"
   kontovar[11] = "3410" 
   kontovar[12] = "3510"
   kontovar[13] = "3420" 
   kontovar[14] = "3520" 
   kontovar[15] = "9300" 
   kontovar[16] = "9301" 
   kontovar[17] = "9302"
   kontovar[18] = "9340".  
   DEF VAR TT AS INTEGER.
   OPEN QUERY akdq FOR EACH sumaotemp WHERE sumaotemp.VFAKTNR NE 0, 
   EACH FAKTAONRKONTO WHERE 
   FAKTAONRKONTO.FDELNR = 0 AND
   FAKTAONRKONTO.FAKTNR = sumaotemp.FAKTNR AND 
   FAKTAONRKONTO.VFAKTNR = sumaotemp.VFAKTNR AND
   FAKTAONRKONTO.AONR = sumaotemp.AONR AND 
   FAKTAONRKONTO.DELNR = sumaotemp.DELNR NO-LOCK.
   GET FIRST akdq NO-LOCK.
   DO WHILE AVAILABLE(FAKTAONRKONTO):       
      IF FAKTAONRKONTO.K2 NE "" THEN DO:
         FIND FIRST sumkonto WHERE sumkonto.AONR = sumaotemp.AONR AND
         sumkonto.DELNR = sumaotemp.DELNR AND        
         sumkonto.ORT = sumaotemp.ORT AND   
         sumkonto.FAKTTYP = sumaotemp.FAKTTYP AND
         sumkonto.BESTID = sumaotemp.BESTID AND
         sumkonto.KONTO = FAKTAONRKONTO.K2 AND
         sumkonto.ARTAL = sumaotemp.ARTAL AND
         sumkonto.MANAD = sumaotemp.MANAD 
         NO-ERROR.
         IF NOT AVAILABLE sumkonto THEN CREATE sumkonto.
         ASSIGN
         sumkonto.ARTAL = sumaotemp.ARTAL 
         sumkonto.MANAD = sumaotemp.MANAD
         sumkonto.AONR = sumaotemp.AONR
         sumkonto.DELNR = sumaotemp.DELNR         
         sumkonto.ORT = sumaotemp.ORT    
         sumkonto.FAKTTYP = sumaotemp.FAKTTYP
         sumkonto.BESTID = sumaotemp.BESTID
         sumkonto.KONTO = FAKTAONRKONTO.K2
         sumkonto.BELOPP = sumkonto.BELOPP + sumaotemp.FAKTKOST * FAKTAONRKONTO.SATS% / 100.  
      END.
      ELSE DO:     
         FIND FIRST AONRKONTKOD WHERE 
         AONRKONTKOD.AONR = FAKTAONRKONTO.AONR AND
         AONRKONTKOD.DELNR = FAKTAONRKONTO.DELNR AND
         AONRKONTKOD.K1 = FAKTAONRKONTO.K1 AND
         AONRKONTKOD.K2 = FAKTAONRKONTO.K2 AND
         AONRKONTKOD.K3 = FAKTAONRKONTO.K3 AND
         AONRKONTKOD.K4 = FAKTAONRKONTO.K4 AND
         AONRKONTKOD.K5 = FAKTAONRKONTO.K5          
         NO-LOCK NO-ERROR.
         IF AVAILABLE AONRKONTKOD THEN DO:         
            FIND FIRST sumkonto WHERE sumkonto.AONR = sumaotemp.AONR AND
            sumkonto.DELNR = sumaotemp.DELNR AND        
            sumkonto.ORT = sumaotemp.ORT AND   
            sumkonto.FAKTTYP = sumaotemp.FAKTTYP AND
            sumkonto.BESTID = sumaotemp.BESTID AND
            sumkonto.KONTO = AONRKONTKOD.K2 AND
            sumkonto.ARTAL = sumaotemp.ARTAL AND
            sumkonto.MANAD = sumaotemp.MANAD   
            NO-ERROR.
            IF NOT AVAILABLE sumkonto THEN CREATE sumkonto.
            ASSIGN
            sumkonto.ARTAL = sumaotemp.ARTAL 
            sumkonto.MANAD = sumaotemp.MANAD 
            sumkonto.AONR = sumaotemp.AONR
            sumkonto.DELNR = sumaotemp.DELNR         
            sumkonto.ORT = sumaotemp.ORT    
            sumkonto.FAKTTYP = sumaotemp.FAKTTYP
            sumkonto.BESTID = sumaotemp.BESTID
            sumkonto.KONTO = AONRKONTKOD.K2
            sumkonto.BELOPP = sumkonto.BELOPP + sumaotemp.FAKTKOST * AONRKONTKOD.SATS% / 100.                            
         END.         
      END.
      GET NEXT akdq NO-LOCK.      
   END.   
   /*sumaotemp.FAKTKOST FÖR KREDIT ÄR NEGATIV*/
   OPEN QUERY akkq FOR EACH sumaotemp WHERE sumaotemp.VKREDIT NE 0, 
   EACH FAKTAONRKONTOKRED WHERE 
   FAKTAONRKONTOKRED.FDELNR = 0 AND
   FAKTAONRKONTOKRED.FAKTNR = sumaotemp.FAKTNR AND 
   FAKTAONRKONTOKRED.VKREDIT = sumaotemp.VKREDIT AND
   FAKTAONRKONTOKRED.AONR = sumaotemp.AONR AND 
   FAKTAONRKONTOKRED.DELNR = sumaotemp.DELNR 
   NO-LOCK.
   GET FIRST akkq NO-LOCK.
   DO WHILE AVAILABLE(FAKTAONRKONTOKRED):           
      IF FAKTAONRKONTOKRED.K2 NE "" THEN DO:
         FIND FIRST sumkonto WHERE sumkonto.AONR = sumaotemp.AONR AND
         sumkonto.DELNR = sumaotemp.DELNR AND        
         sumkonto.ORT = sumaotemp.ORT AND   
         sumkonto.FAKTTYP = sumaotemp.FAKTTYP AND
         sumkonto.BESTID = sumaotemp.BESTID AND
         sumkonto.KONTO = FAKTAONRKONTOKRED.K2 AND
         sumkonto.ARTAL = sumaotemp.ARTAL AND
         sumkonto.MANAD = sumaotemp.MANAD   
         NO-ERROR.
         IF NOT AVAILABLE sumkonto THEN CREATE sumkonto.
         ASSIGN
         sumkonto.ARTAL = sumaotemp.ARTAL 
         sumkonto.MANAD = sumaotemp.MANAD 
         sumkonto.AONR = sumaotemp.AONR
         sumkonto.DELNR = sumaotemp.DELNR         
         sumkonto.ORT = sumaotemp.ORT    
         sumkonto.FAKTTYP = sumaotemp.FAKTTYP
         sumkonto.BESTID = sumaotemp.BESTID
         sumkonto.KONTO = FAKTAONRKONTOKRED.K2
         sumkonto.BELOPP = sumkonto.BELOPP + sumaotemp.FAKTKOST * FAKTAONRKONTOKRED.SATS% / 100.      
      END.
      ELSE DO:
         FIND FIRST AONRKONTKOD WHERE 
         AONRKONTKOD.AONR = FAKTAONRKONTOKRED.AONR AND
         AONRKONTKOD.DELNR = FAKTAONRKONTOKRED.DELNR AND
         AONRKONTKOD.K1 = FAKTAONRKONTOKRED.K1 AND
         AONRKONTKOD.K2 = FAKTAONRKONTOKRED.K2 AND
         AONRKONTKOD.K3 = FAKTAONRKONTOKRED.K3 AND
         AONRKONTKOD.K4 = FAKTAONRKONTOKRED.K4 AND
         AONRKONTKOD.K5 = FAKTAONRKONTOKRED.K5          
         NO-LOCK NO-ERROR.
         IF AVAILABLE AONRKONTKOD THEN DO:         
            FIND FIRST sumkonto WHERE sumkonto.AONR = sumaotemp.AONR AND
            sumkonto.DELNR = sumaotemp.DELNR AND        
            sumkonto.ORT = sumaotemp.ORT AND   
            sumkonto.FAKTTYP = sumaotemp.FAKTTYP AND
            sumkonto.BESTID = sumaotemp.BESTID AND
            sumkonto.KONTO = AONRKONTKOD.K2 AND
            sumkonto.ARTAL = sumaotemp.ARTAL AND
            sumkonto.MANAD = sumaotemp.MANAD   
            NO-ERROR.
            IF NOT AVAILABLE sumkonto THEN CREATE sumkonto.
            ASSIGN
            sumkonto.ARTAL = sumaotemp.ARTAL 
            sumkonto.MANAD = sumaotemp.MANAD 
            sumkonto.AONR = sumaotemp.AONR
            sumkonto.DELNR = sumaotemp.DELNR         
            sumkonto.ORT = sumaotemp.ORT    
            sumkonto.FAKTTYP = sumaotemp.FAKTTYP
            sumkonto.BESTID = sumaotemp.BESTID
            sumkonto.KONTO = AONRKONTKOD.K2
            sumkonto.BELOPP = sumkonto.BELOPP + sumaotemp.FAKTKOST * AONRKONTKOD.SATS% / 100.                            
         END.
      END.      
      GET NEXT akkq NO-LOCK.      
   END. 
   FOR EACH sumaotemp BREAK 
      BY sumaotemp.ARTAL BY sumaotemp.MANAD BY sumaotemp.AONR BY sumaotemp.DELNR BY sumaotemp.BESTID BY 
      sumaotemp.FAKTNR: 
      ACCUMULATE sumaotemp.FAKTKOST (TOTAL BY sumaotemp.MANAD BY sumaotemp.AONR BY sumaotemp.DELNR BY sumaotemp.BESTID BY sumaotemp.FAKTNR).
      IF LAST-OF(sumaotemp.FAKTNR) THEN DO: 
         CREATE sumaotemp2.         
         ASSIGN      
         sumaotemp2.AONR = sumaotemp.AONR 
         sumaotemp2.DELNR = sumaotemp.DELNR
         sumaotemp2.ARTAL = sumaotemp.ARTAL
         sumaotemp2.MANAD = sumaotemp.MANAD
         sumaotemp2.ORT = sumaotemp.ORT  
         sumaotemp2.TIDSTART = sumaotemp.TIDSTART  
         sumaotemp2.AVSLUTDAT = sumaotemp.AVSLUTDATUM          
         sumaotemp2.FAKTTYP = sumaotemp.FAKTTYP 
         sumaotemp2.FAKTNR = sumaotemp.FAKTNR 
         sumaotemp2.BESTID = sumaotemp.BESTID
         sumaotemp2.TOTKOST = sumaotemp.TOTKOST     
         sumaotemp2.FAKTKOST = (ACCUM TOTAL BY sumaotemp.FAKTNR sumaotemp.FAKTKOST)     
         sumaotemp2.SATS% = sumaotemp.SATS%.       
         i = 1.
         DO WHILE i <= 50:
            sumaotemp2.KONTOBELOPP[i] = sumaotemp.KONTOBELOPP[i].
            i = i + 1.
         END.
         
      END.
   END.
   FOR EACH sumkonto:
      IF sumkonto.KONTO =  kontovar[1] OR sumkonto.KONTO = kontovar[2] THEN sumkonto.ORDNING = 1.
      ELSE IF sumkonto.KONTO = kontovar[3] OR sumkonto.KONTO = kontovar[4] THEN sumkonto.ORDNING = 2.
      ELSE IF sumkonto.KONTO = kontovar[5] OR sumkonto.KONTO = kontovar[6] THEN sumkonto.ORDNING = 3. 
      ELSE IF sumkonto.KONTO = kontovar[7] OR sumkonto.KONTO = kontovar[8] THEN sumkonto.ORDNING = 4. 
      ELSE IF sumkonto.KONTO = kontovar[9] OR sumkonto.KONTO = kontovar[10] THEN sumkonto.ORDNING = 5.
      ELSE IF sumkonto.KONTO = kontovar[11] OR sumkonto.KONTO = kontovar[12] THEN sumkonto.ORDNING = 6.
      ELSE IF sumkonto.KONTO = kontovar[13] OR sumkonto.KONTO = kontovar[14] THEN sumkonto.ORDNING = 7.
      ELSE IF sumkonto.KONTO = kontovar[15] THEN sumkonto.ORDNING = 8.  
      ELSE IF sumkonto.KONTO = kontovar[16] OR sumkonto.KONTO = kontovar[17] OR sumkonto.KONTO = kontovar[18] THEN sumkonto.ORDNING = 9.
      ELSE DO:         
         DELETE sumkonto.      
      END.
   END.
   FOR EACH sumkonto BREAK BY sumkonto.ARTAL BY sumkonto.MANAD BY 
   sumkonto.AONR BY sumkonto.DELNR BY sumkonto.BESTID BY
   sumkonto.FAKTTYP BY sumkonto.ORDNING: 
      ACCUMULATE sumkonto.BELOPP (TOTAL BY sumkonto.ARTAL BY sumkonto.MANAD BY 
      sumkonto.AONR BY sumkonto.DELNR BY sumkonto.BESTID BY 
      sumkonto.FAKTTYP BY sumkonto.ORDNING).       
      IF LAST-OF(sumkonto.ORDNING) THEN DO:
         CREATE sumkonto2.         
         ASSIGN
         sumkonto2.ARTAL = sumkonto.ARTAL
         sumkonto2.MANAD = sumkonto.MANAD
         sumkonto2.AONR = sumkonto.AONR
         sumkonto2.DELNR = sumkonto.DELNR         
         sumkonto2.ORT = sumkonto.ORT    
         sumkonto2.FAKTTYP = sumkonto.FAKTTYP
         sumkonto2.BESTID = sumkonto.BESTID
         sumkonto2.KONTO = sumkonto.KONTO
         sumkonto2.ORDNING = sumkonto.ORDNING
         sumkonto2.BELOPP = (ACCUM TOTAL BY sumkonto.ORDNING sumkonto.BELOPP).      
      END.   
   END.  
   FOR EACH sumaotemp2:
      FOR EACH sumkonto2 WHERE 
      sumkonto2.ARTAL = sumaotemp2.ARTAL AND
      sumkonto2.MANAD = sumaotemp2.MANAD AND
      sumkonto2.AONR = sumaotemp2.AONR AND 
      sumkonto2.DELNR = sumaotemp2.DELNR AND 
      sumkonto2.BESTID = sumaotemp2.BESTID AND
      sumkonto2.FAKTTYP = sumaotemp2.FAKTTYP :
         sumaotemp2.KONTOBELOPP[sumkonto2.ORDNING] = sumkonto2.BELOPP.
      END.
   END.
END PROCEDURE.   
                                             
