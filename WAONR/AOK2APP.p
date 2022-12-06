/*AOK2APP.P*/
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
RUN STYRFORE.P (INPUT Guru.Konstanter:globforetag).
&Scoped-define NEW NEW
{FAKTTYPDEF.I}
&Scoped-define NEW 
{FAKTTYPSKAP.I}
{DIRDEF.I}
{TIDUTTT.I}


DEFINE INPUT PARAMETER TABLE FOR uppvaltemp.
DEFINE INPUT PARAMETER TABLE FOR valdaao.
DEFINE OUTPUT PARAMETER antalk3 AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR tidut.
DEFINE VARIABLE str AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE breddantal AS INTEGER NO-UNDO.
DEFINE VARIABLE utnr AS INTEGER EXTENT 200 NO-UNDO.
DEFINE VARIABLE bredd AS INTEGER EXTENT 200 NO-UNDO.
DEFINE VARIABLE nrcol AS INTEGER EXTENT 200 NO-UNDO.
DEFINE TEMP-TABLE sumaotemp NO-UNDO
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR   
   FIELD ORT LIKE AONRTAB.ORT 
   FIELD OPRIS AS DECIMAL
   FIELD K2 AS CHARACTER
   FIELD SATS% AS INTEGER
   INDEX AONR IS PRIMARY AONR DELNR K2
   INDEX K2 K2.
DEFINE TEMP-TABLE rubriktemp NO-UNDO   
   FIELD PLACERING AS INTEGER
   FIELD K2 AS CHARACTER
   INDEX K2 IS PRIMARY K2
   INDEX PLACERING PLACERING.



FIND FIRST uppvaltemp NO-ERROR.

RUN summa_UI.
RUN huvud_UI.
PROCEDURE huvud_UI :  
   CREATE tidut. 
   SUBSTRING(tidut.UT,60) = STRING(TODAY) + " " + STRING(TIME,"HH:MM").
   CREATE tidut.
   {KUURV.I}
   CREATE tidut.
   tidut.UT = uppvaltemp.VALDLISTA. 
   CREATE tidut.
   ASSIGN
   nrcol[1] = 1         
   nrcol[2] = 2         
   nrcol[3] = 3         
   nrcol[4] = 4.        
   i = 5.
   FOR EACH rubriktemp:
      IF i <= 200 THEN DO:
         ASSIGN
         nrcol[i] = i
         bredd[i] = 5.
         i = i + 1.        
      END.           
   END.
   ASSIGN
   antalk3 = i
   breddantal = i   /*antal kolumner*/
   bredd[1] = 6
   bredd[2] = 5
   bredd[3] = 15
   bredd[4] = 10.
   
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
   
   FIND FIRST KBENAMNING USE-INDEX KBEN NO-LOCK NO-ERROR.
   CREATE tidut.             
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = KBENAMNING.K2.
   CREATE tidut.
   ASSIGN                                                                                                       
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = CAPS(Guru.Konstanter:gaok)             
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = CAPS(Guru.Konstanter:gdelnrk)        
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = CAPS(Guru.Konstanter:gaonamnk)                                       
   /*SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "%"*/
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "PRIS".
   FOR EACH rubriktemp USE-INDEX PLACERING:
      SUBSTRING(tidut.UT,utnr[nrcol[rubriktemp.PLACERING]]) = rubriktemp.K2.
   END.
   CREATE tidut.     
   tidut.UT = str.
   FOR EACH valdaao USE-INDEX AONR:      
      FIND FIRST sumaotemp WHERE sumaotemp.AONR = valdaao.AONR AND  sumaotemp.DELNR = valdaao.DELNR NO-LOCK NO-ERROR.
      IF AVAILABLE sumaotemp THEN DO:
         CREATE tidut.                 
         ASSIGN 
         SUBSTRING(tidut.UT,utnr[nrcol[1]]) = sumaotemp.AONR
         SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(sumaotemp.DELNR,Guru.Konstanter:varforetypchar[1])
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = SUBSTRING(sumaotemp.ORT,1,bredd[3])              
        /* SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(sumaotemp.SATS%,">>9")*/
         SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(sumaotemp.OPRIS,">>>>>>>>>9").
         FOR EACH sumaotemp WHERE sumaotemp.AONR = valdaao.AONR AND  sumaotemp.DELNR = valdaao.DELNR:
            FIND FIRST rubriktemp WHERE rubriktemp.K2 = sumaotemp.K2 NO-LOCK NO-ERROR.
            IF AVAILABLE rubriktemp THEN DO:
               SUBSTRING(tidut.UT,utnr[nrcol[rubriktemp.PLACERING]]) = STRING(sumaotemp.SATS%,">>9") + "%".
            END.
         END.
      END.
   END.
END PROCEDURE.

PROCEDURE summa_UI.
   OPEN QUERY aq FOR EACH valdaao,
   EACH AONRKONTKOD WHERE AONRKONTKOD.AONR = valdaao.AONR AND AONRKONTKOD.DELNR = valdaao.DELNR NO-LOCK.
   GET FIRST aq NO-LOCK.
   DO WHILE AVAILABLE(valdaao):      
      FIND FIRST FAKTAONR WHERE FAKTAONR.AONR = valdaao.AONR AND FAKTAONR.DELNR = valdaao.DELNR NO-LOCK NO-ERROR.
      CREATE sumaotemp.
      ASSIGN
      sumaotemp.AONR = valdaao.AONR 
      sumaotemp.DELNR = valdaao.DELNR
      sumaotemp.ORT = valdaao.ORT
      sumaotemp.K2 = AONRKONTKOD.K2 
      sumaotemp.SATS% = AONRKONTKOD.SATS%.
      IF AVAILABLE FAKTAONR THEN DO:
         sumaotemp.OPRIS = FAKTAONR.OPRIS.
      END.
      GET NEXT aq NO-LOCK.
   END.      
   i = 4.
   FOR EACH sumaotemp BREAK BY sumaotemp.K2: 
      ACCUMULATE sumaotemp.SATS% (COUNT BY sumaotemp.K2).
      IF LAST-OF(sumaotemp.K2) THEN DO:
         IF sumaotemp.K2 NE "" THEN DO:
            CREATE rubriktemp.
            i = i + 1.
            ASSIGN
            rubriktemp.PLACERING = i
            rubriktemp.K2 = sumaotemp.K2.           
         END.         
      END.
   END.
   
END PROCEDURE.   
