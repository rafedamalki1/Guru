/*AOUTANA.P*/
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
{TIDUTTTNEW.I}

DEFINE INPUT PARAMETER valvar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER valomr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER lopfast AS LOGICAL NO-UNDO.  
DEFINE INPUT PARAMETER valfak AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER valanv AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER seallt AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR tidut.

DEFINE VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.  
DEFINE VARIABLE fbestapph2 AS HANDLE NO-UNDO.

IF lopfast = FALSE THEN DO:
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "elpa" THEN DO: 
      OPEN QUERY aonrq FOR EACH AONRTAB WHERE AONRTAB.FASTAAONR = FALSE AND          
      AONRTAB.FAKTNR = 0 NO-LOCK 
      BY AONRTAB.BESTID BY AONRTAB.AONR BY AONRTAB.DELNR.  
   END.
   ELSE DO:
      IF valvar = 1 THEN DO:
         OPEN QUERY aonrq FOR EACH AONRTAB WHERE AONRTAB.ANVANDARE = valanv AND          
         AONRTAB.FAKTNR = 0 NO-LOCK 
         BY AONRTAB.BESTID BY AONRTAB.ANVANDARE BY AONRTAB.AONR BY AONRTAB.DELNR.    
      END.
      IF valvar = 2 THEN DO:
         /*HUVUDANSV*/
         OPEN QUERY aonrq FOR EACH AONRTAB WHERE AONRTAB.DELNR = 0 AND 
         AONRTAB.ANVANDARE = valanv AND AONRTAB.FAKTNR = 0 NO-LOCK 
         BY AONRTAB.BESTID BY AONRTAB.ANVANDARE BY AONRTAB.AONR BY AONRTAB.DELNR.    
      END.
      IF valvar = 3 THEN DO:
         IF valomr = Guru.Konstanter:gomrk + " : alla" THEN DO:
            OPEN QUERY aonrq FOR EACH AONRTAB WHERE AONRTAB.FAKTNR = 0 NO-LOCK 
            BY AONRTAB.BESTID BY AONRTAB.ANVANDARE BY AONRTAB.AONR BY AONRTAB.DELNR.  
         END.      
         ELSE DO:
            FIND FIRST OMRADETAB WHERE OMRADETAB.NAMN = valomr NO-LOCK NO-ERROR.
            OPEN QUERY aonrq FOR EACH AONRTAB WHERE AONRTAB.OMRADE = OMRADETAB.OMRADE AND 
            AONRTAB.FAKTNR = 0 NO-LOCK 
            BY AONRTAB.BESTID BY AONRTAB.ANVANDARE BY AONRTAB.AONR BY AONRTAB.DELNR.
         END.         
      END.      
   END.
END.
ELSE DO:
   IF valvar = 1 THEN DO:
      OPEN QUERY aonrq FOR EACH AONRTAB WHERE AONRTAB.ANVANDARE = valanv AND          
      AONRTAB.FAKTNR NE 0 NO-LOCK 
      BY AONRTAB.BESTID BY AONRTAB.ANVANDARE BY AONRTAB.AONR BY AONRTAB.DELNR.          
   END.
   IF valvar = 2 THEN DO:
      OPEN QUERY aonrq FOR EACH AONRTAB WHERE AONRTAB.DELNR = 0 AND 
      AONRTAB.ANVANDARE = valanv AND AONRTAB.FAKTNR NE 0 NO-LOCK 
      BY AONRTAB.BESTID BY AONRTAB.ANVANDARE BY AONRTAB.AONR BY AONRTAB.DELNR.    
   END.
   IF valvar = 3 THEN DO:
      IF seallt = TRUE THEN DO:
         IF valomr = Guru.Konstanter:gomrk + " : alla" THEN DO:
            OPEN QUERY aonrq FOR EACH AONRTAB WHERE AONRTAB.FAKTNR NE 0 NO-LOCK 
            BY AONRTAB.BESTID BY AONRTAB.ANVANDARE BY AONRTAB.AONR BY AONRTAB.DELNR.  
         END.      
         ELSE DO:
            FIND FIRST OMRADETAB WHERE OMRADETAB.NAMN = valomr NO-LOCK NO-ERROR.
            OPEN QUERY aonrq FOR EACH AONRTAB WHERE AONRTAB.OMRADE = OMRADETAB.OMRADE AND 
            AONRTAB.FAKTNR NE 0 NO-LOCK 
            BY AONRTAB.BESTID BY AONRTAB.ANVANDARE BY AONRTAB.AONR BY AONRTAB.DELNR.
         END.         
      END.
      ELSE DO:
         IF valomr = Guru.Konstanter:gomrk + " : alla" THEN DO:
            OPEN QUERY aonrq FOR EACH AONRTAB WHERE AONRTAB.ANVANDARE = valanv AND AONRTAB.FAKTNR NE 0 NO-LOCK 
            BY AONRTAB.BESTID BY AONRTAB.ANVANDARE BY AONRTAB.AONR BY AONRTAB.DELNR.  
         END.      
         ELSE DO:
            FIND FIRST OMRADETAB WHERE OMRADETAB.NAMN = valomr NO-LOCK NO-ERROR.
            OPEN QUERY aonrq FOR EACH AONRTAB WHERE AONRTAB.ANVANDARE = valanv AND AONRTAB.OMRADE = OMRADETAB.OMRADE AND 
            AONRTAB.FAKTNR NE 0 NO-LOCK 
            BY AONRTAB.BESTID BY AONRTAB.ANVANDARE BY AONRTAB.AONR BY AONRTAB.DELNR.
         END.         
      END.
   END.
END.   
GET FIRST aonrq NO-LOCK.
IF AVAILABLE AONRTAB THEN DO:
   /*faktfor*/
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "elpa" THEN DO: 
     RUN EXTRATABHMT.P PERSISTENT SET fbestapph2.
   END.
   RUN huvud_UI.
   DO WHILE AVAILABLE(AONRTAB):
      RUN summa_UI. 
      GET NEXT aonrq NO-LOCK. 
   END.
   IF VALID-HANDLE(fbestapph2) THEN DELETE PROCEDURE fbestapph2.
   fbestapph2 = ?.

END.
ELSE DO:
   CREATE tidut.
   IF lopfast = FALSE THEN DO:              
      tidut.UT = "Det finns inga " + LC(Guru.Konstanter:gaok) + " utan fakturor!".
   END.
   ELSE DO:
      tidut.UT = "Det finns inga " + LC(Guru.Konstanter:gaok) + " kopplade till fakturor!".
   END.
END.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI WINDOW-2 
PROCEDURE huvud_UI :                                                               
   str = "===========.====================.========.================.=============.===============.===========================".        
   CREATE tidut. 
   SUBSTRING(tidut.UT,60) = STRING(TODAY).
   CREATE tidut.
   CREATE tidut.                             
   IF lopfast = FALSE THEN DO:              
      tidut.UT = CAPS(Guru.Konstanter:gaok) + " UTAN FAKTURAPLANER".    
   END.
   ELSE DO:  
      tidut.UT = CAPS(Guru.Konstanter:gaok) + " KOPPLADE TILL FAKTURAPLANER". 
   END.
   CREATE tidut.                    
   CREATE tidut.
   CREATE tidut.      
   ASSIGN                   
   SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok)   
   SUBSTRING(tidut.UT,13) = CAPS(Guru.Konstanter:gaonamnk)                         
   SUBSTRING(tidut.UT,34) = "ANSVARIG"                             
   SUBSTRING(tidut.UT,43) = "TIDSKRIV.STARTAD"
   SUBSTRING(tidut.UT,60) = CAPS(Guru.Konstanter:gaok) + " AVSLUT"
   SUBSTRING(tidut.UT,74) = "FAKTURA TYP".
   IF VALID-HANDLE(fbestapph2) THEN DO:
      SUBSTRING(tidut.UT,90) = "FAKTURA BESTÄLLARE " + CAPS(Guru.Konstanter:gaok).   
   END.      
   CREATE tidut.       
   SUBSTRING(tidut.UT,1) = str.                         
END PROCEDURE.
   
PROCEDURE summa_UI :   
           
   IF valfak = "Alla" THEN musz = musz.
   ELSE IF AONRTAB.FAKTTYP NE valfak THEN RETURN.
   
   IF AONRTAB.STARTDATUM = ? THEN DO:       
       IF AONRTAB.AONRAVDATUM = 01/01/91 THEN musz = musz.
      ELSE RETURN.       
   END.    
   CREATE tidut.                                 
   ASSIGN        
   SUBSTRING(tidut.UT,1) = AONRTAB.AONR   
   SUBSTRING(tidut.UT,8) = STRING(AONRTAB.DELNR,Guru.Konstanter:varforetypchar[1])                                                       
   SUBSTRING(tidut.UT,13,20) = STRING(AONRTAB.ORT,"X(20)")   
   SUBSTRING(tidut.UT,34) = STRING(AONRTAB.ANVANDARE,"X(6)").  
   IF AONRTAB.STARTDATUM = ? THEN musz = musz.
   ELSE SUBSTRING(tidut.UT,43) = STRING(AONRTAB.STARTDATUM).   
   IF AONRTAB.AONRAVDATUM = 01/01/91 THEN musz = musz.
   ELSE SUBSTRING(tidut.UT,60) = STRING(AONRTAB.AONRAVDATUM).      
   SUBSTRING(tidut.UT,74) = SUBSTRING(faktyp(AONRTAB.FAKTTYP),1,15). 
   IF VALID-HANDLE(fbestapph2) THEN DO:
      EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
      CREATE inextrakopptemp.          
      ASSIGN
      inextrakopptemp.PROGRAM = "FBAONR"                   
      inextrakopptemp.KOPPLACHAR1 = AONRTAB.AONR       
      inextrakopptemp.KOPPLAINT1 =  AONRTAB.DELNR      
      inextrakopptemp.KOPPLACHAR2 = ?            
      inextrakopptemp.KOPPLAINT2 =  ?.
      RUN etabhamt_UI IN fbestapph2 (INPUT TABLE inextrakopptemp,OUTPUT TABLE extrakopptemp).        
      FOR EACH extrakopptemp:
         SUBSTRING(tidut.UT,90) = extrakopptemp.KOPPLACHAR2 + " " + STRING(extrakopptemp.KOPPLAINT2,">99") + " " + extrakopptemp.SOKCHAR[1].            
      END.
      EMPTY TEMP-TABLE inextrakopptemp NO-ERROR.               
      GET NEXT aonrq NO-LOCK.
   END.       
END PROCEDURE.
