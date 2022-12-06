/*BAAPP.P*/
FUNCTION klockan100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):

  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.


FUNCTION klockan60 RETURNS DECIMAL
  ( INPUT ber100 AS DECIMAL ):
  RETURN TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) / 100) * 60 . 

END FUNCTION.
{TIDUTTT.I}
{EXTRADATA.I}
DEFINE TEMP-TABLE sumaotemp    
   FIELD AONR LIKE AONRTAB.AONR
   FIELD DELNR LIKE AONRTAB.DELNR         
   FIELD ORT LIKE AONRTAB.ORT 
   FIELD BEREDARE LIKE TIDREGITAB.PERSONALKOD
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD NAMN AS CHARACTER
   FIELD DATUM LIKE TIDREGITAB.DATUM 
   FIELD SLUT LIKE TIDREGITAB.SLUT
   FIELD START LIKE TIDREGITAB.START
   FIELD TOT AS DECIMAL
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP
   FIELD NY AS LOGICAL 
   FIELD ANM AS CHARACTER
   INDEX PKOD IS PRIMARY PERSONALKOD
   INDEX AONR AONR DELNR PRISTYP PERSONALKOD.

DEFINE TEMP-TABLE sumaotemp2    
   FIELD AONR LIKE AONRTAB.AONR
   FIELD DELNR LIKE AONRTAB.DELNR         
   FIELD ORT LIKE AONRTAB.ORT 
   FIELD BEREDARE LIKE TIDREGITAB.PERSONALKOD
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD NAMN AS CHARACTER
   FIELD DATUM LIKE TIDREGITAB.DATUM 
   FIELD SLUT LIKE TIDREGITAB.SLUT
   FIELD START LIKE TIDREGITAB.START
   FIELD TOT AS DECIMAL
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP
   FIELD NY AS LOGICAL 
   INDEX PKOD IS PRIMARY PERSONALKOD
   INDEX AONR BEREDARE AONR DELNR PRISTYP PERSONALKOD.
DEFINE TEMP-TABLE sumaotemp3    
   FIELD AONR LIKE AONRTAB.AONR
   FIELD DELNR LIKE AONRTAB.DELNR         
   FIELD ORT LIKE AONRTAB.ORT 
   FIELD BEREDARE LIKE TIDREGITAB.PERSONALKOD
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD NAMN AS CHARACTER
   FIELD DATUM LIKE TIDREGITAB.DATUM 
   FIELD SLUT LIKE TIDREGITAB.SLUT
   FIELD START LIKE TIDREGITAB.START
   FIELD TOT AS DECIMAL
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP
   FIELD NY AS LOGICAL 
   INDEX PKOD IS PRIMARY PERSONALKOD
   INDEX AONR BEREDARE AONR DELNR PRISTYP PERSONALKOD.

DEFINE TEMP-TABLE vispers
   FIELD PERSONALKOD LIKE PERSONALTAB.PERSONALKOD
   FIELD FORNAMN LIKE PERSONALTAB.FORNAMN 
   FIELD EFTERNAMN LIKE PERSONALTAB.EFTERNAMN
   INDEX PKOD IS PRIMARY PERSONALKOD.
DEFINE TEMP-TABLE valtemp
   FIELD VALDLISTA AS CHARACTER
   FIELD BAVAL AS INTEGER
   FIELD ALLTID AS LOGICAL
   FIELD STARTDATUM AS DATE
   FIELD SLUTDATUM AS DATE.

DEFINE INPUT PARAMETER TABLE FOR valtemp.
DEFINE INPUT PARAMETER TABLE FOR vispers.
DEFINE INPUT  PARAMETER ganv AS CHARACTER NO-UNDO.      
DEFINE OUTPUT PARAMETER TABLE FOR tidut.
DEFINE OUTPUT PARAMETER str AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE OUTPUT PARAMETER str2 AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE OUTPUT PARAMETER str3 AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE pkod AS CHARACTER NO-UNDO.
DEFINE VARIABLE utnr AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE tisek AS LOGICAL NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.

RUN STYRFORE.P (INPUT Guru.Konstanter:globforetag). 

DEFINE VARIABLE sealltid AS LOGICAL NO-UNDO.
RUN kollsealltid_UI (INPUT Guru.Konstanter:globanv ,OUTPUT sealltid).

FIND FIRST valtemp NO-ERROR.
RUN open_UI.
RUN summa_UI.
RUN huvud_UI.
{GDPRLOGGCLIENT.I}
PROCEDURE huvud_UI :
   CREATE tidut. 
   SUBSTRING(tidut.UT,60) = STRING(TODAY) + " " + STRING(TIME,"HH:MM").
   CREATE tidut.
   CREATE tidut.
   tidut.UT = valtemp.VALDLISTA. 
   IF valtemp.BAVAL = 1 OR valtemp.BAVAL = 11  THEN DO: 
      SUBSTRING(tidut.UT,35) = Guru.Konstanter:gberek. /*"för beredare".*/
   END.
   IF valtemp.BAVAL = 2 OR valtemp.BAVAL = 12 THEN DO:
      SUBSTRING(tidut.UT,35) = Guru.Konstanter:gprojk. /*"för projektörer".     */
   END.
   IF valtemp.BAVAL = 3 OR valtemp.BAVAL = 13 THEN DO:
      SUBSTRING(tidut.UT,35) = Guru.Konstanter:garbak. /*"för arbetsansvariga".     */
   END.
   FOR EACH vispers:
      CREATE tidut.
      ASSIGN
      tidut.UT = vispers.PERSONALKOD + " " + vispers.FORNAMN + " " + vispers.EFTERNAMN.
   END.
   CREATE tidut.
   ASSIGN
   utnr[1] = 1
   utnr[2] = 12
   utnr[3] = 30
   utnr[4] = 41
   utnr[5] = 51
   utnr[6] = 58
   utnr[7] = 70
   utnr[8] = 79 
   utnr[9] = 85   
   utnr[10] = 91   
   str = "".      
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   IF valtemp.BAVAL < 10 THEN DO:
      DO WHILE i <= 88:
         i = i + 1.
         str = str + "=".      
      END.   
      i = 2.      
      DO WHILE i <= 9:             
         SUBSTRING(str,(utnr[i] - 1),1) = ".".      
         i = i + 1.
      END.                                
   END.
   ELSE DO:
      DO WHILE i <= 132:
         i = i + 1.
         str = str + "=".      
      END.   
      i = 2. 
      DO WHILE i <= 10:             
         SUBSTRING(str,(utnr[i] - 1),1) = ".".      
         i = i + 1.
      END.                                
   END.   
   CREATE tidut.
   SUBSTRING(tidut.UT,utnr[5]) = "ENHET/".
   str2 = tidut.UT.
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[1]) = CAPS(Guru.Konstanter:gaok)
   SUBSTRING(tidut.UT,utnr[2]) = CAPS(Guru.Konstanter:gaonamnk) 
   SUBSTRING(tidut.UT,utnr[3]) = CAPS(Guru.Konstanter:gdebk)  
   SUBSTRING(tidut.UT,utnr[4]) = "TOTALT"
   SUBSTRING(tidut.UT,utnr[5]) = "SIGN"
   SUBSTRING(tidut.UT,utnr[6]) = "NAMN"
   SUBSTRING(tidut.UT,utnr[7]) = "DATUM"      
   SUBSTRING(tidut.UT,utnr[8]) = "START"      
   SUBSTRING(tidut.UT,utnr[9]) = "SLUT".  
   IF valtemp.BAVAL GE 10 THEN DO:
      SUBSTRING(tidut.UT,utnr[10]) = "KOMMENTAR".
   END.
   str3 = tidut.UT.                
   CREATE tidut.
   tidut.UT = str.
   FOR EACH vispers:
      CREATE tidut.
      CREATE tidut.
      ASSIGN
      tidut.UT = vispers.PERSONALKOD + " " + vispers.FORNAMN + " " + vispers.EFTERNAMN + 
      " -SUMMA PER " + CAPS(Guru.Konstanter:gaok).
      CREATE tidut.
      FOR EACH sumaotemp3 WHERE sumaotemp3.BEREDARE = vispers.PERSONALKOD USE-INDEX AONR:
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,utnr[1]) = sumaotemp3.AONR + " " + STRING(sumaotemp3.DELNR,Guru.Konstanter:varforetypchar[1])
         SUBSTRING(tidut.UT,utnr[2]) = SUBSTRING(sumaotemp3.ORT,1,17)      
         SUBSTRING(tidut.UT,utnr[3]) = sumaotemp3.PRISTYP    
         SUBSTRING(tidut.UT,utnr[4]) = STRING(sumaotemp3.TOT,"->>>>>9.9").     
      END.     
   END.      
   FOR EACH vispers:
      CREATE tidut.
      CREATE tidut.
      ASSIGN
      tidut.UT = vispers.PERSONALKOD + " " + vispers.FORNAMN + " " + vispers.EFTERNAMN + 
      " -SUMMA PER PERSON OCH " + CAPS(Guru.Konstanter:gaok).      
      CREATE tidut.
      FOR EACH sumaotemp2 WHERE sumaotemp2.BEREDARE = vispers.PERSONALKOD USE-INDEX AONR:
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,utnr[1]) = sumaotemp2.AONR + " " + STRING(sumaotemp2.DELNR,Guru.Konstanter:varforetypchar[1])
         SUBSTRING(tidut.UT,utnr[2]) = SUBSTRING(sumaotemp2.ORT,1,17)      
         SUBSTRING(tidut.UT,utnr[3]) = sumaotemp2.PRISTYP  
         SUBSTRING(tidut.UT,utnr[4]) = STRING(sumaotemp2.TOT,"->>>>>9.9")
         SUBSTRING(tidut.UT,utnr[5]) = sumaotemp2.PERSONALKOD
         SUBSTRING(tidut.UT,utnr[6]) = SUBSTRING(sumaotemp2.NAMN,1,11).
      END.     
   END. 
   FOR EACH vispers:
      CREATE tidut.
      CREATE tidut.
      ASSIGN
      tidut.UT = vispers.PERSONALKOD + " " + vispers.FORNAMN + " " + vispers.EFTERNAMN + 
      " -SUMMA PER PERSON MED KLOCKSLAG OCH " + CAPS(Guru.Konstanter:gaok).
      CREATE tidut.
      FOR EACH sumaotemp WHERE sumaotemp.BEREDARE = vispers.PERSONALKOD USE-INDEX AONR:
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,utnr[1]) = sumaotemp.AONR + " " + STRING(sumaotemp.DELNR,Guru.Konstanter:varforetypchar[1])
         SUBSTRING(tidut.UT,utnr[2]) = SUBSTRING(sumaotemp.ORT,1,17)      
         SUBSTRING(tidut.UT,utnr[3]) = sumaotemp.PRISTYP  
         SUBSTRING(tidut.UT,utnr[4]) = STRING(sumaotemp.TOT,"->>>>>9.9")
         SUBSTRING(tidut.UT,utnr[5]) = sumaotemp.PERSONALKOD
         SUBSTRING(tidut.UT,utnr[6]) = SUBSTRING(sumaotemp.NAMN,1,11)
         SUBSTRING(tidut.UT,utnr[7]) = STRING(sumaotemp.DATUM)
         SUBSTRING(tidut.UT,utnr[8]) = STRING(sumaotemp.START,"99.99")
         SUBSTRING(tidut.UT,utnr[9]) = STRING(sumaotemp.SLUT,"99.99").
         IF valtemp.BAVAL GE 10 THEN DO:
            SUBSTRING(tidut.UT,utnr[10]) = SUBSTRING(sumaotemp.ANM,1,40).
         END.
      END.     
   END.
END PROCEDURE.
PROCEDURE open_UI :
   OPEN QUERY vpq FOR EACH vispers NO-LOCK. 
   GET FIRST vpq NO-LOCK.
   DO WHILE AVAILABLE(vispers): 
      IF valtemp.BAVAL = 1 OR valtemp.BAVAL = 11 THEN DO:
         IF valtemp.ALLTID = FALSE THEN DO:
            OPEN QUERY atq FOR EACH AONRTAB WHERE  
            AONRTAB.BEREDARE = vispers.PERSONALKOD NO-LOCK,
            EACH TIDREGITAB WHERE TIDREGITAB.VECKOKORD = "" AND
            TIDREGITAB.TIDLOG = TRUE AND
            TIDREGITAB.AONR = AONRTAB.AONR AND
            TIDREGITAB.DELNR = AONRTAB.DELNR NO-LOCK.
         END.         
         ELSE DO:
            OPEN QUERY atq FOR EACH AONRTAB WHERE  
            AONRTAB.BEREDARE = vispers.PERSONALKOD NO-LOCK,
            EACH TIDREGITAB WHERE TIDREGITAB.TIDLOG = TRUE AND
            TIDREGITAB.AONR = AONRTAB.AONR AND
            TIDREGITAB.DELNR = AONRTAB.DELNR AND
            TIDREGITAB.DATUM >= valtemp.STARTDATUM AND
            TIDREGITAB.DATUM <= valtemp.SLUTDATUM 
            NO-LOCK.
            OPEN QUERY atfq FOR EACH AONRTAB WHERE  
            AONRTAB.BEREDARE = vispers.PERSONALKOD NO-LOCK,
            EACH TIDFEL WHERE TIDFEL.TIDLOG = TRUE AND
            TIDFEL.AONR = AONRTAB.AONR AND
            TIDFEL.DELNR = AONRTAB.DELNR AND
            TIDFEL.DATUM >= valtemp.STARTDATUM AND
            TIDFEL.DATUM <= valtemp.SLUTDATUM 
            NO-LOCK.         
         END.
      END.
      ELSE IF valtemp.BAVAL = 2 OR valtemp.BAVAL = 12 THEN DO: 
         IF valtemp.ALLTID = FALSE THEN DO: 
            OPEN QUERY atq FOR EACH AONRTAB WHERE  
            AONRTAB.STARTDAG = vispers.PERSONALKOD NO-LOCK,
            EACH TIDREGITAB WHERE TIDREGITAB.VECKOKORD = "" AND
            TIDREGITAB.TIDLOG = TRUE AND
            TIDREGITAB.AONR = AONRTAB.AONR AND
            TIDREGITAB.DELNR = AONRTAB.DELNR NO-LOCK.
         END.
         ELSE DO:
            OPEN QUERY atq FOR EACH AONRTAB WHERE  
            AONRTAB.STARTDAG = vispers.PERSONALKOD NO-LOCK,
            EACH TIDREGITAB WHERE TIDREGITAB.TIDLOG = TRUE AND
            TIDREGITAB.AONR = AONRTAB.AONR AND
            TIDREGITAB.DELNR = AONRTAB.DELNR AND
            TIDREGITAB.DATUM >= valtemp.STARTDATUM AND
            TIDREGITAB.DATUM <= valtemp.SLUTDATUM 
            NO-LOCK.
            OPEN QUERY atfq FOR EACH AONRTAB WHERE  
            AONRTAB.STARTDAG = vispers.PERSONALKOD NO-LOCK,
            EACH TIDFEL WHERE TIDFEL.TIDLOG = TRUE AND
            TIDFEL.AONR = AONRTAB.AONR AND
            TIDFEL.DELNR = AONRTAB.DELNR AND
            TIDFEL.DATUM >= valtemp.STARTDATUM AND
            TIDFEL.DATUM <= valtemp.SLUTDATUM 
            NO-LOCK.         
         END.
      END.
      ELSE IF valtemp.BAVAL = 3 OR valtemp.BAVAL = 13 THEN DO: 
         IF valtemp.ALLTID = FALSE THEN DO: 
            OPEN QUERY atq FOR EACH AONRTAB WHERE  
            AONRTAB.ARBANSVARIG = vispers.PERSONALKOD NO-LOCK,
            EACH TIDREGITAB WHERE TIDREGITAB.VECKOKORD = "" AND
            TIDREGITAB.TIDLOG = TRUE AND
            TIDREGITAB.AONR = AONRTAB.AONR AND
            TIDREGITAB.DELNR = AONRTAB.DELNR NO-LOCK.
         END.
         ELSE DO:
            OPEN QUERY atq FOR EACH AONRTAB WHERE  
            AONRTAB.ARBANSVARIG = vispers.PERSONALKOD NO-LOCK,
            EACH TIDREGITAB WHERE TIDREGITAB.TIDLOG = TRUE AND
            TIDREGITAB.AONR = AONRTAB.AONR AND
            TIDREGITAB.DELNR = AONRTAB.DELNR AND
            TIDREGITAB.DATUM >= valtemp.STARTDATUM AND
            TIDREGITAB.DATUM <= valtemp.SLUTDATUM 
            NO-LOCK.
            OPEN QUERY atfq FOR EACH AONRTAB WHERE  
            AONRTAB.ARBANSVARIG = vispers.PERSONALKOD NO-LOCK,
            EACH TIDFEL WHERE TIDFEL.TIDLOG = TRUE AND
            TIDFEL.AONR = AONRTAB.AONR AND
            TIDFEL.DELNR = AONRTAB.DELNR AND
            TIDFEL.DATUM >= valtemp.STARTDATUM AND
            TIDFEL.DATUM <= valtemp.SLUTDATUM 
            NO-LOCK.         
         END.
      END.
      GET FIRST atq NO-LOCK.
      DO WHILE AVAILABLE(AONRTAB):
         {ANVTIDSEKKOLL.I}
         IF sealltid = TRUE THEN tisek = TRUE.
         IF tisek = TRUE THEN DO:              
            CREATE sumaotemp.
            ASSIGN      
            sumaotemp.AONR = AONRTAB.AONR
            sumaotemp.DELNR = AONRTAB.DELNR         
            sumaotemp.ORT = AONRTAB.ORT            
            sumaotemp.BEREDARE = vispers.PERSONALKOD 
            sumaotemp.PERSONALKOD = TIDREGITAB.PERSONALKOD 
            sumaotemp.DATUM = TIDREGITAB.DATUM 
            sumaotemp.SLUT = TIDREGITAB.SLUT
            sumaotemp.START = TIDREGITAB.START         
            sumaotemp.PRISTYP = TIDREGITAB.PRISTYP
            sumaotemp.NY = FALSE.
            sumaotemp.TOT = klockan100(TIDREGITAB.TOTALT).
            sumaotemp.ANM = TIDREGITAB.RESMAL.
         END.   
         GET NEXT atq NO-LOCK.
      END.
      IF valtemp.ALLTID = TRUE THEN DO:
         GET FIRST atfq NO-LOCK.
         DO WHILE AVAILABLE(AONRTAB):
            {ANVTIDFSEKKOLL.I}
            IF sealltid = TRUE THEN tisek = TRUE.
            IF tisek = TRUE THEN DO:           
               CREATE sumaotemp.
               ASSIGN      
               sumaotemp.AONR = AONRTAB.AONR
               sumaotemp.DELNR = AONRTAB.DELNR         
               sumaotemp.ORT = AONRTAB.ORT            
               sumaotemp.BEREDARE = vispers.PERSONALKOD 
               sumaotemp.PERSONALKOD = TIDFEL.PERSONALKOD 
               sumaotemp.DATUM = TIDFEL.DATUM 
               sumaotemp.SLUT = TIDFEL.SLUT
               sumaotemp.START = TIDFEL.START         
               sumaotemp.PRISTYP = TIDFEL.PRISTYP
               sumaotemp.NY = FALSE.         
               sumaotemp.ANM = TIDFEL.RESMAL.
               IF TIDFEL.DEBET = TRUE THEN sumaotemp.TOT = klockan100(TIDFEL.TOTALT).
               ELSE sumaotemp.TOT = klockan100(TIDFEL.TOTALT) * ( -1).
            END.   
            GET NEXT atfq NO-LOCK.
         END. 
      END.
      GET NEXT vpq NO-LOCK.
   END.
       
   
   REPEAT:
      FIND FIRST sumaotemp WHERE sumaotemp.NY = FALSE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE sumaotemp THEN LEAVE.
      pkod = sumaotemp.PERSONALKOD.
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod NO-LOCK NO-ERROR.
      FOR EACH sumaotemp WHERE sumaotemp.PERSONALKOD = pkod:
         sumaotemp.NY = TRUE.
         IF AVAILABLE PERSONALTAB THEN sumaotemp.NAMN =
         SUBSTRING(PERSONALTAB.FORNAMN,1,1) + "." + PERSONALTAB.EFTERNAMN. 
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
      END.
   END.
END PROCEDURE.
PROCEDURE summa_UI.
   FOR EACH sumaotemp BREAK BY sumaotemp.BEREDARE BY sumaotemp.AONR BY 
      sumaotemp.DELNR BY sumaotemp.PERSONALKOD BY sumaotemp.PRISTYP: 
      ACCUMULATE sumaotemp.TOT (TOTAL BY sumaotemp.BEREDARE BY sumaotemp.AONR BY
      sumaotemp.DELNR BY sumaotemp.PERSONALKOD BY sumaotemp.PRISTYP).      
      IF LAST-OF(sumaotemp.PRISTYP) THEN DO:
         CREATE sumaotemp2.                  
         ASSIGN
         sumaotemp2.AONR  = sumaotemp.AONR 
         sumaotemp2.DELNR = sumaotemp.DELNR 
         sumaotemp2.ORT = sumaotemp.ORT 
         sumaotemp2.BEREDARE = sumaotemp.BEREDARE  
         sumaotemp2.PERSONALKOD = sumaotemp.PERSONALKOD
         sumaotemp2.NAMN = sumaotemp.NAMN
         sumaotemp2.PRISTYP = sumaotemp.PRISTYP          
         sumaotemp2.TOT =  
         (ACCUM TOTAL BY sumaotemp.PRISTYP sumaotemp.TOT).         
      END.   
   END. 
   FOR EACH sumaotemp2 BREAK BY sumaotemp2.BEREDARE BY sumaotemp2.AONR BY 
      sumaotemp2.DELNR BY sumaotemp2.PRISTYP: 
      ACCUMULATE sumaotemp2.TOT 
      (TOTAL BY sumaotemp2.BEREDARE BY sumaotemp2.AONR BY sumaotemp2.DELNR BY sumaotemp2.PRISTYP).      
      IF LAST-OF(sumaotemp2.PRISTYP) THEN DO:
         CREATE sumaotemp3.                  
         ASSIGN
         sumaotemp3.AONR = sumaotemp2.AONR 
         sumaotemp3.DELNR = sumaotemp2.DELNR 
         sumaotemp3.ORT = sumaotemp2.ORT 
         sumaotemp3.BEREDARE = sumaotemp2.BEREDARE  
         sumaotemp3.PRISTYP = sumaotemp2.PRISTYP          
         sumaotemp3.TOT =  
          (ACCUM TOTAL BY sumaotemp2.PRISTYP sumaotemp2.TOT).         
      END.   
   END.
END PROCEDURE.   
PROCEDURE kollsealltid_UI :
   DEFINE INPUT  PARAMETER ganv AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER setidall AS LOGICAL NO-UNDO.
   IF NOT VALID-HANDLE(edataapph) THEN RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   setidall = FALSE.   
   FIND FIRST ANVANDARE  WHERE ANVANDARE.ANVANDARE = ganv NO-LOCK NO-ERROR.
   IF AVAILABLE ANVANDARE THEN DO:        
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "SEALLTID"                   
      inextradatatemp.HUVUDCH = ANVANDARE.PERSONALKOD.  
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
      IF AVAILABLE extradatatemp THEN DO:      
         ASSIGN   setidall = extradatatemp.SOKLOG[1].                        
      END.   
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
      EMPTY TEMP-TABLE extradatatemp NO-ERROR.
   END.
END PROCEDURE.

