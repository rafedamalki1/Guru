/*INLOGRAP.P*/
/*Anders Olsson Elpool i Umeå AB  28 maj 2021 09:58:18 
KÖRS INTE 
*/
DEFINE INPUT PARAMETER anvdator LIKE ANVANDARE.ANVANDARE NO-UNDO.
DEFINE INPUT PARAMETER inutvar AS LOGICAL NO-UNDO. 
DEFINE VARIABLE globanv AS CHARACTER NO-UNDO.
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED   
{GLOBVAR2DEL1.I}   
/*{EGENBVAR.I}*/



  

DEFINE VARIABLE datornamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE globanvnt AS CHARACTER NO-UNDO.
DEFINE VARIABLE dbvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE filtest AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommandoinut AS CHARACTER NO-UNDO.
DEFINE VARIABLE antalvar AS INTEGER NO-UNDO.
DEFINE VARIABLE klientyp AS CHARACTER NO-UNDO.
DEFINE VARIABLE Urls AS CHARACTER NO-UNDO.
DEFINE VARIABLE InProg AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE intemp      
   FIELD DATUMIN AS DATE
   FIELD KLOCKAIN AS CHARACTER
   FIELD DATUMUT AS DATE
   FIELD KLOCKANUT AS CHARACTER
   FIELD ANTAL AS INTEGER  
   FIELD ANVANDARE LIKE ANVANDARE.ANVANDARE    
   FIELD DATOR AS CHARACTER   
   FIELD OSANVANDARE AS CHARACTER   
   FIELD GFORETAG AS CHARACTER   
   FIELD KLIENT AS CHARACTER
   FIELD PID AS INTEGER
   INDEX ANVANDARE IS PRIMARY ANVANDARE DATUMIN KLOCKAIN  
   INDEX DATUMIN DATUMIN
   INDEX DATUMUT DATUMUT
   INDEX DATOR DATOR DATUMIN .
   

DEFINE VARIABLE intProcessHandle AS INTEGER NO-UNDO.

PROCEDURE GetCurrentProcessId EXTERNAL "KERNEL32.DLL":
   DEFINE RETURN PARAMETER intProcessHandle AS LONG.
END PROCEDURE.

RUN GetCurrentProcessId (OUTPUT intProcessHandle).


   
DEFINE TEMP-TABLE ccintemp NO-UNDO LIKE intemp
 FIELD FORNAMN  AS CHARACTER
 FIELD EFTERNAMN AS CHARACTER
 FIELD OMRADE AS CHARACTER
 FIELD NAMN AS CHARACTER
 FIELD EPOST AS CHARACTER.   
{PROVAG.I}
{EXECLIN2.I}
ASSIGN
globanv = TRIM(SUBSTRING(anvdator,1,20))
datornamn =  TRIM(SUBSTRING(anvdator,25,20))
globanvnt = TRIM(SUBSTRING(anvdator,50,20)) 
klientyp  = TRIM(SUBSTRING(anvdator,75,30)).
Urls  = TRIM(SUBSTRING(anvdator,110,39)).
InProg = TRIM(SUBSTRING(anvdator,150)).
kommando = ?.

{AMERICANEUROPEAN.I}
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
   /*dbvar = "\\pc112\DELAD\PRO9S\DB\".*/
   kommando = dbvar + "LOGGRAPP.TXT".  /*statistik*/
   kommandoinut = dbvar + "LOGGIN.TXT".
END.
ELSE IF Guru.Konstanter:globforetag = "BORL" THEN DO:
   dbvar = "c:\DELAD\PRO9S\DB\".
   kommando = dbvar + "LOGGRAPP.TXT".  /*statistik*/
   kommandoinut = dbvar + "LOGGIN.TXT".
END.
ELSE IF Guru.Konstanter:globforetag = "BIRK" THEN DO:
   dbvar = "c:\DELAD\PRO9S\DB\".
   kommando = dbvar + "LOGGRAPP.TXT".  /*statistik*/
   kommandoinut = dbvar + "LOGGIN.TXT".
END.
ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
   dbvar = "D:\DELAD\PRO9S\DB\".
   kommando = dbvar + "LOGGRAPP.TXT".  /*statistik*/
   kommandoinut = dbvar + "LOGGIN.TXT".
END.
ELSE IF Guru.Konstanter:globforetag = "GRIT" OR 
        Guru.Konstanter:globforetag = "GADM"  OR Guru.Konstanter:globforetag = "GKAL"  THEN DO:
   dbvar = "d:\DELAD\server\PRO9S\DB\". 
   kommando = dbvar + "LOGGRAPP.TXT".  /*statistik*/
   kommandoinut = dbvar + "LOGGIN.TXT".
END.
ELSE IF Guru.Konstanter:globforetag = "GRAN"  THEN DO:
   dbvar = "d:\elpool\delad\pro9\wrk\". 
   kommando = dbvar + "LOGGRAPP.TXT".  /*statistik*/
   kommandoinut = dbvar + "LOGGIN.TXT".
END.
ELSE DO:    
   RUN PROVAG.P.
   kommando = Guru.Konstanter:guruvar + "LOGGRAPP.TXT".
   kommandoinut = Guru.Konstanter:guruvar + "LOGGIN.TXT".
END.
filtest = SEARCH(kommandoinut).
IF filtest = ? THEN DO:
   kommando = Guru.Konstanter:guruvar + "LOGGRAPP.TXT".    
   kommandoinut = Guru.Konstanter:guruvar + "LOGGIN.TXT".  
   /*blank rad*/
   CREATE intemp.
   OUTPUT TO VALUE(kommandoinut) APPEND.
   PUT dbvar.
   OUTPUT CLOSE.
END.

IF inutvar = FALSE THEN RUN ut_UI.
IF inutvar = TRUE THEN RUN in_UI. 
IF inutvar = ? THEN RUN statestik_UI.
{EUROPEANAMERICAN.I}   
RETURN.
PROCEDURE in_UI :
   
   /*KOLLAR HUR MÅNGA SOM ÄR INLOGGADE*/
   INPUT FROM VALUE(kommandoinut) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE intemp.
         ASSIGN.
         IMPORT intemp NO-ERROR.
      END.
   END. 
   FOR EACH intemp WHERE intemp.ANVANDARE = "":
      DELETE intemp.
   END.
   FOR EACH intemp WHERE intemp.DATUMUT = ?:
      ACCUMULATE intemp.ANVANDARE (COUNT).
   END.
  
   /*SKAPAR EN POST TILL*/
   CREATE intemp.
   ASSIGN
   intemp.ANVANDARE = globanv
   intemp.OSANVANDARE = globanvnt
   intemp.GFORETAG = Guru.Konstanter:globforetag
   intemp.DATOR = datornamn
   intemp.KLIENT = klientyp
   intemp.DATUMIN = TODAY
   intemp.KLOCKAIN = STRING(TIME,"HH:MM")   
   intemp.ANTAL = 1 + (ACCUM COUNT intemp.ANVANDARE) 
   intemp.PID = intProcessHandle.
   OUTPUT TO VALUE(kommandoinut).
   FOR EACH intemp BY intemp.DATUMIN BY intemp.KLOCKAIN BY intemp.ANVANDARE :
      EXPORT intemp.
   END. 
   OUTPUT CLOSE.
   DO TRANSACTION:
      
      CREATE LOGGRAPP.
      ASSIGN
      LOGGRAPP.ANVANDARE = globanv
      LOGGRAPP.OSANVANDARE = globanvnt
      LOGGRAPP.GFORETAG = Guru.Konstanter:globforetag
      LOGGRAPP.DATOR = datornamn
      LOGGRAPP.KLIENT = klientyp
      LOGGRAPP.DATUM = NOW
      LOGGRAPP.PID = intProcessHandle
      LOGGRAPP.PROGRAM = InProg
      LOGGRAPP.TYP = "IN"
      LOGGRAPP.URLSITE = Urls.
      
   END.
   RELEASE LOGGRAPP NO-ERROR.  
END PROCEDURE.
PROCEDURE ut_UI :
   /*VILKA ÄR INLOGGADE*/
   INPUT FROM VALUE(kommandoinut) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE intemp.
         ASSIGN.
         IMPORT intemp NO-ERROR.
      END.
   END. 
   FOR EACH intemp WHERE intemp.ANVANDARE = "":
      DELETE intemp.
   END.
   /*VEM SKA LOGGA UT*/
   FIND FIRST intemp WHERE intemp.ANVANDARE = globanv AND
   intemp.DATUMIN = TODAY AND intemp.DATUMUT = ? NO-ERROR.
   IF AVAILABLE intemp THEN DO:
      ASSIGN
      intemp.DATUMUT = TODAY
      intemp.KLOCKANUT = STRING(TIME,"HH:MM").
      OUTPUT TO VALUE(kommando) APPEND.   
      EXPORT intemp.
      OUTPUT CLOSE.
      DELETE intemp.
   END. 
   ELSE DO:
      /*HAR MAN VARIT INLOGGADE LÄNGE*/
      FIND FIRST intemp WHERE intemp.ANVANDARE = globanv AND
      intemp.DATUMUT = ? NO-ERROR.
      IF AVAILABLE intemp THEN DO:
         ASSIGN
         intemp.DATUMUT = TODAY
         intemp.KLOCKANUT = STRING(TIME,"HH:MM").
         OUTPUT TO VALUE(kommando) APPEND.   
         EXPORT intemp.
         OUTPUT CLOSE.
         DELETE intemp.
      END.
   END.
   /*RENSNING AV GAMMLA*/
   IF Guru.Konstanter:globforetag = "ELPA" OR 
   Guru.Konstanter:globforetag = "GRAN"  OR 
   Guru.Konstanter:globforetag = "GADM"  OR Guru.Konstanter:globforetag = "GKAL"  THEN DO:
      OUTPUT TO VALUE(kommando) APPEND.
      FOR EACH intemp WHERE intemp.ANVANDARE = globanv AND intemp.DATUMUT  = ?:
         ASSIGN
         intemp.DATUMUT = TODAY
         intemp.KLOCKANUT = "24:00".
         EXPORT intemp.
         DELETE intemp.
      END.
      OUTPUT CLOSE.
   END.
   ELSE DO:
      OUTPUT TO VALUE(kommando) APPEND.
      FOR EACH intemp WHERE intemp.DATUMIN NE TODAY:
         IF intemp.DATUMUT = ? THEN DO:
            ASSIGN
            intemp.DATUMUT = TODAY
            intemp.KLOCKANUT = "24:00".
         END.
         EXPORT intemp.
         DELETE intemp.
      END.
      OUTPUT CLOSE.
   END.
   OUTPUT TO VALUE(kommandoinut).
   FOR EACH intemp BY intemp.DATUMIN BY intemp.KLOCKAIN BY intemp.ANVANDARE :
      EXPORT intemp.
   END. 
   OUTPUT CLOSE.
END PROCEDURE.
PROCEDURE statestik_UI :
   /*VILKA ÄR INLOGGADE*/
   
   INPUT FROM VALUE(kommando) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE intemp.
         ASSIGN.
         IMPORT intemp NO-ERROR.
      END.
   END. 
   
   FOR EACH intemp WHERE intemp.ANVANDARE = "":
      DELETE intemp.
   END.    
   
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
      FOR EACH intemp USE-INDEX DATOR BREAK BY intemp.DATOR:
         ACCUMULATE intemp.ANTAL (TOTAL BY intemp.DATOR).
         ACCUMULATE intemp.ANTAL (COUNT BY intemp.DATOR).
         
         IF LAST-OF(intemp.DATOR) THEN DO: 
            antalvar = antalvar + 1.
            CREATE ccintemp.
            BUFFER-COPY intemp TO ccintemp.
            
         END.                                      
      END.
   END.
   ELSE DO:   
      FOR EACH intemp BREAK BY intemp.ANVANDARE:
         ACCUMULATE intemp.ANTAL (TOTAL BY intemp.ANVANDARE).
         ACCUMULATE intemp.ANTAL (COUNT BY intemp.ANVANDARE).
         
         IF LAST-OF(intemp.ANVANDARE) THEN DO: 
            antalvar = antalvar + 1.
            CREATE ccintemp.
            BUFFER-COPY intemp TO ccintemp.
            
         END.                                      
      END.
   END.           
/*
   DISPLAY antalvar.
   OUTPUT TO c:\temp\anv.txt.
   FOR EACH ccintemp:
      DISPLAY ccintemp.
   END.
   
   OUTPUT CLOSE.
*/
   FOR EACH ccintemp WHERE NO-LOCK:
      FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = ccintemp.ANVANDARE NO-LOCK NO-ERROR.
      IF AVAILABLE ANVANDARE THEN DO:
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = ANVANDARE.PERSONALKOD NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALTAB THEN DO:
            FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
            IF AVAILABLE OMRADETAB THEN DO:
               ccintemp.OMRADE  = OMRADETAB.OMRADE.
               ccintemp.NAMN = OMRADETAB.NAMN.
            END.
            ASSIGN
            ccintemp.FORNAMN = PERSONALTAB.FORNAMN
            ccintemp.EFTERNAMN = PERSONALTAB.EFTERNAMN
            ccintemp.EPOST = SUBSTRING(PERSONALTAB.PERSONSOK,20).
         END.      
      END.   
   END.
   RUN startexcel_UI.
   FOR EACH ccintemp:
      iRad = iRad + 1.
      RUN put_UI ("A",ccintemp.DATOR).
      RUN put_UI ("C",ccintemp.ANVANDARE).
      RUN put_UI ("D",ccintemp.EFTERNAMN + " " + ccintemp.FORNAMN ).
      RUN put_UI ("E",ccintemp.OMRADE + "," + ccintemp.NAMN).
      
      RUN put_UI ("G",ccintemp.EPOST).
      RUN put_UI ("I",ccintemp.KLIENT).
      RUN put_UI ("J",ccintemp.GFORETAG).
      RUN put_UI ("K",ccintemp.DATUMIN).
      
      
   END.
   RUN slutexcel_UI.
  
END PROCEDURE.
