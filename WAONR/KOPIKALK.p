/*KOPIKALK.P
PROGRAMMET KOPIERAR EN KALKYL KOPPLAD TILL ETT AONR*/

DEFINE INPUT PARAMETER globforetag AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER valaonr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER valdelnr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER valomrade AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER vartyp AS INTEGER NO-UNDO.

DEFINE INPUT PARAMETER tillaonr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER tilldelnr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER tillomrade AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER varkalk AS INTEGER NO-UNDO.
&Scoped-define NEW NEW

&Scoped-define NEW



RUN STYRFORE.P (INPUT globforetag).
DEFINE VARIABLE varbernr AS INTEGER NO-UNDO.
DEFINE VARIABLE tillbernr AS INTEGER NO-UNDO.

DEFINE BUFFER nybuff FOR FASTSPEC.
DEFINE BUFFER berbuff FOR FASTKALK.
DEFINE BUFFER ordbuff FOR KALKUPP.
DEFINE BUFFER fribuff FOR FAKTOR.
DEFINE BUFFER idbuff FOR KALKBEF.
DEFINE BUFFER scbuff FOR MTRL.    
DEFINE BUFFER kalkaonrbuff FOR KALKAONR.    
DEFINE BUFFER kalkspecbuff FOR KALKSPEC.
DEFINE BUFFER kalkradbuff FOR KALKYL.
DEFINE VARIABLE tempvar AS INTEGER NO-UNDO.
{KALKSISTA.I}
OPEN QUERY kalkq FOR EACH FASTSPEC WHERE 
FASTSPEC.KALKNR = varkalk  NO-LOCK.
GET FIRST kalkq NO-LOCK.
DO WHILE AVAILABLE(FASTSPEC):
   DO TRANSACTION:   
      IF Guru.Konstanter:varforetypchar[3] NE "" THEN DO:
         FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = Guru.Konstanter:varforetypchar[3] EXCLUSIVE-LOCK NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = tillomrade EXCLUSIVE-LOCK NO-ERROR.
      END.
      IF OMRADETAB.KALKYLINT2 < OMRADETAB.KALKYLSIST THEN DO:
         FIND CURRENT OMRADETAB NO-LOCK NO-ERROR.   
         RETURN.
      END.
      tempvar = OMRADETAB.KALKYLSIST.
      RUN kalksista_UI (INPUT-OUTPUT tempvar).
      IF tempvar = ? THEN RETURN.
      ELSE DO:
         ASSIGN                 
         tillbernr = OMRADETAB.KALKYLSIST.
         ASSIGN OMRADETAB.KALKYLSIST = OMRADETAB.KALKYLSIST + 1.       
      END.
   END.  
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = tillomrade NO-LOCK NO-ERROR.
   varbernr = FASTSPEC.KALKNR.
   DO TRANSACTION:      
      CREATE nybuff.
      BUFFER-COPY FASTSPEC TO nybuff.
      ASSIGN
      nybuff.KALKNR = tillbernr 
         /*
      nybuff.AONR = tillaonr
      nybuff.DELNR = tilldelnr
      */
      nybuff.OMRADE = tillomrade.
      FIND FIRST KALKAONR WHERE KALKAONR.KALKNR = FASTSPEC.KALKNR.
      CREATE kalkaonrbuff.
      BUFFER-COPY KALKAONR TO kalkaonrbuff.
      ASSIGN
      kalkaonrbuff.KALKNR = tillbernr  
      kalkaonrbuff.AONR = tillaonr     
      kalkaonrbuff.DELNR = tilldelnr   
      kalkaonrbuff.OMRADE = tillomrade.
   END.
   OPEN QUERY kq FOR EACH FASTKALK WHERE FASTKALK.KALKNR = varbernr
   AND FASTKALK.OMRADE = valomrade USE-INDEX OMRADE NO-LOCK.
   GET FIRST kq NO-LOCK.
   DO WHILE AVAILABLE(FASTKALK):
      DO TRANSACTION:      
         CREATE berbuff.
         BUFFER-COPY FASTKALK TO berbuff.
         ASSIGN
         berbuff.KALKNR = tillbernr
         berbuff.OMRADE = tillomrade.
      END.
      GET NEXT kq NO-LOCK.
   END.
   RELEASE berbuff NO-ERROR.
   CLOSE QUERY kq.

   IF globforetag = "ELPA" OR globforetag = "GRAN" OR globforetag = "GKAL" OR globforetag = "LULE" THEN DO:
      FIND FIRST KALKUPP WHERE KALKUPP.KALKNR = varbernr
      USE-INDEX KALKNR NO-LOCK NO-ERROR.      
      IF AVAILABLE KALKUPP THEN DO TRANSACTION:      
         CREATE ordbuff.
         BUFFER-COPY KALKUPP TO ordbuff.
         ASSIGN
         ordbuff.KALKNR = tillbernr.
      END.
   END.
      
   FIND FIRST FAKTOR WHERE FAKTOR.KALKNR = varbernr AND
   FAKTOR.OMRADE = valomrade USE-INDEX OMRADE NO-LOCK NO-ERROR.
   IF AVAILABLE FAKTOR THEN DO TRANSACTION:      
      CREATE fribuff.
      BUFFER-COPY FAKTOR TO fribuff.
      ASSIGN
      fribuff.KALKNR = tillbernr
      fribuff.OMRADE = tillomrade.
   END.

   OPEN QUERY befq FOR EACH KALKBEF WHERE KALKBEF.KALKNR = varbernr AND
   KALKBEF.OMRADE = valomrade USE-INDEX OMRADE NO-LOCK.
   GET FIRST befq NO-LOCK.
   DO WHILE AVAILABLE(KALKBEF):
      DO TRANSACTION:      
         CREATE idbuff.
         BUFFER-COPY KALKBEF TO idbuff.
         ASSIGN
         idbuff.KALKNR = tillbernr
         idbuff.OMRADE = tillomrade.
      END.
      GET NEXT befq NO-LOCK.
   END.
      
   OPEN QUERY mtrlq FOR EACH MTRL WHERE MTRL.KALKNR = FASTSPEC.KALKNR 
   USE-INDEX KALK NO-LOCK.
   GET FIRST mtrlq NO-LOCK.
   DO WHILE AVAILABLE(MTRL):
      DO TRANSACTION:      
         CREATE scbuff.
         BUFFER-COPY MTRL TO scbuff.
         ASSIGN
         scbuff.KALKNR = tillbernr.
      END.      
      GET NEXT mtrlq NO-LOCK.
   END.         
   GET NEXT kalkq NO-LOCK.
END.

OPEN QUERY kalkfriq FOR EACH KALKSPEC WHERE 
KALKSPEC.AONR = valaonr AND 
KALKSPEC.DELNR = valdelnr NO-LOCK.
GET FIRST kalkfriq NO-LOCK.
DO WHILE AVAILABLE(KALKSPEC):
   DO TRANSACTION:   
      IF Guru.Konstanter:varforetypchar[3] NE "" THEN DO:
         FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = Guru.Konstanter:varforetypchar[3] EXCLUSIVE-LOCK NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = tillomrade EXCLUSIVE-LOCK NO-ERROR.
      END.
      IF OMRADETAB.KALKYLINT2 < OMRADETAB.KALKYLSIST THEN DO:
         FIND CURRENT OMRADETAB NO-LOCK NO-ERROR.   
         RETURN.
      END.
      tempvar = OMRADETAB.KALKYLSIST.
      RUN kalksista_UI (INPUT-OUTPUT tempvar).
      IF tempvar = ? THEN RETURN.
      ELSE DO:
         ASSIGN                 
         tillbernr = OMRADETAB.KALKYLSIST.
         ASSIGN OMRADETAB.KALKYLSIST = OMRADETAB.KALKYLSIST + 1.       
      END.
   END.
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = tillomrade NO-LOCK NO-ERROR.
   varbernr = KALKSPEC.KALKNR.
   DO TRANSACTION:      
      CREATE kalkspecbuff.
      BUFFER-COPY KALKSPEC TO kalkspecbuff.
      ASSIGN
      kalkspecbuff.KALKNR = tillbernr      
      kalkspecbuff.AONR = tillaonr
      kalkspecbuff.DELNR = tilldelnr
      kalkspecbuff.OMRADE = tillomrade.
      FIND FIRST KALKAONR WHERE KALKAONR.KALKNR = KALKSPEC.KALKNR.
      CREATE kalkaonrbuff.
      BUFFER-COPY KALKAONR TO kalkaonrbuff.
      ASSIGN
      kalkaonrbuff.KALKNR = tillbernr  
      kalkaonrbuff.AONR = tillaonr     
      kalkaonrbuff.DELNR = tilldelnr   
      kalkaonrbuff.OMRADE = tillomrade.
   END.
   OPEN QUERY fkq FOR EACH KALKYL WHERE KALKYL.RECKALKYL = varbernr
   AND KALKYL.OMRADE = valomrade NO-LOCK.
   GET FIRST kq NO-LOCK.
   DO WHILE AVAILABLE(KALKYL):
      DO TRANSACTION:      
         CREATE kalkradbuff.
         BUFFER-COPY KALKYL TO kalkradbuff.
         ASSIGN
         kalkradbuff.RECKALKYL = tillbernr
         kalkradbuff.OMRADE = tillomrade.
      END.
      GET NEXT fkq NO-LOCK.
   END.  
   CLOSE QUERY fkq.    
   GET NEXT kalkfriq NO-LOCK.
END.
      
 

