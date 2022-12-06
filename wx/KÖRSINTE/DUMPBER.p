/*DUMPBER.P
PROGRAMMET DUMPAR UT EN VALD BEREDNING*/

DEFINE INPUT PARAMETER valaonr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER valomrade AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER globforetag AS CHARACTER NO-UNDO.
DEFINE VARIABLE sokvag AS CHARACTER NO-UNDO.
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
DEFINE VARIABLE expimphdh AS HANDLE NO-UNDO.
{AMERICANEUROPEAN.I}
IF globforetag = "ELPA" THEN sokvag = "\\pc112\delad\elpool\elpNJ\BEREDNINGAR\".
ELSE IF globforetag = "GRAN" THEN sokvag = "d:\elpool\delad\pro9\wrk\".
ELSE IF globforetag = "GKAL" THEN sokvag = "d:\DELAD\SERVER\PRO9S\DBTILLBAKA\BEREDNINGAR\".
ELSE sokvag = "".
IF globforetag = "ELPA" OR globforetag = "GRAN" OR globforetag = "GKAL" THEN DO:
   OUTPUT TO VALUE(sokvag + valaonr + "berednin" + valomrade + ".d") convert target "iso8859-1" source "iso8859-1".
END.   
ELSE DO:   
   OUTPUT TO berednin.d convert target "iso8859-1" source "iso8859-1".
END.
OPEN QUERY bq FOR EACH BEREDNING WHERE BEREDNING.BERAONR = valaonr AND
BEREDNING.OMRADE = valomrade NO-LOCK.
GET FIRST bq NO-LOCK.
DO WHILE AVAILABLE(BEREDNING):
   EXPORT BEREDNING.
   GET NEXT bq NO-LOCK.
END.
CLOSE QUERY bq.
OUTPUT CLOSE.

IF globforetag = "ELPA" OR globforetag = "GRAN" OR globforetag = "GKAL" THEN DO:
   OUTPUT TO VALUE(sokvag + valaonr + "berval" + valomrade + ".d") convert target "iso8859-1" source "iso8859-1".
END.   
ELSE DO:   
   OUTPUT TO berval.d convert target "iso8859-1" source "iso8859-1".
END.   
OPEN QUERY kq FOR EACH BERVAL WHERE BERVAL.AONR = valaonr AND
BERVAL.OMRADE = valomrade NO-LOCK.
GET FIRST kq NO-LOCK.
DO WHILE AVAILABLE(BERVAL):
   EXPORT BERVAL.
   GET NEXT kq NO-LOCK.
END.
CLOSE QUERY kq.
OUTPUT CLOSE.

IF globforetag = "ELPA" OR globforetag = "GRAN" OR globforetag = "GKAL" THEN DO:
   OUTPUT TO VALUE(sokvag + valaonr + "berord" + valomrade + ".d") convert target "iso8859-1" source "iso8859-1".
END.  
ELSE DO:   
   OUTPUT TO berord.d convert target "iso8859-1" source "iso8859-1".
END.   
OPEN QUERY ordq FOR EACH BERORD WHERE BERORD.AONR = valaonr AND
BERORD.OMRADE = valomrade USE-INDEX ORD NO-LOCK.
GET FIRST ordq NO-LOCK.
DO WHILE AVAILABLE(BERORD):
   EXPORT BERORD.
   GET NEXT ordq NO-LOCK.
END.
CLOSE QUERY ordq.
OUTPUT CLOSE.

IF globforetag = "ELPA" OR globforetag = "GRAN" OR globforetag = "GKAL" THEN DO:
   OUTPUT TO VALUE(sokvag + valaonr + "frikort" + valomrade + ".d") convert target "iso8859-1" source "iso8859-1".
END.   
ELSE DO:   
   OUTPUT TO frikort.d convert target "iso8859-1" source "iso8859-1".
END.   
OPEN QUERY friq FOR EACH FRIKORT WHERE FRIKORT.AONR = valaonr AND
FRIKORT.OMRADE = valomrade USE-INDEX OMR NO-LOCK.
GET FIRST friq NO-LOCK.
DO WHILE AVAILABLE(FRIKORT):
   EXPORT FRIKORT.
   GET NEXT friq NO-LOCK.
END.
CLOSE QUERY friq.
OUTPUT CLOSE.

IF globforetag = "ELPA" OR globforetag = "GRAN" OR globforetag = "GKAL" THEN DO:
   OUTPUT TO VALUE(sokvag + valaonr + "berid" + valomrade + ".d") convert target "iso8859-1" source "iso8859-1".
END.   
ELSE DO:   
   OUTPUT TO berid.d convert target "iso8859-1" source "iso8859-1".
END.   
OPEN QUERY berqid FOR EACH BERID WHERE BERID.AONR = valaonr AND
BERID.OMRADE = valomrade USE-INDEX OMR NO-LOCK.
GET FIRST berqid NO-LOCK.
DO WHILE AVAILABLE(BERID):
   EXPORT BERID.
   GET NEXT berqid NO-LOCK.
END.
CLOSE QUERY berqid.
OUTPUT CLOSE.
IF globforetag = "ELPA" OR globforetag = "GRAN" OR globforetag = "GKAL" THEN DO:
   OUTPUT TO VALUE(sokvag + valaonr + "berkalk" + valomrade + ".d") convert target "iso8859-1" source "iso8859-1".
END.   
ELSE DO:   
   OUTPUT TO berkalk.d convert target "iso8859-1" source "iso8859-1".
END.   
OPEN QUERY berqkal FOR EACH BERKALK WHERE BERKALK.AONR = valaonr AND
BERKALK.OMRADE = valomrade USE-INDEX OMR NO-LOCK.
GET FIRST berqkal NO-LOCK.
DO WHILE AVAILABLE(BERKALK):
   EXPORT BERKALK.
   GET NEXT berqkal NO-LOCK.
END.
CLOSE QUERY berqkal.
OUTPUT CLOSE.
 
IF globforetag = "ELPA" OR globforetag = "GRAN" OR globforetag = "GKAL" THEN DO:
   OUTPUT TO VALUE(sokvag + valaonr + "bermtrl" + valomrade + ".d") convert target "iso8859-1" source "iso8859-1".
END.   
ELSE DO:   
   OUTPUT TO bermtrl.d convert target "iso8859-1" source "iso8859-1".
END.   
OPEN QUERY berqmtrl FOR EACH BERMTRL WHERE BERMTRL.AONR = valaonr AND
BERMTRL.OMRADE = valomrade USE-INDEX OMR NO-LOCK.
GET FIRST berqmtrl NO-LOCK.
DO WHILE AVAILABLE(BERMTRL):
   EXPORT BERMTRL.      
   GET NEXT berqmtrl NO-LOCK.
END.
CLOSE QUERY berqmtrl. 
OUTPUT CLOSE.

IF globforetag = "ELPA" OR globforetag = "GRAN" OR globforetag = "GKAL"  THEN DO:
   OUTPUT TO VALUE(sokvag + valaonr + "berlink" + valomrade + ".d") convert target "iso8859-1" source "iso8859-1".
END.   
ELSE DO:   
   OUTPUT TO berlink.d convert target "iso8859-1" source "iso8859-1".
END.  
OPEN QUERY berqlin FOR EACH BERLINKAB WHERE BERLINKAB.AONR = valaonr AND
BERLINKAB.OMRADE = valomrade USE-INDEX OMR NO-LOCK.
GET FIRST berqlin NO-LOCK.
DO WHILE AVAILABLE(BERLINKAB):
   EXPORT BERLINKAB.
   GET NEXT berqlin NO-LOCK.
END.
CLOSE QUERY berqlin.
OUTPUT CLOSE.

IF globforetag = "ELPA" OR globforetag = "GRAN" OR globforetag = "GKAL" THEN DO:
   OUTPUT TO VALUE(sokvag + valaonr + "kskydd" + valomrade + ".d") convert target "iso8859-1" source "iso8859-1".
END.   
ELSE DO:   
   OUTPUT TO kskydd.d convert target "iso8859-1" source "iso8859-1".
END.   
OPEN QUERY berqskydd FOR EACH KSKYDD WHERE KSKYDD.AONR = valaonr AND
KSKYDD.OMRADE = valomrade AND KSKYDD.BERED = TRUE
USE-INDEX OMR NO-LOCK.
GET FIRST berqskydd NO-LOCK.
DO WHILE AVAILABLE(KSKYDD):
   EXPORT KSKYDD.
   GET NEXT berqskydd NO-LOCK.
END.
CLOSE QUERY berqskydd.   
OUTPUT CLOSE.

IF globforetag = "ELPA" OR globforetag = "GRAN" OR globforetag = "GKAL"  THEN DO:
   OUTPUT TO VALUE(sokvag + valaonr + "berupp" + valomrade + ".d") convert target "iso8859-1" source "iso8859-1".
END.   
ELSE DO:   
   OUTPUT TO berupp.d convert target "iso8859-1" source "iso8859-1".
END.  
OPEN QUERY uppq FOR EACH BERUPP WHERE BERUPP.AONR = valaonr AND
BERUPP.OMRADE = valomrade NO-LOCK.
GET FIRST uppq NO-LOCK.
DO WHILE AVAILABLE(BERUPP):
   EXPORT BERUPP.
   GET NEXT uppq NO-LOCK.
END.
CLOSE QUERY uppq.
OUTPUT CLOSE.
/*HD HÄR*/

RUN FINNSTABELL.P (INPUT "HDKALK", OUTPUT bloblog).
IF bloblog = TRUE THEN DO:
   RUN EXPIMPHD.P PERSISTENT SET expimphdh.
   RUN hddump_UI (INPUT INTEGER(valaonr), INPUT valomrade).
   IF VALID-HANDLE(expimphdh) THEN DELETE PROCEDURE expimphdh NO-ERROR.   
END. 
 {EUROPEANAMERICAN.I}  
