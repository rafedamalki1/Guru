/*BORLBACK.P*/
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE prognamnold AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnold2 AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE progflytt AS CHARACTER NO-UNDO.

DEFINE INPUT PARAMETER prognamnque AS CHARACTER NO-UNDO.
progflytt = SUBSTRING(prognamnque,1,INDEX(prognamnque,"autotid.txt") - 1).
RUN ut_UI (INPUT "BORL AI").
prognamnold = progflytt + "AI\BORL.A1".
prognamnold2 = progflytt + "BACKUP\BORL.A1".
OS-COPY VALUE(prognamnold) VALUE(prognamnold2).
prognamnold = progflytt + "AI\BORL.A2".
prognamnold2 = progflytt + "BACKUP\BORL.A2".
OS-COPY VALUE(prognamnold) VALUE(prognamnold2).
RUN ut_UI (INPUT "BORL EMP").
kommando = "rfutil " + progflytt + "db\borl -C aimage extent empty".
RUN os_UI.
RUN ut_UI (INPUT "BORL BK").
kommando = "probkup online " + progflytt + "db\BORL  " + progflytt + "backup\BORL.bck -com".
RUN os_UI.

PROCEDURE os_UI:
   OS-COMMAND SILENT VALUE(kommando) .
END PROCEDURE.
PROCEDURE ut_UI:
   DEFINE INPUT PARAMETER uttxt AS CHARACTER NO-UNDO.
   OUTPUT TO VALUE(prognamnque) APPEND.
   PUT uttxt " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END PROCEDURE.

