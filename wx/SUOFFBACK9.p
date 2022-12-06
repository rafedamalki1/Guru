/*SUoffBACK.P*/

DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE prognamnold AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnold2 AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE ers AS INTEGER NO-UNDO.

RUN ut_UI (INPUT "sundn9 AI").
prognamnold = "d:\delad\server\PRO9S\AI\sundn9.A1".
prognamnold2 = "d:\delad\server\PRO9S\DBKOPIA\sundn9.A1".
OS-COPY VALUE(prognamnold) VALUE(prognamnold2).
ers = OS-ERROR.
RUN ut_UI (INPUT ers).
RUN ut_UI (INPUT "sundn9 AI2").
prognamnold = "d:\delad\server\PRO9S\AI\sundn9.A2".
prognamnold2 = "d:\delad\server\PRO9S\DBKOPIA\sundn9.A2".
OS-COPY VALUE(prognamnold) VALUE(prognamnold2).
ers = OS-ERROR.
RUN ut_UI (INPUT ers).
RUN ut_UI (INPUT "sundn9 EMP").
kommando = "rfutil  d:\delad\server\PRO9S\db\sundn9 -C aimage extent empty".
RUN os_UI.
ers = OS-ERROR.
RUN ut_UI (INPUT ers).
RUN ut_UI (INPUT "sundn9 BK").
kommando = "probkup d:\delad\server\PRO9S\db\sundn9  d:\delad\server\PRO9S\DBKOPIA\sundn9.bck -com".
RUN os_UI.
ers = OS-ERROR.
RUN ut_UI (INPUT ers).
RUN ut_UI (INPUT "klar").

PROCEDURE os_UI:
   OS-COMMAND SILENT VALUE(kommando) .
END PROCEDURE.
PROCEDURE ut_UI:
   DEFINE INPUT PARAMETER uttxt AS CHARACTER NO-UNDO.
   OUTPUT TO d:\delad\server\PRO9S\autotid.txt  APPEND.
   PUT uttxt " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END PROCEDURE.

