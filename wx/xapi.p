
DEFINE VARIABLE chrUserID   AS CHARACTER NO-UNDO.
DEFINE VARIABLE intResult   AS INTEGER   NO-UNDO.
DEFINE VARIABLE intSize     AS INTEGER   NO-UNDO.

PROCEDURE GetUserNameA EXTERNAL "ADVAPI32.DLL":
    DEFINE OUTPUT       PARAMETER chrUserID     AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER intBufferSize AS LONG      NO-UNDO.
    DEFINE RETURN       PARAMETER intResult     AS SHORT     NO-UNDO.
END PROCEDURE.

ASSIGN chrUserID = FILL(' ',256)
       intSize   = 255.

RUN GetUserNameA (OUTPUT       chrUserID,
                  INPUT-OUTPUT intSize,
                  OUTPUT       intResult).

IF intResult = 1 THEN
    DO:
        MESSAGE "Logon ID = " TRIM(chrUserID) VIEW-AS ALERT-BOX.
    END.
ELSE
    MESSAGE "Buffer Was Not Large Enough" VIEW-AS ALERT-BOX.
                /*
DEFINE VARIABLE chrComputerName AS CHARACTER NO-UNDO FORMAT "X(16)".
DEFINE VARIABLE intBufferSize   AS INTEGER   NO-UNDO INITIAL 16.
DEFINE VARIABLE intResult       AS INTEGER   NO-UNDO.
DEFINE VARIABLE ptrToString     AS MEMPTR    NO-UNDO.

PROCEDURE GetComputerNameA EXTERNAL "KERNEL32.DLL":
    DEFINE OUTPUT       PARAMETER ptrToString     AS MEMPTR.
    DEFINE INPUT-OUTPUT PARAMETER intBufferSize   AS LONG.
    DEFINE RETURN       PARAMETER intResult       AS SHORT.
END PROCEDURE.

SET-SIZE(ptrToString) = 16.

RUN GetComputerNameA (OUTPUT       ptrToString,
                      INPUT-OUTPUT intBufferSize,
                      OUTPUT       intResult).

IF intResult = 1 THEN
    DO:
        ASSIGN chrComputerName = GET-STRING(ptrToString,1).
        MESSAGE chrComputerName VIEW-AS ALERT-BOX.
    END.
ELSE
    MESSAGE "Buffer size is too small.  Must be as least " +
            STRING(intBufferSize) VIEW-AS ALERT-BOX.

SET-SIZE(ptrToString) = 0.

                  */
