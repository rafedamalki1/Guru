/*INLOAPI.P*/
DEFINE OUTPUT PARAMETER outanvanv AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER outdatornamn AS CHARACTER NO-UNDO.


DEFINE VARIABLE anvdator AS CHARACTER NO-UNDO.
IF OPSYS NE "win32" THEN RETURN.
DEFINE VARIABLE anvanv AS CHARACTER NO-UNDO.
DEFINE VARIABLE datornamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrComputerName AS CHARACTER NO-UNDO FORMAT "X(16)".
DEFINE VARIABLE intBufferSize   AS INTEGER   NO-UNDO INITIAL 16.
DEFINE VARIABLE intResultI       AS INTEGER   NO-UNDO.
DEFINE VARIABLE ptrToString     AS MEMPTR    NO-UNDO.

DEFINE VARIABLE chrUserID   AS CHARACTER NO-UNDO.
DEFINE VARIABLE intResult   AS INTEGER   NO-UNDO.
DEFINE VARIABLE intSize     AS INTEGER   NO-UNDO.

DEFINE VARIABLE lpServer     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lpNameBuffer   AS MEMPTR NO-UNDO.
DEFINE VARIABLE BufferType       AS MEMPTR NO-UNDO.
/*
DEFINE VARIABLE chWinNTSystemInfo      AS COM-HANDLE.


/* create a new WinNTSystemInfo object */
CREATE "WinNTSystemInfo" chWinNTSystemInfo.

/* User details under which the WinNTSystemInfo object is created */
MESSAGE chWinNTSystemInfo:PDC SKIP
        chWinNTSystemInfo:DomainName SKIP
        chWinNTSystemInfo:ComputerName SKIP
        chWinNTSystemInfo:UserName
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/* Clean up */
RELEASE OBJECT chWinNTSystemInfo NO-ERROR.
*/
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

IF intResult = 1 THEN anvanv = TRIM(chrUserID).
ELSE anvanv = "".
PROCEDURE GetComputerNameA EXTERNAL "KERNEL32.DLL":
   DEFINE OUTPUT       PARAMETER ptrToString     AS MEMPTR.
   DEFINE INPUT-OUTPUT PARAMETER intBufferSize   AS LONG.
   DEFINE RETURN       PARAMETER intResultI       AS SHORT.
END PROCEDURE.
SET-SIZE(ptrToString) = 16.
RUN GetComputerNameA (OUTPUT       ptrToString,
                      INPUT-OUTPUT intBufferSize,
                      OUTPUT       intResultI).


IF intResultI = 1 THEN DO:
   ASSIGN chrComputerName = GET-STRING(ptrToString,1).
   datornamn = chrComputerName.
END.
ELSE datornamn = "".
anvdator = "".
SUBSTRING(anvdator,1,20) = anvanv.
SUBSTRING(anvdator,25,20) = datornamn.

outanvanv = anvanv.
outdatornamn = datornamn.





/*
PROCEDURE NetGetJoinInformation EXTERNAL "Netapi32.dll":
   DEFINE INPUT        PARAMETER lpServer     AS CHARACTER NO-UNDO.
   /*DEFINE OUTPUT       PARAMETER lpNameBuffer   AS CHARACTER NO-UNDO.*/
   DEFINE OUTPUT       PARAMETER lpNameBuffer   AS MEMPTR NO-UNDO.
   DEFINE    OUTPUT       PARAMETER BufferType       AS MEMPTR NO-UNDO.
END PROCEDURE.

PROCEDURE NetApiBufferFree EXTERNAL "Netapi32.dll" :
   DEFINE INPUT PARAMETER pDomain AS LONG NO-UNDO.
END PROCEDURE.

SET-SIZE(BufferType) = 32.
SET-SIZE(lpNameBuffer) = 16.

/*RUN NetGetJoinInformation (INPUT datornamn, OUTPUT lpNameBuffer, OUTPUT BufferType).*/
/*RUN NetApiBufferFree (INPUT 0).*/
/*RUN NetGetJoinInformation (INPUT "", OUTPUT lpNameBuffer, OUTPUT BufferType).*/

/*MESSAGE STRING(BufferType) STRING(lpNameBuffer)
VIEW-AS ALERT-BOX.*/

MESSAGE GET-STRING(BufferType,1) GET-STRING(lpNameBuffer,1)
VIEW-AS ALERT-BOX.
DEFINE VARIABLE hCP     AS HANDLE    NO-UNDO.
DEFINE VARIABLE keyC     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lResult AS LOGICAL   NO-UNDO.
CREATE CLIENT-PRINCIPAL hCp.
KEYC = hCp:DOMAIN-NAME.
*/