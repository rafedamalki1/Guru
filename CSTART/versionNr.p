    /* versionNr.p */
DEFINE VARIABLE inp      AS CHARACTER           NO-UNDO. /* hold 1st line of version file */
DEF VAR dlcValue AS CHAR NO-UNDO.
DEF VAR patchLevel AS CHAR NO-UNDO.
DEF VAR i AS INT NO-UNDO.

  IF OPSYS = "Win32":U THEN /* Get DLC from Registry */
    GET-KEY-VALUE SECTION "Startup":U KEY "DLC":U VALUE dlcValue.

  IF (dlcValue = "" OR dlcValue = ?) THEN DO:
    ASSIGN dlcValue = OS-GETENV("DLC":U). /* Get DLC from environment */
      IF (dlcValue = "" OR dlcValue = ?) THEN DO: /* Still nothing? */
        ASSIGN patchLevel = "".
        RETURN.
      END.
  END.
  FILE-INFO:FILE-NAME = dlcValue + "/version":U.
  IF FILE-INFO:FULL-PATHNAME NE ? THEN DO: /* Read the version file */
    INPUT FROM VALUE(FILE-INFO:FULL-PATHNAME).
      IMPORT UNFORMATTED inp. /* Get the first line */
    INPUT CLOSE.
    /*
     * As of version 9.1D just append everything from the version file
     * after the version from PROVERSION property
     */
    LEVEL:
    DO i = 2 TO NUM-ENTRIES(inp," ":U):
      IF ENTRY(i,inp," ") BEGINS PROVERSION THEN DO:
        ASSIGN patchLevel = REPLACE(ENTRY(i,inp," "),PROVERSION,"").
        IF patchLevel = '' THEN patchLevel = 'NO PATCH INSTALLED'. 
        LEAVE LEVEL.
      END.
    END.
  END.
  MESSAGE 'Progress Version:' PROVERSION SKIP 'Patch:' patchlevel
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
