/*----------------------------------------------------------------------------

File: _admin.p

Description:
   This is the startup program for the admin functions (formerly part of 
   the dictionary).

Author: Laura Stern

Date Created: 08/21/92

Modified:
  8/8/95 gfs Add warning for running procedures
----------------------------------------------------------------------------*/
&SCOPED-DEFINE SKP &IF "{&WINDOW-SYSTEM}" = "OSF/Motif" &THEN SKIP &ELSE &ENDIF

/* Check for persistent procedures which contain database tables */
RUN Chk_PPs.

{ adecomm/adewrap.i
  &TTY     = prodict/_dictc.p
  &MSWIN   = prodict/_dictc.p
  &MOTIF   = prodict/_dictc.p 
  &PRODUCT = "ADMIN"
  &SETCURS = WAIT }

PROCEDURE Chk_PPs:
  /* Finds all persistent procedures and generates a list of databases which they
   * use. The user gets a warning message stating that if they modify the schema
   * of any of these database, PROGRESS will restart and all unsaved work will
   * be lost.
   */
  DEFINE VARIABLE h          AS HANDLE    NO-UNDO. /* procedure handle */
  DEFINE VARIABLE dbentry    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE run_dblist AS CHARACTER NO-UNDO. /* list of databases */
  DEFINE VARIABLE i          AS INTEGER   NO-UNDO.
  
  ASSIGN h  = SESSION:FIRST-PROCEDURE.
  DO WHILE VALID-HANDLE(h):
    IF NOT (h:FILE-NAME BEGINS "adetran") THEN 
    DO i = 1 TO NUM-ENTRIES(h:DB-REFERENCES):
      ASSIGN dbentry = ENTRY(i,h:DB-REFERENCES).
      IF LOOKUP(dbentry, run_dblist) = 0 THEN
        ASSIGN run_dblist = run_dblist + (IF run_dblist NE "" THEN "," + dbentry ELSE dbentry).
    END.
    h = h:NEXT-SIBLING.
  END.
  IF run_dblist NE "" THEN
    MESSAGE "The following database" + (IF NUM-ENTRIES(run_dblist) > 1 THEN "s are" ELSE " is") + 
            " currently in use by running procedures:" skip(1)
            run_dblist skip(1)
            "Making schema changes to" + 
            (IF NUM-ENTRIES(run_dblist) > 1 THEN " any of these databases" ELSE " this database") +
            " at this time will" {&SKP}
            "cause PROGRESS to initiate a session restart causing all" {&SKP}
            "unsaved work to be lost."
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
END PROCEDURE.
