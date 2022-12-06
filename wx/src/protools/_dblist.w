&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME f
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS f 
/*------------------------------------------------------------------------

  File: _dblist.w

  Description: List connected databases (and let the user add/remove some).

    Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Bill Wood

  Created: 08/09/93 -  7:37 am

  Modified by Gerry Seidl on 11/15/94 - Added Fields dialog and changed the
                                        UI of this dialog.
                                        
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
{ protools/ptlshlp.i } /* help definitions */
{ adecomm/_adetool.i }

/* ***************************  Definitions  ************************** */
&SCOPED-DEFINE SKP &IF "{&WINDOW-SYSTEM}" = "OSF/Motif" &THEN SKIP &ELSE &ENDIF

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF VARIABLE db_lname    AS CHARACTER NO-UNDO.
DEF VARIABLE run_dblist  AS CHARACTER NO-UNDO.
DEF VARIABLE run_pglist  AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE run-list
  FIELD db-name  AS CHARACTER /* database name */
  FIELD sh-name  AS CHARACTER /* schema holder name */
  FIELD prg-name AS CHARACTER /* program which uses it */
  INDEX db-name  IS PRIMARY db-name.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS dbrect Btn_Close db-list b_Help b_connect 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Close AUTO-GO 
     LABEL "C&lose":L 
     SIZE 17 BY 1.14.

DEFINE BUTTON b_connect 
     LABEL "&Connect...":L 
     SIZE 17 BY 1.14.

DEFINE BUTTON b_Details 
     LABEL "Show &Details >>":L 
     SIZE 17 BY 1.14.

DEFINE BUTTON b_disconnect 
     LABEL "D&isconnect":L 
     SIZE 17 BY 1.14.

DEFINE BUTTON b_fields 
     LABEL "&Fields...":L 
     SIZE 17 BY 1.14.

DEFINE BUTTON b_Help 
     LABEL "&Help":L 
     SIZE 17 BY 1.14.

DEFINE VARIABLE eInfo AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 73 BY 5.24
     FONT 0 NO-UNDO.

DEFINE VARIABLE eLbl AS CHARACTER FORMAT "X(256)":U INITIAL " Connection Information" 
      VIEW-AS TEXT 
     SIZE 23 BY .76 NO-UNDO.

DEFINE RECTANGLE connrect
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 75 BY 6.

DEFINE RECTANGLE dbrect
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 57 BY 8.86.

DEFINE VARIABLE db-list AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     SIZE 55 BY 8
     FONT 0 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f
     Btn_Close AT ROW 1.48 COL 60
     db-list AT ROW 2 COL 3 NO-LABEL
     b_Help AT ROW 2.71 COL 60
     b_connect AT ROW 4.81 COL 60
     b_disconnect AT ROW 6.1 COL 60
     b_fields AT ROW 7.67 COL 60
     b_Details AT ROW 9.33 COL 60
     eLbl AT ROW 10.48 COL 1 COLON-ALIGNED NO-LABEL
     eInfo AT ROW 11.33 COL 3 NO-LABEL
     " Databases" VIEW-AS TEXT
          SIZE 12 BY .67 AT ROW 1.14 COL 3
     dbrect AT ROW 1.48 COL 2
     connrect AT ROW 10.81 COL 2
     SPACE(1.15) SKIP(0.05)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS THREE-D  SCROLLABLE 
         TITLE "Database Connections":L
         DEFAULT-BUTTON Btn_Close.

 

/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX f
   UNDERLINE Default                                                    */
ASSIGN 
       FRAME f:SCROLLABLE       = FALSE.

/* SETTINGS FOR BUTTON Btn_Close IN FRAME f
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON b_connect IN FRAME f
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON b_Details IN FRAME f
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON b_disconnect IN FRAME f
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR BUTTON b_fields IN FRAME f
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE connrect IN FRAME f
   NO-ENABLE                                                            */
ASSIGN 
       connrect:HIDDEN IN FRAME f           = TRUE.

/* SETTINGS FOR SELECTION-LIST db-list IN FRAME f
   NO-DISPLAY                                                           */
/* SETTINGS FOR EDITOR eInfo IN FRAME f
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       eInfo:HIDDEN IN FRAME f           = TRUE
       eInfo:READ-ONLY IN FRAME f        = TRUE.

/* SETTINGS FOR FILL-IN eLbl IN FRAME f
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       eLbl:HIDDEN IN FRAME f           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME b_connect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b_connect f
ON CHOOSE OF b_connect IN FRAME f /* Connect... */
DO:
  Define var lline         as char    NO-UNDO. /* sel-list line */
  
  /* Database Parameters */
  DEFINE VARIABLE PysName       AS CHARACTER    NO-UNDO. /* Physical DB Name */
  DEFINE VARIABLE LogName       AS CHARACTER    NO-UNDO. /* Logical DB Name  */
  DEFINE VARIABLE theType       AS CHARACTER    NO-UNDO. /* DB Name Type - eg. "PROGRESS" */
  DEFINE VARIABLE Db_Multi_User AS LOGICAL      NO-UNDO.

  /* Addl. Parameters */
  DEFINE VARIABLE network       AS CHARACTER    NO-UNDO.
  DEFINE VARIABLE host          AS CHARACTER    NO-UNDO.
  DEFINE VARIABLE service       AS CHARACTER    NO-UNDO.
  DEFINE VARIABLE uid           AS CHARACTER    NO-UNDO.
  DEFINE VARIABLE pwd           AS CHARACTER    NO-UNDO.
  DEFINE VARIABLE trigloc       AS CHARACTER    NO-UNDO.
  DEFINE VARIABLE pfile         AS CHARACTER    NO-UNDO.
  DEFINE VARIABLE uparms        AS CHARACTER    NO-UNDO.
  DEFINE VARIABLE pargs         AS CHARACTER    NO-UNDO.
   
  DEFINE VARIABLE args          AS CHARACTER EXTENT 4 NO-UNDO.
  DEFINE VARIABLE logname2      AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE constring     AS CHARACTER          NO-UNDO.
  
  DEFINE VARIABLE gwflag        AS LOGICAL            NO-UNDO.
  DEFINE VARIABLE db-type       AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE db-name       AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE x             AS INTEGER            NO-UNDO.
  
  if substring(db-list:SCREEN-VALUE ,28, -1, "CHARACTER") = "(not connected)" THEN 
  DO: /* connect dataserver */ 
      ASSIGN     
      logname = trim(substring(db-list:SCREEN-VALUE, 1, 15, "CHARACTER"))
      theType = trim(substring(db-list:SCREEN-VALUE,17,11, "CHARACTER"))
      theType = substring(theType,1,length(theType, "CHARACTER") - 1, "CHARACTER").

      run prodict/misc/_getconp.p
              (INPUT        logname,
               INPUT-OUTPUT PysName,
               INPUT-OUTPUT logname2,
               INPUT-OUTPUT theType,
               OUTPUT       trigloc,
               OUTPUT       pfile,
               OUTPUT       Db_Multi_User,
               OUTPUT       network,
               OUTPUT       host,
               OUTPUT       service,
               OUTPUT       uid,
               OUTPUT       pwd,
               OUTPUT       args[2],
               OUTPUT       args[3],
               OUTPUT       args[4]
               ).  
      ASSIGN pargs = trim(args[2] + " " + args[3] + " " + args[4]).
  END.  
  ELSE
  /* Set defaults for the db connect dialog. */
  ASSIGN DB_Multi_User = no
         theType       = "PROGRESS":U.

  run adecomm/_dbconnx.p ( YES,
                          INPUT-OUTPUT PysName,
                          INPUT-OUTPUT LogName,
                          INPUT-OUTPUT theType,       
                          INPUT-OUTPUT Db_Multi_User,
                          INPUT-OUTPUT network,
                          INPUT-OUTPUT host,
                          INPUT-OUTPUT service,
                          INPUT-OUTPUT uid,
                          INPUT-OUTPUT pwd,
                          INPUT-OUTPUT trigloc,
                          INPUT-OUTPUT pfile,
                          INPUT-OUTPUT pargs,
                          OUTPUT       constring ).

  /* Was a database connected */    
  /*message constring length(constring) view-as alert-box. */            
  IF CONNECTED(logname) THEN DO: /* hey! we connected something */
    IF theType <> "PROGRESS" THEN DO:
        IF NOT db-list:SCREEN-VALUE BEGINS logname THEN DO:
          search-blk:
          DO x = 1 TO db-list:NUM-ITEMS:
            IF TRIM(db-list:ENTRY(x)) BEGINS logname THEN DO:
              ASSIGN lline = db-list:ENTRY(x)
                     db-list:SCREEN-VALUE = lline.
              LEAVE search-blk.
            END.  /* Found the correct line */
          END.  /* search-blk */
        END.  /* If selected line isn't the correct line */
        lline = substring(db-list:SCREEN-VALUE,1,27,"CHARACTER").
        IF db-list:REPLACE(lline, db-list:SCREEN-VALUE) THEN.
        ASSIGN db-list:SCREEN-VALUE   = lline
               b_details:SENSITIVE    = yes
               b_fields:SENSITIVE     = yes
               b_disconnect:SENSITIVE = yes.
    END.
    ELSE DO:
        ASSIGN lline                = logname
               substring(lline, 16, -1, "CHARACTER") = "[" + dbtype(LogName) + "]".
        IF db-list:ADD-LAST(lline) THEN.
        CREATE ALIAS tinydict FOR DATABASE VALUE(logname). /* needed for next procedure */
        RUN protools/_db_gw.p (OUTPUT gwflag, OUTPUT db-name, OUTPUT db-type). /* Is schema-holder? If so, of what? */
        DELETE ALIAS tinydict.
        IF gwflag THEN DO x = 1 to NUM-ENTRIES(db-name): /* go after foreign databases */
          ASSIGN lline = "  " + entry(x,db-name)
                 substring(lline,16,-1,"CHARACTER") = "[" + entry(x,db-type) + "]".
          IF NOT CONNECTED(entry(x,db-name)) THEN
                 substring(lline,28,-1,"CHARACTER") = "(not connected)".
          IF db-list:ADD-LAST(lline) THEN.
       END.        
    END.
    IF PysName ne ? AND eInfo:VISIBLE THEN RUN fill-editor.
    IF db-list:NUM-ITEMS <> 0 and db-list:SCREEN-VALUE = ? THEN DO:
        db-list:SCREEN-VALUE = db-list:ENTRY(1).
        APPLY "VALUE-CHANGED" TO db-list.
        IF b_details:SENSITIVE = no THEN b_details:SENSITIVE = yes.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b_Details
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b_Details f
ON CHOOSE OF b_Details IN FRAME f /* Show Details >> */
DO:     
  /* The expanded Infromation is HIDDEN if the dialog is alreadey collapsed */
  IF eInfo:HIDDEN THEN
  DO: /* Expand dialog */
                    
      ASSIGN FRAME {&FRAME-NAME}:HEIGHT = connrect:ROW + connrect:HEIGHT - 1.0 +
                                          FRAME {&FRAME-NAME}:BORDER-TOP + 
                                          FRAME {&FRAME-NAME}:BORDER-BOTTOM +
                                          0.4                  
             eInfo:HIDDEN = no
             eLbl:HIDDEN  = no
             connrect:HIDDEN = no
             eInfo:SENSITIVE = yes.
               
      RUN Fill-Editor.
      ASSIGN self:label = "Hide &Details <<".
  END.
  ELSE /* Collapse dialog */
  DO:
      ASSIGN eInfo:HIDDEN = yes
             connrect:HIDDEN = yes
             eLbl:HIDDEN  = yes 
                    
             FRAME {&FRAME-NAME}:HEIGHT = dbrect:ROW + dbrect:HEIGHT - 1.0 +
                                          FRAME {&FRAME-NAME}:BORDER-TOP + 
                                          FRAME {&FRAME-NAME}:BORDER-BOTTOM +
                                          0.4 
             self:LABEL = "Show &Details >>"
             .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b_disconnect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b_disconnect f
ON CHOOSE OF b_disconnect IN FRAME f /* Disconnect */
DO:
  DEFINE VARIABLE dbtodisc AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE errmsg   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE prgstr   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE fdstr    AS CHARACTER NO-UNDO.
  
  ASSIGN db-list
         dbtodisc = trim(substring(db-list,1,15,"CHARACTER":U)).
  IF CAN-FIND (FIRST run-list WHERE db-name EQ dbtodisc) THEN DO:
    FOR EACH run-list WHERE db-name EQ dbtodisc:
      prgstr = prgstr + "  " + prg-name + CHR(10).
    END.
    MESSAGE CAPS(dbtodisc) "is in use by the following running procedure" + 
            (IF NUM-ENTRIES(RIGHT-TRIM(prgstr),CHR(10)) > 1 THEN "s:" ELSE ":") skip(1)
            RIGHT-TRIM(prgstr) skip(1)
            CAPS(dbtodisc) "cannot be disconnected until all procedures that" {&SKP}
            "access it are terminated."
            VIEW-AS ALERT-BOX INFORMATION.
    RETURN NO-APPLY.
  END.  
  ELSE IF CAN-FIND (FIRST run-list WHERE sh-name EQ dbtodisc) THEN DO:
    /* Trying to disconnect the schema of a database which has running code! */
    FOR EACH run-list WHERE sh-name EQ dbtodisc BREAK BY db-name:
      IF FIRST-OF(db-name) THEN 
        prgstr = CHR(10) + "Foreign Database: " + db-name + CHR(10) + "Running Procedure(s): " + prg-name.
      ELSE prgstr = prgstr + ", " + prg-name.
    END.
    MESSAGE CAPS(dbtodisc) "is a schema-holder database for at least one foreign" skip
            "database which is currently in use by a running procedure:" skip(1)
            TRIM(prgstr) skip(1)
            CAPS(dbtodisc) "cannot be disconnected until all procedures that" skip
            "access its foreign databases are terminated."
            VIEW-AS ALERT-BOX INFORMATION.
    RETURN NO-APPLY.
  END.    
  IF db-list NE "" THEN DISCONNECT VALUE(dbtodisc) NO-ERROR.  
  IF ERROR-STATUS:ERROR THEN DO: /* Oh no! */
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
        errmsg = errmsg + ERROR-STATUS:GET-MESSAGE(i) + 
                 (IF i NE ERROR-STATUS:NUM-MESSAGES THEN chr(10) ELSE ""). 
      END.
      MESSAGE errmsg VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    END.
    ELSE MESSAGE "An error occurred during disconnect." 
           VIEW-AS ALERT-BOX ERROR.
    RETURN.
  END. 
  /* Is the database still connected? If so, 'disconnect-pending' */
  IF CONNECTED(dbtodisc) THEN
    MESSAGE "Database: " + dbtodisc + " cannot be disconnected at this time." skip(1)
            "It may be in use by a running procedure or SmartObject." skip(1)
            "It will be disconnected automatically when the last" skip
            "dependant object terminates."
            VIEW-AS ALERT-BOX WARNING.
  ELSE DO:
    RUN initialize-dblist. /* Rebuild list of connected dbs */
    DO i = 1 TO db-list:NUM-ITEMS:
      IF TRIM(SUBSTRING(db-list:ENTRY(i),1,15,"CHARACTER":U)) = dbtodisc THEN
        MESSAGE "Database: " + dbtodisc + " cannot be disconnected at this time." skip(1)
                "It may be in use by a running procedure or SmartObject." skip(1)
                "It will be disconnected automatically when the last" skip
                "dependant object terminates."
                VIEW-AS ALERT-BOX WARNING.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b_fields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b_fields f
ON CHOOSE OF b_fields IN FRAME f /* Fields... */
DO:
  DEFINE VARIABLE dbnam AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dbtyp AS CHARACTER NO-UNDO.
  
  dbnam = TRIM(substring(db-list:SCREEN-VALUE,1,15,"CHARACTER")).
  dbtyp = TRIM(substring(db-list:SCREEN-VALUE,17,11,"CHARACTER")).
  dbtyp = substring(dbtyp,1,length(dbtyp,"CHARACTER") - 1,"CHARACTER").
  
  IF db-list:SCREEN-VALUE NE ? THEN 
  DO:
        
      create alias tinydict for DATABASE value(SDBNAME(dbnam)).
      RUN protools/_tblflds.w (INPUT dbnam, INPUT dbtyp).
      delete alias tinydict.
  END.
  ELSE MESSAGE "You must select a database." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b_Help f
ON CHOOSE OF b_Help IN FRAME f /* Help */
OR HELP OF FRAME {&FRAME-NAME} ANYWHERE
DO:
  RUN adecomm/_adehelp.p ( "ptls", "CONTEXT", {&Database_Connections}, ? ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME db-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL db-list f
ON VALUE-CHANGED OF db-list IN FRAME f
DO:  
  IF SUBSTRING(db-list:SCREEN-VALUE, 28, -1,"CHARACTER") = "(not connected)" THEN b_disconnect:SENSITIVE = no.
  ELSE b_disconnect:SENSITIVE = yes.
  IF eInfo:VISIBLE THEN RUN fill-editor. /* Show more info about the database */
  ASSIGN b_fields:SENSITIVE = yes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK f 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Initialize lists */
RUN initialize-dblist.

/* Generate list of database which code is currently running against */
RUN Running_DBList.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI f _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME f.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI f _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE dbrect Btn_Close db-list b_Help b_connect 
      WITH FRAME f.
  {&OPEN-BROWSERS-IN-QUERY-f}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Fill-Editor f 
PROCEDURE Fill-Editor :
/* -----------------------------------------------------------
  Purpose:     Fill editor with db info.
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE VARIABLE dbtyp AS CHARACTER NO-UNDO.
  
  ASSIGN 
    db_lname = trim(substring(db-list:SCREEN-VALUE IN FRAME {&FRAME-NAME}, 1, 15,"CHARACTER"))
    dbtyp = trim(substring(db-list:SCREEN-VALUE,17,11,"CHARACTER"))
    dbtyp = substring(dbtyp,1,length(dbtyp,"CHARACTER") - 1, "CHARACTER").

  IF db_lname = "" OR db_lname = ? THEN
  ASSIGN einfo = "Database  n/a" + chr(10) +
                 "  Physical Name: n/a" + chr(10) +
                 "  Database Type: n/a" + chr(10) +
                 "  Schema Name  : n/a" + chr(10) +
                 "  Restrictions : n/a". 
  ELSE 
  ASSIGN eInfo = "Database " + db_lname + chr(10) +
                 "  Physical Name: " + (IF PDBNAME(db_lname) = ? THEN "n/a" ELSE PDBNAME(db_lname)) + chr(10).
         eInfo = eInfo + "  Database Type: " + (IF DBTYPE(db_lname) = ?  THEN dbtyp ELSE DBTYPE(db_lname))  + chr(10).
         eInfo = eInfo + "  Schema Holder: " + (IF SDBNAME(db_lname) = ? THEN "n/a" ELSE SDBNAME(db_lname)) + chr(10).
         eInfo = eInfo + "  Restrictions : " + (IF DBRESTRICTIONS(db_lname) = "" THEN "None" 
                                       ELSE DBRESTRICTIONS(db_lname)).
  DISPLAY eLbl eInfo with frame {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-dblist f 
PROCEDURE initialize-dblist :
/* -----------------------------------------------------------
  Purpose:     Create a list of Connected databases.  Assign
               the first one as the "current" selection and
               show the info about it.  
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE VARIABLE x       AS INTEGER                NO-UNDO.
  DEFINE VARIABLE i       AS INTEGER                NO-UNDO.
  DEFINE VARIABLE lline   AS CHAR    FORMAT "X(40)" NO-UNDO.
  DEFINE VARIABLE db-name AS CHAR                   NO-UNDO.
  DEFINE VARIABLE db-type AS CHAR                   NO-UNDO.
  DEFINE VARIABLE gwflag  AS LOGICAL INITIAL no     NO-UNDO.
  
  ASSIGN eInfo:HIDDEN IN FRAME {&FRAME-NAME}    = yes
         connrect:HIDDEN IN FRAME {&FRAME-NAME} = yes
         eLbl:HIDDEN  IN FRAME {&FRAME-NAME}    = yes
         &IF "{&WINDOW-SYSTEM}" EQ "OSF/Motif" &THEN
           b_Details:LABEL = "Show Details >>".
         &ELSE
           b_Details:LABEL = "Show &Details >>".
         &ENDIF  
  ASSIGN FRAME {&FRAME-NAME}:HEIGHT = dbrect:ROW + dbrect:HEIGHT - 1.0 +
                                          FRAME {&FRAME-NAME}:BORDER-TOP + 
                                          FRAME {&FRAME-NAME}:BORDER-BOTTOM +
                                          0.4  .
   
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN db-list:SCREEN-VALUE = ""
           db-list:LIST-ITEMS   = "".
    DO i = 1 to NUM-DBS: /* loop through connected databases */
      IF DBTYPE(ldbname(i)) <> "PROGRESS" THEN next. /* skip non-progress databases */
      ASSIGN lline                = ldbname(i)
             substring(lline, 16, -1, "CHARACTER") = "[" + DBTYPE(ldbname(i)) + "]".
      IF db-list:ADD-LAST(lline) THEN.
      CREATE ALIAS tinydict FOR DATABASE VALUE(ldbname(i)).
      RUN protools/_db_gw.p (OUTPUT gwflag, OUTPUT db-name, OUTPUT db-type). /* Is schema-holder? If so, of what? */
      DELETE ALIAS tinydict.
      IF gwflag THEN DO x = 1 to NUM-ENTRIES(db-name): /* go after foreign databases */
          ASSIGN lline = "  " + entry(x,db-name).
                 SUBSTRING(lline, 16, -1, "CHARACTER") = "[" + entry(x,db-type) + "]".
          IF NOT CONNECTED(entry(x,db-name)) THEN
                 SUBSTRING(lline, 28, -1, "CHARACTER") = "(not connected)".
          IF db-list:ADD-LAST(lline) THEN.
      END.
    END.
    IF NUM-DBS > 0 THEN db-list:SCREEN-VALUE = db-list:ENTRY(1).
  END.
  IF eInfo:VISIBLE THEN RUN fill-editor.  
  IF db-list:SCREEN-VALUE = ? OR db-list:NUM-ITEMS = 0 THEN
      ASSIGN  b_Details:SENSITIVE = no
              b_fields:SENSITIVE  = no
              b_Disconnect:SENSITIVE = no.
  ELSE ASSIGN b_Details:SENSITIVE = yes
              b_fields:SENSITIVE  = yes
              b_Disconnect:SENSITIVE = yes.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Running_DBList f 
PROCEDURE Running_DBList :
/* -----------------------------------------------------------
  Purpose:     Build used db list.
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE VARIABLE h       AS HANDLE.
  DEFINE VARIABLE i       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE dbentry AS CHARACTER NO-UNDO.

  /* Run through the list of procedures and build a list
   * of databases which they rely on
   */
  ASSIGN h = SESSION:FIRST-PROCEDURE.
  DO WHILE VALID-HANDLE(h):
    DO i = 1 TO NUM-ENTRIES(h:DB-REFERENCES):
      ASSIGN dbentry = ENTRY(i, h:DB-REFERENCES).
      IF LOOKUP(dbentry, run_dblist) = 0 THEN
        FIND run-list WHERE db-name = dbentry AND
                              prg-name = h:FILE-NAME NO-ERROR.
        IF NOT AVAILABLE run-list THEN DO:
          CREATE run-list.
          ASSIGN db-name  = dbentry
                 sh-name  = (IF SDBNAME(dbentry) NE dbentry THEN SDBNAME(dbentry) ELSE ?)
                 prg-name = h:FILE-NAME.
        END.
    END.    
    h = h:NEXT-SIBLING.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


