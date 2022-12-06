&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          TINYDICT         PROGRESS
*/
&Scoped-define WINDOW-NAME    dlg-field
&Scoped-define FRAME-NAME     dlg-field
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS dlg-field 
/*------------------------------------------------------------------------

  File: _TBLFLDS.W

  Description: View schema fields

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Gerry Seidl

  Created: 11/15/94 -  9:01 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
{ protools/ptlshlp.i } /* help definitions */

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER dbnam AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER dbtyp AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE rc AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse (alphabetically)                   */
&Scoped-define FRAME-NAME  dlg-field
&Scoped-define BROWSE-NAME bField

/* Custom List Definitions                                              */
&Scoped-define LIST-1 
&Scoped-define LIST-2 
&Scoped-define LIST-3 

/* Definitions for BROWSE bField                                        */
&Scoped-define FIELDS-IN-QUERY-bField tinydict._field._field-name 
&Scoped-define OPEN-QUERY-bField OPEN QUERY bField FOR EACH tinydict._field NO-LOCK.
&Scoped-define FIRST-TABLE-IN-QUERY-bField tinydict._field
&Scoped-define TABLES-IN-QUERY-bField tinydict._field 

/* Definitions for BROWSE bTable                                        */
&Scoped-define FIELDS-IN-QUERY-bTable tinydict._file._file-name 
&Scoped-define OPEN-QUERY-bTable OPEN QUERY bTable FOR EACH tinydict._file ~
      WHERE _file._File-Number > 0 ~
 AND tinydict._file._Db-recid = RECID(tinydict._db) NO-LOCK.
&Scoped-define FIRST-TABLE-IN-QUERY-bTable tinydict._file
&Scoped-define TABLES-IN-QUERY-bTable tinydict._file 

/* Definitions for DIALOG-BOX dlg-field                                 */
&Scoped-define FIELDS-IN-QUERY-dlg-field 
&Scoped-define ENABLED-FIELDS-IN-QUERY-dlg-field 

/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&Close":L 
     SIZE 15 BY 1.125
     BGCOLOR 8 .

DEFINE BUTTON b_Help 
     LABEL "&Help":L 
     SIZE 15 BY 1.125.

DEFINE VARIABLE descr AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 28 BY 3.35
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE viewas AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 32 BY 3.35
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE clbl AS CHARACTER FORMAT "X(30)":U 
     LABEL "Col. Label" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Dec AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Decimals" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE dtype AS CHARACTER FORMAT "X(9)":U 
     LABEL "Data Type" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Ext AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Extent" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fmt AS CHARACTER FORMAT "X(30)":U 
     LABEL "Format" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE hlp AS CHARACTER FORMAT "X(63)":U 
     LABEL "Help" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Initval AS CHARACTER FORMAT "X(30)":U 
     LABEL "Initial" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE lbl AS CHARACTER FORMAT "X(30)":U 
     LABEL "Label" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE orderno AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Order #" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE rField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 25 BY 6.19.

DEFINE RECTANGLE rField-Info
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 64 BY 12.69.

DEFINE RECTANGLE rTable
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 25 BY 6.

DEFINE VARIABLE casesen AS LOGICAL INITIAL no 
     LABEL "Case Sensitive":L 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .85 NO-UNDO.

DEFINE VARIABLE man AS LOGICAL INITIAL no 
     LABEL "Mandatory":L 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .85 NO-UNDO.


/* Query definitions                                                    */
DEFINE QUERY bField FOR tinydict._field SCROLLING.
DEFINE QUERY bTable FOR tinydict._file SCROLLING.

/* Browse definitions                                                   */
DEFINE BROWSE bField QUERY bField NO-LOCK DISPLAY 
      tinydict._field._field-name
    WITH NO-LABELS SIZE 23 BY 5.23.

DEFINE BROWSE bTable QUERY bTable NO-LOCK DISPLAY 
      tinydict._file._file-name
    WITH NO-LABELS SIZE 23 BY 5.23.


/* ************************  Frame Definitions  *********************** */
DEFINE FRAME dlg-field
     bTable AT ROW 2 COL 3
     dtype AT ROW 2.08 COL 39 COLON-ALIGNED
     orderno AT ROW 2.08 COL 67 COLON-ALIGNED
     fmt AT ROW 3.31 COL 39 COLON-ALIGNED
     Dec AT ROW 3.31 COL 67 COLON-ALIGNED
     lbl AT ROW 4.65 COL 39 COLON-ALIGNED
     Ext AT ROW 4.65 COL 67 COLON-ALIGNED
     clbl AT ROW 5.85 COL 39 COLON-ALIGNED
     Initval AT ROW 5.85 COL 67 COLON-ALIGNED
     hlp AT ROW 7.08 COL 39 COLON-ALIGNED
     bField AT ROW 8.54 COL 3
     descr AT ROW 9.15 COL 30 NO-LABEL
     viewas AT ROW 9.15 COL 59 NO-LABEL
     man AT ROW 12.85 COL 41
     casesen AT ROW 12.85 COL 63
     Btn_OK AT ROW 14.46 COL 27
     b_Help AT ROW 14.5 COL 55
     " Tables" VIEW-AS TEXT
          SIZE 8 BY .69 AT ROW 1.23 COL 4
     " Field Information" VIEW-AS TEXT
          SIZE 19 BY .69 AT ROW 1.27 COL 30
     rField-Info AT ROW 1.5 COL 28
     rTable AT ROW 1.54 COL 2
     " Fields" VIEW-AS TEXT
          SIZE 8 BY .69 AT ROW 7.77 COL 4
     rField AT ROW 8 COL 2
     "Description:" VIEW-AS TEXT
          SIZE 12 BY .69 AT ROW 8.27 COL 30
     "View-As phrase:" VIEW-AS TEXT
          SIZE 15 BY .69 AT ROW 8.27 COL 59
     SPACE(18.47) SKIP(7.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE 
         TITLE "":L
         DEFAULT-BUTTON Btn_OK.

 


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX dlg-field
  VISIBLE,L                                                             */
ASSIGN 
       FRAME dlg-field:SCROLLABLE       = FALSE.

/* SETTINGS FOR TOGGLE-BOX casesen IN FRAME dlg-field
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN clbl IN FRAME dlg-field
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Dec IN FRAME dlg-field
   NO-ENABLE                                                            */
ASSIGN 
       descr:READ-ONLY IN FRAME dlg-field        = TRUE.

/* SETTINGS FOR FILL-IN dtype IN FRAME dlg-field
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ext IN FRAME dlg-field
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fmt IN FRAME dlg-field
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN hlp IN FRAME dlg-field
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Initval IN FRAME dlg-field
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl IN FRAME dlg-field
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX man IN FRAME dlg-field
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN orderno IN FRAME dlg-field
   NO-ENABLE                                                            */
ASSIGN 
       viewas:READ-ONLY IN FRAME dlg-field        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE bField
/* Query rebuild information for BROWSE bField
     _TblList          = "tinydict._field"
     _Options          = "NO-LOCK"
     _OrdList          = ""
     _FldNameList[1]   = tinydict._field._field-name
     _Query            is NOT OPENED
*/  /* BROWSE bField */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE bTable
/* Query rebuild information for BROWSE bTable
     _TblList          = "tinydict._file"
     _Options          = "NO-LOCK"
     _OrdList          = ""
     _JoinCode[1]      = ?
     _Where[1]         = "_file._File-Number > 0
 AND tinydict._file._Db-recid = RECID(tinydict._db)"
     _FldNameList[1]   = tinydict._file._file-name
     _Query            is NOT OPENED
*/  /* BROWSE bTable */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME bField
&Scoped-define SELF-NAME bField
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bField dlg-field
ON VALUE-CHANGED OF bField IN FRAME dlg-field
DO:
  IF AVAILABLE (tinydict._field) THEN 
    ASSIGN dtype   = tinydict._Field._Data-Type
           orderno = tinydict._Field._Order 
           fmt     = tinydict._Field._Format
           Dec     = tinydict._Field._Decimals
           lbl     = tinydict._Field._Label
           Ext     = tinydict._Field._Extent
           clbl    = tinydict._Field._Col-label
           Initval = tinydict._Field._Initial
           hlp     = tinydict._Field._Help
           descr   = tinydict._Field._Desc
           man     = tinydict._Field._Mandatory
           casesen = tinydict._Field._Fld-Case
           viewas  = tinydict._Field._View-As
           .
  ELSE
    ASSIGN dtype   = ""
           orderno = ? 
           fmt     = ""
           Dec     = ?
           lbl     = ""
           Ext     = ?
           clbl    = ""
           Initval = ""
           hlp     = ""
           descr   = ""
           man     = no
           casesen = no
           viewas  = ""
           .
  DISPLAY dtype orderno fmt Dec lbl Ext clbl Initval hlp 
      descr man casesen viewas
      WITH FRAME {&FRAME-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME bTable
&Scoped-define SELF-NAME bTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bTable dlg-field
ON VALUE-CHANGED OF bTable IN FRAME dlg-field
DO:
  open query bfield for each tinydict._field 
    where tinydict._field._file-recid = recid(tinydict._file) 
    NO-LOCK BY tinydict._field._Order.
  
  IF AVAILABLE (tinydict._field) THEN rc = bfield:select-row(1).
  APPLY "VALUE-CHANGED" TO bfield.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b_Help dlg-field
ON CHOOSE OF b_Help IN FRAME dlg-field /* Help */
DO:
  RUN adecomm/_adehelp.p ( "ptls", "CONTEXT", {&DC_Fields_DB}, ? ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME bField
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dlg-field 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

ASSIGN FRAME {&FRAME-NAME}:TITLE = "Fields - " + dbnam + " [" + dbtyp + "]".
IF dbtyp = "PROGRESS" THEN
    FIND tinydict._db WHERE tinydict._db._db-name = ? AND 
        tinydict._db._db-type = "PROGRESS" NO-LOCK NO-ERROR. /*local */
ELSE
    FIND tinydict._db WHERE tinydict._db._db-name = dbnam AND 
        tinydict._db._db-type = dbtyp NO-LOCK NO-ERROR. /* foreign */

/* Take a guess as to how many tables and fields are in a customer db */
ASSIGN bTable:MAX-DATA-GUESS = 750
       bField:MAX-DATA-GUESS = 750.
       
{&OPEN-QUERY-bTable}
GET FIRST btable.
IF NOT AVAILABLE tinydict._file THEN DO:
  MESSAGE "There are no tables defined in this database" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF AVAILABLE tinydict._file THEN DO:
    rc = btable:select-row(1).
    APPLY "VALUE-CHANGED" TO btable.
  END.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI dlg-field _DEFAULT-DISABLE
PROCEDURE disable_UI :
/* --------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
   -------------------------------------------------------------------- */
  /* Hide all frames. */
  HIDE FRAME dlg-field.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI dlg-field _DEFAULT-ENABLE
PROCEDURE enable_UI :
/* --------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
   -------------------------------------------------------------------- */
  DISPLAY dtype orderno fmt Dec lbl Ext clbl Initval hlp descr viewas man 
          casesen 
      WITH FRAME dlg-field.
  ENABLE rField-Info rTable bTable rField bField descr viewas Btn_OK b_Help 
      WITH FRAME dlg-field.
  {&OPEN-BROWSERS-IN-QUERY-dlg-field}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE BROWSE-NAME
&UNDEFINE FRAME-NAME
&UNDEFINE WINDOW-NAME
