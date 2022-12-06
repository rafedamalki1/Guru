&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-2



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/06/14 -  1:24 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
        
&Scoped-define NEW  NEW
&Scoped-define SHARED SHARED
DEFINE TEMP-TABLE arbtidtemp NO-UNDO
   FIELD ARBTIDKOD AS INTEGER 
   FIELD START AS DECIMAL
   FIELD FRUKOSTSTART   AS DECIMAL
   FIELD FRUKOSTSLUT    AS DECIMAL
   FIELD LUNCHSTART     AS DECIMAL
   FIELD LUNCHSLUT      AS DECIMAL
   FIELD KAFESTART      AS DECIMAL
   FIELD KAFESLUT       AS DECIMAL
   FIELD SLUT           AS DECIMAL
   FIELD TOTDAGTID      AS DECIMAL
   FIELD TYP            AS INTEGER
   INDEX ARBTIDKOD IS PRIMARY ARBTIDKOD
   INDEX TYP TYP.
DEFINE NEW SHARED VARIABLE dagsrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE temprec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE klocka AS DECIMAL NO-UNDO. 
DEFINE VARIABLE arbtk AS INTEGER NO-UNDO.
DEFINE VARIABLE arbtk2 AS INTEGER NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(80)" NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE sekkaffestarten AS INTEGER NO-UNDO.
DEFINE VARIABLE sekkaffeslutet AS INTEGER NO-UNDO.
DEFINE VARIABLE sekfrukoststarten AS INTEGER NO-UNDO.
DEFINE VARIABLE sekfrukostslutet AS INTEGER NO-UNDO.
DEFINE VARIABLE seklunchstarten AS INTEGER NO-UNDO.
DEFINE VARIABLE seklunchslutet AS INTEGER NO-UNDO.
DEFINE VARIABLE sekslut AS INTEGER NO-UNDO.
DEFINE VARIABLE sekstart AS INTEGER NO-UNDO.
DEFINE VARIABLE lunchen AS INTEGER NO-UNDO.
DEFINE VARIABLE frukost AS INTEGER NO-UNDO.
DEFINE VARIABLE rasten AS INTEGER NO-UNDO.
DEFINE VARIABLE tidsummasek AS INTEGER NO-UNDO.
DEFINE VARIABLE totdagsek AS INTEGER NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE tidut
   FIELD UT AS CHARACTER FORMAT "X(132)".   

DEFINE BUFFER arbtidbuff FOR arbtidtemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-2
&Scoped-define BROWSE-NAME BRW_DAGS

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES arbtidtemp

/* Definitions for BROWSE BRW_DAGS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_DAGS arbtidtemp.ARBTIDKOD ~
arbtidtemp.START arbtidtemp.FRUKOSTSTART arbtidtemp.FRUKOSTSLUT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_DAGS arbtidtemp.START ~
arbtidtemp.FRUKOSTSTART arbtidtemp.FRUKOSTSLUT 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_DAGS arbtidtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_DAGS arbtidtemp
&Scoped-define OPEN-QUERY-BRW_DAGS OPEN QUERY BRW_DAGS FOR EACH arbtidtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_DAGS arbtidtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_DAGS arbtidtemp


/* Definitions for DIALOG-BOX DIALOG-2                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_DAGS BTN_OK BTN_AVB RECT-26 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "cancel":L 
     SIZE 12 BY 2.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok":L 
     SIZE 12 BY 2.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 100.6 BY 20.52
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_DAGS FOR 
      arbtidtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_DAGS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_DAGS DIALOG-2 _STRUCTURED
  QUERY BRW_DAGS NO-LOCK DISPLAY
      arbtidtemp.ARBTIDKOD COLUMN-LABEL "Dag!schema" FORMAT "99":U
      arbtidtemp.START  FORMAT "99.99":U
      arbtidtemp.FRUKOSTSTART COLUMN-LABEL "Frukost!börjar" FORMAT "99.99":U
      arbtidtemp.FRUKOSTSLUT COLUMN-LABEL "Frukost!slutar" FORMAT "99.99":U
  ENABLE
      arbtidtemp.START
      arbtidtemp.FRUKOSTSTART
      arbtidtemp.FRUKOSTSLUT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 83.6 BY 15
         TITLE "Dagschema".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-2
     BRW_DAGS AT ROW 1.57 COL 2
     BTN_OK AT ROW 15.71 COL 87.6
     BTN_AVB AT ROW 18.71 COL 87.6
     RECT-26 AT ROW 1 COL 1
     SPACE(0.02) SKIP(0.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Dagschema uppläggning":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: arbtidtemp T "?" NO-UNDO temp-db arbtidtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-2
                                                                        */
/* BROWSE-TAB BRW_DAGS 1 DIALOG-2 */
ASSIGN 
       FRAME DIALOG-2:SCROLLABLE       = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_DAGS
/* Query rebuild information for BROWSE BRW_DAGS
     _TblList          = "Temp-Tables.arbtidtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.arbtidtemp.ARBTIDKOD
"ARBTIDKOD" "Dag!schema" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.arbtidtemp.START
"START" "Arbetets!början" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.arbtidtemp.FRUKOSTSTART
"FRUKOSTSTART" "Frukost!börjar" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.arbtidtemp.FRUKOSTSLUT
"FRUKOSTSLUT" "Frukost!slutar" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_DAGS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-2
/* Query rebuild information for DIALOG-BOX DIALOG-2
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-2 DIALOG-2
ON END-ERROR OF FRAME DIALOG-2 /* Dagschema uppläggning */
DO:   
   /*
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().  
   ASSIGN     
   arbtidtemp.START        = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.START          
   arbtidtemp.FRUKOSTSTART = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.FRUKOSTSTART   
   arbtidtemp.FRUKOSTSLUT  = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.FRUKOSTSLUT.    
   */
   APPLY "GO" TO FRAME {&FRAME-NAME}.
   APPLY "CHOOSE" TO BTN_AVB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_DAGS
&Scoped-define SELF-NAME BRW_DAGS
&Scoped-define SELF-NAME arbtidtemp.ARBTIDKOD


&Scoped-define SELF-NAME arbtidtemp.START
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.START BRW_DAGS _BROWSE-COLUMN DIALOG-2
ON ENTRY OF arbtidtemp.START IN BROWSE BRW_DAGS /* Arbetets!början */
DO:
  DISPLAY arbtidtemp.START WITH BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.START BRW_DAGS _BROWSE-COLUMN DIALOG-2
ON LEAVE OF arbtidtemp.START IN BROWSE BRW_DAGS /* Arbetets!början */
DO:
   IF arbtidtemp.START = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.START THEN musz = musz.
   ELSE DO:
      ASSIGN
      arbtidtemp.TYP = 2
      arbtidtemp.START = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.START.   
      DISPLAY arbtidtemp.START WITH BROWSE {&BROWSE-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME arbtidtemp.FRUKOSTSTART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.FRUKOSTSTART BRW_DAGS _BROWSE-COLUMN DIALOG-2
ON ENTRY OF arbtidtemp.FRUKOSTSTART IN BROWSE BRW_DAGS /* Frukost!börjar */
DO:
   DISPLAY arbtidtemp.FRUKOSTSTART WITH BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.FRUKOSTSTART BRW_DAGS _BROWSE-COLUMN DIALOG-2
ON LEAVE OF arbtidtemp.FRUKOSTSTART IN BROWSE BRW_DAGS /* Frukost!börjar */
DO:
  IF arbtidtemp.FRUKOSTSTART = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.FRUKOSTSTAR THEN musz = musz.
   ELSE DO:
      ASSIGN
      arbtidtemp.TYP = 2
      arbtidtemp.FRUKOSTSTART = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.FRUKOSTSTART.   
      DISPLAY arbtidtemp.FRUKOSTSTART WITH BROWSE {&BROWSE-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME arbtidtemp.FRUKOSTSLUT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.FRUKOSTSLUT BRW_DAGS _BROWSE-COLUMN DIALOG-2
ON ENTRY OF arbtidtemp.FRUKOSTSLUT IN BROWSE BRW_DAGS /* Frukost!slutar */
DO:
   DISPLAY arbtidtemp.FRUKOSTSLUT WITH BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.FRUKOSTSLUT BRW_DAGS _BROWSE-COLUMN DIALOG-2
ON LEAVE OF arbtidtemp.FRUKOSTSLUT IN BROWSE BRW_DAGS /* Frukost!slutar */
DO:
   IF arbtidtemp.FRUKOSTSLUT = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.FRUKOSTSLUT THEN musz = musz.
   ELSE DO:
      ASSIGN
      arbtidtemp.TYP = 2
      arbtidtemp.FRUKOSTSLUT = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.FRUKOSTSLUT.   
      DISPLAY arbtidtemp.FRUKOSTSLUT WITH BROWSE {&BROWSE-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-2
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-2 /* cancel */
DO:
   RUN avb_UI.   
   APPLY "GO" TO BTN_AVB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-2
ON GO OF BTN_AVB IN FRAME DIALOG-2 /* cancel */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-2
ON CHOOSE OF BTN_OK IN FRAME DIALOG-2 /* Ok */
DO:
   arbtidtemp.TYP = 2.
   RUN andra_UI.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-2 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   CREATE arbtidtemp.
   ASSIGN
   arbtidtemp.ARBTIDKOD = 1
   arbtidtemp.START  =     7.00          
   arbtidtemp.FRUKOSTSTART = 9.15
   arbtidtemp.FRUKOSTSLUT  = 9.30.
   CREATE arbtidtemp.
   ASSIGN
   arbtidtemp.ARBTIDKOD = 2
   arbtidtemp.START  =     8.00          
   arbtidtemp.FRUKOSTSTART = 10.15
   arbtidtemp.FRUKOSTSLUT  = 10.30.
   CREATE arbtidtemp.
   ASSIGN
   arbtidtemp.ARBTIDKOD = 3
   arbtidtemp.START  =     9.00          
   arbtidtemp.FRUKOSTSTART = 11.15
   arbtidtemp.FRUKOSTSLUT  = 11.30.
   RUN enable_UI. 
   OPEN QUERY {&BROWSE-NAME} FOR EACH arbtidtemp.
   
   APPLY "HOME" TO {&BROWSE-NAME}.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI DIALOG-2 
PROCEDURE andra_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH arbtidtemp:
       MESSAGE arbtidtemp.START       
               arbtidtemp.FRUKOSTSTART
               arbtidtemp.FRUKOSTSLUT VIEW-AS ALERT-BOX.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avb_UI DIALOG-2 
PROCEDURE avb_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   MESSAGE "Save ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
   CASE val:
      WHEN TRUE THEN DO:
         arbtidtemp.TYP = 2.
         RUN andra_UI.
         RETURN.
      END.
   END CASE.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-2  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-2.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-2  _DEFAULT-ENABLE
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
  ENABLE BRW_DAGS BTN_OK BTN_AVB RECT-26 
      WITH FRAME DIALOG-2.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-2}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



