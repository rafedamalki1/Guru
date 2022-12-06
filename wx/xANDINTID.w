&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE utsokaonr NO-UNDO LIKE utsokaonr.
DEFINE TEMP-TABLE tidallt NO-UNDO LIKE tidallt.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/07/05 - 10:41 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF VAR nydat AS INTEGER.
DEF VAR nydat2 AS date.
/*
DEFINE TEMP-TABLE tidallt NO-UNDO
   FIELD ANVANDARE        AS CHARACTER
   FIELD AONR             AS CHARACTER
   FIELD BERANTAL         AS DECIMAL
   FIELD BERBEORD         AS LOGICAL      INITIAL NO
   FIELD BEREDSKAP        AS CHARACTER
   FIELD BEREDSKAPSLUT    AS DECIMAL
   FIELD BEREDSKAPSTART   AS DECIMAL
   FIELD BILFORARE        AS LOGICAL      INITIAL NO
   FIELD DAG              AS CHARACTER    INITIAL TODAY
   FIELD DATUM            AS DATE
   FIELD DELNR            AS INTEGER
   FIELD ENFLERDAGS       AS CHARACTER
   FIELD GODKAND          AS CHARACTER
   FIELD LAGANTAL         AS DECIMAL
   FIELD LAGBAS           AS LOGICAL      INITIAL NO
   FIELD LONAUTO          AS LOGICAL      INITIAL YES
   FIELD LONTILLAGG       AS CHARACTER
   FIELD LONTILLANTAL     AS DECIMAL
   FIELD NODF             AS LOGICAL      INITIAL  NO
   FIELD OANT1            AS DECIMAL
   FIELD OANT2            AS DECIMAL
   FIELD OANT3            AS DECIMAL
   FIELD OKOD1            AS CHARACTER
   FIELD OKOD2            AS CHARACTER
   FIELD OKOD3            AS CHARACTER
   FIELD OSL1             AS DECIMAL
   FIELD OSL2             AS DECIMAL
   FIELD OSL3             AS DECIMAL
   FIELD OST1             AS DECIMAL
   FIELD OST2             AS DECIMAL
   FIELD OST3             AS DECIMAL
   FIELD OVERANTAL        AS DECIMAL
   FIELD OVERAUTO         AS LOGICAL      INITIAL YES
   FIELD OVERTIDTILL      AS CHARACTER
   FIELD OVERTIDUTTAG     AS CHARACTER    INITIAL "K"
   FIELD PERSONALKOD      AS CHARACTER
   FIELD PRIS             AS DECIMAL
   FIELD PRISTYP          AS CHARACTER    INITIAL "TOT.PRIS."
   FIELD PROGRAM          AS CHARACTER
   FIELD RECTIDVIS        AS RECID
   FIELD RESMAL           AS CHARACTER
   FIELD SLUT             AS DECIMAL      INITIAL 16.00
   FIELD START            AS DECIMAL      INITIAL 7.00
   FIELD TIDLOG           AS LOGICAL      INITIAL YES
   FIELD TOTALT           AS DECIMAL
   FIELD TRAKTAMENTE      AS INTEGER
   FIELD TRAKTANTAL       AS DECIMAL
   FIELD TRAKTAUTO        AS LOGICAL      INITIAL YES
   FIELD TRAKTKOD         AS CHARACTER
   FIELD TRAKTTOT         AS DECIMAL
   FIELD UTRYCKNING       AS LOGICAL      INITIAL NO
   FIELD VECKOKORD        AS CHARACTER
   FIELD VECKONUMMER      AS INTEGER
   FIELD VILART           AS CHARACTER
   FIELD ENHET           AS CHARACTER
   FIELD TYP              AS CHARACTER
   FIELD ANDRA            AS LOGICAL      INITIAL NO
   INDEX PSTART IS PRIMARY DATUM START SLUT PRISTYP
   INDEX PKOD DATUM BEREDSKAPSTART BEREDSKAPSLUT
   INDEX AONR AONR DELNR
   INDEX VILART VILART
   INDEX REC RECTIDVIS
   INDEX ANDRA ANDRA.

  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_NYTID

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidallt

/* Definitions for BROWSE BRW_NYTID                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_NYTID DAY(tidallt.DATUM) @ nydat ~
tidallt.AONR tidallt.DELNR tidallt.START tidallt.SLUT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_NYTID tidallt.AONR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_NYTID tidallt
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_NYTID tidallt
&Scoped-define OPEN-QUERY-BRW_NYTID OPEN QUERY BRW_NYTID FOR EACH tidallt NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_NYTID tidallt
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_NYTID tidallt


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_AVB BRW_NYTID 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 16.75 BY 2.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_NYTID FOR 
      tidallt SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_NYTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_NYTID DIALOG-1 _STRUCTURED
  QUERY BRW_NYTID DISPLAY
      DAY(tidallt.DATUM) @ nydat COLUMN-LABEL "Dat!um."
      tidallt.AONR COLUMN-LABEL "Aonr" FORMAT "X(9)":U
      tidallt.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U
      tidallt.START COLUMN-LABEL "Start!tid" FORMAT "99.99":U
      tidallt.SLUT COLUMN-LABEL "Slut!tid" FORMAT "99.99":U
  ENABLE
      tidallt.AONR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 42 BY 12.91
         TITLE "Nya tidregistreringar" TOOLTIP "Välj en registrering för ändring. Dubbel-klicka för bortag.".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BTN_AVB AT ROW 19.82 COL 33
     BRW_NYTID AT ROW 4.82 COL 31.5
     SPACE(24.49) SKIP(5.71)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ändra tidregistrering":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: utsokaonr T "?" NO-UNDO temp-db utsokaonr
      TABLE: ? T "?" NO-UNDO temp-db tidallt
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   Custom                                                               */
/* BROWSE-TAB BRW_NYTID BTN_AVB DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_NYTID
/* Query rebuild information for BROWSE BRW_NYTID
     _TblList          = "Temp-Tables.tidallt"
     _FldNameList[1]   > "_<CALC>"
"DAY(tidallt.DATUM) @ nydat" "Dat!um." ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.tidallt.AONR
"AONR" "Aonr" "X(9)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.tidallt.DELNR
"DELNR" "Del!nr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.tidallt.START
"START" "Start!tid" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.tidallt.SLUT
"SLUT" "Slut!tid" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_NYTID */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Ändra tidregistrering */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Ändra tidregistrering */
DO:
  
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_NYTID
&Scoped-define SELF-NAME BRW_NYTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_NYTID DIALOG-1
ON ROW-DISPLAY OF BRW_NYTID IN FRAME DIALOG-1 /* Nya tidregistreringar */
DO:

      ASSIGN
      tidallt.AONR:FGCOLOR IN BROWSE {&BROWSE-NAME} = 12
      tidallt.DELNR:FGCOLOR IN BROWSE {&BROWSE-NAME} = 12 
      tidallt.START:FGCOLOR IN BROWSE {&BROWSE-NAME} = 12 
      tidallt.SLUT:FGCOLOR IN BROWSE {&BROWSE-NAME} = 12.     
      nydat:FGCOLOR IN BROWSE {&BROWSE-NAME} = 12.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
    RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-1 


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
   ASSIGN
   &Scoped-define BROWSE-NAME BRW_NYTID
   tidallt.AONR:READ-ONLY IN BROWSE {&BROWSE-NAME} = TRUE.
   CREATE tidallt.
      ASSIGN
      tidallt.AONR = "112"
      tidallt.DATUM = today
      tidallt.DELNR = 0
      tidallt.SLUT = 7.00 
      tidallt.START = 16.00. 
   CREATE tidallt.
      ASSIGN
      tidallt.AONR = "113"
      tidallt.DATUM = today
      tidallt.DELNR = 0
      tidallt.SLUT = 7.00 
      tidallt.START = 16.00. 
    CREATE tidallt.
      ASSIGN
      tidallt.AONR = "114"
      tidallt.DATUM = today
      tidallt.DELNR = 0
      tidallt.SLUT = 7.00 
      tidallt.START = 16.00. 
   OPEN QUERY BRW_NYTID FOR EACH tidallt NO-LOCK.
   RUN enable_UI.       
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.  
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-1  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-1  _DEFAULT-ENABLE
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
  ENABLE BTN_AVB BRW_NYTID 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

