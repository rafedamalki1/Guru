&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/08/18 - 10:19 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Local Variable Definitions ---                                       */
/*RESDEF.I*/
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE  TEMP-TABLE respers NO-UNDO   
   FIELD AONR AS CHARACTER 
   FIELD DELNR AS INTEGER 
   FIELD VECKONUMMER AS INTEGER
   FIELD DATUM AS DATE 
   FIELD DAG AS CHARACTER 
   FIELD START AS DECIMAL 
   FIELD SLUT AS DECIMAL
   FIELD PRIS AS DECIMAL
   FIELD PRISTYP AS CHARACTER 
   FIELD NATTRAKT AS LOGICAL FORMAT "JA/NEJ" LABEL "NATT TRAKT"
   FIELD OVERTIDUTTAG AS CHARACTER 
   FIELD BILFORARE AS LOGICAL
   FIELD ENFLERDAGS AS CHARACTER  
   FIELD TIDREC AS RECID
   FIELD KILOMETER AS DECIMAL
   INDEX RESPERS IS PRIMARY DATUM START SLUT ASCENDING.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_RESA-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES respers

/* Definitions for BROWSE BRW_RESA-2                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_RESA-2 respers.DATUM respers.DAG ~
respers.AONR respers.DELNR respers.Kilometer respers.START respers.SLUT ~
respers.BILFORARE respers.NATTRAKT respers.OVERTIDUTTAG 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_RESA-2 respers.AONR ~
respers.DELNR respers.Kilometer respers.START respers.SLUT ~
respers.BILFORARE respers.NATTRAKT respers.OVERTIDUTTAG 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_RESA-2 respers
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_RESA-2 respers
&Scoped-define QUERY-STRING-BRW_RESA-2 FOR EACH respers NO-LOCK
&Scoped-define OPEN-QUERY-BRW_RESA-2 OPEN QUERY BRW_RESA-2 FOR EACH respers NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_RESA-2 respers
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_RESA-2 respers


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 12 BY 2.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_RESA-2 FOR 
      respers SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_RESA-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_RESA-2 DIALOG-1 _STRUCTURED
  QUERY BRW_RESA-2 NO-LOCK DISPLAY
      respers.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      respers.DAG COLUMN-LABEL "Dag" FORMAT "x(3)":U
      respers.AONR COLUMN-LABEL "Aonr" FORMAT "X(256)":U WIDTH 6
      respers.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      respers.Kilometer COLUMN-LABEL "Kilo-!meter" FORMAT ">>>>9":U
      respers.START COLUMN-LABEL "Start tid" FORMAT "99.99":U
      respers.SLUT COLUMN-LABEL "Slut tid" FORMAT "99.99":U
      respers.BILFORARE COLUMN-LABEL "Bil-!förare" FORMAT "Ja/Nej":U
      respers.NATTRAKT COLUMN-LABEL "Natt-!trakt." FORMAT "Ja/Nej":U
      respers.OVERTIDUTTAG COLUMN-LABEL "Övertids!uttag" FORMAT "X(1)":U
  ENABLE
      respers.AONR
      respers.DELNR
      respers.Kilometer
      respers.START
      respers.SLUT
      respers.BILFORARE
      respers.NATTRAKT
      respers.OVERTIDUTTAG
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 74.5 BY 12.96.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BRW_RESA-2 AT ROW 8.38 COL 6
     BTN_AVB AT ROW 20.25 COL 86
     SPACE(8.24) SKIP(0.69)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Tjänsteresor":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db respers
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_RESA-2 1 DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE.

/* SETTINGS FOR BROWSE BRW_RESA-2 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_RESA-2:HIDDEN  IN FRAME DIALOG-1                = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_RESA-2
/* Query rebuild information for BROWSE BRW_RESA-2
     _TblList          = "Temp-Tables.respers"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.respers.DATUM
"respers.DATUM" "Datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.respers.DAG
"respers.DAG" "Dag" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.respers.AONR
"respers.AONR" "Aonr" "X(256)" "character" ? ? ? ? ? ? yes ? no no "6" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.respers.DELNR
"respers.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.respers.Kilometer
"respers.Kilometer" "Kilo-!meter" ">>>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.respers.START
"respers.START" "Start tid" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.respers.SLUT
"respers.SLUT" "Slut tid" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.respers.BILFORARE
"respers.BILFORARE" "Bil-!förare" "Ja/Nej" "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > Temp-Tables.respers.NATTRAKT
"respers.NATTRAKT" "Natt-!trakt." "Ja/Nej" "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > Temp-Tables.respers.OVERTIDUTTAG
"respers.OVERTIDUTTAG" "Övertids!uttag" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_RESA-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Tjänsteresor */
DO:
   
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_RESA-2
&Scoped-define SELF-NAME BRW_RESA-2
&Scoped-define SELF-NAME respers.AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.AONR BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ANY-KEY OF respers.AONR IN BROWSE BRW_RESA-2 /* Aonr */
DO:
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:            
      APPLY "ENTRY" TO respers.DELNR IN BROWSE BRW_RESA-2.      
      RETURN NO-APPLY.      
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.AONR BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ENTRY OF respers.AONR IN BROWSE BRW_RESA-2 /* Aonr */
DO:
  
   RUN visa_UI.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.AONR BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON LEAVE OF respers.AONR IN BROWSE BRW_RESA-2 /* Aonr */
DO:
   respers.AONR = INPUT BROWSE BRW_RESA-2 respers.AONR.
   DISPLAY respers.AONR WITH BROWSE BRW_RESA-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME respers.DELNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.DELNR BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ANY-KEY OF respers.DELNR IN BROWSE BRW_RESA-2 /* Delnr */
DO:
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:            
      APPLY "ENTRY" TO respers.KILOMETER IN BROWSE BRW_RESA-2.      
      RETURN NO-APPLY.      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.DELNR BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ENTRY OF respers.DELNR IN BROWSE BRW_RESA-2 /* Delnr */
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.DELNR BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON LEAVE OF respers.DELNR IN BROWSE BRW_RESA-2 /* Delnr */
DO:
   respers.DELNR = INPUT BROWSE BRW_RESA-2 respers.DELNR.   
   RUN visa_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME respers.Kilometer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.Kilometer BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ANY-KEY OF respers.Kilometer IN BROWSE BRW_RESA-2 /* Kilo-!meter */
DO:
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:            
      APPLY "ENTRY" TO respers.START IN BROWSE BRW_RESA-2.      
      RETURN NO-APPLY.      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.Kilometer BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ENTRY OF respers.Kilometer IN BROWSE BRW_RESA-2 /* Kilo-!meter */
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.Kilometer BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON LEAVE OF respers.Kilometer IN BROWSE BRW_RESA-2 /* Kilo-!meter */
DO:
   respers.KILOMETER = INPUT BROWSE BRW_RESA-2 respers.KILOMETER.
   RUN visa_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME respers.START
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.START BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ANY-KEY OF respers.START IN BROWSE BRW_RESA-2 /* Start tid */
DO:
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:            
      APPLY "ENTRY" TO respers.SLUT IN BROWSE BRW_RESA-2.      
      RETURN NO-APPLY.      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.START BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ENTRY OF respers.START IN BROWSE BRW_RESA-2 /* Start tid */
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.START BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON LEAVE OF respers.START IN BROWSE BRW_RESA-2 /* Start tid */
DO:
   respers.START = INPUT BROWSE BRW_RESA-2 respers.START.
   DISPLAY respers.START WITH BROWSE BRW_RESA-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME respers.SLUT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.SLUT BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ANY-KEY OF respers.SLUT IN BROWSE BRW_RESA-2 /* Slut tid */
DO:
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:            
      APPLY "ENTRY" TO respers.BILFORARE IN BROWSE BRW_RESA-2.      
      RETURN NO-APPLY.      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.SLUT BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ENTRY OF respers.SLUT IN BROWSE BRW_RESA-2 /* Slut tid */
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.SLUT BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON LEAVE OF respers.SLUT IN BROWSE BRW_RESA-2 /* Slut tid */
DO:
   respers.SLUT = INPUT BROWSE BRW_RESA-2 respers.SLUT.
   DISPLAY respers.SLUT WITH BROWSE BRW_RESA-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME respers.BILFORARE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.BILFORARE BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ANY-KEY OF respers.BILFORARE IN BROWSE BRW_RESA-2 /* Bil-!förare */
DO:
  IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:            
      APPLY "ENTRY" TO respers.NATTRAKT IN BROWSE BRW_RESA-2.      
      RETURN NO-APPLY.      
   END. 
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.BILFORARE BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ENTRY OF respers.BILFORARE IN BROWSE BRW_RESA-2 /* Bil-!förare */
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.BILFORARE BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON LEAVE OF respers.BILFORARE IN BROWSE BRW_RESA-2 /* Bil-!förare */
DO:
   respers.BILFORARE = INPUT BROWSE BRW_RESA-2 respers.BILFORARE.
   DISPLAY respers.BILFORARE WITH BROWSE BRW_RESA-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.BILFORARE BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON MOUSE-SELECT-CLICK OF respers.BILFORARE IN BROWSE BRW_RESA-2 /* Bil-!förare */
DO:
   IF INPUT BROWSE BRW_RESA-2 respers.BILFORARE = TRUE THEN respers.BILFORARE = FALSE.
   IF INPUT respers.BILFORARE = FALSE THEN respers.BILFORARE = TRUE.
   DISPLAY respers.BILFORARE WITH BROWSE BRW_RESA-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME respers.NATTRAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.NATTRAKT BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ANY-KEY OF respers.NATTRAKT IN BROWSE BRW_RESA-2 /* Natt-!trakt. */
DO:
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:            
      APPLY "ENTRY" TO respers.OVERTIDUTTAG IN BROWSE BRW_RESA-2.      
      RETURN NO-APPLY.      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.NATTRAKT BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ENTRY OF respers.NATTRAKT IN BROWSE BRW_RESA-2 /* Natt-!trakt. */
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.NATTRAKT BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON LEAVE OF respers.NATTRAKT IN BROWSE BRW_RESA-2 /* Natt-!trakt. */
DO:
    respers.NATTRAKT = INPUT BROWSE BRW_RESA-2 respers.NATTRAKT.
   DISPLAY respers.NATTRAKT WITH BROWSE BRW_RESA-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.NATTRAKT BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON MOUSE-SELECT-CLICK OF respers.NATTRAKT IN BROWSE BRW_RESA-2 /* Natt-!trakt. */
DO:
   IF INPUT BROWSE BRW_RESA-2 respers.NATTRAKT = TRUE THEN respers.NATTRAKT = FALSE.
   IF INPUT respers.NATTRAKT = FALSE THEN respers.NATTRAKT = TRUE.
   DISPLAY respers.NATTRAKT WITH BROWSE BRW_RESA-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME respers.OVERTIDUTTAG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.OVERTIDUTTAG BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ENDKEY OF respers.OVERTIDUTTAG IN BROWSE BRW_RESA-2 /* Övertids!uttag */
DO:
   APPLY "ENTRY" TO respers.AONR IN BROWSE BRW_RESA-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.OVERTIDUTTAG BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ENTRY OF respers.OVERTIDUTTAG IN BROWSE BRW_RESA-2 /* Övertids!uttag */
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.OVERTIDUTTAG BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON LEAVE OF respers.OVERTIDUTTAG IN BROWSE BRW_RESA-2 /* Övertids!uttag */
DO:
      respers.OVERTIDUTTAG = INPUT BROWSE BRW_RESA-2 respers.OVERTIDUTTAG.
      DISPLAY respers.OVERTIDUTTAG WITH BROWSE BRW_RESA-2.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON GO OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
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
   
   CREATE respers.
   respers.AONR   = "123456".
   CREATE respers.
   respers.AONR   = "123457".
   CREATE respers.
   respers.AONR   = "123458".
   CREATE respers.
   respers.AONR   = "123459".
   &Scoped-define FORMATNAMN respers.AONR
   &Scoped-define BROWSE-NAME BRW_RESA-2
   {&FORMATNAMN}:WIDTH-CHARS IN BROWSE {&BROWSE-NAME} = 8.
   
   RUN enable_UI.       
   OPEN QUERY BRW_RESA-2 FOR EACH respers USE-INDEX RESPERS.
   ENABLE BRW_RESA-2 WITH FRAME {&FRAME-NAME}.
   status-ok = BRW_RESA-2:SELECT-FOCUSED-ROW() NO-ERROR.      
   FRAME {&FRAME-NAME}:HIDDEN = FALSE. 
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
  ENABLE BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI DIALOG-1 
PROCEDURE visa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DISPLAY
   respers.AONR 
   respers.BILFORARE 
   respers.DELNR respers.Kilometer respers.SLUT respers.START   
   respers.NATTRAKT respers.OVERTIDUTTAG
      WITH BROWSE BRW_RESA-2.   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

