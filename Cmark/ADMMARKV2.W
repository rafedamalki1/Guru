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

  Created: 04/15/97 -  9:24 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

&Scoped-define SHARED SHARED   
{MARKVARD.I}
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE kindrec AS RECID NO-UNDO.
DEFINE VARIABLE kindrec2 AS RECID NO-UNDO.
DEFINE VARIABLE volkrec AS RECID NO-UNDO.
DEFINE VARIABLE volkrec2 AS RECID NO-UNDO.
DEFINE VARIABLE diamvar AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE artalvar AS INTEGER NO-UNDO.
DEFINE VARIABLE manadvar AS INTEGER NO-UNDO.
DEFINE VARIABLE admmarkh AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_KINDEX

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES kindextemp volkrtemp

/* Definitions for BROWSE BRW_KINDEX                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_KINDEX kindextemp.AR kindextemp.MANAD ~
kindextemp.VARDE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KINDEX kindextemp.AR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_KINDEX kindextemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_KINDEX kindextemp
&Scoped-define QUERY-STRING-BRW_KINDEX FOR EACH kindextemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KINDEX OPEN QUERY BRW_KINDEX FOR EACH kindextemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KINDEX kindextemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KINDEX kindextemp


/* Definitions for BROWSE BRW_VOLKR                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_VOLKR volkrtemp.DIAM volkrtemp.TALL ~
volkrtemp.GRAN volkrtemp.BJORK 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VOLKR volkrtemp.DIAM 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VOLKR volkrtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VOLKR volkrtemp
&Scoped-define QUERY-STRING-BRW_VOLKR FOR EACH volkrtemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_VOLKR OPEN QUERY BRW_VOLKR FOR EACH volkrtemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_VOLKR volkrtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VOLKR volkrtemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DIALOG-1 ~
    ~{&OPEN-QUERY-BRW_VOLKR}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_VOLKR BTN_NYVOL BTN_UPPVOL BTN_BORTVOL ~
BRW_KINDEX BTN_NY BTN_UPP BTN_BORT BTN_AVSL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVSL 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_BORTVOL 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NYVOL 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_UPP 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_UPPVOL 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_KINDEX FOR 
      kindextemp SCROLLING.

DEFINE QUERY BRW_VOLKR FOR 
      volkrtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_KINDEX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KINDEX DIALOG-1 _STRUCTURED
  QUERY BRW_KINDEX NO-LOCK DISPLAY
      kindextemp.AR COLUMN-LABEL "År" FORMAT "9999":U
      kindextemp.MANAD COLUMN-LABEL "Månad" FORMAT ">9":U
      kindextemp.VARDE COLUMN-LABEL "Värde" FORMAT "->>>>>>9":U
  ENABLE
      kindextemp.AR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 24.38 BY 8.42
         TITLE "Index Iv".

DEFINE BROWSE BRW_VOLKR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VOLKR DIALOG-1 _STRUCTURED
  QUERY BRW_VOLKR NO-LOCK DISPLAY
      volkrtemp.DIAM COLUMN-LABEL "Diam" FORMAT ">9":U
      volkrtemp.TALL COLUMN-LABEL "Tall" FORMAT "->>>>>>9":U
      volkrtemp.GRAN COLUMN-LABEL "Gran" FORMAT "->>>>>>9":U
      volkrtemp.BJORK COLUMN-LABEL "Björk" FORMAT "->>>>>>9":U
  ENABLE
      volkrtemp.DIAM
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 43 BY 7.75
         TITLE "Rotvärdestabell".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BRW_VOLKR AT ROW 1.29 COL 1.5
     BTN_NYVOL AT ROW 9.75 COL 3.13
     BTN_UPPVOL AT ROW 9.75 COL 16.13
     BTN_BORTVOL AT ROW 9.75 COL 29.13
     BRW_KINDEX AT ROW 11.63 COL 9.13
     BTN_NY AT ROW 20.75 COL 3.13
     BTN_UPP AT ROW 20.75 COL 16.13
     BTN_BORT AT ROW 20.75 COL 29.13
     BTN_AVSL AT ROW 22.33 COL 30.5
     SPACE(0.87) SKIP(0.37)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ändra markvärderingsdata".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: kindextemp T "?" NO-UNDO temp-db kindextemp
      TABLE: volkrtemp T "?" NO-UNDO temp-db volkrtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_VOLKR 1 DIALOG-1 */
/* BROWSE-TAB BRW_KINDEX BTN_BORTVOL DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KINDEX
/* Query rebuild information for BROWSE BRW_KINDEX
     _TblList          = "Temp-Tables.kindextemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kindextemp.AR
"kindextemp.AR" "År" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kindextemp.MANAD
"kindextemp.MANAD" "Månad" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.kindextemp.VARDE
"kindextemp.VARDE" "Värde" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KINDEX */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VOLKR
/* Query rebuild information for BROWSE BRW_VOLKR
     _TblList          = "Temp-Tables.volkrtemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.volkrtemp.DIAM
"volkrtemp.DIAM" "Diam" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.volkrtemp.TALL
"volkrtemp.TALL" "Tall" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.volkrtemp.GRAN
"volkrtemp.GRAN" "Gran" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.volkrtemp.BJORK
"volkrtemp.BJORK" "Björk" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_VOLKR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Ändra markvärderingsdata */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(admmarkh) THEN DELETE PROCEDURE admmarkh.
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Ändra markvärderingsdata */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVSL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVSL DIALOG-1
ON CHOOSE OF BTN_AVSL IN FRAME DIALOG-1 /* Avsluta */
DO:                                
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT DIALOG-1
ON CHOOSE OF BTN_BORT IN FRAME DIALOG-1 /* Ta bort */
DO:
   RUN bort_UI.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORTVOL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORTVOL DIALOG-1
ON CHOOSE OF BTN_BORTVOL IN FRAME DIALOG-1 /* Ta bort */
DO:
   RUN bovolk_UI.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY DIALOG-1
ON CHOOSE OF BTN_NY IN FRAME DIALOG-1 /* Ny */
DO:
   vart = "NYA".
   artalvar = ?.
   manadvar = ?.
   RUN ANDRAKIND.W (INPUT-OUTPUT artalvar,INPUT-OUTPUT manadvar).   
   IF musz = FALSE THEN DO:
      OPEN QUERY BRW_KINDEX FOR EACH kindextemp USE-INDEX AR NO-LOCK.
      FIND FIRST kindextemp WHERE kindextemp.AR = artalvar AND 
      kindextemp.MANAD = manadvar NO-LOCK NO-ERROR.  
      IF AVAILABLE kindextemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kindextemp)).
         RUN lastselectdyn_UI IN brwproc[1].    
         musz = FALSE.      
      END.     
   END.
   musz = FALSE. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NYVOL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NYVOL DIALOG-1
ON CHOOSE OF BTN_NYVOL IN FRAME DIALOG-1 /* Ny */
DO:
   diamvar = ?.
   vart = "NYA".
   RUN ANDRAVOLKR.W (INPUT-OUTPUT diamvar).
   IF musz = FALSE THEN DO:
      OPEN QUERY BRW_VOLKR FOR EACH volkrtemp USE-INDEX DIAM NO-LOCK.  
      FIND FIRST volkrtemp WHERE volkrtemp.DIAM = diamvar NO-LOCK NO-ERROR.
      IF AVAILABLE volkrtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(volkrtemp)).
         RUN lastselectdyn_UI IN brwproc[2].    
      END.
   END.
   musz = FALSE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP DIALOG-1
ON CHOOSE OF BTN_UPP IN FRAME DIALOG-1 /* Ändra */
DO:
   RUN andra_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPPVOL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPPVOL DIALOG-1
ON CHOOSE OF BTN_UPPVOL IN FRAME DIALOG-1 /* Ändra */
DO:
   RUN anvol_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KINDEX
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
   {DIA_M_START.I}
   {muswait.i}   
   {ALLSTARTDYN.I} 
   RUN laddabrw IN admmarkh (OUTPUT TABLE kindextemp,OUTPUT TABLE volkrtemp).
   RUN enable_UI.       
   {FRMSIZED.I} 
   musz = TRUE.
   OPEN QUERY BRW_KINDEX FOR EACH kindextemp USE-INDEX AR NO-LOCK .
   FIND LAST kindextemp USE-INDEX AR NO-LOCK NO-ERROR.
   IF AVAILABLE kindextemp THEN DO:
      ASSIGN
      artalvar = kindextemp.AR
      manadvar = kindextemp.MANAD.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kindextemp)).
      RUN lastselectdyn_UI IN brwproc[1].      
      musz = FALSE.      
   END.     
   IF musz = TRUE THEN DO:
      musz = FALSE.
      BRW_KINDEX:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END. 
   ELSE DO:
      ENABLE BRW_KINDEX WITH FRAME {&FRAME-NAME}.
      BRW_KINDEX:HIDDEN = FALSE.
   END. 

   {musarrow.i}
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   ASSIGN
   kindextemp.AR:READ-ONLY IN BROWSE BRW_KINDEX = TRUE    
   volkrtemp.DIAM:READ-ONLY IN BROWSE BRW_VOLKR = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_KINDEX:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_VOLKR:HANDLE IN FRAME {&FRAME-NAME}).

   IF Guru.Konstanter:appcon THEN DO:
      RUN ADMMARKVAPP.P PERSISTENT SET admmarkh ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN ADMMARKVAPP.P PERSISTENT SET admmarkh.
   END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI DIALOG-1 
PROCEDURE andra_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   status-ok = BRW_KINDEX:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   APPLY "VALUE-CHANGED" TO BRW_KINDEX.
   ASSIGN
   artalvar = kindextemp.AR
   manadvar = kindextemp.MANAD.
   kindrec = RECID(kindextemp).
   {muswait.i}
   vart = "AND".
   RUN ANDRAKIND.W (INPUT-OUTPUT artalvar,INPUT-OUTPUT manadvar).
   OPEN QUERY BRW_KINDEX FOR EACH kindextemp USE-INDEX AR NO-LOCK.
   FIND FIRST kindextemp WHERE kindextemp.AR = artalvar AND 
   kindextemp.MANAD = manadvar NO-LOCK NO-ERROR.
   IF AVAILABLE kindextemp THEN DO:
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kindextemp)).
      RUN lastselectdyn_UI IN brwproc[1].  
   END.
   {musarrow.i}
   musz = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anvol_UI DIALOG-1 
PROCEDURE anvol_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   status-ok = BRW_VOLKR:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   APPLY "VALUE-CHANGED" TO BRW_VOLKR.
   diamvar = volkrtemp.DIAM.
   {muswait.i}
   vart = "AND".
   RUN ANDRAVOLKR.W (INPUT-OUTPUT diamvar).
   OPEN QUERY BRW_VOLKR FOR EACH volkrtemp USE-INDEX DIAM NO-LOCK .    
   FIND FIRST volkrtemp WHERE volkrtemp.DIAM  = diamvar NO-LOCK NO-ERROR.
   IF AVAILABLE volkrtemp THEN DO:
      RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(volkrtemp)).
      RUN lastselectdyn_UI IN brwproc[2].  
   END.
   {musarrow.i}
   musz = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI DIALOG-1 
PROCEDURE bort_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   status-ok = BRW_KINDEX:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   APPLY "VALUE-CHANGED" TO BRW_KINDEX.
   kindrec = RECID(kindextemp).
   kindrec2 = kindrec.
   MESSAGE "Vill du ta bort denna post"
   kindextemp.AR kindextemp.MANAD kindextemp.VARDE "?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bortag av index Iv"
   UPDATE answer AS LOGICAL.
   {muswait.i}
   IF answer THEN DO:      
      status-ok = BRW_KINDEX:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME}.  
      IF status-ok = TRUE THEN kindrec2 = RECID(kindextemp).
      ELSE DO:          
         status-ok = BRW_KINDEX:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME}.          
         IF status-ok = TRUE THEN kindrec2 = RECID(kindextemp).          
         ELSE kindrec2 = kindrec.
      END.
      FIND kindextemp WHERE RECID(kindextemp) = kindrec NO-LOCK NO-ERROR.
      IF AVAILABLE kindextemp THEN DO:
         RUN delkindex IN admmarkh (INPUT kindextemp.AR,INPUT kindextemp.MANAD).
         FIND kindextemp WHERE RECID(kindextemp) = kindrec NO-LOCK NO-ERROR.
         DELETE kindextemp.        
      END.
      OPEN QUERY BRW_KINDEX FOR EACH kindextemp USE-INDEX AR NO-LOCK.
      IF kindrec2 NE kindrec THEN DO: 
         FIND kindextemp WHERE RECID(kindextemp) = kindrec2 NO-LOCK NO-ERROR.
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kindextemp)).
         RUN lastselectdyn_UI IN brwproc[1].      
      END.
   END.
   musz = FALSE.  
   {musarrow.i}   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bovolk_UI DIALOG-1 
PROCEDURE bovolk_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   status-ok = BRW_VOLKR:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   APPLY "VALUE-CHANGED" TO BRW_VOLKR.
   volkrec = RECID(volkrtemp).
   volkrec2 = volkrec.
   MESSAGE "VILL DU VERKLIGEN TA BORT DENNA REGISTRERING "
   volkrtemp.DIAM "?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "BORTAGNING AV ROTVÄRDE"
   UPDATE answer AS LOGICAL.
   {muswait.i}
   IF answer THEN DO:      
      status-ok = BRW_VOLKR:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME}.  
      IF status-ok = TRUE THEN volkrec2 = RECID(volkrtemp).
      ELSE DO:          
         status-ok = BRW_VOLKR:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME}.          
         IF status-ok = TRUE THEN volkrec2 = RECID(volkrtemp).          
         ELSE volkrec2 = volkrec.
      END.
      FIND FIRST volkrtemp WHERE RECID(volkrtemp) = volkrec NO-LOCK NO-ERROR.     
      IF AVAILABLE volkrtemp THEN DO:
         RUN delvolkr IN admmarkh (INPUT volkrtemp.DIAM).
         FIND FIRST volkrtemp WHERE RECID(volkrtemp) = volkrec NO-LOCK NO-ERROR.
         DELETE volkrtemp.  
      END.
      OPEN QUERY BRW_VOLKR FOR EACH volkrtemp USE-INDEX DIAM NO-LOCK.
      IF volkrec2 NE volkrec THEN DO:
         FIND FIRST volkrtemp WHERE RECID(volkrtemp) = volkrec2 NO-LOCK NO-ERROR.
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(volkrtemp)).
         RUN lastselectdyn_UI IN brwproc[2].  
      END.
   END.
   musz = FALSE.  
   {musarrow.i}   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  ENABLE BRW_VOLKR BTN_NYVOL BTN_UPPVOL BTN_BORTVOL BRW_KINDEX BTN_NY BTN_UPP 
         BTN_BORT BTN_AVSL 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE klar_UI DIALOG-1 
PROCEDURE klar_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   
   END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

