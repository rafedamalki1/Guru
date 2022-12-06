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

  Created: 95/10/22 -  2:57 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

 
 
/* Local Variable Definitions ---                                       */
 {ALLDEF.I}
 {GLOBVAR2DEL1.I}
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE sokant AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE invdat AS DATE NO-UNDO.

DEFINE VARIABLE mtrlbapph AS HANDLE NO-UNDO.
DEFINE VARIABLE fackvar AS CHARACTER NO-UNDO.  
DEFINE VARIABLE var1 AS CHARACTER FORMAT "X(6)" NO-UNDO.
DEFINE VARIABLE var2 AS CHARACTER FORMAT "X(6)" NO-UNDO.   
DEFINE VARIABLE fackid AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO. 
DEFINE VARIABLE nya AS CHARACTER NO-UNDO. 
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{SPECMTRLTEMP.I}
{MTRLTEMP.I}


DEFINE INPUT PARAMETER TABLE FOR spec_mtrl.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_EJLAGER

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES spec_mtrl

/* Definitions for BROWSE BRW_EJLAGER                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_EJLAGER spec_mtrl.Enr ~
spec_mtrl.Benamning spec_mtrl.Enhet spec_mtrl.BESTPUNKT spec_mtrl.Bestkvant ~
spec_mtrl.FACKID1 spec_mtrl.FACKID2 spec_mtrl.OMSATT spec_mtrl.LAGER 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_EJLAGER spec_mtrl.BESTPUNKT ~
spec_mtrl.Bestkvant spec_mtrl.FACKID1 spec_mtrl.FACKID2 spec_mtrl.OMSATT ~
spec_mtrl.LAGER 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_EJLAGER spec_mtrl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_EJLAGER spec_mtrl
&Scoped-define QUERY-STRING-BRW_EJLAGER FOR EACH spec_mtrl NO-LOCK
&Scoped-define OPEN-QUERY-BRW_EJLAGER OPEN QUERY BRW_EJLAGER FOR EACH spec_mtrl NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_EJLAGER spec_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_EJLAGER spec_mtrl


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_EJLAGER btn_ok BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BRW_EJLAGER 
       MENU-ITEM m_Ta_med_markerade_poster LABEL "Lagerförs"     
       MENU-ITEM m_Ta_ej_med_markerade_poster LABEL "Lagerförs ej"  .


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON btn_ok 
     LABEL "Ok":L 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_EJLAGER FOR 
      spec_mtrl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_EJLAGER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_EJLAGER DIALOG-1 _STRUCTURED
  QUERY BRW_EJLAGER NO-LOCK DISPLAY
      spec_mtrl.Enr FORMAT "X(11)":U
      spec_mtrl.Benamning FORMAT "x(30)":U
      spec_mtrl.Enhet FORMAT "x(5)":U
      spec_mtrl.BESTPUNKT FORMAT ">>>>9":U
      spec_mtrl.Bestkvant FORMAT "->>>>9.99":U
      spec_mtrl.FACKID1 COLUMN-LABEL "Fack-id (aa)" FORMAT "X(8)":U
      spec_mtrl.FACKID2 COLUMN-LABEL "+ (999999)" FORMAT "X(8)":U
      spec_mtrl.OMSATT FORMAT "->>>>9.99":U WIDTH 14
      spec_mtrl.LAGER FORMAT "Ja/Nej":U
  ENABLE
      spec_mtrl.BESTPUNKT
      spec_mtrl.Bestkvant
      spec_mtrl.FACKID1
      spec_mtrl.FACKID2
      spec_mtrl.OMSATT
      spec_mtrl.LAGER
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 122 BY 14.25
         TITLE "Ej lagerförd materiel".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BRW_EJLAGER AT ROW 1.75 COL 1.5
     btn_ok AT ROW 16.25 COL 94.5
     BTN_AVB AT ROW 16.25 COL 109.5
     SPACE(0.49) SKIP(0.32)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Depå information":L
         CANCEL-BUTTON BTN_AVB.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: spec_mtrl T "?" NO-UNDO temp-db spec_mtrl
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_EJLAGER 1 DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

ASSIGN 
       BRW_EJLAGER:POPUP-MENU IN FRAME DIALOG-1             = MENU POPUP-MENU-BRW_EJLAGER:HANDLE
       BRW_EJLAGER:ALLOW-COLUMN-SEARCHING IN FRAME DIALOG-1 = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_EJLAGER
/* Query rebuild information for BROWSE BRW_EJLAGER
     _TblList          = "Temp-Tables.spec_mtrl"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.spec_mtrl.Enr
     _FldNameList[2]   > Temp-Tables.spec_mtrl.Benamning
"spec_mtrl.Benamning" ? "x(30)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   = Temp-Tables.spec_mtrl.Enhet
     _FldNameList[4]   > Temp-Tables.spec_mtrl.BESTPUNKT
"spec_mtrl.BESTPUNKT" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.spec_mtrl.Bestkvant
"spec_mtrl.Bestkvant" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.spec_mtrl.FACKID1
"spec_mtrl.FACKID1" "Fack-id (aa)" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.spec_mtrl.FACKID2
"spec_mtrl.FACKID2" "+ (999999)" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.spec_mtrl.OMSATT
"spec_mtrl.OMSATT" ? ? "decimal" ? ? ? ? ? ? yes ? no no "14" yes no no "U" "" ""
     _FldNameList[9]   > Temp-Tables.spec_mtrl.LAGER
"spec_mtrl.LAGER" ? "Ja/Nej" "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_EJLAGER */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Depå information */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(mtrlbapph) THEN DELETE PROCEDURE mtrlbapph.
   APPLY "CHOOSE" TO BTN_OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Depå information */
DO:   
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_EJLAGER
&Scoped-define SELF-NAME BRW_EJLAGER
&Scoped-define SELF-NAME spec_mtrl.BESTPUNKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.BESTPUNKT BRW_EJLAGER _BROWSE-COLUMN DIALOG-1
ON ENTRY OF spec_mtrl.BESTPUNKT IN BROWSE BRW_EJLAGER /* best-punkt */
DO:
  RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.BESTPUNKT BRW_EJLAGER _BROWSE-COLUMN DIALOG-1
ON LEAVE OF spec_mtrl.BESTPUNKT IN BROWSE BRW_EJLAGER /* best-punkt */
DO:  
   IF spec_mtrl.BESTPUNKT NE INPUT BROWSE BRW_EJLAGER spec_mtrl.BESTPUNKT THEN DO:        
      spec_mtrl.BESTPUNKT = INPUT BROWSE BRW_EJLAGER spec_mtrl.BESTPUNKT.
      DISPLAY spec_mtrl.BESTPUNKT WITH BROWSE BRW_EJLAGER.         
   END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME spec_mtrl.Bestkvant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.Bestkvant BRW_EJLAGER _BROWSE-COLUMN DIALOG-1
ON ENTRY OF spec_mtrl.Bestkvant IN BROWSE BRW_EJLAGER /* best-kvant */
DO:
  RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.Bestkvant BRW_EJLAGER _BROWSE-COLUMN DIALOG-1
ON LEAVE OF spec_mtrl.Bestkvant IN BROWSE BRW_EJLAGER /* best-kvant */
DO:
  IF spec_mtrl.Bestkvant NE INPUT BROWSE BRW_EJLAGER spec_mtrl.Bestkvant THEN DO:     
     IF INPUT BROWSE BRW_EJLAGER spec_mtrl.Bestkvant = 0 THEN DO:
        IF INPUT BROWSE BRW_EJLAGER spec_mtrl.LAGER = TRUE THEN DO:
           MESSAGE "Best-antal får inte vara 0."   
           VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
        END.
     END.              
     spec_mtrl.Bestkvant = INPUT BROWSE BRW_EJLAGER spec_mtrl.Bestkvant.
     DISPLAY spec_mtrl.Bestkvant WITH BROWSE BRW_EJLAGER.         
      
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME spec_mtrl.FACKID1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.FACKID1 BRW_EJLAGER _BROWSE-COLUMN DIALOG-1
ON ENTRY OF spec_mtrl.FACKID1 IN BROWSE BRW_EJLAGER /* Fack-id (aa) */
DO:
  RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.FACKID1 BRW_EJLAGER _BROWSE-COLUMN DIALOG-1
ON LEAVE OF spec_mtrl.FACKID1 IN BROWSE BRW_EJLAGER /* Fack-id (aa) */
DO:
  IF spec_mtrl.FACKID1 NE INPUT BROWSE BRW_EJLAGER spec_mtrl.FACKID1 THEN DO:     
      ASSIGN
      nya = INPUT BROWSE BRW_EJLAGER spec_mtrl.FACKID1
      var1 =  nya
      var1 = TRIM(var1)
      langd = LENGTH(var1).  
      IF langd = 2 THEN DO:
         nya = var1.      
      END.
      ELSE DO:
         DO WHILE langd < 2:
            ASSIGN
            var1 = var1 + " " 
            langd = langd + 1.
         END.  
         nya = var1.     
      END.      
   
     spec_mtrl.FACKID1 = nya.
     DISPLAY spec_mtrl.FACKID1 WITH BROWSE BRW_EJLAGER.       
     
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME spec_mtrl.FACKID2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.FACKID2 BRW_EJLAGER _BROWSE-COLUMN DIALOG-1
ON ENTRY OF spec_mtrl.FACKID2 IN BROWSE BRW_EJLAGER /* + (999999) */
DO:
  RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.FACKID2 BRW_EJLAGER _BROWSE-COLUMN DIALOG-1
ON LEAVE OF spec_mtrl.FACKID2 IN BROWSE BRW_EJLAGER /* + (999999) */
DO:  
   ASSIGN
   nya = INPUT BROWSE BRW_EJLAGER spec_mtrl.FACKID2 
   var2 = nya
   var2 = TRIM(var2)
   langd = LENGTH(var2).  
   IF langd = 6 THEN DO:
      nya = var2.      
   END.
   ELSE DO:
      DO WHILE langd < 6:
         ASSIGN
         var2 = " " + var2
         langd = langd + 1.
      END.  
      nya = var2.     
   END.  
   spec_mtrl.FACKID2 = nya.
   DISPLAY spec_mtrl.FACKID2 WITH BROWSE BRW_EJLAGER.       
  
  fackid = var1 + var2.
  spec_mtrl.FACKID = fackid.
  IF spec_mtrl.FACKID NE "" THEN DO:
     FIND FIRST mtrldeptemp WHERE mtrldeptemp.FACKID = spec_mtrl.FACKID AND mtrldeptemp.DEPNR = vald_depa
      NO-LOCK NO-ERROR.
      IF AVAILABLE mtrldeptemp THEN DO:            
         MESSAGE "Det finns redan ett " + Guru.Konstanter:genk + " kopplat till detta Fack-id. Ange ett nytt." 
         VIEW-AS ALERT-BOX.         
         RETURN NO-APPLY.
     END.         
  END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME spec_mtrl.OMSATT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.OMSATT BRW_EJLAGER _BROWSE-COLUMN DIALOG-1
ON ENTRY OF spec_mtrl.OMSATT IN BROWSE BRW_EJLAGER /* omsättning/år */
DO:
  RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.OMSATT BRW_EJLAGER _BROWSE-COLUMN DIALOG-1
ON LEAVE OF spec_mtrl.OMSATT IN BROWSE BRW_EJLAGER /* omsättning/år */
DO:
  IF spec_mtrl.OMSATT NE INPUT BROWSE BRW_EJLAGER spec_mtrl.OMSATT THEN DO:     
     spec_mtrl.OMSATT = INPUT BROWSE BRW_EJLAGER spec_mtrl.OMSATT.
     DISPLAY spec_mtrl.OMSATT WITH BROWSE BRW_EJLAGER.            
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME spec_mtrl.LAGER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.LAGER BRW_EJLAGER _BROWSE-COLUMN DIALOG-1
ON ENTRY OF spec_mtrl.LAGER IN BROWSE BRW_EJLAGER /* lagerförs */
DO:
  RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.LAGER BRW_EJLAGER _BROWSE-COLUMN DIALOG-1
ON LEAVE OF spec_mtrl.LAGER IN BROWSE BRW_EJLAGER /* lagerförs */
DO:
  IF spec_mtrl.LAGER NE INPUT BROWSE BRW_EJLAGER spec_mtrl.LAGER THEN DO:     
     spec_mtrl.LAGER = INPUT BROWSE BRW_EJLAGER spec_mtrl.LAGER.
     DISPLAY spec_mtrl.LAGER WITH BROWSE BRW_EJLAGER.            
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.LAGER BRW_EJLAGER _BROWSE-COLUMN DIALOG-1
ON MOUSE-SELECT-CLICK OF spec_mtrl.LAGER IN BROWSE BRW_EJLAGER /* lagerförs */
DO:
  IF INPUT BROWSE BRW_EJLAGER spec_mtrl.LAGER = TRUE THEN spec_mtrl.LAGER = FALSE.
  IF INPUT BROWSE BRW_EJLAGER spec_mtrl.LAGER = FALSE THEN spec_mtrl.LAGER = TRUE.
  DISPLAY spec_mtrl.LAGER WITH BROWSE BRW_EJLAGER. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok DIALOG-1
ON CHOOSE OF btn_ok IN FRAME DIALOG-1 /* Ok */
DO: 
   RUN spmtrldep_UI IN mtrlbapph (INPUT vald_depa, INPUT invdat, INPUT TABLE spec_mtrl).             
   IF VALID-HANDLE(mtrlbapph) THEN DELETE PROCEDURE mtrlbapph.
   {BORTBRWPROC.I}
   APPLY "GO" TO FRAME {&FRAME-NAME}.           
   
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
   {DIA_M_START.I}
   {ALLSTARTDYN.I}
   spec_mtrl.ENR:LABEL IN BROWSE BRW_EJLAGER = Guru.Konstanter:genk.
   RUN openbdynspec_UI IN brwproc[1].  
 
   RUN enable_UI.       
   {FRMSIZED.I}  
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
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_EJLAGER:HANDLE IN FRAME {&FRAME-NAME}).   
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph.
   END.   
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
  ENABLE BRW_EJLAGER btn_ok BTN_AVB 
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
   spec_mtrl.BESTPUNKT spec_mtrl.BESTKVANT spec_mtrl.FACKID1 spec_mtrl.FACKID2 spec_mtrl.OMSATT spec_mtrl.LAGER
   WITH BROWSE BRW_EJLAGER.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

