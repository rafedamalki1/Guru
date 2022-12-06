&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/02 - 12:41 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER valvar AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE brow AS ROWID NO-UNDO.

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE mtrlbapph AS HANDLE NO-UNDO.
DEFINE VARIABLE nykonsgrp AS INTEGER NO-UNDO.
DEFINE VARIABLE nyord AS INTEGER NO-UNDO.
DEFINE VARIABLE skrivov AS LOGICAL NO-UNDO.
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{KONSTRMTRL.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_GRUPP

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES konstgrptemp

/* Definitions for BROWSE BRW_GRUPP                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_GRUPP konstgrptemp.KONSKOD ~
konstgrptemp.BENAMNING konstgrptemp.ORDNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_GRUPP 
&Scoped-define QUERY-STRING-BRW_GRUPP FOR EACH konstgrptemp NO-LOCK ~
    BY konstgrptemp.BENAMNING
&Scoped-define OPEN-QUERY-BRW_GRUPP OPEN QUERY BRW_GRUPP FOR EACH konstgrptemp NO-LOCK ~
    BY konstgrptemp.BENAMNING.
&Scoped-define TABLES-IN-QUERY-BRW_GRUPP konstgrptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_GRUPP konstgrptemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_GRUPP BTN_OK BTN_AVS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_GRUPP FOR 
      konstgrptemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_GRUPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_GRUPP WINDOW-1 _STRUCTURED
  QUERY BRW_GRUPP NO-LOCK DISPLAY
      konstgrptemp.KONSKOD COLUMN-LABEL "Konskod" FORMAT ">>>>9":U
            WIDTH 10.5
      konstgrptemp.BENAMNING FORMAT "X(30)":U
      konstgrptemp.ORDNING FORMAT ">>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 53.5 BY 14.42.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_GRUPP AT ROW 1.5 COL 1.5
     BTN_OK AT ROW 16.29 COL 26
     BTN_AVS AT ROW 16.29 COL 41
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 56.25 BY 16.67.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: konstgrptemp T "?" NO-UNDO temp-db konstgrptemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Val av grupp"
         HEIGHT             = 16.71
         WIDTH              = 56.38
         MAX-HEIGHT         = 27.25
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 27.25
         VIRTUAL-WIDTH      = 100
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-1
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* BROWSE-TAB BRW_GRUPP 1 FRAME-A */
ASSIGN 
       BRW_GRUPP:MAX-DATA-GUESS IN FRAME FRAME-A         = 1000
       BRW_GRUPP:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_GRUPP:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_GRUPP
/* Query rebuild information for BROWSE BRW_GRUPP
     _TblList          = "Temp-Tables.konstgrptemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.konstgrptemp.BENAMNING|yes"
     _FldNameList[1]   > Temp-Tables.konstgrptemp.KONSKOD
"konstgrptemp.KONSKOD" "Konskod" ? "integer" ? ? ? ? ? ? no ? no no "10.5" yes no no "U" "" ""
     _FldNameList[2]   = Temp-Tables.konstgrptemp.BENAMNING
     _FldNameList[3]   = Temp-Tables.konstgrptemp.ORDNING
     _Query            is NOT OPENED
*/  /* BROWSE BRW_GRUPP */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_GRUPP
&Scoped-define SELF-NAME BRW_GRUPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_GRUPP WINDOW-1
ON VALUE-CHANGED OF BRW_GRUPP IN FRAME FRAME-A
DO: 
   skrivov = FALSE.
   ASSIGN
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   ASSIGN
   brow = ROWID(konstgrptemp).
   IF konstgrptemp.BENAMNING = "Ny konst.grupp" THEN skrivov = FALSE.
   ELSE skrivov = TRUE.
   IF valvar = "Ladda nya konstruktioner till befintlig konstruktionsgrupp" THEN skrivov = FALSE. 
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-1
ON CHOOSE OF BTN_AVS IN FRAME FRAME-A /* Avsluta */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK WINDOW-1
ON CHOOSE OF BTN_OK IN FRAME FRAME-A /* Ok */
DO:
   {muswait.i}
   IF valvar = "Dumpa konstruktionsgrupp" THEN DO:       
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN DUMPGRUP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT Guru.Konstanter:globforetag, INPUT konstgrptemp.KONSKOD).
      END.
      ELSE DO:
         RUN DUMPGRUP.P
         (INPUT Guru.Konstanter:globforetag, INPUT konstgrptemp.KONSKOD).
      END.                                                                          
   END.
   
   ELSE IF valvar = "Ladda konstruktionsgrupp" THEN DO:    
      EMPTY TEMP-TABLE felmeddtemp NO-ERROR.    
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN LADDGRUP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT Guru.Konstanter:globforetag, INPUT konstgrptemp.KONSKOD, INPUT konstgrptemp.ORDNING, INPUT skrivov, OUTPUT TABLE felmeddtemp).
      END.
      ELSE DO:
         RUN LADDGRUP.P 
         (INPUT Guru.Konstanter:globforetag, INPUT konstgrptemp.KONSKOD , INPUT konstgrptemp.ORDNING, INPUT skrivov, OUTPUT TABLE felmeddtemp).
      END.   
      FIND FIRST felmeddtemp  NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:      
         MESSAGE felmeddtemp.FELMEDD
         VIEW-AS ALERT-BOX.
      END.                                                                    
   END.
   ELSE IF valvar = "Ladda nya konstruktioner till befintlig konstruktionsgrupp" THEN DO:    
      EMPTY TEMP-TABLE felmeddtemp NO-ERROR.    
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN LADDGRUPNY.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT Guru.Konstanter:globforetag, INPUT konstgrptemp.KONSKOD, INPUT konstgrptemp.ORDNING, INPUT skrivov, OUTPUT TABLE felmeddtemp).
      END.
      ELSE DO:
         RUN LADDGRUPNY.P 
         (INPUT Guru.Konstanter:globforetag, INPUT konstgrptemp.KONSKOD , INPUT konstgrptemp.ORDNING, INPUT skrivov, OUTPUT TABLE felmeddtemp).
      END.   
      FIND FIRST felmeddtemp  NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:      
         MESSAGE felmeddtemp.FELMEDD
         VIEW-AS ALERT-BOX.
      END.                                                                    
   END.
    
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE  DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(tthandle) THEN DELETE OBJECT tthandle.
   IF VALID-HANDLE(mtrlbapph) THEN DELETE PROCEDURE mtrlbapph.
   RUN disable_UI.
END.
/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {WIN_M_START.I}
   {ALLSTARTDYN.I}
   {muswait.i}
   {&WINDOW-NAME}:TITLE = valvar.
   IF valvar = "Ladda konstruktionsgrupp" THEN DO:
      RUN nykonsgrp_UI IN mtrlbapph ( OUTPUT nykonsgrp, OUTPUT nyord).
      CREATE konstgrptemp.
      ASSIGN
      /*konstgrptemp.KONSKOD = 777*/
      konstgrptemp.KONSKOD = nykonsgrp
      konstgrptemp.BENAMNING = "Ny konst.grupp"
      konstgrptemp.ORDNING = nyord.
      /*konstgrptemp.ORDNING = 777.*/
      
      
      IF Guru.Konstanter:globforetag = "cGRAN" THEN DO:
         nyord = nyord + 1.
         CREATE konstgrptemp.
         ASSIGN      
         konstgrptemp.KONSKOD = 60
         konstgrptemp.BENAMNING = "Ny konst.grupp"
         konstgrptemp.ORDNING = nyord.
         nyord = nyord + 1.
         CREATE konstgrptemp.
         ASSIGN      
         konstgrptemp.KONSKOD = 61
         konstgrptemp.BENAMNING = "Ny konst.grupp"
         konstgrptemp.ORDNING = nyord.
         nyord = nyord + 1.
         CREATE konstgrptemp.
         ASSIGN      
         konstgrptemp.KONSKOD = 62
         konstgrptemp.BENAMNING = "Ny konst.grupp"
         konstgrptemp.ORDNING = nyord.
         nyord = nyord + 1.
         CREATE konstgrptemp.
         ASSIGN      
         konstgrptemp.KONSKOD = 63
         konstgrptemp.BENAMNING = "Ny konst.grupp"
         konstgrptemp.ORDNING = nyord.
         nyord = nyord + 1.
         CREATE konstgrptemp.
         ASSIGN      
         konstgrptemp.KONSKOD = 64
         konstgrptemp.BENAMNING = "Ny konst.grupp"
         konstgrptemp.ORDNING = nyord.
         nyord = nyord + 1.
         
         
      END.
      IF Guru.Konstanter:globforetag = "POFO" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         nyord = nyord + 1.
         CREATE konstgrptemp.
         ASSIGN      
         konstgrptemp.KONSKOD = 33
         konstgrptemp.BENAMNING = "Ny konst.grupp"
         konstgrptemp.ORDNING = nyord.     
      END.
      IF Guru.Konstanter:globforetag = "POWE"  THEN DO:
         nyord = nyord + 1.
         CREATE konstgrptemp.
         ASSIGN      
         konstgrptemp.KONSKOD = 15
         konstgrptemp.BENAMNING = "Ny konst.grupp"
         konstgrptemp.ORDNING = nyord.     
      END.
   END.
   RUN enable_UI.   
   {FRMSIZE.I}  
   RUN openbdyn_UI IN brwproc[1] (INPUT "").


   /*OPEN QUERY BRW_GRUPP FOR EACH KONSTGRUPP NO-LOCK 
   USE-INDEX ORD INDEXED-REPOSITION.    */
   {musarrow.i}            
   {WIN_M_SLUT.I}
   APPLY "VALUE-CHANGED" TO BRW_GRUPP.
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:      
  Parameters:  
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_GRUPP:HANDLE IN FRAME {&FRAME-NAME}).   
   tthandle = TEMP-TABLE konstgrptemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KONSTGRUPP", INPUT "").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KONSTGRUPP", INPUT "").
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.            
   END.
   ELSE DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph.       
   END.   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-1  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
  THEN DELETE WIDGET WINDOW-1.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-1  _DEFAULT-ENABLE
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
  ENABLE BRW_GRUPP BTN_OK BTN_AVS 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo_UI WINDOW-1 
PROCEDURE repo_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER browrec AS RECID NO-UNDO.
   {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
   REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

