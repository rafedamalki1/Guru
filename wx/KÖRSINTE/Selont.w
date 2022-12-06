&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
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

  Created: 05/13/96 -  1:55 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE ifil AS CHARACTER NO-UNDO. 
DEFINE VARIABLE excellista AS INTEGER NO-UNDO.
{TIDUTTTNEW.I}
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_UT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidut

/* Definitions for BROWSE BRW_UT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_UT tidut.ut 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UT 
&Scoped-define QUERY-STRING-BRW_UT FOR EACH tidut NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UT OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UT tidut
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT tidut


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_SKRIV BTN_EXCEL BTN_AVS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXCEL 
     LABEL "Visa i Excel":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_UT FOR 
      tidut SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UT DIALOG-1 _STRUCTURED
  QUERY BRW_UT NO-LOCK DISPLAY
      tidut.ut FORMAT "X(132)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 81.5 BY 15.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BRW_UT AT ROW 1.5 COL 1.5
     BTN_SKRIV AT ROW 17.25 COL 38.5
     BTN_EXCEL AT ROW 17.25 COL 53.5
     BTN_AVS AT ROW 17.25 COL 69
     SPACE(0.99) SKIP(0.16)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Senaste l�ne�verf�ring tj�nstem�n".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Temp-Tables and Buffers:
      TABLE: tidut T "?" NO-UNDO temp-db tidut
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_UT 1 DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_UT IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "Temp-Tables.tidut"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.tidut.ut
     _Query            is NOT OPENED
*/  /* BROWSE BRW_UT */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-1
ON CHOOSE OF BTN_AVS IN FRAME DIALOG-1 /* Avsluta */
DO:
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_EXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXCEL DIALOG-1
ON CHOOSE OF BTN_EXCEL IN FRAME DIALOG-1 /* Visa i Excel */
DO: 
   excellista = 0.
   IF Guru.Konstanter:globforetag = "cSUND" OR Guru.Konstanter:globforetag = "elpa"  THEN DO:
      IF Guru.Konstanter:globforetag = "cSUND" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
         MESSAGE "Vill du att varje person skall ha ett eget blad?" 
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val2 AS LOGICAL.        
      END.
      IF val2 = TRUE THEN DO:
         RUN EXLONFIL2.P (INPUT excellista , INPUT Guru.Konstanter:globforetag, INPUT TABLE tidut, INPUT TRUE).        
      END.
      ELSE DO:
         RUN EXLONFIL2.P (INPUT excellista , INPUT Guru.Konstanter:globforetag, INPUT TABLE tidut, INPUT FALSE).        
      END.
   END.
   IF Guru.Konstanter:globforetag = "Celpa"  THEN DO:
      RUN EXLONFIL3.P (INPUT excellista , INPUT Guru.Konstanter:globforetag, INPUT TABLE tidut, INPUT FALSE).             
   END.
   
   {musarrow.i}    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXCEL DIALOG-1
ON MOUSE-MENU-CLICK OF BTN_EXCEL IN FRAME DIALOG-1 /* Visa i Excel */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV DIALOG-1
ON CHOOSE OF BTN_SKRIV IN FRAME DIALOG-1 /* Skriv ut */
DO:
   RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN musz = FALSE.
   ELSE RUN EKLOGS.P.
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV DIALOG-1
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME DIALOG-1 /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_UT
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
   FRAME {&FRAME-NAME}:TITLE = "Senaste till�ggsfil ".
   EMPTY TEMP-TABLE tidut NO-ERROR.    
   RUN huvud_UI.
   FIND FIRST tidut NO-LOCK NO-ERROR. 
   /*FIND NEXT tidut NO-LOCK NO-ERROR.*/
   IF AVAILABLE tidut THEN DO:    
      ENABLE BRW_UT WITH FRAME {&FRAME-NAME}.
      BRW_UT:HIDDEN = FALSE.
      OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK. 
   END.
   ELSE DO:
      /*status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").*/
      status-mus2 = SESSION:SET-WAIT-STATE("").
      LEAVE MAIN-BLOCK.                 
   END.
   RUN enable_UI.       
   {FRMSIZED.I}  
   {musarrow.i}    
   {DIA_M_SLUT.I}
   ASSIGN
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   IF BTN_EXCEL:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = BTN_EXCEL:HANDLE.      
      RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   ASSIGN
   Guru.GlobalaVariabler:colrighth = BTN_SKRIV:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-1 
PROCEDURE allstartbrw_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_UT:HANDLE IN FRAME {&FRAME-NAME}).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE appfel_UI DIALOG-1 
PROCEDURE appfel_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {APCONFEL.I}
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
  ENABLE BTN_SKRIV BTN_EXCEL BTN_AVS 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI DIALOG-1 
PROCEDURE huvud_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   /*HUVUD*/                    
   /*IF Guru.Konstanter:globforetag = "NORD" OR Guru.Konstanter:globforetag = "ETA" OR Guru.Konstanter:globforetag = "ESAN"
   OR Guru.Konstanter:globforetag = "ESMA" THEN DO:
      IF NOT Guru.Konstanter:appcon THEN DO:
         RUN appfel_UI.
         RETURN.         
      END.
      RUN SELONIN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "pa90.regold", OUTPUT TABLE tidut).            
      RETURN.
   END.        
   ELSE IF  Guru.Konstanter:globforetag = "ELPA" THEN DO:                      
      IF NOT Guru.Konstanter:appcon THEN DO:
         RUN appfel_UI.
         RETURN.
      END.
      ELSE DO:
         RUN SELONIN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "stanstill.d", OUTPUT TABLE tidut).              
         RETURN.
      END.   
   END.*/
   
   IF Guru.Konstanter:globforetag = "LULE" THEN DO:
      ifil = "D:\elpool\DELAD\PRO9s\EXPORT\LON\Lonback\LULELEAB.d".
   END.      
   ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:      
      ifil = "\\goliat\DELAD\server\PRO9S\gkal\lonutGSEAB.d".      
   END. 
   /*IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
      ifil = "\\GRANGURU\guru_ser\server\PRO9S\gran\lotjan.d".
   END. 
   ELSE IF Guru.Konstanter:globforetag = "GADM" THEN DO:
      ifil = "\\GRANGURU\guru_ser\server\PRO9S\gadm\lotjan.d".
   END. 
   ELSE IF Guru.Konstanter:globforetag = "GSYD" THEN DO:
      ifil = "\\GRANGURU\guru_ser\server\PRO9S\gsyd\lotjan.d".
   END.     
   ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
      ifil = "\\GRANGURU\guru_ser\server\PRO9S\grit\lotjan.d".
   END.     
   ELSE IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:
      ifil = "\\GRANGURU\guru_ser\server\PRO9S\gkrva\lotjan.d".
   END.    
   ELSE DO:
      ifil = "C:\GURU\pakop02.d".
   END.     */
   IF NOT Guru.Konstanter:appcon THEN DO:      
      RUN appfel_UI.
      RETURN.
   END.       
   RUN SELONIN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT ifil, OUTPUT TABLE tidut).            
   /*REPEAT TRANSACTION:
      CREATE tidut.
      ASSIGN.
      IMPORT UNFORMATTED tidut.     
   END.
   FOR EACH tidut:
      IF tidut.UT = "" THEN DELETE tidut.
   END.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

