&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-2



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/09/15 -  2:57 pm

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

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
&Scoped-define NEW 
DEFINE SHARED VARIABLE vallista AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE alla AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE alla2 AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE forvar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE uttyp AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE period AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valfore AS LOGICAL NO-UNDO.
DEFINE VARIABLE antalomr AS INTEGER NO-UNDO.
DEFINE VARIABLE timmar100 AS DECIMAL NO-UNDO.
DEFINE VARIABLE timmar60 AS DECIMAL NO-UNDO.
DEFINE VARIABLE distvar AS INTEGER NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE anmtyp AS LOGICAL NO-UNDO.

{ANMARKD.I}

&SCOPED-DEFINE NEW
&SCOPED-DEFINE SHARED SHARED 
{STORTEMP.I}
{AVDELNINGTEMP.I}
{STRTEMP.I}
DEFINE TEMP-TABLE extravstorntemp NO-UNDO LIKE urstorntemp.
DEFINE TEMP-TABLE extrabortkoppladtemp NO-UNDO LIKE bortkoppladtemp.

{TIDUTTTNEW.I}
   
DEFINE SHARED TEMP-TABLE omr_temp
   FIELD AVDELNINGNR AS INTEGER
   FIELD DISTRIKTID AS INTEGER
   FIELD NAMN AS CHARACTER
   INDEX OMR IS PRIMARY AVDELNINGNR DISTRIKTID.

DEFINE SHARED TEMP-TABLE avd_temp
   FIELD AVDELNINGNR AS INTEGER
   FIELD NAMN AS CHARACTER
   INDEX AVD IS PRIMARY AVDELNINGNR.
   
DEFINE SHARED TEMP-TABLE spann_temp2    
   FIELD SPANID AS INTEGER
   FIELD NAMN AS CHARACTER
   INDEX SPAN SPANID.   

DEFINE TEMP-TABLE stor_temp
   FIELD AVDELNING AS CHARACTER
   FIELD DISTRIKT AS CHARACTER
   FIELD STORNUMMERID AS INTEGER
   FIELD HDATUM AS DATE
   FIELD HKLOCKAN AS DECIMAL
   FIELD ANVANDARE AS CHARACTER
   FIELD ANSVARIGPERS AS CHARACTER
   FIELD TYP AS CHARACTER
   INDEX DATUM HDATUM HKLOCKAN ASCENDING.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-TIDS
&Scoped-define BROWSE-NAME BRW_UT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidut

/* Definitions for BROWSE BRW_UT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_UT tidut.ut 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UT 
&Scoped-define QUERY-STRING-BRW_UT FOR EACH tidut NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_UT OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_UT tidut
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT tidut


/* Definitions for FRAME FRAME-TIDS                                     */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_SKRIV BTN_AVS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD klockan100 WINDOW-2 
FUNCTION klockan100 RETURNS DECIMAL
  (INPUT ber60 AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD klockan60 WINDOW-2 
FUNCTION klockan60 RETURNS DECIMAL
  (INPUT ber100 AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.08.

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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UT WINDOW-2 _STRUCTURED
  QUERY BRW_UT NO-LOCK DISPLAY
      tidut.ut FORMAT "X(132)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 123.5 BY 25.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-TIDS
     BRW_UT AT ROW 1.5 COL 1.5
     BTN_SKRIV AT ROW 27.75 COL 96
     BTN_AVS AT ROW 27.75 COL 111
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db tidut
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Lista ?terst?ende arbete"
         HEIGHT             = 28.42
         WIDTH              = 125
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 125
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
/* SETTINGS FOR WINDOW WINDOW-2
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-TIDS
                                                                        */
/* BROWSE-TAB BRW_UT 1 FRAME-TIDS */
/* SETTINGS FOR BROWSE BRW_UT IN FRAME FRAME-TIDS
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "temp-db.tidut"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = temp-db.tidut.ut
     _Query            is NOT OPENED
*/  /* BROWSE BRW_UT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-TIDS
/* Query rebuild information for FRAME FRAME-TIDS
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-TIDS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-2
ON CHOOSE OF BTN_AVS IN FRAME FRAME-TIDS /* Avsluta */
DO: 
   {BORTBRWPROC.I}
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-TIDS /* Skriv ut */
DO:
   
   RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN musz = FALSE.
   ELSE RUN EKLOGS.P.        
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME FRAME-TIDS /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_UT
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   {BORTBRWPROC.I}
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
   {muswait.i}   
   {ALLSTARTDYN.I} 
   EMPTY TEMP-TABLE tidut NO-ERROR. 
      
str="====================================================================================================================================".

   RUN huvud_UI.   
   RUN fore_UI.
   FIND FIRST tidut  NO-LOCK NO-ERROR. 
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
   {FRMSIZE.I}    
   ASSIGN
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_SKRIV:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   {musarrow.i}   
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-2 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_UT:HANDLE IN FRAME {&FRAME-NAME}).
  
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anmark_UI WINDOW-2 
PROCEDURE anmark_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER anmark AS INTEGER NO-UNDO.
   IF anmark = 2 THEN DO: 
      IF anmtyp = FALSE THEN DO:
         ASSIGN            
         SUBSTRING(tidut.UT,23) = ":"   
         SUBSTRING(tidut.UT,24) = tidtext.
         CREATE tidut.
      END.
      ELSE DO:
         ASSIGN                     
         SUBSTRING(tidut.UT,1) = tidtext.
         CREATE tidut.
      END.
   END.                              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE det_UI WINDOW-2 
PROCEDURE det_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   FOR EACH stor_temp:
      EMPTY TEMP-TABLE extravstorntemp NO-ERROR. 
      EMPTY TEMP-TABLE extrabortkoppladtemp NO-ERROR.       
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN ATERARBAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT stor_temp.STORNUMMERID, OUTPUT TABLE extravstorntemp, OUTPUT TABLE extrabortkoppladtemp).      
      END.
      ELSE DO:
         RUN ATERARBAPP.P 
         (INPUT stor_temp.STORNUMMERID, OUTPUT TABLE extravstorntemp, OUTPUT TABLE extrabortkoppladtemp).
      END.      
      RUN visa_UI.      
      SUBSTRING(tidut.UT,132) = "$".
   END.                   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-2  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
  THEN DELETE WIDGET WINDOW-2.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-2  _DEFAULT-ENABLE
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
  ENABLE BTN_SKRIV BTN_AVS 
      WITH FRAME FRAME-TIDS IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-TIDS}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fore_UI WINDOW-2 
PROCEDURE fore_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   ASSIGN   
   antalomr = 0.
   IF valfore = TRUE THEN DO:
      RUN storning_UI.
      IF vallista = 1 THEN
      RUN totalt_UI.
      ELSE RUN det_UI.
   END.
   ELSE DO:
      IF alla = TRUE THEN DO:
         RUN storning_UI.
         IF vallista = 1 THEN
         RUN totalt_UI.
         ELSE RUN det_UI.
      END.
      ELSE DO:
         IF alla2 = TRUE THEN DO:
            OPEN QUERY kq FOR EACH stordistemp WHERE stordistemp.AVDELNINGNR = forvar
            AND stordistemp.ARTAL = YEAR(bdatum) USE-INDEX AVDARTAL NO-LOCK.
            GET FIRST kq NO-LOCK.
            DO WHILE AVAILABLE(stordistemp):
               ASSIGN
               distvar = stordistemp.DISTRIKTID
               antalomr = antalomr + 1.
               IF vallista = 1 THEN DO:
                  CREATE tidut. 
                  CREATE tidut.
                  ASSIGN           
                  SUBSTRING(tidut.UT,1) = "Distrikt:" + stordistemp.NAMN.           
               END.
               RUN storning_UI.            
               IF vallista = 1 THEN RUN utlista_UI.
               ELSE RUN det_UI.
               GET NEXT kq NO-LOCK.
            END.
            CLOSE QUERY kq.           
         END.
         ELSE DO:
            FOR EACH omr_temp USE-INDEX OMR NO-LOCK:
               ASSIGN
               distvar = omr_temp.DISTRIKTID
               antalomr = antalomr + 1.                       
               IF vallista = 1 THEN DO:
                  CREATE tidut. 
                  CREATE tidut.
                  ASSIGN            
                  SUBSTRING(tidut.UT,1) = "Distrikt:" + omr_temp.NAMN.                           
               END.
               RUN storning_UI.               
               IF vallista = 1 THEN RUN utlista_UI.
               ELSE RUN det_UI.
            END.           
         END.
      END.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI WINDOW-2 
PROCEDURE huvud_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
     /*HUVUD*/       
   DO TRANSACTION:              
      CREATE tidut.               
      IF vallista = 1 THEN
      SUBSTRING(tidut.UT,1) = "Lista ?terst?ende arbete".      
      ELSE
      SUBSTRING(tidut.UT,1) = "Detaljerad lista ?terst?ende arbete".      
      SUBSTRING(tidut.UT,60) = STRING(TODAY). 
      CREATE tidut.
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = "Urval:".
      CREATE tidut.
      IF uttyp = 1 THEN
      SUBSTRING(tidut.UT,1) = "Driftst?rningar".
      ELSE IF uttyp = 2 THEN
      SUBSTRING(tidut.UT,1) = "Planerade avbrott".
      ELSE
      SUBSTRING(tidut.UT,1) = "Driftst?rningar och planerade avbrott".
      CREATE tidut.
      IF period = 1 THEN
      SUBSTRING(tidut.UT,1) = "Period:" + STRING(YEAR(bdatum)).
      ELSE
      SUBSTRING(tidut.UT,1) = "Period:" + STRING(bdatum,"9999/99/99") + " - " + STRING(avdatum,"9999/99/99").
      CREATE tidut.
      IF valfore = TRUE THEN DO:
         SUBSTRING(tidut.UT,1) = "Valda f?retag:".
         CREATE tidut.         
         FOR EACH avd_temp USE-INDEX AVD:
            SUBSTRING(tidut.UT,10) = avd_temp.NAMN.
            CREATE tidut.
         END.
      END.
      ELSE DO:
         IF alla = TRUE THEN DO:
            SUBSTRING(tidut.UT,1) = "Alla f?retag".
         END.
         ELSE DO:
            FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNR = forvar NO-LOCK NO-ERROR.
            IF alla2 = TRUE THEN DO:               
               SUBSTRING(tidut.UT,1) = "Valt f?retag:" + avdelningtemp.AVDELNINGNAMN.
            END.
            ELSE DO:               
               SUBSTRING(tidut.UT,1) = "Valt f?retag:" + avdelningtemp.AVDELNINGNAMN.
               CREATE tidut.
               SUBSTRING(tidut.UT,1) = "Distrikt:".
               FOR EACH omr_temp USE-INDEX OMR:
                  SUBSTRING(tidut.UT,10) = omr_temp.NAMN.
                  CREATE tidut.
               END.
            END.
         END.
      END.
   END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rubrik2_UI WINDOW-2 
PROCEDURE rubrik2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   CREATE tidut.   
   ASSIGN
   SUBSTRING(tidut.UT,1) = "Datum"
   SUBSTRING(tidut.UT,12) = "Klockan"
   SUBSTRING(tidut.UT,20) = "Uppr?ttad av"
   SUBSTRING(tidut.UT,36) = "Ansvarig"
   SUBSTRING(tidut.UT,53) = "Typ".               
   CREATE tidut.
   SUBSTRING(tidut.UT,1) = "==========.=======.===============.================.=====".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rubrik_UI WINDOW-2 
PROCEDURE rubrik_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   CREATE tidut.   
   ASSIGN
   SUBSTRING(tidut.UT,1) = "F?retag"
   SUBSTRING(tidut.UT,17) = "Distrikt"
   SUBSTRING(tidut.UT,33) = "Datum"
   SUBSTRING(tidut.UT,44) = "Klockan"
   SUBSTRING(tidut.UT,52) = "Uppr?ttad av"
   SUBSTRING(tidut.UT,65) = "Ansvarig"
   SUBSTRING(tidut.UT,77) = "Typ".               
   CREATE tidut.
   SUBSTRING(tidut.UT,1) = "===============.===============.==========.=======.============.===========.=====".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE storning_UI WINDOW-2 
PROCEDURE storning_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   EMPTY TEMP-TABLE stor_temp NO-ERROR.    
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN ATERSUM.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, INPUT bdatum, INPUT avdatum, INPUT period, 
      INPUT uttyp, INPUT vallista, INPUT alla, OUTPUT TABLE stor_temp).      
   END.
   ELSE DO:
      RUN ATERSUM.P 
      (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, INPUT bdatum, INPUT avdatum, INPUT period, 
      INPUT uttyp, INPUT vallista, INPUT alla, OUTPUT TABLE stor_temp).
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE totalt_UI WINDOW-2 
PROCEDURE totalt_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   CREATE tidut.
   RUN rubrik_UI.
   FOR EACH stor_temp:
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = SUBSTRING(stor_temp.AVDELNING,1,15)
      SUBSTRING(tidut.UT,17) = SUBSTRING(stor_temp.DISTRIKT,1,15)
      SUBSTRING(tidut.UT,33) = STRING(stor_temp.HDATUM,"9999/99/99")
      SUBSTRING(tidut.UT,44) = STRING(stor_temp.HKLOCKAN,"99.99")
      SUBSTRING(tidut.UT,52) = SUBSTRING(stor_temp.ANVANDARE,1,15)
      SUBSTRING(tidut.UT,65) = SUBSTRING(stor_temp.ANSVARIGPERS,1,15)
      SUBSTRING(tidut.UT,77) = stor_temp.TYP.
   END.                   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE utlista_UI WINDOW-2 
PROCEDURE utlista_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   CREATE tidut.
   RUN rubrik2_UI.
   FOR EACH stor_temp:
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = STRING(stor_temp.HDATUM,"9999/99/99")
      SUBSTRING(tidut.UT,12) = STRING(stor_temp.HKLOCKAN,"99.99")
      SUBSTRING(tidut.UT,20) = SUBSTRING(stor_temp.ANVANDARE,1,15)
      SUBSTRING(tidut.UT,36) = SUBSTRING(stor_temp.ANSVARIGPERS,1,15)
      SUBSTRING(tidut.UT,53) = stor_temp.TYP.
   END.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI WINDOW-2 
PROCEDURE visa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DO TRANSACTION: 
      FIND FIRST extravstorntemp NO-LOCK NO-ERROR.
      anmtyp = FALSE.
      CREATE tidut.
      IF extravstorntemp.STORTYPID = 1 THEN SUBSTRING(tidut.UT,35) = "DRIFTST?RNING".
      ELSE SUBSTRING(tidut.UT,35) = "PLANERAT AVBROTT".
      CREATE tidut.
      FIND FIRST stordistemp WHERE stordistemp.DISTRIKTID = extravstorntemp.DISTRIKTID
      USE-INDEX DISTRIKTID NO-LOCK NO-ERROR.
      FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNR = stordistemp.AVDELNINGNR NO-LOCK NO-ERROR.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "F?retag:" + SUBSTRING(avdelningtemp.AVDELNINGNAMN,1,30) 
      SUBSTRING(tidut.UT,50) = "Distrikt:" + stordistemp.NAMN.      
      CREATE tidut.
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = str.
      CREATE tidut.
      SUBSTRING(tidut.UT,35) = "KUNDUPPGIFTER".
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = str.
      CREATE tidut.      
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Felanm?lare:" + SUBSTRING(extravstorntemp.VEMFEL,1,30).
      SUBSTRING(tidut.UT,50) = "Fel uppgivet:" + extravstorntemp.LOSEN.
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = "Feladress:" + extravstorntemp.ADRESSVEMFEL.
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Postnr:" + extravstorntemp.VEMPOSTNUMMER 
      SUBSTRING(tidut.UT,50) = "Ort:" + extravstorntemp.VEMORT.
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = "Telefonnummer:" + extravstorntemp.TELVEMFEL.
      CREATE tidut.
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = str.
      CREATE tidut.
      SUBSTRING(tidut.UT,35) = "?VRIGA UPPGIFTER".
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = str.      
      CREATE tidut.
      FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = extravstorntemp.ANSVARIGPERS
      NO-LOCK NO-ERROR.
      IF AVAILABLE anvandartemp THEN
      SUBSTRING(tidut.UT,1) = "?verl?mnat till:" + anvandartemp.AV-NAMN.
      ELSE SUBSTRING(tidut.UT,1) = "?verl?mnat till:" + extravstorntemp.ANSVARIGPERS.
      SUBSTRING(tidut.UT,50) = "?terst?ende arbete:" + STRING(extravstorntemp.MERJOBB,"Ja/Nej").
      CREATE tidut.
      IF extravstorntemp.MERJOBB = TRUE THEN
      SUBSTRING(tidut.UT,1) = "F?rdigdatum:".
      ELSE SUBSTRING(tidut.UT,1) = "F?rdigdatum:" + STRING(extravstorntemp.FDATUM,"9999/99/99").
      FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = extravstorntemp.ANVANDARE
      NO-LOCK NO-ERROR.
      IF AVAILABLE anvandartemp THEN
      SUBSTRING(tidut.UT,50) = "Uppr?ttad av:" + anvandartemp.AV-NAMN.
      ELSE SUBSTRING(tidut.UT,50) = "Uppr?ttad av:" + extravstorntemp.ANVANDARE.
      CREATE tidut.      
      IF LENGTH(extravstorntemp.VEMINFO) > 0 THEN DO:
         CREATE tidut.
         ASSIGN         
         SUBSTRING(tidut.UT,1) = "Anm?rkning"
         retvar = 1
         ednum = 1
         ednum3 = LENGTH(extravstorntemp.VEMINFO)
         retvar = INDEX(extravstorntemp.VEMINFO,CHR(10),ednum)
         edtecken = 42
         edtext = extravstorntemp.VEMINFO
         tidtext = "".
         {ANMARK2.I}
      END.
      CREATE tidut.
      CREATE tidut.
      /*SUBSTRING(tidut.UT,1) = str.
      CREATE tidut.
      SUBSTRING(tidut.UT,35) = "ST?RNINGS UPPGIFTER".
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = str.
      CREATE tidut.      
      FIND FIRST spanningsnivtemp WHERE spanningsnivtemp.SPANID = extravstorntemp.FRANSPANID
      USE-INDEX SPANID NO-LOCK NO-ERROR.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Fr?nkopplingsst?lle" 
      SUBSTRING(tidut.UT,23) = ":"
      SUBSTRING(tidut.UT,24) = CAPS(SUBSTRING(spanningsnivtemp.NAMN,1,1)) + LC(SUBSTRING(spanningsnivtemp.NAMN,2)).      
      IF extravstorntemp.STORTYPID = 1 THEN DO:
         CREATE tidut.
         CREATE tidut.
         FIND FIRST spanningsnivtemp WHERE spanningsnivtemp.SPANID = extravstorntemp.FELSPANID
         USE-INDEX SPANID NO-LOCK NO-ERROR.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Felst?lle" 
         SUBSTRING(tidut.UT,23) = ":"
         SUBSTRING(tidut.UT,24) = CAPS(SUBSTRING(spanningsnivtemp.NAMN,1,1)) + LC(SUBSTRING(spanningsnivtemp.NAMN,2)).
      END.
      CREATE tidut.
      CREATE tidut.
      FIND FIRST brytorgantemp WHERE brytorgantemp.BRYTOID = extravstorntemp.BRYTOID 
      USE-INDEX BRYTOID NO-LOCK NO-ERROR.      
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Fr?nkopplingsorgan" 
      SUBSTRING(tidut.UT,23) = ":"
      SUBSTRING(tidut.UT,24) = CAPS(SUBSTRING(brytorgantemp.NAMN,1,1)) + LC(SUBSTRING(brytorgantemp.NAMN,2))
      SUBSTRING(tidut.UT,60) = "Littera :" +  extravstorntemp.BRYTORGLIT.
      CREATE tidut.
      CREATE tidut.
      IF extravstorntemp.STORTYPID = 1 THEN DO:
         FIND FIRST sektioneringtemp WHERE sektioneringtemp.SEKTIONERID = extravstorntemp.SEKTIONERID 
         USE-INDEX SEKTIONERID NO-LOCK NO-ERROR.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Sektionering" 
         SUBSTRING(tidut.UT,23) = ":"
         SUBSTRING(tidut.UT,24) = CAPS(SUBSTRING(sektioneringtemp.NAMN,1,1)) + LC(SUBSTRING(sektioneringtemp.NAMN,2))
         SUBSTRING(tidut.UT,60) = "Littera :" + extravstorntemp.SEKTIONLIT.
         CREATE tidut.
         CREATE tidut.         
         FIND FIRST utlosningskyddtemp WHERE utlosningskyddtemp.UTLOSID = extravstorntemp.UTLOSID
         USE-INDEX UTLOSID NO-LOCK NO-ERROR.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Utl?sning" 
         SUBSTRING(tidut.UT,23) = ":"
         SUBSTRING(tidut.UT,24) = CAPS(SUBSTRING(utlosningskyddtemp.NAMN,1,1)) + LC(SUBSTRING(utlosningskyddtemp.NAMN,2)).
         CREATE tidut.
         CREATE tidut.                
         FIND FIRST relaindikeringtemp WHERE relaindikeringtemp.RELINID = extravstorntemp.RELINID
         USE-INDEX RELINID NO-LOCK NO-ERROR.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Rel?indekering" 
         SUBSTRING(tidut.UT,23) = ":"
         SUBSTRING(tidut.UT,24) = CAPS(SUBSTRING(relaindikeringtemp.NAMN,1,1)) + LC(SUBSTRING(relaindikeringtemp.NAMN,2)).
      END.
      CREATE tidut.
      CREATE tidut.
      FIND FIRST stordriftomrtemp WHERE stordriftomrtemp.STDRIFTID = extravstorntemp.STDRIFTID 
      USE-INDEX STDRIFTID NO-LOCK NO-ERROR.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Bel?genhet" 
      SUBSTRING(tidut.UT,23) = ":"
      SUBSTRING(tidut.UT,24) = CAPS(SUBSTRING(stordriftomrtemp.NAMN,1,1)) + LC(SUBSTRING(stordriftomrtemp.NAMN,2))
      SUBSTRING(tidut.UT,60) = "Littera :" + extravstorntemp.STDRIFTLIT.
      CREATE tidut.
      CREATE tidut.   
      FIND FIRST nattyptemp WHERE nattyptemp.NATTYPID = extravstorntemp.NATTYPID 
      USE-INDEX NATTYPID NO-LOCK NO-ERROR.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "N?ttyp" 
      SUBSTRING(tidut.UT,23) = ":"
      SUBSTRING(tidut.UT,24) = CAPS(SUBSTRING(nattyptemp.NAMN,1,1)) + LC(SUBSTRING(nattyptemp.NAMN,2)).
      CREATE tidut.
      CREATE tidut.   
      FIND FIRST anlaggningsdeltemp WHERE anlaggningsdeltemp.ADELID = extravstorntemp.ADELID
      USE-INDEX ADELID NO-LOCK NO-ERROR.
      FIND FIRST anlaggningstyptemp WHERE anlaggningstyptemp.ATYPID = 
      anlaggningsdeltemp.ATYPID USE-INDEX ATYPID NO-LOCK NO-ERROR.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Anl?ggning, -del" 
      SUBSTRING(tidut.UT,23) = ":"
      SUBSTRING(tidut.UT,24) = CAPS(SUBSTRING(anlaggningstyptemp.NAMN,1,1)) + LC(SUBSTRING(anlaggningstyptemp.NAMN,2)) + 
      " - " + CAPS(SUBSTRING(anlaggningsdeltemp.NAMN,1,1)) + LC(SUBSTRING(anlaggningsdeltemp.NAMN,2)).
      IF extravstorntemp.STORTYPID = 1 THEN DO:
         CREATE tidut.
         CREATE tidut.
         FIND FIRST felorsaktemp WHERE felorsaktemp.FELOID = extravstorntemp.FELOID
         USE-INDEX FELOID NO-LOCK NO-ERROR.           
         FIND FIRST grundfeltyptemp WHERE grundfeltyptemp.GRUNDFELID = 
         felorsaktemp.GRUNDFELID USE-INDEX GRUNDFELID NO-LOCK NO-ERROR. 
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Felorsak" 
         SUBSTRING(tidut.UT,23) = ":"
         SUBSTRING(tidut.UT,24) = CAPS(SUBSTRING(grundfeltyptemp.NAMN,1,1)) + LC(SUBSTRING(grundfeltyptemp.NAMN,2)) + 
         " - " + CAPS(SUBSTRING(felorsaktemp.NAMN,1,1)) + LC(SUBSTRING(felorsaktemp.NAMN,2)).
      END.
      CREATE tidut.
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Bortkopplad belastning"
      SUBSTRING(tidut.UT,23) = ":"
      SUBSTRING(tidut.UT,24) = STRING(extravstorntemp.BORTMW) +
      " MW " + STRING(extravstorntemp.BORTKW) + " kW".
      CREATE tidut.
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Startdatum"
      SUBSTRING(tidut.UT,23) = ":"
      SUBSTRING(tidut.UT,24) = STRING(extravstorntemp.HDATUM,"9999/99/99").
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Klockan" 
      SUBSTRING(tidut.UT,23) = ":"
      SUBSTRING(tidut.UT,24) = STRING(extravstorntemp.HKLOCKAN,"99.99").
      FIND LAST extrabortkoppladtemp WHERE extrabortkoppladtemp.STORNUMMERID = extravstorntemp.STORNUMMERID
      USE-INDEX AVBROTT NO-LOCK NO-ERROR.
      IF AVAILABLE extrabortkoppladtemp THEN DO:         
         CREATE tidut.
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Slutdatum"
         SUBSTRING(tidut.UT,23) = ":"
         SUBSTRING(tidut.UT,24) = STRING(extrabortkoppladtemp.ATERDATUM,"9999/99/99").
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Klockan" 
         SUBSTRING(tidut.UT,23) = ":"
         SUBSTRING(tidut.UT,24) = STRING(extrabortkoppladtemp.ATERKLOCKAN,"99.99").
      END.
      ELSE DO:
         IF extravstorntemp.DATUM70% NE ? THEN DO:
            CREATE tidut.
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,1) = "Huvuddel tillkopplad"
            SUBSTRING(tidut.UT,23) = ":"
            SUBSTRING(tidut.UT,24) = STRING(extravstorntemp.DATUM70%,"9999/99/99").
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,1) = "Klockan" 
            SUBSTRING(tidut.UT,23) = ":"
            SUBSTRING(tidut.UT,24) = STRING(extravstorntemp.KLOCKAN70%,"99.99").
         END.
         CREATE tidut.
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Slutdatum"
         SUBSTRING(tidut.UT,23) = ":"
         SUBSTRING(tidut.UT,24) = STRING(extravstorntemp.DATUM100%,"9999/99/99").
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Klockan"
         SUBSTRING(tidut.UT,23) = ":"
         SUBSTRING(tidut.UT,24) = STRING(extravstorntemp.KLOCKAN100%,"99.99").
      END.
      CREATE tidut.
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Sp?nningsl?sa kunder" 
      SUBSTRING(tidut.UT,23) = ":"
      SUBSTRING(tidut.UT,24) = STRING(extravstorntemp.ANTALHSP) +
      " HSP " + STRING(extravstorntemp.ANTALLSP) + " LSP".
      CREATE tidut.
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Kundavbrottstid" 
      SUBSTRING(tidut.UT,23) = ":"
      SUBSTRING(tidut.UT,24) = STRING(klockan60(extravstorntemp.AVBROTTSTID),">>>>>9.99").
      CREATE tidut.
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Energibortfall kWh" 
      SUBSTRING(tidut.UT,23) = ":"
      SUBSTRING(tidut.UT,24) = STRING(klockan60(extravstorntemp.BORTFALL),">>>>>>>>>9.99").
      IF AVAILABLE extrabortkoppladtemp THEN DO:
         CREATE tidut.
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = str.
         CREATE tidut.
         SUBSTRING(tidut.UT,35) = "SPECIFIKATION".
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = str.         
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Datum"
         SUBSTRING(tidut.UT,13) = "Klockan"
         SUBSTRING(tidut.UT,26) = "HSP"
         SUBSTRING(tidut.UT,36) = "LSP"
         SUBSTRING(tidut.UT,44) = "Avb.tid"
         SUBSTRING(tidut.UT,55) = "Aktivitet".
         OPEN QUERY aq FOR EACH extrabortkoppladtemp WHERE 
         extrabortkoppladtemp.STORNUMMERID = extravstorntemp.STORNUMMERID USE-INDEX AVBROTT NO-LOCK.
         GET FIRST aq NO-LOCK.
         DO WHILE AVAILABLE(extrabortkoppladtemp):
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,1) = STRING(extrabortkoppladtemp.ATERDATUM,"9999/99/99") 
            SUBSTRING(tidut.UT,14) = STRING(extrabortkoppladtemp.ATERKLOCKAN,">99.99")
            SUBSTRING(tidut.UT,22) = STRING(extrabortkoppladtemp.ANTALHSP,">>>>>>9")
            SUBSTRING(tidut.UT,32) = STRING(extrabortkoppladtemp.ANTALLSP,">>>>>>9")
            SUBSTRING(tidut.UT,42) = STRING(klockan60(extrabortkoppladtemp.AVBROTTSTID),">>>>>9.99")
            SUBSTRING(tidut.UT,55) = SUBSTRING(extrabortkoppladtemp.AKTIVITET,1,25).
            GET NEXT aq NO-LOCK.
         END.
         CLOSE QUERY aq.
      END.
      IF LENGTH(extravstorntemp.KOMMENTAR) > 0 THEN DO:
         anmtyp = TRUE.
         CREATE tidut.
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = str.
         CREATE tidut.
         SUBSTRING(tidut.UT,35) = "ANM?RKNING".
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = str.
         CREATE tidut.
         ASSIGN
         retvar = 1
         ednum = 1
         ednum3 = LENGTH(extravstorntemp.KOMMENTAR)
         retvar = INDEX(extravstorntemp.KOMMENTAR,CHR(10),ednum)
         edtecken = 49
         edtext = extravstorntemp.KOMMENTAR
         tidtext = "".
         {ANMARK2.I}
      END.*/
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION klockan100 WINDOW-2 
FUNCTION klockan100 RETURNS DECIMAL
  (INPUT ber60 AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600. 
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION klockan60 WINDOW-2 
FUNCTION klockan60 RETURNS DECIMAL
  (INPUT ber100 AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) / 100) * 60.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

