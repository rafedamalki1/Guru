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
DEFINE SHARED VARIABLE distvar AS INTEGER FORMAT "->>>>>>9" NO-UNDO.
DEFINE SHARED VARIABLE stornrid AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE VARIABLE anmtyp AS LOGICAL NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE tabellhandle AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE spann_temp NO-UNDO
   FIELD SPANID AS INTEGER
   FIELD NAMN AS CHARACTER.

DEFINE INPUT PARAMETER excel AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER foretag AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER art AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR spann_temp. 
/*DEFINE INPUT PARAMETER dist AS INTEGER NO-UNDO.*/

{ANMARKD.I}

&SCOPED-DEFINE NEW 
&SCOPED-DEFINE SHARED SHARED
{AVDELNINGTEMP.I} 

{STRTEMP.I}

{STORTEMP.I}

DEFINE TEMP-TABLE extrastorntemp NO-UNDO LIKE urstorntemp.

{TIDUTTTNEW.I}
   /*FIELD UT AS CHARACTER FORMAT "X(132)".*/


DEFINE NEW SHARED TEMP-TABLE kundstortemp NO-UNDO
   FIELD ANTALBLAND AS INTEGER
   FIELD ANTALKABEL AS INTEGER
   FIELD ANTALKUNDER AS INTEGER
   FIELD ANTALLUFT AS INTEGER
   FIELD ARTAL AS INTEGER
   FIELD DISTRIKTID AS INTEGER FORMAT "->>>>>>9"
   FIELD INDATUM AS DATE FORMAT "99/99/99"
   FIELD SPANID AS INTEGER.

DEFINE NEW SHARED TEMP-TABLE ledningsdatatemp NO-UNDO
   FIELD LANGDLUFT AS DECIMAL
   FIELD LANGDKABEL AS DECIMAL
   FIELD LANGDBLAND AS DECIMAL
   FIELD LANGD AS DECIMAL
   FIELD DISTRIKTID AS INTEGER  FORMAT "->>>>>>9"
   FIELD ARTAL AS INTEGER FORMAT "9999"
   FIELD SPANID AS INTEGER.





/*  finns i execlin.i */
{EXECLIN.I}

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
&Scoped-define QUERY-STRING-BRW_UT FOR EACH tidut NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UT OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UT tidut
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT tidut


/* Definitions for FRAME FRAME-TIDS                                     */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_UT BTN_SKRIV BTN_EXCEL BTN_AVB 

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
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXCEL 
     LABEL "Till Excel" 
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UT WINDOW-2 _STRUCTURED
  QUERY BRW_UT NO-LOCK DISPLAY
      tidut.ut FORMAT "X(132)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 123.5 BY 26.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-TIDS
     BRW_UT AT ROW 1.5 COL 1.5
     BTN_SKRIV AT ROW 28 COL 81.25
     BTN_EXCEL AT ROW 28 COL 96.13
     BTN_AVB AT ROW 28 COL 111
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
         TITLE              = "Visa arbetsorder"
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "temp-db.tidut"
     _Options          = "NO-LOCK"
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

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-2
ON CHOOSE OF BTN_AVB IN FRAME FRAME-TIDS /* Avsluta */
DO:   
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_EXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXCEL WINDOW-2
ON CHOOSE OF BTN_EXCEL IN FRAME FRAME-TIDS /* Till Excel */
DO:   
   RUN excel_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-TIDS /* Skriv ut */
DO:
   RUN SKRIVVAL.W (INPUT TRUE).       
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
   {ALLSTARTDYN.I}
   /*
   FIND FIRST vstorntemp WHERE vstorntemp.STORNUMMERID = stornrid
          AND vstorntemp.DISTRIKTID = distvar NO-LOCK NO-ERROR.
          */
   ASSIGN {&WINDOW-NAME}:TITLE = "Kunddata".
   {muswait.i}    
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   
      
str="====================================================================================================================================".
str="------------------------------------------------------------------------------------------------------------------------------------".


   /*hämta kundstor */
   tabellhandle = TEMP-TABLE kundstortemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:     
      RUN GETTABLE.p ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT-OUTPUT TABLE-HANDLE tabellhandle, INPUT "KUNDSTOR").
   END.
   ELSE DO:
     RUN GETTABLE.p (INPUT-OUTPUT TABLE-HANDLE tabellhandle, INPUT "KUNDSTOR").
   END.
   /*hämta ledningsdata*/
   tabellhandle = TEMP-TABLE ledningsdatatemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:    
      RUN GETTABLE.p ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT-OUTPUT TABLE-HANDLE tabellhandle, INPUT "LEDNINGSDATA").
   END.
   ELSE DO:
     RUN GETTABLE.p (INPUT-OUTPUT TABLE-HANDLE tabellhandle, INPUT "LEDNINGSDATA").
   END.
     RUN huvud_UI.
   IF excel = TRUE THEN DO:
      RUN excel_UI.
      LEAVE.
   END.
   
   FIND FIRST tidut NO-LOCK NO-ERROR.    
   IF AVAILABLE tidut THEN DO:    
      ENABLE BRW_UT WITH FRAME {&FRAME-NAME}.
      BRW_UT:HIDDEN = FALSE.
      OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
   END.
   ELSE DO:
      status-mus2 = SESSION:SET-WAIT-STATE("").
      LEAVE MAIN-BLOCK.                 
   END.
   RUN enable_UI.   
   {FRMSIZE.I}    
   ASSIGN
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVB:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_EXCEL:HANDLE.      
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
  ENABLE BRW_UT BTN_SKRIV BTN_EXCEL BTN_AVB 
      WITH FRAME FRAME-TIDS IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-TIDS}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excel_UI WINDOW-2
PROCEDURE excel_UI :

   /*Vilka kolumner*/     
   ASSIGN
   bredd[1] = 40
   bredd[2] = 30
   bredd[3] = 25
   bredd[4] = 25
   bredd[5] = 25
   bredd[6] = 25
   bredd[7] = 25
   bredd[8] = 25
   bredd[9] = 25
   bredd[10] = 25
   bredd[11] = 25
   bredd[12] = 25
   bredd[13] = 25
   bredd[14] = 25
   bredd[15] = 25
   bredd[16] = 25
   bredd[17] = 25
   bredd[18] = 25
   bredd[19] = 25
   bredd[20] = 25
   bredd[21] = 25
   bredd[22] = 25
   bredd[23] = 25
   bredd[24] = 25
   bredd[25] = 25
   bredd[26] = 25
   bredd[27] = 25
   bredd[28] = 25
   slutbredd =  bredd[28].
   RUN colbredd_UI.  
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.                                   
      IF SUBSTRING(tidut.UT,estartnr[1],7) = "FÖRETAG" THEN DO:
         LEAVE.
      END.
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,2) = "==" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).     
      END.
      ELSE DO:
         /*IF tidut.UT NE "" THEN DO:*/
         IF SUBSTRING(tidut.UT,estartnr[1],7) = "FÖRETAG" THEN DO:
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 22,INPUT FALSE,INPUT TRUE,INPUT 0,INPUT 0).
         END.
         ELSE DO:
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).        
         END.                
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
   END.
   RUN slutexcel_UI.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI WINDOW-2 
PROCEDURE huvud_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
  FOR EACH AVDELNING WHERE AVDELNING.KOSTMASK = 1 AND AVDELNING.AVDELNINGNR = 59:
      DISP AVDELNING.AVDELNINGNAMN AVDELNING.AVDELNINGNR AVDELNING.KOSTMASK.
      FOR EACH STORDISTRIKT WHERE STORDISTRIKT.AVDELNINGNR = AVDELNING.AVDELNINGNR AND STORDISTRIK.ARTAL = 2003:
         /*DISP STORDISTRIKT.ARTAL STORDISTRIKT.AVDELNINGNR STORDISTRIKT.DISTRIKTID STORDISTRIKT.NAMN.          */
         FOR EACH KUNDSTOR WHERE KUNDSTOR.ARTAL = STORDISTRIKT.ARTAL AND 
            KUNDSTOR.DISTRIKTID = STORDISTRIKT.DISTRIKTID:
            DISP KUNDSTOR.SPANID KUNDSTOR.INDATUM KUNDSTOR.DISTRIKTID KUNDSTOR.ARTAL KUNDSTOR.ANTALLUFT KUNDSTOR.ANTALKUNDER 
            KUNDSTOR.ANTALKABEL KUNDSTOR.ANTALBLAND.
         END.
      END.
      
  END.
  
  
  
-------------------------------------------------------------*/
DEFINE VARIABLE utnr AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE bredd AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE nrcol AS INTEGER EXTENT 50 NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

DEFINE VARIABLE rubriker AS CHARACTER EXTENT 31 NO-UNDO.
DEFINE VARIABLE rubrikerint AS INTEGER EXTENT 31 NO-UNDO.
DEFINE VARIABLE antalrub AS INTEGER NO-UNDO.
DEFINE VARIABLE j AS INTEGER NO-UNDO.
DEFINE VARIABLE z AS INTEGER NO-UNDO.

   FIND FIRST  avdelningtemp WHERE avdelningtemp.AVDELNINGNR = foretag AND avdelningtemp.KOSTMASK = 1.
   CREATE tidut.
   ASSIGN
   /*SUBSTRING(tidut.UT,1) = "Företag:"
   SUBSTRING(tidut.UT, 15) = avdelningtemp.AVDELNINGNAMN*/
   SUBSTRING(tidut.UT,1) = "Kunddata för: " + STRING(art).

   ASSIGN
   nrcol[1] = 1
   nrcol[2] = 2
   nrcol[3] = 3
   nrcol[4] = 4
   nrcol[5] = 5
   nrcol[6] = 6
   nrcol[7] = 7
   nrcol[8] = 8
   nrcol[9] = 9
   nrcol[10] = 10
   nrcol[11] = 11
   nrcol[12] = 12
   nrcol[13] = 13
   nrcol[14] = 14
   nrcol[15] = 15
   nrcol[16] = 16
   nrcol[17] = 17
   nrcol[18] = 18
   nrcol[19] = 19
   nrcol[20] = 20
   nrcol[21] = 21
   nrcol[22] = 22
   nrcol[23] = 23
   nrcol[24] = 24
   nrcol[25] = 25
   nrcol[26] = 26
   nrcol[27] = 27
   nrcol[28] = 28
   nrcol[29] = 29
   nrcol[30] = 30

 
   breddantal = 30   /*antal kolumner*/
   bredd[1] = 40
   bredd[2] = 30
   bredd[3] = 25
   bredd[4] = 25
   bredd[5] = 25
   bredd[6] = 25
   bredd[7] = 25
   bredd[8] = 25
   bredd[9] = 25
   bredd[10] = 25
   bredd[11] = 25
   bredd[12] = 25
   bredd[13] = 25
   bredd[14] = 25
   bredd[15] = 25
   bredd[16] = 25
   bredd[17] = 25
   bredd[18] = 25
   bredd[19] = 25
   bredd[20] = 25
   bredd[21] = 25
   bredd[22] = 25
   bredd[23] = 25
   bredd[24] = 25
   bredd[25] = 25
   bredd[26] = 25
   bredd[27] = 25
   bredd[28] = 25
   bredd[29] = 25
   bredd[30] = 25

   
   utnr[1] = 1
   utnr[2] = 2
   utnr[3] = 3
   utnr[4] = 4
   utnr[5] = 5
   utnr[6] = 6
   utnr[7] = 7
   utnr[8] = 8
   utnr[9] = 9
   utnr[10] = 10
   utnr[11] = 11
   utnr[12] = 12
   utnr[13] = 13
   utnr[14] = 14
   utnr[15] = 15
   utnr[16] = 16
   utnr[17] = 17
   utnr[18] = 18
   utnr[19] = 19
   utnr[20] = 20
   utnr[21] = 21
   utnr[22] = 22
   utnr[23] = 23
   utnr[24] = 24
   utnr[25] = 25
   utnr[26] = 26
   utnr[27] = 27
   utnr[28] = 28
   utnr[29] = 29
   utnr[30] = 30.
   
   i = 2.     
   utnr[nrcol[1]] = 1.
   DO WHILE i <= breddantal:             
      utnr[i] = utnr[i - 1] + bredd[i - 1] + 1.            
      i = i + 1.
   END.   
   ASSIGN
   str = "".  
   i = 1.
   DO WHILE i <= utnr[breddantal] + bredd[breddantal] - 1:
      str = str + "=".     
      i = i + 1.
   END.   
   i = 2.      
   DO WHILE i <= breddantal:             
      SUBSTRING(str,(utnr[i] - 1),1) = ".".      
      i = i + 1.
   END.          
   CREATE tidut.  
   CREATE tidut.   
   

   /* rubriker */
   
   rubriker[1] = "FÖRETAG".
   rubriker[2] = "DISTRIKT".
   j = 3.
   
   /*spänningsrubriker*/
   FOR EACH spann_temp BY spanid DESCENDING:
      rubriker[j] = spann_temp.NAMN.
      rubrikerint[j] = spann_temp.SPANID.

      j = j + 1.
   END.

/*
   FOR EACH spanningsnivtemp NO-LOCK:
      rubriker[j] = spanningsnivtemp.NAMN.
      rubrikerint[j] = spanningsnivtemp.SPANID.

      j = j + 1.
   END.*/

   antalrub = j.

   /*mer rubriker*/
   rubriker[j] = "LÄNGD".
   j = j + 1.
   rubriker[j] = "ANTAL KABEL".
   j = j + 1.
   rubriker[j] = "ANTAL LUFT".
   j = j + 1.
   rubriker[j] = "ANTAL BLAND".
   
   z = 1.
   CREATE tidut.
   DO WHILE z <= j:
      SUBSTRING(tidut.UT,utnr[nrcol[z]]) = rubriker[z].
      z = z + 1.
   END.

   /*
   CREATE tidut. 
   ASSIGN                                                                                                       
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "FÖRETAG"             
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "DISTRIKT"             
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "SPÄNNING"         
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "ANTAL KUNDER"
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "LÄNGD"
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "LÄNGD KABEL"
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "LÄNGD LUFT"
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "LÄNGD BLAND".
   */
   DEFINE VARIABLE temprub AS INTEGER NO-UNDO.
   DEFINE VARIABLE hitta AS INTEGER NO-UNDO.
   DEFINE VARIABLE varde AS INTEGER NO-UNDO.
   
/**/
   /*FOR EACH avdelningtemp WHERE avdelningtemp.AVDELNINGNR = foretag AND avdelningtemp.KOSTMASK = 1:*/
   FOR EACH avdelningtemp WHERE avdelningtemp.KOSTMASK = 1:
      FOR EACH stordistemp WHERE stordistemp.AVDELNINGNR = avdelningtemp.AVDELNINGNR AND stordistemp.ARTAL = artal:
      
         /*CREATE tidut.
         SUBSTRING(tidut.UT,1) = str.
         CREATE tidut.*/
         
         FOR EACH kundstortemp WHERE kundstortemp.ARTAL = art AND kundstortemp.DISTRIKTID = stordistemp.DISTRIKTID:
            CREATE tidut.
            SUBSTRING(tidut.UT,utnr[nrcol[1]]) = avdelningtemp.AVDELNINGNAMN.
            SUBSTRING(tidut.UT,utnr[nrcol[2]]) = stordistemp.NAMN.
            
            FIND FIRST spanningsnivtemp WHERE spanningsnivtemp.SPANID = kundstortemp.SPANID NO-LOCK NO-ERROR.
            IF AVAILABLE spanningsnivtemp THEN DO:
               hitta = 3.
            
               DO WHILE hitta <= j:
                  varde = rubrikerint[hitta].
                  
                  IF varde = spanningsnivtemp.SPANID THEN DO:
                     SUBSTRING(tidut.UT,utnr[nrcol[hitta]]) = STRING(kundstortemp.ANTALKUNDER,">>>>>>9").
                     LEAVE.
                  END.
                  hitta = hitta + 1.
               END.
               
               /*SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(spanningsnivtemp.NAMN).   */
            END.
            /*SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(kundstortemp.ANTALKUNDER,">>>>>>>>>>>>9").*/
            FIND FIRST ledningsdatatemp WHERE ledningsdatatemp.SPANID = kundstortemp.SPANID AND ledningsdatatemp.DISTRIKTID = kundstortemp.DISTRIKTID AND kundstortemp.ARTAL = stordistemp.ARTAL NO-LOCK NO-ERROR.
            IF AVAILABLE ledningsdatatemp THEN DO:               
               temprub = antalrub.
               SUBSTRING(tidut.UT,utnr[nrcol[temprub]]) = STRING(ledningsdatatemp.LANGD,">>>>>>9").
               temprub = temprub + 1.
               SUBSTRING(tidut.UT,utnr[nrcol[temprub]]) = STRING(ledningsdatatemp.LANGDKABEL,">>>>>>9").
               temprub = temprub + 1.
               SUBSTRING(tidut.UT,utnr[nrcol[temprub]]) = STRING(ledningsdatatemp.LANGDLUFT,">>>>>>9").
               temprub = temprub + 1.
               SUBSTRING(tidut.UT,utnr[nrcol[temprub]]) = STRING(ledningsdatatemp.LANGDBLAND,">>>>>>9").
               
               /*
               SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(ledningsdatatemp.LANGD,">>>>9").
               SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(ledningsdatatemp.LANGDKABEL,">>>>9").
               SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(ledningsdatatemp.LANGDLUFT,">>>>9").
               SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(ledningsdatatemp.LANGDBLAND,">>>>9").*/
            END.
            


         END.
      END.
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

