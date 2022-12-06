&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
DEFINE TEMP-TABLE vispers
   FIELD PERSONALKOD AS CHARACTER
   FIELD FORNAMN AS CHARACTER
   FIELD EFTERNAMN AS CHARACTER
   INDEX PKOD IS PRIMARY PERSONALKOD.
DEFINE TEMP-TABLE valtemp
   FIELD VALDLISTA AS CHARACTER
   FIELD BAVAL AS INTEGER
   FIELD ALLTID AS LOGICAL
   FIELD STARTDATUM AS DATE
   FIELD SLUTDATUM AS DATE.
   
DEFINE INPUT PARAMETER vallista AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR valtemp.
DEFINE INPUT PARAMETER TABLE FOR vispers.

/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{EXECLIN.I}
{PERSOTB.I}
DEFINE SHARED VARIABLE SEL_UPP AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE  visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/
DEFINE VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE str2 AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE str3 AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE otbeordgapph AS HANDLE NO-UNDO.   
DEFINE VARIABLE entrygkand AS LOGICAL NO-UNDO.   
{TIDUTTTNEW.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-VINST
&Scoped-define BROWSE-NAME BRW_GOTID

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES persotb

/* Definitions for BROWSE BRW_GOTID                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_GOTID persotb.OTB persotb.OTBNAMN ~
persotb.PERSONALKOD persotb.FORNAMN persotb.EFTERNAMN persotb.DATUM ~
persotb.START persotb.SLUT persotb.TOTALT persotb.AONR persotb.DELNR ~
persotb.GODKANNARE persotb.GNAMN persotb.GKAND persotb.GKANDVEMNAR ~
persotb.KOMMENTAR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_GOTID persotb.GKAND 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_GOTID persotb
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_GOTID persotb
&Scoped-define QUERY-STRING-BRW_GOTID FOR EACH persotb NO-LOCK
&Scoped-define OPEN-QUERY-BRW_GOTID OPEN QUERY BRW_GOTID FOR EACH persotb NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_GOTID persotb
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_GOTID persotb


/* Definitions for FRAME FRAME-VINST                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-VINST ~
    ~{&OPEN-QUERY-BRW_GOTID}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_GOTID BTN_AVS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_GOTID FOR 
      persotb SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_GOTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_GOTID WINDOW-2 _STRUCTURED
  QUERY BRW_GOTID NO-LOCK DISPLAY
      persotb.OTB COLUMN-LABEL "Övertids!beordrare" FORMAT "x(8)":U
      persotb.OTBNAMN COLUMN-LABEL "Namn" FORMAT "x(256)":U WIDTH 10
      persotb.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(9)":U
      persotb.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 10
      persotb.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 10
      persotb.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      persotb.START COLUMN-LABEL "Start" FORMAT ">9.99":U
      persotb.SLUT COLUMN-LABEL "Slut" FORMAT ">9.99":U
      persotb.TOTALT COLUMN-LABEL "Totalt" FORMAT ">9.99":U
      persotb.AONR FORMAT "x(8)":U
      persotb.DELNR FORMAT ">>>>9":U
      persotb.GODKANNARE COLUMN-LABEL "Godkännare" FORMAT "x(10)":U
      persotb.GNAMN COLUMN-LABEL "Namn" FORMAT "x(256)":U WIDTH 10
      persotb.GKAND COLUMN-LABEL "Ötid!godkänd" FORMAT "Ja/Nej":U
      persotb.GKANDVEMNAR COLUMN-LABEL "Vem när" FORMAT "x(256)":U
            WIDTH 20
      persotb.KOMMENTAR COLUMN-LABEL "Kommentar" FORMAT "x(256)":U
            WIDTH 30
  ENABLE
      persotb.GKAND
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 122.5 BY 23.25
         TITLE "Godkänn övertid" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BRW_GOTID AT ROW 2 COL 2.5 WIDGET-ID 100
     BTN_AVS AT ROW 26 COL 110.88
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 26.21.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: persotb T "?" NO-UNDO temp-db persotb
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Godkänn övertid"
         HEIGHT             = 26.29
         WIDTH              = 125
         MAX-HEIGHT         = 28.79
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.79
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
/* SETTINGS FOR FRAME FRAME-VINST
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_GOTID 1 FRAME-VINST */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_GOTID
/* Query rebuild information for BROWSE BRW_GOTID
     _TblList          = "Temp-Tables.persotb"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.persotb.OTB
"persotb.OTB" "Övertids!beordrare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.persotb.OTBNAMN
"persotb.OTBNAMN" "Namn" "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.persotb.PERSONALKOD
"persotb.PERSONALKOD" "Enhet/!Sign" "x(9)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.persotb.FORNAMN
"persotb.FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.persotb.EFTERNAMN
"persotb.EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.persotb.DATUM
"persotb.DATUM" "Datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.persotb.START
"persotb.START" "Start" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.persotb.SLUT
"persotb.SLUT" "Slut" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.persotb.TOTALT
"persotb.TOTALT" "Totalt" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   = Temp-Tables.persotb.AONR
     _FldNameList[11]   = Temp-Tables.persotb.DELNR
     _FldNameList[12]   > Temp-Tables.persotb.GODKANNARE
"persotb.GODKANNARE" "Godkännare" "x(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > Temp-Tables.persotb.GNAMN
"persotb.GNAMN" "Namn" "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > Temp-Tables.persotb.GKAND
"persotb.GKAND" "Ötid!godkänd" "Ja/Nej" "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > Temp-Tables.persotb.GKANDVEMNAR
"persotb.GKANDVEMNAR" "Vem när" "x(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > Temp-Tables.persotb.KOMMENTAR
"persotb.KOMMENTAR" "Kommentar" "x(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_GOTID */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-VINST
/* Query rebuild information for FRAME FRAME-VINST
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-VINST */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_GOTID
&Scoped-define SELF-NAME BRW_GOTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_GOTID WINDOW-2
ON ROW-LEAVE OF BRW_GOTID IN FRAME FRAME-VINST /* Godkänn övertid beordrat av ej ordinarie godkännare */
DO:
   RUN rowleave_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME persotb.GKAND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL persotb.GKAND BRW_GOTID _BROWSE-COLUMN WINDOW-2
ON ENTRY OF persotb.GKAND IN BROWSE BRW_GOTID /* Ötid!godkänd */
DO:
   
   IF AVAILABLE persotb THEN DO:
      entrygkand = persotb.GKAND.
      DISPLAY persotb.GKAND WITH BROWSE BRW_GOTID.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL persotb.GKAND BRW_GOTID _BROWSE-COLUMN WINDOW-2
ON LEAVE OF persotb.GKAND IN BROWSE BRW_GOTID /* Ötid!godkänd */
DO:
  IF AVAILABLE persotb THEN DO:
      persotb.GKAND = INPUT BROWSE BRW_GOTID persotb.GKAND.           
      DISPLAY persotb.GKAND WITH BROWSE BRW_GOTID.             
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL persotb.GKAND BRW_GOTID _BROWSE-COLUMN WINDOW-2
ON MOUSE-SELECT-CLICK OF persotb.GKAND IN BROWSE BRW_GOTID /* Ötid!godkänd */
DO:
  IF AVAILABLE persotb THEN DO:
      IF INPUT BROWSE BRW_GOTID persotb.GKAND = TRUE THEN persotb.GKAND = FALSE.
      IF INPUT BROWSE BRW_GOTID persotb.GKAND = FALSE THEN persotb.GKAND = TRUE.        
      DISPLAY persotb.GKAND WITH BROWSE BRW_GOTID.             
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-2
ON CHOOSE OF BTN_AVS IN FRAME FRAME-VINST /* Avsluta */
DO:
   RUN rowleave_UI.
   {BORTBRWPROC.I}   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

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
   {AMERICANEUROPEAN.I}
   EMPTY TEMP-TABLE tidut NO-ERROR.    
   FIND FIRST valtemp NO-ERROR.
   IF vallista = 29 THEN DO:   
      RUN persotbhmt_UI IN otbeordgapph (INPUT TABLE valtemp, INPUT TABLE vispers, OUTPUT TABLE persotb).               
   END.   
   musz = FALSE.                                                  
   RUN enable_UI.   
   {FRMSIZE.I}
   ASSIGN
   Guru.GlobalaVariabler:collefth = ?
   Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).     
   {musarrow.i}  
   IF visvalvar = 2 THEN DO:     
      APPLY "CHOOSE" TO BTN_AVS IN FRAME {&FRAME-NAME}. 
      RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
      Guru.GlobalaVariabler:retvalkoll = FALSE.
      LEAVE MAIN-BLOCK.
   END.
   {WIN_M_SLUT.I}
   {EUROPEANAMERICAN.I}
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
      (INPUT BRW_GOTID:HANDLE IN FRAME {&FRAME-NAME}).
   RUN dynprogextra IN brwproc[1] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[1] (INPUT TRUE).
   IF Guru.Konstanter:appcon THEN DO:              
      RUN OTBTIDGAPP.P PERSISTENT SET otbeordgapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN OTBTIDGAPP.P PERSISTENT SET otbeordgapph.
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
  ENABLE BRW_GOTID BTN_AVS 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VINST}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excel_UI WINDOW-2 
PROCEDURE excel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
       /*Vilka kolumner*/
   IF vallista = 20 THEN DO:   
      ASSIGN
      startc = "A"
      slutc = "I"
      slutbredd = 5
      utnr[1] = 1
      utnr[2] = 12
      utnr[3] = 30
      utnr[4] = 41
      utnr[5] = 51
      utnr[6] = 58
      utnr[7] = 70
      utnr[8] = 79
      utnr[9] = 85.
      FIND FIRST valtemp NO-ERROR.
      IF valtemp.BAVAL > 10 THEN DO:   
         ASSIGN
         utnr[10] = 91   
         slutbredd = 40
         slutc = "J".
      END.
      allachar[7] = TRUE.    /*vilka kolumner skall vara character*/
      RUN satestat_UI. /*BARA FÖR DE SOM HAR UTNR ANARS COLBREDD_ui*/
      RUN startexcel_UI.
      FIND FIRST tidut NO-LOCK NO-ERROR.
      /*Kolumnbredd*/
      raknare = 1.
      RUN kolumnexcel_UI.
      /*Rubriker*/   
      REPEAT:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
         FIND NEXT tidut NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidut THEN DO:
            LEAVE.
         END.
         IF SUBSTRING(tidut.UT,utnr[2],5) = "ORT/B" THEN DO:    /*Rubrikslut FÖRSTA */         
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
            LEAVE.
         END.
      END.   
      /*Poster*/
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF AVAILABLE tidut THEN DO:         
         raknare = 1.
         REPEAT:
            IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
               RUN understryk_UI (INPUT 4,INPUT 2). 
               musz = musz.
            END.
            ELSE IF SUBSTRING(tidut.UT,1,5) = "-----" THEN DO:
               rubrikvar = TRUE.
               RUN understryk_UI (INPUT 4,INPUT 2). 
               musz = musz.
            END.
            ELSE IF SUBSTRING(tidut.UT,utnr[3],9) = "SIGN NAMN" THEN DO:    /*Rubrikslut ANDRA */         
               RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
            END.
            ELSE DO:
               IF rubrikvar = TRUE THEN do:
                  RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0).
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
      END.   
   END.
   IF vallista = 29 THEN DO:
      ASSIGN
      startc = "A"
      slutc = "H" 
      slutbredd = 40
      utnr[1] = 1
      utnr[2] = 7
      utnr[3] = 13
      utnr[4] = 22
      utnr[5] = 28
      utnr[6] = 35
      utnr[7] = 42
      utnr[8] = 53.      
      FIND FIRST valtemp NO-ERROR.
  
      allachar[3] = TRUE.    /*vilka kolumner skall vara character*/
      allachar[7] = TRUE. 
      RUN satestat_UI. /*BARA FÖR DE SOM HAR UTNR ANARS COLBREDD_ui*/
      RUN startexcel_UI.
      FIND FIRST tidut NO-LOCK NO-ERROR.
      /*Kolumnbredd*/
      raknare = 1.
      RUN kolumnexcel_UI.
      /*Rubriker*/   
      REPEAT:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
         FIND NEXT tidut NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidut THEN DO:
            LEAVE.
         END.
         IF SUBSTRING(tidut.UT,utnr[1],6) = "ENHET/" THEN DO:    /*Rubrikslut FÖRSTA */         
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
            LEAVE.
         END.
      END.   
      /*Poster*/
      FIND NEXT tidut NO-LOCK NO-ERROR.
      raknare = 1.
      REPEAT:
         IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
            RUN understryk_UI (INPUT 4,INPUT 2). 
            musz = musz.
         END.
         ELSE IF SUBSTRING(tidut.UT,1,5) = "-----" THEN DO:
            rubrikvar = TRUE.
            RUN understryk_UI (INPUT 4,INPUT 2). 
            musz = musz.
         END.
         ELSE IF SUBSTRING(tidut.UT,1,17) = "Övertidsbeordrare" THEN DO:    
            RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).            
         END.
         ELSE IF SUBSTRING(tidut.UT,utnr[1],4) = "SIGN" THEN DO:    /*Rubrikslut ANDRA */         
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
         END.
         ELSE DO:
            IF rubrikvar = TRUE THEN do:
               RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0).
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
   END.
   RUN slutexcel_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispextra_UI WINDOW-2 
PROCEDURE rowdispextra_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER TABLE FOR coltemp.
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.   
   IF brwh:NAME = "BRW_GOTID" THEN RUN rowdispgod_UI (INPUT 1). 
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispgod_UI WINDOW-2 
PROCEDURE rowdispgod_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   
   IF vad = 1 THEN DO:
      IF AVAILABLE persotb THEN DO:
         IF persotb.GKAND = FALSE THEN DO: 
            ASSIGN
            persotb.GKAND:BGCOLOR IN BROWSE BRW_GOTID = 12.   /*varforetypval[28].*/
         END.
         ELSE persotb.GKAND:BGCOLOR IN BROWSE BRW_GOTID = ?.                
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowleave_UI WINDOW-2 
PROCEDURE rowleave_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*entrygkand = persotb.GKAND.*/
  IF AVAILABLE persotb THEN DO:  
     persotb.GKAND  = INPUT BROWSE BRW_GOTID persotb.GKAND .
     IF entrygkand NE persotb.GKAND THEN DO:
        DISPLAY persotb.GKAND  WITH BROWSE BRW_GOTID.
        EMPTY TEMP-TABLE epersotb NO-ERROR.
        CREATE epersotb.
        BUFFER-COPY persotb TO epersotb .     
        IF persotb.GKAND = FALSE THEN DO:
           ASSIGN 
           persotb.GKANDVEMNAR = ""
           epersotb.GKANDVEMNAR = "".      
         END.       
         ELSE DO:
            IF persotb.GKAND = TRUE THEN DO:
               persotb.GKANDVEMNAR = Guru.Konstanter:globanv + " " +  STRING(TODAY) + " " + STRING(TIME,"HH:MM").
               epersotb.GKANDVEMNAR = Guru.Konstanter:globanv + " " +  STRING(TODAY) + " " + STRING(TIME,"HH:MM").
            END.
         END.           
         RUN sppersotb_UI IN otbeordgapph (INPUT-OUTPUT TABLE  epersotb).        
         RUN rowdispgod_UI (INPUT 1).              
         DISPLAY  persotb.GKAND persotb.GKANDVEMNAR WITH BROWSE BRW_GOTID.
      END.   
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI WINDOW-2 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  /*UT*/         
   FIND LAST tidut NO-LOCK NO-ERROR.     
   FIND FIRST valtemp NO-ERROR.
   IF  valtemp.BAVAL < 10 THEN DO:   
      {PRINTSTAENDE.I}             
   END.
   ELSE DO:
      {PRINTLIGGANDE.I}              
   END.
   PUT SKIP (2).   
   PUT PAGE-NUMBER AT 80 SKIP.              /*RAD 3*/  
   FOR EACH tidut:  
      IF SUBSTRING(tidut.UT,1,6) = "BILAGA" THEN DO:
         PAGE.
         PUT SKIP (2).
         PUT PAGE-NUMBER AT 80  SKIP. 
         PUT tidut.UT AT 6 SKIP .       
      END.
      ELSE IF SUBSTRING(tidut.UT,132,1) = "$" THEN DO:
         PAGE.
         PUT SKIP (2).
         PUT PAGE-NUMBER AT 80  SKIP. 
         PUT tidut.UT AT 6 SKIP .      
      END.
      ELSE DO:                               
         IF LINE-COUNTER = 1 THEN DO:
           /* PAGE.*/
            PUT SKIP (2).
            PUT PAGE-NUMBER AT 80 SKIP.
            PUT UNFORMATTED str2 AT 6 SKIP.
            PUT UNFORMATTED str3 AT 6 SKIP.
            PUT UNFORMATTED str AT 6 SKIP.
         END.         
         PUT tidut.UT AT 6 SKIP.     
         IF LINE-COUNTER > PAGE-SIZE THEN DO:
            PAGE.
            PUT SKIP (2).
            PUT PAGE-NUMBER AT 80 SKIP.
            PUT UNFORMATTED str2 AT 6 SKIP.
            PUT UNFORMATTED str3 AT 6 SKIP.
            PUT UNFORMATTED str AT 6 SKIP.
         END.
         
      END.                     
   END.  
   OUTPUT CLOSE.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

