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
{DIRDEF.I}
&SCOPED-DEFINE NEW
{GLOBVAR2DEL1.I}
{REGVAR.I}
{OMRTEMPW.I}
DEFINE SHARED TEMP-TABLE omrtemp2 NO-UNDO LIKE omrtemp.
{SOKDEF.I}
DEFINE INPUT-OUTPUT PARAMETER vallista AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR valsoktemp.
/* Local Variable Definitions ---                                       */  
{ALLDEF.I}
&Scoped-define NEW
{BYTAO.I}
DEFINE SHARED VARIABLE  visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/
DEFINE SHARED VARIABLE valdelnrlog AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE SEL_UPP AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE aonummer AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE delnummer AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE utomr AS CHARACTER NO-UNDO.  
DEFINE VARIABLE arrhjsumtb AS DECIMAL NO-UNDO. 
DEFINE VARIABLE arrhjsum AS DECIMAL NO-UNDO.    
DEFINE VARIABLE arrhjsumtid AS DECIMAL NO-UNDO.  
DEFINE VARIABLE arrhjsumotid AS DECIMAL NO-UNDO. 
DEFINE VARIABLE arrhjsumove AS DECIMAL NO-UNDO.
DEFINE VARIABLE arrhjsumtra AS DECIMAL NO-UNDO.   
DEFINE VARIABLE arrhjsumlon AS DECIMAL NO-UNDO. 
DEFINE VARIABLE arrhjsumind AS DECIMAL NO-UNDO. 
DEFINE VARIABLE arrhjsumikost AS DECIMAL NO-UNDO.   
DEFINE VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO. 
DEFINE VARIABLE str2 AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE utomr2 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE utomr3 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE aoval AS LOGICAL NO-UNDO.
DEFINE VARIABLE vardelnr AS INTEGER NO-UNDO.
DEFINE VARIABLE varaonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE berindvar AS DECIMAL NO-UNDO.
DEFINE VARIABLE omrlistutfapph AS HANDLE NO-UNDO.

&Scoped-define NEW
{VISUPPTMP.I}          
   

DEFINE TEMP-TABLE slutsum           
   FIELD PERSONALKOD AS CHARACTER 
   FIELD PRIS AS DECIMAL     
   FIELD PRISTYP AS CHARACTER 
   FIELD OMRADE AS CHARACTER 
   FIELD TIMMAR AS DECIMAL LABEL "TIMMAR"      
   FIELD OTIMMAR AS DECIMAL LABEL "OTIMMAR"         
   FIELD BELOPP AS DECIMAL LABEL "ARBKOSTNAD"           
   FIELD OBELOPP AS DECIMAL LABEL "Ö-KOSTNAD"  
   FIELD OANTAL AS DECIMAL  LABEL "Ö-ANTAL"         
   FIELD TBELOPP AS DECIMAL LABEL "T-KOSTNAD"
   FIELD TANTAL AS DECIMAL  LABEL "T-ANTAL"    
   FIELD LONKOST AS DECIMAL LABEL "L-KOSTNAD" 
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER 
   FIELD INDEREKT AS DECIMAL   
   FIELD INOMRADE AS CHARACTER
   FIELD MED AS LOGICAL 
   FIELD ORT AS CHARACTER       
   FIELD IKOST AS DECIMAL
   INDEX OMR IS PRIMARY OMRADE AONR DELNR
   INDEX AONR AONR DELNR
   INDEX MED MED. 

DEFINE BUFFER slutsumbuff FOR slutsum.

   DEFINE TEMP-TABLE slutsum1           
   FIELD PERSONALKOD AS CHARACTER  
   FIELD PRIS AS DECIMAL   
   FIELD PRISTYP AS CHARACTER 
   FIELD OMRADE AS CHARACTER
   FIELD TIMMAR AS DECIMAL LABEL "TIMMAR"      
   FIELD OTIMMAR AS DECIMAL LABEL "OTIMMAR"         
   FIELD BELOPP AS DECIMAL LABEL "ARBKOSTNAD"           
   FIELD OBELOPP AS DECIMAL LABEL "Ö-KOSTNAD"  
   FIELD OANTAL AS DECIMAL  LABEL "Ö-ANTAL"         
   FIELD TBELOPP AS DECIMAL LABEL "T-KOSTNAD"
   FIELD TANTAL AS DECIMAL  LABEL "T-ANTAL"    
   FIELD LONKOST AS DECIMAL LABEL "L-KOSTNAD"  
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD INDEREKT AS DECIMAL  
   FIELD ORT AS CHARACTER      
   INDEX OMR IS PRIMARY OMRADE AONR DELNR.         
   
DEFINE TEMP-TABLE slutsum2           
   FIELD PERSONALKOD AS CHARACTER
   FIELD PRIS AS DECIMAL   
   FIELD PRISTYP AS CHARACTER
   FIELD OMRADE AS CHARACTER
   FIELD TIMMAR AS DECIMAL LABEL "TIMMAR"      
   FIELD OTIMMAR AS DECIMAL LABEL "OTIMMAR"         
   FIELD BELOPP AS DECIMAL LABEL "ARBKOSTNAD"           
   FIELD OBELOPP AS DECIMAL LABEL "Ö-KOSTNAD"  
   FIELD OANTAL AS DECIMAL  LABEL "Ö-ANTAL"         
   FIELD TBELOPP AS DECIMAL LABEL "T-KOSTNAD"
   FIELD TANTAL AS DECIMAL  LABEL "T-ANTAL"    
   FIELD LONKOST AS DECIMAL LABEL "L-KOSTNAD"  
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD INDEREKT AS DECIMAL 
   FIELD ORT AS CHARACTER         
   INDEX OMR IS PRIMARY OMRADE.       
   
DEFINE TEMP-TABLE prislutsum           
   FIELD PERSONALKOD AS CHARACTER
   FIELD PRIS AS DECIMAL  
   FIELD PRISTYP AS CHARACTER
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER  
   FIELD OMRADE AS CHARACTER
   FIELD TIMMAR AS DECIMAL LABEL "TIMMAR"          
   FIELD BELOPP AS DECIMAL LABEL "ARBKOSTNAD"           
   FIELD OBELOPP AS DECIMAL LABEL "Ö-KOSTNAD"  
   FIELD OANTAL AS DECIMAL LABEL "Ö-ANTAL"         
   FIELD TBELOPP AS DECIMAL LABEL "T-KOSTNAD"
   FIELD TANTAL AS DECIMAL  LABEL "T-ANTAL"         
   INDEX AONR IS PRIMARY AONR DELNR ASCENDING.  

DEFINE TEMP-TABLE prislutsum1           
   FIELD PERSONALKOD AS CHARACTER 
   FIELD PRIS AS DECIMAL    
   FIELD PRISTYP AS CHARACTER
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD OMRADE AS CHARACTER
   FIELD TIMMAR AS DECIMAL LABEL "TIMMAR"          
   FIELD BELOPP AS DECIMAL LABEL "ARBKOSTNAD"           
   FIELD OBELOPP AS DECIMAL LABEL "Ö-KOSTNAD"  
   FIELD OANTAL AS DECIMAL  LABEL "Ö-ANTAL"         
   FIELD TBELOPP AS DECIMAL LABEL "T-KOSTNAD"
   FIELD TANTAL AS DECIMAL  LABEL "T-ANTAL"         
   INDEX AONR IS PRIMARY AONR DELNR ASCENDING.


DEFINE TEMP-TABLE restid2  
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER                   
   FIELD OMRADE AS CHARACTER
   FIELD TIMMAR AS DECIMAL LABEL "RTIMMAR"                
   INDEX OMR IS PRIMARY OMRADE. 
                
DEFINE TEMP-TABLE restid3  
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER                  
   FIELD OMRADE AS CHARACTER
   FIELD TIMMAR AS DECIMAL LABEL "RTIMMAR"                
   INDEX OMR IS PRIMARY OMRADE.       
   
DEFINE TEMP-TABLE restid4  
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER                     
   FIELD OMRADE AS CHARACTER
   FIELD TIMMAR AS DECIMAL LABEL "RTIMMAR"                
   INDEX OMR IS PRIMARY OMRADE.


DEFINE TEMP-TABLE slutsumpt
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD GEOMRADE AS CHARACTER            
   FIELD OMRADE AS CHARACTER
   FIELD TIMMAR AS DECIMAL LABEL "TIMMAR" 
   FIELD OTIMMAR AS DECIMAL
   FIELD BELOPP AS DECIMAL
   FIELD OBELOPP AS DECIMAL
   FIELD LONKOST AS DECIMAL
   FIELD TBELOPP AS DECIMAL                 
   INDEX OMR IS PRIMARY OMRADE
   INDEX AONRGEOMR AONR DELNR GEOMRADE
   INDEX AONROMR AONR DELNR OMRADE.
DEFINE TEMP-TABLE slutsumpf 
   FIELD GEOMRADE AS CHARACTER              
   FIELD OMRADE AS CHARACTER
   FIELD TIMMAR AS DECIMAL LABEL "TIMMAR"
   FIELD OTIMMAR AS DECIMAL
   FIELD BELOPP AS DECIMAL
   FIELD OBELOPP AS DECIMAL
   FIELD LONKOST AS DECIMAL
   FIELD TBELOPP AS DECIMAL                           
   INDEX OMR IS PRIMARY OMRADE
   INDEX GEOMR GEOMRADE.  
DEFINE TEMP-TABLE slutsumot              
   FIELD OMRADE AS CHARACTER
   FIELD TIMMAR AS DECIMAL LABEL "TIMMAR"                   
   INDEX OMR IS PRIMARY OMRADE.    
DEFINE TEMP-TABLE slutsumof              
   FIELD OMRADE AS CHARACTER
   FIELD TIMMAR AS DECIMAL LABEL "TIMMAR"                   
   INDEX OMR IS PRIMARY OMRADE.  
DEFINE TEMP-TABLE slutsumat                 
   FIELD TIMMAR AS DECIMAL LABEL "TIMMAR".    
DEFINE TEMP-TABLE slutsumaf                 
   FIELD TIMMAR AS DECIMAL LABEL "TIMMAR".                        

DEFINE TEMP-TABLE sumartemp 
   FIELD AONR AS CHARACTER 
   FIELD DELNR AS INTEGER
   FIELD PERSONALKOD AS CHARACTER 
   FIELD NAMN AS CHARACTER FORMAT "X(12)"
   FIELD PRISTYP AS CHARACTER
   FIELD OMRADE AS CHARACTER 
   FIELD GEOMRADE AS CHARACTER 
   FIELD FASTAAONR AS LOGICAL
   FIELD TIMMAR AS DECIMAL LABEL "TIMMAR"  
   FIELD OTIMMAR AS DECIMAL LABEL "OTIMMAR" 
   FIELD BELOPP AS DECIMAL 
   FIELD OBELOPP AS DECIMAL 
   FIELD LONKOST AS DECIMAL 
   FIELD TBELOPP AS DECIMAL          
   INDEX OMRADE IS PRIMARY OMRADE PERSONALKOD.      

DEFINE {&NEW} SHARED VARIABLE RAD_PERIOD AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Visning per år", 1,
"Visning per period", 2
     SIZE 21.5 BY 2.5 NO-UNDO.      

DEFINE {&NEW} SHARED VARIABLE RAD_TYP AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Intäkter", 1,
"Kostnader", 2
     SIZE 12 BY 3.09 BGCOLOR 8 NO-UNDO.  


{TIDUTTTNEW.I}       
{EXECLIN.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-VINST
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


/* Definitions for FRAME FRAME-VINST                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-VINST ~
    ~{&OPEN-QUERY-BRW_UT}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FBTN_SKRIV FBTN_EXCEL BTN_AVS 

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

DEFINE BUTTON FBTN_EXCEL 
     LABEL "Till Excel":L 
     SIZE 14 BY 1 TOOLTIP "Visa i Excel format".

DEFINE BUTTON FBTN_SKRIV 
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
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 120 BY 24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BRW_UT AT ROW 1.5 COL 1.5
     FBTN_SKRIV AT ROW 26 COL 77.5
     FBTN_EXCEL AT ROW 26 COL 92.5
     BTN_AVS AT ROW 26 COL 107.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 121.5 BY 26.17.


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
         TITLE              = "Uppföljning"
         HEIGHT             = 26.46
         WIDTH              = 121.75
         MAX-HEIGHT         = 27.92
         MAX-WIDTH          = 123.75
         VIRTUAL-HEIGHT     = 27.92
         VIRTUAL-WIDTH      = 123.75
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
                                                                        */
/* BROWSE-TAB BRW_UT 1 FRAME-VINST */
/* SETTINGS FOR BROWSE BRW_UT IN FRAME FRAME-VINST
   NO-ENABLE                                                            */
ASSIGN 
       BRW_UT:HIDDEN  IN FRAME FRAME-VINST                = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "Temp-Tables.tidut"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.tidut.ut
     _Query            is OPENED
*/  /* BROWSE BRW_UT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-VINST
/* Query rebuild information for FRAME FRAME-VINST
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-VINST */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_UT
&Scoped-define SELF-NAME BRW_UT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_UT WINDOW-2
ON RIGHT-MOUSE-CLICK OF BRW_UT IN FRAME FRAME-VINST
DO:
   MESSAGE "Vill du visa listan i din webläsare?" VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO TITLE "Visa i din webläsare?" UPDATE svar AS LOGICAL.         
   CASE svar:
      WHEN TRUE THEN DO:
         RUN htmlut.p (INPUT vallista,INPUT TABLE tidut).
      END.
      WHEN FALSE THEN DO:
         musz = musz.
      END.
   END CASE.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-2
ON CHOOSE OF BTN_AVS IN FRAME FRAME-VINST /* Avsluta */
DO:
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_EXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EXCEL WINDOW-2
ON CHOOSE OF FBTN_EXCEL IN FRAME FRAME-VINST /* Till Excel */
DO:
   {muswait.i}
   IF vallista = 10 OR vallista = 11 THEN DO:
      RUN totutfexcel_UI.
   END.
   ELSE IF vallista = 13 THEN DO:
      RUN omrutfexcel_UI.
   END.
   ELSE IF vallista = 14 THEN DO:
      RUN prisutfexcel_UI.
   END.
   ELSE RUN excel_UI. 
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-2
ON CHOOSE OF FBTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO: 
   RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:    
     RUN ut_UI.      
   END.
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF FBTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO:
   RUN SIDLANGD.W.
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
DO:
   IF VALID-HANDLE(omrlistutfapph) THEN DELETE PROCEDURE omrlistutfapph.
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
   FIND FIRST visaupp WHERE visaupp.UPPFOLJVAL = vallista NO-LOCK NO-ERROR.
   FIND FIRST uppvaltemp WHERE uppvaltemp.VALDLISTA = visaupp.UT NO-LOCK NO-ERROR.
   FIND FIRST omrtemp2 WHERE omrtemp2.OMRADE = utomr NO-LOCK NO-ERROR.
   IF vallista = 10 THEN DO:
      RUN totutf_UI.
   END.
   ELSE IF vallista = 11 THEN DO:
      MESSAGE "Vill du att benämningen till " + CAPS(Guru.Konstanter:gaok) + " skall vara med i listan. OBS! välj liggande utskrift om svaret är Ja."
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL TITLE "Benämning till " + CAPS(Guru.Konstanter:gaok) UPDATE svar AS LOGICAL.         
      IF svar THEN DO:
         aoval = TRUE.
      END.
      ELSE IF NOT svar THEN DO:
          aoval = FALSE.
      END.
      ELSE DO:
         musz = FALSE.         
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
         LEAVE MAIN-BLOCK. 
      END.
      RUN totutf_UI.
   END.
   
   ELSE IF vallista = 13 THEN DO:
      RUN omrutf_UI.
   END.
   ELSE IF vallista = 14 THEN DO:
      RUN prisutf_UI.
   END.
   ELSE DO: 
      IF Guru.Konstanter:appcon THEN DO:
         RUN ARBGRFAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT TABLE valsoktemp,INPUT TABLE omrtemp2,OUTPUT TABLE tidut).
      END.
      ELSE DO:    
         RUN ARBGRFAPP.P
         (INPUT TABLE valsoktemp,INPUT TABLE omrtemp2,OUTPUT TABLE tidut).
      END.
      musz = FALSE.                       
      ENABLE BRW_UT WITH FRAME FRAME-VINST.
      BRW_UT:HIDDEN = FALSE.             
   END.
   RUN enable_UI. 
   inder = FALSE. 
   {FRMSIZE.I}  
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth). 
   IF FBTN_EXCEL:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_EXCEL:HANDLE.           
      RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).   
   END.
   Guru.GlobalaVariabler:colrighth = FBTN_SKRIV:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).   
   {musarrow.i}   
   IF visvalvar = 2 THEN DO:
      APPLY "CHOOSE" TO  FBTN_EXCEL.
      APPLY "CHOOSE" TO BTN_AVS IN FRAME {&FRAME-NAME}. 
      RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
      Guru.GlobalaVariabler:retvalkoll = FALSE.
      LEAVE MAIN-BLOCK.
   END.
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
   IF Guru.Konstanter:appcon THEN DO:
      RUN OMRLISTUTFAPP.P PERSISTENT SET omrlistutfapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN OMRLISTUTFAPP.P PERSISTENT SET omrlistutfapph.
   END.
   RUN anvglob_UI IN omrlistutfapph (INPUT Guru.Konstanter:globanv).
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
  ENABLE FBTN_SKRIV FBTN_EXCEL BTN_AVS 
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
   ASSIGN
   startc = "A"
   slutc = "J"
   slutbredd = 8
   utnr[1] = 1
   utnr[2] = 14
   utnr[3] = 30
   utnr[4] = 38
   utnr[5] = 46
   utnr[6] = 54
   utnr[7] = 61
   utnr[8] = 68
   utnr[9] = 76
   utnr[10] = 84.
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
      IF SUBSTRING(tidut.UT,utnr[2],4) = "NAMN" THEN DO:    /*Rubrikslut FÖRSTA */         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
         LEAVE.
      END.
   END.   
   /*Poster*/
   FIND NEXT tidut NO-LOCK NO-ERROR.
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,2) = "==" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2). 
         musz = musz.
      END.
      ELSE IF SUBSTRING(tidut.UT,1,5) = "-----" THEN DO:
         rubrikvar = TRUE.
         RUN understryk_UI (INPUT 4,INPUT 2). 
         musz = musz.
      END.
      ELSE IF SUBSTRING(tidut.UT,utnr[3],7) = "ARB.TID" THEN DO:    /*Rubrikslut ANDRA */         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
      END.
      ELSE IF SUBSTRING(tidut.UT,utnr[9],7) = "NÄRVARO" THEN DO:    /*Rubrikslut ANDRA */         
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
   RUN slutexcel_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE omrutfexcel_UI WINDOW-2 
PROCEDURE omrutfexcel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*Vilka kolumner*/
   ASSIGN
   startc = "A"
   slutc = "H"
   slutbredd = 8
   utnr[1] = 1 
   utnr[2] = 12
   utnr[3] = 20
   utnr[4] = 29
   utnr[5] = 38
   utnr[6] = 47
   utnr[7] = 56
   utnr[8] = 65.
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
      IF SUBSTRING(tidut.UT,utnr[3],4) = "ARB." THEN DO:    /*Rubrikslut FÖRSTA */         
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
      ELSE IF SUBSTRING(tidut.UT,utnr[3],4) = "ARB." THEN DO:    /*Rubrikslut FÖRSTA */         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).        
      END.
      ELSE IF SUBSTRING(tidut.UT,utnr[3],6) = "TIMMAR" THEN DO:    /*Rubrikslut ANDRA */         
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
   RUN slutexcel_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE omrutfhuvud_UI WINDOW-2 
PROCEDURE omrutfhuvud_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO TRANSACTION:
      str =               
"==========.=======.========.========.========.========.========.========".      
      CREATE tidut. 
      SUBSTRING(tidut.UT,57) = STRING(TODAY) + " " + STRING(TIME,"HH:MM").
      CREATE tidut. 
      CREATE tidut. 
      IF RAD_TYP = 1 THEN DO:                          
         tidut.UT = "U130- " + CAPS(Guru.Konstanter:gomrl) + " - INTÄKTER PERIOD".
      END.
      IF RAD_TYP = 2 THEN DO: 
         tidut.UT = "U130- " + CAPS(Guru.Konstanter:gomrl) + " - KOSTNADER PERIOD".
      END.     
      IF RAD_PERIOD = 1 THEN DO: 
         SUBSTRING(tidut.UT,37) = STRING(YEAR(bdatum),"9999").
      END.
      IF RAD_PERIOD = 2 THEN DO:
         SUBSTRING(tidut.UT,37) = STRING(bdatum) + " - " + STRING(avdatum).     
      END.                   
      CREATE tidut.
      CREATE tidut.            
      SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gomrk) + " :".      
      FIND FIRST omrtemp2 WHERE omrtemp2.OMRADE = utomr USE-INDEX OMR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE omrtemp2 THEN  SUBSTRING(tidut.UT,10) = CAPS(Guru.Konstanter:gomrk) + " SAKNAS".
      ELSE DO:   
         ASSIGN
         SUBSTRING(tidut.UT,10) = omrtemp2.OMRADE
         SUBSTRING(tidut.UT,17) = omrtemp2.NAMN.           
      END.
      CREATE tidut.     
      CREATE tidut.     
      ASSIGN                            
      SUBSTRING(tidut.UT,12) = CAPS(Guru.Konstanter:gaok) + "."              
      SUBSTRING(tidut.UT,20) = "ARB."
      SUBSTRING(tidut.UT,29) = "ARBETS"
      SUBSTRING(tidut.UT,38) = "ÖVER."
      SUBSTRING(tidut.UT,47) = "ÖVERTID"    
      SUBSTRING(tidut.UT,56) = "TRAKT."     
      SUBSTRING(tidut.UT,65) = "LÖNETILL".     
      IF RAD_TYP = 2 THEN DO: 
         SUBSTRING(tidut.UT,12) = "PERS.".  
      END.                                                    
      CREATE tidut.
      ASSIGN                     
      SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok)               
      SUBSTRING(tidut.UT,12) = CAPS(Guru.Konstanter:gomrk)
      SUBSTRING(tidut.UT,20) = "TIMMAR"      
      SUBSTRING(tidut.UT,29) = "KOSTNAD"
      SUBSTRING(tidut.UT,38) = "TIMMAR"
      SUBSTRING(tidut.UT,47) = "KOSTNAD"
      SUBSTRING(tidut.UT,56) = "KOSTNAD"
      SUBSTRING(tidut.UT,65) = "KOSTNAD". 
      CREATE tidut. 
      SUBSTRING(tidut.UT,1) = str.             
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE omrutfsumma_UI WINDOW-2 
PROCEDURE omrutfsumma_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   RUN omrutfsum_UI IN omrlistutfapph (INPUT valdelnrlog,INPUT utomr,INPUT RAD_TYP,INPUT RAD_PERIOD,
                                       INPUT bdatum,INPUT avdatum,INPUT aoval,
                                       INPUT inder,INPUT str,OUTPUT TABLE felmeddtemp,
                                       INPUT-OUTPUT TABLE tidut).            
   FOR EACH felmeddtemp NO-LOCK :
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".
      DELETE felmeddtemp.
      musz = TRUE. 
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE omrutf_UI WINDOW-2 
PROCEDURE omrutf_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  EMPTY TEMP-TABLE slutsumpt NO-ERROR. 
  EMPTY TEMP-TABLE slutsumpf NO-ERROR. 
  EMPTY TEMP-TABLE slutsumof NO-ERROR. 
  EMPTY TEMP-TABLE slutsumat NO-ERROR. 
  EMPTY TEMP-TABLE slutsumaf NO-ERROR. 
  EMPTY TEMP-TABLE dagtemp NO-ERROR. 
  EMPTY TEMP-TABLE sumartemp NO-ERROR.   
str=
"=====================================================================================".      
    
   DEFINE VARIABLE val1 AS LOGICAL.
   MESSAGE "Vill du att samtliga " + LC(Guru.Konstanter:gaok) + " skall summeras på huvudnummret ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1.
   CASE val1:
      WHEN TRUE THEN DO:
         valdelnrlog = TRUE.          
      END.
      WHEN FALSE THEN DO:
         valdelnrlog = FALSE.          
      END.
   END CASE.     
   RUN omrutfhuvud_UI. 
   IF musz = FALSE THEN RUN omrutfsumma_UI.    
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN. 
   END.                    
   ELSE DO:   
      ENABLE BRW_UT WITH FRAME FRAME-VINST.
      BRW_UT:HIDDEN = FALSE.             
   END.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prisutfexcel_UI WINDOW-2 
PROCEDURE prisutfexcel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
   slutbredd = 13
   utnr[1] = 1
   utnr[2] = 18
   utnr[3] = 29.
   
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
      IF SUBSTRING(tidut.UT,utnr[2],10) = CAPS(Guru.Konstanter:gdebk) THEN DO:    /*Rubrikslut FÖRSTA */         
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
   RUN slutexcel_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prisutfhuvud_UI WINDOW-2 
PROCEDURE prisutfhuvud_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
     /*HUVUD*/ 
   DO TRANSACTION:
      str = "================.==========.===========". 
      str2 = "---------------------------------------".      
      CREATE tidut.                     
      SUBSTRING(tidut.UT,30) = STRING(TODAY) + " " + STRING(TIME,"HH:MM").       
      CREATE tidut.
      CREATE tidut.                           
      tidut.UT = visaupp.TYP + "- ARBETSTIDENS FÖRDELNING PERIOD".
      IF RAD_PERIOD = 1 THEN DO: 
         SUBSTRING(tidut.UT,40) = STRING(YEAR(bdatum),"9999").
      END.
      IF RAD_PERIOD = 2 THEN DO:
         SUBSTRING(tidut.UT,40) = STRING(bdatum) + " - " + STRING(avdatum).      
      END.      
      CREATE tidut.  
      CREATE tidut.            
      SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gomrk) + " :".
      IF utomr NE "ALLA" THEN DO:
         FIND FIRST omrtemp2 WHERE omrtemp2.OMRADE = utomr USE-INDEX OMR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE omrtemp2 THEN  SUBSTRING(tidut.UT,10) = CAPS(Guru.Konstanter:gomrk) + " SAKNAS".
         ELSE SUBSTRING(tidut.UT,10) = omrtemp2.NAMN.
      END.
      ELSE DO: 
         SUBSTRING(tidut.UT,10) = utomr.
      END.       
      CREATE tidut.
      CREATE tidut.     
      ASSIGN                               
      SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gomrk)
      SUBSTRING(tidut.UT,18) = CAPS(Guru.Konstanter:gdebk)
      SUBSTRING(tidut.UT,29) = "ARB. TIMMAR".                                 
      CREATE tidut.                   
      SUBSTRING(tidut.UT,1) = str.             
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prisutfsumma_UI WINDOW-2 
PROCEDURE prisutfsumma_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   RUN prisutfsum_UI IN omrlistutfapph (INPUT utomr,INPUT RAD_PERIOD,
                                        INPUT bdatum,INPUT avdatum,
                                        INPUT aoval,INPUT inder,INPUT str,
                                        INPUT str2,INPUT-OUTPUT TABLE tidut).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prisutf_UI WINDOW-2 
PROCEDURE prisutf_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   str=
   "=====================================================================================".      
   RUN prisutfhuvud_UI.
   IF musz = FALSE THEN RUN prisutfsumma_UI.    
      
   ENABLE BRW_UT WITH FRAME FRAME-VINST.
   BRW_UT:HIDDEN = FALSE. 
   
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE totutfexcel_UI WINDOW-2 
PROCEDURE totutfexcel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF aoval = FALSE THEN DO:   
      ASSIGN
      startc = "A"
      slutc = "J"
      slutbredd = 8
      utnr[1] = 1 
      utnr[2] = 8 
      utnr[3] = 14
      utnr[4] = 23
      utnr[5] = 33
      utnr[6] = 42
      utnr[7] = 52
      utnr[8] = 61
      utnr[9] = 71
      utnr[9] = 81.
   END.
   ELSE DO:
      ASSIGN   
      startc = "A"
      slutc = "J"
      slutbredd = 8
      utnr[1] = 1 
      utnr[2] = 8 
      utnr[3] = 14
      utnr[4] = 55
      utnr[5] = 64
      utnr[6] = 74
      utnr[7] = 83
      utnr[8] = 93
      utnr[9] = 112
      utnr[10] = 122.  
   END.
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
      IF aoval = FALSE THEN DO:   
         IF SUBSTRING(tidut.UT,utnr[3],4) = "ARB." THEN DO:    /*Rubrikslut FÖRSTA */         
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
            LEAVE.
         END.
      END.
      ELSE DO:   
         IF SUBSTRING(tidut.UT,utnr[4],4) = "ARB." THEN DO:    /*Rubrikslut FÖRSTA */         
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
            LEAVE.
         END.
      END.
   END.   
   /*Poster*/
   FIND NEXT tidut NO-LOCK NO-ERROR.
   raknare = 1.
   REPEAT:
      IF aoval = FALSE THEN DO:   
         IF SUBSTRING(tidut.UT,utnr[4],7) = "KOSTNAD" THEN DO:    /*Rubrikslut FÖRSTA */         
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
            FIND NEXT tidut NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tidut THEN DO:
               LEAVE.
            END.
            NEXT.
         END.
      END.
      ELSE DO:   
         IF SUBSTRING(tidut.UT,utnr[5],7) = "KOSTNAD" THEN DO:    /*Rubrikslut FÖRSTA */         
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
            FIND NEXT tidut NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tidut THEN DO:
               LEAVE.
            END.
            NEXT.
         END.
      END.
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2). 
         musz = musz.
      END.
      ELSE IF SUBSTRING(tidut.UT,1,5) = "-----" THEN DO:        
         RUN understryk_UI (INPUT 4,INPUT 2). 
         musz = musz.
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
   RUN slutexcel_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE totutfhuvud_UI WINDOW-2 
PROCEDURE totutfhuvud_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*HUVUD*/ 
   DO TRANSACTION:         
      IF musz = TRUE THEN musz = musz.
      ELSE DO:
         IF aoval = FALSE THEN DO:
            inder = FALSE.                                                                                                   
            str = "======.=====.========.=========.========.=========.========.=========.=========". 
            str2= "-------------------------------------------------------------------------------".
         
            CREATE tidut. 
            SUBSTRING(tidut.UT,60) = STRING(TODAY) + " " + STRING(TIME,"HH:MM").
            CREATE tidut.
            CREATE tidut.             
            IF vallista = 11 THEN DO:
                                         
               tidut.UT = visaupp.TYP + "- DETALJERAD LISTA TID-ÖVERTID-TRAKTAMENTEN PERIOD".
            END.
            
            ELSE DO:
               tidut.UT = visaupp.TYP + "- TID-ÖVERTID-TRAKTAMENTEN PERIOD".    
            END.   
            IF RAD_PERIOD = 1 THEN DO: 
               SUBSTRING(tidut.UT,60) = STRING(YEAR(bdatum),"9999").
            END.
            IF RAD_PERIOD = 2 THEN DO:
               SUBSTRING(tidut.UT,60) = STRING(bdatum) + " - " + STRING(avdatum).     
            END.     
            CREATE tidut.  
            CREATE tidut.            
            SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gomrk) + " :".    
            IF utomr NE "ALLA" THEN DO:
               FIND FIRST omrtemp2 WHERE omrtemp2.OMRADE = utomr 
               USE-INDEX OMR NO-LOCK NO-ERROR.
               IF NOT AVAILABLE omrtemp2 THEN  SUBSTRING(tidut.UT,10) = "OMRADETAB SAKNAS".
               ELSE SUBSTRING(tidut.UT,10) = omrtemp2.NAMN.
            END.
            ELSE DO: 
               SUBSTRING(tidut.UT,10) = utomr.
            END.       
            CREATE tidut.
            CREATE tidut.                                                 
            ASSIGN                 
            SUBSTRING(tidut.UT,14) = "ARB."
            SUBSTRING(tidut.UT,23) = "ARBETS" 
            SUBSTRING(tidut.UT,33) = "ÖVER."  
            SUBSTRING(tidut.UT,42) = "ÖVERTID"   
            SUBSTRING(tidut.UT,52) = "RES."
            SUBSTRING(tidut.UT,61) = "TRAKT."                  
            SUBSTRING(tidut.UT,71) = "LÖNETILL.".         
                 
            CREATE tidut.      
            ASSIGN       
            SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok)   
            SUBSTRING(tidut.UT,8) = Guru.Konstanter:gdelnrk                                     
            SUBSTRING(tidut.UT,14) = "TIMMAR"             
            SUBSTRING(tidut.UT,23) = "KOSTNAD"         
            SUBSTRING(tidut.UT,33) = "TIMMAR"  
            SUBSTRING(tidut.UT,42) = "KOSTNAD"
            SUBSTRING(tidut.UT,52) = "TIMMAR"
            SUBSTRING(tidut.UT,61) = "KOSTNAD"
            SUBSTRING(tidut.UT,71) = "KOSTNAD".  
                                                  
            CREATE tidut.       
            SUBSTRING(tidut.UT,1) = str.             
         END. 
         ELSE DO:                                                                                                 
            str = "======.=====.========================================.========.=========.========.=========.========.=========.=========". 
            str2= "------------------------------------------------------------------------------------------------------------------------".         
            CREATE tidut. 
            SUBSTRING(tidut.UT,60) = STRING(TODAY) + " " + STRING(TIME,"HH:MM").
            CREATE tidut.
            CREATE tidut.
            IF vallista = 11 THEN DO:                                      
               tidut.UT = visaupp.TYP + "- DETALJERAD LISTA TID-ÖVERTID-TRAKTAMENTEN PERIOD". 
            END.
            ELSE DO:
               tidut.UT = visaupp.TYP + "- TID-ÖVERTID-TRAKTAMENTEN PERIOD".    
            END.   
            IF RAD_PERIOD = 1 THEN DO: 
               SUBSTRING(tidut.UT,60) = STRING(YEAR(bdatum),"9999").
            END.
            IF RAD_PERIOD = 2 THEN DO:
               SUBSTRING(tidut.UT,60) = STRING(bdatum) + " - " + STRING(avdatum).     
            END.     
            CREATE tidut.  
            CREATE tidut.            
            SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gomrk) + " :".    
            IF utomr NE "ALLA" THEN DO:
               FIND FIRST omrtemp2 WHERE omrtemp2.OMRADE = utomr 
               USE-INDEX OMR NO-LOCK NO-ERROR.
               IF NOT AVAILABLE omrtemp2 THEN  SUBSTRING(tidut.UT,10) = "OMRADETAB SAKNAS".
               ELSE SUBSTRING(tidut.UT,10) = omrtemp2.NAMN.
            END.
            ELSE DO: 
               SUBSTRING(tidut.UT,10) = utomr.
            END.       
            CREATE tidut.
            CREATE tidut.                                                 
            ASSIGN                 
            SUBSTRING(tidut.UT,55) = "ARB."
            SUBSTRING(tidut.UT,64) = "ARBETS" 
            SUBSTRING(tidut.UT,74) = "ÖVER."  
            SUBSTRING(tidut.UT,83) = "ÖVERTID"   
            SUBSTRING(tidut.UT,93) = "RES."
            SUBSTRING(tidut.UT,102) = "TRAKT."                  
            SUBSTRING(tidut.UT,112) = "LÖNETILL.".         
                
            CREATE tidut.      
            ASSIGN       
            SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok)   
            SUBSTRING(tidut.UT,8) = Guru.Konstanter:gdelnrk
            SUBSTRING(tidut.UT,14) = CAPS(Guru.Konstanter:gaonamnk)                                     
            SUBSTRING(tidut.UT,55) = "TIMMAR"             
            SUBSTRING(tidut.UT,64) = "KOSTNAD"         
            SUBSTRING(tidut.UT,74) = "TIMMAR"  
            SUBSTRING(tidut.UT,83) = "KOSTNAD"
            SUBSTRING(tidut.UT,93) = "TIMMAR"
            SUBSTRING(tidut.UT,102) = "KOSTNAD"
            SUBSTRING(tidut.UT,112) = "KOSTNAD".                                                   
            CREATE tidut.       
            SUBSTRING(tidut.UT,1) = str.             
         END.   
      END.   
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE totutfskrivut_UI WINDOW-2 
PROCEDURE totutfskrivut_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*UTSKRIFT*/
   RUN totutfhuvud_UI. 
   IF musz = FALSE THEN DO:
      RUN totutfsumma_UI. 
   END.             
   musz = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE totutfsumma_UI WINDOW-2 
PROCEDURE totutfsumma_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN totutfsum_UI IN omrlistutfapph (INPUT aoval,INPUT utomr,INPUT RAD_PERIOD,
                                       INPUT vallista,INPUT inder,INPUT bdatum,
                                       INPUT avdatum,INPUT str,INPUT str2,INPUT TABLE visaupp,
                                       INPUT-OUTPUT TABLE tidut).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE totutfut_UI WINDOW-2 
PROCEDURE totutfut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  /*UT*/ 
   FIND LAST tidut NO-LOCK NO-ERROR.     
   IF aoval = TRUE THEN RUN EKLOGL.P.  
   ELSE RUN EKLOGS.P. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE totutf_UI WINDOW-2 
PROCEDURE totutf_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  str=
"=====================================================================================".         
   RUN totutfhuvud_UI.             
   IF musz = FALSE THEN DO: 
      RUN totutfsumma_UI.
   END. 
   musz = FALSE.                   
   IF musz = TRUE THEN DO:
      musz = FALSE.      
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
      RETURN.
   END.                 
   ELSE DO:                         
      ENABLE BRW_UT WITH FRAME FRAME-VINST.
      BRW_UT:HIDDEN = FALSE.             
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
   skrivut = FALSE.                         
   FIND LAST tidut NO-LOCK NO-ERROR.     
   RUN EKLOGS.P. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

