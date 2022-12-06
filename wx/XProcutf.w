&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rt               PROGRESS
*/
&Scoped-define WINDOW-NAME    WINDOW-2
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
&Scoped-define NEW
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE SHARED VARIABLE aonummer LIKE AONRTAB.AONR NO-UNDO.
DEFINE SHARED VARIABLE delnummer LIKE AONRTAB.DELNR NO-UNDO.
DEFINE SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE SHARED VARIABLE utomr LIKE OMRADETAB.OMRADE NO-UNDO.   
DEFINE VARIABLE arrhjsum LIKE TIDREGITAB.BERANTAL NO-UNDO.    
DEFINE VARIABLE arrhjsumtid LIKE TIDREGITAB.BERANTAL NO-UNDO.  
DEFINE VARIABLE arrhjsumotid LIKE TIDREGITAB.BERANTAL NO-UNDO. 
DEFINE VARIABLE arrhjsumove LIKE TIDREGITAB.BERANTAL NO-UNDO.
DEFINE VARIABLE arrhjsumtra LIKE TIDREGITAB.BERANTAL NO-UNDO.   
DEFINE VARIABLE arrhjsumlon LIKE TIDREGITAB.BERANTAL NO-UNDO. 
DEFINE VARIABLE arrhjsumind LIKE TIDREGITAB.BERANTAL NO-UNDO. 
DEFINE VARIABLE arrhjsumikost LIKE TIDREGITAB.BERANTAL NO-UNDO.   
DEFINE VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO. 
DEFINE VARIABLE str2 AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE inder AS LOGICAL NO-UNDO.
DEFINE VARIABLE utomr2 LIKE OMRADETAB.OMRADE NO-UNDO. 
DEFINE VARIABLE utomr3 LIKE OMRADETAB.OMRADE NO-UNDO. 
DEFINE VARIABLE aoval AS LOGICAL NO-UNDO.
DEFINE VARIABLE vardelnr LIKE AONRTAB.DELNR NO-UNDO.
DEFINE VARIABLE varaonr LIKE AONRTAB.AONR NO-UNDO.
DEFINE VARIABLE berindvar AS DECIMAL NO-UNDO.
DEFINE TEMP-TABLE slutsum           
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD PRIS LIKE TIDREGITAB.PRIS     
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP 
   FIELD OMRADE LIKE SUMTID.GEOMRADE
   FIELD TIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"      
   FIELD OTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "OTIMMAR"         
   FIELD BELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "ARBKOSTNAD"           
   FIELD OBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "Ö-KOSTNAD"  
   FIELD OANTAL LIKE EKRAPPRESULT.EANTAL  LABEL "Ö-ANTAL"         
   FIELD TBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "T-KOSTNAD"
   FIELD TANTAL LIKE EKRAPPRESULT.EANTAL  LABEL "T-ANTAL"    
   FIELD LONKOST LIKE EKRAPPRESULT.EBELOPP LABEL "L-KOSTNAD" 
   FIELD AONR LIKE AONRTAB.AONR
   FIELD DELNR LIKE AONRTAB.DELNR 
   FIELD INDEREKT AS DECIMAL   
   FIELD INOMRADE LIKE SUMTID.OMRADE 
   FIELD MED AS LOGICAL 
   FIELD ORT LIKE AONRTAB.ORT          
   FIELD IKOST LIKE SUMTID.IKOSTNAD  
   INDEX OMR IS PRIMARY OMRADE AONR DELNR
   INDEX AONR AONR DELNR
   INDEX MED MED.  
DEFINE BUFFER slutsumbuff FOR slutsum.
DEFINE TEMP-TABLE slutsum1           
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD PRIS LIKE TIDREGITAB.PRIS     
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP 
   FIELD OMRADE LIKE SUMTID.GEOMRADE
   FIELD TIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"      
   FIELD OTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "OTIMMAR"         
   FIELD BELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "ARBKOSTNAD"           
   FIELD OBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "Ö-KOSTNAD"  
   FIELD OANTAL LIKE EKRAPPRESULT.EANTAL  LABEL "Ö-ANTAL"         
   FIELD TBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "T-KOSTNAD"
   FIELD TANTAL LIKE EKRAPPRESULT.EANTAL  LABEL "T-ANTAL"    
   FIELD LONKOST LIKE EKRAPPRESULT.EBELOPP LABEL "L-KOSTNAD"  
   FIELD AONR LIKE AONRTAB.AONR
   FIELD DELNR LIKE AONRTAB.DELNR
   FIELD INDEREKT AS DECIMAL  
   FIELD ORT LIKE AONRTAB.ORT            
   INDEX OMR IS PRIMARY OMRADE AONR DELNR.         
   
DEFINE TEMP-TABLE slutsum2           
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD PRIS LIKE TIDREGITAB.PRIS     
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP 
   FIELD OMRADE LIKE SUMTID.GEOMRADE
   FIELD TIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"      
   FIELD OTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "OTIMMAR"         
   FIELD BELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "ARBKOSTNAD"           
   FIELD OBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "Ö-KOSTNAD"  
   FIELD OANTAL LIKE EKRAPPRESULT.EANTAL  LABEL "Ö-ANTAL"         
   FIELD TBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "T-KOSTNAD"
   FIELD TANTAL LIKE EKRAPPRESULT.EANTAL  LABEL "T-ANTAL"    
   FIELD LONKOST LIKE EKRAPPRESULT.EBELOPP LABEL "L-KOSTNAD"  
   FIELD AONR LIKE AONRTAB.AONR
   FIELD DELNR LIKE AONRTAB.DELNR
   FIELD INDEREKT AS DECIMAL 
   FIELD ORT LIKE AONRTAB.ORT            
   INDEX OMR IS PRIMARY OMRADE.       
   
DEFINE TEMP-TABLE dagtemp
   FIELD OMRADE LIKE SUMTID.GEOMRADE
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP
   FIELD TIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"  
   FIELD OTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "OTIMMAR"         
   FIELD BELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "ARBKOSTNAD"           
   FIELD OBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "Ö-KOSTNAD"  
   FIELD TBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "T-KOSTNAD"
   FIELD LONKOST LIKE EKRAPPRESULT.EBELOPP LABEL "L-KOSTNAD" 
   FIELD AONR LIKE AONRTAB.AONR
   FIELD DELNR LIKE AONRTAB.DELNR 
   FIELD INOMRADE LIKE SUMTID.OMRADE   
   FIELD ORT LIKE AONRTAB.ORT       
   FIELD IKOST LIKE SUMTID.IKOSTNAD   
   INDEX OMR IS PRIMARY OMRADE.
   
DEFINE TEMP-TABLE restid  
   FIELD AONR LIKE AONRTAB.AONR
   FIELD DELNR LIKE AONRTAB.DELNR                     
   FIELD OMRADE LIKE OMRADETAB.OMRADE
   FIELD TIMMAR AS DECIMAL LABEL "RTIMMAR"                
   INDEX OMR IS PRIMARY OMRADE.              

DEFINE TEMP-TABLE restid2  
   FIELD AONR LIKE AONRTAB.AONR
   FIELD DELNR LIKE AONRTAB.DELNR                     
   FIELD OMRADE LIKE OMRADETAB.OMRADE
   FIELD TIMMAR AS DECIMAL LABEL "RTIMMAR"                
   INDEX OMR IS PRIMARY OMRADE. 
                
DEFINE TEMP-TABLE restid3  
   FIELD AONR LIKE AONRTAB.AONR
   FIELD DELNR LIKE AONRTAB.DELNR                     
   FIELD OMRADE LIKE OMRADETAB.OMRADE
   FIELD TIMMAR AS DECIMAL LABEL "RTIMMAR"                
   INDEX OMR IS PRIMARY OMRADE.       
   
DEFINE TEMP-TABLE restid4  
   FIELD AONR LIKE AONRTAB.AONR
   FIELD DELNR LIKE AONRTAB.DELNR                     
   FIELD OMRADE LIKE OMRADETAB.OMRADE
   FIELD TIMMAR AS DECIMAL LABEL "RTIMMAR"                
   INDEX OMR IS PRIMARY OMRADE.

DEFINE SHARED TEMP-TABLE omrtemp2
   FIELD OMRADE LIKE OMRADETAB.OMRADE
   FIELD NAMN LIKE OMRADETAB.NAMN
   INDEX OMR IS PRIMARY OMRADE
   INDEX OMRNAMN NAMN.          
DEFINE NEW SHARED TEMP-TABLE tidut
   FIELD UT AS CHARACTER FORMAT "X(132)".


   
DEFINE QUERY dagsumq FOR TIDREGITAB.   
DEFINE QUERY arsumq FOR SUMTID. 
DEFINE QUERY aonrq FOR AONRTAB.   
   
DEFINE {&NEW} SHARED VARIABLE RAD_PERIOD AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Visning per år", 1,
"Visning per period", 2
     SIZE 21.5 BY 2.5 BGCOLOR 8 NO-UNDO.  
     
DEFINE {&NEW} SHARED VARIABLE SEL_UPP AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "Arbetsorder-tid-övertid-traktamenten",
                "Tid-övertid-traktamenten / område", 
                "Det.lista Tid-övertid-trakt./område",
                "Arbetsorder-kostnadsreg.-kalkyl",
                "Arbetsorder-personal",
                "Arbetstidens fördelning",
                "Debiteringsgrad-område",
                "Personal-lönetillägg-personal",
                "Område-interna intäkter/kostnader"
                
     SIZE 39.5 BY 9.5
     BGCOLOR 8  NO-UNDO.     
     
/*{EGENBEN.I}*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse (alphabetically)                   */
&Scoped-define FRAME-NAME  FRAME-VINST
&Scoped-define BROWSE-NAME BRW_UT

/* Custom List Definitions                                              */
&Scoped-define LIST-1 
&Scoped-define LIST-2 
&Scoped-define LIST-3 

/* Definitions for BROWSE BRW_UT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_UT tidut.ut 
&Scoped-define OPEN-QUERY-BRW_UT OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT tidut
&Scoped-define TABLES-IN-QUERY-BRW_UT tidut 

/* Definitions for FRAME FRAME-VINST                                    */
&Scoped-define FIELDS-IN-QUERY-FRAME-VINST 
&Scoped-define ENABLED-FIELDS-IN-QUERY-FRAME-VINST 
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-VINST ~
    ~{&OPEN-QUERY-BRW_UT}

/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 12 BY 1.5.



DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 12 BY 1.5.

DEFINE RECTANGLE RECT-35
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 76.5 BY 2.5
     BGCOLOR 8 .


/* Query definitions                                                    */
DEFINE QUERY BRW_UT FOR tidut SCROLLING.

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UT QUERY BRW_UT NO-LOCK DISPLAY 
      tidut.ut
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 91 BY 19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST     
     BTN_AVS AT ROW 1.5 COL 14
     BTN_SKRIV AT ROW 1.5 COL 59
     BRW_UT AT ROW 4.5 COL 1.5
     RECT-35 AT ROW 1 COL 4.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.25 ROW 1.41
         SIZE 92 BY 23
         BGCOLOR 8 .

 

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Uppföljning"
         COLUMN             = 6
         ROW                = 1.5
         HEIGHT             = 24
         WIDTH              = 93
         MAX-HEIGHT         = 25
         MAX-WIDTH          = 93
         VIRTUAL-HEIGHT     = 25
         VIRTUAL-WIDTH      = 93
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-2
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR BROWSE BRW_UT IN FRAME FRAME-VINST
   NO-ENABLE                                                            */
ASSIGN 
       BRW_UT:HIDDEN  IN FRAME FRAME-VINST            = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "rt.tidut"
     _Options          = "NO-LOCK"
     _OrdList          = ""
     _FldNameList[1]   = rt.tidut.ut
     _Query            is OPENED
*/  /* BROWSE BRW_UT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-VINST
/* Query rebuild information for FRAME FRAME-VINST
     _TblList          = ""
     _Options          = "NO-LOCK KEEP-EMPTY"
     _OrdList          = ""
     _Query            is NOT OPENED
*/  /* FRAME FRAME-VINST */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-2
ON CHOOSE OF BTN_AVS IN FRAME FRAME-VINST /* Avsluta */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO: 
   RUN SKRIVVAL.W.       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:    
      RUN ut_UI.
      skrivut = FALSE.      
   END.
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
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
   {muswait.i}    
   FOR EACH tidut:
      DELETE tidut.
   END. 
   
str=
"=====================================================================================".         
   RUN huvud_UI.             
   IF musz = FALSE THEN DO:         
      RUN summa_UI.           
   END. 
   musz = FALSE.                   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      /*status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").*/
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
      LEAVE MAIN-BLOCK. 
   END.                 
   ELSE DO:                         
      IF skrivut = FALSE THEN DO:
         ENABLE BRW_UT WITH FRAME FRAME-VINST.
         BRW_UT:HIDDEN = FALSE.       
      END.
      ELSE DO:          
         RUN ut_UI.            
         RUN skrivut_UI.
         IF musz = TRUE THEN DO:
            musz = FALSE.            
         END.                          
         /*status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").*/
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
         LEAVE MAIN-BLOCK. 
      END.
   END.
   RUN enable_UI.    
   inder = FALSE.    
   {musarrow.i}                                                
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-2 _DEFAULT-DISABLE
PROCEDURE disable_UI :
/* --------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
   -------------------------------------------------------------------- */
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U THEN DELETE WIDGET WINDOW-2.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-2 _DEFAULT-ENABLE
PROCEDURE enable_UI :
/* --------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
   -------------------------------------------------------------------- */
  ENABLE RECT-35 BTN_AVS BTN_SKRIV 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VINST}
  VIEW WINDOW-2.
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
      IF musz = TRUE THEN musz = musz.
      ELSE DO:
         IF aoval = FALSE THEN DO:   
            IF Guru.Konstanter:globforetag = "NORD" OR 
            Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "ESAN" OR Guru.Konstanter:globforetag = "ESMA" OR
            Guru.Konstanter:globforetag = "ETA" THEN DO:
               inder = TRUE.           
            END. 
            IF inder = TRUE THEN DO:                                                                
               str = "======.=====.========.=========.========.=========.========.=========.=========.========".
               str2= "----------------------------------------------------------------------------------------".                 
            END.
            ELSE DO:                                                                                    
               str = "======.=====.========.=========.========.=========.========.=========.=========". 
               str2= "-------------------------------------------------------------------------------".
            END.
            CREATE tidut. 
            SUBSTRING(tidut.UT,60) = STRING(TODAY) + " " + STRING(TIME,"HH:MM").
            CREATE tidut.
            CREATE tidut.             
            IF SEL_UPP = "Det.lista Tid-övertid-trakt./ " + gomrk THEN DO:                          
               tidut.UT = "DETALJERAD LISTA TID-ÖVERTID-TRAKTAMENTEN PERIOD".
            END.
            ELSE DO:
               tidut.UT = "TID-ÖVERTID-TRAKTAMENTEN PERIOD".    
            END.   
            IF RAD_PERIOD = 1 THEN DO: 
               SUBSTRING(tidut.UT,50) = STRING(YEAR(bdatum),"9999").
            END.
            IF RAD_PERIOD = 2 THEN DO:
               SUBSTRING(tidut.UT,50) = STRING(bdatum) + " - " + STRING(avdatum).     
            END.     
            CREATE tidut.  
            CREATE tidut.            
            SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gomrk) + " :".    
            IF utomr NE "ALLA" THEN DO:
               FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = utomr 
               USE-INDEX OMR NO-LOCK NO-ERROR.
               IF NOT AVAILABLE OMRADETAB THEN  SUBSTRING(tidut.UT,10) = "OMRADETAB SAKNAS".
               ELSE SUBSTRING(tidut.UT,10) = OMRADETAB.NAMN.
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
            IF inder = TRUE THEN             
            ASSIGN SUBSTRING(tidut.UT,81) = "INDIREKT".     
            CREATE tidut.      
            ASSIGN       
            SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok)   
            SUBSTRING(tidut.UT,8) = "DELNR"                                     
            SUBSTRING(tidut.UT,14) = "TIMMAR"             
            SUBSTRING(tidut.UT,23) = "KOSTNAD"         
            SUBSTRING(tidut.UT,33) = "TIMMAR"  
            SUBSTRING(tidut.UT,42) = "KOSTNAD"
            SUBSTRING(tidut.UT,52) = "TIMMAR"
            SUBSTRING(tidut.UT,61) = "KOSTNAD"
            SUBSTRING(tidut.UT,71) = "KOSTNAD".  
            IF inder = TRUE THEN ASSIGN SUBSTRING(tidut.UT,81) = "KOSTNAD".                                      
            CREATE tidut.       
            SUBSTRING(tidut.UT,1) = str.             
         END. 
         ELSE DO: 
            IF Guru.Konstanter:globforetag = "NORD" OR 
            Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "ESAN" OR Guru.Konstanter:globforetag = "ESMA" OR
            Guru.Konstanter:globforetag = "ETA" THEN DO:
               inder = TRUE.           
            END. 
            IF inder = TRUE THEN DO:                                                                
               str = "======.=====.========================================.========.=========.========.=========.========.=========.=========.========".
               str2= "---------------------------------------------------------------------------------------------------------------------------------".                 
            END.
            ELSE DO:                                                                                    
               str = "======.=====.========================================.========.=========.========.=========.========.=========.=========". 
               str2= "------------------------------------------------------------------------------------------------------------------------".
            END.
            CREATE tidut. 
            SUBSTRING(tidut.UT,60) = STRING(TODAY) + " " + STRING(TIME,"HH:MM").
            CREATE tidut.
            CREATE tidut.
            IF SEL_UPP = "Det.lista Tid-övertid-trakt./ " + gomrk THEN DO:                                      
               tidut.UT = "DETALJERAD LISTA TID-ÖVERTID-TRAKTAMENTEN PERIOD". 
            END.
            ELSE DO:
               tidut.UT = "TID-ÖVERTID-TRAKTAMENTEN PERIOD".    
            END.   
            IF RAD_PERIOD = 1 THEN DO: 
               SUBSTRING(tidut.UT,50) = STRING(YEAR(bdatum),"9999").
            END.
            IF RAD_PERIOD = 2 THEN DO:
               SUBSTRING(tidut.UT,50) = STRING(bdatum) + " - " + STRING(avdatum).     
            END.     
            CREATE tidut.  
            CREATE tidut.            
            SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gomrk) + " :".    
            IF utomr NE "ALLA" THEN DO:
               FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = utomr 
               USE-INDEX OMR NO-LOCK NO-ERROR.
               IF NOT AVAILABLE OMRADETAB THEN  SUBSTRING(tidut.UT,10) = "OMRADETAB SAKNAS".
               ELSE SUBSTRING(tidut.UT,10) = OMRADETAB.NAMN.
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
            IF inder = TRUE THEN             
            ASSIGN SUBSTRING(tidut.UT,122) = "INDIREKT".     
            CREATE tidut.      
            ASSIGN       
            SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok)   
            SUBSTRING(tidut.UT,8) = "DELNR" 
            SUBSTRING(tidut.UT,14) = "ORT/BENÄMNING"                                      
            SUBSTRING(tidut.UT,55) = "TIMMAR"             
            SUBSTRING(tidut.UT,64) = "KOSTNAD"         
            SUBSTRING(tidut.UT,74) = "TIMMAR"  
            SUBSTRING(tidut.UT,83) = "KOSTNAD"
            SUBSTRING(tidut.UT,93) = "TIMMAR"
            SUBSTRING(tidut.UT,102) = "KOSTNAD"
            SUBSTRING(tidut.UT,112) = "KOSTNAD".  
            IF inder = TRUE THEN ASSIGN SUBSTRING(tidut.UT,122) = "KOSTNAD".                                      
            CREATE tidut.       
            SUBSTRING(tidut.UT,1) = str.             
         END.   
      END.   
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE summa_UI WINDOW-2 
PROCEDURE summa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   IF musz = TRUE THEN RETURN.
   FOR EACH slutsum:
      DELETE slutsum.
   END. 
   FOR EACH slutsum1:
      DELETE slutsum1.
   END.
   FOR EACH slutsum2:
      DELETE slutsum2.
   END.
   FOR EACH dagtemp:
      DELETE dagtemp.
   END.  
   FOR EACH restid:
      DELETE restid.
   END.  
   FOR EACH restid2:
      DELETE restid2.
   END.  
   FOR EACH restid3:
      DELETE restid3.
   END.  
   FOR EACH restid4:
      DELETE restid4.
   END.      
   ASSIGN   
   arrhjsum = 0   
   arrhjsumtid = 0  
   arrhjsumotid = 0    
   arrhjsumove = 0    
   arrhjsumtra = 0       
   arrhjsumlon = 0.   
   IF utomr NE "ALLA" THEN DO:      
      IF RAD_PERIOD = 1 THEN DO:                                       
         OPEN QUERY arsumq FOR EACH SUMTID 
         WHERE SUMTID.DATUM = bdatum AND SUMTID.GEOMRADE = utomr         
         USE-INDEX GEORGAONR NO-LOCK. 
         GET FIRST arsumq NO-LOCK.
         DO WHILE AVAILABLE(SUMTID) TRANSACTION:                                        
            IF SUMTID.DATUM >= 01/01/99 THEN berindvar = 1.15.
            ELSE IF SUMTID.DATUM < 01/01/99 THEN berindvar = 0.70.
            IF SUMTID.PRISTYP = "RESTID..." THEN DO: 
               CREATE restid.
               ASSIGN 
               restid.AONR = SUMTID.AONR
               restid.DELNR = SUMTID.DELNR         
               restid.OMRADE = SUMTID.GEOMRADE
               restid.TIMMAR = restid.TIMMAR + SUMTID.TIMMAR. 
               CREATE dagtemp.
               ASSIGN  
               dagtemp.AONR = SUMTID.AONR
               dagtemp.DELNR = SUMTID.DELNR 
               dagtemp.ORT = SUMTID.ORT     
               dagtemp.INOMRADE = SUMTID.OMRADE  
               dagtemp.OMRADE = SUMTID.GEOMRADE
               dagtemp.PRISTYP = SUMTID.PRISTYP               
               dagtemp.OTIMMAR = SUMTID.OTIMMAR 
               dagtemp.BELOPP = SUMTID.BELOPP 
               dagtemp.OBELOPP = SUMTID.OBELOPP 
               dagtemp.TBELOPP = SUMTID.TBELOPP
               dagtemp.LONKOST = SUMTID.LONKOST
               dagtemp.IKOST = SUMTID.IKOSTNAD * berindvar.             
            END.
            ELSE DO:
               IF SUMTID.DATUM >= 01/01/99 THEN berindvar = 1.15.
               ELSE IF SUMTID.DATUM < 01/01/99 THEN berindvar = 0.70.
               CREATE dagtemp.
               ASSIGN  
               dagtemp.AONR = SUMTID.AONR
               dagtemp.DELNR = SUMTID.DELNR 
               dagtemp.ORT = SUMTID.ORT     
               dagtemp.INOMRADE = SUMTID.OMRADE  
               dagtemp.OMRADE = SUMTID.GEOMRADE
               dagtemp.PRISTYP = SUMTID.PRISTYP
               dagtemp.TIMMAR = SUMTID.TIMMAR
               dagtemp.OTIMMAR = SUMTID.OTIMMAR 
               dagtemp.BELOPP = SUMTID.BELOPP 
               dagtemp.OBELOPP = SUMTID.OBELOPP 
               dagtemp.TBELOPP = SUMTID.TBELOPP
               dagtemp.LONKOST = SUMTID.LONKOST
               dagtemp.IKOST = SUMTID.IKOSTNAD * berindvar.
            END.
            GET NEXT arsumq NO-LOCK. 
         END.
      END. 
      IF RAD_PERIOD = 2 THEN DO:
         OPEN QUERY dagsq FOR EACH omrtemp2,
         EACH PERSONALTAB WHERE PERSONALTAB.OMRADE = omrtemp2.OMRADE NO-LOCK,
         EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM >= bdatum AND TIDREGITAB.DATUM <= avdatum AND
         TIDREGITAB.TIDLOG = TRUE USE-INDEX PVKORD NO-LOCK.
         GET FIRST dagsumq NO-LOCK.
         DO WHILE AVAILABLE(TIDREGITAB) TRANSACTION:            
            IF TIDREGITAB.PRISTYP = "RESTID..." THEN musz = musz. 
            ELSE IF TIDREGITAB.OANT1 > 0 THEN musz = musz. 
            ELSE IF date(TIDREGITAB.VECKOKORD,2,8) > 
            ELSE DO:               
               CREATE dagtemp.
               ASSIGN    
               dagtemp.AONR = TIDREGITAB.AONR
               dagtemp.DELNR = TIDREGITAB.DELNR 
               dagtemp.PRISTYP = TIDREGITAB.PRISTYP
               dagtemp.TIMMAR = TIDREGITAB.TOTALT.
            END.
            GET NEXT dagsumq NO-LOCK. 
         END.   
      END.      
   END.
   ELSE DO:
      IF RAD_PERIOD = 1 THEN DO:                       
         OPEN QUERY arsumq FOR EACH SUMTID 
         WHERE SUMTID.DATUM = bdatum 
         USE-INDEX GEORGAONR NO-LOCK. 
         GET FIRST arsumq NO-LOCK.
         DO WHILE AVAILABLE(SUMTID) TRANSACTION:                           
            IF SUMTID.PRISTYP = "RESTID..." THEN DO: 
               IF SUMTID.DATUM >= 01/01/99 THEN berindvar = 1.15.
               ELSE IF SUMTID.DATUM < 01/01/99 THEN berindvar = 0.70.
               CREATE restid.
               ASSIGN   
               restid.AONR = SUMTID.AONR
               restid.DELNR = SUMTID.DELNR              
               restid.OMRADE = SUMTID.GEOMRADE
               restid.TIMMAR = restid.TIMMAR + SUMTID.TIMMAR.
               CREATE dagtemp.
               ASSIGN  
               dagtemp.AONR = SUMTID.AONR
               dagtemp.DELNR = SUMTID.DELNR 
               dagtemp.ORT = SUMTID.ORT     
               dagtemp.INOMRADE = SUMTID.OMRADE             
               dagtemp.OMRADE = SUMTID.GEOMRADE
               dagtemp.PRISTYP = SUMTID.PRISTYP               
               dagtemp.OTIMMAR = SUMTID.OTIMMAR 
               dagtemp.BELOPP = SUMTID.BELOPP 
               dagtemp.OBELOPP = SUMTID.OBELOPP 
               dagtemp.TBELOPP = SUMTID.TBELOPP
               dagtemp.LONKOST = SUMTID.LONKOST
               dagtemp.IKOST = SUMTID.IKOSTNAD * berindvar.
            END.
            ELSE DO:
               IF SUMTID.DATUM >= 01/01/99 THEN berindvar = 1.15.
               ELSE IF SUMTID.DATUM < 01/01/99 THEN berindvar = 0.70.
               CREATE dagtemp.
               ASSIGN  
               dagtemp.AONR = SUMTID.AONR
               dagtemp.DELNR = SUMTID.DELNR 
               dagtemp.ORT = SUMTID.ORT     
               dagtemp.INOMRADE = SUMTID.OMRADE             
               dagtemp.OMRADE = SUMTID.GEOMRADE
               dagtemp.PRISTYP = SUMTID.PRISTYP
               dagtemp.TIMMAR = SUMTID.TIMMAR
               dagtemp.OTIMMAR = SUMTID.OTIMMAR 
               dagtemp.BELOPP = SUMTID.BELOPP 
               dagtemp.OBELOPP = SUMTID.OBELOPP 
               dagtemp.TBELOPP = SUMTID.TBELOPP
               dagtemp.LONKOST = SUMTID.LONKOST
               dagtemp.IKOST = SUMTID.IKOSTNAD * berindvar.
            END.
            GET NEXT arsumq NO-LOCK. 
         END.
      END. 
      IF RAD_PERIOD = 2 THEN DO:
         OPEN QUERY dagsumq FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM >= bdatum AND
         TIDREGITAB.DATUM <= avdatum AND TIDREGITAB.TIDLOG = TRUE
         USE-INDEX PVKORD NO-LOCK.
         GET FIRST dagsumq NO-LOCK.
         DO WHILE AVAILABLE(TIDREGITAB) TRANSACTION:            
            IF TIDREGITAB.PRISTYP = "RESTID..." THEN musz = musz.
            ELSE IF TIDREGITAB.OANT1 > 0 THEN musz = musz.
            ELSE DO:
               CREATE dagtemp.
               ASSIGN         
               dagtemp.AONR = TIDREGITAB.AONR
               dagtemp.DELNR = TIDREGITAB.DELNR 
               dagtemp.PRISTYP = TIDREGITAB.PRISTYP
               dagtemp.TIMMAR = TIDREGITAB.TOTALT.                          
            END.
            GET NEXT dagsumq NO-LOCK. 
         END.   
      END.       
   END.        
   FOR EACH dagtemp BREAK BY dagtemp.INOMRADE BY dagtemp.OMRADE BY dagtemp.AONR BY dagtemp.DELNR:           
      ACCUMULATE 
      dagtemp.BELOPP (TOTAL BY dagtemp.INOMRADE BY dagtemp.OMRADE BY dagtemp.AONR BY dagtemp.DELNR). 
      ACCUMULATE 
      dagtemp.TIMMAR (TOTAL BY dagtemp.INOMRADE BY dagtemp.OMRADE BY dagtemp.AONR BY dagtemp.DELNR). 
      ACCUMULATE 
      dagtemp.OTIMMAR (TOTAL BY dagtemp.INOMRADE BY dagtemp.OMRADE BY dagtemp.AONR BY dagtemp.DELNR). 
      ACCUMULATE 
      dagtemp.OBELOPP (TOTAL BY dagtemp.INOMRADE BY dagtemp.OMRADE BY dagtemp.AONR BY dagtemp.DELNR).  
      ACCUMULATE 
      dagtemp.TBELOPP (TOTAL BY dagtemp.INOMRADE BY dagtemp.OMRADE BY dagtemp.AONR BY dagtemp.DELNR). 
      ACCUMULATE 
      dagtemp.LONKOST (TOTAL BY dagtemp.INOMRADE BY dagtemp.OMRADE BY dagtemp.AONR BY dagtemp.DELNR).
      ACCUMULATE 
      dagtemp.IKOST (TOTAL BY dagtemp.INOMRADE BY dagtemp.OMRADE BY dagtemp.AONR BY dagtemp.DELNR).
      IF LAST-OF(dagtemp.DELNR) THEN DO TRANSACTION:         
         CREATE slutsum.
         ASSIGN 
         slutsum.MED = FALSE
         slutsum.AONR = dagtemp.AONR
         slutsum.DELNR = dagtemp.DELNR  
         slutsum.ORT = dagtemp.ORT               
         slutsum.INOMRADE = dagtemp.INOMRADE   
         slutsum.OMRADE = dagtemp.OMRADE      
         slutsum.BELOPP = (ACCUM TOTAL dagtemp.BELOPP) - arrhjsum                       
         slutsum.TIMMAR = (ACCUM TOTAL dagtemp.TIMMAR) - arrhjsumtid 
         slutsum.OTIMMAR = (ACCUM TOTAL dagtemp.OTIMMAR) - arrhjsumotid
         slutsum.OBELOPP = (ACCUM TOTAL dagtemp.OBELOPP) - arrhjsumove    
         slutsum.TBELOPP = (ACCUM TOTAL dagtemp.TBELOPP) - arrhjsumtra 
         slutsum.LONKOST = (ACCUM TOTAL dagtemp.LONKOST) - arrhjsumlon
         slutsum.IKOST = (ACCUM TOTAL dagtemp.IKOST) - arrhjsumikost.            
         arrhjsum = ACCUM TOTAL dagtemp.BELOPP.  
         arrhjsumtid = ACCUM TOTAL dagtemp.TIMMAR. 
         arrhjsumotid = ACCUM TOTAL dagtemp.OTIMMAR.
         arrhjsumove = ACCUM TOTAL dagtemp.OBELOPP.  
         arrhjsumtra = ACCUM TOTAL dagtemp.TBELOPP.       
         arrhjsumlon = ACCUM TOTAL dagtemp.LONKOST. 
         arrhjsumikost = ACCUM TOTAL dagtemp.IKOST.               
      END.                 
   END.         
   IF inder = TRUE THEN DO:           
      ASSIGN
      vardelnr = 0
      varaonr = "".      
      FOR EACH slutsum USE-INDEX AONR:                     
         IF slutsum.INOMRADE NE slutsum.OMRADE THEN NEXT.
         IF vardelnr = slutsum.DELNR AND varaonr = slutsum.AONR THEN DO:
            musz = musz.  
            IF AVAILABLE AONRKONTKOD THEN DO:
               IF TRIM(AONRKONTKOD.K3) = "0" THEN NEXT.               
            END.   
         END.
         ELSE DO:                                           
            ASSIGN
            vardelnr = slutsum.DELNR 
            varaonr = slutsum.AONR.
            FIND FIRST AONRKONTKOD WHERE AONRKONTKOD.AONR = slutsum.AONR AND 
            AONRKONTKOD.DELNR = slutsum.DELNR USE-INDEX AONRKONT NO-LOCK NO-ERROR. 
            IF AVAILABLE AONRKONTKOD THEN DO:
               IF TRIM(AONRKONTKOD.K3) = "0" THEN NEXT.               
            END.    
         END.        
         ASSIGN         
         slutsum.INDEREKT = slutsum.IKOST.                         
      END. 
   END.                   
   ASSIGN   
   arrhjsum = 0   
   arrhjsumtid = 0  
   arrhjsumotid = 0    
   arrhjsumove = 0    
   arrhjsumtra = 0       
   arrhjsumlon = 0
   arrhjsumind = 0. 
   FOR EACH slutsum WHERE slutsum.MED = FALSE 
   BREAK BY slutsum.OMRADE BY slutsum.AONR BY slutsum.DELNR:           
      ACCUMULATE 
      slutsum.BELOPP (TOTAL BY slutsum.OMRADE BY slutsum.AONR BY slutsum.DELNR). 
      ACCUMULATE 
      slutsum.TIMMAR (TOTAL BY slutsum.OMRADE BY slutsum.AONR BY slutsum.DELNR). 
      ACCUMULATE 
      slutsum.OTIMMAR (TOTAL BY slutsum.OMRADE BY slutsum.AONR BY slutsum.DELNR). 
      ACCUMULATE 
      slutsum.OBELOPP (TOTAL BY slutsum.OMRADE BY slutsum.AONR BY slutsum.DELNR).  
      ACCUMULATE 
      slutsum.TBELOPP (TOTAL BY slutsum.OMRADE BY slutsum.AONR BY slutsum.DELNR). 
      ACCUMULATE 
      slutsum.LONKOST (TOTAL BY slutsum.OMRADE BY slutsum.AONR BY slutsum.DELNR). 
      ACCUMULATE 
      slutsum.INDEREKT (TOTAL BY slutsum.OMRADE BY slutsum.AONR BY slutsum.DELNR).
      IF LAST-OF(slutsum.DELNR) THEN DO TRANSACTION:
         CREATE slutsumbuff.
         ASSIGN        
         slutsumbuff.MED = TRUE 
         slutsumbuff.OMRADE = slutsum.OMRADE 
         slutsumbuff.AONR = slutsum.AONR
         slutsumbuff.DELNR = slutsum.DELNR  
         slutsumbuff.ORT = slutsum.ORT                                        
         slutsumbuff.BELOPP = (ACCUM TOTAL slutsum.BELOPP) - arrhjsum                       
         slutsumbuff.TIMMAR = (ACCUM TOTAL slutsum.TIMMAR) - arrhjsumtid 
         slutsumbuff.OTIMMAR = (ACCUM TOTAL slutsum.OTIMMAR) - arrhjsumotid
         slutsumbuff.OBELOPP = (ACCUM TOTAL slutsum.OBELOPP) - arrhjsumove    
         slutsumbuff.TBELOPP = (ACCUM TOTAL slutsum.TBELOPP) - arrhjsumtra 
         slutsumbuff.LONKOST = (ACCUM TOTAL slutsum.LONKOST) - arrhjsumlon
         slutsumbuff.INDEREKT = (ACCUM TOTAL slutsum.INDEREKT) - arrhjsumind.             
         arrhjsum = ACCUM TOTAL slutsum.BELOPP.  
         arrhjsumtid = ACCUM TOTAL slutsum.TIMMAR. 
         arrhjsumotid = ACCUM TOTAL slutsum.OTIMMAR.
         arrhjsumove = ACCUM TOTAL slutsum.OBELOPP.  
         arrhjsumtra = ACCUM TOTAL slutsum.TBELOPP.       
         arrhjsumlon = ACCUM TOTAL slutsum.LONKOST.  
         arrhjsumind = ACCUM TOTAL slutsum.INDEREKT.             
      END.                 
   END.              
   FOR EACH slutsum WHERE slutsum.MED = FALSE USE-INDEX MED:
      DELETE slutsum.
   END.     
   ASSIGN   
   arrhjsum = 0   
   arrhjsumtid = 0  
   arrhjsumotid = 0    
   arrhjsumove = 0    
   arrhjsumtra = 0       
   arrhjsumlon = 0
   arrhjsumind = 0.      
   FOR EACH slutsum BREAK BY slutsum.OMRADE:           
      ACCUMULATE 
      slutsum.BELOPP (TOTAL BY slutsum.OMRADE). 
      ACCUMULATE 
      slutsum.TIMMAR (TOTAL BY slutsum.OMRADE). 
      ACCUMULATE 
      slutsum.OTIMMAR (TOTAL BY slutsum.OMRADE). 
      ACCUMULATE 
      slutsum.OBELOPP (TOTAL BY slutsum.OMRADE).  
      ACCUMULATE 
      slutsum.TBELOPP (TOTAL BY slutsum.OMRADE). 
      ACCUMULATE 
      slutsum.LONKOST (TOTAL BY slutsum.OMRADE). 
      ACCUMULATE 
      slutsum.INDEREKT (TOTAL BY slutsum.OMRADE).
      IF LAST-OF(slutsum.OMRADE) THEN DO TRANSACTION:
         CREATE slutsum1.
         ASSIGN          
         slutsum1.OMRADE = slutsum.OMRADE      
         slutsum1.BELOPP = (ACCUM TOTAL slutsum.BELOPP) - arrhjsum                       
         slutsum1.TIMMAR = (ACCUM TOTAL slutsum.TIMMAR) - arrhjsumtid 
         slutsum1.OTIMMAR = (ACCUM TOTAL slutsum.OTIMMAR) - arrhjsumotid
         slutsum1.OBELOPP = (ACCUM TOTAL slutsum.OBELOPP) - arrhjsumove    
         slutsum1.TBELOPP = (ACCUM TOTAL slutsum.TBELOPP) - arrhjsumtra 
         slutsum1.LONKOST = (ACCUM TOTAL slutsum.LONKOST) - arrhjsumlon
         slutsum1.INDEREKT = (ACCUM TOTAL slutsum.INDEREKT) - arrhjsumind.             
         arrhjsum = ACCUM TOTAL slutsum.BELOPP.  
         arrhjsumtid = ACCUM TOTAL slutsum.TIMMAR. 
         arrhjsumotid = ACCUM TOTAL slutsum.OTIMMAR.
         arrhjsumove = ACCUM TOTAL slutsum.OBELOPP.  
         arrhjsumtra = ACCUM TOTAL slutsum.TBELOPP.       
         arrhjsumlon = ACCUM TOTAL slutsum.LONKOST.  
         arrhjsumind = ACCUM TOTAL slutsum.INDEREKT.             
      END.                 
   END.         
   CREATE slutsum2.        
   ASSIGN        
   slutsum2.BELOPP = arrhjsum                       
   slutsum2.TIMMAR = arrhjsumtid 
   slutsum2.OTIMMAR = arrhjsumotid
   slutsum2.OBELOPP = arrhjsumove    
   slutsum2.TBELOPP = arrhjsumtra 
   slutsum2.LONKOST = arrhjsumlon
   slutsum2.INDEREKT = arrhjsumind.
                                        
   /*SUMMERING AV RESTID*/
   ASSIGN      
   arrhjsum = 0   
   arrhjsumtid = 0.
   /* TOTALT PÅ VARJE " + CAPS(Guru.Konstanter:gaol)*/  
   FOR EACH restid BREAK BY restid.OMRADE BY restid.AONR BY restid.DELNR:           
      ACCUMULATE 
      restid.TIMMAR (TOTAL BY restid.OMRADE BY restid.AONR BY restid.DELNR).       
      IF LAST-OF(restid.DELNR) THEN DO TRANSACTION:
         CREATE restid2.
         ASSIGN 
         restid2.AONR = restid.AONR
         restid2.DELNR = restid.DELNR
         restid2.OMRADE = restid.OMRADE                              
         restid2.TIMMAR = (ACCUM TOTAL restid.TIMMAR) - arrhjsumtid          
         arrhjsumtid = ACCUM TOTAL restid.TIMMAR.              
      END.                 
   END.  
   /* TOTALT PÅ VARJE " + CAPS(Guru.Konstanter:gomrl)*/ 
   ASSIGN      
   arrhjsumtid = 0.  
   FOR EACH restid2 BREAK BY restid2.OMRADE:           
      ACCUMULATE 
      restid2.TIMMAR (TOTAL BY restid2.OMRADE).       
      IF LAST-OF(restid2.OMRADE) THEN DO TRANSACTION:
         CREATE restid3.
         ASSIGN 
         restid3.AONR = restid2.AONR
         restid3.DELNR = restid2.DELNR
         restid3.OMRADE = restid2.OMRADE                              
         restid3.TIMMAR = (ACCUM TOTAL restid2.TIMMAR) - arrhjsumtid          
         arrhjsumtid = ACCUM TOTAL restid2.TIMMAR.              
      END.                 
   END. 
   /* TOTALT PÅ ALLA " + CAPS(Guru.Konstanter:gomrl)*/ 
   CREATE restid4.
   ASSIGN
   restid4.TIMMAR = arrhjsumtid. 
   /*SLUT SUMMERING AV RESTID*/
                                                            
   IF utomr = "ALLA" THEN DO:
      ASSIGN   
      arrhjsum = 0   
      arrhjsumtid = 0  
      arrhjsumotid = 0    
      arrhjsumove = 0    
      arrhjsumtra = 0       
      arrhjsumlon = 0
      arrhjsumind = 0.         
      CREATE tidut. 
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = "SUMMA SAMTLIGA " + CAPS(Guru.Konstanter:gomrl).               
      CREATE tidut.  
      FOR EACH restid4 NO-LOCK:                                 
         ASSIGN                                                       
         arrhjsumotid = arrhjsumotid + restid4.TIMMAR.         
      END.
      IF aoval = FALSE THEN DO:                                                                   
         FOR EACH slutsum2 USE-INDEX OMR NO-LOCK:                                                 
            CREATE tidut.                                 
            ASSIGN                                                                     
            SUBSTRING(tidut.UT,14) = STRING(slutsum2.TIMMAR,">>>>>>99")   
            SUBSTRING(tidut.UT,23) = STRING(slutsum2.BELOPP,">>>>>>>99")  
            SUBSTRING(tidut.UT,33) = STRING(slutsum2.OTIMMAR,">>>>>>99")   
            SUBSTRING(tidut.UT,42) = STRING(slutsum2.OBELOPP,">>>>>>>99")
            SUBSTRING(tidut.UT,52) = STRING(arrhjsumotid,">>>>>>99")
            SUBSTRING(tidut.UT,61) = STRING(slutsum2.TBELOPP,">>>>>>>99")
            SUBSTRING(tidut.UT,71) = STRING(slutsum2.LONKOST,"->>>>>>99").                           
            ASSIGN           
            arrhjsum =                                
            arrhjsum + slutsum2.BELOPP + slutsum2.OBELOPP + slutsum2.TBELOPP + 
            slutsum2.LONKOST
            arrhjsumtid = arrhjsumtid + slutsum2.TIMMAR + slutsum2.OTIMMAR
            arrhjsumind = arrhjsumind + slutsum2.INDEREKT.    
            IF inder = TRUE THEN DO:
                SUBSTRING(tidut.UT,81) = STRING(slutsum2.INDEREKT,">>>>>>99").
            END. 
         END.                                                    
      END.
      ELSE DO:
         FOR EACH slutsum2 USE-INDEX OMR NO-LOCK:                                                 
            CREATE tidut.                                 
            ASSIGN                                                                     
            SUBSTRING(tidut.UT,55) = STRING(slutsum2.TIMMAR,">>>>>>99")   
            SUBSTRING(tidut.UT,64) = STRING(slutsum2.BELOPP,">>>>>>>99")  
            SUBSTRING(tidut.UT,74) = STRING(slutsum2.OTIMMAR,">>>>>>99")   
            SUBSTRING(tidut.UT,83) = STRING(slutsum2.OBELOPP,">>>>>>>99")
            SUBSTRING(tidut.UT,93) = STRING(arrhjsumotid,">>>>>>99")
            SUBSTRING(tidut.UT,102) = STRING(slutsum2.TBELOPP,">>>>>>>99")
            SUBSTRING(tidut.UT,112) = STRING(slutsum2.LONKOST,"->>>>>>99").                           
            ASSIGN           
            arrhjsum =                                
            arrhjsum + slutsum2.BELOPP + slutsum2.OBELOPP + slutsum2.TBELOPP + slutsum2.LONKOST
            arrhjsumtid = arrhjsumtid + slutsum2.TIMMAR + slutsum2.OTIMMAR
            arrhjsumind = arrhjsumind + slutsum2.INDEREKT.    
            IF inder = TRUE THEN DO:
                SUBSTRING(tidut.UT,122) = STRING(slutsum2.INDEREKT,">>>>>>99").
            END. 
         END.                       
      END.  
      CREATE tidut.
      CREATE tidut.      
      CREATE tidut.   
      ASSIGN tidut.UT = "TOTALT".        
      CREATE tidut.  
      CREATE tidut.    
      ASSIGN                   
      SUBSTRING(tidut.UT,1) = "TOTALT ANTAL TIMMAR :"    
      SUBSTRING(tidut.UT,23) = STRING(arrhjsumtid,">>>>>>99.99").
      CREATE tidut.
      ASSIGN                   
      SUBSTRING(tidut.UT,1) = "TOTAL KOSTNAD       :"    
      SUBSTRING(tidut.UT,23) = STRING(arrhjsum + arrhjsumind,">>>>>>99.99").
      CREATE tidut.   
      CREATE tidut. 
      ASSIGN tidut.UT = str2.    
   END.           
   musz = TRUE.                      
   FOR EACH slutsum1 USE-INDEX OMR NO-LOCK:
      ASSIGN   
      arrhjsum = 0   
      arrhjsumtid = 0  
      arrhjsumotid = 0    
      arrhjsumove = 0    
      arrhjsumtra = 0       
      arrhjsumlon = 0
      arrhjsumind = 0.           
      IF utomr = "ALLA" THEN DO:
         IF utomr2 = slutsum1.OMRADE THEN utomr = utomr.
         ELSE DO:              
            utomr2 = slutsum1.OMRADE. 
            IF musz = TRUE THEN musz = FALSE.
            ELSE DO:
               CREATE tidut.
               ASSIGN tidut.UT = str2.                
            END.
            CREATE tidut. 
            CREATE tidut.
            FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = slutsum1.OMRADE
            USE-INDEX OMR NO-LOCK NO-ERROR.           
            IF NOT AVAILABLE OMRADETAB THEN  SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gomrl) + " SAKNAS".
            ELSE SUBSTRING(tidut.UT,1) = OMRADETAB.NAMN.                            
            CREATE tidut.
         END.
      END.             
      IF SEL_UPP = "Det.lista Tid-övertid-trakt./ " + gomrk THEN DO:
         IF aoval = FALSE THEN DO:
            FOR EACH slutsum WHERE slutsum.OMRADE = slutsum1.OMRADE:            
               CREATE tidut.                                 
               ASSIGN   
               SUBSTRING(tidut.UT,1) = slutsum.AONR     
               SUBSTRING(tidut.UT,8) = STRING(slutsum.DELNR,">>>")                                                           
               SUBSTRING(tidut.UT,14) = STRING(slutsum.TIMMAR,">>>>>>99")   
               SUBSTRING(tidut.UT,23) = STRING(slutsum.BELOPP,">>>>>>>99")  
               SUBSTRING(tidut.UT,33) = STRING(slutsum.OTIMMAR,">>>>>>99")   
               SUBSTRING(tidut.UT,42) = STRING(slutsum.OBELOPP,">>>>>>>99")
               SUBSTRING(tidut.UT,61) = STRING(slutsum.TBELOPP,">>>>>>>99")
               SUBSTRING(tidut.UT,71) = STRING(slutsum.LONKOST,"->>>>>>99"). 
               FIND FIRST restid2 WHERE restid2.OMRADE = slutsum.OMRADE AND
               restid2.AONR = slutsum.AONR AND restid2.DELNR = slutsum.DELNR
               USE-INDEX OMR NO-LOCK NO-ERROR.  
               IF AVAILABLE restid2 THEN DO:                                 
                  ASSIGN                                         
                  SUBSTRING(tidut.UT,52) = STRING(restid2.TIMMAR,">>>>>>99").             
               END.  
               IF inder = TRUE THEN DO:                               
                  SUBSTRING(tidut.UT,81) = STRING(slutsum.INDEREKT,">>>>>>99").
               END.
            END.
            CREATE tidut.  
            CREATE tidut. 
            SUBSTRING(tidut.UT,1) = "SUMMA:".                                       
            ASSIGN                                                                   
            SUBSTRING(tidut.UT,14) = STRING(slutsum1.TIMMAR,">>>>>>99")   
            SUBSTRING(tidut.UT,23) = STRING(slutsum1.BELOPP,">>>>>>>99")  
            SUBSTRING(tidut.UT,33) = STRING(slutsum1.OTIMMAR,">>>>>>99")   
            SUBSTRING(tidut.UT,42) = STRING(slutsum1.OBELOPP,">>>>>>>99")
            SUBSTRING(tidut.UT,61) = STRING(slutsum1.TBELOPP,">>>>>>>99")
            SUBSTRING(tidut.UT,71) = STRING(slutsum1.LONKOST,"->>>>>>99"). 
            FIND FIRST restid3 WHERE restid3.OMRADE = slutsum1.OMRADE 
            USE-INDEX OMR NO-LOCK NO-ERROR.  
            IF AVAILABLE restid3 THEN DO:                                 
               ASSIGN                                         
               SUBSTRING(tidut.UT,52) = STRING(restid3.TIMMAR,">>>>>>99").             
            END.  
            IF inder = TRUE THEN DO:                               
               SUBSTRING(tidut.UT,81) = STRING(slutsum1.INDEREKT,">>>>>>99").
            END.
         END.
         ELSE DO: 
            FOR EACH slutsum WHERE slutsum.OMRADE = slutsum1.OMRADE:            
               CREATE tidut.                                 
               ASSIGN   
               SUBSTRING(tidut.UT,1) = slutsum.AONR     
               SUBSTRING(tidut.UT,8) = STRING(slutsum.DELNR,">>>") 
               SUBSTRING(tidut.UT,14) = SUBSTRING(slutsum.ORT,1,40)                                                             
               SUBSTRING(tidut.UT,55) = STRING(slutsum.TIMMAR,">>>>>>99")   
               SUBSTRING(tidut.UT,64) = STRING(slutsum.BELOPP,">>>>>>>99")  
               SUBSTRING(tidut.UT,74) = STRING(slutsum.OTIMMAR,">>>>>>99")   
               SUBSTRING(tidut.UT,83) = STRING(slutsum.OBELOPP,">>>>>>>99")
               SUBSTRING(tidut.UT,102) = STRING(slutsum.TBELOPP,">>>>>>>99")
               SUBSTRING(tidut.UT,112) = STRING(slutsum.LONKOST,"->>>>>>99"). 
               FIND FIRST restid2 WHERE restid2.OMRADE = slutsum.OMRADE AND
               restid2.AONR = slutsum.AONR AND restid2.DELNR = slutsum.DELNR
               USE-INDEX OMR NO-LOCK NO-ERROR.  
               IF AVAILABLE restid2 THEN DO:                                 
                  ASSIGN                                         
                  SUBSTRING(tidut.UT,93) = STRING(restid2.TIMMAR,">>>>>>99").             
               END.  
               IF inder = TRUE THEN DO:                               
                  SUBSTRING(tidut.UT,122) = STRING(slutsum.INDEREKT,">>>>>>99").
               END.
            END. 
            CREATE tidut. 
            CREATE tidut. 
            SUBSTRING(tidut.UT,1) = "SUMMA:".                                            
            ASSIGN                                                                   
            SUBSTRING(tidut.UT,55) = STRING(slutsum1.TIMMAR,">>>>>>99")   
            SUBSTRING(tidut.UT,64) = STRING(slutsum1.BELOPP,">>>>>>>99")  
            SUBSTRING(tidut.UT,74) = STRING(slutsum1.OTIMMAR,">>>>>>99")   
            SUBSTRING(tidut.UT,83) = STRING(slutsum1.OBELOPP,">>>>>>>99")
            SUBSTRING(tidut.UT,102) = STRING(slutsum1.TBELOPP,">>>>>>>99")
            SUBSTRING(tidut.UT,112) = STRING(slutsum1.LONKOST,"->>>>>>99"). 
            FIND FIRST restid3 WHERE restid3.OMRADE = slutsum1.OMRADE 
            USE-INDEX OMR NO-LOCK NO-ERROR.  
            IF AVAILABLE restid3 THEN DO:                                 
               ASSIGN                                         
               SUBSTRING(tidut.UT,93) = STRING(restid3.TIMMAR,">>>>>>99").             
            END.  
            IF inder = TRUE THEN DO:                               
               SUBSTRING(tidut.UT,122) = STRING(slutsum1.INDEREKT,">>>>>>99").
            END.
         END.   
      END. 
      ELSE DO:                  
         CREATE tidut.                                 
         ASSIGN                                                                   
         SUBSTRING(tidut.UT,14) = STRING(slutsum1.TIMMAR,">>>>>>99")   
         SUBSTRING(tidut.UT,23) = STRING(slutsum1.BELOPP,">>>>>>>99")  
         SUBSTRING(tidut.UT,33) = STRING(slutsum1.OTIMMAR,">>>>>>99")   
         SUBSTRING(tidut.UT,42) = STRING(slutsum1.OBELOPP,">>>>>>>99")
         SUBSTRING(tidut.UT,61) = STRING(slutsum1.TBELOPP,">>>>>>>99")
         SUBSTRING(tidut.UT,71) = STRING(slutsum1.LONKOST,"->>>>>>99"). 
         FIND FIRST restid3 WHERE restid3.OMRADE = slutsum1.OMRADE 
         USE-INDEX OMR NO-LOCK NO-ERROR.  
         IF AVAILABLE restid3 THEN DO:                                 
            ASSIGN                                         
            SUBSTRING(tidut.UT,52) = STRING(restid3.TIMMAR,">>>>>>99").             
         END.  
         IF inder = TRUE THEN DO:                               
            SUBSTRING(tidut.UT,81) = STRING(slutsum1.INDEREKT,">>>>>>99").
         END.
      END.                                         
      CREATE tidut.      
      CREATE tidut.   
      ASSIGN tidut.UT = "TOTALT".        
      CREATE tidut.  
      CREATE tidut.    
      ASSIGN                   
      SUBSTRING(tidut.UT,1) = "TOTALT ANTAL TIMMAR :"    
      SUBSTRING(tidut.UT,23) = STRING((slutsum1.TIMMAR + slutsum1.OTIMMAR),">>>>>>99.99").
      CREATE tidut.
      ASSIGN                   
      SUBSTRING(tidut.UT,1) = "TOTAL KOSTNAD       :"    
      SUBSTRING(tidut.UT,23) = 
      STRING((slutsum1.BELOPP + slutsum1.OBELOPP + slutsum1.TBELOPP + 
      slutsum1.LONKOST + slutsum1.INDEREKT),">>>>>>99.99").      
      CREATE tidut.                         
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
   IF aoval = TRUE THEN RUN EKLOGL.P.  
   ELSE RUN EKLOGS.P. 
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skrivut_UI WINDOW-2 
PROCEDURE skrivut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
/*UTSKRIFT*/
          
   RUN huvud_UI. 
   IF musz = FALSE THEN DO:
      RUN summa_UI.
   END.             
   musz = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&UNDEFINE BROWSE-NAME
&UNDEFINE FRAME-NAME
&UNDEFINE WINDOW-NAME
