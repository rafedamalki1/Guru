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

/* Local Variable Definitions ---                                       */  
{ALLDEF.I}
{GLOBVAR2DEL1.I}
&Scoped-define NEW                                    
DEFINE SHARED VARIABLE alla AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE nytt_bestnr AS INTEGER NO-UNDO.                                          
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE epostvar AS CHARACTER NO-UNDO.

DEFINE VARIABLE EDITOR_MEDD AS CHARACTER  VIEW-AS EDITOR SCROLLBAR-VERTICAL SIZE 53 BY 9 FONT 25 NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.        
DEFINE VARIABLE str AS CHARACTER FORMAT "X(86)" NO-UNDO.
DEFINE VARIABLE totalt AS DECIMAL NO-UNDO. 
DEFINE VARIABLE totalt2 AS DECIMAL NO-UNDO. 
DEFINE VARIABLE leverant AS CHARACTER NO-UNDO.
DEFINE VARIABLE lev AS CHARACTER NO-UNDO.
DEFINE VARIABLE firstlev AS CHARACTER NO-UNDO.  
DEFINE VARIABLE mtrlrak AS INTEGER NO-UNDO.  
DEFINE VARIABLE radrak AS INTEGER NO-UNDO.   
DEFINE VARIABLE langd AS INTEGER NO-UNDO.     
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(41)" NO-UNDO. 
DEFINE VARIABLE prognamn2 AS CHARACTER FORMAT "X(41)" NO-UNDO.
DEFINE VARIABLE skick AS LOGICAL NO-UNDO.
DEFINE VARIABLE efel AS CHARACTER FORMAT "X(30)" NO-UNDO.

DEFINE VARIABLE remotLogin  AS CHARACTER NO-UNDO.
DEFINE VARIABLE remotpasswd AS CHARACTER NO-UNDO.
DEFINE VARIABLE skicka      AS LOGICAL NO-UNDO.
DEFINE VARIABLE ascibin     AS INTEGER NO-UNDO.
DEFINE VARIABLE cLocalFile  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRemoteFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE motagandemaskin AS CHARACTER NO-UNDO.
DEFINE VARIABLE ftpanv AS CHARACTER NO-UNDO.
DEFINE VARIABLE ftplord AS CHARACTER NO-UNDO.  
/*DEFINE TEMP-TABLE felmeddtemp 
  FIELD FELMEDD AS CHARACTER
  FIELD VAL AS INTEGER.*/
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{LEVTEMP.I}
{SPECMTRLTEMP.I}
{SKAPAMTRL.I}
DEFINE INPUT  PARAMETER bestoff  AS CHARACTER.
   
DEFINE SHARED TEMP-TABLE lev_temp NO-UNDO
   FIELD LEVKOD AS CHARACTER.      
   
{TIDUTTTNEW.I}

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
&Scoped-Define ENABLED-OBJECTS BTN_AVS 

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
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 81 BY 25.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BRW_UT AT ROW 1.5 COL 1.5
     BTN_AVS AT ROW 28 COL 68.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.25 ROW 1.04
         SIZE 83 BY 28.38.


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
         TITLE              = "Specifikation av beställning"
         HEIGHT             = 28.42
         WIDTH              = 83.38
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 28.42
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
     _TblList          = "temp-db.tidut"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = temp-db.tidut.ut
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

&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-2
ON CHOOSE OF BTN_AVS IN FRAME FRAME-VINST /* Avsluta */
DO:
   {BORTBRWPROC.I}
   RETURN.
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
   RUN disable_UI.

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  {BORTBRWPROC.I}
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
   {BORTBRWPROC.I}
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
   FIND FIRST spec_mtrl WHERE spec_mtrl.LEVKOD = vald_lev NO-LOCK NO-ERROR.
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_lev NO-LOCK NO-ERROR.     
                              
   FOR EACH tidut:
     DELETE tidut.
   END.   
str=
"=============================================================================".    
   RUN huvud_UI.     
   IF musz = FALSE THEN RUN klar_UI.   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
      
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
      LEAVE MAIN-BLOCK. 
   END.                 
   ELSE DO:
      RUN ut_UI.
      status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
      LEAVE MAIN-BLOCK. 
   END.
   RUN enable_UI.   
   {FRMSIZE.I}          
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
  ENABLE BTN_AVS 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VINST}
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
         FIND FIRST skapa_mtrl NO-LOCK NO-ERROR.
         CREATE tidut.  
         IF Guru.Konstanter:globforetag = "ELPA" THEN DO:                
            ASSIGN SUBSTRING(tidut.UT,1) = "H01;GURUNORD;SE.ABB.SKA;220;".            
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,1) = "H02;" + "ED." + STRING(vald_depa) + "." + STRING(skapa_mtrl.bestnr).
         END.
         IF Guru.Konstanter:globforetag = "VAST" THEN DO:                
            ASSIGN SUBSTRING(tidut.UT,1) = "H01;GURUVAST;SE.ABB.SKA;220;".            
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,1) = "H02;" + "VD." + STRING(vald_depa) + "." + STRING(skapa_mtrl.bestnr).
         END.
         IF Guru.Konstanter:globforetag = "VORD" THEN DO:                
            ASSIGN SUBSTRING(tidut.UT,1) = "H01;GURUNORD;SE.ABB.SKA;220;".            
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,1) = "H02;" + "ND." + STRING(vald_depa) + "." + STRING(skapa_mtrl.bestnr).
         END.
         IF Guru.Konstanter:globforetag = "VSYD" THEN DO:                
            ASSIGN SUBSTRING(tidut.UT,1) = "H01;GURUSYD;SE.ABB.SKA;220;".          
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,1) = "H02;" + "SD." + STRING(vald_depa) + "." + STRING(skapa_mtrl.bestnr).
         END.         
         IF Guru.Konstanter:globforetag = "VOST" THEN DO:                
            ASSIGN SUBSTRING(tidut.UT,1) = "H01;GURUOST;SE.ABB.SKA;220;".           
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,1) = "H02;" + "OD." + STRING(vald_depa) + "." + STRING(skapa_mtrl.bestnr).
         END.                     
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,1) = "H03;" + STRING(TODAY,"999999") + ";" + STRING(skapa_mtrl.DATUM,"999999").

         IF skapa_mtrl.KOM NE " " THEN DO:
            CREATE tidut.         
            ASSIGN SUBSTRING(tidut.UT,1) = "H04;GEN;" + SUBSTRING(skapa_mtrl.KOM,1,40).
            IF LENGTH(skapa_mtrl.KOM) > 40 THEN DO:
               CREATE tidut.         
               ASSIGN SUBSTRING(tidut.UT,1) = "H04;GEN;" + SUBSTRING(skapa_mtrl.KOM,41,40).
            END.
         END.         

         IF skapa_mtrl.AVIS = TRUE THEN DO:   
            CREATE tidut.         
            ASSIGN SUBSTRING(tidut.UT,1) = "H04;GEN;" + "Avisering" + ";" + skapa_mtrl.AVISPERS.  
         END.         
         IF skapa_mtrl.MARK NE " " THEN DO:
            CREATE tidut.         
            ASSIGN SUBSTRING(tidut.UT,1) = "H04;MKS;" + skapa_mtrl.MARK.
         END.
         IF epostvar NE "" THEN DO:         
            CREATE tidut.
            ASSIGN SUBSTRING(tidut.UT,1) = "H04;ZZZ;" + epostvar.
         END.
         
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,1) = "H08;" + STRING(skapa_mtrl.KUNDNR,"99999"). 
         
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,1) = "H10;" + skapa_mtrl.KIKONTAKT + ";" +
         STRING(skapa_mtrl.KITELE) + ";" + STRING(skapa_mtrl.KIMOBIL) + ";" + 
         STRING(skapa_mtrl.FAX).
         
         /*IF skapa_mtrl.KTKONTAKT NE " " THEN DO:
 *             CREATE tidut.
 *             ASSIGN SUBSTRING(tidut.UT,1) = "H11;" + skapa_mtrl.KTKONTAKT + ";" +
 *             STRING(skapa_mtrl.KTTELE) + ";" + STRING(skapa_mtrl.KTMOBIL) + ";".        
 *          END.*/        
         
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,1) = "H12;SE.ABB.SKA".
         
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,1) = "H15;;" + skapa_mtrl.FORE.        
         
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,1) = "H16;" +   
         skapa_mtrl.L1 + ";;;" + STRING(skapa_mtrl.L3) + ";" + skapa_mtrl.L2.
      END.
   END.                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE klar_UI WINDOW-2 
PROCEDURE klar_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   mtrlrak = 0.
   FOR EACH spec_mtrl WHERE spec_mtrl.LEVKOD = vald_lev:
      CREATE tidut.
      ASSIGN
      mtrlrak = mtrlrak + 1.
      IF SUBSTRING(spec_mtrl.ENR,1,1) = "E" THEN DO:
         SUBSTRING(tidut.UT,1) = "L01;" + STRING(mtrlrak) + ";" + spec_mtrl.ENR + ";40".
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,1) = "L02;" + spec_mtrl.ENR + ";" + 
         SUBSTRING(spec_mtrl.BENAMNING,1,25). 
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,1) = "L03;" + STRING(spec_mtrl.BERKVANT,">>>>9") + ";" + 
         spec_mtrl.ENHET.
      END.         
      ELSE DO:
         SUBSTRING(tidut.UT,1) = "L01;" + STRING(mtrlrak) + ";" + "E" + spec_mtrl.ENR + ";40".
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,1) = "L02;" + "E" + spec_mtrl.ENR + ";" + 
         SUBSTRING(spec_mtrl.BENAMNING,1,25). 
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,1) = "L03;" + STRING(spec_mtrl.BERKVANT,">>>>9") + ";" + 
         spec_mtrl.ENHET.
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
   /*MAIL EXCHANGE*/            
   IF Guru.Konstanter:globforetag = "VAST" THEN DO:                      
      prognamn = "VD." + STRING(vald_depa) + "." + STRING(skapa_mtrl.bestnr) + ".guru".
   END.
   ELSE IF Guru.Konstanter:globforetag = "VORD" THEN DO:                      
      prognamn = "ND." + STRING(vald_depa) + "." + STRING(skapa_mtrl.bestnr) + ".guru".
   END.
   ELSE IF Guru.Konstanter:globforetag = "VSYD" THEN DO:                      
      prognamn = "SD." + STRING(vald_depa) + "." + STRING(skapa_mtrl.bestnr) + ".guru".
   END.         
   ELSE IF Guru.Konstanter:globforetag = "VOST" THEN DO:                      
      prognamn = "OD." + STRING(vald_depa) + "." + STRING(skapa_mtrl.bestnr) + ".guru".
   END.
   ELSE DO:
      prognamn = "ED." + STRING(vald_depa) + "." + STRING(skapa_mtrl.bestnr) + ".guru".     
   END.
   prognamn2 = prognamn.
   prognamn = SESSION:TEMP-DIRECTORY.
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN prognamn = webclienttempdir.    
   prognamn = prognamn + prognamn2.
      
   EDITOR_MEDD = "".
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,1) = "EOF".

   OUTPUT TO VALUE(prognamn).
   FOR EACH tidut:                                                              
      PUT UNFORMATTED                                                           
      TRIM(tidut.UT) + CHR(10).                                                 
   END.                                                                         
   OUTPUT CLOSE.       

   FOR EACH tidut:
      EDITOR_MEDD = EDITOR_MEDD  + tidut.UT + CHR(13) + CHR(10).
   END.    
   IF Guru.Konstanter:globforetag = "cELPA" THEN DO:
      ASSIGN   
        ftpanv = CHR(101) + CHR(108) + CHR(112) + CHR(111) + CHR(111) + CHR(108) 
        ftplord = CHR(103) + CHR(117) + CHR(114) + CHR(117) + CHR(49) + CHR(50) + CHR(51). 
        RUN FTPFILE.P (INPUT ftpanv, 
                       INPUT ftplord,
                       INPUT TRUE, INPUT 1,
                     INPUT prognamn, INPUT "/guru/" + prognamn2,
                     INPUT "xxx", OUTPUT TABLE felmeddtemp).
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF felmeddtemp.FELMEDD BEGINS "Fil skickad..." THEN DO:
         MESSAGE "Beställningen är nu skickad. Filen finns att se under:" + prognamn VIEW-AS ALERT-BOX TITLE "Meddelande".
         DELETE felmeddtemp.
      END.
      ELSE DO:
         prognamn2 = SESSION:TEMP-DIRECTORY.        
         IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN prognamn2 = webclienttempdir.
         prognamn2 = prognamn2 + "edifel.txt".
         MESSAGE "Ett fel inträffade. Kontakta Elpool 090 - 18 45 40." VIEW-AS ALERT-BOX TITLE "Meddelande".
         OUTPUT TO VALUE(prognamn).
         FOR EACH felmeddtemp:         
            PUT UNFORMATTED
            TRIM(felmeddtemp.FELMEDD) + CHR(10).            
            DELETE felmeddtemp.
         END.
         OUTPUT CLOSE.
      END.            
   END.
   ELSE DO:     
      ASSIGN   
        ftpanv = CHR(103) + CHR(117) + CHR(114) + CHR(117)   
        ftplord = CHR(98) + CHR(108) + CHR(97) + CHR(122) + CHR(101) + CHR(114). 
        RUN FTPFILE.P (INPUT ftpanv, 
                        INPUT ftplord,
                     INPUT TRUE, INPUT 1,
                     INPUT prognamn, INPUT prognamn2,
                     INPUT "crux", OUTPUT TABLE felmeddtemp).
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF felmeddtemp.FELMEDD BEGINS "Fil skickad..." THEN DO:
         MESSAGE "Beställningen är nu skickad. Filen finns att se under:" + prognamn VIEW-AS ALERT-BOX TITLE "Meddelande".
         DELETE felmeddtemp.      
      END.
      ELSE DO:
         prognamn2 = SESSION:TEMP-DIRECTORY.        
         IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN prognamn2 = webclienttempdir.
         prognamn2 = prognamn2 + "edifel.txt".
         MESSAGE "Ett fel inträffade. Kontakta Elpool 090 - 18 45 40." VIEW-AS ALERT-BOX TITLE "Meddelande".
         OUTPUT TO VALUE(prognamn).
         FOR EACH felmeddtemp:         
            PUT UNFORMATTED
            TRIM(felmeddtemp.FELMEDD) + CHR(10).            
            DELETE felmeddtemp.      
         END.
         OUTPUT CLOSE.
      END.           
   END.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

