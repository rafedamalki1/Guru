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
{SOKMTRL.I}
DEFINE INPUT PARAMETER TABLE FOR skapa_mtrl.
/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}
&Scoped-define NEW  
{GLOBVAR2DEL1.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{BESTKUNDALLT.I}
{HOPPSEK2W.I}
DEFINE SHARED VARIABLE valkalknr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE vald_kund AS CHARACTER NO-UNDO.  
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.        
DEFINE VARIABLE str AS CHARACTER FORMAT "X(86)" NO-UNDO.
DEFINE VARIABLE totalt AS DECIMAL NO-UNDO. 
DEFINE VARIABLE totalt2 AS DECIMAL NO-UNDO. 

DEFINE VARIABLE rabatt AS DECIMAL NO-UNDO.       


{ANMARKD.I}    
{TIDUTTTNEW.I}
/*{EGENBEN.I}*/



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
&Scoped-Define ENABLED-OBJECTS BTN_SKRIV BTN_AVS 

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
      tidut.ut FORMAT "X(83)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 87.5 BY 26.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BRW_UT AT ROW 1.5 COL 1.5
     BTN_SKRIV AT ROW 28.13 COL 58.5
     BTN_AVS AT ROW 28.13 COL 75
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89 BY 28.42.


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
         TITLE              = "Specifikation av vald materiel"
         HEIGHT             = 28.42
         WIDTH              = 89
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 89
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 89
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
     _FldNameList[1]   > temp-db.tidut.ut
"tidut.ut" ? "X(83)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
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
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
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
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   str= "==============================================================================".      
   RUN huvud_UI.
   RUN rubrik_UI.
   IF musz = FALSE THEN RUN klar_UI.   
   RUN totalt_UI.
   RUN sidfot_UI.    
   IF musz = TRUE THEN DO:
      musz = FALSE.
      status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
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
         status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
         APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
         LEAVE MAIN-BLOCK. 
      END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anm2_UI WINDOW-2 
PROCEDURE anm2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   CREATE tidut.      
   ASSIGN
   SUBSTRING(tidut.UT,1) = "KOMMENTARER"
   retvar = 1
   ednum = 1
   ednum3 = LENGTH(skapa_mtrl.KOM)
   retvar = INDEX(skapa_mtrl.KOM,CHR(10),ednum)
   edtecken = 50
   edtext = skapa_mtrl.KOM
   tidtext = "".  
   {ANMARK2.I}   
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
   IF anmark = 1 THEN DO:                        
      ASSIGN            
      SUBSTRING(tidut.UT,12) = ":"   
      SUBSTRING(tidut.UT,13) = SUBSTRING(edtext,ednum,edtecken).
      CREATE tidut.
   END.  
   ELSE IF anmark = 2 THEN DO:                
      ASSIGN            
      SUBSTRING(tidut.UT,12) = ":"   
      SUBSTRING(tidut.UT,13) = tidtext.
      CREATE tidut.
   END.   
   ELSE IF anmark = 3 THEN DO:           
      ASSIGN           
      SUBSTRING(tidut.UT,12) = ":"    
      SUBSTRING(tidut.UT,13) = SUBSTRING(edtext,1 + ednum2 * edtecken,edtecken).
      CREATE tidut.
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
         FIND FIRST bestkundallt WHERE bestkundallt.BESTID= vald_kund NO-LOCK NO-ERROR.                  
         CREATE tidut. 
         ASSIGN        
         SUBSTRING(tidut.UT,1) = "OFFERT" 
         SUBSTRING(tidut.UT,40) = STRING(TODAY)
         SUBSTRING(tidut.UT,50) = STRING(TIME,"HH:MM:SS").
         CREATE tidut.  
         CREATE tidut.    
         FIND FIRST skapa_mtrl NO-LOCK NO-ERROR.
         SUBSTRING(tidut.UT,1) = skapa_mtrl.LEVNAMN.         
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = STRING(skapa_mtrl.LADR).   
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = STRING(skapa_mtrl.LPNR)   
         SUBSTRING(tidut.UT,8) = STRING(skapa_mtrl.LORT).  
         CREATE tidut. 
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = STRING(skapa_mtrl.LKONTAKT).
         CREATE tidut.
         ASSIGN                                       
         SUBSTRING(tidut.UT,1) = "Tele:"
         SUBSTRING(tidut.UT,6) = STRING(skapa_mtrl.LTELE).                                                           
         CREATE tidut. 
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = str.
         CREATE tidut.
         CREATE tidut.         
         SUBSTRING(tidut.UT,40) = STRING(skapa_mtrl.FORE).
         CREATE tidut.
         ASSIGN            
         SUBSTRING(tidut.UT,40) = STRING(skapa_mtrl.KADR)
         SUBSTRING(tidut.UT,65) = "Box:"
         SUBSTRING(tidut.UT,70) = STRING(skapa_mtrl.BOX).        
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,40) = STRING(skapa_mtrl.KPNR)   
         SUBSTRING(tidut.UT,48) = STRING(skapa_mtrl.KORT).  
         CREATE tidut. 
         CREATE tidut.
         ASSIGN                                                
         SUBSTRING(tidut.UT,40) = "Kontaktperson:".        
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,40) = STRING(skapa_mtrl.KIKONTAKT).
         CREATE tidut.
         ASSIGN 
         SUBSTRING(tidut.UT,40) = "Tele:"
         SUBSTRING(tidut.UT,46) = STRING(skapa_mtrl.KITELE)                                 
         SUBSTRING(tidut.UT,60) = "Fax:"
         SUBSTRING(tidut.UT,65) = STRING(skapa_mtrl.FAX).               
         CREATE tidut.      
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
   FIND FIRST off_mtrl WHERE off_mtrl.KALKNR = valkalknr
   NO-LOCK NO-ERROR.
   IF AVAILABLE off_mtrl THEN DO:
      rabatt = ((off_mtrl.RABTOT / off_mtrl.TOTALT)).
   END.
   ELSE DO:
      rabatt = 1.
   END.   
   FOR EACH kund_mtrl.      
      CREATE tidut.      
      ASSIGN  
      SUBSTRING(tidut.UT,1) = kund_mtrl.ENR
      SUBSTRING(tidut.UT,13) = SUBSTRING(kund_mtrl.BENAMNING,1,35).
      IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN.
      ELSE SUBSTRING(tidut.UT,49) = STRING(kund_mtrl.KPRIS * rabatt,">>>>>>9.99").       
      ASSIGN
      SUBSTRING(tidut.UT,60) = kund_mtrl.ENHET    
      SUBSTRING(tidut.UT,66) = STRING(kund_mtrl.BERKVANT,">>>>9").
      IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN.
      ELSE SUBSTRING(tidut.UT,72) = STRING(kund_mtrl.KPRIS * rabatt * kund_mtrl.BERKVANT,">>>>>>9").
   END.                                           
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
      ASSIGN
      str=                                                                    
"===========.===================================.==========.=====.=====.=======". 
      CREATE tidut.       
      SUBSTRING(tidut.UT,1) = str.                     
      CREATE tidut.      
      ASSIGN  
      SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:genk)                                                                          
      SUBSTRING(tidut.UT,13) = "BENÄMNING"                     
      SUBSTRING(tidut.UT,49) = "PRIS"    
      SUBSTRING(tidut.UT,60) = "ENHET"
      SUBSTRING(tidut.UT,66) = "ANTAL"
      SUBSTRING(tidut.UT,72) = "SUMMA".                         
      CREATE tidut.       
      SUBSTRING(tidut.UT,1) = str.         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sidfot_UI WINDOW-2 
PROCEDURE sidfot_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   str=
"==============================================================================". 
   CREATE tidut.         
   SUBSTRING(tidut.UT,1) = str.
   CREATE tidut. 
   CREATE tidut.
   IF skapa_mtrl.KOM NE "" THEN DO:         
      RUN anm2_UI.
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
  /*SUMMERING AV ALLA ENR*/
   ASSIGN
   totalt = 0 
   totalt2 = 0.  
   FOR EACH kund_mtrl. 
      totalt = totalt + kund_mtrl.KPRIS * rabatt * kund_mtrl.BERKVANT.      
   END. 
   str=
"======================================================================.=======".      
   CREATE tidut. 
   CREATE tidut.      
   SUBSTRING(tidut.UT,1) = str.
   CREATE tidut.
   CREATE tidut.         
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN.
   ELSE DO:      
      SUBSTRING(tidut.UT,58) = "TOTAL SUMMA:".
      CREATE tidut.         
      SUBSTRING(tidut.UT,72) = STRING(totalt,">>>>>>9").
      CREATE tidut.         
      SUBSTRING(tidut.UT,72) = "=======".
   END.
   IF Guru.Konstanter:globforetag = "ELPA"  OR Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
      musz = musz.   
   END.
   ELSE DO:            
      FOR EACH kund_mtrl. 
         totalt2 = totalt2 + (kund_mtrl.BPRIS * kund_mtrl.BERKVANT).      
      END.  
      IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN.
      ELSE DO:      
         CREATE tidut.         
         SUBSTRING(tidut.UT,52) = "TOTALT BRUTTOPRIS:".
         CREATE tidut.         
         SUBSTRING(tidut.UT,72) = STRING(totalt2,">>>>99").
         CREATE tidut.         
         SUBSTRING(tidut.UT,72) = "=======".  
      END.
      IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN.
      ELSE DO:                                                
         FIND FIRST off_mtrl WHERE off_mtrl.KALKNR = valkalknr
         NO-LOCK NO-ERROR.
         IF AVAILABLE off_mtrl THEN DO:          
            CREATE tidut.         
            SUBSTRING(tidut.UT,52) = "TOTALT OFFERTPRIS:".
            CREATE tidut.         
            SUBSTRING(tidut.UT,72) = STRING(off_mtrl.RABTOT,">>>>99").
            CREATE tidut.         
            SUBSTRING(tidut.UT,72) = "=======".         
         END.
         ELSE DO:
            CREATE tidut.         
            SUBSTRING(tidut.UT,52) = "TOTALT OFFERTPRIS:".
            CREATE tidut.  
            SUBSTRING(tidut.UT,72) = STRING(totalt,">>>>99").                         
            CREATE tidut.         
            SUBSTRING(tidut.UT,72) = "=======".
         END.                 
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
   skrivut = FALSE.                         
   FIND LAST tidut NO-LOCK NO-ERROR.     
   RUN EKLOGS.P. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

