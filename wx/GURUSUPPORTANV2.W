&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog 
/*------------------------------------------------------------------------

  File: GURUSUPPORTANV.W

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.


/* ***************************  Definitions  ************************** */
DEFINE VARIABLE ExtradataAppServerHandleSet AS HANDLE NO-UNDO. 
DEFINE VARIABLE ExtraDataDS AS HANDLE NO-UNDO.
DEFINE VARIABLE ExtraFieldH AS HANDLE NO-UNDO.
DEFINE VARIABLE ExtraVadHmt AS CHARACTER NO-UNDO.
DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.


/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-NAMN FILL-IN-LOSEN BTN_ANVANDARE ~
BTN_BORTPROFIL Btn_AVS FILL-IN-ANV 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-NAMN FILL-IN-LOSEN FILL-IN-ANV 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ANVANDARE 
     LABEL "Skapa Användare" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON Btn_AVS AUTO-GO 
     LABEL "Avsluta" 
     SIZE 15 BY 1.13.

DEFINE BUTTON BTN_BORTPROFIL 
     LABEL "Ta bort" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-ANV AS CHARACTER FORMAT "X(256)":U INITIAL "GURUSUPPORT" 
      VIEW-AS TEXT 
     SIZE 14 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-LOSEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lösenord" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NAMN AS CHARACTER FORMAT "X(256)":U INITIAL "First Line Support" 
     VIEW-AS FILL-IN 
     SIZE 28.5 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog
     FILL-IN-NAMN AT ROW 4.75 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     FILL-IN-LOSEN AT ROW 6.5 COL 30.5 COLON-ALIGNED WIDGET-ID 10
     BTN_ANVANDARE AT ROW 23.25 COL 20.5 WIDGET-ID 2
     BTN_BORTPROFIL AT ROW 23.5 COL 57.5 WIDGET-ID 4
     Btn_AVS AT ROW 24 COL 80.5
     FILL-IN-ANV AT ROW 3 COL 29.5 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     SPACE(58.37) SKIP(23.15)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "GuruSupportUsers"
         DEFAULT-BUTTON Btn_AVS WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME Dialog:SCROLLABLE       = FALSE
       FRAME Dialog:HIDDEN           = TRUE.

ASSIGN 
       BTN_ANVANDARE:HIDDEN IN FRAME Dialog           = TRUE.

ASSIGN 
       BTN_BORTPROFIL:HIDDEN IN FRAME Dialog           = TRUE.

ASSIGN 
       FILL-IN-ANV:READ-ONLY IN FRAME Dialog        = TRUE.

ASSIGN 
       FILL-IN-NAMN:READ-ONLY IN FRAME Dialog        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog
/* Query rebuild information for DIALOG-BOX Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog Dialog
ON WINDOW-CLOSE OF FRAME Dialog /* GuruSupportUsers */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog 


/* ***************************  Main Block  *************************** */

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AnslutDS_UI Dialog 
PROCEDURE AnslutDS_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF Guru.Konstanter:appcon THEN DO:
   RUN EXTRADATAHMTDS.P PERSISTENT SET ExtradataAppServerHandleSet ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
END.
ELSE DO:
   RUN EXTRADATAHMTDS.p PERSISTENT SET ExtradataAppServerHandleSet.
END. 
Guru.SharedVariable:ExtradataAppServerHandle = ExtradataAppServerHandleSet.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Avsluta_UI Dialog 
PROCEDURE Avsluta_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   Guru.Konstanter:AvslutaExtradata().
DELETE OBJECT ExtraDataDS NO-ERROR.
ExtraDataDS = ?.     
DELETE OBJECT ExtradataAppServerHandleSet NO-ERROR.
ExtradataAppServerHandleSet = ?.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-NAMN FILL-IN-LOSEN FILL-IN-ANV 
      WITH FRAME Dialog.
  ENABLE FILL-IN-NAMN FILL-IN-LOSEN BTN_ANVANDARE BTN_BORTPROFIL Btn_AVS 
         FILL-IN-ANV 
      WITH FRAME Dialog.
  {&OPEN-BROWSERS-IN-QUERY-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hamtanv_UI Dialog 
PROCEDURE Hamtanv_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ExtraDataDS = ?.
   ExtraVadHmt =  ' EXTRADATA.PROGRAM = "PROFIL"' + ' AND EXTRADATA.HUVUDCH = "SCHAKT" '.
   RUN laddaExtraDataDS_UI IN ExtradataAppServerHandleSet (INPUT ExtraVadHmt,OUTPUT DATASET-HANDLE ExtraDataDS BIND).
   IF VALID-HANDLE(ExtraDataDS) THEN DO:
      Guru.SharedVariable:ExtraDataDS = ExtraDataDS.
      Guru.Konstanter:ExtradataLaddTracking().
   END.
   RUN KollSchaktProfil_UI.
   FIND FIRST schackprofiltt WHERE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE schackprofiltt THEN DO:
      RUN SkapSprofil_UI IN schapph (OUTPUT TABLE schackprofiltt).
      RUN SchProfilHuvCreate_UI.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Open_UI Dialog 
PROCEDURE Open_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   queryvar =  "FOR EACH " + Guru.SharedVariable:ExtradataDsbufftH:TABLE.
Guru.Konstanter:CreateCustomQuery(Guru.SharedVariable:ExtradataDsbufftH,queryvar).

 Guru.GlobalaVariabler:CustomQueryDyn:GET-FIRST().
 ExtraFieldH = Guru.SharedVariable:ExtradataDsbufftH:BUFFER-FIELD("SOKCHAR").
         DO WHILE Guru.GlobalaVariabler:CustomQueryDyn:QUERY-OFF-END = FALSE:
            
            MESSAGE Guru.SharedVariable:ExtradataDsbufftH:BUFFER-FIELD("PROGRAM"):BUFFER-VALUE
            ExtraFieldH:BUFFER-VALUE(4).
            
VIEW-AS ALERT-BOX.
            Guru.GlobalaVariabler:CustomQueryDyn:GET-NEXT().   
         END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Spara_UI Dialog 
PROCEDURE Spara_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   Guru.Konstanter:ExtraDataDSSpar().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

