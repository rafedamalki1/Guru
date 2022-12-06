&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
USING Progress.Windows.*.
USING Infragistics.Win.UltraWinToolbars.*.

DEF VAR bOk              AS LOG    NO-UNDO.
DEF VAR ix               AS INT    NO-UNDO.
DEF VAR hParent          AS HANDLE NO-UNDO.
DEF VAR iMainMenuId      AS INT    NO-UNDO.
DEF VAR httMenu          AS HANDLE NO-UNDO.
DEF VAR httMenuBuffer    AS HANDLE NO-UNDO.
DEF VAR hParameterField  AS HANDLE NO-UNDO.
DEF VAR hMultiple        AS HANDLE NO-UNDO.
DEF VAR iTimerInterval   AS INT    NO-UNDO.
DEF VAR hTimer           AS HANDLE NO-UNDO.
DEF VAR iCurrMenuStyle   AS INT    NO-UNDO.
DEF VAR cTimerServerProc AS CHAR   NO-UNDO.
DEF VAR cTimerClientProc AS CHAR   NO-UNDO.
DEF VAR vObject          AS CLASS Progress.Lang.Object NO-UNDO.
DEF VAR cObjectType      AS CHAR   NO-UNDO.
DEF VAR goJBoxMainForm   AS JBoxMainForm NO-UNDO.
DEF VAR hWin             AS HANDLE NO-UNDO.
DEF VAR hEmbedded        AS HANDLE NO-UNDO.
DEF VAR iDesignHeight    AS INTEGER NO-UNDO.
DEF VAR iDesignWidth     AS INTEGER NO-UNDO.
DEF VAR vGrpWinMenu      AS CLASS Progress.Lang.Object NO-UNDO.
DEF VAR ixNode           AS INT NO-UNDO INIT 1000.
DEF VAR vCurrRibbonTab   AS CLASS Progress.Lang.Object NO-UNDO.
DEF VAR oChildForm       AS Progress.Windows.Form   NO-UNDO.
DEF VAR bWinStatusArea   AS LOG NO-UNDO.
DEF VAR hStatusFrame     AS HANDLE NO-UNDO.
DEF VAR iCurrNodeKey     AS INT NO-UNDO.
DEF VAR iClientXsize     AS INT NO-UNDO INIT 1024.
DEF VAR iClientYsize     AS INT NO-UNDO INIT 768.
DEF VAR cWinMenuGrpLabel AS CHAR NO-UNDO INIT "Behandle".
DEF VAR bDontQuit        AS LOG  NO-UNDO.

DEF TEMP-TABLE ttProgram
    FIELD oRibbonObj   AS CLASS Progress.Lang.Object
    FIELD oChildForm   AS CLASS Progress.Lang.Object
    FIELD iNodeIndex   AS INT
    FIELD cObjectType  AS CHAR
    FIELD cProcName    AS CHAR
    FIELD hWinProc     AS HANDLE
    FIELD iFormTag     AS INT
    FIELD cMenuTitle   AS CHAR 
    FIELD hWinMenuItem AS HANDLE
    FIELD hParent      AS HANDLE
    FIELD bChildWin    AS LOG
    FIELD hTrigWidget  AS HANDLE 
    FIELD cMenuLabel   AS CHAR 
    FIELD cLaunchType  AS CHAR
    FIELD bEmbed       AS LOG 
    FIELD cAccelerator AS CHAR
    FIELD cImage       AS CHAR
    FIELD bUseTimer    AS LOG 
/*     FIELD iSequence    AS INT */
    .

DEF TEMP-TABLE ttMenuAccelerators
   FIELD cAccelerator    AS CHAR 
   FIELD bTriggerDefined AS LOG
   .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-BuildActionRibbonGroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD BuildActionRibbonGroup Procedure 
FUNCTION BuildActionRibbonGroup RETURNS LOGICAL
  (INPUT ihMenu       AS HANDLE,
   INPUT ioParent     AS CLASS Progress.Lang.Object,
   INPUT icParentType AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ClearActionRibbonGroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ClearActionRibbonGroup Procedure 
FUNCTION ClearActionRibbonGroup RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EmbedMe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EmbedMe Procedure 
FUNCTION EmbedMe RETURNS LOGICAL
  ( INPUT ihProc AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWindowFrames) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getWindowFrames Procedure 
FUNCTION getWindowFrames RETURNS HANDLE
  ( INPUT hWidget AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAppStyleSheet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAppStyleSheet Procedure 
FUNCTION setAppStyleSheet RETURNS LOGICAL
  ( INPUT icStyleSheet AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setClientSize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setClientSize Procedure 
FUNCTION setClientSize RETURNS LOGICAL
  ( INPUT iiClientXsize AS INT,
    INPUT iiClientYsize AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setMenuBgImage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMenuBgImage Procedure 
FUNCTION setMenuBgImage RETURNS LOGICAL
  ( INPUT icBgImage AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setRibbonIconImage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setRibbonIconImage Procedure 
FUNCTION setRibbonIconImage RETURNS LOGICAL
  ( INPUT icImageName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setStatusText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setStatusText Procedure 
FUNCTION setStatusText RETURNS LOGICAL
  ( INPUT icStatusText AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setWindowTitle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setWindowTitle Procedure 
FUNCTION setWindowTitle RETURNS LOGICAL
  ( INPUT icTitle AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setWinMenuGroupLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setWinMenuGroupLabel Procedure 
FUNCTION setWinMenuGroupLabel RETURNS LOGICAL
  ( INPUT icWinMenuGrpLabel AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 36.1
         WIDTH              = 63.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{incl\devmode.i}
{incl\custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

hParent = SOURCE-PROCEDURE.

&IF DEFINED(UIB_is_Running) NE 0 &THEN
  RUN InitializeObject(0).
&ENDIF

ON 'CLOSE':U OF THIS-PROCEDURE DO:
  RUN InvalidateHandle(?).
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ActivateWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ActivateWindow Procedure 
PROCEDURE ActivateWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iiNodeKey AS INT NO-UNDO.

DEF VAR hMenu          AS HANDLE NO-UNDO.
DEF VAR cDummy         AS CHAR   NO-UNDO.
DEF VAR cRibbonTabType AS CHAR   NO-UNDO INIT "Infragistics.Win.UltraWinToolbars.RibbonTab".
DEF VAR hFirstFrame    AS HANDLE NO-UNDO.

iCurrNodeKey = iiNodeKey.

DEF BUFFER ttProgram FOR ttProgram.

ClearActionRibbonGroup().
FIND FIRST ttProgram 
     WHERE ttProgram.iNodeIndex = iiNodeKey
     NO-ERROR.
IF AVAIL ttProgram AND VALID-HANDLE(ttProgram.hWinProc) THEN DO:
  PUBLISH "SuspendNamedTimer" ("RefreshStatusBarAndMenu",NOT ttProgram.bUseTimer).
  hMenu = ttProgram.hWinProc:CURRENT-WINDOW:MENU-BAR NO-ERROR.
  IF NOT VALID-HANDLE(hMenu) THEN
    hMenu = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",ttProgram.hWinProc:CURRENT-WINDOW,"MENUBAR")) NO-ERROR.
  IF NOT VALID-HANDLE(hMenu) THEN
    hMenu = ttProgram.hTrigWidget.
  IF VALID-HANDLE(hMenu) THEN DO:
    vGrpWinMenu = goJBoxMainForm:AddRibbonGroup(vCurrRibbonTab,cRibbonTabType,ixNode,cWinMenuGrpLabel,OUTPUT cDummy).
    ixNode = ixNode + 1.
    BuildActionRibbonGroup(hMenu:FIRST-CHILD,vGrpWinMenu,"Infragistics.Win.UltraWinToolbars.RibbonGroup").
  END.
  hFirstFrame = ttProgram.hWinProc:CURRENT-WINDOW:FIRST-CHILD NO-ERROR.
  IF VALID-HANDLE(hFirstFrame) THEN
    APPLY "ENTRY" TO hFirstFrame.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ApplyAccelerator) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ApplyAccelerator Procedure 
PROCEDURE ApplyAccelerator :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icAccelerator AS CHAR NO-UNDO.

FIND FIRST ttProgram
     WHERE ttProgram.iNodeIndex > 1000
       AND ttProgram.cAccelerator = icAccelerator
NO-ERROR.
IF AVAIL ttProgram THEN DO:
  APPLY "CHOOSE" TO ttProgram.hTrigWidget.
  RETURN "NO-APPLY".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BuildMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildMenu Procedure 
PROCEDURE BuildMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iiMenuId     AS INT NO-UNDO.
DEF INPUT PARAM ivParent     AS Progress.Lang.Object NO-UNDO.
DEF INPUT PARAM iiParentNode AS INT  NO-UNDO.
DEF INPUT PARAM icParentType AS CHAR NO-UNDO.

DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR hBuffer       AS HANDLE NO-UNDO.
DEF VAR hMenuObject   AS HANDLE NO-UNDO.
DEF VAR hBuffParamFld AS HANDLE NO-UNDO.
DEF VAR bLarge        AS LOG    NO-UNDO.
DEF VAR bFirstInGroup AS LOG    NO-UNDO.
DEF VAR cImage        AS CHAR   NO-UNDO.

CREATE QUERY hQuery.
CREATE BUFFER hBuffer FOR TABLE(httMenuBuffer).
hBuffParamFld = hBuffer:BUFFER-FIELD("cParameter") NO-ERROR.

hQuery:ADD-BUFFER(hBuffer).
hQuery:QUERY-PREPARE("FOR EACH JBoxMenu WHERE JBoxMenu.iParentMenuId = " + STRING(iiMenuId) + " BY iNodeIndex").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:

  IF hBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE = "menubar" THEN DO:      
    iCurrMenuStyle = 0.

    CREATE ttProgram.
    ASSIGN ttProgram.iNodeIndex = hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE     
           ttProgram.oRibbonObj = goJBoxMainForm:getToolbarHandle(OUTPUT ttProgram.cObjectType)
           ttProgram.cMenuLabel = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
           .
    RUN BuildMenu (hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE,
                   ttProgram.oRibbonObj,
                   hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE,
                   ttProgram.cObjectType).


  END.
  ELSE IF hBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE = "tv-nav-bar" THEN DO:      
    iCurrMenuStyle = 1.

/*     MESSAGE hBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE SKIP hBuffer:BUFFER-FIELD("iLevel"):BUFFER-VALUE */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                    */
/*     IF hBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE NE "" THEN         */
/*       cDefImage = hBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE.        */
/*     IF hBuffer:BUFFER-FIELD("cSelImage"):BUFFER-VALUE NE "" THEN      */
/*       cDefSelImage = hBuffer:BUFFER-FIELD("cSelImage"):BUFFER-VALUE.  */

    RUN BuildMenu (hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE,
                   ?,
                   hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE,
                   "").
  END.

  IF hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE BEGINS "JBoxTimer" THEN DO:
    iTimerInterval  = INT(hBuffer:BUFFER-FIELD("cAccelerator"):BUFFER-VALUE) NO-ERROR.
    IF ERROR-STATUS:ERROR OR iTimerInterval = 0 THEN iTimerInterval = 500000.
    cTimerServerProc = ENTRY(1,hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE).
    IF cTimerServerProc = "" THEN cTimerServerProc = "jbserv_checkbroadcastmessage.p".
    IF NUM-ENTRIES(hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE) > 1 THEN
      cTimerClientProc = ENTRY(2,hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE).
    ELSE cTimerClientProc = "JBoxGetBroadCast.w".
    
    hQuery:GET-NEXT().
    NEXT.
  END.

  CASE STRING(hBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE):
    WHEN "SUB-MENU" THEN DO:
      IF iCurrMenuStyle > 0 THEN DO:
        CASE INTEGER(hBuffer:BUFFER-FIELD("iLevel"):BUFFER-VALUE):
          WHEN 3 THEN DO:
            CREATE ttProgram.
            ASSIGN ttProgram.iNodeIndex = hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE * -1 
                   ttProgram.oRibbonObj = goJBoxMainForm:AddRibbonTab(ttProgram.iNodeIndex,
                                                                      hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE,
                                                                      OUTPUT ttProgram.cObjectType)
                   ttProgram.cMenuLabel = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   .
            IF vCurrRibbonTab = ? THEN vCurrRibbonTab = ttProgram.oRibbonObj.
            IF hBuffer:BUFFER-FIELD("bHasChildMenuItem"):BUFFER-VALUE THEN DO:
              ASSIGN vObject     = ttProgram.oRibbonObj
                     cObjectType = ttProgram.cObjectType
                     .
              CREATE ttProgram.
              ASSIGN ttProgram.iNodeIndex = hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE     
                     ttProgram.oRibbonObj = goJBoxMainForm:AddRibbonGroup(vObject,
                                                                          cObjectType,
                                                                          ttProgram.iNodeIndex,
                                                                          hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE,
                                                                          OUTPUT ttProgram.cObjectType)
                     ttProgram.cMenuLabel = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                     .
            END.
          END.
/*          WHEN 4 THEN DO:
            CREATE ttProgram.
            ASSIGN ttProgram.iNodeIndex = hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE     
                   ttProgram.oRibbonObj = goJBoxMainForm:AddRibbonGroup(ivParent,
                                                                        icParentType,
                                                                        ttProgram.iNodeIndex,
                                                                        hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE,
                                                                        OUTPUT ttProgram.cObjectType)
                   .
          END.    */
          OTHERWISE DO:
            CREATE ttProgram.
            ASSIGN ttProgram.iNodeIndex = hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE
                   ttProgram.cMenuLabel = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   ttProgram.oRibbonObj = goJBoxMainForm:AddRibbonPopupMenuTool(ivParent,
                                                                                icParentType,
                                                                                ttProgram.iNodeIndex,
                                                                                hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE,
                                                                                "", /* Image */
                                                                                "", /* Accelerator */
                                                                                NO, /* Large */
                                                                                NO, /* First in group */
                                                                                OUTPUT ttProgram.cObjectType)
                   .
          END.
        END CASE.
      END.
      ELSE DO:
        CREATE ttProgram.
        ASSIGN ttProgram.iNodeIndex = hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE
               ttProgram.cMenuLabel = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
               ttProgram.oRibbonObj = goJBoxMainForm:AddRibbonPopupMenuTool(ivParent,
                                                                            icParentType,
                                                                            ttProgram.iNodeIndex,
                                                                            hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE,
                                                                            "", /* Image */
                                                                            "", /* Accelerator */
                                                                            NO, /* Large */
                                                                            NO, /* First in group */
                                                                            OUTPUT ttProgram.cObjectType)
               .
      END.
      RUN BuildMenu (hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE,
               ttProgram.oRibbonObj,
               hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE,
               ttProgram.cObjectType).
    END.
    WHEN "MENU-ITEM" THEN DO:
      IF hBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE NE "" THEN
        cImage = ENTRY(NUM-ENTRIES(hBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE,"\"),hBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE,"\").
      ELSE IF hBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE NE "" THEN
        cImage = ENTRY(NUM-ENTRIES(hBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE,"\"),hBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE,"\").
      ELSE cImage = "".  
      bLarge = cImage NE "".

      CREATE ttProgram.
      ASSIGN ttProgram.iNodeIndex  = hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE
             ttProgram.oRibbonObj  = goJBoxMainForm:AddRibbonButtonTool(ivParent,icParentType,ttProgram.iNodeIndex,hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE,
                                                                       IF bLarge THEN cImage ELSE "",
/*                                                                        "j0431631.png", */
                                                                       "", /* Accelerator */
                                                                       bLarge, /* Large */
                                                                       bFirstInGroup, /* First in group */
                                                                       OUTPUT ttProgram.cObjectType)
             ttProgram.cProcName   = hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE
             ttProgram.cMenuLabel  = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
             ttProgram.cLaunchType = hBuffer:BUFFER-FIELD("cLaunchType"):BUFFER-VALUE
             ttProgram.bEmbed      = NOT hBuffer:BUFFER-FIELD("bConfigurable"):BUFFER-VALUE
             bFirstInGroup         = NO 
             .
    END.
    WHEN "RULE" THEN bFirstInGroup = YES.
    /*
         RUN addNode IN hTree (
                 STRING(hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE),
                 STRING(iiMenuId),
                 hBuffer:BUFFER-FIELD("bHasChildSubMenu"):BUFFER-VALUE,
                 hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE,
                 hBuffer:BUFFER-FIELD("cMenuTooltip"):BUFFER-VALUE,

                 IF hBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE NE "" THEN
                   hBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE
                 ELSE cDefImage 
                ,IF hBuffer:BUFFER-FIELD("cSelImage"):BUFFER-VALUE NE "" THEN
                   hBuffer:BUFFER-FIELD("cSelImage"):BUFFER-VALUE
                 ELSE cDefSelImage 
                ,""
                ,hBuffer:BUFFER-FIELD("cFontStyle"):BUFFER-VALUE
                ,hBuffer:BUFFER-FIELD("cTextColor"):BUFFER-VALUE
                ,"enable"
                ,"sub-menu").
      ELSE 
        CREATE SUB-MENU hMenuObject
          ASSIGN PARENT = ihParent
                 LABEL  = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                 NAME   = "sm_" + hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                 .
      RUN BuildMenu (hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE,
                     hMenuObject,
                     hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE).
    END.
    WHEN "PLACEHOLDER" THEN DO:
      IF iCurrMenuStyle = 0 THEN DO:
        CREATE SUB-MENU hMenuObject
          ASSIGN PARENT = ihParent
                 LABEL  = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                 NAME   = "ph_" + hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                 .
        IF hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE = "winmenu" THEN
          ON MENU-DROP OF hMenuObject PERSISTENT RUN KeepWindowOnTop IN THIS-PROCEDURE (hMenuObject).
      END.
    END.
    WHEN "MENU-ITEM" THEN DO:
      IF iCurrMenuStyle > 0 THEN DO:
        IF NOT CAN-DO("this-procedure,rule",hBuffer:BUFFER-FIELD("cLaunchType"):BUFFER-VALUE) THEN
          RUN addNode IN hTree (
                  STRING(hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE),
                  STRING(iiMenuId),
                  NO,
                  hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE,
                  hBuffer:BUFFER-FIELD("cMenuTooltip"):BUFFER-VALUE,
                  IF hBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE NE "" THEN
                    hBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE
                  ELSE (IF iCurrMenuStyle = 1 THEN cDefSelImage ELSE cDefImage)
                 ,IF hBuffer:BUFFER-FIELD("cSelImage"):BUFFER-VALUE NE "" THEN
                    hBuffer:BUFFER-FIELD("cSelImage"):BUFFER-VALUE
                  ELSE cDefSelImage 
                 ,""
                 ,hBuffer:BUFFER-FIELD("cFontStyle"):BUFFER-VALUE
                 ,hBuffer:BUFFER-FIELD("cTextColor"):BUFFER-VALUE
                 ,"enable"
                 ,"menu-item").
      END.     

      ELSE CASE STRING(hBuffer:BUFFER-FIELD("cLaunchType"):BUFFER-VALUE):
        WHEN "START-WINDOW" THEN DO:
          IF VALID-HANDLE(hMultiple) THEN
            CREATE MENU-ITEM hMenuObject
              ASSIGN PARENT      = ihParent
                     LABEL       = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                     NAME        = "mi_" + hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                     ACCELERATOR = hBuffer:BUFFER-FIELD("cAccelerator"):BUFFER-VALUE
              TRIGGERS:
                ON CHOOSE PERSISTENT RUN StartWindow IN THIS-PROCEDURE (hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE,
                                                                        hBuffer:BUFFER-FIELD("bMultiple"):BUFFER-VALUE,
                                                                        hMenuObject).
              END TRIGGERS.
          ELSE
            CREATE MENU-ITEM hMenuObject
              ASSIGN PARENT      = ihParent
                     LABEL       = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                     NAME        = "mi_" + hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                     ACCELERATOR = hBuffer:BUFFER-FIELD("cAccelerator"):BUFFER-VALUE
              TRIGGERS:
                ON CHOOSE PERSISTENT RUN StartWindow IN THIS-PROCEDURE (hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE,
                                                                        FALSE,
                                                                        hMenuObject).
              END TRIGGERS.
        END.
        WHEN "THIS-PROCEDURE" THEN
          CREATE MENU-ITEM hMenuObject
            ASSIGN PARENT      = ihParent
                   LABEL       = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   NAME        = "mi_" + hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   ACCELERATOR = hBuffer:BUFFER-FIELD("cAccelerator"):BUFFER-VALUE
            TRIGGERS:
              ON CHOOSE PERSISTENT RUN VALUE(hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE) IN THIS-PROCEDURE.
            END TRIGGERS.
        WHEN "DATA-BROWSE" THEN
          CREATE MENU-ITEM hMenuObject
            ASSIGN PARENT      = ihParent
                   LABEL       = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   NAME        = "mi_" + hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   ACCELERATOR = hBuffer:BUFFER-FIELD("cAccelerator"):BUFFER-VALUE
            TRIGGERS:
              ON CHOOSE PERSISTENT RUN StartDataBrw  IN THIS-PROCEDURE (hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE,hMenuObject,
                                                                        (IF VALID-HANDLE(hBuffParamFld) THEN hBuffParamFld:BUFFER-VALUE ELSE "")).
            END TRIGGERS.
        WHEN "OS-COMMAND" THEN
          CREATE MENU-ITEM hMenuObject
            ASSIGN PARENT      = ihParent
                   LABEL       = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   NAME        = "mi_" + hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   ACCELERATOR = hBuffer:BUFFER-FIELD("cAccelerator"):BUFFER-VALUE
            TRIGGERS:
              ON CHOOSE PERSISTENT RUN StartOsCommand IN THIS-PROCEDURE (hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE).
            END TRIGGERS.
        WHEN "URL" THEN
          CREATE MENU-ITEM hMenuObject
            ASSIGN PARENT      = ihParent
                   LABEL       = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   NAME        = "mi_" + hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   ACCELERATOR = hBuffer:BUFFER-FIELD("cAccelerator"):BUFFER-VALUE
            TRIGGERS:
              ON CHOOSE PERSISTENT RUN StartURL IN THIS-PROCEDURE (hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE).
            END TRIGGERS.
        OTHERWISE
          CREATE MENU-ITEM hMenuObject
            ASSIGN PARENT      = ihParent
                   LABEL       = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   NAME        = "mi_" + hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   ACCELERATOR = hBuffer:BUFFER-FIELD("cAccelerator"):BUFFER-VALUE
            TRIGGERS:
              ON CHOOSE PERSISTENT RUN StartProcedure IN THIS-PROCEDURE (hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE,
                                                                         hMenuObject).
            END TRIGGERS.
      END CASE.
    END.
    WHEN "RULE" THEN DO:
      IF iCurrMenuStyle = 0 THEN        
        CREATE MENU-ITEM hMenuObject
          ASSIGN PARENT      = ihParent
                 SUBTYPE     = "rule"
                 .
    END.
    */
  END CASE.
/*
  IF hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE = "ChangeCompany" THEN 
    hMenuCompany = hMenuObject.

  hBuffer:BUFFER-FIELD("hMenuItem"):BUFFER-VALUE = hMenuObject.
  */
  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.
DELETE OBJECT hBuffer.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ChangeCompany) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChangeCompany Procedure 
PROCEDURE ChangeCompany :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iCurrCompany     AS INT    NO-UNDO.
DEF VAR iMenuChoice      AS INT    NO-UNDO.
DEF VAR iDummy           AS INT    NO-UNDO.
DEF VAR hDummyWin        AS HANDLE NO-UNDO.

FIND FIRST ttProgram WHERE ttProgram.hWinMenuItem = SELF NO-ERROR.

iCurrCompany = DYNAMIC-FUNCTION("getCompanyId").

DYNAMIC-FUNCTION("setMenuRestart",YES).
RUN JBoxDummyWin.w PERSIST SET hDummyWin.
RUN JBoxDSelectCompany.w (TRUE, INPUT-OUTPUT iDummy).
DELETE PROCEDURE hDummyWin.
DYNAMIC-FUNCTION("setMenuRestart",NO).

IF DYNAMIC-FUNCTION("getCompanyId") NE iCurrCompany AND
   CAN-DO(hParent:INTERNAL-ENTRIES,"RestartMenu")
   THEN DO:
  RUN SelectRestartMenu (OUTPUT iMenuChoice).
  RUN CloseMenu.
  RUN RestartMenu IN hParent (iMenuChoice).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CheckTimerEvent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckTimerEvent Procedure 
PROCEDURE CheckTimerEvent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
DYNAMIC-FUNCTION("setShowHourGlass",FALSE).
DYNAMIC-FUNCTION("runproc","dummy_oppslag.p","",?).
DYNAMIC-FUNCTION("setShowHourGlass",TRUE).
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CloseChildWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CloseChildWindow Procedure 
PROCEDURE CloseChildWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iiNodeKey AS INT NO-UNDO.

ClearActionRibbonGroup().

FIND FIRST ttProgram 
     WHERE ttProgram.iNodeIndex = iiNodeKey
     NO-ERROR.
IF AVAIL ttProgram THEN DO: 
  IF VALID-HANDLE(ttProgram.hWinProc) THEN 
    APPLY "CLOSE" TO ttProgram.hWinProc.
  ttProgram.oChildForm = ?.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CloseMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CloseMenu Procedure 
PROCEDURE CloseMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH ttProgram:
  IF VALID-HANDLE(ttProgram.hWinProc) THEN
    APPLY "close" TO ttProgram.hWinProc.
END.
DELETE OBJECT httMenuBuffer NO-ERROR.
DELETE OBJECT httMenu NO-ERROR.

bDontQuit = YES. /* DELETE OBJECT causes a FormClosed event... */
DELETE OBJECT goJBoxMainForm.
bDontQuit = NO.

/* PUBLISH "InvalidateHandle" (THIS-PROCEDURE).  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CloseWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CloseWindow Procedure 
PROCEDURE CloseWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* ClearActionRibbonGroup(). */
IF DYNAMIC-FUNCTION("getAttribute",SESSION,"mainmenuhandle") = STRING(THIS-PROCEDURE) OR DYNAMIC-FUNCTION("getAttribute",SESSION,"mainmenuhandle") = "" THEN
  QUIT.
ELSE RUN CloseMenu.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EventHandler) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EventHandler Procedure 
PROCEDURE EventHandler :
/*------------------------------------------------------------------------------
  Purpose: Handles all events from the form                                                                                                                                       
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER icEventName AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER poSender    AS System.Object  NO-UNDO.
DEFINE INPUT PARAMETER poArgs      AS System.EventArgs NO-UNDO.

DEF VAR cRetVal AS CHARACTER NO-UNDO.

CASE icEventName:
  /* pass menu item stuff through */
  WHEN 'ToolClick' THEN DO:
    IF TYPE-OF(poSender, UltraToolbarsManager) THEN DO:
      FIND FIRST ttProgram 
           WHERE ttProgram.iNodeIndex = INT(cast(poArgs, ToolClickEventArgs):Tool:KEY)
           NO-ERROR.
      IF AVAIL ttProgram THEN DO: 
        IF ttProgram.oChildForm NE ? THEN 
          CAST(ttProgram.oChildForm,"Progress.Windows.MDIChildForm"):BringToFront().
        ELSE IF VALID-HANDLE(ttProgram.hTrigWidget) THEN DO:
          IF ttProgram.hTrigWidget:TOGGLE-BOX THEN DO:
            IF NOT PROGRAM-NAME(3) BEGINS "setStatusMenuItem" THEN DO:
              ttProgram.hTrigWidget:CHECKED = NOT ttProgram.hTrigWidget:CHECKED.
              APPLY "VALUE-CHANGED" TO ttProgram.hTrigWidget.
            END.
          END.
          ELSE
            APPLY "CHOOSE" TO ttProgram.hTrigWidget.
          RUN SetRibbonActionState.
        END.
        ELSE RUN StartWindow (cast(poArgs, ToolClickEventArgs):Tool:KEY).
      END.
    END.
  END.    
  WHEN 'FormClosed' THEN DO:
    IF NOT bDontQuit AND DYNAMIC-FUNCTION("getAttribute",SESSION,"mainmenuhandle") = STRING(THIS-PROCEDURE) OR DYNAMIC-FUNCTION("getAttribute",SESSION,"mainmenuhandle") = "" THEN
      QUIT.
  END.
  WHEN 'FormClosing' THEN 
    RUN CloseChildWindow (INT(STRING(CAST(poSender, Form):Tag))).
  WHEN 'FormActivated' THEN 
    RUN ActivateWindow (INT(STRING(CAST(poSender, Form):Tag))).
  WHEN 'TabSelected' THEN 
    RUN SetCurrTab (cast(poArgs, RibbonTabEventArgs):TAB:KEY).

END CASE.

RETURN cRetVal.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitializeObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject Procedure 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iiMenuId    AS INT NO-UNDO.

DEF VAR iCompanyId       AS INT    NO-UNDO.
DEF VAR cReadOnlyUsers   AS CHAR   NO-UNDO.
DEF VAR cReadOnlyActions AS CHAR   NO-UNDO.
DEF VAR cImageList       AS CHAR   NO-UNDO.
DEF VAR cLargeImageList  AS CHAR   NO-UNDO.
DEF VAR cGoToList        AS CHAR   NO-UNDO.
DEF VAR hPHnavBarOptions AS HANDLE NO-UNDO.
DEF VAR iNodeIdx         AS INT    NO-UNDO.
DEF VAR cLogoImage       AS CHAR   NO-UNDO.
DEF VAR cAnyCompany      AS CHAR   NO-UNDO.
DEF VAR iDummy           AS INT    NO-UNDO.
DEF VAR cAppImage        AS CHAR   NO-UNDO.
DEF VAR cPanelURL        AS CHAR   NO-UNDO.
DEF VAR cPanelFile       AS CHAR   NO-UNDO.
DEF VAR hDummyWin        AS HANDLE NO-UNDO.

goJBoxMainForm = NEW JBoxMainForm(THIS-PROCEDURE).  
goJBoxMainForm:ClientSize = NEW System.Drawing.Size(iClientXsize,iClientYsize).
/* goJBoxMainForm:setWindowTitle("OVF - Eiendom og kontraktsystem").  */
/* goJBoxMainForm:setBgImage("bg-green.bmp").                         */
/* goJBoxMainForm:setIconImage("RUN.bmp").                            */
/* goJBoxMainForm:setStatusText("Brynjar").                           */
/* goJBoxMainForm:setRibbonIconImage("j0433808.png").                 */
/* goJBoxMainForm:setAppStyleSheet("vs2008_test.isl").                */
/* goJBoxMainForm:setAppStyleSheet("JukeBoxGray.isl").  */

iCompanyId = DYNAMIC-FUNCTION("getCompanyId").
cAnyCompany = DYNAMIC-FUNCTION("getFieldValues","FIRST JBoxCompany","WHERE iJBoxCompanyId > 0","iJBoxCompanyId").
IF iCompanyId = 0 AND cAnyCompany NE ? AND cAnyCompany NE "" THEN DO:
  RUN JBoxDummyWin.w PERSIST SET hDummyWin.
  RUN JBoxDSelectCompany.w PERSIST (TRUE,INPUT-OUTPUT iDummy).
  IF DYNAMIC-FUNCTION("getUserLevel") = "super" THEN 
    RUN SelectRestartMenu (OUTPUT iiMenuId).
  DELETE PROCEDURE hDummyWin.
END.

IF iiMenuId = 0 THEN DO:
  IF DYNAMIC-FUNCTION("runProc","jbsetup_getuser_mainmenu.p","",?) THEN 
    iiMenuId = INT(DYNAMIC-FUNCTION("getTransactionMessage")).
  ELSE 
    iiMenuId = INT(DYNAMIC-FUNCTION("getFieldValues","JBoxMenu","WHERE cMenuType = 'menu'","iJBoxMenuId")).
END.

IF iiMenuId = ? THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"No menu definitions found in database" + CHR(10)
                                 + DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  QUIT.
END.

/* Normally the session object is already created here.. */
DYNAMIC-FUNCTION("NewObject",SESSION,SESSION,"session").

SUBSCRIBE TO "SetRibbonActionState" ANYWHERE.
SUBSCRIBE TO "RebuildActionMenu"    ANYWHERE.

SUBSCRIBE TO "RefreshMenu"          ANYWHERE.
SUBSCRIBE TO "getPlaceholderHandle" ANYWHERE.
SUBSCRIBE TO "getMenuImage"         ANYWHERE.
SUBSCRIBE TO "InvalidateHandle"     ANYWHERE.
SUBSCRIBE TO "ApplyMenu"            ANYWHERE.
SUBSCRIBE TO "setCompanyLogo"       ANYWHERE.
  
SUBSCRIBE TO "UserNotification" ANYWHERE.
SUBSCRIBE TO "setViewBtnEvents" ANYWHERE.
SUBSCRIBE TO "setMenuPanelURL"  ANYWHERE.
SUBSCRIBE TO "setEventMsgProg"  ANYWHERE.
SUBSCRIBE TO "StartChildWindow" ANYWHERE.
  
iMainMenuId = iiMenuId.

httMenu = DYNAMIC-FUNCTION("getTempTable","jbsetup_getmenustruct.p"
                            ,"WHERE iJBoxMenuId = " + STRING(iMainMenuId) 
                            ,?).

httMenuBuffer = httMenu:DEFAULT-BUFFER-HANDLE.
hParameterField = httMenuBuffer:BUFFER-FIELD("cParameter") NO-ERROR.
hMultiple = httMenuBuffer:BUFFER-FIELD("bMultiple") NO-ERROR.

/* RUN toexcelviafile.p (httMenuBuffer,0). */

bOk = httMenuBuffer:FIND-FIRST("WHERE cMenuType = 'menu'") NO-ERROR.
IF bOk THEN DO:
  IF httMenuBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE MATCHES "*.png" AND 
     SEARCH(httMenuBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE) NE ? THEN DO:
    cAppImage = httMenuBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE.
  END.
  IF cAppImage NE "" THEN
    goJBoxMainForm:setRibbonIconImage(ENTRY(NUM-ENTRIES(cAppImage,"\"),cAppImage,"\")).

/*   IF httMenuBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE NE "" THEN                             */
/*     THIS-PROCEDURE:CURRENT-WINDOW:TITLE = httMenuBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE.  */
  IF httMenuBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE NE "" AND SEARCH(httMenuBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE) NE ? THEN
    cLogoImage = httMenuBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE.

  ASSIGN cPanelURL    = httMenuBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE
         cPanelFile   = (IF cPanelURL NE "" THEN cPanelURL ELSE cPanelFile)
/*          bDefStartMax = httMenuBuffer:BUFFER-FIELD("bConfigurable"):BUFFER-VALUE */
         iNodeIdx     = httMenuBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE 
         .
END.
ELSE DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Error in menu definition - no root (menu) node defined","","").
  QUIT.    
END.

RUN BuildMenu (iMainMenuId,?,iNodeIdx,"").

/* RUN getPlaceholderHandle("WinMenu",OUTPUT hWinMenu). */

/*   IF cLogoImage NE "" THEN                                  */
/*     DYNAMIC-FUNCTION("setNavBarImage" IN hTree,cLogoImage). */

/* {&WINDOW-NAME}:TITLE = DYNAMIC-FUNCTION("getAppTitle"). */

  DYNAMIC-FUNCTION("getFunctionRestrictions").    
/*   DYNAMIC-FUNCTION("setCompanyHeader",THIS-PROCEDURE:CURRENT-WINDOW). */


IF LOGICAL(DYNAMIC-FUNCTION("getFieldValues","JBoxUser","WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","bSuperUser")) THEN
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userlevel","super").
ELSE IF LOGICAL(DYNAMIC-FUNCTION("getFieldValues","JBoxCompanyUser","WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASuserId") + "'" +
                                                                    "  AND iJBoxCompanyId = " + STRING(DYNAMIC-FUNCTION("getCompanyId")),"bSuperUserCompany")) THEN DO:
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userlevel","companysuper").
/*   setDisabledMenus(THIS-PROCEDURE:CURRENT-WINDOW:MENUBAR:FIRST-CHILD,DYNAMIC-FUNCTION("getFieldList","JBoxGenCode;cCodeValue","WHERE cCodeType = 'SU-menu'")). */
END.
ELSE DO:
/*   setDisabledMenus(THIS-PROCEDURE:CURRENT-WINDOW:MENUBAR:FIRST-CHILD,DYNAMIC-FUNCTION("getFieldList","JBoxGenCode;cCodeValue","WHERE cCodeType MATCHES '*SU-menu*'")). */

  cReadOnlyUsers   = REPLACE(DYNAMIC-FUNCTION("getFieldList","JBoxGenCode;cCodeValue","WHERE cCodeType = 'ro-users'"),"|",",").
  cReadOnlyActions = REPLACE(DYNAMIC-FUNCTION("getFieldList","JBoxGenCode;cCodeValue","WHERE cCodeType = 'ro-actions'"),"|",",").
  IF cReadOnlyActions = "" THEN cReadOnlyActions = "new,edit,copy,delete,undo,save".

  IF CAN-DO(cReadOnlyUsers,DYNAMIC-FUNCTION("getASuserId")) THEN
    DYNAMIC-FUNCTION("setSecDisabledActions",cReadOnlyActions,"").
END.

/* DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW). */

/* If the menu contains an entry for message publication (requires JukeBox messaging installed) 
   start the timer here: */

IF iTimerInterval > 0 THEN DO:
  IF SEARCH("controls.dll") NE ? AND SEARCH("JBoxJLWTimer.r") NE ? THEN 
    RUN JBoxJLWtimer.w PERSIST SET hTimer ("CheckTimerEvent",iTimerInterval).
  ELSE IF SEARCH("JBoxABLtimer.r") NE ? AND PROPATH MATCHES "*webclient*" THEN
    RUN JBoxABLtimer.w PERSIST SET hTimer ("CheckTimerEvent",iTimerInterval).
  ELSE IF SEARCH("rstimer.ocx") NE ? AND PROPATH MATCHES "*webclient*" THEN
    RUN JBoxRsTimer.w PERSIST SET hTimer ("CheckTimerEvent",iTimerInterval).
  ELSE
    RUN JBoxTimer.w PERSIST SET hTimer ("CheckTimerEvent",iTimerInterval).
  IF iTimerInterval > 1000 THEN
    RUN CheckTimerEvent.
END.


PUBLISH "setProgBarProperty" ("text"," ").
PUBLISH "setProgBarProperty" ("position","0").

goJBoxMainForm:Show().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InvalidateHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InvalidateHandle Procedure 
PROCEDURE InvalidateHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihWinProc AS HANDLE NO-UNDO.

DEF BUFFER ttProgram FOR ttProgram.

FOR EACH ttProgram
    WHERE (IF ihWinProc NE ? THEN ttProgram.hParent = ihWinProc ELSE TRUE):
  IF VALID-HANDLE(ttProgram.hWinProc) THEN 
    APPLY "close" TO ttProgram.hWinProc.
  IF AVAIL ttProgram THEN DO:
    IF ttProgram.oChildForm NE ? THEN DO:
      oChildForm = CAST(ttProgram.oChildForm,"Progress.Windows.Form") NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN
        oChildForm:CLOSE().
    END.
    DELETE OBJECT ttProgram.hWinMenuItem NO-ERROR.
    DELETE ttProgram.
  END.
END.

FIND FIRST ttProgram
     WHERE ttProgram.hWinProc = ihWinProc
     NO-ERROR.
IF AVAIL ttProgram THEN DO:
  IF ttProgram.oChildForm NE ? THEN DO:
    oChildForm = CAST(ttProgram.oChildForm,"Progress.Windows.Form") NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      oChildForm:CLOSE().
  END.
  DELETE OBJECT ttProgram.hWinMenuItem NO-ERROR.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RebuildActionMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RebuildActionMenu Procedure 
PROCEDURE RebuildActionMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN ActivateWindow (iCurrNodeKey).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RestartMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RestartMenu Procedure 
PROCEDURE RestartMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SetRestart IN hParent (YES) NO-ERROR.
RUN CloseMenu.
RUN RestartMenu IN hParent (iMainMenuId).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SelectRestartMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectRestartMenu Procedure 
PROCEDURE SelectRestartMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM oiMenuChoice  AS INT NO-UNDO.

DEF VAR cCompanyMenu AS CHAR NO-UNDO.
DEF VAR cSuperMenu   AS CHAR NO-UNDO.
DEF VAR cReturn      AS CHAR NO-UNDO.

IF DYNAMIC-FUNCTION("getUserLevel") = "super" THEN DO:
  cCompanyMenu = DYNAMIC-FUNCTION("getFieldList",
                                  "JBoxCompanyMenu,JBoxMenu;cMenuLabel;iJBoxMenuId",
                                  "WHERE iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany")
                                + ",FIRST JBoxMenu NO-LOCK OF JBoxCompanyMenu WHERE cMenuType = 'menu'"
                                  ).
  
  IF cCompanyMenu NE "" THEN DO:
    IF DYNAMIC-FUNCTION("runProc","jbsetup_getuser_mainmenu.p","",?) THEN 
      cSuperMenu = DYNAMIC-FUNCTION("getTransactionMessage").
    
    IF LOOKUP(cSuperMenu,cCompanyMenu,"|") = 0 THEN
      RUN JBoxDSimpleSelectList.w (cCompanyMenu + "|" 
                                 + DYNAMIC-FUNCTION("getFieldValues","JBoxMenu",
                                                    "WHERE iJBoxMenuId = " + cSuperMenu,"cMenuLabel,iJBoxMenuId")
                                  ,?
                                  ,OUTPUT cReturn).
    oiMenuChoice = INTEGER(cReturn).
  END.
END.
ELSE IF DYNAMIC-FUNCTION("runProc","jbsetup_getuser_mainmenu.p","",?) THEN 
  oiMenuChoice = INT(DYNAMIC-FUNCTION("getTransactionMessage")).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetCurrTab) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCurrTab Procedure 
PROCEDURE SetCurrTab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iiNodeKey AS INT NO-UNDO.

DEF BUFFER ttProgram FOR ttProgram.

FIND FIRST ttProgram 
     WHERE ttProgram.iNodeIndex = iiNodeKey
     NO-ERROR.
IF AVAIL ttProgram AND ttProgram.oRibbonObj NE ? THEN DO:
  vCurrRibbonTab = ttProgram.oRibbonObj.
  RUN ActivateWindow (iCurrNodeKey).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetRibbonActionState) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetRibbonActionState Procedure 
PROCEDURE SetRibbonActionState :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER ttProgram FOR ttProgram.
FOR EACH ttProgram 
    WHERE ttProgram.iNodeIndex > 1000:
  IF VALID-HANDLE(ttProgram.hTrigWidget) THEN DO:
    IF ttProgram.hTrigWidget:SENSITIVE THEN
      goJBoxMainForm:EnableMenuItem(ttProgram.iNodeIndex).
    ELSE
      goJBoxMainForm:DisableMenuItem(ttProgram.iNodeIndex).
    IF ttProgram.hTrigWidget:TOGGLE-BOX THEN
      goJBoxMainForm:SetStatusMenuItem(ttProgram.oRibbonObj,ttProgram.hTrigWidget:CHECKED).
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StartWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartWindow Procedure 
PROCEDURE StartWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icNodeKey      AS CHAR   NO-UNDO.

DEFINE VARIABLE hWinFrame     AS HANDLE  NO-UNDO.
DEFINE VARIABLE cInitProc     AS CHAR    NO-UNDO.
DEFINE VARIABLE hInitProc     AS HANDLE  NO-UNDO.

SESSION:SET-WAIT-STATE("general").

DEFINE BUFFER ttProgram FOR ttProgram.

FIND FIRST ttProgram 
     WHERE ttProgram.iNodeIndex = INT(icNodeKey)
     NO-ERROR.
IF NOT AVAIL ttProgram THEN RETURN.

IF ttProgram.cLaunchType     = "THIS-PROCEDURE" THEN DO:
  RUN VALUE(ttProgram.cProcName).
  RETURN.
END.
ELSE IF ttProgram.cLaunchType = "URL" THEN DO:
  DYNAMIC-FUNCTION("setWebDoc","",ttProgram.cProcName).  
  RETURN.
END.

IF ttProgram.cLaunchType     = "DATA-BROWSE" THEN
  ASSIGN cInitProc           = ttProgram.cProcName
         ttProgram.cProcName = "JBoxDataBrw.w".

IF ttProgram.cProcName NE "JBoxDataBrw.w" AND VALID-HANDLE(hParameterField) AND hParameterField:BUFFER-VALUE NE "" THEN
  RUN VALUE(ttProgram.cProcName) PERSIST SET ttProgram.hWinProc (hParameterField:BUFFER-VALUE) NO-ERROR.
ELSE 
  RUN VALUE(ttProgram.cProcName) PERSIST SET ttProgram.hWinProc NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  RUN VALUE(ttProgram.cProcName) PERSIST SET ttProgram.hWinProc ("") NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE "Failed to start " ttProgram.cProcName SKIP 
            ERROR-STATUS:GET-MESSAGE(1)
            VIEW-AS ALERT-BOX ERROR.
    DELETE ttProgram.
    RETURN.
  END.
END.

IF VALID-HANDLE(ttProgram.hWinProc:NEXT-SIBLING) AND ttProgram.hWinProc:NEXT-SIBLING:CURRENT-WINDOW NE ? THEN 
  ASSIGN hInitProc          = ttProgram.hWinProc
         ttProgram.hWinProc = ttProgram.hWinProc:NEXT-SIBLING
         hWin               = ttProgram.hWinProc:CURRENT-WINDOW
         .

IF ttProgram.bEmbed THEN DO:
  EmbedMe(ttProgram.hWinProc).
                                                 
  IF bWinStatusArea THEN DO:
    DYNAMIC-FUNCTION("CreateStatusBar",hWin,"",1,YES,?).
    ttProgram.bUseTimer = YES.
  END.
END.

IF ttProgram.cLaunchType = "DATA-BROWSE" THEN DO:
  IF VALID-HANDLE(hParameterField) AND hParameterField:BUFFER-VALUE NE "" THEN
    RUN VALUE(cInitProc) PERSIST SET hInitProc (ttProgram.hWinProc,hParameterField:BUFFER-VALUE).
  ELSE
    RUN VALUE(cInitProc) PERSIST SET hInitProc (ttProgram.hWinProc).

  IF VALID-HANDLE(hInitProc) AND NOT CAN-DO(hInitProc:INTERNAL-ENTRIES,"InvalidateHandle") THEN DELETE PROCEDURE hInitProc.    
END.
ELSE IF VALID-HANDLE(hInitProc) AND CAN-DO(hInitProc:INTERNAL-ENTRIES,"InitializeObject") THEN DO:
  DYNAMIC-FUNCTION("DoLockWindow",hWin).
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").
  RUN InitializeObject IN hInitProc NO-ERROR.
END.
ELSE IF CAN-DO(ttProgram.hWinProc:INTERNAL-ENTRIES,"InitializeObject") THEN DO:
  DYNAMIC-FUNCTION("DoLockWindow",hWin).
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").
  RUN InitializeObject IN ttProgram.hWinProc NO-ERROR.
END.
ELSE IF CAN-DO(ttProgram.hWinProc:INTERNAL-ENTRIES,"local-initialize") THEN DO:
  DYNAMIC-FUNCTION("DoLockWindow",hWin).
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").
  RUN local-initialize IN ttProgram.hWinProc NO-ERROR.
  ttProgram.bUseTimer = YES.
END.
ELSE IF CAN-DO(ttProgram.hWinProc:INTERNAL-ENTRIES,"dispatch") THEN DO:
  DYNAMIC-FUNCTION("DoLockWindow",hWin).
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").
  RUN dispatch IN ttProgram.hWinProc ("initialize") NO-ERROR.
  ttProgram.bUseTimer = YES.
END.
  
IF ttProgram.bEmbed THEN DO:
  RUN ActivateWindow(INT(icNodeKey)).
  RUN SetRibbonActionState.
  
  oChildForm:Activated:Subscribe(goJBoxMainForm:OnMdiChildFormActivated).
  

  /* One way to detect that resize setting are in place: */
  IF DYNAMIC-FUNCTION("getWinMinXmove",hWin,"X") > 0 THEN DO:
    hStatusFrame = ?.
    getWindowFrames(hWin:FIRST-CHILD).
  
    /* The window is automatically sized to fit the available space so to get a delta for the frames to grow or shrink we need to re-assign design-size to current */
    DYNAMIC-FUNCTION("setCurrWidthHeight",hWin,iDesignWidth - 3,iDesignHeight + (IF VALID-HANDLE(hStatusFrame) THEN hStatusFrame:HEIGHT-PIXELS ELSE 0)).
    /* The same values need to be assigned as org. win size if any suppressed windows that are not started on initialize are to be correctly adjusted */
    DYNAMIC-FUNCTION("setOrgWidthHeight",hWin,iDesignWidth - 3,iDesignHeight + (IF VALID-HANDLE(hStatusFrame) THEN hStatusFrame:HEIGHT-PIXELS ELSE 0)).
    
    /* The status-bar frame is created after the window was resized in MDIChildForm and therefore it shouldn't be touched this one time: */
    DYNAMIC-FUNCTION("setOneTimeException",hStatusFrame).
    
    APPLY "WINDOW-RESIZED" TO hWin.
  END.
END.
ELSE DO:
  /*   IF httMenuBuffer:AVAIL THEN DO:                                                        */
  /*     IF httMenuBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE MATCHES "*.ico" THEN            */
  /*       hWin:LOAD-ICON(STRING(httMenuBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE)).         */
  /*     ELSE IF httMenuBuffer:BUFFER-FIELD("cParentImage"):BUFFER-VALUE MATCHES "*.ico" THEN */
  /*       hWin:LOAD-ICON(STRING(httMenuBuffer:BUFFER-FIELD("cParentImage"):BUFFER-VALUE)).   */
  /*     ELSE IF hWin:ICON = "" AND cAppImage NE "" THEN                                      */
  /*       hWin:LOAD-ICON(cAppImage).                                                         */
  /*   END.                                                                                   */
    
END.

DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
DYNAMIC-FUNCTION("DoLockWindow",?). 
SESSION:SET-WAIT-STATE("").

IF ttProgram.cProcName NE "JBoxDataBrw.w" THEN DO:
  IF CAN-DO(ttProgram.hWinProc:INTERNAL-ENTRIES,"MoveToTop") THEN 
    RUN MoveToTop IN ttProgram.hWinProc.
  ELSE DO:
    hWinFrame = hWin:FIRST-CHILD NO-ERROR.
    IF ttProgram.hWinProc:ADM-DATA NE ? THEN DO:
      hWin:VISIBLE = NO.
      hWin:VISIBLE = YES.
    END.
    IF VALID-HANDLE(hStatusFrame) AND hStatusFrame:WINDOW = hWin THEN
      hStatusFrame:MOVE-TO-TOP().
    IF VALID-HANDLE(hWinFrame) THEN 
      APPLY "entry" TO hWinFrame.
  END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-BuildActionRibbonGroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION BuildActionRibbonGroup Procedure 
FUNCTION BuildActionRibbonGroup RETURNS LOGICAL
  (INPUT ihMenu       AS HANDLE,
   INPUT ioParent     AS CLASS Progress.Lang.Object,
   INPUT icParentType AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hChild        AS HANDLE NO-UNDO.
DEF VAR cDummy        AS CHAR   NO-UNDO.
DEF VAR bFirstInGroup AS LOG    NO-UNDO.

DEF BUFFER ttProgram FOR ttProgram.

/* hChild = ihMenu:FIRST-CHILD. */
hChild = ihMenu.

REPEAT WHILE VALID-HANDLE(hChild):
  CASE hChild:TYPE:
    WHEN "SUB-MENU" THEN DO:
      CREATE ttProgram.
      ASSIGN ttProgram.iNodeIndex = ixNode
             ttProgram.oRibbonObj = goJBoxMainForm:AddRibbonPopupMenuTool(
                                    ioParent,icParentType,ttProgram.iNodeIndex,hChild:LABEL,"","",NO,bFirstInGroup,OUTPUT ttProgram.cObjectType)
             ixNode               = ixNode + 1
             bFirstInGroup        = NO
             .
      IF VALID-HANDLE(hChild:FIRST-CHILD) THEN
        BuildActionRibbonGroup(hChild:FIRST-CHILD,ttProgram.oRibbonObj,ttProgram.cObjectType).
    END.
    WHEN "MENU-ITEM" THEN DO:
      IF hChild:SUBTYPE = "RULE" THEN
        bFirstInGroup = YES.
      ELSE DO:
        CREATE ttProgram.
        ASSIGN ttProgram.iNodeIndex   = ixNode
               ttProgram.oRibbonObj   = IF hChild:TOGGLE-BOX THEN
                                          goJBoxMainForm:AddRibbonStateButtonTool(
                                                 ioParent,icParentType,ttProgram.iNodeIndex,hChild:LABEL,
                                                 "",hChild:ACCELERATOR,NO,bFirstInGroup,OUTPUT ttProgram.cObjectType)
                                        ELSE
                                          goJBoxMainForm:AddRibbonButtonTool(
                                                 ioParent,icParentType,ttProgram.iNodeIndex,hChild:LABEL,
                                                 "",hChild:ACCELERATOR,NO,bFirstInGroup,OUTPUT ttProgram.cObjectType)
               ttProgram.hTrigWidget  = hChild
               ttProgram.cAccelerator = hChild:ACCELERATOR
               ixNode                 = ixNode + 1
               bFirstInGroup          = NO
               .
      END.
    END.
  END CASE.
  hChild = hChild:NEXT-SIBLING.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ClearActionRibbonGroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ClearActionRibbonGroup Procedure 
FUNCTION ClearActionRibbonGroup RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF vGrpWinMenu NE ? THEN DO:
  goJBoxMainForm:RemoveRibbonGroup(vGrpWinMenu).
  FOR EACH ttProgram
      WHERE ttProgram.iNodeIndex > 1000:
    DELETE ttProgram.
  END.
  vGrpWinMenu = ?.
  RETURN YES.
END.
RETURN NO.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EmbedMe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EmbedMe Procedure 
FUNCTION EmbedMe RETURNS LOGICAL
  ( INPUT ihProc AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: Might be called from the procedure itself. For ADM this call should
           be added to windowmn.i before create-objects:
            
    DYNAMIC-FUNCTION("EmbedMe" IN SOURCE-PROCEDURE,THIS-PROCEDURE) NO-ERROR.
------------------------------------------------------------------------------*/
IF ihProc = hEmbedded THEN RETURN NO.

ASSIGN hWin          = ihProc:CURRENT-WINDOW
       iDesignWidth  = hWin:WIDTH-PIXELS 
       iDesignHeight = hWin:HEIGHT-PIXELS   
       .

IF NOT ttProgram.bEmbed THEN RETURN NO.

IF VALID-HANDLE(hWin:MENUBAR) THEN
  ttProgram.hTrigWidget = hWin:MENUBAR.
bWinStatusArea = hWin:STATUS-AREA.


oChildForm = NEW MDIChildForm(goJBoxMainForm, hWin).

oChildForm:Tag = ttProgram.iNodeIndex.
/*   oChildForm:Icon = goJBoxMainForm:Icon. */
oChildForm:Text = REPLACE(ttProgram.cMenuLabel,"&","").

oChildForm:FormClosing:Subscribe(goJBoxMainForm:OnMdiChildFormClosing).
oChildForm:Show().

ttProgram.oChildForm = CAST(oChildForm,"Progress.Lang.Object").

hEmbedded = ihProc.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWindowFrames) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getWindowFrames Procedure 
FUNCTION getWindowFrames RETURNS HANDLE
  ( INPUT hWidget AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  List widgets to file
    Notes:  NB! Use level = 0 for initial call
------------------------------------------------------------------------------*/
REPEAT WHILE hWidget NE ?:
  IF hWidget:TYPE = "FRAME" THEN DO:
    IF hWidget:NAME = "StatusFrame" THEN 
      hStatusFrame = hWidget.
/*     hWidget:BGCOLOR = ?.  */
  END.
  IF CAN-QUERY(hWidget,'FIRST-CHILD') AND hWidget:FIRST-CHILD  <> ? THEN
    getWindowFrames (hWidget:FIRST-CHILD).

  hWidget = hWidget:NEXT-SIBLING.
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAppStyleSheet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAppStyleSheet Procedure 
FUNCTION setAppStyleSheet RETURNS LOGICAL
  ( INPUT icStyleSheet AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
goJBoxMainForm:setAppStyleSheet(icStyleSheet).

/* goJBoxMainForm:setAppStyleSheet("vs2008_test.isl"). */
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setClientSize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setClientSize Procedure 
FUNCTION setClientSize RETURNS LOGICAL
  ( INPUT iiClientXsize AS INT,
    INPUT iiClientYsize AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN iClientXsize = iiClientXsize
       iClientYsize = iiClientYsize
       .

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setMenuBgImage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setMenuBgImage Procedure 
FUNCTION setMenuBgImage RETURNS LOGICAL
  ( INPUT icBgImage AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
goJBoxMainForm:setBgImage(icBgImage).

/* goJBoxMainForm:setBgImage("bg-green.bmp"). */
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setRibbonIconImage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setRibbonIconImage Procedure 
FUNCTION setRibbonIconImage RETURNS LOGICAL
  ( INPUT icImageName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
goJBoxMainForm:setRibbonIconImage(icImageName).

/* goJBoxMainForm:setRibbonIconImage("j0433808.png"). */
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setStatusText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setStatusText Procedure 
FUNCTION setStatusText RETURNS LOGICAL
  ( INPUT icStatusText AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
goJBoxMainForm:setStatusText(icStatusText).
/* goJBoxMainForm:setStatusText("Brynjar"). */

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setWindowTitle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setWindowTitle Procedure 
FUNCTION setWindowTitle RETURNS LOGICAL
  ( INPUT icTitle AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
goJBoxMainForm:setWindowTitle(icTitle).

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setWinMenuGroupLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setWinMenuGroupLabel Procedure 
FUNCTION setWinMenuGroupLabel RETURNS LOGICAL
  ( INPUT icWinMenuGrpLabel AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cWinMenuGrpLabel = icWinMenuGrpLabel.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

