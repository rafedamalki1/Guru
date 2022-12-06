/* e-clpbrd.p */

DEFINE VARIABLE Stat AS LOGICAL.
DEFINE VARIABLE MainWindow AS WIDGET-HANDLE.
DEFINE VARIABLE Editor  AS CHARACTER 
       VIEW-AS EDITOR SIZE 20 BY 4 SCROLLBAR-VERTICAL.
DEFINE VARIABLE Fillin AS CHARACTER FORMAT "x(20)".
DEFINE VARIABLE TogDemo AS LOGICAL EXTENT 2
       INITIAL ["FALSE", "TRUE"]
       LABEL "Pick-Me1",
             "Pick-Me2"
       VIEW-AS TOGGLE-BOX.
DEFINE VARIABLE Radios AS INTEGER INITIAL 3
       LABEL "Time-O-Day"
       VIEW-AS RADIO-SET RADIO-BUTTONS
       "1pm", 1,
       "2pm", 2,
       "3pm", 3.
DEFINE VARIABLE Slider1 AS INTEGER
       VIEW-AS SLIDER MAX-VALUE 100 MIN-VALUE 10 LABEL "Slide Me:".
DEFINE VARIABLE Slider2 AS INTEGER
       VIEW-AS SLIDER MAX-VALUE 1000 MIN-VALUE 100 VERTICAL LABEL "Slide Me:".
DEFINE VARIABLE SelectList AS CHARACTER
       VIEW-AS SELECTION-LIST
       SINGLE SIZE 23 BY 7
       LIST-ITEMS
       "Line 1",
       "Line 2",
       "Line 3",
       "Line 4",
       "Line 5",
       "Line 6",
       "Line 7",
       "Line 8",
       "Line 9",
       "Line 10",
       "Line 11",
       "Line 12",
       "Line 13",
       "Line 14",
       "Line 15".

DEFINE SUB-MENU FileMenu
    MENU-ITEM FM_New       LABEL "&New"
    MENU-ITEM FM_Open      LABEL "&Open... "
    RULE
    MENU-ITEM FM_Save      LABEL "&Save "
    MENU-ITEM FM_Save_as   LABEL "Save &As... "
    RULE
    MENU-ITEM FM_Exit      LABEL "E&xit ".

DEFINE SUB-MENU EditMenu
    MENU-ITEM EM_Cut       LABEL "&Cut "
    MENU-ITEM EM_Copy      LABEL "C&opy "
    MENU-ITEM EM_Paste     LABEL "&Paste ".

DEFINE MENU MainMenu
    MENUBAR
    SUB-MENU  FileMenu     LABEL "&File "
    SUB-MENU  EditMenu     LABEL "&Edit ".

DEFINE BUTTON b_OK         LABEL "   OK   ".
DEFINE BUTTON b_Cancel     LABEL " CANCEL ".

FORM
    "Enter Text Here" AT ROW 1 COLUMN 2
    Editor AT ROW 2 COLUMN 2
    "Fill In Here" AT ROW 1 COLUMN 39
    Fillin AT ROW 2 COLUMN 39
    TogDemo[1] AT ROW 2 COLUMN 25
    TogDemo[2] AT ROW 4 COLUMN 25
    Radios AT ROW 7 COLUMN 2
    "Selection List" AT ROW 6 COLUMN 22
    SelectList AT ROW 7 COLUMN 17
    Slider2 AT ROW 7 COLUMN 45
    Slider1 AT ROW 12 COLUMN 43
    b_OK AT ROW 12 COLUMN 2
    b_Cancel AT ROW 14 COLUMN 2
    SKIP (0.5)
WITH FRAME MainFrame NO-LABEL
     CENTERED
     WIDTH 62.

ON CHOOSE OF b_OK IN FRAME MainFrame MESSAGE "OK pressed".
ON CHOOSE OF b_Cancel IN FRAME MainFrame MESSAGE "CANCEL pressed".
ON CHOOSE OF MENU-ITEM FM_Exit IN MENU FileMenu STOP.

/****** Begin Clipboard Code ******/
ON MENU-DROP OF MENU EditMenu DO:
    IF FOCUS:TYPE = "EDITOR" THEN DO:
       MENU-ITEM EM_Cut:SENSITIVE IN MENU EditMenu =
          IF  LENGTH(FOCUS:SELECTION-TEXT) > 0
          THEN TRUE
          ELSE FALSE.
       MENU-ITEM Em_Copy:SENSITIVE IN MENU EditMenu =
          IF  LENGTH(FOCUS:SELECTION-TEXT) > 0
          THEN TRUE
          ELSE FALSE.
       MENU-ITEM EM_Paste:SENSITIVE IN MENU EditMenu =
          IF CLIPBOARD:NUM-FORMATS > 0 
          THEN TRUE 
          ELSE FALSE.
    END.
    ELSE IF FOCUS:TYPE = "RADIO-SET"      OR 
            FOCUS:TYPE = "SELECTION-LIST" OR
            FOCUS:TYPE = "SLIDER"         OR
            FOCUS:TYPE = "TOGGLE-BOX" THEN DO:
       MENU-ITEM EM_Cut:SENSITIVE IN MENU EditMenu = FALSE.
       MENU-ITEM Em_Copy:SENSITIVE IN MENU EditMenu = TRUE.
       MENU-ITEM Em_Paste:SENSITIVE IN MENU EditMenu = FALSE.
    END.
    ELSE IF FOCUS:TYPE = "FILL-IN" THEN DO:
       MENU-ITEM EM_Cut:SENSITIVE IN MENU EditMenu =
          IF LENGTH(FOCUS:SCREEN-VALUE) > 0
          THEN TRUE
          ELSE FALSE.
       MENU-ITEM Em_Copy:SENSITIVE IN MENU EditMenu =
          IF LENGTH(FOCUS:SCREEN-VALUE) > 0
          THEN TRUE
          ELSE FALSE.
       MENU-ITEM EM_Paste:SENSITIVE IN MENU EDitMenu =
          IF CLIPBOARD:NUM-FORMATS > 0 
          THEN TRUE 
          ELSE FALSE.
    END.
    ELSE DO:
        MENU-ITEM EM_Cut:SENSITIVE IN MENU EditMenu = FALSE.
        MENU-ITEM EM_Copy:SENSITIVE IN MENU EditMenu = FALSE.
        MENU-ITEM EM_Paste:SENSITIVE IN MENU EditMenu = FALSE.
    END.
END. /* ON MENU-DROP IN EditMenu */

ON CHOOSE OF MENU-ITEM EM_Cut IN MENU EditMenu DO:
    IF FOCUS:TYPE = "EDITOR" THEN DO:
        IF FOCUS:SELECTION-START <> FOCUS:SELECTION-END THEN DO:
            CLIPBOARD:VALUE = FOCUS:SELECTION-TEXT.
            Stat = FOCUS:REPLACE-SELECTION-TEXT("").
        END.
        ELSE DO:
            CLIPBOARD:VALUE = FOCUS:SCREEN-VALUE.
            FOCUS:SCREEN-VALUE = "".
        END.
    END.
    ELSE DO: /* For FILL-IN */
        CLIPBOARD:VALUE = FOCUS:SCREEN-VALUE.
        FOCUS:SCREEN-VALUE = "".
    END.
END. /* ON CHOOSE OF MENU-ITEM EM_Cut */

ON CHOOSE OF MENU-ITEM EM_Copy IN MENU EditMenu DO:
    IF FOCUS:TYPE = "EDITOR" THEN
        IF FOCUS:SELECTION-START <> FOCUS:SELECTION-END THEN
            CLIPBOARD:VALUE = FOCUS:SELECTION-TEXT.
        ELSE 
            CLIPBOARD:VALUE = FOCUS:SCREEN-VALUE.
    ELSE IF FOCUS:TYPE = "RADIO-SET" THEN
        CLIPBOARD:VALUE = ENTRY(LOOKUP(FOCUS:SCREEN-VALUE, 
                                       FOCUS:RADIO-BUTTONS) - 1, 
                                FOCUS:RADIO-BUTTONS).
    ELSE IF FOCUS:TYPE = "TOGGLE-BOX" THEN
        IF FOCUS:SCREEN-VALUE = "yes" THEN
            CLIPBOARD:VALUE = FOCUS:LABEL + " selected.".
        ELSE
            CLIPBOARD:VALUE = FOCUS:LABEL + " not selected.".
    ELSE /* For FILL-IN */
        CLIPBOARD:VALUE = FOCUS:SCREEN-VALUE.
END. /* ON CHOOSE OF MENU-ITEM EM_Copy */

ON CHOOSE OF MENU-ITEM EM_Paste IN MENU EditMenu DO:
    IF FOCUS:TYPE = "EDITOR" THEN DO:
        IF FOCUS:SELECTION-START <> FOCUS:SELECTION-END THEN
            Stat = FOCUS:REPLACE-SELECTION-TEXT(CLIPBOARD:VALUE).
        ELSE 
            Stat = FOCUS:INSERT-STRING(CLIPBOARD:VALUE).
    END.
    ELSE /* For FILL-IN */
        FOCUS:SCREEN-VALUE = CLIPBOARD:VALUE.
END. /* ON CHOOSE OF MENU-ITEM EM_Paste */
/****** End Clipboard Code ******/

CREATE WINDOW MainWindow
    ASSIGN
        X = 0 Y = 0
        MENUBAR = MENU MainMenu:HANDLE
        TITLE = "CLIPBOARD SUPPORT".

CURRENT-WINDOW = MainWindow.

ON WINDOW-CLOSE OF MainWindow STOP.

ENABLE ALL WITH FRAME MainFrame.
STATUS DEFAULT "Widgets and the Clipboard".

WAIT-FOR CHOOSE OF b_Cancel IN FRAME MainFrame.
DELETE WIDGET MainWindow.












