DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO. 
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.  

DEFINE {&NEW} {&SHARED} TEMP-TABLE utsokaonr NO-UNDO
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD OMRADE AS CHARACTER
   FIELD ORT AS CHARACTER
   INDEX AONR IS PRIMARY AONR DELNR.

DEFINE TEMP-TABLE valdaao  NO-UNDO
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD ORT AS CHARACTER
   INDEX AONR IS PRIMARY AONR DELNR.
DEFINE QUERY BRW_AONR FOR 
      utsokaonr SCROLLING.

DEFINE QUERY BRW_VAONR FOR 
      valdaao SCROLLING.
DEFINE BUTTON BTN_ALLBACK 
     LABEL "Alla ":L 
     SIZE 5.5 BY 1.77 TOOLTIP "Alla valda aonr tas bort från vallistan".
DEFINE BUTTON BTN_BACK 
     LABEL "Back":L 
     SIZE 5.5 BY 1.77 TOOLTIP "Markerade tas bort från vallistan".
DEFINE BROWSE BRW_AONR
QUERY BRW_AONR NO-LOCK DISPLAY
      utsokaonr.OMRADE COLUMN-LABEL "Område" FORMAT "X(6)":U
      utsokaonr.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      utsokaonr.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      utsokaonr.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(40)":U
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 47 BY 9.95
         TITLE "Urvalsresultat".

DEFINE BROWSE BRW_VAONR
QUERY BRW_VAONR NO-LOCK DISPLAY
      valdaao.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      valdaao.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      valdaao.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(40)":U
   WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 21.75 BY 9.95
            TITLE "Arbeta vidare med".

DEFINE FRAME DEFAULT-FRAME
     BRW_AONR AT ROW 9.73 COL 2
     BRW_VAONR AT ROW 9.73 COL 58
     BTN_BACK AT ROW 15.77 COL 51
     BTN_ALLBACK AT ROW 17.95 COL 51
     
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98.13 BY 25.45.
&Scoped-define WINDOW-NAME C-Win
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.
&Scoped-define FRAME-NAME DEFAULT-FRAME
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
&Scoped-define PROCEDURE-TYPE WINDOW  
CREATE WINDOW C-Win ASSIGN
         HIDDEN             = yes
         TITLE              = "<insert window title>"
         HEIGHT             = 25
         WIDTH              = 98.75
         MAX-HEIGHT         = 25.45
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 25.45
         VIRTUAL-WIDTH      = 100
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.


ON CHOOSE OF BTN_ALLBACK IN FRAME DEFAULT-FRAME /* Alla aonr i listan */
DO:
   FOR EACH valdaao:
      DELETE valdaao.
   END.
   status-ok = BRW_VAONR:REFRESH() IN FRAME {&FRAME-NAME} NO-ERROR.    
END.
ON CHOOSE OF BTN_BACK IN FRAME DEFAULT-FRAME
DO:        
   antal_valda = BRW_VAONR:NUM-SELECTED-ROWS.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                      
      status-ok = BRW_VAONR:FETCH-SELECTED-ROW(antal_raknare).                                  
      DELETE valdaao.    
      antal_raknare = antal_raknare + 1.   
   END.      
   BRW_VAONR:SELECT-ROW(1).      
   status-ok = BRW_VAONR:REFRESH() IN FRAME {&FRAME-NAME} NO-ERROR.
END.


MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   &Scoped-define FORMATNAMN utsokaonr.AONR
   &Scoped-define BROWSE-NAME BRW_AONR
   ON ROW-DISPLAY OF {&BROWSE-NAME} IN FRAME {&FRAME-NAME} 
   DO:
     {&FORMATNAMN}:FORMAT IN BROWSE {&BROWSE-NAME} = "X(7)" .     
   END.
   {&FORMATNAMN}:WIDTH-CHARS IN BROWSE {&BROWSE-NAME} = 7.
   &Scoped-define FORMATNAMN valdaao.AONR
   &Scoped-define BROWSE-NAME BRW_VAONR
   ON ROW-DISPLAY OF {&BROWSE-NAME} IN FRAME {&FRAME-NAME} 
   DO:
     {&FORMATNAMN}:FORMAT IN BROWSE {&BROWSE-NAME} = "X(7)" .     
   END.
   {&FORMATNAMN}:WIDTH-CHARS IN BROWSE {&BROWSE-NAME} = 7.
   CREATE valdaao.
   valdaao.aonr = "1234567".
   CREATE valdaao.
   valdaao.aonr = "2345678".
   RUN ENABLE_UI.
   {&WINDOW-NAME}:HIDDEN = FALSE.
   OPEN QUERY brw_vaonr FOR EACH valdaao.
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

PROCEDURE enable_UI :
  ENABLE  
         BRW_AONR 
         BRW_VAONR BTN_BACK BTN_ALLBACK 
           WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  
END PROCEDURE.



