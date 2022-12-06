/*DEPWID.I*/
{WHANDLTEMP.I}
   
DEFINE VARIABLE C-WIN AS HANDLE NO-UNDO.
DEFINE VARIABLE      BRW_DEPA          AS HANDLE NO-UNDO.    
DEFINE VARIABLE      Fbtn_best          AS HANDLE NO-UNDO.   
DEFINE VARIABLE      Fbtn_lager       AS HANDLE NO-UNDO.     
DEFINE VARIABLE      FBTN_RAPP       AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FBTN_BSTAT        AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FBTN_LEV         AS HANDLE NO-UNDO.     
DEFINE VARIABLE      Fbtn_inventering   AS HANDLE NO-UNDO.   
DEFINE VARIABLE      FBTN_UT            AS HANDLE NO-UNDO.   
DEFINE VARIABLE      RAD_PERIOD         AS HANDLE NO-UNDO.   
DEFINE VARIABLE      FBTN_LEVE          AS HANDLE NO-UNDO.   
DEFINE VARIABLE      FBTN_MTRL        AS HANDLE NO-UNDO.   
DEFINE VARIABLE      FBTN_MTRLPRIS      AS HANDLE NO-UNDO.   
DEFINE VARIABLE      FBTN_SEK         AS HANDLE NO-UNDO.     
DEFINE VARIABLE      FBTN_VISA        AS HANDLE NO-UNDO.   
DEFINE VARIABLE      BTN_NY           AS HANDLE NO-UNDO.   
DEFINE VARIABLE      BTN_AND            AS HANDLE NO-UNDO.   
DEFINE VARIABLE      BTN_BORT         AS HANDLE NO-UNDO.   
DEFINE VARIABLE      FBTN_LAS       AS HANDLE NO-UNDO.     
DEFINE VARIABLE      BTN_AVB         AS HANDLE NO-UNDO.   
DEFINE VARIABLE FRAME-DEPALOGG AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_LOGGIN AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_LOGGOUT AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN_DEPANVANDARE AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN_AV-DEPLOSEN AS HANDLE NO-UNDO.
DEFINE VARIABLE FRAME-DEPA AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN_AV-DEPANVNAMN AS HANDLE NO-UNDO.


DEFINE INPUT PARAMETER huvprogh AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER btnguruh AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER framesizeextrah AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR whandltemp.


DEFINE VARIABLE ordnr AS INTEGER NO-UNDO.
DEFINE VARIABLE brworgwith  AS DECIMAL NO-UNDO.
DEFINE VARIABLE brworghight AS DECIMAL NO-UNDO.
DEFINE VARIABLE brworgrow  AS DECIMAL NO-UNDO.
DEFINE VARIABLE brworgcol  AS DECIMAL NO-UNDO.
DEFINE VARIABLE btnorgrow  AS DECIMAL NO-UNDO.
DEFINE VARIABLE btnorgcol  AS DECIMAL NO-UNDO.

PROCEDURE franstart_UI :   
   RUN main_UI.
END PROCEDURE.
FIND FIRST whandltemp WHERE NO-LOCK NO-ERROR.
ordnr = 0.
ASSIGN
ordnr = ordnr + 1
C-WIN = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BRW_DEPA         = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
Fbtn_best         = whandltemp.WF[ordnr]   
ordnr = ordnr + 1
Fbtn_lager      = whandltemp.WF[ordnr]     
ordnr = ordnr + 1
FBTN_RAPP      = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FBTN_BSTAT       = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FBTN_LEV        = whandltemp.WF[ordnr]     
ordnr = ordnr + 1
Fbtn_inventering  = whandltemp.WF[ordnr]   
ordnr = ordnr + 1
FBTN_UT           = whandltemp.WF[ordnr]   
ordnr = ordnr + 1
RAD_PERIOD        = whandltemp.WF[ordnr]   
ordnr = ordnr + 1
FBTN_LEVE         = whandltemp.WF[ordnr]   
ordnr = ordnr + 1
FBTN_MTRL       = whandltemp.WF[ordnr]   
ordnr = ordnr + 1
FBTN_MTRLPRIS     = whandltemp.WF[ordnr]   
ordnr = ordnr + 1
FBTN_SEK        = whandltemp.WF[ordnr]     
ordnr = ordnr + 1
FBTN_VISA       = whandltemp.WF[ordnr]   
ordnr = ordnr + 1
BTN_NY          = whandltemp.WF[ordnr]   
ordnr = ordnr + 1
BTN_AND           = whandltemp.WF[ordnr]   
ordnr = ordnr + 1
BTN_BORT        = whandltemp.WF[ordnr]   
ordnr = ordnr + 1
FBTN_LAS      = whandltemp.WF[ordnr]     
ordnr = ordnr + 1
BTN_AVB        = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FRAME-DEPALOGG        = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_LOGGIN        = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_LOGGOUT        = whandltemp.WF[ordnr]
ordnr = ordnr + 1
FILL-IN_DEPANVANDARE        = whandltemp.WF[ordnr]
ordnr = ordnr + 1
FILL-IN_AV-DEPLOSEN        = whandltemp.WF[ordnr]
ordnr = ordnr + 1
FRAME-DEPA        = whandltemp.WF[ordnr]
ordnr = ordnr + 1
FILL-IN_AV-DEPANVNAMN        = whandltemp.WF[ordnr].




ASSIGN
brworgwith = BRW_DEPA:WIDTH-CHARS
brworghight = BRW_DEPA:HEIGHT-CHARS
brworgrow = BRW_DEPA:ROW
brworgcol = BRW_DEPA:COLUMN.
