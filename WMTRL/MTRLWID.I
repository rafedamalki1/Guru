/*MTRLWID.I*/
{WHANDLTEMP.I}
   
DEFINE VARIABLE C-WIN AS HANDLE NO-UNDO.
DEFINE VARIABLE BRW_HLEV      AS HANDLE NO-UNDO.   
DEFINE VARIABLE BRW_MTRL       AS HANDLE NO-UNDO.  
DEFINE VARIABLE btn_over     AS HANDLE NO-UNDO.  
DEFINE VARIABLE FBTN_MTRL     AS HANDLE NO-UNDO.   
DEFINE VARIABLE FBTN_VISA     AS HANDLE NO-UNDO.   
DEFINE VARIABLE btn_back     AS HANDLE NO-UNDO.  
DEFINE VARIABLE FBTN_OFF       AS HANDLE NO-UNDO.  
DEFINE VARIABLE FBTN_SKRIV     AS HANDLE NO-UNDO.  
DEFINE VARIABLE CMB_LEV        AS HANDLE NO-UNDO.  
DEFINE VARIABLE BTN_LEV        AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-ENR    AS HANDLE NO-UNDO.  
DEFINE VARIABLE BTN_UP         AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-BEN  AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-ENR2   AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-ANTAL  AS HANDLE NO-UNDO.  
DEFINE VARIABLE BTN_MIN        AS HANDLE NO-UNDO.  
DEFINE VARIABLE RAD_SOK        AS HANDLE NO-UNDO.  
DEFINE VARIABLE BTN_AVB      AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-SOKALT AS HANDLE NO-UNDO.  
DEFINE VARIABLE RECT-4         AS HANDLE NO-UNDO.  
DEFINE VARIABLE IMAGE-1        AS HANDLE NO-UNDO.  
DEFINE VARIABLE RECT-2         AS HANDLE NO-UNDO.  
DEFINE VARIABLE MENUVISINF       AS HANDLE NO-UNDO.  
DEFINE VARIABLE MENUVISINF2       AS HANDLE NO-UNDO.
DEFINE VARIABLE m_AvmarkeraHLEV       AS HANDLE NO-UNDO.
DEFINE VARIABLE m_Sats_information       AS HANDLE NO-UNDO.
DEFINE VARIABLE m_Sats_information2       AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_IEXC       AS HANDLE NO-UNDO.  
DEFINE VARIABLE mtrlantal AS HANDLE NO-UNDO.
DEFINE VARIABLE TOG_ENR7   AS HANDLE NO-UNDO.
DEFINE VARIABLE FRAME-MTRL AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_JMFPRIS  AS HANDLE NO-UNDO.
DEFINE VARIABLE TOG_LEVMTRL AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_AOF AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_HAOF AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER huvprogh AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER framesizeextrah AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR whandltemp.
DEFINE VARIABLE ordnr AS INTEGER NO-UNDO.
DEFINE VARIABLE brworgwith  AS DECIMAL NO-UNDO.
DEFINE VARIABLE brworghight AS DECIMAL NO-UNDO.
DEFINE VARIABLE brworgrow  AS DECIMAL NO-UNDO.
DEFINE VARIABLE brworgcol  AS DECIMAL NO-UNDO.
DEFINE VARIABLE btnorgrow  AS DECIMAL NO-UNDO.
DEFINE VARIABLE btnorgcol  AS DECIMAL NO-UNDO.

DEFINE SHARED VARIABLE satsinn AS LOGICAL NO-UNDO.  /*lena satsinformation i mtrlmeny*/
DEFINE SHARED VARIABLE satstrue AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE satsmtrl AS LOGICAL NO-UNDO.

PROCEDURE franstart_UI :   
   RUN main_UI.
END PROCEDURE.
FIND FIRST whandltemp WHERE NO-LOCK NO-ERROR.
ordnr = 0.
ASSIGN
ordnr = ordnr + 1
C-WIN = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BRW_HLEV       = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
BRW_MTRL        = whandltemp.WF[ordnr]  
ordnr = ordnr + 1
btn_over      = whandltemp.WF[ordnr]  
ordnr = ordnr + 1
FBTN_MTRL      = whandltemp.WF[ordnr]   
ordnr = ordnr + 1
FBTN_VISA      = whandltemp.WF[ordnr]   
ordnr = ordnr + 1
btn_back      = whandltemp.WF[ordnr]  
ordnr = ordnr + 1
FBTN_OFF        = whandltemp.WF[ordnr]  
ordnr = ordnr + 1
FBTN_SKRIV      = whandltemp.WF[ordnr]  
ordnr = ordnr + 1
CMB_LEV         = whandltemp.WF[ordnr]  
ordnr = ordnr + 1
BTN_LEV         = whandltemp.WF[ordnr]  
ordnr = ordnr + 1
FILL-IN-ENR     = whandltemp.WF[ordnr]  
ordnr = ordnr + 1
BTN_UP          = whandltemp.WF[ordnr]  
ordnr = ordnr + 1
FILL-IN-BEN   = whandltemp.WF[ordnr]  
ordnr = ordnr + 1
FILL-IN-ENR2    = whandltemp.WF[ordnr]  
ordnr = ordnr + 1
FILL-IN-ANTAL   = whandltemp.WF[ordnr]  
ordnr = ordnr + 1
BTN_MIN         = whandltemp.WF[ordnr]  
ordnr = ordnr + 1
RAD_SOK         = whandltemp.WF[ordnr]  
ordnr = ordnr + 1
BTN_AVB       = whandltemp.WF[ordnr]  
ordnr = ordnr + 1
FILL-IN-SOKALT  = whandltemp.WF[ordnr]  
ordnr = ordnr + 1
RECT-4          = whandltemp.WF[ordnr]  
ordnr = ordnr + 1
IMAGE-1         = whandltemp.WF[ordnr]  
ordnr = ordnr + 1
RECT-2          = whandltemp.WF[ordnr]
ordnr = ordnr + 1
/*
MENUVISINF      = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
MENUVISINF2      = whandltemp.WF[ordnr]
ordnr = ordnr + 1
*/
BTN_IEXC      = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
mtrlantal = whandltemp.WF[ordnr]
ordnr = ordnr + 1
m_AvmarkeraHLEV     = whandltemp.WF[ordnr]
ordnr = ordnr + 1
m_Sats_information    = whandltemp.WF[ordnr]
ordnr = ordnr + 1
m_Sats_information2    = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
TOG_ENR7    = whandltemp.WF[ordnr]  

ordnr = ordnr + 1
FRAME-MTRL    = whandltemp.WF[ordnr]

ordnr = ordnr + 1
BTN_JMFPRIS   = whandltemp.WF[ordnr]
 

ordnr = ordnr + 1
TOG_LEVMTRL   = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_AOF   = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_HAOF   = whandltemp.WF[ordnr].

ASSIGN
brworgwith = BRW_MTRL:WIDTH-CHARS
brworghight = BRW_MTRL:HEIGHT-CHARS
brworgrow = BRW_MTRL:ROW
brworgcol = BRW_MTRL:COLUMN.
