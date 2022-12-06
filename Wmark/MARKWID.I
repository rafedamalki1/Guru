/*MARKWID.I*/

{WHANDLTEMP.I}
   
DEFINE VARIABLE C-WIN AS HANDLE NO-UNDO.
DEFINE VARIABLE TOG_AKT              AS HANDLE NO-UNDO. 
DEFINE VARIABLE TOG_INAKT            AS HANDLE NO-UNDO. 
DEFINE VARIABLE CMB_OMR              AS HANDLE NO-UNDO. 
DEFINE VARIABLE CMB_UTF              AS HANDLE NO-UNDO. 
DEFINE VARIABLE CMB_ANSV             AS HANDLE NO-UNDO. 
DEFINE VARIABLE BTN_HAMT             AS HANDLE NO-UNDO. 
DEFINE VARIABLE BRW_URMARK             AS HANDLE NO-UNDO. 
DEFINE VARIABLE BRW_VMARK              AS HANDLE NO-UNDO. 
DEFINE VARIABLE FBTN_UPP             AS HANDLE NO-UNDO. 
DEFINE VARIABLE BTN_AOF              AS HANDLE NO-UNDO. 
DEFINE VARIABLE BTN_HAOF AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_EXPM AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_IMPM AS HANDLE NO-UNDO.
DEFINE VARIABLE FBTN_FASTIGHET         AS HANDLE NO-UNDO. 
DEFINE VARIABLE BTN_ALLOVER          AS HANDLE NO-UNDO. 
DEFINE VARIABLE FBTN_VARD              AS HANDLE NO-UNDO. 
DEFINE VARIABLE FBTN_VISKAL          AS HANDLE NO-UNDO. 
DEFINE VARIABLE BTN_OVER             AS HANDLE NO-UNDO. 
DEFINE VARIABLE FBTN_VISMARK           AS HANDLE NO-UNDO. 
DEFINE VARIABLE FBTN_OMRAKNA           AS HANDLE NO-UNDO. 
DEFINE VARIABLE BTN_BACK             AS HANDLE NO-UNDO. 
DEFINE VARIABLE FBTN_INAKTIV         AS HANDLE NO-UNDO. 
DEFINE VARIABLE FBTN_ADM             AS HANDLE NO-UNDO. 
DEFINE VARIABLE BTN_ALLBACK          AS HANDLE NO-UNDO. 
DEFINE VARIABLE FILL-IN_SVARD          AS HANDLE NO-UNDO. 
DEFINE VARIABLE FILL-IN_VARDANV        AS HANDLE NO-UNDO. 
DEFINE VARIABLE BTN_NY               AS HANDLE NO-UNDO. 
DEFINE VARIABLE BTN_BORT             AS HANDLE NO-UNDO. 
DEFINE VARIABLE FILL-IN_AONR         AS HANDLE NO-UNDO. 
DEFINE VARIABLE FILL-IN_BEN          AS HANDLE NO-UNDO. 
DEFINE VARIABLE FILL-IN_EVARD          AS HANDLE NO-UNDO. 
DEFINE VARIABLE FILL-IN_EAONR        AS HANDLE NO-UNDO. 
DEFINE VARIABLE FILL-IN_DELNR        AS HANDLE NO-UNDO. 
DEFINE VARIABLE BTN_AVB              AS HANDLE NO-UNDO. 
DEFINE VARIABLE FILL-IN-AOTEXT       AS HANDLE NO-UNDO. 
DEFINE VARIABLE FILL-IN-SOKTEXT      AS HANDLE NO-UNDO. 
DEFINE VARIABLE FILL-IN-HTEXT        AS HANDLE NO-UNDO. 
DEFINE VARIABLE RECT-52                AS HANDLE NO-UNDO. 
DEFINE VARIABLE RECT-23                AS HANDLE NO-UNDO. 
DEFINE VARIABLE TOG_ALLAMARK AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER huvprogh AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER framesizeextrah AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR whandltemp.
DEFINE INPUT PARAMETER franvart AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER markaonr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER markdelnr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR markintemp.
DEFINE VARIABLE ordnr AS INTEGER NO-UNDO.
DEFINE VARIABLE brworgwith  AS DECIMAL NO-UNDO.
DEFINE VARIABLE brworghight AS DECIMAL NO-UNDO.
DEFINE VARIABLE ubrworgwith  AS DECIMAL NO-UNDO.
DEFINE VARIABLE ubrworghight AS DECIMAL NO-UNDO.
DEFINE VARIABLE brworgrow  AS DECIMAL NO-UNDO.
DEFINE VARIABLE brworgcol  AS DECIMAL NO-UNDO.
DEFINE VARIABLE btnorgrow  AS DECIMAL NO-UNDO.
DEFINE VARIABLE btnorgcol  AS DECIMAL NO-UNDO.
DEFINE VARIABLE mbrworgwith  AS DECIMAL NO-UNDO.
DEFINE VARIABLE mbrworghight AS DECIMAL NO-UNDO.
DEFINE VARIABLE mubrworgwith  AS DECIMAL NO-UNDO.
DEFINE VARIABLE mubrworghight AS DECIMAL NO-UNDO.
DEFINE VARIABLE mbrworgrow  AS DECIMAL NO-UNDO.
DEFINE VARIABLE mbrworgcol  AS DECIMAL NO-UNDO.
DEFINE VARIABLE mbtnorgrow  AS DECIMAL NO-UNDO.
DEFINE VARIABLE mbtnorgcol  AS DECIMAL NO-UNDO.
DEFINE VARIABLE BRW_MAXMALL AS HANDLE NO-UNDO.

PROCEDURE franstart_UI :
   DEFINE INPUT PARAMETER franvart2 AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER faonr2 AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER fdelnr2 AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR markintemp.
   ASSIGN
   franvart = franvart2
   markaonr  = faonr2 
   markdelnr = fdelnr2.
   /*direkt här
   IF franvart = 1 THEN  EMPTY TEMP-TABLE markintemp NO-ERROR. 
   */
   RUN main_UI.
END PROCEDURE.
FIND FIRST whandltemp WHERE NO-LOCK NO-ERROR.
ordnr = 0.
ASSIGN
ordnr = ordnr + 1
C-WIN = whandltemp.WF[ordnr]
ordnr = ordnr + 1
TOG_AKT              = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
TOG_INAKT            = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
CMB_OMR              = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
CMB_UTF              = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
CMB_ANSV             = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
BTN_HAMT             = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
BRW_URMARK             = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
BRW_VMARK              = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FBTN_UPP             = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
BTN_AOF              = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FBTN_FASTIGHET         = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
BTN_ALLOVER          = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FBTN_VARD              = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FBTN_VISKAL          = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
BTN_OVER             = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FBTN_VISMARK           = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FBTN_OMRAKNA           = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
BTN_BACK             = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FBTN_INAKTIV         = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FBTN_ADM            = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
BTN_ALLBACK          = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FILL-IN_SVARD          = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FILL-IN_VARDANV        = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
BTN_NY               = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
BTN_BORT             = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FILL-IN_AONR         = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FILL-IN_BEN          = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FILL-IN_EVARD          = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FILL-IN_EAONR        = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FILL-IN_DELNR        = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
BTN_AVB              = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FILL-IN-AOTEXT       = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FILL-IN-SOKTEXT      = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FILL-IN-HTEXT        = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
RECT-52                = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
RECT-23                = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_HAOF = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_EXPM = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_IMPM = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BRW_MAXMALL = whandltemp.WF[ordnr]
ordnr = ordnr + 1
TOG_ALLAMARK = whandltemp.WF[ordnr].

RUN hojbered_UI.   
PROCEDURE hojbered_UI :
   
END PROCEDURE.
PROCEDURE bredhoj_UI :
  
  DEFINE VARIABLE x-multi AS DECIMAL NO-UNDO.
   DEFINE VARIABLE y-multi AS DECIMAL NO-UNDO.
   IF brworgrow > 0 THEN DO:
      x-multi = c-win:WIDTH-PIXELS / c-win:MIN-WIDTH-PIXELS. 
      y-multi = c-win:HEIGHT-PIXELS / c-win:MIN-HEIGHT-PIXELS.     
      ASSIGN
      BRW_VMARK:WIDTH-CHARS =     brworgwith  * x-multi 
      BRW_VMARK:HEIGHT-CHARS =  brworghight   * y-multi 
      BRW_VMARK:ROW =            brworgrow    * y-multi 
      BRW_VMARK:COLUMN =          brworgcol   * x-multi 
      BTN_NY:ROW =             btnorgrow      * y-multi 
      BTN_NY:COL =             btnorgcol     * x-multi. 
   END.
END PROCEDURE.
