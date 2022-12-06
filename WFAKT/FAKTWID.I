/*FAKTWID.I*/

{WHANDLTEMP.I}
   
DEFINE VARIABLE C-WIN AS HANDLE NO-UNDO.
DEFINE VARIABLE CMB_FAK AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN_PROJEKTKOD AS HANDLE NO-UNDO.
DEFINE VARIABLE CMB_OMR AS HANDLE NO-UNDO.
DEFINE VARIABLE CMB_BESORG AS HANDLE NO-UNDO.
DEFINE VARIABLE TOG_ANSV AS HANDLE NO-UNDO.
DEFINE VARIABLE TOG_HUVA AS HANDLE NO-UNDO.
DEFINE VARIABLE TOG_PREL AS HANDLE NO-UNDO.
DEFINE VARIABLE TOG_GOD AS HANDLE NO-UNDO.
DEFINE VARIABLE TOG_GAMLA AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-STARTDAT AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-STOPPDAT AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_HAMT AS HANDLE NO-UNDO.
DEFINE VARIABLE FBTN_KOPP AS HANDLE NO-UNDO.
DEFINE VARIABLE BRW_UFAKT AS HANDLE NO-UNDO.
DEFINE VARIABLE BRW_VFAKT AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_ALLOVER AS HANDLE NO-UNDO.
DEFINE VARIABLE FBTN_FAK AS HANDLE NO-UNDO.
DEFINE VARIABLE FBTN_PRELB AS HANDLE NO-UNDO.
DEFINE VARIABLE FBTN_Kred AS HANDLE NO-UNDO.
DEFINE VARIABLE FBTN_VISAO AS HANDLE NO-UNDO.
DEFINE VARIABLE FBTN_UPP AS HANDLE NO-UNDO.
DEFINE VARIABLE FBTN_VFAK AS HANDLE NO-UNDO.
DEFINE VARIABLE FBTN_FLISTA AS HANDLE NO-UNDO.
DEFINE VARIABLE FBTN_AONRM AS HANDLE NO-UNDO.
DEFINE VARIABLE FBTN_ADM AS HANDLE NO-UNDO.
DEFINE VARIABLE FBTN_AONRU AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_OVER AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_BACK AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_ALLBACK AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_NY AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_BORT AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN_SFAKTNR AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN_SNAMN AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-SOK AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN_EFAKTNR AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN_EFAKUNR AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-HAMT AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN_EAONR AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN_DELNR AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_AVB AS HANDLE NO-UNDO.     
DEFINE VARIABLE RECT-21 AS HANDLE NO-UNDO.
DEFINE VARIABLE RECT-20 AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_AOF AS HANDLE NO-UNDO.
DEFINE VARIABLE   BTN_HAOF AS HANDLE NO-UNDO.
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
{FAKTIN.I}
DEFINE INPUT PARAMETER huvprogh AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER framesizeextrah AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR whandltemp.
DEFINE INPUT PARAMETER franvart AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER faonr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER fdelnr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR faktintemp.

PROCEDURE franstart_UI :
   DEFINE INPUT PARAMETER franvart2 AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER faonr2 AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER fdelnr2 AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR faktintemp.
   ASSIGN
   franvart = franvart2
   faonr  = faonr2 
   fdelnr = fdelnr2.
   /*direkt här
   IF franvart = 1 THEN  EMPTY TEMP-TABLE faktintemp NO-ERROR. 
   */
   RUN main_UI.
END PROCEDURE.
FIND FIRST whandltemp WHERE NO-LOCK NO-ERROR.
ordnr = 0.
ASSIGN
ordnr = ordnr + 1
C-WIN = whandltemp.WF[ordnr]
ordnr = ordnr + 1
CMB_FAK = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FILL-IN_PROJEKTKOD = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
CMB_OMR = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
CMB_BESORG = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
TOG_ANSV = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
TOG_HUVA = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
TOG_PREL = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
TOG_GOD = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
TOG_GAMLA = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FILL-IN-STARTDAT = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FILL-IN-STOPPDAT = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
BTN_HAMT = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FBTN_KOPP = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
BRW_UFAKT = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
BRW_VFAKT = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
BTN_ALLOVER = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FBTN_FAK = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FBTN_PRELB = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FBTN_Kred = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FBTN_VISAO = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FBTN_UPP = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FBTN_VFAK = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FBTN_FLISTA = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FBTN_AONRM = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FBTN_ADM = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FBTN_AONRU = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
BTN_OVER = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
BTN_BACK = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
BTN_ALLBACK = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
BTN_NY = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
BTN_BORT = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FILL-IN_SFAKTNR = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FILL-IN_SNAMN = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FILL-IN-SOK = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FILL-IN_EFAKTNR = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FILL-IN_EFAKUNR = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FILL-IN-HAMT = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FILL-IN_EAONR = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
FILL-IN_DELNR = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
BTN_AVB = whandltemp.WF[ordnr] 
ordnr = ordnr + 1     
RECT-21 = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
RECT-20 = whandltemp.WF[ordnr] 
ordnr = ordnr + 1
BTN_AOF = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_HAOF = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BRW_MAXMALL = whandltemp.WF[ordnr].
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
      BRW_VFAKT:WIDTH-CHARS = brworgwith * x-multi
      BRW_VFAKT:HEIGHT-CHARS = brworghight * y-multi      
      BRW_VFAKT:ROW         = brworgrow * y-multi       
      BRW_VFAKT:COLUMN      = brworgcol * x-multi.
   END.
   IF ubrworgwith > 0  THEN BRW_UFAKT:WIDTH-CHARS  = ubrworgwith * x-multi.
END PROCEDURE.
 
