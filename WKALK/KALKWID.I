/*KALKWID.I.I*/

{WHANDLTEMP.I}
   

DEFINE VARIABLE C-WIN AS HANDLE NO-UNDO.
DEFINE VARIABLE TOG_AKT AS HANDLE NO-UNDO.
DEFINE VARIABLE TOG_INAK AS HANDLE NO-UNDO.
DEFINE VARIABLE TOG_KALKAO AS HANDLE NO-UNDO.
DEFINE VARIABLE CMB_JURP AS HANDLE NO-UNDO.
DEFINE VARIABLE CMB_AVD AS HANDLE NO-UNDO.
DEFINE VARIABLE CMB_OMR AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_BACK AS HANDLE NO-UNDO.
DEFINE VARIABLE CMB_UTF AS HANDLE NO-UNDO.  
DEFINE VARIABLE CMB_BESORG AS HANDLE NO-UNDO.
DEFINE VARIABLE CMB_KANSV AS HANDLE NO-UNDO.
DEFINE VARIABLE CMB_KTYP AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_HAMT AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_OVER AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_ALLOVER AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_ALLBACK AS HANDLE NO-UNDO.
DEFINE VARIABLE FBTN_KALK AS HANDLE NO-UNDO.
DEFINE VARIABLE BRW_VKALK AS HANDLE NO-UNDO.
DEFINE VARIABLE BRW_MAXMALL AS HANDLE NO-UNDO.
DEFINE VARIABLE BRW_UKALK AS HANDLE NO-UNDO.

DEFINE VARIABLE FBTN_INAKTIV AS HANDLE NO-UNDO.
DEFINE VARIABLE FBTN_ADM AS HANDLE NO-UNDO.
DEFINE VARIABLE FBTN_VISKAL AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN_KALKB AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_AVB AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_NY AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_BORT AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN_KAONR AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN_EAONR AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-KPLANNR AS HANDLE NO-UNDO.
DEFINE VARIABLE  FILL-IN-KATEXT  AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN_EKALNR AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN_DELNR AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-VALK AS HANDLE NO-UNDO.
DEFINE VARIABLE RECT-51 AS HANDLE NO-UNDO.
DEFINE VARIABLE RECT-53 AS HANDLE NO-UNDO.     
DEFINE VARIABLE BTN_AOF AS HANDLE NO-UNDO.
DEFINE VARIABLE   BTN_HAOF AS HANDLE NO-UNDO.
DEFINE VARIABLE FRAME-AONR AS HANDLE NO-UNDO.
DEFINE VARIABLE FRAME-PLAN AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN_KALKYL AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-SOK AS HANDLE NO-UNDO.
DEFINE VARIABLE valkbenamn AS HANDLE NO-UNDO.
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
DEFINE VARIABLE TOG_ALLAKALK AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_ARENDESTATUS AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_EKG AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_SLAIHOP AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_JMFKALK AS HANDLE NO-UNDO.

{KALKIN.I}
DEFINE INPUT PARAMETER huvprogh AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER framesizeextrah AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR whandltemp.
DEFINE INPUT PARAMETER franvart AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER kalknrao AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER kalktypvar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR kalkinmtemp.   
   

PROCEDURE franstart_UI :
   DEFINE INPUT PARAMETER franvart2 AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER kalknrao2 AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER kalktypvar2 AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR kalkinmtemp.
   ASSIGN
   franvart = franvart2
   kalknrao  = kalknrao2 
   kalktypvar = kalktypvar2.
   /*direkt här
   IF franvart = 1 THEN EMPTY TEMP-TABLE kalkintemp NO-ERROR. 
   */
   RUN main_UI.
 
END PROCEDURE.
FIND FIRST whandltemp WHERE NO-LOCK NO-ERROR.
ASSIGN
ordnr = ordnr + 1
C-WIN = whandltemp.WF[ordnr]
ordnr = ordnr + 1
TOG_AKT = whandltemp.WF[ordnr]
ordnr = ordnr + 1
TOG_INAK = whandltemp.WF[ordnr]
ordnr = ordnr + 1
CMB_JURP = whandltemp.WF[ordnr]
ordnr = ordnr + 1
CMB_AVD = whandltemp.WF[ordnr]
ordnr = ordnr + 1
CMB_OMR = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_BACK = whandltemp.WF[ordnr]
ordnr = ordnr + 1
CMB_UTF = whandltemp.WF[ordnr]  
ordnr = ordnr + 1
CMB_BESORG = whandltemp.WF[ordnr]
ordnr = ordnr + 1
CMB_KANSV = whandltemp.WF[ordnr]
ordnr = ordnr + 1
CMB_KTYP = whandltemp.WF[ordnr]
ordnr = ordnr + 1

BTN_HAMT = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_OVER = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_ALLOVER = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_ALLBACK = whandltemp.WF[ordnr]
ordnr = ordnr + 1
FBTN_KALK = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BRW_VKALK = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BRW_UKALK = whandltemp.WF[ordnr]
ordnr = ordnr + 1

FBTN_INAKTIV = whandltemp.WF[ordnr]
ordnr = ordnr + 1
FBTN_ADM = whandltemp.WF[ordnr]
ordnr = ordnr + 1
FBTN_VISKAL = whandltemp.WF[ordnr]
ordnr = ordnr + 1

FILL-IN_KALKB = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_AVB = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_NY = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_BORT = whandltemp.WF[ordnr]
ordnr = ordnr + 1
FILL-IN_KAONR = whandltemp.WF[ordnr]
ordnr = ordnr + 1
FILL-IN_EAONR = whandltemp.WF[ordnr]
ordnr = ordnr + 1
FILL-IN-KPLANNR = whandltemp.WF[ordnr]
ordnr = ordnr + 1
 FILL-IN-KATEXT  = whandltemp.WF[ordnr]
ordnr = ordnr + 1
FILL-IN_EKALNR = whandltemp.WF[ordnr]
ordnr = ordnr + 1
FILL-IN_DELNR = whandltemp.WF[ordnr]
ordnr = ordnr + 1
FILL-IN-VALK = whandltemp.WF[ordnr]
ordnr = ordnr + 1
RECT-51 = whandltemp.WF[ordnr]
ordnr = ordnr + 1
RECT-53 = whandltemp.WF[ordnr]     
ordnr = ordnr + 1
BTN_AOF = whandltemp.WF[ordnr]
ordnr = ordnr + 1
FILL-IN_KALKYL = whandltemp.WF[ordnr]
ordnr = ordnr + 1
FILL-IN-SOK = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_HAOF = whandltemp.WF[ordnr]
ordnr = ordnr + 1
FRAME-AONR = whandltemp.WF[ordnr]
ordnr = ordnr + 1
FRAME-PLAN = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BRW_MAXMALL = whandltemp.WF[ordnr]
ordnr = ordnr + 1
TOG_KALKAO = whandltemp.WF[ordnr]
ordnr = ordnr + 1
valkbenamn = whandltemp.WF[ordnr]
ordnr = ordnr + 1
TOG_ALLAKALK = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_ARENDESTATUS = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_EKG = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_SLAIHOP = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_JMFKALK = whandltemp.WF[ordnr].


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
      BRW_VKALK:WIDTH-CHARS =     brworgwith  * x-multi        
      BRW_VKALK:HEIGHT-CHARS =  brworghight  * y-multi
      BRW_VKALK:ROW =            brworgrow   * y-multi
      BRW_VKALK:COLUMN =          brworgcol  * x-multi
      BTN_NY:ROW =             btnorgrow    * y-multi
      BTN_NY:COL =             btnorgcol   * x-multi.
   END.
END PROCEDURE.
