/*PLANWID.I*/
{WHANDLTEMP.I}
   
DEFINE VARIABLE C-WIN AS HANDLE NO-UNDO.
DEFINE VARIABLE      BTN_NVE-3                 AS HANDLE NO-UNDO.    
DEFINE VARIABLE      BTN_NVE-4                 AS HANDLE NO-UNDO.    
DEFINE VARIABLE      TOG_FASTA                 AS HANDLE NO-UNDO.    
DEFINE VARIABLE      TOG_AVSLUTADE             AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FILL-IN-MELL              AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FILL-IN-AVSTARTD          AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FILL-IN-OCH               AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FILL-IN-AVSLUTD           AS HANDLE NO-UNDO.    
DEFINE VARIABLE      BTN_FVE-3                 AS HANDLE NO-UNDO.    
DEFINE VARIABLE      BTN_FVE-4                 AS HANDLE NO-UNDO.    
DEFINE VARIABLE      TOG_TILLF                 AS HANDLE NO-UNDO.    
DEFINE VARIABLE      TOG_PAGA                  AS HANDLE NO-UNDO.    
DEFINE VARIABLE      CMB_OMR                   AS HANDLE NO-UNDO.    
DEFINE VARIABLE      RAD_PERIOD                AS HANDLE NO-UNDO.    
DEFINE VARIABLE      CMB_BESORG                AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FILL-IN-K1                AS HANDLE NO-UNDO.    
DEFINE VARIABLE      CMB_ARTAL                 AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FILL-IN-K2                AS HANDLE NO-UNDO.    
DEFINE VARIABLE      CMB_ANSV                  AS HANDLE NO-UNDO.    
DEFINE VARIABLE      CMB_FRAN                  AS HANDLE NO-UNDO.    
DEFINE VARIABLE      CMB_TILL                  AS HANDLE NO-UNDO.    
DEFINE VARIABLE      CMB_ARBART                AS HANDLE NO-UNDO.
DEFINE VARIABLE      CMB_PRIO                AS HANDLE NO-UNDO.    
DEFINE VARIABLE      BTN_HAMT-6                  AS HANDLE NO-UNDO.    
DEFINE VARIABLE      BRW_PLAN                    AS HANDLE NO-UNDO.    
DEFINE VARIABLE      BRW_VPLAN                   AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FBTN_UPP-6                  AS HANDLE NO-UNDO.    
DEFINE VARIABLE      BTN_ALLOVER               AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FBTN_UNDER-2                AS HANDLE NO-UNDO.    
DEFINE VARIABLE      BTN_OVER                  AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FBTN_BUNDER                 AS HANDLE NO-UNDO.    
DEFINE VARIABLE      BTN_BACK                  AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FBTN_BUD                    AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FBTN_KALK-4                 AS HANDLE NO-UNDO.    
DEFINE VARIABLE      BTN_ALLBACK               AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FBTN_RAPP-2                 AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FBTN_AVSAONR-2              AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FBTN_VISAO-3                AS HANDLE NO-UNDO.    
DEFINE VARIABLE      BTN_AOF                   AS HANDLE NO-UNDO.    
DEFINE VARIABLE   BTN_HAOF AS HANDLE NO-UNDO.
DEFINE VARIABLE      BTN_NY                    AS HANDLE NO-UNDO.    
DEFINE VARIABLE      BTN_BORT                  AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FILL-IN_SPLANNR           AS HANDLE NO-UNDO.    
DEFINE VARIABLE      BTN_AVB                   AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FILL-IN_ORT               AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FILL-IN_EPLANNR           AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FILL-IN_ARTAL             AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FILL-IN_PLANNRVAL           AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FILL-IN-KTO               AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FILL-IN-SOK                 AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FILL-IN-VAL                 AS HANDLE NO-UNDO.    
DEFINE VARIABLE      RECT-27                     AS HANDLE NO-UNDO.    
DEFINE VARIABLE      RECT-28                     AS HANDLE NO-UNDO. 
DEFINE VARIABLE      BTN_PTIDPLAN               AS HANDLE NO-UNDO.
DEFINE VARIABLE      FILL-IN-K3                AS HANDLE NO-UNDO.    
DEFINE VARIABLE      FILL-IN-K4                AS HANDLE NO-UNDO.   
DEFINE VARIABLE BTN_KALKNATREG AS HANDLE NO-UNDO. 
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

PROCEDURE franstart_UI :   
   RUN main_UI.
END PROCEDURE.
FIND FIRST whandltemp WHERE NO-LOCK NO-ERROR.
ordnr = 0.
ASSIGN
ordnr = ordnr + 1
C-WIN = whandltemp.WF[ordnr]
ordnr = ordnr + 1 
BTN_NVE-3 = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
BTN_NVE-4 = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
TOG_FASTA = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
TOG_AVSLUTADE = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FILL-IN-MELL = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FILL-IN-AVSTARTD = whandltemp.WF[ordnr]
ordnr = ordnr + 1
FILL-IN-OCH = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FILL-IN-AVSLUTD = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
BTN_FVE-3 = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
BTN_FVE-4 = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
TOG_TILLF = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
TOG_PAGA = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
CMB_OMR  = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
RAD_PERIOD = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
CMB_BESORG = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FILL-IN-K1 = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
CMB_ARTAL = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FILL-IN-K2 = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
CMB_ANSV = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
CMB_FRAN = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
CMB_TILL = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
CMB_ARBART = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
CMB_PRIO = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
BTN_HAMT-6  = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
BRW_PLAN = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
BRW_VPLAN = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FBTN_UPP-6 = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
BTN_ALLOVER = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FBTN_UNDER-2 = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
BTN_OVER = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FBTN_BUNDER = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
BTN_BACK = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FBTN_BUD = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FBTN_KALK-4 = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
BTN_ALLBACK = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FBTN_RAPP-2 = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FBTN_AVSAONR-2 = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FBTN_VISAO-3 = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
BTN_AOF = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
BTN_NY = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
BTN_BORT = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FILL-IN_SPLANNR = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
BTN_AVB  = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FILL-IN_ORT = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FILL-IN_EPLANNR = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FILL-IN_ARTAL = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FILL-IN_PLANNRVAL = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FILL-IN-KTO = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FILL-IN-SOK = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FILL-IN-VAL = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
RECT-27 = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
RECT-28 = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_HAOF = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_PTIDPLAN = whandltemp.WF[ordnr]
ordnr = ordnr + 1
FILL-IN-K3 = whandltemp.WF[ordnr]    
ordnr = ordnr + 1
FILL-IN-K4 = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_KALKNATREG = whandltemp.WF[ordnr].    



