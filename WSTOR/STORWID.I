
/*STRORWID.I*/
{WHANDLTEMP.I}
   
DEFINE VARIABLE C-WIN AS HANDLE NO-UNDO.

DEFINE VARIABLE      RAD_VAL  AS HANDLE NO-UNDO.  
DEFINE VARIABLE      CMB_ANL  AS HANDLE NO-UNDO.  
DEFINE VARIABLE      CMB_FOR  AS HANDLE NO-UNDO.  
DEFINE VARIABLE      CMB_SYS  AS HANDLE NO-UNDO.  
DEFINE VARIABLE      CMB_OMR  AS HANDLE NO-UNDO.  
DEFINE VARIABLE      CMB_AR   AS HANDLE NO-UNDO.  
DEFINE VARIABLE      CMB_SYS2 AS HANDLE NO-UNDO.  
DEFINE VARIABLE      CMB_BEL  AS HANDLE NO-UNDO.  
DEFINE VARIABLE      CMB_FEL  AS HANDLE NO-UNDO.  
DEFINE VARIABLE      BTN_HAMT AS HANDLE NO-UNDO.  
DEFINE VARIABLE      BRW_URSTR   AS HANDLE NO-UNDO.  
DEFINE VARIABLE      BRW_VSTR   AS HANDLE NO-UNDO.  
DEFINE VARIABLE      BTN_ALLOVER  AS HANDLE NO-UNDO.  
DEFINE VARIABLE      BTN_OVER AS HANDLE NO-UNDO.  
DEFINE VARIABLE      FBTN_AND AS HANDLE NO-UNDO.  
DEFINE VARIABLE      BTN_BACK AS HANDLE NO-UNDO.  
DEFINE VARIABLE      FBTN_RAPP AS HANDLE NO-UNDO.  
DEFINE VARIABLE      BTN_ALLBACK AS HANDLE NO-UNDO.  
DEFINE VARIABLE      FBTN_ADM                  AS HANDLE NO-UNDO.  
DEFINE VARIABLE      FBTN_LAS                  AS HANDLE NO-UNDO.  
DEFINE VARIABLE      FBTN_VIS                  AS HANDLE NO-UNDO.  
DEFINE VARIABLE      BTN_NY                     AS HANDLE NO-UNDO.  
DEFINE VARIABLE      BTN_BORT                  AS HANDLE NO-UNDO.  
DEFINE VARIABLE      FILL-IN-BEN               AS HANDLE NO-UNDO.  
DEFINE VARIABLE      FILL-IN-STOR              AS HANDLE NO-UNDO.  
DEFINE VARIABLE      FILL-IN-BEN2              AS HANDLE NO-UNDO.  
DEFINE VARIABLE      FILL-IN-STRNR             AS HANDLE NO-UNDO.  
DEFINE VARIABLE      BTN_AVB                   AS HANDLE NO-UNDO.  
DEFINE VARIABLE      FILL-IN-AOTEXT            AS HANDLE NO-UNDO.  
DEFINE VARIABLE      RECT-29                      AS HANDLE NO-UNDO.  
DEFINE VARIABLE      RECT-33                    AS HANDLE NO-UNDO.  
DEFINE VARIABLE      BTN_AOF                    AS HANDLE NO-UNDO.  
DEFINE VARIABLE   BTN_HAOF AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_DARWINPANV AS HANDLE NO-UNDO.                                                  
DEFINE INPUT PARAMETER huvprogh AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER framesizeextrah AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR whandltemp.
DEFINE INPUT PARAMETER valprog AS INTEGER NO-UNDO.
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
RAD_VAL= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
CMB_ANL= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
CMB_FOR= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
CMB_SYS= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
CMB_OMR= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
CMB_AR = whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
CMB_SYS2= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
CMB_BEL= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
CMB_FEL= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
BTN_HAMT= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
BRW_URSTR= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
BRW_VSTR= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
BTN_ALLOVER= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
BTN_OVER= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
FBTN_AND= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
BTN_BACK= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
FBTN_RAPP= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
BTN_ALLBACK= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
FBTN_ADM= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
FBTN_LAS= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
FBTN_VIS= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
BTN_NY= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
BTN_BORT = whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
FILL-IN-BEN= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
FILL-IN-STOR= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
FILL-IN-BEN2= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
FILL-IN-STRNR= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
BTN_AVB= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
FILL-IN-AOTEXT= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
RECT-29= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
RECT-33= whandltemp.WF[ordnr]  
ordnr = ordnr + 1 
BTN_AOF = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_HAOF = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_DARWINpANV = whandltemp.WF[ordnr].





