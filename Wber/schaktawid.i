/*
               KSV Editor
    Copyright: (C) 2000-2003 Serguey Klimoff (bulkl0DD)
     Filename: SCHAKTAWID.I
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2008.08.18 16:23 ELPAO   
     Modified: 
*/

&Scoped-define NEW
&Scoped-define SHARED SHARED

{WHANDLTEMP.I}
   
DEFINE INPUT PARAMETER huvprogh AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER framesizeextrah AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR whandltemp.

DEFINE VARIABLE ordnr AS INTEGER NO-UNDO.
DEFINE VARIABLE C-WIN AS HANDLE NO-UNDO.

DEFINE VARIABLE BRW_PUNKT AS HANDLE NO-UNDO.
DEFINE VARIABLE BRW_OKAB AS HANDLE NO-UNDO.
DEFINE VARIABLE BRW_PKAB AS HANDLE NO-UNDO.
DEFINE VARIABLE BRW_MKAB AS HANDLE NO-UNDO.
DEFINE VARIABLE BRW_AKAB AS HANDLE NO-UNDO.
DEFINE VARIABLE CMB_KAB AS HANDLE NO-UNDO.
DEFINE VARIABLE BRW_SCHAKT AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_KABTILLSC AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_KABFRSC AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_KABTILLP AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_KABFRP AS HANDLE NO-UNDO.
DEFINE VARIABLE BRW_FOR AS HANDLE NO-UNDO.
DEFINE VARIABLE BRW_SAM AS HANDLE NO-UNDO.
DEFINE VARIABLE BRW_YT AS HANDLE NO-UNDO.
DEFINE VARIABLE BRW_SAMVAL AS HANDLE NO-UNDO.
DEFINE VARIABLE BRW_SF AS HANDLE NO-UNDO.

DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

FIND FIRST whandltemp WHERE NO-LOCK NO-ERROR.
ordnr = 0.
ASSIGN
ordnr = ordnr + 1
C-WIN = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BRW_PUNKT = whandltemp.WF[ordnr]          
ordnr = ordnr + 1
BRW_OKAB = whandltemp.WF[ordnr]          
ordnr = ordnr + 1
BRW_PKAB = whandltemp.WF[ordnr]          
ordnr = ordnr + 1
BRW_MKAB = whandltemp.WF[ordnr]          
ordnr = ordnr + 1
BRW_AKAB = whandltemp.WF[ordnr]          
ordnr = ordnr + 1
CMB_KAB = whandltemp.WF[ordnr]          
ordnr = ordnr + 1
BRW_SCHAKT = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_KABTILLSC = whandltemp.WF[ordnr]          
ordnr = ordnr + 1
BTN_KABFRSC = whandltemp.WF[ordnr]          
ordnr = ordnr + 1
BTN_KABTILLP = whandltemp.WF[ordnr]          
ordnr = ordnr + 1
BTN_KABFRP = whandltemp.WF[ordnr]          
ordnr = ordnr + 1
BRW_FOR = whandltemp.WF[ordnr]          
ordnr = ordnr + 1
BRW_SAM = whandltemp.WF[ordnr]          
ordnr = ordnr + 1
BRW_YT = whandltemp.WF[ordnr]          
ordnr = ordnr + 1
BRW_SAMVAL = whandltemp.WF[ordnr]          
ordnr = ordnr + 1
BRW_SF = whandltemp.WF[ordnr]          
ordnr = ordnr + 1.
