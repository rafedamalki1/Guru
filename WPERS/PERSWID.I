/*SEKWID.I*/
{WHANDLTEMP.I}
   
DEFINE VARIABLE C-WIN AS HANDLE NO-UNDO.
 DEFINE VARIABLE     CMB_JURP             AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     CMB_AKTIV            AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     CMB_SEMFOR           AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     CMB_AVD              AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     CMB_VECKO            AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     CMB_DELTID           AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     CMB_OMR              AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     CMB_BEFATTNING       AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     CMB_FLEX             AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     CMB_TIDSGODK         AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     CMB_TRA              AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     FILL-IN_FORNAMN      AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     CMB_BER              AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     FILL-IN_EFTERNAMN    AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     CMB_ANST             AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     BTN_HAMT             AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     FBTN_UPP             AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     BRW_PERS             AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     BRW_VPERS               AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     FBTN_SCH                AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     FBTN_ARB             AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     BTN_ALLOVER          AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     FBTN_VISA            AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     FBTN_DEBPR              AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     BTN_OVER             AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     FBTN_VISAP              AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     FBTN_SEK             AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     BTN_BACK             AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     FBTN_OT                 AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     BTN_ALLBACK          AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     BTN_NY               AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     BTN_BORT             AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     FILL-IN_SPERSONALKOD AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     FILL-IN_SFORNAMN     AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     FILL-IN_SEFTERNAMN   AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     FILL-IN_EPERSONALKOD AS HANDLE NO-UNDO.
 DEFINE VARIABLE     FILL-IN_EPNR         AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     BTN_AVB              AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     FILL-IN-PERSONAL     AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     RECT-24                 AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     RECT-25                 AS HANDLE NO-UNDO.         
 DEFINE VARIABLE     BTN_AOF                 AS HANDLE NO-UNDO.         
 DEFINE VARIABLE   BTN_HAOF AS HANDLE NO-UNDO.
 DEFINE VARIABLE BTN_TELEFONLISTA AS HANDLE NO-UNDO.
 DEFINE VARIABLE     CMB_RULL            AS HANDLE NO-UNDO.
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
CMB_JURP = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
CMB_AKTIV = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
CMB_SEMFOR = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
CMB_AVD  = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
CMB_VECKO = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
CMB_DELTID = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
CMB_OMR  = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
CMB_BEFATTNING = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
CMB_FLEX = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
CMB_TIDSGODK = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
CMB_TRA  = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
FILL-IN_FORNAMN = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
CMB_BER  = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
FILL-IN_EFTERNAMN = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
CMB_ANST = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
BTN_HAMT = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
FBTN_UPP = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
BRW_PERS = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
BRW_VPERS = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
FBTN_SCH = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
FBTN_ARB = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
BTN_ALLOVER = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
FBTN_VISA = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
FBTN_DEBPR  = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
BTN_OVER = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
FBTN_VISAP = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
FBTN_SEK = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
BTN_BACK = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
FBTN_OT = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
BTN_ALLBACK = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
BTN_NY = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
BTN_BORT = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
FILL-IN_SPERSONALKOD = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
FILL-IN_SFORNAMN = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
FILL-IN_SEFTERNAMN = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
FILL-IN_EPERSONALKOD = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
BTN_AVB  = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
FILL-IN-PERSONAL = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
RECT-24 = whandltemp.WF[ordnr]         
ordnr = ordnr + 1
RECT-25 = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_AOF = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_HAOF = whandltemp.WF[ordnr]
ordnr = ordnr + 1
BTN_TELEFONLISTA = whandltemp.WF[ordnr]
ordnr = ordnr + 1
CMB_RULL = whandltemp.WF[ordnr]
ordnr = ordnr + 1
FILL-IN_EPNR = whandltemp.WF[ordnr].         


DEFINE VARIABLE colvar AS DECIMAL NO-UNDO.
colvar = 10.
ASSIGN
brworgwith = BRW_VPERS:WIDTH-CHARS
brworghight = BRW_VPERS:HEIGHT-CHARS
brworgrow = BRW_VPERS:ROW
brworgcol = BRW_VPERS:COLUMN.
   /*
CMB_JURP:COLUMN             = CMB_JURP:COLUMN - colvar        
CMB_AKTIV:COLUMN            = CMB_AKTIV:COLUMN - colvar        
CMB_SEMFOR:COLUMN           = CMB_SEMFOR:COLUMN  - colvar       
CMB_AVD:COLUMN              = CMB_AVD:COLUMN      - colvar   
CMB_VECKO:COLUMN            = CMB_VECKO:COLUMN     - colvar    
CMB_DELTID:COLUMN           = CMB_DELTID:COLUMN   - colvar      
CMB_OMR:COLUMN              = CMB_OMR:COLUMN         
CMB_BEFATTNING:COLUMN       = CMB_BEFATTNING:COLUMN - colvar        
CMB_FLEX:COLUMN             = CMB_FLEX:COLUMN       - colvar  
CMB_TIDSGODK:COLUMN         = CMB_TIDSGODK:COLUMN   - colvar      
CMB_TRA:COLUMN              = CMB_TRA:COLUMN        - colvar 
FILL-IN_FORNAMN:COLUMN      = FILL-IN_FORNAMN:COLUMN - colvar         
CMB_BER:COLUMN              = CMB_BER:COLUMN         - colvar
FILL-IN_EFTERNAMN:COLUMN    = FILL-IN_EFTERNAMN:COLUMN - colvar        
CMB_ANST:COLUMN             = CMB_ANST:COLUMN          - colvar.
*/
ASSIGN
CMB_TIDSGODK:LABEL = "Godkänner tidsedlar" 
CMB_OMR:LABEL = "Visa personal för".

