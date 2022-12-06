DEF VAR HEADLINE AS INT INIT 1 NO-UNDO.
DEF VAR BODYTEXT AS INT INIT 2 NO-UNDO.
DEF VAR headline_height AS INT NO-UNDO.
DEF VAR group_title_height AS INT NO-UNDO.
DEF VAR detail_height AS INT NO-UNDO.
DEF VAR sum_amount AS DEC NO-UNDO FORMAT ">>>,>>>,>>9.99".
DEF VAR sum_prorated AS DEC NO-UNDO.
DEF VAR sum_tax AS DEC NO-UNDO.
DEF VAR lineheight AS INT NO-UNDO.
DEF VAR factor AS INT NO-UNDO INIT 49.
DEF VAR curr-dir AS CHAR NO-UNDO.
DEF VAR DATAPATH AS CHAR NO-UNDO.
DEF VAR WOOD AS CHAR NO-UNDO.
DEF VAR SAND AS CHAR NO-UNDO.
DEF VAR BAU AS CHAR NO-UNDO.
DEF VAR VPELOGO AS CHAR NO-UNDO.
DEF VAR SILENCE AS CHAR NO-UNDO.
DEF VAR MARBLE AS CHAR NO-UNDO.
DEF VAR PROD AS CHAR NO-UNDO.
DEFINE VARIABLE utskriv AS LOGICAL NO-UNDO.
ASSIGN DATAPATH = "d:\program\VPE_S32\images".
/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE Auto AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chAuto AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE Cap AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCap AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE Clr AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chClr AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE Report AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chReport AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE Speed AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chSpeed AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE Welcome AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chWelcome AS COMPONENT-HANDLE NO-UNDO.

DEFINE FRAME DEFAULT-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 116 BY 20.19.

/*själva funktionen kommer längre ned*/
FUNCTION compx RETURNS INTEGER
  ( INPUT w AS INT,
    INPUT numcols AS INT,
    INPUT vCOL AS INT )  FORWARD.

FUNCTION compy RETURNS INTEGER
  ( h as int,
    vcol as int,
    vrow as int )  FORWARD.

FUNCTION detailline RETURNS LOGICAL
  ( input vcolor as int,
    input country as char,
    input quantity as dec )  FORWARD.

FUNCTION getDirectory RETURNS CHARACTER
  (  )  FORWARD.

FUNCTION groupfooter RETURNS LOGICAL
  ( input quantity as dec )  FORWARD.

FUNCTION groupHeader RETURNS LOGICAL
  ( input product as char )  FORWARD.

FUNCTION ok2open RETURNS LOGICAL
  ( input chTest as com-handle )  FORWARD.

FUNCTION printfooter RETURNS LOGICAL
  ( )  FORWARD.

FUNCTION printheader RETURNS LOGICAL
  ( vtable as char )  FORWARD.

FUNCTION printpagefooter RETURNS LOGICAL
  ( v-name as char )  FORWARD.

FUNCTION setDirectory RETURNS LOGICAL
  ( v-dir as char )  FORWARD.

FUNCTION sine RETURNS DECIMAL
  ( INPUT decinput AS DECIMAL )  FORWARD.

/*namn på de olika ocx erna */

CREATE CONTROL-FRAME Cap ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 1
       COLUMN       = 1
       HEIGHT       = 3.81
       WIDTH        = 20
       HIDDEN       = yes
       SENSITIVE    = yes.

CREATE CONTROL-FRAME Speed ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 1
       COLUMN       = 23
       HEIGHT       = 3.81
       WIDTH        = 20
       HIDDEN       = no
       SENSITIVE    = yes.

CREATE CONTROL-FRAME Clr ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 1
       COLUMN       = 45
       HEIGHT       = 3.81
       WIDTH        = 57
       HIDDEN       = yes
       SENSITIVE    = yes.

CREATE CONTROL-FRAME Welcome ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 6
       COLUMN       = 2
       HEIGHT       = 3.81
       WIDTH        = 19
       HIDDEN       = yes
       SENSITIVE    = yes.

CREATE CONTROL-FRAME Report ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 6
       COLUMN       = 23
       HEIGHT       = 3.81
       WIDTH        = 20
       HIDDEN       = no
       SENSITIVE    = yes.

CREATE CONTROL-FRAME Auto ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 6
       COLUMN       = 45
       HEIGHT       = 3.81
       WIDTH        = 20
       HIDDEN       = yes
       SENSITIVE    = yes.
      Cap:NAME = "Cap":U .
/* Cap OCXINFO:CREATE-CONTROL from: {399548B6-253E-11D2-BE13-000000000000} type: VPE */
      Speed:NAME = "Speed":U .
/* Speed OCXINFO:CREATE-CONTROL from: {399548B6-253E-11D2-BE13-000000000000} type: VPE */
      Clr:NAME = "Clr":U .
/* Clr OCXINFO:CREATE-CONTROL from: {399548B6-253E-11D2-BE13-000000000000} type: VPE */
      Welcome:NAME = "Welcome":U .
/* Welcome OCXINFO:CREATE-CONTROL from: {399548B6-253E-11D2-BE13-000000000000} type: VPE */
      Report:NAME = "Report":U .
/* Report OCXINFO:CREATE-CONTROL from: {399548B6-253E-11D2-BE13-000000000000} type: VPE */
      Auto:NAME = "Auto":U .
/* Auto OCXINFO:CREATE-CONTROL from: {399548B6-253E-11D2-BE13-000000000000} type: VPE */
      Speed:MOVE-AFTER(Cap).
      Clr:MOVE-AFTER(Speed).
      Welcome:MOVE-AFTER(Clr).
      Report:MOVE-AFTER(Welcome).
      Auto:MOVE-AFTER(Report).


/*procedurer för ocx erna egentligen triggers*/
PROCEDURE Auto.VPE.AfterMail .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Result
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Result AS INTEGER NO-UNDO.

setDirectory(curr-dir).
END PROCEDURE.

PROCEDURE Auto.VPE.BeforeMail .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

/* Use of VPE mail choice changes the Progress current directory.
     We need to get this and then change it back in the AfterMail trigger.*/
  curr-dir = getDirectory().


END PROCEDURE.

PROCEDURE Cap.VPE.AfterMail .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Result
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Result AS INTEGER NO-UNDO.

setDirectory(curr-dir).

END PROCEDURE.

PROCEDURE Cap.VPE.BeforeMail .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
  /* Use of VPE mail choice changes the Progress current directory.
     We need to get this and then change it back in the AfterMail trigger.*/
  curr-dir = getDirectory().

END PROCEDURE.
                                                   
PROCEDURE Clr.VPE.AfterMail .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Result
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Result AS INTEGER NO-UNDO.

setDirectory(curr-dir).
END PROCEDURE.
                                                   
PROCEDURE Clr.VPE.BeforeMail .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  /* Use of VPE mail choice changes the Progress current directory.
     We need to get this and then change it back in the AfterMail trigger.*/
  curr-dir = getDirectory().


END PROCEDURE.
               


/*funktioner*/
/* ************************  Function Implementations ***************** */

FUNCTION compx RETURNS INTEGER
  ( INPUT w AS INT,
    INPUT numcols AS INT,
    INPUT vCOL AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN INT(w / numcols * vcol + 250).   /* Function return value. */
END FUNCTION.

FUNCTION compy RETURNS INTEGER
  ( h as int,
    vcol as int,
    vrow as int ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN int(h / exp(2,vcol) * vrow + (h / exp(2,vcol)) / 2 + 200).   /* Function return value. */
  
 

END FUNCTION.

FUNCTION detailline RETURNS LOGICAL
  ( input vcolor as int,
    input country as char,
    input quantity as dec ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   
  
   DEF VAR Y AS INTEGER.
   Y = chReport:nBottom.
   chReport:BkgMode = {&VBKG_SOLID}.
   chReport:BkgColor = vcolor.
   chReport:PenColor = vcolor.
   chReport:BOX(150, Y, 1550, LineHeight).
   chReport:BkgMode = {&VBKG_TRANSPARENT}.

   
   
   chReport:WriteBox(150, Y, -400, Lineheight, "[L S 14 C Black]" + country).
   chReport:TextAlignment = {&ALIGN_RIGHT}.
   chReport:WriteBox(650, Y, 900, LineHeight, trim(STRING(quantity, ">>>,>>9.99"))).
   chReport:WriteBox(1150, Y, 1450, LineHeight, trim(STRING(quantity * factor, ">>>,>>9.99"))).
   
  
  
  RETURN true.   /* Function return value. */

END FUNCTION.

FUNCTION getDirectory RETURNS CHARACTER
  (  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE chrDirectoryName AS CHARACTER NO-UNDO FORMAT "X(256)".
  DEFINE VARIABLE intBufferSize    AS INTEGER   NO-UNDO INITIAL 256.
  DEFINE VARIABLE intResult        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE ptrToString      AS MEMPTR    NO-UNDO.
  
  SET-SIZE(ptrToString) = 256.
  RUN GetCurrentDirectoryA (INPUT        intBufferSize,
                          INPUT-OUTPUT ptrToString,
                          OUTPUT       intResult).

  ASSIGN chrDirectoryName = GET-STRING(ptrToString,1).


  
  
  RETURN TRIM(chrDirectoryName).   /* Function return value. */

END FUNCTION.

FUNCTION groupfooter RETURNS LOGICAL
  ( input quantity as dec ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    
   DEF VAR Y AS INTEGER.
   /*Rem PrintBox and WriteBox are used here, because in the Detail-Section they are used, too.
 *    Rem If we wouldn't use them, the Footer-Section would be misaligned, because of
 *    Rem the invisible surrounding rectangle of the Detail-Section.*/
   Y = chReport:nBottom + chReport:PenSize + 2.
               /* Rem  + Pensize, because the white frame would otherwise overlap the Detail-Section"s frame
 *                 Rem  +2, to avoid rounding problems we use a bit more*/
   chReport:PenColor = {&COLOR_WHITE}.
   chReport:PrintBox(150, Y, "Total").
   chReport:WriteBox(650, Y, 900, LineHeight, trim(STRING(quantity, ">>>,>>9.99"))).
   chReport:WriteBox(1150, Y, 1450, LineHeight, trim(STRING(quantity * factor, ">>>,>>9.99"))).
   chReport:nBottom = Y + 180.
  
  
  
  
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

FUNCTION groupHeader RETURNS LOGICAL
  ( input product as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEF VAR Y AS INT.
   chReport:TextAlignment = {&ALIGN_LEFT}.
   Y = chReport:Print(150, chReport:nBottom, "[N S 18 C Blue]Product: " + product).
   Y = Y + 10.
   chReport:VpePrint(150, Y, "[S 16 C Purple]Country").
   chReport:WRITE(650, Y, 900, {&VFREE}, "[R]Quantity").
   chReport:WRITE(1150, Y, 1450, {&VFREE}, "Value (in $)").
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

FUNCTION ok2open RETURNS LOGICAL
  ( input chTest as com-handle ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF chTest:isopen THEN chTest:closedoc.
  RETURN NOT chTest:isopen.   /* Function return value. */

END FUNCTION.

FUNCTION printfooter RETURNS LOGICAL
  ( ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   chSpeed:SetFontAttr({&ALIGN_CENTER}, TRUE, FALSE, FALSE, FALSE).
   chSpeed:BkgColor = {&COLOR_LTGRAY}.
   chSpeed:WriteBox({&VLEFTMARGIN}, {&VBOTTOM}, -420, group_title_height, "Sum").
   chSpeed:TextAlignment = {&ALIGN_RIGHT}.
   chSpeed:WriteBox({&VRIGHT}, {&VTOP}, -250, {&VBOTTOM}, TRIM(STRING(sum_amount,">>>,>>>,>>9.99"))).
   chSpeed:WriteBox({&VRIGHT}, {&VTOP}, -250, {&VBOTTOM}, TRIM(STRING(sum_prorated,">>>,>>>,>>9.99"))).
   chSpeed:WriteBox({&VRIGHT}, {&VTOP}, -250, {&VBOTTOM}, TRIM(STRING(sum_tax,">>>,>>>,>>9.99"))).
   chSpeed:BOX({&VRIGHT}, {&VTOP}, {&VRIGHTMARGIN}, {&VBOTTOM}).
   chSpeed:SetFontAttr({&ALIGN_LEFT}, FALSE, FALSE, FALSE, FALSE).
   chSpeed:BkgColor = {&COLOR_WHITE}.
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

FUNCTION printheader RETURNS LOGICAL
  ( vtable as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    
   chSpeed:TextAlignment = {&ALIGN_CENTER}.
   chSpeed:SelectFont("Arial", 14).
   chSpeed:BkgColor = {&COLOR_LTGRAY}.
   If headline_height = 0 Then do:
      /*Rem determine height of headline only once for better performance:*/
       chSpeed:RenderPrintBox(0, 0, "x").
      headline_height = chSpeed:nRenderHeight * -1.
   End.
   chSpeed:WriteBox({&VLEFTMARGIN}, {&VBOTTOM}, {&VRIGHTMARGIN}, headline_height, vtable).

   chSpeed:TextBold = True.
   chSpeed:SelectFont("Arial", 11).
   IF group_title_height = 0 THEN DO:
      /*Rem determine height of titles only once for better performance:*/
       chSpeed:RenderPrintBox(0, 0, "x").
      group_title_height = chSpeed:nRenderHeight * -1.
   END.
   
    chSpeed:WriteBox({&VLEFTMARGIN}, {&VBOTTOM}, -200, group_title_height, "No.").
    chSpeed:WriteBox({&VRIGHT}, {&VTOP}, -220, {&VBOTTOM}, "Date").
    chSpeed:WriteBox({&VRIGHT}, {&VTOP}, -250, {&VBOTTOM}, "Amount").
    chSpeed:WriteBox({&VRIGHT}, {&VTOP}, -250, {&VBOTTOM}, "Prorated").
    chSpeed:WriteBox({&VRIGHT}, {&VTOP}, -250, {&VBOTTOM}, "Tax").
    chSpeed:WriteBox({&VRIGHT}, {&VTOP}, {&VRIGHTMARGIN}, {&VBOTTOM}, "Remark").
    chSpeed:SetFontAttr({&ALIGN_LEFT}, False, False, False, False).
   chSpeed:BkgColor = {&COLOR_WHITE}.
  
  
  RETURN true.   /* Function return value. */

END FUNCTION.

FUNCTION printpagefooter RETURNS LOGICAL
  ( v-name as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  chSpeed:StorePos.
  chSpeed:Write({&VLEFTMARGIN}, {&VBOTTOMMARGIN}, chSpeed:nRightMargin - 400, detail_height, v-name).
  chSpeed:RestorePos.
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

FUNCTION setDirectory RETURNS LOGICAL
  ( v-dir as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR intresult AS INT NO-UNDO.
  RUN SetCurrentDirectoryA (INPUT v-dir, OUTPUT intResult).

  IF intresult = 1 THEN RETURN TRUE.
  ELSE
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

FUNCTION sine RETURNS DECIMAL
  ( INPUT decinput AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR decresult AS DEC NO-UNDO.
  RUN sin(INPUT decinput, OUTPUT decresult).
  RETURN decresult.   /* Function return value. */

END FUNCTION.

/*externa procedurer*/



PROCEDURE sin EXTERNAL "MSVCRT40.DLL" CDECL:
    DEFINE INPUT  PARAMETER dblValue  AS DOUBLE NO-UNDO.
    DEFINE RETURN PARAMETER dblResult AS DOUBLE NO-UNDO.
END PROCEDURE.
PROCEDURE GetCurrentDirectoryA EXTERNAL "KERNEL32.DLL":
    DEFINE INPUT        PARAMETER intBufferSize AS LONG.
    DEFINE INPUT-OUTPUT PARAMETER ptrToString   AS MEMPTR.
    DEFINE RETURN       PARAMETER intResult     AS SHORT.
END PROCEDURE.
PROCEDURE SetCurrentDirectoryA EXTERNAL "KERNEL32.DLL":
    DEFINE INPUT  PARAMETER chrCurDir AS CHARACTER.
    DEFINE RETURN PARAMETER intResult AS LONG.
END PROCEDURE.
