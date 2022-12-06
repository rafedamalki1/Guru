ON CHOOSE OF MENU-ITEM m_Auto_Page_Break /* Auto Page Break */
DO:
   /*läser från fil*/
   DEF VAR block AS CHAR NO-UNDO.
   DEF VAR s AS CHAR NO-UNDO.
   IF NOT ok2open(chAuto) THEN RETURN.

   IF SEARCH("vpedemo.w") = ? THEN DO:
      MESSAGE "Source file 'VPEDEMO.W' not found." VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.
   
   /*CM_chAuto:Enabled = False*/
   /*chAuto:Visible = True.*/
   chAuto:REFRESH.
   
   INPUT FROM VALUE(SEARCH("vpedemo.w")).
   
   REPEAT:
     IMPORT UNFORMATTED s.
     
     block = block + CHR(10) + s.
     /* Don't blow up the 32k progress variable limit */
     IF SEEK(INPUT) GT 28000 THEN LEAVE.
   END.
   INPUT CLOSE.
   
   
   MESSAGE "We loaded the file into a Progress variable using IMPORT.~n" + 
   "NOW we run VPE and create a document from the data.~n" + 
   "VPE will create the page breaks itself. This will work very fast!"
   view-as alert-box.
   
   chAuto:OpenDoc.
   chAuto:SelectFont("Courier New", 10).

   /* Set the bottom margin, so the report will fit
    * onto A4 as well as onto US-Letter paper:
    * ============================================== */
   chAuto:SetOutRect(200, 200, 1900, 2650).

   /*Rem Header will be placed outside default output rectangle:*/
   chAuto:NoPen.
   chAuto:TextUnderline = TRUE.
   chAuto:DefineHeader(100, 100, -700, -50, "Auto Text Break Demo - Page @PAGE").

   /*Rem On every intial page:
 *    Rem VLEFT   = VLEFTMARGIN
 *    Rem VTOP    = VTOPMARGIN
 *    Rem VRIGHT  = VRIGHTMARGIN
 *    Rem VBOTTOM = VTOPMARGIN !!!!!!!!!!*/
   chAuto:TextUnderline = FALSE.
   chAuto:SetPen(3, {&psSolid}, 0).
   chAuto:WriteBox({&VLEFT}, {&VBOTTOM}, {&VRIGHT}, {&VFREE}, "[N TO BC LtGray CE S 12 B]Start of Listing").
   chAuto:WriteBox({&VLEFT}, {&VBOTTOM}, {&VRIGHT}, {&VFREE}, block).
   chAuto:WriteBox({&VLEFT}, {&VBOTTOM}, {&VRIGHT}, {&VFREE}, "[N TO BC LtGray CE S 12 B]End of Listing").

   chAuto:Preview.
END.

ON CHOOSE OF MENU-ITEM m_Background /* Background */
DO:
   SYSTEM-DIALOG PRINTER-SETUP PORTRAIT UPDATE utskriv.
   RUN precision (1).

END.
