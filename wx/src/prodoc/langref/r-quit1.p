DEFINE SUB-MENU cusmaint1
    MENU-ITEM crecust LABEL "Create New Customer"
    MENU-ITEM chgcust LABEL "Chan&ge Existing Customer"
    MENU-ITEM delcust LABEL "Delete Customer"
    MENU-ITEM prtcust LABEL "Print Customer List"
    MENU-ITEM extcust LABEL "E&xit PROGRESS".

DEFINE MENU mainbar MENUBAR
    SUB-MENU cusmaint1 LABEL "Customer".

ON CHOOSE OF MENU-ITEM crecust
   RUN newcust.p.

ON CHOOSE OF MENU-ITEM chgcust
   RUN chgcust.p.

ON CHOOSE OF MENU-ITEM delcust
   RUN delcust.p.

ON CHOOSE OF MENU-ITEM prtcust
   RUN prncust.p.

ON CHOOSE OF MENU-ITEM extcust
   QUIT.

CURRENT-WINDOW:MENUBAR = MENU mainbar:HANDLE.
CURRENT-WINDOW:VISIBLE = TRUE.


WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
