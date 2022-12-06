/* p-frmwin.p */

DEFINE VARIABLE hwin AS WIDGET-HANDLE.
CREATE WINDOW hwin
    ASSIGN TITLE = "New Window".
CURRENT-WINDOW:TITLE = "Current Window".

FIND FIRST customer.

DISPLAY name balance WITH FRAME cust-frame.
DISPLAY name balance WITH FRAME cust-frame IN WINDOW hwin.

PAUSE.
