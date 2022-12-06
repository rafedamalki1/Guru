/* r-param1.p */

DEFINE OUTPUT PARAMETER xout-param AS DECIMAL FORMAT "$->>>,>>>,>>9.99".
DEFINE INPUT PARAMETER newin AS INTEGER.
DEFINE OUTPUT PARAMETER xnew-param AS CHARACTER.
DEFINE INPUT PARAMETER xin-param AS INTEGER.

FOR EACH customer:
   xout-param = balance + xout-param.
END.
DISPLAY xout-param LABEL "Balance" WITH SIDE-LABELS.
xout-param = xout-param + newin + xin-param.
xnew-param = "Example Complete".
