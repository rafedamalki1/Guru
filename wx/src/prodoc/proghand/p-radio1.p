/* p-radio1.p */

DEFINE VARIABLE pay-stat AS INTEGER INITIAL 1 LABEL "Pay Status"
                   VIEW-AS RADIO-SET RADIO-BUTTONS "Unpaid", 1,
                                                   "Partially paid", 2,
                                                   "Paid in full", 3.
FORM
   pay-stat
   WITH FRAME in-frame.

FORM
  pay-stat VIEW-AS TEXT
  WITH FRAME out-frame.
     
ON GO OF FRAME in-frame
   DO:
      ASSIGN pay-stat.
      DISPLAY pay-stat WITH FRAME out-frame.
   END. 

DISPLAY pay-stat WITH FRAME in-frame.
ENABLE pay-stat WITH FRAME in-frame.
STATUS INPUT "Select a pay status and GO".

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
