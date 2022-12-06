/* p-radio2.p */

DEFINE VARIABLE math-const AS DECIMAL FORMAT "9.999"
        VIEW-AS RADIO-SET RADIO-BUTTONS "e", 2.72,
                                        "pi", 3.14,
                                        "square root of 2", 1.41
        INITIAL 2.72.

FORM
   math-const SKIP
   description AS CHARACTER FORMAT "x(50)"
   WITH FRAME const-frame NO-LABELS TITLE "Common Constants".
                                            
ON VALUE-CHANGED OF math-const
   DO:
      DISPLAY "The value of this constant is approximately " +
              SELF:SCREEN-VALUE @ description WITH FRAME const-frame.
   END. 
   
DISPLAY math-const WITH FRAME const-frame.
ENABLE math-const WITH FRAME const-frame.
APPLY "VALUE-CHANGED" TO math-const.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.                                          
