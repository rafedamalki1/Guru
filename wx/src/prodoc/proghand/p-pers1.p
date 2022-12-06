/* p-pers1.p */

DEFINE VARIABLE num-butts 	AS INTEGER INITIAL 0.
DEFINE VARIABLE temp-hand	AS WIDGET-HANDLE.

DEFINE BUTTON make-butt 	LABEL "Make new button".

DEFINE FRAME  butt-frame 	WITH WIDTH 44.
 
ENABLE make-butt WITH FRAME make-frame.

ON CHOOSE OF make-butt
   DO:
      if num-butts >= 100
      THEN DO:
          MESSAGE "Too many buttons.".
          RETURN NO-APPLY.
      END.
      num-butts = num-butts + 1.
      ASSIGN FRAME butt-frame:HEIGHT-CHARS =
                     TRUNC(((num-butts - 1) / 10), 0) + 2.
      
      CREATE BUTTON temp-hand 
           ASSIGN FRAME = FRAME butt-frame:HANDLE
                  ROW = TRUNCATE((num-butts - 1) / 10, 0) + 1
                  COLUMN = (((num-butts - 1) * 4) MODULO 40) + 1
                  LABEL = STRING(num-butts)
                  SENSITIVE = TRUE
                  VISIBLE = TRUE 
           TRIGGERS:
              ON CHOOSE
                 PERSISTENT RUN but-mess.
           END TRIGGERS.            
   END.
 
VIEW FRAME butt-frame.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
 
 
PROCEDURE but-mess.  
   
   MESSAGE "You have selected button number" SELF:LABEL
           "of" num-butts.
   
END PROCEDURE.
