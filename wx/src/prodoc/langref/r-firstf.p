/* r-firstf.p */

FOR EACH item BREAK BY Cat-Page:
     IF FIRST-OF(Cat-Page) THEN CLEAR ALL.
     DISPLAY Cat-Page item-num item-name.
END.
