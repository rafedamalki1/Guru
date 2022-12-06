/*
     Filename: MIKINPAR.I
      Created: 03.03.0020 13:56ELPAO     
     Modified: 
*/
/*Mikinpar.i*/
/* DEFINE VARIABLE tempchar AS CHARACTER NO-UNDO. */
/* &Scoped-define NEW "NEW" */

IF "{1}" = "A" THEN DO:
   MESSAGE {1} VIEW-AS ALERT-BOX.
END.
ELSE IF "{1}" = "B" THEN DO:
   MESSAGE {1} VIEW-AS ALERT-BOX.
END.
ELSE IF "{1}" = "C" THEN DO:
   MESSAGE {1} VIEW-AS ALERT-BOX.
END.


/* MESSAGE "{2}" {2} "{3}" {3} "{4}" {4} "{5}" {5} */
/*    VIEW-AS ALERT-BOX.                           */
/* MESSAGE {&NEW} VIEW-AS ALERT-BOX. */



/* tempvar = INTEGER({3}). */
/* tempvar = 1.                 "At" "{1}" "{2}" {2}    */
/* IF tempvar = 1 THEN DO:         */
/*    &Scoped-define NEW NEW       */
/*    &Scoped-define SHARED SHARED */
/* END.                            */
/* IF tempvar = 2 THEN DO:         */
/*    &Scoped-define NEW           */
/*    &Scoped-define SHARED SHARED */
/* END.                            */
/* IF tempvar = 3 THEN DO:         */
/*    &Scoped-define NEW           */
/*    &Scoped-define SHARED        */
/* END.                            */
/* MESSAGE {&NEW} {&SHARED} */
/*    VIEW-AS ALERT-BOX.    */
