


/* r-funfun.p */
/* demonstrates DYNAMIC-FUNCTION function */
/* Requires a connection to the Sports database */

/* define data items */
DEFINE VAR funcs AS Char EXTENT 5 INITIAL ["firstrec",
                                           "lastrec",
                                           "nextrec",
                                           "prevrec",
                                           "quitting"] NO-UNDO.
DEFINE VAR action AS Char LABEL "Action" FORMAT "x" INITIAL "N" NO-UNDO.
DEFINE VAR idx AS Int NO-UNDO.
DEFINE VAR alldone AS Logical INITIAL No NO-UNDO.
FORM WITH FRAME x SIDE-LABELS 
2 COLUMNS 1 DOWN COLUMN 25.
FUNCTION dispcust RETURNS Logical:
  DISPLAY Customer  EXCEPT Comments WITH FRAME x.
END.

/* define user-defined functions */
FUNCTION firstrec RETURNS Logical:
  FIND FIRST Customer.
  dispcust().
  RETURN yes.
END.
FUNCTION lastrec RETURNS Logical:
  FIND LAST Customer.
  dispcust().
  RETURN yes.
END.


/* define more user-defined functions */
FUNCTION nextrec RETURNS Logical:
  FIND NEXT Customer NO-ERROR.
  IF AVAILABLE Customer THEN
     dispcust().
  RETURN AVAILABLE(Customer).
END.
FUNCTION prevrec RETURNS Logical:
  FIND PREV Customer NO-ERROR.
  IF AVAILABLE Customer THEN
     dispcust().
  RETURN AVAILABLE(Customer).
END.
FUNCTION quitting RETURNS Logical:
  alldone = yes.
  RETURN no.
END.

/* main routine */
REPEAT WHILE NOT alldone:
  UPDATE action HELP 
    "Enter F(irst), L(ast), N(ext), P(rior), or Q(uit) to navigate.".
  idx = LOOKUP(action,"f,l,n,p,q").
  IF idx EQ 0 THEN DO:
    MESSAGE "Enter F(irst), L(ast), N(ext), P(rior), or Q(uit)" 
        VIEW-AS ALERT-BOX.
    NEXT.
  END.
  DISPLAY DYNAMIC-FUNCTION(funcs[idx]) LABEL "Record Found?".
END.





