/* p-combo1.p */

DEFINE VARIABLE finish-var AS CHARACTER INITIAL "Medium"
  FORMAT "x(10)" LABEL "Finish" VIEW-AS COMBO-BOX INNER-LINES 3
  LIST-ITEMS "Fine", "Medium", "Coarse".
  
DEFINE BUTTON bGrain LABEL "Grain List".
DEFINE BUTTON bColor LABEL "Color List".
DEFINE BUTTON bCancel LABEL "Cancel".
DEFINE BUTTON bAdd LABEL "Add".
DEFINE BUTTON bNow LABEL "Now".
DEFINE VARIABLE stat AS LOGICAL.
DEFINE VARIABLE cnt AS INTEGER.
DEFINE VARIABLE type-var AS INTEGER INITIAL 2.
DEFINE VARIABLE type-char AS CHARACTER FORMAT "x(10)".

DEFINE FRAME a finish-var SKIP(1) bGrain bColor bAdd bNow bCancel
  WITH SIDE-LABELS.

ON CHOOSE OF bNow IN FRAME a
DO:
  MESSAGE "finish-var value is" finish-var.
END.

ON CHOOSE OF bAdd IN FRAME a
DO:
  REPEAT cnt = 1 to 3:
    type-char = ENTRY(cnt, finish-var:LIST-ITEMS) + STRING(type-var).
    stat = finish-var:ADD-LAST(type-char).
  END.
  type-var = type-var + 1.
END.

ON CHOOSE OF bGrain IN FRAME a
DO:
  finish-var:LIST-ITEMS = "Fine,Medium,Coarse".
  type-var = 2.
  APPLY "VALUE-CHANGED" TO finish-var.
END.

ON CHOOSE OF bColor IN FRAME a
DO:
  finish-var:LIST-ITEMS = "Light,Neutral,Dark".
  type-var = 2.
  APPLY "VALUE-CHANGED" TO finish-var.
END.

ON VALUE-CHANGED OF finish-var IN FRAME a
DO:
  ASSIGN finish-var.
  MESSAGE "Assigning" finish-var.
END.

ENABLE ALL WITH FRAME a.
DISPLAY finish-var WITH FRAME a.

WAIT-FOR CHOOSE OF bCancel IN FRAME a.
