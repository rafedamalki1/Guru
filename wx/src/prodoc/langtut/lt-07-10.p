/**********  DEFINE WIDGETS  **********/
DEFINE VARIABLE Catalog AS Logical LABEL "Product Catalog"
  VIEW-AS TOGGLE-BOX.
DEFINE VARIABLE Price AS Logical LABEL "Price List"
  VIEW-AS TOGGLE-BOX.
DEFINE VARIABLE Credit AS Logical LABEL "Credit Application"
  VIEW-AS TOGGLE-BOX.
DEFINE VARIABLE Mail AS Logical LABEL "Put on Mailing List?"
  VIEW-AS TOGGLE-BOX.
DEFINE VARIABLE Comments AS CHARACTER LABEL "Comments"
  VIEW-AS EDITOR INNER-CHARS 25 INNER-LINES 6.
DEFINE BUTTON btn-Exit LABEL "Exit".

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Questions
  "Customer wants our:" AT ROW 2 COLUMN 2
  Catalog   AT ROW 3 COLUMN 2
  Price     AT ROW 4 COLUMN 2
  Credit    AT ROW 5 COLUMN 2
  Mail      AT ROW 7 COLUMN 2
  btn-Exit  AT ROW 9 COLUMN 2
  Comments  AT ROW 2 COLUMN 30
    WITH SIDE-LABELS CENTERED TITLE "Customer Questionnaire" THREE-D.

/**********  MAIN LOGIC  **********/
DISPLAY Catalog Price Credit Mail Comments WITH FRAME Questions.
ENABLE ALL WITH FRAME Questions.
WAIT-FOR CHOOSE OF btn-Exit.



