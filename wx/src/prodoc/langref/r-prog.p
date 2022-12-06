DEFINE VARIABLE attribute AS CHARACTER FORMAT "x(24)" LABEL "Attribute".
DEFINE VARIABLE queryable AS LOGICAL VIEW-AS TOGGLE-BOX LABEL "Query".
DEFINE VARIABLE setable   AS LOGICAL VIEW-AS TOGGLE-BOX LABEL "Set".
DEFINE VARIABLE temp-handle AS WIDGET-HANDLE.
DEFINE VARIABLE widget-type AS CHARACTER FORMAT "x(24)" LABEL "Widget".

FORM
   widget-type attribute setable queryable.


REPEAT:
   UPDATE widget-type attribute.

   CREATE VALUE(widget-type) temp-handle.

   queryable = CAN-QUERY(temp-handle, attribute).
   setable = CAN-SET(temp-handle, attribute).
   
   DISPLAY queryable setable.
   
   DELETE WIDGET temp-handle.
END.
