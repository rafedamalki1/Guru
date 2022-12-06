/* p-dump.p */

DEFINE VARIABLE i AS INTEGER.
FIND _file "item".
OUTPUT TO item.d.
DO i = 1 TO 1000:
  FIND item WHERE RECID(item) = i NO-ERROR.
  IF AVAILABLE item AND i <> INTEGER(_file._template)
  THEN EXPORT item.
END.
