/*r-seek1.p */
DEFINE VARIABLE itemno LIKE item.item-num.
DEFINE VARIABLE itdesc LIKE item-name.
DEFINE VARIABLE m-pos AS INT.

SET itemno LABEL
  "Select a record number to position the output file" WITH SIDE-LABELS.
OUTPUT TO test.fil.
FIND item WHERE itemno = item-num.
    IF item-num = itemno THEN m-pos = SEEK(OUTPUT).
    EXPORT item-num item-name.
OUTPUT CLOSE.

INPUT FROM test.fil.
SEEK INPUT TO m-pos.
SET itemno itdesc WITH FRAME d2.
INPUT CLOSE.
