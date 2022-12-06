/* r-othru.p */

OUTPUT THROUGH wc >wcdata. /* word count UNIX utility */

FOR EACH customer:
    DISPLAY name WITH NO-LABELS NO-BOX.
END.

OUTPUT CLOSE.
PAUSE 1 NO-MESSAGE.
UNIX cat wcdata.
UNIX SILENT rm wcdata.
