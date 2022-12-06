/* r-dbname.p - DBNAME */

DEFINE VARIABLE pageno AS INTEGER FORMAT "zzz9" INITIAL 1.

FORM HEADER "Date:" TO 10 TODAY
    "Page:" AT 65 pageno SKIP
    "Database:" TO 10 DBNAME FORMAT "x(60)" SKIP
    "Userid:" TO 10 userid WITH NO-BOX NO-LABELS.

VIEW.
