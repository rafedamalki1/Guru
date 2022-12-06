/* r-day.p */

DEFINE VARIABLE d1 AS DATE LABEL "Date".
DEFINE VARIABLE d2 AS DATE LABEL "Same date next year".
DEFINE VARIABLE d-day AS INTEGER.
DEFINE VARIABLE d-mon AS INTEGER.

REPEAT:
    SET d1.
    d-day = DAY(d1).
    d-mon = MONTH(d1).
    IF d-mon = 2 AND d-day = 29 THEN d-day = 28.
    d2 = DATE(d-mon,d-day,YEAR(d1) + 1).
    DISPLAY d2.
END.
