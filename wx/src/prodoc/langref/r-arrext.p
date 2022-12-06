DEFINE VARIABLE int_value AS INTEGER EXTENT 3 INITIAL [1, 2, 3].
DEFINE VARIABLE i         AS INTEGER.
DEFINE VARIABLE tot       AS INTEGER.

DO i = 1 TO EXTENT(int_value):
   tot = tot + int_value[i].
END.

DISPLAY tot.
