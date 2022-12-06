/* r-array2.p */

DEFINE VARIABLE i AS INTEGER.

FOR EACH item:
    DISPLAY item-num idesc mnth-shp[1] mnth-shp[2].
    DO i = 1 TO 12:
	SET mnth-shp[i].
    end.
    DISPLAY mnth-shp WITH FRAME aaa COLUMN 40 1 COLUMN.
end.
