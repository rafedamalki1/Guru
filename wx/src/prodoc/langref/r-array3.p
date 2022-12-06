/* r-array.p */

DEFINE VARIABLE i AS INTEGER.

FOR EACH item:
    DISPLAY item-num idesc mnth-shp[1] mnth-shp[2] WITH 6 DOWN.
    FORM i mnth-shp[i].
    DO i = 1 to 12:
	DISPLAY i.
	SET mnth-shp[i].
    END.
    DISPLAY mnth-shp WITH FRAME aaa 2 COLUMNS.
END.
