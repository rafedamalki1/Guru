/* r-gt.p */

FOR EACH item:
    IF alloc > 0
    THEN IF (on-hand <= 0) OR
	(alloc / on-hand > .9)
    THEN DISPLAY item-num item-name on-hand alloc.
END.
