/* r-color.p */

DEFINE VARIABLE hilite AS CHARACTER.

hilite = "messages". /* Use standard messages attribute to
			highlight on-hand less than 50 */
FOR EACH item:
    DISPLAY item-num item-name on-hand WITH ATTR-SPACE.
    IF on-hand < 50 THEN
	COLOR DISPLAY VALUE(hilite) item-num on-hand.
END.
