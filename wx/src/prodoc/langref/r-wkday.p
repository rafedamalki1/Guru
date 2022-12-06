/* r-wkday.p */

DEFINE VARIABLE birth-date AS DATE
	LABEL "Birth Date".
DEFINE VARIABLE daynum AS INTEGER.
DEFINE VARIABLE daylist AS CHARACTER
	FORMAT "x(9)"
	INITIAL "Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday".
DEFINE VARIABLE dayname AS CHARACTER
	LABEL "Day You Were Born".
DEFINE VARIABLE daysold AS INTEGER
	LABEL "Days Since You Were Born".

REPEAT:
	SET birth-date.
	daynum = WEEKDAY(birth-date).
	dayname = ENTRY(daynum,daylist).
	daysold = TODAY - birth-date.
	DISPLAY dayname daysold.
END.
