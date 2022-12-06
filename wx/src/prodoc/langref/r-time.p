/* r-time.p */

DEFINE VARIABLE hour AS INTEGER.
DEFINE VARIABLE minute AS INTEGER.
DEFINE VARIABLE sec AS INTEGER.
DEFINE VARIABLE timeleft AS INTEGER.

timeleft = (24 * 60 * 60) - TIME.  /* seconds till next midnight */
sec = timeleft MOD 60.
timeleft = (timeleft - sec) / 60.  /* minutes till next midnight */
minute = timeleft MOD 60.
hour = (timeleft - minute) / 60.   /* hours till next midnight */
DISPLAY "Time to midnight:" hour  minute  sec .
