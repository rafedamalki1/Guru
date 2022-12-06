/* r-inc.p */

DEFINE VARIABLE txt AS CHARACTER.
DEFINE VARIABLE num AS INTEGER.

txt = "PROGRESS VERSION".
num = 7.

{r-inc.i &int=num &str=txt}
