/* r-param2.p */

DEFINE INPUT-OUTPUT PARAMETER io-param AS INTEGER.
DEFINE VARIABLE inp-qty AS INTEGER.
PROMPT-FOR inp-qty LABEL "Quantity Recieved?".
ASSIGN inp-qty.
io-param = io-param + inp-qty.
