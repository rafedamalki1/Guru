/* r-encode.p */

DEFINE VARIABLE password AS CHARACTER FORMAT "x(16)".
DEFINE VARIABLE id AS CHARACTER FORMAT "x(12)".
DEFINE VARIABLE n-coded-p-wrd AS CHARACTER FORMAT "x(16)".

SET id LABEL "Enter user id"
    password LABEL "Enter password" BLANK WITH CENTERED SIDE-LABELS.

n-coded-p-wrd = ENCODE(password).
DISPLAY n-coded-p-wrd LABEL "Encoded password".
