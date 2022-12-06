/* r-dfvar.p */

DEFINE NEW SHARED VARIABLE del AS LOGICAL.
DEFINE NEW SHARED VARIABLE nrecs AS INTEGER.

del = no.
MESSAGE "Do you want to delete the orders being printed (y/n)?"
    UPDATE del.
RUN r-dfvar2.p.
IF del THEN MESSAGE nrecs "Orders have been shipped and were deleted".
ELSE MESSAGE nrecs "Orders have been shipped".
