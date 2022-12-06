/* p-lock10.p */

DEFINE INPUT PARAMETER curr-rec AS RECID.

DO TRANSACTION:
   FIND customer WHERE RECID(customer) = curr-rec EXCLUSIVE-LOCK.
   UPDATE customer.name WITH FRAME main-frame.
   RELEASE customer.
END.  /* TRANSACTION */
