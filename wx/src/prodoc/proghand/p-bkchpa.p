/* p-bkchpa.p */

/* On each iteration of the loop, the FOR EACH state-
   ment reads a single record from the database into
   the record buffer.  The scope of the record is the
   FOR EACH block since that is the outermost block in
   which the record is referenced.  At the end of the
   record scope, which is the end of the iteration
   that uses that record, PROGRESS writes the record to
   the database. */

FOR EACH customer:
    UPDATE customer WITH 2 COLUMNS.
END.
