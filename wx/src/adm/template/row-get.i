/* row-get.i - 8/07/96 */
  &IF DEFINED(queryfind) = 0 &THEN   
      RUN send-records IN record-source-hdl
          (INPUT tbl-list, OUTPUT rowid-list) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN.  /* send-records not defined. */
  &ENDIF
