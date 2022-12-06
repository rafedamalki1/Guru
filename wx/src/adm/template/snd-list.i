/* snd-list - 8/21/95 */
    WHEN "{1}":U THEN p-rowid-list = p-rowid-list + 
        IF AVAILABLE {1} THEN STRING(ROWID({1}))
        ELSE "?":U.
   
