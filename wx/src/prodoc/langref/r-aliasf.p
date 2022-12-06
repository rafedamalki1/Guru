/* r-aliasf.p */
DEF VAR i as INT.
REPEAT i = 1 to NUM-ALIASES.
    DISPLAY ALIAS(i) LABEL "Alias"
	LDBNAME(ALIAS(i)) LABEL "Logical database".
END.
