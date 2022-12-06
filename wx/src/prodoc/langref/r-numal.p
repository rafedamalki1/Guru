/*r-numal.p */
DEF VAR I AS INT.
DISPLAY NUM-ALIASES LABEL "Number of Defined Aliases:".
REPEAT I = 1 TO NUM-ALIASES.
    DISPLAY ALIAS(I) LABEL "Aliases"
	LDBNAME(ALIAS(I)) LABEL "Logical Database".
END.
