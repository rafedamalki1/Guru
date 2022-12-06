/* r-modulo.p */

REPEAT:
  SET qty-avail AS INTEGER LABEL "Qty. Avail.".
  SET std-cap AS INTEGER
	LABEL "Std. Truck Capacity".
  DISPLAY TRUNCATE(qty-avail / std-cap,0)
    FORMAT ">,>>9" LABEL "# Full Loads"
    qty-avail MODULO std-cap LABEL "Qty. Left".
END.
