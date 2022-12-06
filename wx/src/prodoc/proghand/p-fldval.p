/* p-fldval.p */

FOR EACH customer:
  UPDATE cust-num name max-credit terms
	 VALIDATE(IF (INPUT max-credit > 1000) AND
		      LOOKUP(terms,"net30,net45") <> 0 THEN TRUE
		  ELSE IF (INPUT max-credit <= 1000) AND
			   LOOKUP(terms, "net30, net60") <> 0 THEN TRUE
		  ELSE FALSE,"Terms are incorrect").
END.
