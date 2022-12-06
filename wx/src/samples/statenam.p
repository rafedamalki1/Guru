/************************************************************************************
	PROCEDURE: statenam.p

	PURPOSE:   Returns the state name

	SYNTAX:    RUN statenam.p (INPUT in, OUTPUT out).

	REMARKS:   This code returns the name of the state given in the 
		   state abbreviation.

	PARAMETERS:
            INPUT:  state abbreviation
            OUTPUT: state name

	AUTHORS:   Judy Rothermal
	DATE:      February 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */

/*Code_Start*/

DEFINE INPUT  PARAMETER state-abbr  AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER state-name  AS CHAR NO-UNDO.

state-name =
ENTRY(LOOKUP(state-abbr,"AK,AL,AR,AZ,CA,CO,CT,DC,DE,FL,GA,HI,IA,ID," +
  "IL,IN,KA,KY,LA,MA,MD,ME,MI,MN,MO,MS,MT,NC,ND,NE,NH,NJ,NM,NV,NY,OH,OK," +
  "OR,PA,PR,RI,SC,SD,TN,TX,UT,VA,VI,VT,WA,WI,WV,WY") + 1,
  "?,Alaska,Alabama,Arkansas,Arizona,California,Colorado,Connecticut," +
  "Dst of Columbia,Delaware,Florida,Georgia,Hawaii,Iowa,Idaho,Illinois," +
  "Indiana,Kansas,Kentucky,Louisiana,Massachusetts,Maryland,Maine,Michigan," +
  "Minnesota,Missouri,Mississippi,Montana,North Carolina,North Dakota," +
  "Nebraska,New Hampshire,New Jersey,New Mexico,Nevada,New York,Ohio," +
  "Oklahoma,Oregon,Pennsylvania,Puerto Rico,Rhode Island,South Carolina," +
  "South Dakota,Tennessee,Texas,Utah,Virginia,Virgin Islands,Vermont," +
  "Washington,Wisconsin,West Virginia,Wyoming").


