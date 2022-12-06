/*r-rdvh.p

Sample program to read a Volume Home Block using CTOS requests.
*/
DEF NEW SHARED VARIABLE ercret      AS INTEGER.
DEF NEW SHARED VARIABLE vhb         AS INTEGER   EXTENT  128.
DEF NEW SHARED VARIABLE dev         AS CHARACTER INITIAL "Sys".
DEF            VARIABLE FreeSectors AS INTEGER.

/* request vhb */
CTOS OS-REQUEST 15 ercret 3 1 1 0 0 0 %c[12] dev %u[128] vhb[1].

FreeSectors = vhb[55] + vhb[56] * 65536.
DISPLAY ercret dev
	SKIP(1)
	vhb[45]     LABEL "Free File Headers"
	SKIP(1)
	FreeSectors LABEL "Free Sectors"     WITH SIDE-LABELS.
