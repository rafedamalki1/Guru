/* External Procedure for Language Tutorial Problem 6-1 */

DEFINE INPUT PARAMETER Conference AS INTEGER.
DEFINE INPUT PARAMETER Hotel AS INTEGER.
DEFINE OUTPUT PARAMETER Total AS INTEGER.

ASSIGN Total = (Conference * 75) + (Hotel * 100).
