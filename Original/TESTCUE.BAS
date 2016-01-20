DECLARE SUB PPstartDigit (centerx!, centery!, radius!, barva%, c!)
DECLARE SUB PPcueDigit (centerx!, centery!, radius!, barva%, cleft!, cright!, segments!)
SCREEN 12
FOR i = 1 TO 6
cleft = i
cright = i + .2
'CIRCLE (200, 200), 150, 15, cleft, cright, 1
CALL PPcueDigit(200, 200, 150, 4, cleft, cright, 3)
CALL PPstartDigit(200, 200, 150, 2, cleft)
NEXT

