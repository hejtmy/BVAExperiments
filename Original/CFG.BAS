experimentu! = 2 ' !TODO! 
prekreslinastartu! = 0 'jestli se ma leva horni obrazovka automaticky prekreslit po stlaceni c'
DIM expname(experimentu!) AS STRING
DIM filename(experimentu!) AS STRING
' !TODO! 
expname(1) = "AlloEgo navigace"
expname(2) = "Exit program"
' !TODO! 
filename(1) = "CFGeeg.bas"


DO
  ' $INCLUDE: 'CFGview.bas'
  PRINT
  PRINT "MENU:"
  FOR i = 1 TO experimentu!
	PRINT i; "- "; expname(i)
  NEXT i
  PRINT " enter number 1 -"; experimentu!; ":"
  DO
		k$ = INKEY$
		IF k$ = "-" THEN CALL levelforcompare(getlevel - 1)
		IF k$ = "+" THEN CALL levelforcompare(getlevel + 1)
		if k$ = "b" then call beepbvastart(6,1)
		call beepbva
		LOCATE 12, 1
		PRINT " tracking threshold (level):"; getlevel
		PRINT " use -/+ to change"
		print " use b to beep"
		print
		PRINT " Jmeno: ";jmeno$;
		
		
    IF usetrackval = 0 THEN PRINT ",trackuje se MYS" ELSE PRINT ", trackuje se DIODA"
		configno = VAL(k$)
		CALL getxy(x, y)
		CALL arenapos(angle, nic)
		CALL nahledcx(x, y, prr, angle, 0, 0, phases())
		
		LOCATE 28,1
		Print "DRF2 - software pro prezentaci navigacnich experimentu v BVA";
		PRINT "(c) Fyziologicky ustav AVCR, 2006-2010, Kamil Vlcek";

  LOOP WHILE configno = 0 OR configno > experimentu!
  PRINT configno
  
  ' !TODO! 
   IF configno = 1 THEN
    ' $INCLUDE: 'CFGad.bas'
		CALL main
  ELSEIF configno = 2 THEN
   PRINT "THE END"
  ELSE
    PRINT "  zadana spatna volba"; configno
  END IF
LOOP WHILE configno <> experimentu!