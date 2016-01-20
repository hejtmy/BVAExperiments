DECLARE SUB eegwrite ()
DECLARE SUB debugopen ()
DECLARE SUB debugcues ()
DECLARE SUB debugclose ()
DECLARE FUNCTION usetrackval! ()
DECLARE FUNCTION getlevel ()
DECLARE SUB beepbvastart (beepcount!, vzdy!)
DECLARE SUB sndstart ()
DECLARE SUB sndstop ()
DECLARE SUB prepocitatxy (xa!, ya!)
DECLARE FUNCTION powerfit! (x!, Params AS ANY, minus AS INTEGER)
DECLARE FUNCTION Arcsin! (x!)
DECLARE FUNCTION arcsinfit! (x!, Params AS ANY)
DECLARE SUB setparam (vyska AS SINGLE, Arena AS ANY, Params AS ANY)
DECLARE SUB readini (lev AS INTEGER, ArenaRozmeryX AS ANY, ArenaRozmeryY AS ANY)
DECLARE SUB setvyska (vyska AS INTEGER)
DECLARE SUB getcameraxy (x!, y!)
DECLARE SUB arenapos (deg!, turn!)
DECLARE SUB mouseActual (tlac%, pozX%, pozY%)
DECLARE SUB mouseCursorOff ()
DECLARE SUB mouseCursorOn ()
DECLARE SUB mouseInit (ovladac%, tlacitek%)
' VERZE leden 2007 pro DRF 2 - jina deklarace cueswrite a mozna i neceho jineho
DECLARE SUB cekat (n!)
DECLARE SUB cueswrite (position%, symbol%, start%)
DECLARE SUB writeTolaser (location%)
DECLARE SUB clearcues ()
DECLARE SUB writeToStartPoint (location%)
DECLARE SUB writeSymbol (location%, symbol%)
DECLARE SUB rewritecues ()

DECLARE SUB levelforcompare (lev AS INTEGER)
DECLARE SUB resetframeflag ()
DECLARE SUB getxy (xa, ya)
DECLARE SUB setuptrack ()
DECLARE SUB cuesupdate (cuesnew() AS ANY)

' typy zavisle na projektu'
' $INCLUDE: 'typedef.bas'

' microsofti typy na assembler - kvuli mysi
' $INCLUDE: 'qbx.bi'

CONST USETRACK% = 1
CONST USECUES% = 1
CONST ROTATEARENA% = 0
CONST USESOUND% = 1
CONST DEBUG% = 0

CONST BEEPONLENGHT% = 6
CONST BEEPOFFLENGHT% = 3
CONST BEEPBETWEENLENGHT% = 6

DIM SHARED cues(8) AS cue
DIM ovladac AS OvladacType ' pro mys'
DIM actual AS StavType

DIM Paramsx AS PositionCompute
DIM Paramsy AS PositionCompute

' tohle se nacita z ini souboru'
DIM ArenaRozmeryX AS ArenaDim
DIM ArenaRozmeryY AS ArenaDim

DIM lev AS INTEGER 'tracking thrashold'

'private'
FUNCTION Arcsin (x!)
a = -x! * x! + 1
IF a < 0 THEN a = .1
Arcsin = ATN(x! / SQR(a))
END FUNCTION

'private'
FUNCTION arcsinfit (x!, Params AS SinFitParams)
  arcsinfit = Arcsin((x! - Params.x0) / Params.amplitude) * Params.period * 2 / 3.131592 + Params.y0
END FUNCTION

'PUBLIC'
SUB arenapos (deg, turn)
  STATIC lastpos
  IF USETRACK = 1 THEN
    wt = 7000 ' wait time
    docas = INP(&H306) AND &HF8 ' store the existing value
    OUT &H307, &H80 ' FF vstup, 80 vystup 306i305
    OUT &H306, docas + 1 ' give me low byte of position
    FOR j = 1 TO wt    ' wait to respond
    NEXT j
    
    OUT &H307, &HFF ' FF vstup, 80 vystup 306i305
    low = INP(&H305)  ' read the low byte
    
    OUT &H307, &H80 ' FF vstup, 80 vystup 306i305
    OUT &H306, docas + 3    ' give me the high byte of position
    FOR j = 1 TO wt ' wait to respond
    NEXT j
    
    OUT &H307, &HFF ' FF vstup, 80 vystup 306i305
    high = INP(&H305)    ' read the high byte
    
    OUT &H307, &H80 ' FF vstup, 80 vystup 306i305
    OUT &H306, docas + 5    ' give me the number of turns
    FOR j = 1 TO wt ' wait to respond
    NEXT j
    
    OUT &H307, &HFF ' FF vstup, 80 vystup 306i305
    turns = INP(&H305)    ' read the number of turns
    
    OUT &H307, &H80 ' FF vstup, F0 vystup 306i305
    OUT &H306, docas + 6 ' end of reading from the sensor
    FOR j = 1 TO wt ' wait to respond
    NEXT j
    
    deg = CINT((high * 256 + low) / 150)
  ELSE
    IF ROTATEARENA% THEN
	deg = lastpos + .2
    END IF
    lastpos = deg
    IF lastpos > 360 THEN lastpos = lastpos - 360
    deg = CINT(deg)
  END IF
END SUB

SUB beepbva ' to se vola pri kazdem cyklu'
  LOCATE 27, 35
  SHARED beeptodo, beepon, beepoff
  IF beeptodo > 0 THEN 'pokud se ma nekolikrat kratce beepnout'
    PRINT "beep"; beeptodo; beepon; beepoff;
    IF beepoff = 0 THEN
      IF beepon > 0 THEN
	beepon = beepon - 1
	  IF beepon = 0 THEN
	      IF beeptodo > 1 THEN
		beepoff = BEEPOFFLENGHT%
	      ELSE
		beepoff = BEEPBETWEENLENGHT%
	      END IF
	      CALL sndstop
	  END IF
      END IF
    END IF
    IF beepon = 0 THEN
      IF beepoff > 0 THEN
	beepoff = beepoff - 1
	IF beepoff = 0 THEN
	  beeptodo = beeptodo - 1
	  CALL beepbvastart(beeptodo, 1)
	END IF
      END IF
    END IF
  ELSEIF beepon > 0 THEN 'pokud trvale zni zvuk'
		PRINT "beep avoid     ";
	ELSE
		PRINT "               ";
  END IF
END SUB

SUB beepbvastart (beepcount, vzdy) ' to se vola na spusteni beepani'
  SHARED beeptodo, beepon, beepoff 'pocty kolikrat '
  IF beepcount > 0 AND (beeptodo = 0 OR vzdy > 0) THEN
    beeptodo = beepcount
    beepon = BEEPONLENGHT% ' pocet cyklu po ktere se ma trvat jeden beep'
    beepoff = 0
    CALL sndstart
  END IF
END SUB

SUB cekat (n)
  DIM i AS LONG
  FOR i = 1 TO n * 100
  NEXT i
END SUB

'PUBLIC'
SUB clearcues
  ' smaze vsechny cues a napise to do globalni promenne cues
  DIM pom AS cue
  FOR i% = 1 TO 8
    pom.startPoint = 0
    pom.segments = 0
    pom.laser = i%
    cues(i%) = pom
  NEXT i%
  CALL rewritecues
END SUB

SUB cuesreset
	' smaze jen ty cues,ktere sviti
	DIM i AS INTEGER
	FOR i = 1 TO 8
		IF (cues(i).segments > 0 OR cues(i).startPoint > 0) THEN
			CALL cueswrite(i, 0, 0)
			cues(i).segments = 0
			cues(i).startPoint = 0
		END IF
	NEXT
	
END SUB

' kamil 28.1.2008 - prijima pole CUESNEW(8), ktere obsahuje nove hodnoty, jak se maji cues rozsvitit
' porovna se starymi hodnotami a zmeni jen ty potrebne
SUB cuesupdate (cuesnew() AS cue)
    FOR i% = 1 TO 8
      IF cuesnew(i%).startPoint <> cues(i%).startPoint OR cuesnew(i%).segments <> cues(i%).segments THEN
	CALL cueswrite(i%, cuesnew(i%).segments, cuesnew(i%).startPoint)'to zapise in informace do globalni promenne cues'
      END IF
    NEXT i%
    CALL debugcues
END SUB

SUB cueswrite (position%, symbol%, start%)
	' napisu to do globalni promenne
	cues(position%).laser = position%
	cues(position%).startPoint = start%
	cues(position%).segments = symbol%
	
	IF USECUES% = 1 THEN
	IF (position% >= 1 AND position% <= 8 AND symbol% >= 0 AND symbol% <= 3) THEN
		FOR i = 1 TO 30000
		NEXT

		OUT &H378, position% - 1 + 8
		IF symbol% = 0 THEN segments = 1 'to switch off
		IF symbol% = 1 THEN segments = 3 'two horizontal lines'
		IF symbol% = 2 THEN segments = 0 'three vertical lines'
		IF symbol% = 3 THEN segments = 2 'zero sign'
		IF (start% > 0) THEN
			startPoint = 0
		ELSE
			startPoint = 8
		END IF
		FOR i = 1 TO 30000
		NEXT
	
		'OUT &H37A, startPoint OR (segments * 2) OR 1
		OUT &H37A, ((segments) * 2 + startPoint) OR (INP(&H37A) AND &HF1)
		'OUT &H37A, ((pom.segments XOR 5) * 2 + pom.startPoint * 8) OR (INP(&H37A) AND &HF1)
		FOR i = 1 TO 30000
		NEXT

		OUT &H378, INP(&H378) AND &HF7
		'PRINT st OR (s * 2) OR 1
		'WritePort (0x37A , (UCHAR) st | s*2 | 1 ) ;

	END IF
	END IF
END SUB

SUB eegwrite (on%)   '14.4.2015 synchronizace s iEEG'
    IF on% == 1
        OUT &H378, 255
        OUT &H37A, 1 'strobe on'
    ELSE
        OUT &H378, 4
        OUT &H37A, 0 'strobe on'
    END IF
END SUB

SUB debugclose
	IF DEBUG = 1 THEN
		CLOSE #4
	END IF
END SUB

SUB debugcues
	IF DEBUG = 1 THEN
	PRINT #4, DATE$, TIME$, "kaminsky: debugcues"
	FOR i% = 1 TO 8
		PRINT #4, cues(i%).laser, cues(i%).segments, cues(i%).startPoint
	NEXT i%
	END IF
END SUB

SUB debugopen
	IF DEBUG = 1 THEN
		OPEN "kaminsky.dbg" FOR OUTPUT AS #4
	END IF
END SUB

SUB debugsetcue (cc AS cue)
	IF DEBUG = 1 THEN
		PRINT #4, DATE$, TIME$, "drf2func: debugsetcue; laser,segments, startpoint"
	PRINT #4, cc.laser, cc.segments, cc.startPoint
  END IF
END SUB

'private'
SUB getcameraxy (x, y) 'oddelena funkce od getxy je kvuli kalibraci rozmeru areny'
  SHARED ArenaRozmeryX AS ArenaDim
  SHARED ArenaRozmeryY AS ArenaDim
  IF USETRACK = 1 THEN
       CALL resetframeflag
       OUT (795), 0 ' reset FF
       WAIT &H302, 8                 'wait for FF on (bit 3 = 1)
       x = INP(&H30C)               'x and y corrected to fit the
       y = INP(&H30D)
       IF x < ArenaRozmeryX.p0 THEN
	  LOCATE 27, 33
	  PRINT "x < arenax0: "; x; ArenaRozmeryX.p0
	  x = ArenaRozmeryX.p0
       END IF
       IF x > ArenaRozmeryX.p1 THEN
	  LOCATE 27, 33
	  PRINT "x > arenax1: "; x; ArenaRozmeryX.p1
	  x = ArenaRozmeryX.p1
       END IF
       IF y < ArenaRozmeryY.p0 THEN
	  LOCATE 27, 33
	  PRINT "y < arenay0: "; y; ArenaRozmeryY.p0
	  y = ArenaRozmeryY.p0
       END IF
       IF y > ArenaRozmeryY.p1 THEN
	  LOCATE 27, 33
	  PRINT "y > arenay1: "; y; ArenaRozmeryY.p1
	  y = ArenaRozmeryY.p1
       END IF
       CALL resetframeflag
  END IF
END SUB

SUB getcues (cc() AS cue)
      FOR i = 1 TO 8
	cc(i) = cues(i)
      NEXT i
END SUB

FUNCTION getlevel
	SHARED lev AS INTEGER
	getlevel = lev
END FUNCTION

'PUBLIC'
SUB getxy (xa, ya)
		STATIC x
		STATIC y
   IF USETRACK = 1 THEN
     CALL getcameraxy(xa, ya)
     CALL prepocitatxy(xa, ya)
   ELSE
     DIM actual AS StavType
     'xa = x
     'ya = y
     'x = x + INT(RND(10) * 4 - 2)
     'y = y + INT(RND(10) * 4 - 2)
     CALL cekat(1000)
     ' MYS:'
      CALL mouseActual(actual.tlac, actual.pozX, actual.pozY)
      'actual.pozX = 241
      'actual.pozY = 288
      'IF actual.tlac > 0 THEN
	LOCATE 27, 2
	PRINT "mys X:"; actual.pozX; "Y:"; actual.pozY;
	' 2009-07-23 pro toucscreen
	xa = actual.pozX - 470 'INT((actual.pozX) / 2.3) - 140
	ya = actual.pozY - 160 'INT((actual.pozY) / 1.7) - 140
      'ELSE
      '  xa = x
      '  ya = y
      'END IF
END IF
END SUB

' virtualni faze phases(currentphase).virtual = 1 '
SUB getxymouse (xa, ya)
  DIM actual AS StavType
     'xa = x
     'ya = y
     'x = x + INT(RND(10) * 4 - 2)
     'y = y + INT(RND(10) * 4 - 2)
  CALL cekat(1000)
     ' MYS:'
  CALL mouseActual(actual.tlac, actual.pozX, actual.pozY)
      'actual.pozX = 241
      'actual.pozY = 288
      'IF actual.tlac > 0 THEN
	LOCATE 27, 2
	PRINT "mys X:"; actual.pozX; "Y:"; actual.pozY
	'xa = INT((actual.pozX) / 2.3) - 140
	'ya = INT((actual.pozY) / 1.7) - 140
	'xa = actual.pozX - 140
	'ya = actual.pozY - 140
	'IF xa > 140 THEN xa = 140
	'IF ya > 140 THEN ya = 140
	' 2009-07-23 - pro touchscreen
	xa = actual.pozX - 355 'INT((actual.pozX) / 2.3) - 140
	ya = actual.pozY - 160 'INT((actual.pozY) / 1.7) - 140
END SUB

SUB levelforcompare (level AS INTEGER)
   SHARED lev AS INTEGER
   IF USETRACK = 1 THEN
      OUT &H316, level 'level for comparato
      FOR i% = 1 TO 1000 ' wait a little
      NEXT
   END IF
   lev = level
END SUB

SUB mouseActual (tlac%, pozX%, pozY%)
DIM regs AS RegType
regs.ax = 3
CALL Interrupt(&H33, regs, regs)
tlac% = regs.bx
pozX% = regs.cx
pozY% = regs.dx
END SUB

SUB mouseCursorOff
DIM regs AS RegType
regs.ax = 2
CALL Interrupt(&H33, regs, regs)
END SUB

SUB mouseCursorOn
DIM regs AS RegType
regs.ax = 1
CALL Interrupt(&H33, regs, regs)
END SUB

SUB mouseInit (ovladac%, tlacitek%)
  DIM regs AS RegType
  regs.ax = 0
  CALL Interrupt(&H33, regs, regs)
  ovladac% = regs.ax
  tlacitek% = regs.bx
END SUB

'private'
FUNCTION powerfit (x!, Params AS PowerFitParams, minus AS INTEGER)
  IF minus = 0 THEN
    powerfit = Params.baseline + (x! / Params.amplitude) ^ Params.power
  ELSE
    powerfit = Params.baseline - (x! / Params.amplitude) ^ Params.power
  END IF
END FUNCTION

SUB prepocitatxy (xa, ya)
   SHARED Paramsx  AS PositionCompute
   SHARED Paramsy  AS PositionCompute

     xa = (xa - Paramsx.Line1a) / Paramsx.Line1b'prepocitam na rozmer 0 - 280'
     ya = (ya - Paramsy.Line1a) / Paramsy.Line1b
     IF xa < 0 THEN xa = 0
     IF xa > 280 THEN xa = 280
     IF ya < 0 THEN ya = 0
     IF ya > 280 THEN ya = 280
     'PRINT "sinparams"; Paramsx.SinParams.x0, Paramsx.SinParams.y0, Paramsx.SinParams.period, Paramsx.SinParams.amplitude
     xa = arcsinfit(xa, Paramsx.SinParams)'prepocitam na primku 0-280'
     'PRINT "2"; xa
     ya = arcsinfit(ya, Paramsy.SinParams)
     xa = CINT(xa) - 140 ' stred areny je 0, vlevo -140'
     ya = CINT(ya) - 140 ' stred areny je 0, nahore -140'

END SUB

'private'
SUB readini (lev AS INTEGER, ArenaRozmeryX AS ArenaDim, ArenaRozmeryY AS ArenaDim)
  OPEN "kaminsky.ini" FOR INPUT AS #3
  LINE INPUT #3, nic$
  ' threshold for identification of the LED diode'
  INPUT #3, lev
  ' pixely z kamery na okrajich areny u tyce 163 cm
  INPUT #3, ArenaRozmeryX.p0
  INPUT #3, ArenaRozmeryX.p1
  INPUT #3, ArenaRozmeryY.p0
  INPUT #3, ArenaRozmeryY.p1
  CLOSE #3
END SUB

SUB resetframeflag
    IF USETRACK = 1 THEN
      FOR i = 1 TO 10000
      NEXT
      OUT &H31B, 0                       'reset Frame Flag (FF)
   END IF
END SUB

SUB rewritecues
  IF USECUES% = 1 THEN
  DIM pom AS cue
   ' stary kod z HGT'
  FOR i% = 1 TO 8
    pom = cues(i%)
    FOR j% = 0 TO 30000
    NEXT j%
    OUT &H378, i% - 1 + 8
    FOR j% = 0 TO 30000
    NEXT j%
    OUT &H37A, ((pom.segments XOR 5) * 2 + pom.startPoint * 8) OR (INP(&H37A) AND &HF1)
    FOR j% = 0 TO 30000
    NEXT j%
    OUT &H378, INP(&H378) AND &HF7
    FOR j% = 0 TO 30000
    NEXT j%
  NEXT i%
  END IF
END SUB

'private'
SUB setparam (vyska AS SINGLE, Arena AS ArenaDim, Params AS PositionCompute)
  Params.stred = Arena.p0 + (Arena.p1 - Arena.p0) / 2
  
  Params.minFitParams.power = 1.5461      'fit pomoci Statistica 8'
  Params.minFitParams.amplitude = 14.3124 'fit pomoci Statistica 8'
  Params.minFitParams.baseline = Arena.p1 - 43    'fit pomoci Statistica 8'
  Params.maxFit = powerfit(vyska, Params.minFitParams, 0)
  Params.minFit = 2 * Params.stred - Params.maxFit
  
  Params.Line1a = Params.minFit
  Params.Line1b = (Params.maxFit - Params.minFit) / (280 - 0)'280 a 0 jsou rozmery areny v cm'
  
  Params.SinAmplParams.power = 1.7695
  Params.SinAmplParams.amplitude = 15.8209
  Params.SinAmplParams.baseline = 210
  
  Params.SinPeriodParams.power = 1.866
  Params.SinPeriodParams.amplitude = 12.4186
  Params.SinPeriodParams.baseline = 300
  
  Params.SinParams.x0 = 140
  Params.SinParams.y0 = 138
  Params.SinParams.amplitude = powerfit(vyska, Params.SinAmplParams, 1)
  Params.SinParams.period = powerfit(vyska, Params.SinPeriodParams, 1)
END SUB

SUB setupkaminsky
  SHARED ArenaRozmeryX AS ArenaDim
  SHARED ArenaRozmeryY AS ArenaDim
  SHARED lev AS INTEGER
  CALL setuptrack
  CALL clearcues
  lev = 0 'threshold for LED indentification'
  CALL readini(lev, ArenaRozmeryX, ArenaRozmeryY)
  CALL levelforcompare(lev)
  vyskaDefault% = 163
  CALL setvyska(vyskaDefault%)
  CALL debugopen
END SUB

SUB setuptrack
   IF USETRACK = 1 THEN
   OUT &H31D, 0
	'Initialization of the TV tracking
	'*******************************************************************
	'ChipSetup2F:
	'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sPP21:         OUT &H303, &H93            '.1.00.1.0.0.1.1
	'             .1.00.A. .Ch.0.B.Cl.  (1-in, 0-out)
		'A-&H0-in, B-&H1-in, C-&H2 (l-in, h-out)
	'---------------------------------------------------
sPP20:          OUT &H307, &H93           '1.0.0.1. .0.0.1.1.
			'.1.0.0.A. .Ch.0.B.Cl.    1-in, 0-out
		' A-&H4-in, B-&H5-in, C-&H6 (l-in, h-out)
	'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	'  .UU.XX.YYY.Z.  UU  - counter number (00,01,10)
	'                 XX  - use high or/and low byte
	'                 YYY - mode
	'                 Z   - binar or bin-dec counter
	'- - - - - - - - - - - - - - - - - - - - - - - - - - - -
	'Counter-Timer 8253 chip 18 :        &H30C, &H30D, &H30E, &H30F
	'............................
	
sTM18.0:        OUT &H30F, &H1A           ' .00.01.101.0.
			OUT &H30C, &HFF   'X1-counter
	'_________________________________________________( 9,10,11)______
	
sTM18.1:        OUT &H30F, &H5A           ' .01.01.101.0.
			OUT &H30D, &HFF   'Y1-counter
	'_________________________________________________(15,13,14)______
	
sTM18.2:        OUT &H30F, &H94           ' .10.01.010.0.
				Ftv = 50  'Hz
				SaRa = 25
				div = Ftv / SaRa  'frame shared system
			OUT &H30E, div    ' FS divider  (T=40mS)
	'_________________________________________________(18,17,16)______
	
	'Counter-Timer 8253 chip 27:          &H310, &H311, &H312, &H313
	'...........................
	
sTM27.2:        OUT &H313, &H84           ' .10.01.010.0.
			OUT &H312, 160    ' divid 5MHz to 32kHz
	'_________________________________________________(18,17,16)______
	
	'*******************************************************************
	ELSE
	 DIM ovladac AS OvladacType ' pro mys'
	 CALL mouseInit(ovladac.pritomnost, ovladac.tlacitek)
	 CALL mouseCursorOn
	END IF
END SUB

'PUBLIC'
SUB setvyska (vyska AS INTEGER)
  SHARED ArenaRozmeryX AS ArenaDim
  SHARED ArenaRozmeryY AS ArenaDim
  SHARED Paramsx AS PositionCompute
  SHARED Paramsy AS PositionCompute
  
  CALL setparam(CSNG(vyska), ArenaRozmeryX, Paramsx)
  CALL setparam(CSNG(vyska), ArenaRozmeryY, Paramsy)
END SUB

SUB sndbvastart 'zapne se trvaly zvuk'
  SHARED beepon, beepcount, beepoff
  beepon = 1
  beepoff = 0
  beepcount = 0
  CALL sndstart
END SUB

SUB sndbvastop 'vypne se trvaly zvuk'
  SHARED beepon, beeptodo
  IF beeptodo = 0 THEN 'jen pokud nebeepam'
    beepon = 0
    CALL sndstop
  END IF
END SUB

SUB sndstart
IF USESOUND = 1 THEN
  docas = (INP(&H80) AND &H7F)
  OUT &H307, &H80 ' FF vstup, 80 vystup 306i305
  OUT &H306, docas + &H80   ' make sound
ELSE
  SOUND 880, 1
END IF
END SUB

SUB sndstop
IF USESOUND = 1 THEN
	docas = (INP(&H80) AND &H7F)
	OUT &H307, &H80 ' FF vstup, 80 vystup 306i305
	OUT &H306, docas   ' make sound
END IF
END SUB

SUB trackreset
  IF USETRACK <> 1 THEN
    mouseCursorOff
  END IF
  CALL debugclose
END SUB

FUNCTION usetrackval
	usetrackval = USETRACK%
END FUNCTION

SUB writeSymbol (location%, symbol%)
  DIM pom AS cue
  IF USETRACK = 1 THEN
	  pom = cues(location%)
	  pom.segments = symbol%
	  cues(location%) = pom
	  'PRINT "###", location%, pom.segments, pom.startPoint
	  'PRINT location% - 1 + 8, ((pom.segments XOR 5) * 2 + pom.startPoint * 8) OR (INP(&H37A) AND &HF1)
	  FOR i% = 1 TO 30000
	  NEXT i%
	  OUT &H378, location% - 1 + 8
	  FOR i% = 1 TO 30000
	  NEXT i%
	  OUT &H37A, ((pom.segments XOR 5) * 2 + pom.startPoint * 8) OR (INP(&H37A) AND &HF1)
	  FOR i% = 1 TO 30000
	  NEXT i%
	  OUT &H378, INP(&H378) AND &HF7
	  FOR i% = 1 TO 30000
	  NEXT i%
	END IF
END SUB

SUB writeTolaser (location%)
  IF 0 THEN ' lasery uz ovladame rucne'
   OUT &H378, (((7 + location%) * 16) AND &HF0) OR (INP(&H378) AND &HF)
  END IF
END SUB

SUB writeToStartPoint (location%)
  IF USETRACK = 1 THEN
	  DIM pom AS cue
	  FOR i% = 1 TO 8
	    pom = cues(i%)
	    pom.startPoint = 0
	    cues(i%) = pom
	  NEXT i%
	  IF (location% >= 1) AND (location% <= 8) THEN
	    pom = cues(location%)
	    pom.startPoint = 1
	    cues(location%) = pom
	  END IF
	  CALL rewritecues
	END IF
END SUB

