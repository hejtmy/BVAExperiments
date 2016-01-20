DECLARE SUB PPcueDigit (centerx!, centery!, radius!, barva%, cleft!, cright!, segments!)
DECLARE SUB PPstartDigit (centerx!, centery!, radius!, barva%, c!)
DECLARE SUB linedouble (x0!, y0!, x1!, y1!, barva%)
DECLARE FUNCTION max! (a0!, a1!)
DECLARE SUB getcues (cc() AS ANY)
DECLARE SUB arenapos (deg!, turn!)
DECLARE SUB nahledcx (x!, y!, prr!, angle!, avoid!, curf!, pp() AS ANY)
DECLARE SUB PPcue (centerx!, centery!, radius!, cc AS ANY, angle!)
DECLARE FUNCTION PPsectordist! (pozice AS ANY, prr!, ss AS ANY)
DECLARE SUB cuesreset ()
DECLARE SUB cueswrite (position%, symbol%, start%)
DECLARE SUB PPconsiderCameraAngle (x!, y!, xr!, yr!, xc!, yc!)
DECLARE FUNCTION PPelipseRadius! (a!, b!, rad!)
DECLARE SUB PPavoidcircle2 (Pc1x!, Pc1y!, prr!, angle!, r0!, r!)
DECLARE SUB PPstartnahled (Pc1x!, Pc1y!, prr!, angle!)
DECLARE SUB PPinavoidcircle (x!, y!, ax!, ay!, ar!, inavoid!, prr!)
DECLARE SUB PPangledist2xy (angle!, dist!, x!, y!)
DECLARE SUB PProtate (x!, y!, angledeg!)
DECLARE SUB PParena (x!, y!, r!)
DECLARE SUB PPavoid (x!, y!, r!, c!, w!, d0!, d1!)
DECLARE SUB PPset (x!, y!, xc!, yc!, avoid!)
DECLARE FUNCTION distance! (x0!, y0!, x1!, y1!)
DECLARE FUNCTION deg! (rad!)
DECLARE FUNCTION PPxy2dist! (x, y)
DECLARE FUNCTION PPxy2angle! (x!, y!)
DECLARE FUNCTION rad! (degg!)
DECLARE FUNCTION sectorcenter (ss AS ANY)
DECLARE FUNCTION sectordistance (ss AS ANY)
DECLARE FUNCTION empty (a$)
DECLARE SUB prevcenter (p AS ANY)

' typy zavisle na projektu'
' $INCLUDE: 'typedef.bas'

CONST VYSKAARENY% = 300
CONST VYSKATYCE% = 165

'PUBLIC'
SUB arenado0 (prr, pp() AS phase)
  SCREEN 12
  CLS
  LOCATE 2, 2
  PRINT "natoceni areny"
  PRINT "pro ukonceni stiskete klavesu"
  DO
    CALL arenapos(uhel, turn)
    LOCATE 4, 1
    PRINT uhel; "stupnu"
    CALL nahledcx(0, 0, prr, uhel, 0, 0, pp())
  LOOP WHILE INKEY$ = ""
END SUB

FUNCTION deg! (radd!)
pi! = 3.121592
deg! = 180 * radd! / pi
END FUNCTION

FUNCTION distance (x0, y0, x1, y1)
	distance = SQR((CINT(x1) - CINT(x0)) ^ 2 + (CINT(y1) - CINT(y0)) ^ 2)
	' bez tech cint mi to obcas hazelo nejake division by zero
END FUNCTION

SUB linedouble (x0, y0, x1, y1, barva%)
	LINE (x0, y0)-(x1, y1), barva%
	LINE (x0, y0 + 1)-(x1, y1 + 1), barva%
	LINE (x0 + 1, y0)-(x1 + 1, y1), barva%
END SUB

SUB nahled (x, y, prr, angle, avoid, Rrc, Rrw, Rr0, Rr1, Arc, Arw, Ar0, Ar1)
	radius = 65
	left = 470
	top = 320
	centerx = radius
	centery = centerx
	VIEW (left, top)-(left + radius * 2, top + radius * 2), 0
	CLS
	CALL PParena(centerx, centery, radius)
	CALL PPavoid(centerx, centery, radius, Rrc, Rrw, Rr0, Rr1)
	CALL PPavoid(centerx, centery, radius, Arc + angle, Arw, Ar0, Ar1)
	xa = x * radius / prr ' prr is the radius of the large arena (140)
	ya = y * radius / prr

	CALL PPset(xa, ya, centerx, centery, avoid)
	VIEW
END SUB

' ted se ted pouziva 30.11.2007'
SUB nahledcx (x!, y!, prr!, angle!, avoid!, curf!, pp() AS phase)
' sn - pocet sektoru, s-sektory
' xy - [0;0] is in the middle and radius is prr
  STATIC refresh
  IF pp(curf!).noroomframe = 1 AND pp(curf!).noarenaframe = 1 THEN
	' velky nahled'
	 radius = 140 ' 65
	 left = 200 '460
	 top = 10 '320
	 refreshtime = 7
	ELSEIF pp(curf!).noarenaframe = 1 THEN
	 ' velky nahled'
	 radius = 140 ' 65
	 left = 310 '460
	 top = 10 '320
	 refreshtime = 7
	ELSE 'maly nahled'
	 radius = 65
	 left = 460
	 top = 320
	 refreshtime = 2
	END IF
	center = radius * 1.1
	cueradius = radius * 1.05
	
	VIEW (left, top)-(left + center * 2, top + center * 2)
	' nakreslim arenu
	IF refresh >= refreshtime THEN
    CLS
    CALL PParena(center, center, radius)
    refresh = 0
  ELSE
    refresh = refresh + 1
  END IF
	' nakreslim sektory na vyhybani - hledani
	if pp(curf).virtual = 0 or pp(curf).goalvisible = 1 then
		FOR i = 0 TO pp(curf).sectorsno - 1
		  IF pp(curf).sectors(i).mode = -1 THEN '-1 kopie jine faze, podle kopiez'
		   faze = pp(curf).sectors(i).kopiez
		   DIM ss AS sector
		   ss = pp(faze).sectors(i)
		  ELSE
		   ss = pp(curf).sectors(i)
		  END IF
			IF (ss.shape = 0) THEN '0 - sector
				IF ss.mode = 0 THEN 'rf
					CALL PPavoid(center, center, radius, sectorcenter(ss), ss.w, sectordistance(ss), ss.r1)
				ELSE
					CALL PPavoid(center, center, radius, sectorcenter(ss) + angle, ss.w, sectordistance(ss), ss.r1)
				END IF
			ELSE '1 - circle'
				IF ss.mode = 0 THEN 'rf
					CALL PPavoidcircle2(center, center, radius, sectorcenter(ss), sectordistance(ss), ss.r)
				ELSE
					CALL PPavoidcircle2(center, center, radius, sectorcenter(ss) + angle, sectordistance(ss), ss.r)
				END IF
			END IF
			IF ss.crelativeto > 0 THEN
				CALL PPstartnahled(center, center, radius, ss.cstart)
			END IF
		NEXT i
	end if 'if virtual = 0'
	
	' jeste nakreslim cues - kamil 30.11.2007
	DIM cues(8)  AS CUE
	CALL getcues(cues()) 'ziska nastaveni cues z modulu kaminsky'
	FOR i = 0 TO 8
	  DIM cc AS CUE
	  IF cues(i).segments > 0 OR cues(i).startPoint > 0 THEN
		cc.segments = cues(i).segments
		cc.laser = cues(i).laser
		cc.startPoint = cues(i).startPoint
		CALL PPcue(center, center, cueradius, cc, angle)
	  END IF
	NEXT i
	' nakreslim pozici subjektu
	if pp(curf!).virtual = 0 then
		CALL PPset(x * radius / prr, y * radius / prr, center, center, avoid)
	else
		line (x*radius/prr + center -1,y * radius / prr + center -1)-(x*radius/prr + center +1,y * radius / prr + center +1),,BF
	end if
	VIEW
END SUB

' posunuje carr i r0arr - 8.4.2008
SUB nextcenter (p AS phase, currentpos AS position)
	FOR j = 0 TO p.sectorsno - 1
	    IF p.sectors(j).cno > 0 THEN
	      p.sectors(j).c = p.sectors(j).c + 1
	      IF p.sectors(j).c >= p.sectors(j).cno THEN p.sectors(j).c = 0
	    END IF
	    IF p.sectors(j).r0no > 0 THEN
		p.sectors(j).r0 = p.sectors(j).r0 + 1
		IF p.sectors(j).r0 >= p.sectors(j).r0no THEN p.sectors(j).r0 = 0
	    END IF
	NEXT j
END SUB

'posune na dalsi cue'
SUB nextcue (cuesno AS SINGLE, cues() AS CUE)
	IF cuesno > 0 THEN
	  FOR i = 0 TO cuesno - 1
		 IF cues(i).laserno > 0 THEN '29.11.2007 - vice pozic lazeru'
		   cues(i).laser = cues(i).laser + 1 ' vola jen jednou za fazi'
		   IF cues(i).laser >= cues(i).laserno THEN cues(i).laser = 0
		 END IF
	  NEXT i
	END IF
END SUB

SUB PPangledist2xy (angle, dist, x, y)
' 0 deg is on the right, angle in radians
	x = COS(angle) * dist
	y = SIN(angle) * dist
END SUB

SUB PParena (x, y, r)

CIRCLE (x, y), r

END SUB

SUB PPavoid (x, y, r, c, w, d0, d1)

pi = 3.141592
cx = c MOD 360

a0 = rad(cx) - rad(w) / 2
IF (a0 < 0) THEN a0 = 2 * pi + a0
a1 = rad(cx) + rad(w) / 2
IF (a1 < 0) THEN a1 = 2 * pi + a1
IF (a1 > 2 * pi) THEN a1 = a1 - 2 * pi

CIRCLE (x, y), (r * d1) / 100, 4, a0, a1
CIRCLE (x, y), (r * d0) / 100, 4, a0, a1

END SUB

SUB PPavoidcircle (ax, ay, ar, Pc1x, Pc1y, prr)
	' ax, ay, ar are from 0 to 1
	x = Pc1x + prr * ax
	y = Pc1y + prr * ay
	r = prr * ar
	CIRCLE (x, y), r, 4
END SUB

SUB PPavoidcircle2 (Pc1x, Pc1y, prr, angle, r0, r)
  ' target circle defined by % from center (r0), center angle and radius'
  ' prr is radius of the arena on screen'
  ' r0, r is from 0 to 100'
  ' angle is in deg'
  angle = angle MOD 360
  CALL PPangledist2xy(rad(angle), r0 / 100, x, y)
  CIRCLE (Pc1x + prr * x, Pc1y - prr * y), (r * prr / 100), 4
END SUB

SUB PPavoidcircle2more (Pc1x, Pc1y, prr, s AS sector)
  IF s.cno > 0 OR s.r0no > 0 THEN
    FOR i = 0 TO max(s.cno, s.r0no) - 1
      IF s.cno >= i + 1 THEN sc = s.carr(i) ELSE sc = s.c
      ' tohle kvuli relativnimu startu'
			IF s.crelativeto > 0 THEN
		    sc = sc + s.cstart
	    END IF
	    WHILE sc >= 360
		sc = sc - 360
	    WEND
      IF s.r0no >= i + 1 THEN sr0 = s.r0arr(i) ELSE sr0 = s.r0
      CALL PPavoidcircle2(Pc1x, Pc1y, prr, sc, sr0, s.r)
    NEXT i
  ELSE
    CALL PPavoidcircle2(Pc1x, Pc1y, prr, s.c, s.r0, s.r)
  END IF
END SUB

SUB PPavoidmore (Pc1x, Pc1y, prr, s AS sector)
 IF s.cno > 0 THEN
    FOR i = 0 TO s.cno - 1
      CALL PPavoid(Pc1x, Pc1y, prr, s.carr(i), s.w, s.r0, s.r1)
    NEXT i
  ELSE
    CALL PPavoid(Pc1x, Pc1y, prr, s.c, s.w, s.r0, s.r1)
  END IF
END SUB

SUB PPconsiderCameraAngle (x, y, xr, yr, xc, yc)
' x,y jsou souradnice k prepocitani,
' xr yr jsou polomery areny jak jsou merene kamerou
	x = x - xc
	y = y - yc
	angleXY = PPxy2angle(x, y)
	distA = PPelipseRadius(xr, yr, angleXY)
	beta = ATN(distA / (VYSKAARENY% - VYSKATYCE%))
	alpha = beta * PPxy2dist(x, y) / distA
	dist1 = (VYSKAARENY% - VYSKATYCE%) * TAN(alpha)
	CALL PPangledist2xy(angleXY, dist1, x, y)
	x = x + xc
	y = -y + yc   'to - empiricky kvuli nekonzistenci polarity y
END SUB

SUB PPconsiderHeight (x, y, h%, r0)
	' r0 - sirka areny (max bodu), to je 140
	' h% - vyska cloveka
	' r2 - vzdalenost diody od stredu udana kamerou
	' alpha2 - uhel diody (od 0,ktery je primo dolu)
	' r1 - prepocitana vzdalenost cloveka od stredu
	r2 = PPxy2dist(x, y)
	alfa2 = ATN(r0 / (VYSKAARENY% - VYSKATYCE%)) * r2 / r0
	r1 = TAN(alfa2) * (VYSKAARENY% - h%)
	CALL PPangledist2xy(PPxy2angle(x, -y), r1, x, y)

	' to -y je tam aby se opravila nekonzistence pocitani s uhly - jinak to obracelo polaritu y
END SUB

SUB PPcue (centerx, centery, radius, cc AS CUE, angle!)
' nakresli cue - kamil 30.11.2007'
  pi2 = 2 * 3.14159265#
  'z 2 udelam 1 a z 1 udelam 8
  IF cc.laserno > 0 THEN
    laser = cc.laserarr(cc.laser) - 1
  ELSE
    laser = cc.laser - 1
  END IF
  IF laser < 1 THEN
    IF cc.arenaframe = 1 AND cc.c > 0 THEN 'pokud je laser = 0 a soucasne je nastaveno cc.c'
      c = rad(CINT(cc.c))
    ELSE
      laser = laser + 8
    END IF
  END IF
  IF c = 0 THEN ' pokud jsem ho nenastavil z cc.c
    c = laser / 8 * pi2 ' prevod pozice cue na radiany - nevim proc minus tri ale funguje to 1 je v pravo
  END IF
  IF cc.arenaframe = 1 THEN 'arenaframe cue'
    c = c + rad(angle!)
    WHILE c > pi2
    c = c - pi2
    WEND
    radius = CINT(radius * .85)   'kreslim do areny'
  END IF
  cleft = c - .09     'left margin of the cue = circle from
  WHILE cleft < 0
  cleft = cleft + pi2
  WEND
  cright = c + .09     'rigth margin of the cue
  WHILE cright >= pi2
  cright = cright - pi2
  WEND
  
	
	IF cc.segmentsno > 0 THEN
	  segments = cc.segmentsarr(cc.segments)
	ELSE
	  segments = cc.segments
	END IF
  'SEGMENTY 0 to 3, 1-two lines, 2 three lines, 3 zero sign
  CALL PPcueDigit(centerx, centery, radius, 4, cleft, cright, segments)
	
  '      IF segments = 0 THEN  'two horizontal lines'
  '        barva% = 0
  '      ELSEIF segments = 1 THEN 'three vertical lines'
  '        barva% = 10
  '      ELSEIF segments = 2 THEN
  '        barva% = 12
  '      ELSEIF segments = 3 THEN
  '        barva% = 9
  '      END IF
	
  '      IF barva% > 0 THEN
  '  CIRCLE (centerx, centery), radius, barva%, cleft, cright, 1  'nakresli cue'cue'
  '  END IF
	
	IF cc.startPointno > 0 THEN
	  startPoint = cc.startPointarr(cc.startPoint)
	ELSE
	  startPoint = cc.startPoint
	END IF
	IF startPoint = 1 THEN
	 CALL PPstartDigit(centerx, centery, radius, 4, c)
	 'sleft = c - .03
	 'IF sleft < 0 THEN sleft = sleft + pi2
	 'sright = c + .03
	 'IF sright >= pi2 THEN sright = sright - pi2
	 'CIRCLE (centerx, centery), radius * 1.03, 15, sleft, sright, 1'nakresli start point'
	END IF
	'PRINT cleft, cright
  '     0 - Black
  '   1 - Blue
  '   2 - Green
  '   3 - Cyan
  '   4 - Red
  '   5 - Magenta
  '   6 - Yellow
  '   7 - White
END SUB

SUB PPcueDigit (centerx, centery, radius, barva%, cleft, cright, segments)
' nakresli cue jake jsou opravdu v arene - ze segmentu digitalniho displaye - kamil 19.2.2009'
       size = 1.05
       
  'LOCATE 27, 35
  IF ABS(cleft - cright) > 4 THEN cleft = cleft - 2 * 3.1415

  'PRINT "CUE:"; cright; cleft;
	IF segments = 2 THEN  'tree vertical lines'
		CALL PPangledist2xy(cleft, radius, x0, y0)
		CALL PPangledist2xy(cleft, radius * size, x1, y1)
		CALL linedouble(x0 + centerx, centery - y0, x1 + centerx, centery - y1, barva%)
		
		CALL PPangledist2xy(cright, radius, x0, y0)
		CALL PPangledist2xy(cright, radius * size, x1, y1)
		CALL linedouble(x0 + centerx, centery - y0, x1 + centerx, centery - y1, barva%)
		
		c = (cleft + cright) / 2
		CALL PPangledist2xy(c, radius, x0, y0)
		CALL PPangledist2xy(c, radius * size, x1, y1)
		CALL linedouble(x0 + centerx, centery - y0, x1 + centerx, centery - y1, barva%)
	ELSEIF segments = 1 THEN
	  c = (cleft + cright) / 2
	  c0 = c - .02
	  c1 = c + .02
		CALL PPangledist2xy(cleft, radius, x0, y0)
		CALL PPangledist2xy(c0, radius, x1, y1)
		CALL linedouble(x0 + centerx, centery - y0, x1 + centerx, centery - y1, barva%)
		
		CALL PPangledist2xy(cleft, radius * size, x0, y0)
		CALL PPangledist2xy(c0, radius * size, x1, y1)
		CALL linedouble(x0 + centerx, centery - y0, x1 + centerx, centery - y1, barva%)
		
		CALL PPangledist2xy(cright, radius, x0, y0)
		CALL PPangledist2xy(c1, radius, x1, y1)
		CALL linedouble(x0 + centerx, centery - y0, x1 + centerx, centery - y1, barva%)
		
		CALL PPangledist2xy(cright, radius * size, x0, y0)
		CALL PPangledist2xy(c1, radius * size, x1, y1)
		CALL linedouble(x0 + centerx, centery - y0, x1 + centerx, centery - y1, barva%)
	ELSEIF segments = 3 THEN
		c = (cleft + cright) / 2
	  c0 = c - .02
	  c1 = c + .02
		CALL PPangledist2xy(cleft, radius, x0, y0)
		CALL PPangledist2xy(c0, radius, x1, y1)
		CALL linedouble(x0 + centerx, centery - y0, x1 + centerx, centery - y1, barva%)
		
		CALL PPangledist2xy(cleft, radius * size, x0, y0)
		CALL PPangledist2xy(c0, radius * size, x1, y1)
		CALL linedouble(x0 + centerx, centery - y0, x1 + centerx, centery - y1, barva%)
		
		CALL PPangledist2xy(cright, radius, x0, y0)
		CALL PPangledist2xy(c1, radius, x1, y1)
		CALL linedouble(x0 + centerx, centery - y0, x1 + centerx, centery - y1, barva%)
		
		CALL PPangledist2xy(cright, radius * size, x0, y0)
		CALL PPangledist2xy(c1, radius * size, x1, y1)
		CALL linedouble(x0 + centerx, centery - y0, x1 + centerx, centery - y1, barva%)
		
		CALL PPangledist2xy(cleft, radius, x0, y0)
		CALL PPangledist2xy(cleft, radius * size, x1, y1)
		CALL linedouble(x0 + centerx, centery - y0, x1 + centerx, centery - y1, barva%)
		
		CALL PPangledist2xy(cright, radius, x0, y0)
		CALL PPangledist2xy(cright, radius * size, x1, y1)
		CALL linedouble(x0 + centerx, centery - y0, x1 + centerx, centery - y1, barva%)
	END IF
END SUB

SUB PPcues (start, x, y, r)
' start from 1 to 8
	startx = r + r * .1
	starty = 0
	CALL PProtate(startx, starty, 360 - (start - 1) * 45)
	CIRCLE (startx + x, y + starty), 5, 12

END SUB

FUNCTION PPelipseRadius (a, b, rad)
' returns radius of elipse (a,b) in a given angle 'rad' '
	x = a * COS(rad)
	y = b * SIN(rad)
	PPelipseRadius = PPxy2dist(x, y)
END FUNCTION

SUB PPinavoid (x, y, c, w, d0, d1, inavoid, radius)
' if the person is inavoidance area defined by center(c), width(w),
' and % of arena diameter( d0,d1)


a = PPxy2angle(x, y)
a0 = rad(c) - rad(w) / 2
a1 = rad(c) + rad(w) / 2
d = PPxy2dist(x, y) / radius * 100
IF d1 = 100 THEN d1x = 120 ELSE d1x = d1
'PRINT x, y, a0, a, a1
IF (a > a0 AND a < a1 AND d > d0 AND d < d1x) THEN
	inavoid = 1
ELSE
	inavoid = 0
END IF

END SUB

SUB PPinavoidcircle (x, y, ax, ay, ar, inavoid, prr)
' if the person is in a avoid circle defined by [ax,ay] and radius r
' ax, ay are from 0 to 1
axx = ax * prr
ayy = ay * prr
arr = ar * prr
'LOCATE 2, 2
'PRINT x, y, axx, ayy
IF (distance(x, y, axx, ayy) <= arr) THEN
	inavoid = 1
ELSE
	inavoid = 0
END IF

END SUB

SUB PPinavoidcircle2 (x, y, dist, angle, r, inavoid, prr)
' napr. CALL PPinavoidcircle2(rfx, rfy, ss.r0, sectorcenter(ss), ss.r, ss.avoidstate, prr)'
' the circle is defined by dist, angle and r
' x,y are from 0 to prr
	CALL PPangledist2xy(rad(angle), dist / 100, xc, yc)
	' xc, yc are from 0 to 1
	' yc = 1 - yc
	'PRINT x, y, xc, yc, r, prr
	CALL PPinavoidcircle(x, -y, xc, yc, r / 100, inavoid, prr)
END SUB

SUB PProtate (x, y, angledeg)
	angle = rad(angledeg)
	xnew = x * COS(angle) - y * SIN(angle)
	ynew = x * SIN(angle) + y * COS(angle)
	x = xnew
	y = ynew
END SUB

FUNCTION PPsectordist (pozice AS position, prr, ss AS sector)
  IF ss.mode = 0 THEN 'RF'
    x = pozice.rfx
    y = pozice.rfy
  ELSE
    x = pozice.afx
    y = pozice.afy
  END IF
  IF ss.shape = 1 THEN 'circle'
    CALL PPangledist2xy(rad(sectorcenter(ss)), sectordistance(ss) / 100, xc, yc) 'prevede mi centrum sektoru na xc yc'
  ELSE 'circle sector definovan pomoci c, w, r0, r1,'
    CALL PPangledist2xy(rad(sectorcenter(ss)), ((ss.r1 - sectordistance(ss)) / 2 + sectordistance(ss)) / 100, xc, yc)
  END IF
    ' xc, yc are from 0 to 1
    xc = xc * prr
    yc = -yc * prr 'yc ma 1 nahore a -1 dole, souradnice ktere data karta - x,y - jsou obracene'
    PPsectordist = CINT(distance(x, y, xc, yc))
    'locate 25,2
    'print "x,y,xc,yc";x;y;xc;yc;"                                "
END FUNCTION

SUB PPset (x, y, xc, yc, avoid)
	STATIC xlast, ylast
	
	IF avoid = 1 THEN barva = 14 ELSE barva = 15
	PSET (x + xc, y + yc), barva

	'IF (distance(x, y, xlast, ylast)) < 30 THEN
	'        LINE (xlast + xc, ylast + yc)-(x + xc, y + yc), barva
	'ELSE
	'        PSET (x + xc, y + yc), barva
	'END IF
	xlast = x
	ylast = y
END SUB

SUB PPstart (start, x, y, r)
' start from 1 to 8
	startx = r + r * .1
	starty = 0
	CALL PProtate(startx, starty, 360 - (start - 1) * 45)
	CIRCLE (startx + x, y + starty), 5, 13

END SUB

SUB PPstartDigit (centerx, centery, radius, barva%, c)
    size = 1.02
    CALL PPangledist2xy(c, radius * size, x, y)
    CIRCLE (centerx + x, centery - y), 4, barva%
    PAINT (centerx + x, centery - y), barva%
END SUB

SUB PPstartnahled (Pc1x, Pc1y, prr, angle)
  r0 = 95
  r = 2
	angle = angle MOD 360
  CALL PPangledist2xy(rad(angle), r0 / 100, x, y)
  CIRCLE (Pc1x + prr * x, Pc1y - prr * y), (r * prr / 100), 14 'zluta'
END SUB

'vraci v radianech'
FUNCTION PPxy2angle (x, y)
pi = 3.141592
IF (x = 0) THEN
	IF (y = 0) THEN
		a = 0
	ELSEIF (y > 0) THEN
		a = pi + pi / 2
	ELSE
		a = pi / 2
	END IF
ELSEIF (y = 0) THEN
       IF (x >= 0) THEN
	a = 0
       ELSE
	a = pi
       END IF
ELSE
      a = ATN(y / x)
      'PRINT a,
      IF (y < 0 AND x < 0) THEN
	a = pi - a
      ELSEIF (x < 0) THEN
	a = pi - a
      ELSEIF (y > 0) THEN
	a = 2 * pi - a
      ELSE
	a = -a
      END IF
      'PRINT a
END IF
PPxy2angle = a
END FUNCTION

FUNCTION PPxy2dist (x, y)
	PPxy2dist = SQR(x ^ 2 + y ^ 2)

END FUNCTION

SUB PPxy2screen (x, y, xc, yc, xr, yr, r)
' (x,y, xcenter, ycenter, xradius, yradius, output radius'
' returns xy where [0;0] is in the middle and radius is r
' CALL PPconsiderCameraAngle(x, y, xr, yr, xc, yc)
' zohledneni nelinearity vzdalenosti dane kamerou pridano 26.4.2007
' zruseno 11.5.2007 - presunuto do consider heigth
x = ((x - xc) / xr) * r               ' xc center in x, xr - radius in x
y = ((y - yc) / yr) * r
END SUB

SUB prevcenter (p AS phase)
	FOR j = 0 TO p.sectorsno - 1
	  IF p.sectors(j).cno > 0 THEN
	      p.sectors(j).c = p.sectors(j).c - 1
	      IF p.sectors(j).c < 0 THEN p.sectors(j).c = p.sectors(j).cno
	    END IF
	   IF p.sectors(j).r0no > 0 THEN
			p.sectors(j).r0 = p.sectors(j).r0 - 1
			IF p.sectors(j).r0 < 0 THEN p.sectors(j).r0 = p.sectors(j).r0no
	   END IF
	NEXT j
END SUB

FUNCTION rad! (degg!)
	pi! = 3.141592
	rad! = pi! * degg! / 180
END FUNCTION

'vrati aktualni centrum sektoru - 29.11.2007'
FUNCTION sectorcenter (s AS sector)
  'LOCATE 28, 2
  'PRINT s.cstart;
  IF s.cno > 0 THEN
    sc = s.carr(s.c)
  ELSE
    sc = s.c
  END IF
  IF s.crelativeto > 0 THEN
    sc = sc + s.cstart
  END IF
  WHILE sc >= 360
    sc = sc - 360
  WEND
  sectorcenter = sc
END FUNCTION

' vrati aktualni vzdalenost sektoru od stredu - 8.4.2008'
FUNCTION sectordistance (s AS sector)
	IF s.r0no > 0 THEN
		sr = s.r0arr(s.r0)
	ELSE
		sr = s.r0
	END IF
	sectordistance = sr
END FUNCTION

'nastavi prvni centrum sektoru v prvni faze - kvuli centrum relativnim ke startu'
SUB setcenter (pp() AS phase, currentpos AS position, currentphase, phasesno)
  FOR p = 0 TO phasesno - 1
    FOR j = 0 TO pp(p).sectorsno - 1
	    IF pp(p).sectors(j).crelativeto = currentphase + 1 THEN 'pokud je to ta faze, ke ktere se ma pocitat '
	      IF pp(p).sectors(j).mode = 0 THEN 'RF'
		      pp(p).sectors(j).cstart = deg!(PPxy2angle(currentpos.rfx, currentpos.rfy))
	      ELSE
		      pp(p).sectors(j).cstart = deg!(PPxy2angle(currentpos.afx, currentpos.afy))
	      END IF
	      'LOCATE 27, 25
	      'PRINT pp(p).sectors(j).cstart; currentpos.rfx; currentpos.rfy
	    END IF
	NEXT j
  NEXT p
END SUB

