DECLARE SUB debugsetcue (cc AS ANY)
DECLARE FUNCTION sectorbeepcount! (s AS ANY)
DECLARE FUNCTION getlevel ()
DECLARE SUB cuesupdate (cuesnew() AS ANY)
DECLARE FUNCTION max! (a0!, a1!)
DECLARE SUB main ()
DECLARE SUB arenapos (deg!, turn!)
DECLARE SUB cekat (n!)
DECLARE SUB getxy (xa!, ya!)
DECLARE SUB setupkaminsky ()
DECLARE SUB trackreset ()
DECLARE SUB nahledcx (x!, y!, prr!, angle!, avoid!, curf!, pp() AS ANY)
DECLARE SUB PPinavoid (x!, y!, c!, w!, d0!, d1!, inavoid!, radius!)
DECLARE SUB PPinavoidcircle2 (x!, y!, dist!, angle!, r!, inavoid!, prr!)
DECLARE SUB PProtate (x!, y!, angledeg!)
DECLARE SUB PPset (x!, y!, xc!, yc!, avoid!)
DECLARE SUB checklatency (s AS ANY, frame!, Sl0!, Sl1!, pauseframe!)
DECLARE SUB closeoutput ()
DECLARE SUB closetrack ()
DECLARE SUB closing (frame!, anglesum!, casstojici!, ppno!, pp() AS ANY, cislo!)
DECLARE SUB savepos (pozice AS ANY, rfx!, rfy!, afx!, afy!)
DECLARE SUB savetrack (frame!, rfx!, rfy!, afx!, afy!, angle!, curf!, ppno!, pp() AS ANY, pauseframe!, klavesa$)
DECLARE SUB tisky (angle!, frame!, casstojici!, phase!, ppno!, pp() AS ANY, framestart!)
DECLARE SUB PParena (x!, y!, r!)
DECLARE SUB PPavoidcircle2more (Pc1x!, Pc1y!, prr!, s AS ANY)
DECLARE SUB PPavoidmore (Pc1x!, Pc1y!, prr!, s AS ANY)
DECLARE SUB PPangledist2xy (angle!, dist!, x!, y!)
DECLARE FUNCTION getdist! (pixels!)
DECLARE SUB tiskpos (pozice AS ANY, radek!)
DECLARE SUB cuesreset ()
DECLARE SUB cueswrite (position%, symbol%, start%)
DECLARE FUNCTION gettime$ (frames!)
DECLARE FUNCTION distance! (x0!, y0!, x1!, y1!)
DECLARE FUNCTION rad! (degg!)
DECLARE SUB sndstart ()
DECLARE SUB sndstop ()
DECLARE FUNCTION sectorcenter (ss AS ANY)
DECLARE FUNCTION sectordistance (ss AS ANY)
DECLARE FUNCTION empty (a$)
DECLARE FUNCTION PPsectordist (pozice AS ANY, prr, ss AS ANY)
DECLARE SUB resetcounts (curf!, pp() AS ANY)


' typy zavisle na projektu'
' $INCLUDE: 'typedef.bas'

SUB checklatency (s AS sector, frame, Sl0, Sl1, pauseframe)
	IF pauseframe = 0 THEN
	IF s.lastA <> s.avoidstate THEN ' zmena stavu - veskery vypocet
		IF s.avoidstate = 1 THEN 'entrance to avoidance sector'
			latencytime = Sl0
		ELSE ' leaving the avoidance sector'
			latencytime = Sl1
		END IF
		IF s.frame0 = 0 THEN 'first frame after entrance = last time was not in avoidA'
			s.frame0 = frame
			s.avoidstate = s.lastA
		ELSEIF frame - s.frame0 < latencytime THEN
			s.avoidstate = s.lastA
		END IF ' jinak avoidance state nemenim'
		s.lastA = s.avoidstate
	ELSE
		s.frame0 = 0
	END IF
	ELSE
	   s.lastA = 0 'last avoidance pro checklatency
	END IF
END SUB

SUB closeoutput
CLOSE #2
END SUB

SUB closetrack
CLOSE #1
END SUB

SUB closing (frame, anglesum, casstojici, ppno, pp() AS phase, cislo)

'PRINT #2, "cislo:"; cislo
PRINT #2, "total time:"; gettime(frame)
IF frame > 0 THEN
PRINT #2, "celkova rychlost areny: "; (anglesum * 25) / frame; " deg/sec"
PRINT #2, "arena stala celkove: "; casstojici; " s, tj posun o "; (anglesum * 25) / frame * casstojici; " deg"
END IF
	FOR i = 0 TO ppno - 1
	 FOR j = 0 TO pp(i).sectorsno - 1
	   PRINT #2, "phase "; i; ", sector "; j; "- parameters(mode,shape,avoid,sound,kopiez,kopiedo,c,w,r0,r1,r):";
	   PRINT #2, pp(i).sectors(j).mode; " ";
	   PRINT #2, pp(i).sectors(j).shape; " ";
	   PRINT #2, pp(i).sectors(j).avoid; " ";
	   PRINT #2, pp(i).sectors(j).sound; " ";
	   PRINT #2, pp(i).sectors(j).kopiez; " ";
	   PRINT #2, pp(i).sectors(j).kopiedo; " ";
	   PRINT #2, sectorcenter(pp(i).sectors(j)); " ";
	   PRINT #2, pp(i).sectors(j).w; " ";
	   PRINT #2, sectordistance(pp(i).sectors(j)); " ";
	   PRINT #2, pp(i).sectors(j).r1; " ";
	   PRINT #2, pp(i).sectors(j).r
	   IF pp(i).sectors(j).mode > -1 THEN
	     PRINT #2, "  entrances: "; pp(i).sectors(j).entrances; " time: "; gettime$(pp(i).sectors(j).frames)
	  END IF
	 NEXT j
	 NEXT i

'PRINT #2, "entrances room: "; entrancesR; ", arena: "; entrancesA
'PRINT #2, "time in avoidance area in room: "; gettime$(frameavoidR); ", arena"; gettime$(frameavoidA)
'PRINT #2, "room sector parameters(x,w,0,1)"; Rrc, Rrw, Rr0, Rr1
'PRINT #2, "arena sector parameters(x,w,0,1)"; Arc, Arw, Ar0, Ar1
PRINT #2, "celkove otoceni areny v pokuse: "; anglesum; " deg"
END SUB

FUNCTION empty (b$)
	a$ = b$ 'abych to prenesl hodnotou'
	'PRINT "xxx"; a$; "xxx";
	'IF LTRIM$(RTRIM$(a$)) = " " OR LTRIM$(RTRIM$(a$)) = "" THEN PRINT "empty" ELSE PRINT "not empty"
	IF LTRIM$(RTRIM$(a$)) = " " OR LTRIM$(RTRIM$(a$)) = "" THEN empty = -1 ELSE empty = 0
	' TRUE je pry -1 !!!!!!!!! a ne 1 jako obvykle - pak funguje not true = false
	
END FUNCTION

FUNCTION getdist (pixels)
  a = pixels / 100
  a = INT(a * 100) / 100
  getdist = a
END FUNCTION

FUNCTION gettime$ (frames)
	secs = frames / 25
	minutes = secs \ 60
	secs = secs - minutes * 60
	secs = INT(secs)
	
	retez$ = STR$(minutes) + ":" + LTRIM$(STR$(secs))
	WHILE LEN(retez$) < 6
	 retez$ = retez$ + " "
	WEND
	gettime$ = retez$
END FUNCTION

' kamil 24.7.2008'
SUB importmisto (mm AS sector, ss AS sector, cislo, znak AS STRING)
  IF LEN(RTRIM$(LTRIM$(znak))) > 0 THEN
    cname$ = mm.cname
    MID$(cname$, 3, 1) = znak
  ELSE
    cname$ = mm.cname
  END IF
  IF ss.cno > 0 THEN
    ss.carr(cislo) = mm.c
    ss.cnamearr(cislo) = cname$
  ELSE
    ss.c = mm.c
  END IF
    
  IF ss.r0no > 0 THEN
    ss.r0arr(cislo) = mm.r0
  ELSE
    ss.r0 = mm.r0
  END IF
  
  IF ss.beepcountno > 0 THEN
    ss.beepcountarr(cislo) = mm.beepcount
  ELSE
    ss.beepcount = mm.beepcount
  END IF
END SUB

SUB init (jmeno$, vyska%)
  LINE INPUT "  zadej jmeno cloveka (max 8 znaku): "; jmeno$
  IF LEN(jmeno$) = 0 THEN END
  LINE INPUT "  vyska (cm): "; vyska$
  vyska% = VAL(vyska$)
  LINE INPUT "  datum narozeni (dd.mm.rrrr): "; narozeni$
  
  LINE INPUT "  roky vzdelani: "; vzdelani$
  LINE INPUT "  pohlavi (M/Z): "; pohlavi$
  'LINE INPUT "  poznamka: "; poznamka$
  
  jmenoresults$ = jmeno$ + ".dat"
  IF LEN(DIR$("pacienti\" + jmenoresults$)) <> 0 THEN
	PRINT "  this filename already exists: "; jmenoresults$
	PRINT "   - the new data will be appended"
	INPUT "  do you want to continue?(Y/N)"; reakce$
	IF reakce$ <> "Y" AND reakce$ <> "y" THEN END
  END IF
  OPEN jmenoresults$ FOR APPEND AS #2
  PRINT #2,
  PRINT #2, "**************************************************************"
  PRINT #2, "dissociated reference frames task2 - version from "; VERSION$
  PRINT #2, DATE$, TIME$, jmeno$, jmenoresults$
  PRINT #2, "narozeni: "; narozeni$, "vzdelani: "; vzdelani$, "pohlavi: "; pohlavi$, "vyska: "; vyska%
  PRINT #2, "poznamka: "; poznamka$
END SUB

SUB initdat (configno, expname$, filename() AS STRING)
  PRINT #2,
  PRINT #2, "-------------------------------"
  PRINT #2, configno; "- "; expname$; ", filename: "; filename(configno)
  PRINT #2, DATE$, TIME$
  PRINT #2, "phase", "sector", "cislo", "start(m:s)", "length(m:s)", "dist to(cm)", "min dist(cm)", "distFromSector(cm)"
END SUB

SUB inittrack (jmeno$, ppno!, pp() AS phase, configno, expname$, prr, filename() AS STRING)
	
  jmenotrack$ = jmeno$ + ".tr" + LTRIM$(STR$(configno))
	OPEN jmenotrack$ FOR APPEND AS #1
	PRINT #1, "**********************************************"
	PRINT #1, configno; "- "; expname$; ", filename: "; filename(configno)
	PRINT #1, jmeno$, DATE$, TIME$

	' jeste vytisknu pozice cilu a cues
	DIM cues(4) AS CUE
	PRINT #1, "phase", "cue", "cueno", "laser", "startpoint", "segments"
	FOR i = 0 TO ppno - 1
	    FOR c = 0 TO pp(i).cuesno - 1
		 IF pp(i).cues(c).kopiez > -1 THEN
		  cues(c) = pp(pp(i).cues(c).kopiez).cues(c)
		 ELSE
			cues(c) = pp(i).cues(c)
		 END IF
		 'DIM cc AS CUE
		 'cc = cues(c)
		 IF cues(c).startPointno > 0 OR cues(c).segmentsno > 0 OR cues(c).laserno > 0 THEN
		    FOR j = 0 TO max(max(CDBL(cues(c).startPointno), CDBL(cues(c).segmentsno)), CDBL(cues(c).laserno)) - 1
		IF cues(c).laserno >= j + 1 THEN laser = cues(c).laserarr(j) ELSE laser = cues(c).laser
		IF cues(c).startPointno >= j + 1 THEN startPoint = cues(c).startPointarr(j) ELSE startPoint = cues(c).startPoint
		IF cues(c).segmentsno >= j + 1 THEN segments = cues(c).segmentsarr(j) ELSE segments = cues(c).segments
		      PRINT #1, i, c, j, laser, startPoint, segments
		    NEXT j
		 ELSE
	   PRINT #1, i, c, 0, cues(c).laser, cues(c).startPoint, cues(c).segments
	END IF
	    NEXT c
	NEXT i
	PRINT #1, "phase", "sector", "mode", "avoid", "shape", "r", "r0", "r1", "keytonext", "goalno", "c", "rel_to", "keytocues", "repeatmax", "secondmax", "keyfound", "keyfoundbeepstop", "name"
	FOR i = 0 TO ppno - 1
	  FOR j = 0 TO pp(i).sectorsno - 1
		   DIM ss AS sector
		   ss = pp(i).sectors(j)
	IF ss.cno > 0 OR ss.r0no > 0 THEN
	  FOR k = 0 TO max(CDBL(ss.cno), CDBL(ss.r0no)) - 1 '-1 oprava 14.4.2008'
		IF ss.cno >= k + 1 THEN
				ssc = ss.carr(k)
				ssname$ = ss.cnamearr(k)
		ELSE
				ssc = ss.c
				ssname$ = ss.cname
		END IF
		IF ss.r0no >= k + 1 THEN ssr0 = ss.r0arr(k) ELSE ssr0 = ss.r0
	    PRINT #1, i, j, ss.mode, ss.avoid, ss.shape, ss.r, ssr0, ss.r1, ss.keytonext, k, ssc, ss.crelativeto, pp(i).keytocues, pp(i).repeatmax, pp(i).secondmax, ss.keyfound, ss.keyfoundbeepstop, ssname$
	  NEXT k
	ELSE
	  PRINT #1, i, j, ss.mode, ss.avoid, ss.shape, ss.r, ss.r0, ss.r1, ss.keytonext, 0, ss.c, ss.crelativeto, pp(i).keytocues, pp(i).repeatmax, pp(i).secondmax, ss.keyfound, ss.keyfoundbeepstop, ss.cname
	END IF
	  NEXT j
	NEXT i

	PRINT #1, "frame", "roomx", "roomy", "arena angle", "arenax", "arenay", "phase", "pausa", "frame", "sector",
	FOR i = 0 TO ppno - 1
		FOR j = 0 TO pp(i).sectorsno - 1
			PRINT #1, "sector"; i; "-"; j,
		NEXT j
	NEXT i
	PRINT #1, "klavesa", "faze repeat", "goalno"
	'"shock0-0", "shock1-0"
END SUB

FUNCTION max (a0, a1)
IF a0 < a1 THEN
	max = a1
ELSE
	max = a0
END IF
END FUNCTION

SUB nakresliareny (screenarenas AS position, prr, phases() AS phase, phasesno, currentphase)
  CLS
  if phases(0).noroomframe = 0 then
		CALL PParena(screenarenas.rfx, screenarenas.rfy, prr)
	end if
  IF phases(0).noarenaframe = 0 THEN
    CALL PParena(screenarenas.afx, screenarenas.afy, prr)
  END IF
  FOR p = 0 TO phasesno - 1
    FOR s = 0 TO phases(p).sectorsno - 1
      IF phases(p).sectors(s).mode > -1 AND (phases(p).sectors(s).crelativeto = 0 OR p = currentphase) THEN
		    IF phases(p).sectors(s).shape = 0 THEN 'shape sektor'
						IF phases(p).sectors(s).mode = 0 and phases(p).noroomframe = 0 THEN   'rf
						  CALL PPavoidmore(screenarenas.rfx, screenarenas.rfy, prr, phases(p).sectors(s))
						ELSEIF phases(p).sectors(s).mode = 1 and  phases(p).noarenaframe = 0 THEN
						  CALL PPavoidmore(screenarenas.afx, screenarenas.afy, prr, phases(p).sectors(s))
						END IF
		     ELSE 'shape circle'
			       IF phases(p).sectors(s).mode = 0 and phases(p).noroomframe = 0 THEN 'rf
					     CALL PPavoidcircle2more(screenarenas.rfx, screenarenas.rfy, prr, phases(p).sectors(s))
			       ELSEIF phases(p).sectors(s).mode = 1 and  phases(p).noarenaframe = 0 THEN
					     CALL PPavoidcircle2more(screenarenas.afx, screenarenas.afy, prr, phases(p).sectors(s))
			       END IF
		     END IF
       END IF
    NEXT s
  NEXT p

END SUB

' posunuje beepcount - 24.7.2008'
SUB nextbeepcount (p AS phase)
	FOR j = 0 TO p.sectorsno - 1
	    IF p.sectors(j).beepcountno > 0 THEN
	      p.sectors(j).beepcount = p.sectors(j).beepcount + 1
	      IF p.sectors(j).beepcount >= p.sectors(j).beepcountno THEN p.sectors(j).beepcount = 0
	    END IF
	NEXT j
END SUB

'posune cues na dalsi , pokud se pouziva laserno, segmnetsno a startPointno'
SUB nextcues (phases() AS phase, curphase)
 cuesno = phases(curphase).cuesno
  'DIM cccc(0 TO 2) AS cue
  'cccc = phases(curphase).cues
	IF cuesno = 0 THEN
		CALL cuesreset
	ELSEIF cuesno > 0 THEN
	  FOR i = 0 TO cuesno - 1
		 DIM cc AS CUE
		 IF phases(curphase).cues(i).kopiez > -1 THEN
		    cc = phases(phases(curphase).cues(i).kopiez).cues(i)
	   ELSE
	      cc = phases(curphase).cues(i)
	   END IF
		 
		 IF cc.laserno > 0 THEN '29.11.2007 - vice pozic lazeru'
		   IF phases(curphase).cues(i).kopiez = -1 THEN 'kdyz kopiruju cues z jine faze - tak pozici na v laserarr neposouvam!
			   phases(curphase).cues(i).laser = phases(curphase).cues(i).laser + 1 ' tady to muzu rovnou posunout (na rozdil od c) protoze se to vola jen jednou za fazi'
			   IF phases(curphase).cues(i).laser >= phases(curphase).cues(i).laserno THEN phases(curphase).cues(i).laser = 0
		   END IF
		 END IF
      
     IF cc.segmentsno > 0 THEN '17.1.2008 - vice moznych segmentu'
		   IF phases(curphase).cues(i).kopiez = -1 THEN 'kdyz kopiruju cues z jine faze - tak pozici na v laserarr neposouvam!
			   phases(curphase).cues(i).segments = phases(curphase).cues(i).segments + 1 ' tady to muzu rovnou posunout (na rozdil od c) protoze se to vola jen jednou za fazi'
			   IF phases(curphase).cues(i).segments >= phases(curphase).cues(i).segmentsno THEN phases(curphase).cues(i).segments = 0
		   END IF
		END IF

    IF cc.startPointno > 0 THEN '17.1.2008 - vice moznych startu'
       IF phases(curphase).cues(i).kopiez = -1 THEN 'kdyz kopiruju cues z jine faze - tak pozici na v laserarr neposouvam!
			   phases(curphase).cues(i).startPoint = phases(curphase).cues(i).startPoint + 1 ' tady to muzu rovnou posunout (na rozdil od c) protoze se to vola jen jednou za fazi'
			   IF phases(curphase).cues(i).startPoint >= phases(curphase).cues(i).startPointno THEN phases(curphase).cues(i).startPoint = 0
		   END IF
    END IF

	  NEXT i
	END IF
	phases(curphase).keytocuespressed = 0 'nebyla v nasledujici fazi stlacena klavesa pro cues'
	phases(curphase).keytostartpressed = 0 'nebyla v nasledujici fazi stlacena klavesa pro start'
	phases(curphase).keytostartafterfoundpressed = 0 'nebyla v nasledujici fazi stlacena klavesa pro start'
END SUB

SUB prehodcfg (pokus1, pokus2, pp() AS phase, faze)
  FOR i = 0 TO pp(faze).sectorsno - 1
    IF pp(faze).sectors(i).cno - 1 > max(pokus1, pokus2) THEN
      c = pp(faze).sectors(i).carr(pokus1)
      pp(faze).sectors(i).carr(pokus1) = pp(faze).sectors(i).carr(pokus2)
      pp(faze).sectors(i).carr(pokus2) = c
      
      n$ = pp(faze).sectors(i).cnamearr(pokus1)
      pp(faze).sectors(i).cnamearr(pokus1) = pp(faze).sectors(i).cnamearr(pokus2)
      pp(faze).sectors(i).cnamearr(pokus2) = n$
    END IF
  NEXT i

  FOR i = 0 TO pp(faze).cuesno - 1
    IF pp(faze).cues(i).laserno - 1 > max(pokus1, pokus2) THEN
      l = pp(faze).cues(i).laserarr(pokus1)
      pp(faze).cues(i).laserarr(pokus1) = pp(faze).cues(i).laserarr(pokus2)
      pp(faze).cues(i).laserarr(pokus2) = l
    END IF
    IF pp(faze).cues(i).segmentsno - 1 > max(pokus1, pokus2) THEN
      l = pp(faze).cues(i).segmentsarr(pokus1)
      pp(faze).cues(i).segmentsarr(pokus1) = pp(faze).cues(i).segmentsarr(pokus2)
      pp(faze).cues(i).segmentsarr(pokus2) = l
    END IF
    IF pp(faze).cues(i).startPointno - 1 > max(pokus1, pokus2) THEN
      l = pp(faze).cues(i).startPointarr(pokus1)
      pp(faze).cues(i).startPointarr(pokus1) = pp(faze).cues(i).startPointarr(pokus2)
      pp(faze).cues(i).startPointarr(pokus2) = l
    END IF
  NEXT i
END SUB

SUB prehodcfgto (pp() AS phase, faze, places() AS INTEGER, pocet)
	FOR s = 0 TO pp(faze).sectorsno - 1
		IF pp(faze).sectors(s).cno = pocet THEN
			DIM ss AS sector
			FOR i = 0 TO pocet
				ss.carr(i) = pp(faze).sectors(s).carr(places(i))
				ss.cnamearr(i) = pp(faze).sectors(s).cnamearr(places(i))
			NEXT i
			FOR i = 0 TO pocet
				pp(faze).sectors(s).carr(i) = ss.carr(i)
				pp(faze).sectors(s).cnamearr(i) = ss.cnamearr(i)
			NEXT i
		END IF
	NEXT s
	
	FOR c = 0 TO pp(faze).cuesno - 1
	  DIM cc AS CUE
		IF pp(faze).cues(c).laserno = pocet THEN
			 FOR i = 0 TO pocet
				cc.laserarr(i) = pp(faze).cues(c).laserarr(places(i))
			 NEXT i
			 FOR i = 0 TO pocet
				pp(faze).cues(c).laserarr(i) = cc.laserarr(i)
			 NEXT i
		END IF
		IF pp(faze).cues(c).segmentsno = pocet THEN
			 FOR i = 0 TO pocet
				cc.segmentsarr(i) = pp(faze).cues(c).segmentsarr(places(i))
			 NEXT i
			 FOR i = 0 TO pocet
				pp(faze).cues(c).segmentsarr(i) = cc.segmentsarr(i)
			 NEXT i
		END IF
		IF pp(faze).cues(c).startPointno = pocet THEN
			 FOR i = 0 TO pocet
				cc.startPointarr(i) = pp(faze).cues(c).startPointarr(places(i))
			 NEXT i
			 FOR i = 0 TO pocet
				pp(faze).cues(c).startPointarr(i) = cc.startPointarr(i)
			 NEXT i
		END IF
	NEXT c
		
END SUB

' kamil 30.7.2008 - pri vraceni posledni pokusu zpet '
SUB resetcounts (curf, pp() AS phase)
      pp(curf).repeat = pp(curf).repeat - 1
      FOR j = 0 TO pp(curf).sectorsno - 1
       
       IF pp(curf).sectors(j).mode = -1 THEN
       faze = pp(curf).sectors(j).kopiez
      ELSE
       faze = curf
      END IF
      DIM ss AS sector
      ss = pp(faze).sectors(j) ' zkratka, pouzitelna jen pro vybirani hodnot, ne pro ukladani'
      pp(faze).sectors(j).entrances = ss.entrances - 1
''      IF ss.avoid = 0 THEN 'preference sector
''      else
''      end if
	 NEXT j
END SUB

SUB resetkeyfoundpressed (curf, pp() AS phase)
   FOR j = 0 TO pp(curf).sectorsno - 1
    pp(curf).sectors(j).keyfoundpressed = 0
   NEXT j
END SUB

SUB savepos (pozice AS position, rfx, rfy, afx, afy)
   pozice.rfx = rfx
   pozice.rfy = rfy
   pozice.afx = afx
   pozice.afy = afy
END SUB

SUB saveshock (frame, curf, pp() AS phase, pozice AS position, prr)
  FOR i = 0 TO pp(curf).sectorsno - 1
    IF pp(curf).sectors(i).mode = -1 THEN
     faze = pp(curf).sectors(i).kopiez
    ELSE
      faze = curf
    END IF
    ppname$ = STR$(faze) + "-" + STR$(pp(faze).repeat)
    DIM ss AS sector
    ss = pp(faze).sectors(i) ' zkratka, pouzitelna jen pro vybirani hodnot, ne pro ukladani'
    IF ss.mode = 0 THEN ssname$ = STR$(i) + "R" ELSE ssname$ = STR$(i) + "A"
    IF ss.cno > 0 THEN
      ssname$ = ssname$ + STR$(ss.c)
      IF LEN(LTRIM$(ss.cnamearr(ss.c))) > 0 THEN
	       ssname$ = ssname$ + ss.cnamearr(ss.c)
	    END IF
	  END IF
    IF ss.saveshock = 1 THEN
      'LOCATE 27, 35
      'PRINT "saveshock: "; frame

      IF ss.avoid = 0 THEN 'preference'
			PRINT #2, ppname$, ssname$, ss.entrances, gettime$(ss.shockstart), gettime$(ss.framesto), CINT(ss.distanceto), ss.distancemin, PPsectordist(pozice, prr, ss)
      ELSE 'avoidance'
			PRINT #2, ppname$, ssname$, ss.entrances, gettime$(ss.shockstart), gettime$(frame - ss.shockstart), CINT(ss.distanceto), ss.distancemin, PPsectordist(pozice, prr, ss)
      END IF
      pp(faze).sectors(i).saveshock = 0
      'pp(faze).sectors(i).shockstart = -1
    END IF
  NEXT i
END SUB

SUB savetrack (frame, rfx, rfy, afx, afy, angle, curf, ppno, pp() AS phase, pauseframe, klavesa$)
  PRINT #1, frame, CINT(rfx), CINT(rfy), angle, CINT(afx), CINT(afy), curf, pauseframe,
  'ZAPIS JAKY JE AKTUALNI PREFERENCE SEKTOR - RF/AF, cislo sektoru'
  frameulozen = 0
  FOR j = 0 TO pp(curf).sectorsno - 1
	 IF pp(curf).sectors(j).mode = -1 THEN
	   faze = pp(curf).sectors(j).kopiez
	  ELSE
	   faze = curf
	   END IF
	   IF pp(faze).sectors(j).avoid = 0 THEN 'preference'
	     PRINT #1, pp(faze).sectors(j).mode, j,
	     frameulozen = 1
	    END IF
	NEXT j
   'pokud nebyl nalezen zadny aktualni preference sektor'
	IF frameulozen = 0 THEN PRINT #1, "99", "99",
	
	'ZAPIS STAVU VSECH SEKTORU'
	FOR i = 0 TO ppno - 1
		FOR j = 0 TO pp(i).sectorsno - 1
			IF pp(i).sectors(j).mode = -1 THEN
	   faze = pp(i).sectors(j).kopiez
	  ELSE
	   faze = i
	  END IF
	  DIM ss AS sector
	  ss = pp(faze).sectors(j)
	  IF curf = faze THEN
				IF ss.frame0 > 0 THEN 'latency time
				  ' sektor - latency time'
					PRINT #1, -1, 'ss.avoidstate; ss.avoidstateLast; ss.lastA; ss.frame0,
				ELSE
				  ' sektor - v soku'
					PRINT #1, ss.avoidstate, 'ss.avoidstateLast; ss.lastA; ss.frame0,
				END IF
				IF ss.avoid = 0 AND ss.cno > 0 THEN '21.2.2008'
				  goalno = ss.c
				ELSE
				  goalno = 0
				END IF
			ELSE
			  ' jina aktualni faze'
				PRINT #1, "99", '"avoidlast",
			END IF
		NEXT j
	NEXT i
	' jeste stisknuta klavesa a ukoncit radek
	PRINT #1, klavesa$, pp(curf).repeat, goalno, " "
	
END SUB

'vrati aktualni beepcount sektoru - 24.7.2008'
FUNCTION sectorbeepcount (s AS sector)
  'LOCATE 28, 2
  'PRINT s.cstart;
  IF s.beepcountno > 0 THEN
    sc = s.beepcountarr(s.beepcount)
  ELSE
    sc = s.beepcount
  END IF
  sectorbeepcount = sc
END FUNCTION

' zobrazi cues v arene v soucasne fazi - dohromady jich sviti cuesno'
SUB setcues (phases() AS phase, curphase)
  cuesno = phases(curphase).cuesno
	IF cuesno = 0 THEN
		CALL cuesreset
	ELSEIF cuesno > 0 THEN
	   DIM cuesnew(1 TO 8)  AS CUE 'hodnoty kam budu ukladat nove nastaveni cues'
	   FOR i% = 1 TO 8
	     cuesnew(i%).laser = i%
	     cuesnew(i%).segments = 0
	     cuesnew(i%).startPoint = 0
	   NEXT i%
	   FOR i = 0 TO cuesno - 1
		 DIM cc AS CUE
		 IF phases(curphase).cues(i).kopiez > -1 THEN
		    cc = phases(phases(curphase).cues(i).kopiez).cues(i)
	   ELSE
	      cc = phases(curphase).cues(i)
	   END IF
		 IF cc.arenaframe = 0 THEN 'pokud to neni arenaframe cue'
		IF cc.laserno > 0 THEN '29.11.2007 - vice pozic lazeru'
		   laser% = cc.laserarr(cc.laser)
	  ELSE
			 laser% = cc.laser
		END IF
	  
	IF cc.segmentsno > 0 THEN '17.1.2008 - vice moznych segmentu'
		   segment% = cc.segmentsarr(cc.segments)
		ELSE
		   segments% = cc.segments
		END IF
		IF NOT empty(phases(curphase).keytocues) AND phases(curphase).keytocuespressed = 0 THEN
	'pokud je definovana klavesa na zobrazeni cues, a nebyla stlacena, tak cues nezobrazim'
		  segments% = 0
		END IF
    
	IF cc.startPointno > 0 THEN '17.1.2008 - vice moznych startu'
	   startPoint% = cc.startPointarr(cc.startPoint)
	ELSE
	   startPoint% = cc.startPoint
	END IF
	if empty(phases(curphase).keytostartafterfound) or phases(curphase).keytostartafterfoundpressed = 0 then
	  'kamil 4.3.2009 muzu stlacit klavesu pro zobrazeni startu po nalezeni cile'
		IF NOT empty(phases(curphase).keytocues) AND phases(curphase).keytocuespressed = 1 THEN
		'pokud je definovana klavesa na zobrazeni cues, a byla stlacena, tak nezobrazim start'
			  startPoint% = 0
		ELSEIF NOT empty(phases(curphase).keytostart) AND phases(curphase).keytostartpressed = 0 THEN
		'pokud je definovana klavesa pro zobrazeni startu a nebyla zmacknuta, tak start nezobrazim
				startPoint% = 0
		END IF
		
	end if
	
	
	'CALL cueswrite(laser%, segments%, startPoint%)
	IF laser% >= 1 AND laser% <= 8 THEN
	  cuesnew(laser%).laser = laser%
		IF cuesnew(laser%).segments = 0 THEN cuesnew(laser%).segments = segments%
		IF cuesnew(laser%).startPoint = 0 THEN cuesnew(laser%).startPoint = startPoint%
		CALL debugsetcue(cuesnew(laser%))
	END IF 'laser% >= 1 and laser% <= 8'
    END IF 'cc.arenaframe = 0'
	  NEXT i
	  CALL cuesupdate(cuesnew())
	END IF
END SUB

SUB tiskpos (pozice AS position, radek)
  LOCATE radek, 2
  PRINT "rfx:"; INT(pozice.rfx); "rfy:"; INT(pozice.rfy); "afx:"; INT(pozice.afx); "afy:"; INT(pozice.afy)
END SUB

SUB tisky (angle, frame, casstojici, phase, ppno, pp() AS phase, framestart)

	STATIC lastframespeed, lastangle
  IF pp(phase).noarenaframe = 0 and pp(phase).virtual = 0 THEN
   LOCATE 20, 2
	PRINT "arena angle: "; angle; " deg";
  	   
	IF (frame - lastframespeed >= 25) THEN ' jednou za vterinu'
		speed = (angle - lastangle) * 25 / (frame - lastframespeed)
		PRINT "  arena speed : "; speed; " deg/s      ";
		IF (speed = 0) THEN
			PRINT "ARENA STOJI!"
			casstojici = casstojici + 1
		ELSE
			PRINT "            "
		END IF
		lastangle = angle
		lastframespeed = frame
		
	END IF
  END IF
  if pp(phase).virtual = 0 then 
	  LOCATE 27,2
	   print "start time: "; gettime$(framestart);
		LOCATE 21, 2
		PRINT "phase: "; phase; "time: "; gettime$(frame); "h:m:s "; TIME$;
		PRINT " level: "; getlevel;
	end if 'if virtual = 0'
		radka = 22
		DIM faze AS STRING * 6
	  keytonext$ = ""
	  keyfound$ = ""
	  keycues$ = ""
	  keystart$ = ""
	  keyfoundbeepstop = 0
		FOR i = 0 TO ppno - 1
		 FOR j = 0 TO pp(i).sectorsno - 1
		  DIM ss AS sector
	    ss = pp(i).sectors(j) ' zkratka, pouzitelna jen pro vybirani hodnot, ne pro ukladani'
		  IF ss.mode > -1 THEN
			   LOCATE radka, 2
			   IF VAL(ss.kopiedo) > 0 THEN
			     faze = ss.kopiedo
			   ELSE
			     faze = STR$(i)
			   END IF
			   IF ss.mode = 0 THEN ssname$ = "R" ELSE ssname$ = "A"
			   IF pp(i).sectorsno > 1 THEN ssname$ = ssname$ + LTRIM$(STR$(j))' cislo sektoru se ukazuje jen pokud je jich vic'
			   IF ss.cno > 0 THEN
				   ssname$ = ssname$ + LTRIM$(STR$(ss.c))  'ukazuje cislo cile, pokud je cil na vice mistech'
					 IF LEN(LTRIM$(ss.cnamearr(ss.c))) > 0 THEN
				    ssname$ = ssname$ + ss.cnamearr(ss.c)
				   END IF
				   IF LEN(RTRIM$(LTRIM$(ss.cname))) > 0 THEN 'kamil 15.7.2008 - zkusim tam pridat jeste znak spolecny pro cely sektor'
				    ssname$ = ssname$ + RTRIM$(LTRIM$(ss.cname))
		 END IF
	       END IF
			   WHILE LEN(ssname$) < 6
		  ssname$ = ssname$ + " "
		 WEND
			   IF VAL(faze) = phase THEN ppname$ = "p*" ELSE ppname$ = "p "
			   IF ss.avoid = 1 THEN 'avoidance'
		       if pp(0).virtual = 0 then 
					 		PRINT ppname$; faze; ssname$; "E:"; ss.entrances; "T:"; gettime$(ss.frames);
					 
					 end if
	       ELSE
		       IF i = phase THEN
			' **** TISK PRO AKTIVNI FAZI ***** '
			IF ss.keyfoundbeepstop = 1 AND ss.keyfoundpressed = 1 THEN
			  timetoprint = frame - ss.keyfoundpressedframe
			ELSEIF pp(i).keytocuespressed = 1 THEN
		    timetoprint = frame - pp(i).keytocuespressedframe
		  ELSE
		    timetoprint = frame - ss.phasestart
		  END IF
		  if pp(0).virtual = 0 then
		  	PRINT ppname$; faze; ssname$; "E:"; ss.entrances; "R"; pp(i).repeat; " Tto:"; gettime$(timetoprint); "Dto:"; CINT(getdist(ss.distanceto) * 10) / 10;
		  	IF ss.distancemin > 0 THEN PRINT "/"; CINT(getdist(ss.distancemin) * 10) / 10; 'minimalni vzdalenost do cile'
			else 
					 		locate 28,2
		 					print pp(phase).repeat; ssname$;
			end if
		ELSE
		  ' **** TISK PRO NEAKTIVNI FAZE **** '
			if pp(0).virtual = 0 then
		  	PRINT ppname$; faze; ssname$; "E:"; ss.entrances; "R"; pp(i).repeat; " Tto:"; gettime$(ss.framesto); " Dto:"; CINT(getdist(ss.distanceto) * 10) / 10;
		  end if
		END IF
		 IF pp(i).repeatmax > 0 and pp(0).virtual = 0 THEN PRINT "Rmax:"; pp(i).repeatmax;
	       END IF
			   radka = radka + 1
		   END IF
		   IF i = phase AND NOT empty(ss.keytonext) THEN keytonext$ = ss.keytonext
		   IF i = phase AND NOT empty(ss.keyfound) THEN
				keyfound$ = ss.keyfound
				keyfoundpressed = ss.keyfoundpressed
			   IF i = phase AND ss.keyfoundbeepstop > 0 THEN keyfoundbeepstop = ss.keyfoundbeepstop
		   END IF
		 NEXT j
		 IF i = phase AND NOT (empty(pp(i).keytocues)) THEN keycues$ = pp(i).keytocues
		 IF i = phase AND NOT (empty(pp(i).keytostart)) THEN keystart$ = pp(i).keytostart
		 IF pp(i).secondmax > 0 and pp(0).virtual = 0 THEN
			 LOCATE 28, 25
			 PRINT "time max: "; pp(i).secondmax; "sec";
		 END IF
		 PRINT "                    ";
	  NEXT i
  
  
  LOCATE 29, 2
	IF NOT (empty(keystart$)) AND pp(phase).keytostartpressed = 0 THEN 
		PRINT keystart$; "-start";
	elseif  not empty(pp(phase).keytostartafterfound) and keyfoundpressed then 
		print pp(phase).keytostartafterfound; "-start";
	ELSE 
		PRINT "       ";
	end if
	IF NOT (empty(keycues$)) AND pp(phase).keytocuespressed = 0 AND (empty(keystart$) OR pp(phase).keytostartpressed) THEN PRINT ","; keycues$; "-cues";  ELSE PRINT "       ";
	IF NOT (empty(keyfound$)) AND (keyfoundpressed = 0 or keyfoundpressed < keyfoundbeepstop) AND (empty(keystart$) OR pp(phase).keytostartpressed) _
      AND (empty(keycues$) OR pp(phase).keytocuespressed) THEN
		PRINT ","; keyfound$;
		IF keyfoundbeepstop = 0 or (keyfoundbeepstop = 2 and keyfoundpressed = 0) THEN
			PRINT "-G found";
		ELSEIF keyfoundbeepstop = 1 or (keyfoundbeepstop = 2 and keyfoundpressed = 1)  THEN
			PRINT "-beepoff";
		ELSE
			PRINT "-beep   ";
		END IF
	ELSE
		PRINT "          ";
	END IF
	IF NOT (empty(keytonext$)) AND (empty(keycues$) OR pp(phase).keytocuespressed) _
      AND (empty(keystart$) OR pp(phase).keytostartpressed) AND (empty(keyfound$) OR keyfoundpressed>0) THEN
		PRINT ","; keytonext$; "-nxt phase";
	ELSE 
		PRINT "            ";
	END IF
	PRINT ",s-finish,q-cls,w-wcues,b-beep";	
	if pp(phase).virtual = 1 then print ",v-visible";
	
	

  'LOCATE 24, 2
	'print PPsectordist(pos as position, prr,ss as sector)
	'PRINT "dist: "; dist; ", angleR: "; angleR; ", angleA: "; angleA
END SUB

SUB updatecounts (frame, currentpos AS position, curf, pp() AS phase, lastpos AS position, ppno, prr)
    STATIC avoidstateLast 'ma to fungovat pro vsechny faze spolecne
'  ************* PRO VSECHNY SEKTORY SOUCASNE FAZE ************'
  FOR j = 0 TO pp(curf).sectorsno - 1
      IF pp(curf).sectors(j).mode = -1 THEN
       faze = pp(curf).sectors(j).kopiez
      ELSE
       faze = curf
      END IF
      DIM ss AS sector
      ss = pp(faze).sectors(j) ' zkratka, pouzitelna jen pro vybirani hodnot, ne pro ukladani'
   IF ss.avoidstate = 1 THEN 'jsem v soku'
' 1. ************* JSEM V SOKU ************'
	   pp(faze).sectors(j).frames = ss.frames + 1 'pocet framu v soku'
     IF ss.avoid = 0 THEN 'preference sector
	 ' ------- PREFERENCE SEKTOR ---------'
	  pp(faze).sectors(j).avoidstate = 0 ' reset do pristiho cyklu'
	  IF ss.avoidstateLast = 0 THEN 'preference sektor a minule nebyl v soku'
	   pp(faze).sectors(j).saveshock = 1 'ulozit shock'
	   pp(faze).sectors(j).entrances = ss.entrances + 1
	   pp(faze).sectors(j).shockstart = frame
       IF pp(faze).keytocuespressed = 1 THEN 'pokud jsem stlacil klavesu pro zobrazeni cues, pocitam cas od toho'
	     pp(faze).sectors(j).framesto = frame - max(pp(faze).keytocuespressedframe, ss.phasestart)'cas do nalezeni framu - phasestart se updatuje pri kazdem vstupu do soku
       ELSE
	     pp(faze).sectors(j).framesto = frame - ss.phasestart'cas do nalezeni framu'
       END IF
	   IF empty(pp(faze).keytocues) THEN 'pokud neni definovava klavesa pro zobrazeni cues, pocitam minimalni vzdalenost rovnou ted'
	     pp(faze).sectors(j).distancemin = PPsectordist(currentpos, prr, ss)
       END IF
      END IF
	 ELSEIF ss.avoidstateLast = 0 THEN 'avoid sektor a minule nebyl v soku'
     ' ------- AVOID SEKTOR ---------'
	  pp(faze).sectors(j).entrances = ss.entrances + 1
	  pp(faze).sectors(j).shockstart = frame 'pro ulozeni casu od zacatku a casu ve framu (u avodance)'
	 END IF
	 avoidstateLast = 1 'v minulem framu jsem v soku nebyl'
   ELSE 'nejsem v soku'
' 2. ************* NEJSEM V SOKU ************'
	 IF ss.avoidstateLast = 1 OR avoidstateLast = 1 THEN '.. a minule jsem byl'
	  pp(faze).sectors(j).phasestart = frame 'zacatek faze, pro pocitani casu do vstupu'
	  pp(faze).sectors(j).distanceto = 0 'zacatek faze, pro pocitani vzdalenosti do vstupu'
	  pp(faze).sectors(j).distancemin = 0
	  ss.distanceto = 0 'kvuli tomu prirazeni za chvili
	  IF ss.avoid = 1 THEN pp(faze).sectors(j).saveshock = 1
	 END IF
	 avoidstateLast = 0
  END IF
' 3. ************* KONEC  ************'
  
  
' ********* UPDATE VZDALENOSTI *****************88'
  ' VZDALENOST chci updatovat vzdycky pro soucasnou fazi'
	IF NOT empty(pp(faze).keytocues) AND pp(faze).keytocuespressed = 0 THEN
	     pp(faze).sectors(j).distanceto = 0 'pokud je definovana klavesa cues a nebyla stlacena'
	ELSE
    IF ss.mode = 0 THEN 'vzdalenost do nalezeni framu'
	 pp(faze).sectors(j).distanceto = ss.distanceto + distance(lastpos.rfx, lastpos.rfy, currentpos.rfx, currentpos.rfy)
	ELSE
	 pp(faze).sectors(j).distanceto = ss.distanceto + distance(lastpos.afx, lastpos.afy, currentpos.afx, currentpos.afy)
	END IF
  END IF
  ' MINIMALNI MOZNA VZDALENOST DO CILE (do stredu)
  IF NOT empty(pp(faze).keytocues) AND pp(faze).keytocuespressed = 1 AND pp(faze).sectors(j).distancemin = 0 THEN
	 pp(faze).sectors(j).distancemin = PPsectordist(currentpos, prr, ss)
  END IF
      
  
  pp(faze).sectors(j).avoidstateLast = ss.avoidstate
  NEXT j
' //// ************* PRO VSECHNY SEKTORY SOUCASNE FAZE ************'
END SUB

