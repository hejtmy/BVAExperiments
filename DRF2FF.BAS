DECLARE SUB getxymouse (xa!, ya!)
DECLARE SUB prehodcfgto (pp() AS ANY, faze!, places() AS INTEGER, pocet!)
DECLARE SUB importmisto (mm AS ANY, ss AS ANY, cislo!, znak AS STRING)
DECLARE SUB nextbeepcount (p AS ANY)
DECLARE SUB nakresliareny (screenarenas AS ANY, prr!, phases() AS ANY, phasesno!, currentphase!)
DECLARE SUB initdat (configno!, expname$, filename() AS STRING)
DECLARE SUB beepbvastart (beepcount!, vzdy!)
DECLARE SUB sndbvastart ()
DECLARE SUB sndbvastop ()
DECLARE SUB beepbva ()
DECLARE SUB inittrack (jmeno$, ppno!, pp() AS ANY, configno!, expname$, prr!, filename() AS STRING)
DECLARE SUB setcenter (pp() AS ANY, currentpos AS ANY, currentphase!, phasesno!)
DECLARE SUB rewritecues ()
DECLARE SUB nextcenter (p AS ANY, currentpos AS ANY)
DECLARE SUB updatecounts (frame!, currentpos AS ANY, curf!, pp() AS ANY, lastpos AS ANY, ppno!, prr!)
DECLARE SUB arenado0 (prr!, pp() AS ANY)
DECLARE SUB nextcues (phases() AS ANY, curphase!)
DECLARE SUB prehodcfg (pokus1!, pokus2!, pp() AS ANY, faze!)
DECLARE SUB resetkeyfoundpressed (curf!, pp() AS ANY)
DECLARE SUB saveshock (frame!, curf!, pp() AS ANY, pozice AS ANY, prr!)
DECLARE SUB setvyska (vyska AS INTEGER)
DECLARE SUB init (jmeno$, vyska%)
DECLARE SUB setupkaminsky ()
DECLARE SUB closetrack ()
DECLARE SUB closeoutput ()
DECLARE SUB main ()
DECLARE SUB savetrack (frame!, rfx!, rfy!, afx!, afy!, angle!, curf!, ppno!, pp() AS ANY, pauseframe!, klavesa$)
DECLARE SUB checklatency (s AS ANY, frame!, Sl0!, Sl1!, pauseframe!)
DECLARE SUB tiskpos (pozice AS ANY, radek!)
DECLARE SUB trackreset ()
DECLARE SUB savepos (pozice AS ANY, rfx!, rfy!, afx!, afy!)
DECLARE SUB PPconsiderCameraAngle (x!, y!, xr!, yr!, x0!, y0!)
DECLARE SUB PPconsiderHeight (x!, y!, h%, r0)
'#########################################################################'
'            DECLARATIONS                  '
'#########################################################################'
DECLARE SUB setcues (phases() AS ANY, currentphase!)
DECLARE SUB closing (frame!, anglesum!, casstojici!, ppno!, pp() AS ANY, cislo)
DECLARE SUB tisky (angle!, frame!, casstojici!, phase!, ppno!, pp() AS ANY, framestart!)
DECLARE SUB cekat (n!)
DECLARE SUB nahledcx (x!, y!, prr!, angle!, avoid!, curf!, pp() AS ANY)
DECLARE SUB PPinavoidcircle2 (x!, y!, dist!, angle!, r!, inavoid!, prr!)
DECLARE SUB PPavoidcircle2 (Pc1x!, Pc1y!, prr!, angle!, r0!, r!)
DECLARE SUB readini (lev AS INTEGER, ArenaRozmeryX AS ANY, ArenaRozmeryY AS ANY)
DECLARE SUB cuesreset ()
DECLARE SUB cueswrite (position%, symbol%, start%)

DECLARE SUB checklatencyA (avoidA!, frame!, Sl0!, Sl1!)
DECLARE SUB checklatencyR (avoidR!, frame!, Sl0!, Sl1!)
DECLARE FUNCTION gettime$ (frames!)
DECLARE FUNCTION usetrackval ()
DECLARE FUNCTION PPxy2angle! (x!, y!)
DECLARE SUB PPinavoid (x!, y!, c!, w!, d0!, d1!, inavoid!, radius!)
DECLARE SUB arenapos (deg!, turn!)
DECLARE SUB sndstart ()
DECLARE SUB sndstop ()
DECLARE SUB PPxy2screen (x!, y!, xc!, yc!, xr!, yr!, r!)
DECLARE SUB PPset (x!, y!, xc!, yc!, avoid!)
DECLARE SUB PProtate (x!, y!, deg!)
DECLARE SUB PPavoid (x!, y!, r!, c!, w!, d0!, d1!)
DECLARE FUNCTION PPxy2dist (x, y)
DECLARE SUB getxy (xa!, ya!)
DECLARE SUB levelforcompare (lev AS INTEGER)
DECLARE SUB resetframeflag ()
DECLARE SUB setuptrack ()
DECLARE SUB PParena (x!, y!, r!)
DECLARE FUNCTION deg (rad)
DECLARE FUNCTION sectorcenter (ss AS ANY)
DECLARE FUNCTION sectordistance (ss AS ANY)
DECLARE FUNCTION PPsectordist (currentpos AS ANY, prr, ss AS ANY)
DECLARE FUNCTION distance! (x0!, y0!, x1!, y1!)
DECLARE FUNCTION empty (a$)
DECLARE FUNCTION sectorbeepcount (s AS ANY)
DECLARE FUNCTION getlevel ()
DECLARE SUB prevcenter (p AS ANY)
DECLARE SUB resetcounts (curf!, pp() AS ANY)
DECLARE SUB eegwrite (zapni%)

' typy zavisle na projektu'
' $INCLUDE: 'TYPEDEF.BAS'

'#########################################################################'
'            DISPLAY CONFIGURATION                '
'#########################################################################'
CLEAR , , 20000 'make space for the stack - out of stack error'
CALL setupkaminsky

' coordinates of the two arenas
DIM screenarenas AS position
screenarenas.rfx = 150
screenarenas.rfy = 150
screenarenas.afx = 450
screenarenas.afy = 150
prr = 140

' shock latency
Sl0 = 8 ' to enter the avoid sectors in frames number (1 frame = 40ms)
Sl1 = 8 ' to leave in frames number

' pauza'
pauzadelka = 250 'framu'
shockdelaymin = 30 'framu' - po jake dobe mezi sokama se udela pauza

'kontinuita tracku'
disttoomuch = 100 'kolim cm je prilis mnoho od jednoho framu do druheho'

'#########################################################################'
'            FRAMES CONFIGURATION                 '
'#########################################################################'
CLS
IF usetrackval = 0 THEN PRINT "!!! TRACKUJE SE MYS" ELSE PRINT "  TRACKUJE SE DIODA"
CALL init(jmeno$, vyska%)' zjisti jmeno cloveka a zacne ukladat do #2'
PRINT "stisknete klavesu pro menu"
DO
LOOP WHILE INKEY$ = ""
SCREEN 12
' $INCLUDE: 'CFG.bas'

CALL closeoutput ' zavira soubor se souhrnnymi vysledky #2'
END
' pada mi to ve winxp po prepnuti do grafickeho rezimu 29.11.2007'

' PLAN: chci udelat centra sektoru a cues jako pole'
' musim tedy misto primo cteni z .c volat funkci, ktera mi precte tu spravno hodnotu'
' hodnoty budou v carr - '
' uz to snad je, jesto to radsi zkouknu a zkusim pustit 29.11.2007 15:32:49'

'#########################################################################'
'            PROGRAM STARTS                  '
'#########################################################################'
SUB main

SHARED phasesno
SHARED phases() AS phase
SHARED configno
SHARED expname() AS STRING
SHARED filename() AS STRING
SHARED jmeno$
SHARED vyska%
SHARED lev, px0, px1, py0, py1
SHARED pxr, pyr, prr, screenarenas AS position, Sl0, Sl1, pauzadelka, shockdelaymin
SHARED disttoomuch
SHARED prekreslinastartu!

CALL cuesreset
CALL initdat(configno, expname(configno), filename())
CALL inittrack(jmeno$, phasesno, phases(), configno, expname(configno), prr, filename())
IF vyska% > 0 THEN CALL setvyska(vyska%)
PRINT "stisknete klavesu pro start testu"
DO
LOOP WHILE INKEY$ = ""
'NAKRESLIM UVODNI OBRAZOVKU s KRUHAMA'
CALL nakresliareny(screenarenas, prr, phases(), phasesno, 0)

frame = 0   ' frame count
framestart = 0' framu od objeveni se posledniho startu
'CALL closing(frame, anglesum, casstojici, phasesno, phases(), 0)
CALL arenapos(anglelast, nic)
anglesum = 0
casstojici = 0
currentphase = 0
CALL setcues(phases(), currentphase) 'rozsviti cues pro tuto prvni fazi'
'CALL closing(frame, anglesum, casstojici, phasesno, phases(), 1)
pauseframe = 0 ' kolik framu zbyva do konce pauzy (neprepinaji se framy, nepipa)
DIM lastpos AS position
DIM currentpos AS position
prvnipozice = 1 'jestli nasledujici pozice, kterou ziskam je prvni v teto fazi
DO
	klavesa$ = LCASE$(INKEY$)
	klavesa2$ = ""
	IF klavesa$ = "q" THEN CALL nakresliareny(screenarenas, prr, phases(), phasesno, currentphase)
	IF klavesa$ = "w" THEN CALL rewritecues 'prepise cues, kdyz se objevi nejake chybne'
	IF klavesa$ = "-" THEN CALL levelforcompare(getlevel - 1)
	IF klavesa$ = "+" THEN CALL levelforcompare(getlevel + 1)
	IF klavesa$ = "b" THEN CALL beepbvastart(10, 1)
	IF klavesa$ = "p" THEN ' vraceni pokusu zpet'
	   WHILE currentphase > 0
		 currentphase = currentphase - 1
		 CALL prevcenter(phases(currentphase))
		 CALL resetcounts(currentphase, phases())
	   WEND
		CALL setcues(phases(), currentphase) 'rozsviti cues'
		CALL setcenter(phases(), currentpos, currentphase, phasesno) ' kvuli relativni pozici cile'
		prvnipozice = 1
		PRINT #2, "trial cancelled"
	END IF
	  ' klavesu e pro zapis do paralelniho portu dam na konec - 15.4.2015 kvuli min zpozdeni'
	 ' CALL resetframeflag
    IF phases(currentphase).virtual = 1 THEN
		CALL getxymouse(x, y)' jen mys'
	ELSE
		CALL getxy(x, y) ' zdroj zalezi na nastaveni usetrack'
	END IF
	' CALL resetframeflag
	' ziskam adresu x,y
	' CALL PPxy2screen(x, y, px0 + pxr, py0 + pyr, pxr, pyr, prr)
	' deklarace: PPxy2screen(x,y, xcenter, ycenter, xradius, yradius, output radius'
	' returns xy where [0;0] is in the middle and radius is prr
	
	' CALL PPconsiderHeight(x, y, vyska%, prr)
	rfx = x 'room frame'
	rfy = y
	' POZOR, POKUD PRIDAM ARENA FRAME, vynecha se kazdy druhy frame asi!'
	IF phases(currentphase).noarenaframe = 0 THEN 'pokus existuje arena frame'
	CALL arenapos(angle, nic)    ' zjistim soucasne otoceni areny
	'PRINT angle
	CALL PProtate(x, y, angle)   ' orotuji pozici cloveka
	ELSE
      angle = 0' arena se neotaci
   END IF
	afx = x 'arena frame'
	afy = y
	CALL savepos(currentpos, rfx, rfy, afx, afy) 'ulozim pozici do currentpos
	IF prvnipozice THEN 'prvni pozice uplne nebo po nalezeni pref framu
		lastpos = currentpos
		prvnipozice = 0
	END IF
  IF distance(lastpos.rfx, lastpos.rfy, currentpos.rfx, currentpos.rfy) >= disttoomuch THEN
     LOCATE 26, 25
     PRINT distance(lastpos.rfx, lastpos.rfy, currentpos.rfx, currentpos.rfy); disttoomuch;
     currentpos = lastpos '21.2.2008'
  END IF
   
  
	beepcount = 0
  avoidstate = 0
	tonextphase = 0 ' if to step to next phase at the end of cycle
	avoidbeep = 0 'jestli to ma pipat pri vstupu do avoidance sektoru'
	'beepedalready = 0 'pokud uz beeplo v preference fazi'
	' ***** CYKLUS PRO VSECHNY SEKTORY SOUCASNE FAZE *****
	FOR s = 0 TO phases(currentphase).sectorsno - 1 'pro vsechny sektory v tehle fazi potrebuju zjistit jestli do nich clovek vstoupil
		DIM ss AS sector
		IF phases(currentphase).sectors(s).mode = -1 THEN
	 faze = phases(currentphase).sectors(s).kopiez ' nektere faze maji takhle sektory zkopirovane odjinud'
       ELSE
	 faze = currentphase
       END IF
       ss = phases(faze).sectors(s)
		IF ss.mode = 0 THEN 'RF'
			IF ss.shape = 0 THEN ' sector
				CALL PPinavoid(rfx, rfy, sectorcenter(ss), ss.w, sectordistance(ss), ss.r1, ss.avoidstate, prr)
			ELSE
				CALL PPinavoidcircle2(rfx, rfy, sectordistance(ss), sectorcenter(ss), ss.r, ss.avoidstate, prr)
			END IF
		ELSE ' AF'
			IF ss.shape = 0 THEN ' sector
				CALL PPinavoid(afx, afy, sectorcenter(ss), ss.w, sectordistance(ss), ss.r1, ss.avoidstate, prr)
			ELSE
				CALL PPinavoidcircle2(afx, afy, sectordistance(ss), sectorcenter(ss), ss.r, ss.avoidstate, prr)
			END IF
		END IF
		CALL checklatency(ss, frame, Sl0, Sl1, pauseframe)
		phases(faze).sectors(s).avoidstate = ss.avoidstate ' to musim doplnit, zmenilo se to v PPinavoid'
		phases(faze).sectors(s).frame0 = ss.frame0 ' to musim doplnit - zmenilo se to uvnitr checklatency - 'frame, where change of avoidance state begun'
		phases(faze).sectors(s).lastA = ss.lastA ' to musim doplnit- last avoidance state, after considering latency
		' ***** jsem v preference framu *********** '
		IF ss.avoid = 0 AND ss.avoidstate = 1 THEN ' pokud to je preference frame a jsem v nem,
			IF pauseframe = 0 AND frame - ss.shockstart < shockdelaymin AND empty(ss.keytonext) THEN 'pokud sok byl hodne nedavno'
			 'muze byt jen minimalni pocet framu mezi pipnutimi, jinak se asi prekryvaji a bude pauza
			  pauseframe = pauzadelka ' nastavim pauzu - delka pauzy'
			END IF
			IF pauseframe = 0 THEN 'pokud neni pauza'
	    IF empty(ss.keytonext) THEN 'pokus neni definovana klavesa pro dalsi fazi'
			     tonextphase = 1 ' pokud to je preference frame a jsem v nem, tak jdu na dalsi fazi'
			     phases(faze).sectors(s).lastA = 0 ' pred prepnutim do jine aktivni faze jeste zrusim tohle, ono mi to jinak pretrvava po celou dobu
			   phases(faze).sectors(s).frame0 = 0 ' zase kvuli checklatency
			END IF
			IF empty(ss.keyfound) OR (ss.keyfoundbeepstop = 1 AND ss.keyfoundpressed = 0) OR (ss.keyfoundbeepstop = 2 AND ss.keyfoundpressed = 1) THEN
	     'pipani se spusti ve trech pripadech
	     ' - pokud neni definovana klavesa pro nalezeni cile'
	     ' - je definovana, ale ma zastavit pipani a jeste nebyla stlacena'
	     ' - je definovana, ale ma zapnout pipani a uz byla stlacena'
			     beepcount = sectorbeepcount(ss) 'a mam pipnout tolikrat kolik je v konfiguraci '
			     'sound 440,2
	    END IF
			ELSE 'pokud je pauza'
			  phases(faze).sectors(s).avoidstate = 0
			  ss.avoidstate = 0
			  tonextphase = 0
			  beepcount = 0
			END IF
		ELSEIF ss.avoid = 1 AND ss.avoidstate = 1 AND sectorbeepcount(ss) >= 0 THEN
		  avoidbeep = 1 'vstoupil jsem do avoidance sektoru'
		END IF
		' KLAVESA G DALSI FAZE'
    IF NOT empty(ss.keytonext) AND klavesa$ = LCASE$(ss.keytonext) AND (empty(phases(faze).keytocues) OR phases(faze).keytocuespressed = 1) AND (empty(ss.keyfound) OR ss.keyfoundbeepstop = 1 OR ss.keyfoundpressed > 0) AND (empty(phases(faze). _
keytostart) OR phases(faze).keytostartpressed) THEN
	    tonextphase = 1
	    phases(faze).sectors(s).lastA = 0 ' pred prepnutim do jine aktivni faze jeste zrusim tohle, ono mi to jinak pretrvava po celou dobu
		  phases(faze).sectors(s).frame0 = 0 ' zase kvuli checklatency
		  IF empty(phases(faze).keytostart) THEN framestart = 0 'cas od objeveni se startu'
		  IF ss.keyfoundpressed = 0 THEN 'pokud nebyla v teto fazi stlacena klavesa o nalezeni sektoru, tak nastavim ze jako zmacknuta'
		    klavesa2$ = ss.keyfound
		  END IF
		  IF phases(faze).virtual = 1 THEN
				phases(faze).goalvisible = 0 'na zacatku virtualni faze se vzdy zmici cil'
				CALL nakresliareny(screenarenas, prr, phases(), phasesno, currentphase) ' kamil 21.4.2010 - POKUS prekreslim areny aby zmizela posledni pozice mysi'
			END IF
		ELSEIF NOT empty(ss.keytonext) AND ss.avoidstate = 1 THEN 'zablokuju prechod do dalsi faze, kdyz neni stlacena klavesa'
		  tonextphase = 0
		  ss.avoidstate = 0 '8.1.2010 - nebudou se ukladat do .dat radky, kdy neni stlacene G, ale jen clovek vleze do soku
		  phases(faze).sectors(s).avoidstate = 0
		END IF
		' KLAVESA F GOAL FOUND
		IF klavesa2$ = ss.keyfound OR (NOT empty(ss.keyfound) AND klavesa$ = LCASE$(ss.keyfound) AND (empty(phases(faze).keytocues) OR phases(faze).keytocuespressed = 1)) THEN
		  IF empty(ss.keyfound) AND ss.avoidstate = 1 THEN ' pokud neni F definovane a presto je to tady - cili se to sem dostalo pres stlaceni G'
		    ' 8.1.2010 - sem se to myslim nikdy nemuze dostat - protoze cely blok je platny jen pokud je klavesa keyfound
		    'piskat to ma, jen pokud se nemacka f pri nalezeni cile'
		    ' a jen pokud jsem opravdu v tom sektoru a ne nekde mimo
		    'sem se to muze dostat i bez stlaceni F pri stlaceni G - to to ma piskat
		    'beepcount = sectorbeepcount(ss) 'a mam pipnout tolikrat kolik je v konfiguraci '
		  END IF
		  IF ss.keyfoundpressed = 0 THEN
		     phases(faze).sectors(s).avoidstate = 1
		     ss.avoidstate = 1
		  END IF
		  phases(faze).sectors(s).keyfoundpressed = ss.keyfoundpressed + 1
		  phases(faze).sectors(s).keyfoundpressedframe = frame
		ELSEIF NOT empty(ss.keyfound) AND (ss.keyfoundbeepstop = 0 OR (ss.keyfoundbeepstop = 2 AND ss.keyfoundpressed <> 1)) THEN 'zablokuju ulozeni soku, pokud neni stlacena klavesa f'
		  ' pokud je keyfoundbeepstop=2, tak pred stlacenim klavesy f cil nepipa, po prvnim stlaceni pipa, a po druhem stlaceni zase nepipa - oprava 8.1.2010
		  ' piska to pri definovane klavese f a vstupu do oblasti pokud je keyfoundbeepstop = 1 nebo kfbs= 2 a zmacknuto 1x
			' takze nepiska to pokud je kfbs 0 nebo (2 a zmacknuto <>1 )
		  phases(faze).sectors(s).avoidstate = 0
		  ss.avoidstate = 0
		END IF
	  
		' KLAVESA X ROZSVITIT START'
		IF NOT empty(phases(faze).keytostart) THEN
			IF klavesa$ = LCASE$(phases(faze).keytostart) AND phases(faze).keytocuespressed = 0 THEN
				phases(faze).keytostartpressed = 1
				CALL setcues(phases(), currentphase) 'rozsviti cues'
				framestart = 0 'cas od objeveni se startu'
			ELSEIF phases(faze).keytostartpressed = 0 THEN
				' jinak zablokuju vstup do sektoru
				phases(faze).sectors(s).avoidstate = 0
		      ss.avoidstate = 0
			END IF
		END IF
		' KLAVESA C ROZSVITIT CUES'
		IF NOT empty(phases(faze).keytocues) OR phases(faze).keytostartpressed THEN
	      IF klavesa$ = LCASE$(phases(faze).keytocues) AND (empty(phases(faze).keytostart) OR phases(faze).keytostartpressed) THEN
			    phases(faze).keytocuespressed = 1
			    phases(faze).keytocuespressedframe = frame ' ukladam si cas stlaceni klavesy'
			    CALL setcues(phases(), currentphase) 'rozsviti cues'
			    IF prekreslinastartu! = 1 THEN CALL nakresliareny(screenarenas, prr, phases(), phasesno, currentphase)
			    IF phases(faze).virtual = 1 AND phases(faze).tovisibletarget = 1 THEN phases(faze).goalvisible = 1 'navigace na viditelny cil'
			  ELSEIF phases(faze).keytocuespressed = 0 THEN 'pokud jeste nebyly zobrazeny cues, nemuze clovek najit cil'
			    phases(faze).sectors(s).avoidstate = 0
			    ss.avoidstate = 0
			    CALL setcenter(phases(), currentpos, currentphase, phasesno)' kvuli relativni pozici cile'
			  END IF
		END IF
' KLAVESA V GOAL VISIBLE, POKUD VIRTUAL'
		IF phases(faze).virtual = 1 THEN
			IF klavesa$ = "v" THEN phases(faze).goalvisible = 1 - phases(faze).goalvisible
		END IF
		' KLAVESA T START PO NALEZENI CILE'
		IF NOT empty(phases(faze).keytostartafterfound) THEN
			IF ss.keyfoundpressed > 0 AND klavesa$ = LCASE$(phases(faze).keytostartafterfound) THEN
			  IF phases(faze).keytostartafterfoundpressed = 0 THEN
					phases(faze).keytostartafterfoundpressed = 1
				ELSE
					phases(faze).keytostartafterfoundpressed = 0
				END IF
				CALL setcues(phases(), currentphase) 'rozsviti cues'
			END IF
		END IF
		
		'klavesy natvrdo jsou na zacatku cyklu'
		avoidstate = avoidstate OR ss.avoidstate
    LOCATE 27, 22
	  IF phases(faze).virtual = 0 THEN PRINT "dist:"; PPsectordist(currentpos, prr, ss); "         ";
    'LOCATE 28, 2
    'print "keyfoundpressed: ";ss.keyfoundpressed; "; avoidstate: ";avoidstate;
    
	  'IF ss.keyfoundbeepstop = 1 AND ss.keyfoundpressed >= 1 THEN beepcount = 0
	  'IF ss.keyfoundbeepstop = 2 AND (ss.keyfoundpressed = 0 OR ss.keyfoundpressed > 1) THEN beepcount = 0 'kamil 1.9.2008 keyfoundbeepstop = 2 - ma to zacit piskat az po stlaceni f - a po dalsim stlaceni se to zase vypnout'
	  ' tohle oboji vyse uz je osetreni v sekci o vstupu do oblasti a zapinani pipani'
	  
	NEXT s 'KONEC CYKLU PRO VSECHNY SEKTORY'
   
	IF phases(currentphase).noroomframe = 0 THEN 'pokus existuje room frame'
	 CALL PPset(rfx, rfy, screenarenas.rfx, screenarenas.rfy, avoidstate) 'nakresli bod v RF
	END IF
	IF phases(currentphase).noarenaframe = 0 THEN 'pokus existuje arena frame'
    CALL PPset(afx, afy, screenarenas.afx, screenarenas.afy, avoidstate) 'nakresli bod v AF'
  END IF
  IF klavesa$ = "e" THEN 'singal na paralelni port kvuli EEG - 15.4.2015'
	 CALL eegwrite(1)
	 CALL cekat(10000)
   CALL eegwrite(0)
	END IF
	CALL savetrack(frame, rfx, rfy, afx, afy, angle, currentphase, phasesno, phases(), pauseframe, klavesa$)' tisk tracku
	CALL nahledcx(rfx, rfy, prr, angle, avoidstate, currentphase, phases())

	CALL updatecounts(frame, currentpos, currentphase, phases(), lastpos, phasesno, prr)' updatuje vysledky pro kazdy frame'
	IF pauseframe = 0 THEN 'pokud nejsem v pauze'
	  CALL saveshock(frame, currentphase, phases(), currentpos, prr) 'ulozi sok do souboru
	  LOCATE 28, 2
	  PRINT "                   "
	ELSE
	  LOCATE 28, 2
	  PRINT "pauza:"; gettime$(pauseframe)
	END IF
	
   IF phases(faze).virtual = 0 THEN
	LOCATE 20, 2
	PRINT configno; ": "; expname(configno);
	END IF
	CALL tisky(angle, frame, casstojici, currentphase, phasesno, phases(), framestart) ' tisk na obrazovku'
	lastpos = currentpos ' ulozi posledni pozici'
	
	anglesum = anglesum + angle - anglelast
	IF ABS(angle - anglelast) > 350 THEN
		anglesum = anglesum - SGN(angle - anglelast) * 360
	END IF
	anglelast = angle
		
	IF avoidstate = 1 OR beepcount > 0 THEN 'pokud jsem ve framu a ... muzu beepat jen pokud neni pauza'
		IF beepcount > 0 THEN ' je to preference sector'
		  CALL beepbvastart(beepcount, 0)
		ELSEIF avoidbeep = 1 THEN ' avoidance sector'
			CALL sndbvastart ' zapne trvaly zvuk'
		END IF
	ELSE ' nejsem ve framu'
		CALL sndbvastop 'vypne trvaly zvuk, ale jen pokud neni zapnute pipani'
	END IF
  CALL beepbva

	IF tonextphase = 1 THEN
    CALL resetkeyfoundpressed(currentphase, phases())
		CALL nextcenter(phases(currentphase), currentpos)' jeste musim posunout aktivni c - kamil 29.11.2007
		CALL nextcues(phases(), currentphase) ' nastavi dalsi cues'
		CALL nextbeepcount(phases(currentphase)) 'nastavi dalsi pocet piskani'
		phases(currentphase).repeat = phases(currentphase).repeat + 1'pocet opakovani faze'
		currentphase = currentphase + 1
		IF currentphase >= phasesno THEN currentphase = 0
		tonextphase = 0
		CALL setcues(phases(), currentphase) 'rozsviti cues'
		CALL setcenter(phases(), currentpos, currentphase, phasesno) ' kvuli relativni pozici cile'
		prvnipozice = 1
	END IF

	IF pauseframe > 0 THEN pauseframe = pauseframe - 1
  IF phases(currentphase).repeatmax > 0 AND phases(currentphase).repeat >= phases(currentphase).repeatmax THEN
    klavesa$ = "repeatmax"
  ELSEIF phases(currentphase).secondmax > 0 THEN
    IF phases(currentphase).keytocuespressed > 0 AND (frame - phases(currentphase).keytocuespressedframe) / 25 > phases(currentphase).secondmax THEN
	klavesa$ = "timemax"
    ELSEIF phases(currentphase).keytocuespressed = 0 AND frame / 25 > phases(currentphase).secondmax THEN
	klavesa$ = "timemax"
    END IF
  END IF
	frame = frame + 1
	framestart = framestart + 1
LOOP WHILE klavesa$ <> "s" AND klavesa$ <> "repeatmax" AND klavesa$ <> "timemax"

'konec programu:'

FOR i = 0 TO phasesno - 1
	FOR j = 0 TO phases(i).sectorsno - 1
		IF phases(i).sectors(j).avoid = 0 THEN 'preference sector'
			IF phases(i).sectors(j).avoidstate = 1 AND (empty(phases(i).sectors(j).keyfound) OR phases(i).sectors(j).keyfoundbeepstop = 1) THEN
				'ulozim sok, pokdu jsem byl v soku, a pokud neni definovana klavesa pro ulozeni soku'
				phases(i).sectors(i).saveshock = 1
			END IF
		ELSE 'avoidance sector - ulozit vzdy'
			IF phases(i).sectors(j).avoidstate = 1 THEN
				phases(i).sectors(i).saveshock = 1
			END IF
		END IF
	NEXT j
NEXT i
CALL saveshock(frame, currentphase, phases(), currentpos, prr)
'CLOSE #1
CALL sndstop

'CALL closing(frame, anglesum, entrancesA, entrancesR, frameavoidR, frameavoidA, Rrc, Rrw, Rr0, Rr1, Arc, Arw, Ar0, Ar1, casstojici)
CALL closing(frame, anglesum, casstojici, phasesno, phases(), 99)
CALL trackreset ' resetuje trackovani karty
CALL closetrack ' zavira soubor pro track'
CALL cuesreset
PRINT
PRINT "  finished: "; klavesa$
PRINT "  stisknete klavesu pro konec                                 "
DO
LOOP WHILE INKEY$ = ""
CLS
END SUB 'main'

