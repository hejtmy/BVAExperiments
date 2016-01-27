CONST VERSION$ = "1.9.2008"
CONST VARIABLENO = 48	'Maximum number of array dimensions
TYPE CUE
  startPoint AS INTEGER '0 - no start, 1 - start dot, pokud startPointno > 0, pouziva se jako cisla dalsiho laseru ze startPointarr
  startPointno AS INTEGER '0 - nepouziva se, pokud >0 pouziva se misto startPoint '
  startPointarr(0 TO VARIABLENO)  AS INTEGER 'seznam startpointu'
  segments AS INTEGER '0 to 3, 1-two lines, 2 three lines, 3 zero sign
  segmentsno AS INTEGER '0 - nepouziva se, pokud >0, pouziva se misto segments'
  segmentsarr(0 TO VARIABLENO) AS INTEGER 'seznam segmentu'
  laser AS INTEGER '1 to 8,pokud laserno > 0 pouziva se jako cislo dalsiho laseru z laserarr k rozsviceni
  laserno AS INTEGER 'pocet umisteni cue v laserarr, pokud > 0 pouziva se misto laser'
  laserarr(0 TO VARIABLENO) AS INTEGER 'seznam moznych umisteni cues v hodnotach 1 to 8'
  color AS INTEGER 'barva cue pri vykresleni na obrazovku 12-red, 2-green'
  kopiez AS INTEGER 'kdyz je to >-1, tak urcuje, z ktere faze se kopiruje (cue pod stejnym cislem)
	'- nesmi ukazovat cue, ktere uz je kopirovane!
	'- kopirovany laserarr se neposouva pri prechodu do dalsi faze
  arenaframe AS INTEGER 'pokud je arenaframe = 1 tak je cue vazane na arena frame
	'- a zobrazuje se jen na pocitaci
  c AS INTEGER 'cue se muze nastavit i pomoci uhlu ve stupnich',
	'to se pouzije pokud je laser = 0 a samozrejme jen pokud je arenaframe = 1
END TYPE

TYPE sector
 ' vsechno to jsou INTETEGER, ale s tim se mi to nepodarilo rozchodit
  mode AS SINGLE '0 RF, 1 AF, -1 kopie jine faze, podle kopiez
  shape AS SINGLE '0 - sector, 1 - circle'
  sound AS SINGLE '0 - steady, 1 - 3 short
  avoid AS SINGLE '0 - preference, 1-avoidance; 0 trigers transition to next phase
  w AS SINGLE 'width - sector in deg (sector)
  c AS SINGLE ' center in deg (both sector and circle), pokud cno > 0, pouziva se pro ulozeni cisla dalsiho carr
  cname as string * 3 'jmeno sektoru'
  cno AS SINGLE 'pocet moznych umisteni sektoru, pouziva se carr misto c pokud cno > 0 '
  carr(0 TO VARIABLENO) AS SINGLE ' seznam moznych umisteni sektoru, bude se vybirat postupne jedno za druhym '
  cnamearr(0 TO VARIABLENO) AS STRING * 3 'jmena jednotlivych opakovani'
  crelativeto AS SINGLE ' c se urcuje relativne vzhledem k pozici subjektu na pocatku faze -1 (napr kdyz je to 1, pocita se to relativne k fazi 0) '
  cstart AS SINGLE 'uhel c ktery se pricita k pozici cile - nastavuje se automaticky, pokud je crelativeto>0'
  r AS SINGLE ' radius (circle)
  r0 AS SINGLE ' from r in % of r (sector or distance from center in % - circle), pokud r0no>0, pouziva se pro ulozeni cisla dalsiho r0arr
  r0no AS SINGLE 'pocet moznych vzdalenosti sektoru od stredu'
  r0arr(0 TO VARIABLENO) AS SINGLE 'seznam vzdalenosti sektoru od stredu, bude se vybirat postupne jedno za druhym'
  r1 AS SINGLE ' to r in % of r (sector)
  kopiez AS SINGLE 'cislo faze ze ktere okopirovat vlastnosti - sektory v ruznych fazich se pak povazuji za
  kopiedo AS STRING * 5 'jen pro informaci pri tisku, co kterych fazi se takhle kopiruje'
  ' VYSLEDKOVA CAST'
  avoidstate AS SINGLE '0 - neni v soucasne dobe v avoidance, 1-je v avoidance' - plni se behem programu
  avoidstateLast AS SINGLE ' avoidstate v predchozim cyklu - pro pocitani poctu vstupu'
  entrances AS SINGLE ' pocet vstupu'
  frames AS SINGLE ' pocet framu inside'
  framesto AS SINGLE ' framy k cili' - posledni hodnota
  distancemin AS SINGLE ' minimalni vzdalenost k cili'
  distanceto AS SINGLE ' usla vzdalenost k cili'  - posledni hodnota
  shockstart AS SINGLE ' kdy zacal shock (frames)
  phasestart AS SINGLE 'kdy jsem se dostal do soucasne faze (frames)'
  saveshock AS SINGLE ' signalizace, ze skoncil shock a ze se ma ulozit
  lastA AS SINGLE 'last avoidance state, after considering latency
  frame0 AS SINGLE 'frame, where change of avoidance state begun'
  beepcount AS SINGLE 'number of short beeps after entering (if preference sector) - pokud je -1 tak nepipa ani kdyz je avoid
  beepcountno as single 'pocet ruznych beepani - pokud > 0, pak se pouzivat beepcount jako ukazatel aktualni pozice'
  beepcountarr(0 to VARIABLENO) as single 'ruzny pocet beepani'
  keytonext AS STRING * 1 'klavesa, ktera se ma zmacknout pro postup do dalsi faze - kdyz je nastavena, nefunguje automaticky prechod'
  keyfound AS STRING * 1 'klavesa, kterou se znaci subjektem zadana pozice cile - jako saveshock'
  keyfoundbeepstop AS SINGLE '0 - cil nepipa, f oznacuje cil, 1- f neoznacuje cil, ale jen vypina beepani po nalezeni cile, 2 - f oznacuje cil a startuje pipani po nalezenic cile
  keyfoundpressedframe AS SINGLE 'kdy byla zmacknuta klavesa o nalezeni framu'
  keyfoundpressed AS SINGLE ' indikator jestli byla zmacknuta klavesa o nalezeni framu'
  'keyfoundshowstart as single ' pokud je jedna, po znacknuti keyfound se znovu zobrazi start'
END TYPE

TYPE phase
  sectors(0 TO 3) AS sector
  cues(0 TO 2)  AS CUE
  sectorsno AS SINGLE ' pocet sektoru'
  cuesno AS SINGLE ' pocet cues'
  cuesset AS SINGLE 'pocet cues definovanych v teto fazi - ty nedefinovane jsou podle predchoz
  keytocues AS STRING * 1'klavesa ktera se ma zmacknout aby se zobrazily cues'
  hidenotstart as integer 'pokud 1, start nezmizi, i kdyz je definovana klavesa c'
  keytocuespressed AS INTEGER 'jestli uz v teto fazi bylo zmacknuta klavesa pro zobrazeni cuesi faze'
  keytocuespressedframe AS SINGLE 'cas (frame), kdy byla stlacena klavest na cues'
  repeatmax AS INTEGER 'kolikkrat se muze maximalne faze opakovat'
  repeat AS INTEGER 'kolikrat se faze opakuje - plni se samo'
  secondmax AS INTEGER 'kolik sekund maximalne muze program bezet'
  noarenaframe AS INTEGER ' jestli se ne-ma zobrazovat mapa arenaframu'
  noroomframe as integer ' jestli se ne-ma zobrazovat mapa roomframu'
  virtual as integer ' jestli se hraje primo na obrazovce - zadne tisky, zadne cil,
  goalvisible as integer ' pokud je virtual = 1 tak jestli ma byt cil videt. Prepina se klavesou Visible target
  tovisibletarget as integer ' jestli je to virtualni navigace na viditelny cil - pokud 1 je cil neviditelny pri zobrazeni startu a pak se sam objevi
  startpressed AS SINGLE 'jestli uz byla stlacena klavesa pro start'
  keytostart AS STRING * 1'klavesa ktera se ma zmacknout aby se zobrazil start na zacatku'
  keytostartpressed AS INTEGER 'jestli uz v teto fazi bylo zmacknuta klavesa pro zobrazeni startu'
keytostartafterfound as string * 1 'klavesa, ktera se ma zmacknout, aby se zobrazil start po nalezeni cile (EGO)'
  keytostartafterfoundpressed as integer 'klavesa, ktera se ma zmacknout, aby se zobrazil start po nalezeni cile (EGO)'
END TYPE

TYPE position
  rfx AS SINGLE
  rfy AS SINGLE
  afx AS SINGLE
  afy AS SINGLE
END TYPE
' typy pro mys - podle martina kucery'
TYPE Bod
	x AS INTEGER
	y AS INTEGER
END TYPE

TYPE StavType
	pozX AS INTEGER
	pozY AS INTEGER
	tlac AS INTEGER
END TYPE

TYPE OvladacType
	pritomnost AS INTEGER
	tlacitek AS INTEGER
END TYPE

' typy dulezite pro vypocet skutecne pozice ze souradnic ziskanych od kamery - kamil 5.12.2007
TYPE PowerFitParams ' funkce y = baseline + (x/amplitude)^power'
  power AS SINGLE
  amplitude AS SINGLE
  baseline AS SINGLE
END TYPE

TYPE SinFitParams ' funkce y = sin((x-x0)/period * pi/2) *amplituda + y0'
  x0 AS SINGLE
  y0 AS SINGLE
  amplitude AS SINGLE
  period AS SINGLE
END TYPE


TYPE PositionCompute
  stred AS SINGLE
  minFit AS SINGLE ' minimalni hodnota x z kamery odhadnuta podle vysky cloveka'
  maxFit AS SINGLE
  minFitParams AS PowerFitParams
  
  Line1a AS SINGLE ' parametry primky, pomoci ktere souradnice z kamery prevedu na hodnoty 0 - 280'
  Line1b AS SINGLE
  SinParams AS SinFitParams
  SinAmplParams AS PowerFitParams
  SinPeriodParams AS PowerFitParams
END TYPE

TYPE ArenaDim
  p0 AS INTEGER
  p1 AS INTEGER
END TYPE


