10000 REM :::::::::::::::::::::::::::::::::::::::::::::
10010 REM :: SNAC-SNAKE FOR AgonLight (BBC BASIC v3) ::
10020 REM :::::::::::::::::::::::::::::::::::::::::::::
10030 REM :: 20230710: Version 1.1 - bug fixes       ::
10040 REM :::::::::::::::::::::::::::::::::::::::::::::
10050 REM :: The game was originally created for the ::
10060 REM :: Olimex Weekend Programming Challenge    ::
10070 REM :: Issue #3                                ::
10080 REM :::::::::::::::::::::::::::::::::::::::::::::
10090 REM :: It is best experienced on a 40+ column, ::
10100 REM :: 16+ color display                       ::
10110 REM :::::::::::::::::::::::::::::::::::::::::::::
10120 CLEAR
10130 REPEAT CLS:SY$=FN_TO_UPPER(FN_PROMPT(0,0,"TARGET (A)gon or (B)BC B-SDL:","A")):UNTIL SY$ = "A" OR SY$ = "B"
10140 IF SY$ = "B" THEN LEFT = 136:RIGHT = 137:DOWN = 138:UP = 139:DL% = 8:MO% = 9:ELSE LEFT = 8:RIGHT = 21:DOWN = 10:UP = 11:DL% = 14:MO% = 2
10150 IF SY$ = "A" THEN REPEAT CLS:MO$=FN_PROMPT(0,0,"MODE (1,2,3,...):",STR$(MO%)):UNTIL VAL(MO$) > 0:MO% = VAL(MO$)
10160 MODE MO%
10170 PROC_SETUP
10180 REM ON ERROR PROC_HANDLE_ERROR:REM Handle ESC key
10190 :
10200 PROC_WELCOME
10210 PROC_NEW_GAME
10220 PROC_MAIN_LOOP:REM Invoke main loop
10230 PROC_DEATH_ANIMATION(X(P), Y(P)):VDU 17,RED:PROC_FULL_CENTER_TEXT(" GAME OVER"):VDU 17,YELLOW
10240 Resp$ = FN_PLAY_AGAIN:IF Resp$ = "Y" THEN 10210:ELSE PROC_GOODBYE(GameName$):PROC_HISCORE_WRITE(GameName$)
10250 END
10260 :
10270 REM ::::::::::::::::::::
10280 REM ::   Setup Game   ::
10290 REM ::::::::::::::::::::
10300 DEF PROC_SETUP
10310 GameName$ = "Snac-Snake"
10320 BLACK = 0:RED = 1:GREEN = 2:YELLOW = 3:BLUE = 4:MAGENTA = 5:CYAN = 6:WHITE = 7
10330 BLANK = 32:DOT = 38:BLANK_X = 64:SKULL = 42
10340 MON_WHITE = 134:MON_BLUE = 135:MON_RED = 136:MON_PINK = 137:MON_CYAN = 138:MON_GREEN = 139:MON_RESET = ASC("M")
10350 E_HEART = 129:E_DIAMOND = 130:E_FRUIT = 131:E_CIRC = 132:E_DISC = 133:E_TOADSTOOL = 147
10360 B_VERT = 140:B_HORZ = 141:B_UR = 142:B_UL = 143:B_DL = 144:B_DR = 145
10370 SN_W = 128:SN_L = 123:SN_R = 124:SN_D = 125:SN_U = 126
10380 SN_D1 = 150:SN_D2 = 151:SN_D3 = 152:SN_D4 = 153:SN_D5 = 154:SN_D6 = 155
10390 MAXINT% = &3B9AC9FF
10400 PROC_DEFAULT_COLORS
10410 PROC_REDEFINE_CHARS
10420 TK = TIME:PROC_SLEEP(100):TK = TIME - TK:REM CALIBRATE TIME TICKS
10430 IF SY$ = "A" THEN CW% = FN_getByteVDP(&13):CH% = FN_getByteVDP(&14)-2:ELSE CW% = 40:CH% = 23
10440 UX% = 0:UY% = 1:LX% = CW% - UX% - 1:LY% = CH% - UY%:REM Playing Field Boundaries
10450 UB% = (LX% - UX% + 1) + (LY% - UY% + 2) * LX%
10460 HighScore% = 1500:REM Set minimum high score
10470 PROC_HISCORE_READ(GameName$)
10480 MAX_SIZE% = (CW% + CH% - 2) DIV 2:REM Maximum Size of Snake
10490 MAX_MONSTER_SETS% = CW% DIV 40:REM Maximum number of 4-monster sets
10500 MAX_MONSTERS% = MAX_MONSTER_SETS% * 4:REM Maximum number of monsters that can appear at one time
10510 SP% = DL% * TK / 100:REM Speed Throttler (smaller value speeds up the game)
10520 DIM PV$(1),PH$(1)
10530 PH$(0)=CHR$(B_DL)+CHR$(BLANK_X)+CHR$(B_UL):PH$(1)=CHR$(B_DR)+CHR$(BLANK_X)+CHR$(B_UR)
10540 PV$(0)=CHR$(B_DL)+CHR$(BLANK_X)+CHR$(B_DR):PV$(1)=CHR$(B_UL)+CHR$(BLANK_X)+CHR$(B_UR)
10550 MO$=STRING$(MAX_MONSTER_SETS%,CHR$(MON_RED)+CHR$(MON_PINK)+CHR$(MON_CYAN)+CHR$(MON_GREEN))
10560 BX$=CHR$(B_UR) + CHR$(B_HORZ) + CHR$(B_UL) + CHR$(B_VERT) + CHR$(B_DL) + CHR$(B_HORZ) + CHR$(B_DR) + CHR$(B_VERT)
10570 DIM X(MAX_SIZE%-1),Y(MAX_SIZE%-1),MP%(MAX_MONSTERS% - 1),screen CW% * CH%
10580 ENDPROC
10590 :
10600 REM ::::::::::::::::::::::
10610 REM ::     New Game     ::
10620 REM ::::::::::::::::::::::
10630 DEF PROC_NEW_GAME
10640 LOCAL i%
10650 TI = 0:TIME = 0:Dead% = FALSE:Score% = 0:Size% = 2
10660 P = Size% - 1:REM Index Of Initial Position Of Snake's Head
10670 AF = 0:REM Initial Animation Frame For Snake's Head
10680 X(1) = CW% DIV 2:Y(1) = CH% DIV 2:X(0) = X(1):Y(0) = Y(1) - 1:REM Snake Starting Position
10690 D% = 3:REM Direction For Snake (initial = down)
10700 M_Position% = 0:REM Monster Position Index
10710 Portal_State% = 0:REM Portal State
10720 M_Count% = 0:REM Number Of Active Monsters
10730 MonGoWhite% = FALSE:MonReset% = FALSE:REM Monster color timer values
10740 Rvs_Dir% = FALSE:REM Reverse Direction Flag
10750 FOR i% = 0 TO MAX_MONSTERS% - 1:MP%(i%) = -1:NEXT i%
10760 CLS
10770 PROC_HIDE_CURSOR:PROC_CLEAR_SCREEN
10780 PROC_DRAW_PLAYING_FIELD(UX%, UY%, CW% - 2*UX%, CH% - UY%)
10790 VDU 17,YELLOW:PROC_FULL_CENTER_TEXT("GET READY!"):REM Display GET READY Message
10800 FOR i% = 1 TO 7:PROC_SOUND(i% + 10, 1.05 * i%):NEXT i%:REM Play some tones
10810 PROC_SLEEP(200):PROC_FULL_CENTER_TEXT(STRING$(10, " ")):REM Erase GET READY Message after 2 Seconds
10820 PROC_DRAW(X(0),Y(0),SN_W, TRUE):PROC_DRAW(X(1),Y(1),SN_W, TRUE):REM Draw Snake at Initial Position
10830 PROC_SCORES
10840 ENDPROC
10850 :
10860 REM :::::::::::::::::::::::
10870 REM :: Play Another Game ::
10880 REM :::::::::::::::::::::::
10890 DEF FN_PLAY_AGAIN
10900 LOCAL message$,r$
10910 message$ = "Play Again? (Y/N)":PROC_EMPTY_KEYBOARD_BUFFER
10920 REPEAT r$ = FN_PROMPT((CW% - LEN(message$)) DIV 2, CH% DIV 2 + 2, message$, "") UNTIL INSTR("YN", r$) <> 0
10930 = r$
10940 :
10950 REM :::::::::::::::::
10960 REM :: Say Goodbye ::
10970 REM :::::::::::::::::
10980 DEF PROC_GOODBYE(game$)
10990 PROC_HIDE_CURSOR
11000 CLS:PROC_FULL_CENTER_TEXT("So long and thank you for playing...")
11010 FOR i% = 1 TO FN_CENTER(game$):PRINTTAB(0, CH% DIV 2 + 2)STRING$(i%, " ")CHR$(17)CHR$(i% MOD 7 + 1)game$:PROC_SLEEP(20):NEXT i%
11020 PROC_DEFAULT_COLORS
11030 PROC_SHOW_CURSOR
11040 ENDPROC
11050 :
11060 REM ::::::::::::::::::::::
11070 REM ::     Main Loop    ::
11080 REM ::::::::::::::::::::::
11090 DEF PROC_MAIN_LOOP
11100 LOCAL dd%,prevsec%,sec%
11110 sec% = -1
11120 REPEAT
11130   TI = FN_INT_TIME:PROC_EMPTY_KEYBOARD_BUFFER
11140   prevsec% = sec%:sec% = FN_INT_TIME DIV TK
11150   REM PROC_COUT(STR$(MonGoWhite%)+" "+STR$(MonReset%)+" "+STR$(M_Position%)+"      ",0)
11160   PROC_MONSTER_COLOR_CHECK
11170   IF FN_IS_TIME(sec%, prevsec%, 30) THEN PROC_UPDATE_PORTAL_STATE
11180   REM SP% = FN_MAX(6, DL% - Size%):REM SP% = FN_MAX(6, SP% - Size%):REM SP% = FN_MIN(6, Score% DIV 250)
11190   IF SY$ = "A" THEN dd% = FN_ASCIIKEYCODE ELSE dd% = INKEY(TK/DL%)
11200   dd% = FN_MAP_INPUT(dd%):IF dd% > 0 THEN Rvs_Dir% = FN_CHECK_FOR_REVERSE_DIRECTION(D%, dd%):D% = dd%
11210   PROC_GROW_SNAKE(D%)
11220   IF FN_IS_TIME(sec%, prevsec%, 2.5) THEN PROC_RANDOM_EVENT
11230   PROC_SLEEP(TI + SP% - FN_INT_TIME):REM Throttle Speed (e.g. TI + SP% - FN_INT_TIME)
11240 UNTIL Dead%
11250 ENDPROC
11260 :
11270 REM ::::::::::::::::::::::::::::
11280 REM :: Miscellaneous Routines ::
11290 REM ::::::::::::::::::::::::::::
11300 DEF PROC_COUT(text$, row%):VDU 31,0,CH%+row%,17,WHITE:PRINT text$:ENDPROC
11310 DEF PROC_HIDE_CURSOR:VDU 23,1,0;0;0;0;:ENDPROC
11320 DEF PROC_SHOW_CURSOR:VDU 23,1,1;0;0;0;:ENDPROC
11330 DEF PROC_FULL_CENTER_TEXT(text$):VDU 31,FN_CENTER(text$), CH% DIV 2 - 1:PRINT text$;:ENDPROC
11340 DEF PROC_ERASE(x%, y%):PROC_PLOT(x%, y%, BLANK, BLACK):ENDPROC
11350 DEF PROC_SLEEP(hundredth_seconds%):LOCAL t:hundredth_seconds% = hundredth_seconds% + (hundredth_seconds% < 0) * -hundredth_seconds%:t = TIME:REPEAT UNTIL ((TIME - t) > hundredth_seconds%):ENDPROC
11360 DEF PROC_SCORES:LOCAL hs$, sc$:hs$ = CHR$(17)+CHR$(YELLOW)+"HIGH SCORE "+CHR$(17)+CHR$(WHITE)+STR$(HighScore%):sc$ = CHR$(17)+CHR$(RED)+"1UP "+CHR$(17)+CHR$(WHITE)+STR$(Score%):PRINT TAB(0,0)sc$:PRINT TAB(CW%-LEN(hs$)+4,0)hs$:ENDPROC
11370 DEF PROC_MON_GO_WHITE:MonGoWhite% = FN_INT_TIME + (TK+DL%)*3:MonReset% = (MonGoWhite% + (TK+DL%)*2) MOD MAXINT%:ENDPROC
11380 DEF FN_CENTER(text$):=(CW% - LEN(text$)) DIV 2 - 1)
11390 DEF FN_X_DELTA(d%):=(d% = 1) + (d% = 2)*-1 + (d% = 3)*0 + (d% = 4)*0
11400 DEF FN_Y_DELTA(d%):=(d% = 1)*0 + (d% = 2)*0 + (d% = 3)*-1 + (d% = 4)
11410 DEF FN_INT_TIME:=TIME MOD MAXINT%
11420 DEF FN_MAX(x, y):= y + (x > y) * (y - x)
11430 DEF FN_MIN(x, y):= y + (x < y) * (y - x)
11440 DEF FN_MAP_INPUT(n%):=(n% = LEFT)*-1 + (n% = RIGHT)*-2 + (n% = DOWN)*-3 + (n% = UP)*-4
11450 DEF FN_NEXT_POS(i%):=(i% + 1) MOD MAX_SIZE%
11460 DEF FN_IS_TIME(sec%, prevSec%, targetSec%):= (sec% MOD targetSec% = 0 AND sec% <> prevSec%)
11470 DEF FN_PAUSED(n):=NOT((TIME - TI) > n):REM Attempt To Determine If Game Should Throttle Down
11480 DEF FN_HASH(x%, y%):=x% + y% * CW%:REM (x% - UX%) + (y% - UY%) * (LX% - UX%)
11490 DEF FN_RND_PCT(n%):=RND(1) > (n% / 100):REM Returns TRUE or FALSE
11500 DEF FN_RND_INT(lo%, hi%):=INT(RND(1) * (hi% - lo% + 1)) + lo%
11510 DEF FN_RND_EDIBLE:LOCAL r%:r% = FN_RND_INT(1, 4):= (r% < 4)*-(E_HEART + r% - 1) + (r% = 4)*-E_TOADSTOOL
11520 DEF FN_RND_X:=FN_RND_INT(UX%, LX%):REM 1 - 38
11530 DEF FN_RND_Y:=FN_RND_INT(UY%, LY%):REM 1 - 23
11540 DEF FN_XYINDEX(x%, y%):= y% * CW% + x%
11550 DEF FN_getByteVDP(var%):A% = &A0:L% = var%:=USR(&FFF4)
11560 DEF FN_getWordVDP(var%):=FN_getByteVDP(var%)+256*FN_getByteVDP(var%+1)
11570 DEF FN_ASCIIKEYCODE:=FN_getByteVDP(&05)
11580 :
11590 REM ::::::::::::::::::
11600 REM ::  Frame Wait  ::
11610 REM ::::::::::::::::::
11620 DEF PROC_WAIT(frames%)
11630 LOCAL i%
11640 FOR i% = 1 TO frames%
11650   *FX 19
11660 NEXT i%
11670 ENDPROC
11680 :
11690 REM ::::::::::::::::::::::
11700 REM ::   To Uppercase   ::
11710 REM ::::::::::::::::::::::
11720 DEF FN_TO_UPPER(ch$):LOCAL ch%:ch% = ASC(ch$):ch$ = CHR$(ch% + 32 * (ch% >= 97 AND ch% <= 122)):=ch$
11730 :
11740 REM :::::::::::::::::::::::::
11750 REM :: Prompt For Response ::
11760 REM :::::::::::::::::::::::::
11770 DEF FN_PROMPT(x%, y%, text$, default$)
11780 LOCAL r$
11790 PRINT TAB(x%, y%)text$;" ";default$:PRINT TAB(x% + LEN(text$) + 1, y%);
11800 r$ = GET$:r$ = FN_TO_UPPER(r$):IF r$ = CHR$(13) THEN r$ = default$
11810 = r$
11820 :
11830 REM :::::::::::::::::::::::::::::
11840 REM ::  Display Centered Text  ::
11850 REM :::::::::::::::::::::::::::::
11860 DEF PROC_CENTER(text$)
11870 LOCAL i%, n%, l%
11880 l% = 0
11890 FOR i% = 1 TO LEN(text$)
11900   IF ASC(MID$(text$, i%, 1)) >= 32 THEN l% = l% + 1
11910 NEXT i%
11920 n% = FN_CENTER(STRING$(l%, " "))
11930 i% = VPOS:VDU 31, n%, i%
11940 FOR i% = 1 TO LEN(text$)
11950   VDU ASC(MID$(text$, i%, 1))
11960 NEXT i%
11970 ENDPROC
11980 :
11990 REM ::::::::::::::::::::::::::::
12000 REM :: Restore Default Colors ::
12010 REM ::::::::::::::::::::::::::::
12020 DEF PROC_DEFAULT_COLORS
12030 COLOUR 128+BLACK:COLOUR WHITE
12040 ENDPROC
12050 :
12060 REM :::::::::::::::::::::::::
12070 REM ::  Update High Score  ::
12080 REM :::::::::::::::::::::::::
12090 DEF PROC_UPDATE_HIGH_SCORE
12100 IF (HighScore% < Score%) THEN HighScore% = Score%:REM Check if new highscore has been achieved and update if needed
12110 ENDPROC
12120 :
12130 REM :::::::::::::::::::::::::
12140 REM ::   High Score Read   ::
12150 REM :::::::::::::::::::::::::
12160 DEF PROC_HISCORE_READ(game$)
12170 LOCAL f0, status%, val%
12180 status% = 0
12190 f0% = OPENIN(game$ + ".HI")
12200 IF f0% = 0 THEN status% = -1:GOTO 12220
12210 INPUT#f0%, val%
12220 CLOSE#f0%
12230 IF status% = 0 THEN HighScore% = val%
12240 ENDPROC
12250 :
12260 REM :::::::::::::::::::::::::
12270 REM ::   High Score Write  ::
12280 REM :::::::::::::::::::::::::
12290 DEF PROC_HISCORE_WRITE(game$)
12300 LOCAL f0
12310 f0% = OPENOUT(game$ + ".HI")
12320 PRINT#f0%, HighScore%
12330 CLOSE#f0%
12340 ENDPROC
12350 :
12360 REM :::::::::::::::::::::::::::
12370 REM :: Empty Keyboard Buffer ::
12380 REM :::::::::::::::::::::::::::
12390 DEF PROC_EMPTY_KEYBOARD_BUFFER
12400 REPEAT UNTIL INKEY(0) = -1
12410 ENDPROC
12420 :
12430 REM ::::::::::::::::::::::::::::::::::::::
12440 REM :: Custom "Screen Memory" Functions ::
12450 REM ::::::::::::::::::::::::::::::::::::::
12460 DEF FN_READ(x%, y%)
12470 LOCAL n%
12480 n% = FN_XYINDEX(x%, y%)
12490 := screen?n%
12500 :
12510 DEF PROC_WRITE(x%, y%, ch%)
12520 LOCAL n%
12530 n% = FN_XYINDEX(x%, y%)
12540 screen?n% = ch%
12550 ENDPROC
12560 :
12570 DEF PROC_CLEAR_SCREEN
12580 LOCAL n%, ub%
12590 ub% = CW% * CH% - 1
12600 FOR n% = 0 TO ub%
12610   screen?n% = BLANK
12620 NEXT n%
12630 ENDPROC
12640 :
12650 DEF PROC_PLOT(x%, y%, ch%, co%)
12660 PROC_WRITE(x%, y%, ch%)
12670 VDU 31, x%, y%
12680 IF co% < 0 THEN co% = FN_COLOR_MAP(ch%)
12690 VDU 17, co%, ch%
12700 ENDPROC
12710 :
12720 DEF PROC_DRAW(x%, y%, ch%, overwrite%)
12730 LOCAL f%
12740 f% = FN_READ(x%, y%):REM Is Position Currently Unoccupied?
12750 IF (f% = BLANK OR overwrite%) THEN PROC_PLOT(x%, y%, ch%, -1)
12760 ENDPROC
12770 :
12780 REM :::::::::::::::::::::::::
12790 REM ::  Map Char To Color  ::
12800 REM :::::::::::::::::::::::::
12810 DEF FN_COLOR_MAP(c%)
12820 LOCAL r%
12830 r% = (c% = E_HEART OR c% = MON_RED)*-RED + (c% = E_FRUIT OR c% = MON_GREEN)*-GREEN + ((c% >= SN_L AND c% <= SN_U) OR c% = SN_W OR c% = E_TOADSTOOL)*-YELLOW
12840 r% = r% + (c% = MON_BLUE OR c% = B_VERT OR c% = B_HORZ OR (c% >= B_UR AND c% <= B_DR))*-BLUE
12850 r% = r% + (c% = MON_PINK)*-MAGENTA + (c% = E_DIAMOND OR c% = MON_CYAN)*-CYAN + (c% = DOT OR c% = MON_WHITE OR c% = E_CIRC OR c% = E_DISC)*-WHITE
12860 := r%
12870 :
12880 REM ::::::::::::::::::::
12890 REM ::  Random Event  ::
12900 REM ::::::::::::::::::::
12910 DEF PROC_RANDOM_EVENT
12920 LOCAL c%, f, free%, i%, r, rx%, ry%
12930 REM IF FN_RND_PCT(90) THEN GOTO ENDPROC:REM No new obstacle
12940 rx% = FN_RND_X:ry% = FN_RND_Y:REM Determine random position
12950 free% = (FN_READ(rx%, ry%) = BLANK):IF NOT free% THEN 13030:REM Ensure the position is free
12960 r = RND(1)
12970 IF (r < .85) THEN 13000
12980 i% = FN_NEXT_MONSTER_SLOT
12990 IF (-1 < i%) THEN c% = ASC(MID$(MO$, i% + 1, 1)):PROC_MANAGE_MONSTER(rx%, ry%, c%, TRUE):f = 9.5:GOTO 13020:ELSE c% = E_DISC:GOTO 13010
13000 c% = (r >= 0 AND r < .5)*-DOT + (r >= .5 AND r < .7)*-E_CIRC + (r >= .7 AND r < .85)*-FN_RND_EDIBLE
13010 PROC_DRAW(rx%, ry%, c%, FALSE):f = 4.5
13020 PROC_SOUND(f, 2)
13030 ENDPROC
13040 :
13050 REM :::::::::::::::::
13060 REM ::   REM Eat   ::
13070 REM :::::::::::::::::
13080 DEF FN_EAT(x%, y%)
13090 LOCAL c%, n%, s%
13100 c% = FN_READ(x%, y%):REM PROC_COUT(STR$(x%)+" "+STR$(y%)+" "+STR$(c%)+"      ",0)
13110 n% = (c% = BLANK OR c% = BLANK_X)*0 + ((c% = SN_W AND NOT Rvs_Dir%) OR (c% >= MON_RED AND c% <= MON_GREEN) OR (c% = E_TOADSTOOL))*-1 + (c% = E_CIRC)*-2 + (c% = E_DISC)*-3
13120 n% = n% + (c% = DOT OR (c% >= E_HEART AND c% <= E_FRUIT))*-4 + (c% = MON_WHITE OR c% = MON_BLUE)*-5
13130 n% = n% + (c% = B_VERT OR c% = B_HORZ OR c% = B_UR OR c% = B_UL OR c% = B_DL OR c% = B_DR)*-6
13140 ON n% GOTO 13150,13160,13170,13190,13230,13260:ELSE 13270
13150 Dead% = TRUE:r% = FALSE:GOTO 13280:REM Collided with self, toadstool or deadly monster
13160 PROC_SHRINK_SNAKE(1):GOTO 13200:REM The open circle shrinks the snake
13170 PROC_UPDATE_MONSTER_STATE(TRUE, MON_BLUE):PROC_CHARGE:REM The filled circle makes existing monsters vulnerable
13180 GOTO 13200
13190 s% = 2 + (c% = DOT) + (c% = E_DISC)*2:PROC_INC_SIZE(s%):REM Edible increases size of snake
13200 Score% = Score% + (c% = DOT)*-(Size% * 5) + (c% = E_HEART)*-100 + (c% = E_DIAMOND)*-200 + (c% = E_FRUIT)*-500 + (c% = E_CIRC OR c% = E_DISC)*-25
13210 PROC_UPDATE_HIGH_SCORE:PROC_SCORES:REM Update score display
13220 GOTO 13270
13230 Score% = Score% + (c% = MON_BLUE)*-500 + (c% = MON_WHITE)*-1000:PROC_MANAGE_MONSTER(x%, y%, c%, FALSE):PROC_SHRINK_SNAKE(1):REM Eating cowardly monster shrinks the snake
13240 PROC_UPDATE_HIGH_SCORE:PROC_SCORES:REM Update score display
13250 GOTO 13270
13260 Dead% = TRUE:REM Collided with boundary
13270 IF (Dead% <> TRUE AND c% <> BLANK AND c% <> BLANK_X AND c% <> SN_W) THEN PROC_SOUND(16, 2)
13280 := (Dead%) + (NOT Dead%) * -c%
13290 :
13300 REM :::::::::::::::::::::::::::
13310 REM ::    Recoil The Snake   ::
13320 REM :::::::::::::::::::::::::::
13330 DEF FN_RECOIL_SNAKE
13340 LOCAL i%
13350 i% = P - Size%:REM Locate tail end of snake
13360 IF i% < 0 THEN i% = i% + MAX_SIZE%:REM Wrap around to the end
13370 PROC_ERASE(X(i%), Y(i%))
13380 := i%
13390 :
13400 REM ::::::::::::::::::::::::::
13410 REM ::   Shrink The Snake   ::
13420 REM ::::::::::::::::::::::::::
13430 DEF PROC_SHRINK_SNAKE(d%)
13440 LOCAL n%
13450 PROC_INC_SIZE(-d%)
13460 n% = FN_RECOIL_SNAKE
13470 ENDPROC
13480 :
13490 REM ::::::::::::::::::::::::::
13500 REM ::  Grow Out The Snake  ::
13510 REM ::::::::::::::::::::::::::
13520 DEF PROC_GROW_SNAKE(d%)
13530 LOCAL i%, ch%, dx%, dy%, nx%, ny%
13540 dx% = FN_X_DELTA(d%):dy% = FN_Y_DELTA(d%)
13550 nx% = X(P) + dx%:ny% = Y(P) + dy%
13560 ch% = FN_EAT(nx%, ny%)
13570 IF ch% < 0 THEN 13700
13580 IF nx% <= UX% THEN nx% = LX% - 1:REM Snake entered Left Portal; Exit Out Right Portal
13590 IF nx% >= LX% THEN nx% = UX% + 1:REM Snake entered Right Portal; Exit Out Left Portal
13600 IF ny% <= UY% THEN ny% = LY% - 1:REM Snake entered Top Portal; Exit Out Bottom Portal
13610 IF ny% >= LY% THEN ny% = UY% + 1:REM Snake entered Bottom Portal; Exit Out Top Portal
13620 P = FN_NEXT_POS(P)
13630 i% = FN_RECOIL_SNAKE
13640 X(P) = nx%:Y(P) = ny%
13650 i% = FN_NEXT_POS(i%):IF i% = P THEN 13670
13660 REPEAT:PROC_DRAW(X(i%), Y(i%), SN_W, TRUE):i% = FN_NEXT_POS(i%):UNTIL i% = P
13670 ch% = (AF <> 0) * -SN_W + (AF = 0) * -(d% + SN_L - 1):REM Which Animation Frame To Display For Snake's Head
13680 PROC_DRAW(X(P), Y(P), ch%, TRUE)
13690 AF = (AF + 1) MOD 2
13700 ENDPROC
13710 :
13720 REM ::::::::::::::::::::::::::::::::
13730 REM :: Increase The Size Of Snake ::
13740 REM ::::::::::::::::::::::::::::::::
13750 DEF PROC_INC_SIZE(n%)
13760 Size% = FN_MAX(FN_MIN(Size% + n%, MAX_SIZE%), 2)
13770 ENDPROC
13780 :
13790 REM :::::::::::::::::::::::::::::::::::::
13800 REM :: Check For Reversal Of Direction ::
13810 REM :::::::::::::::::::::::::::::::::::::
13820 DEF FN_CHECK_FOR_REVERSE_DIRECTION(old%, new%)
13830 REM 4 = UP; 3= DOWN; 1 = LEFT; 2 = RIGHT
13840 REM := (old% = 4 AND new% = 3) OR (old% = 3 AND new% = 4) OR (old% = 1 AND new% = 2) OR (old% = 2 AND new% = 1)
13850 := (old% OR new%) = 7 OR (old% OR new%) = 3
13860 :
13870 REM :::::::::::::::::::::::::::::::::
13880 REM :: Monster Management Routines ::
13890 REM :::::::::::::::::::::::::::::::::
13900 DEF PROC_MANAGE_MONSTER(x%, y%, c%, state%)
13910 LOCAL pos%:REM PROC_COUT(STR$(x%)+","+STR$(y%)+" "+STR$(c%)+"  ", 2)
13920 IF state% = FALSE THEN PROC_CLEAR_MONSTER(x%, y%)
13930 IF state% = TRUE THEN PROC_NEW_MONSTER(x%, y%, c%):PROC_DRAW(x%, y%, c%, TRUE)
13940 IF state% = MON_BLUE OR state% = MON_WHITE THEN PROC_DRAW(x%, y%, state%, TRUE):IF state% = MON_BLUE THEN PROC_MON_GO_WHITE
13950 IF state% = MON_RESET THEN PROC_DRAW(x%, y%, c%, TRUE)
13960 ENDPROC
13970 :
13980 DEF PROC_MANAGE_MONSTER_BY_POS(pos%, c%, state%)
13990 LOCAL x%, y%
14000 IF -1 <> pos% THEN y% = pos% DIV CW%:x% = pos% MOD CW%:PROC_MANAGE_MONSTER(x%, y%, c%, state%)
14010 ENDPROC
14020 :
14030 DEF PROC_NEW_MONSTER(x%, y%, c%)
14040 LOCAL i%
14050 IF M_Count% >= MAX_MONSTERS% THEN 14080
14060 i% = FN_NEXT_MONSTER_SLOT
14070 IF -1 <> i% THEN MP%(i%) = FN_HASH(x%, y%):M_Position% = i%:M_Count% = M_Count% + 1
14080 ENDPROC
14090 :
14100 DEF FN_NEXT_MONSTER_SLOT
14110 LOCAL found%, i%, r%
14120 found% = FALSE:i% = 0
14130 REPEAT
14140   IF -1 = MP%(i%) THEN found% = TRUE:ELSE i% = i% + 1
14150 UNTIL found% OR i% = MAX_MONSTERS%
14160 IF found% THEN r% = i%:ELSE r% = -1
14170 := r%
14180 :
14190 DEF FN_FIND_MONSTER(x%, y%)
14200 LOCAL found%, i%, r%
14210 found% = FALSE:i% = 0
14220 REPEAT
14230   IF (FN_HASH(x%, y%) = MP%(i%)) THEN found% = TRUE:ELSE i% = i% + 1
14240 UNTIL found% OR i% = MAX_MONSTERS%
14250 IF found% THEN r% = i%:ELSE r% = -1
14260 := r%
14270 :
14280 DEF FN_MONSTER_AT_POS(pos%)
14290 LOCAL r%, x%, y%
14300 r% = -1:IF -1 <> pos% THEN y% = pos% DIV CW%:x% = pos% MOD CW%:r% = FN_READ(x%, y%)
14310 := r%
14320 :
14330 DEF PROC_CLEAR_MONSTER(x%, y%)
14340 LOCAL i%
14350 i% = FN_FIND_MONSTER(x%, y%)
14360 IF -1 <> i% THEN MP%(i%) = -1:M_Count% = M_Count% - 1
14370 ENDPROC
14380 :
14390 DEF PROC_UPDATE_MONSTER_STATE(oldState%, newState%)
14400 LOCAL c%, i%
14410 FOR i% = 0 TO MAX_MONSTERS% - 1
14420   c% = (newState% = MON_RESET) * -ASC(MID$(MO$, i%+1, 1)) + (newState% = MON_WHITE AND oldState% = MON_BLUE) * -FN_MONSTER_AT_POS(MP%(i%))
14430   IF -1 <> MP%(i%) AND (oldState% = c% OR oldState% = TRUE) THEN PROC_MANAGE_MONSTER_BY_POS(MP%(i%), c%, newState%)
14440 NEXT i%
14450 ENDPROC
14460 :
14470 DEF PROC_MONSTER_COLOR_CHECK
14480 IF MonGoWhite% <> FALSE AND FN_INT_TIME >= MonGoWhite% THEN MonGoWhite% = FALSE:PROC_UPDATE_MONSTER_STATE(MON_BLUE, MON_WHITE)
14490 IF MonReset% <> FALSE AND FN_INT_TIME >= MonReset% THEN MonReset% = FALSE:PROC_UPDATE_MONSTER_STATE(TRUE, MON_RESET)
14500 ENDPROC
14510 :
14520 REM :::::::::::::::::::::::::::::::::::::::::::
14530 REM ::  Calculate type index of a clockwise  ::
14540 REM ::  position on a box's perimeter        ::
14550 REM :::::::::::::::::::::::::::::::::::::::::::
14560 DEF FN_CLOCKWISE_BOX_SIDE_INDEX(pos%, width%, height%)
14570 REM 0 = UPPER_LEFT_CORNER, 1 = UPPER_MIDDLE, 2 = UPPER_RIGHT_CORNER, 3 = MIDDLE_RIGHT, 4 = LOWER_RIGHT_CORNER, 5 = LOWER_MIDDLE, 6 = LOWER_LEFT_CORNER, 7 = MIDDLE_LEFT
14580 LOCAL r%
14590 r% = (pos% > 0 AND pos% < width% - 1) * -1 + (pos% = width% - 1) * -2 + (pos% >= width% AND pos% < width% + height% - 2) * -3 + (pos% = width% + height% - 2) * -4
14600 r% = r% + (pos% > width% + height% - 2 AND pos% < 2 * width% + height% - 3) * -5 + (pos% = 2 * width% + height% - 3) * -6 + (pos% > 2 * width% + height% - 3) * -7
14610 :=r%
14620 :
14630 REM ::::::::::::::::::::::::::
14640 REM ::  Draw Box Clockwise  ::
14650 REM ::::::::::::::::::::::::::
14660 DEF PROC_CLOCKWISE_BOX(ux%, uy%, width%, height%, color%)
14670 LOCAL aq%, bq%, ch%, i%, p%, x%, y%
14680 aq% = width% + height% - 2:bq% = aq% + width%:p% = bq% + height% - 2
14690 FOR i% = 0 TO p% - 1
14700   x% = (i% < width%) * -i% + (i% > (width%-1) AND i% < aq%) * -(width%-1) + (i% >= aq% AND i% < bq%) * (i% - (bq% - 1)) + (i% >= bq%) * 0
14710   y% = (i% < width%) * 0 + (i% > (width%-1) AND i% < aq%) * -(i% - (width%-1)) + (i% >= aq% AND i% < bq%) * -(height%-1) + (i% >= bq%) * -((height%-2) - (i% - bq%))
14720   ch% = ASC(MID$(BX$, FN_CLOCKWISE_BOX_SIDE_INDEX(i%, width%, height%) + 1, 1))
14730   PROC_PLOT(ux% + x%, uy% + y%, ch%, color%)
14740 NEXT i%
14750 ENDPROC
14760 :
14770 REM :::::::::::::::::::::::::
14780 REM ::  Draw Playing Field ::
14790 REM :::::::::::::::::::::::::
14800 DEF PROC_DRAW_PLAYING_FIELD(ux%, uy%, width%, height%)
14810 PROC_CLOCKWISE_BOX(ux%, uy%, width%, height%, BLUE)
14820 ENDPROC
14830 :
14840 REM :::::::::::::::::::
14850 REM :: Draw Portals  ::
14860 REM :::::::::::::::::::
14870 DEF PROC_DRAW_PORTALS(horizontal%, vertical%, ux%, uy%, width%, height%)
14880 LOCAL ch%, h%, i%, j%, lx%, ly%, pColor%, v%, wColor%
14890 pColor% = CYAN:wColor% = BLUE:h% = ux% + width% DIV 2 - 2:v% = uy% + height% DIV 2 - 2:lx% = ux% + width% - 1:ly% = uy% + height% - 1
14900 FOR i% = 0 TO 1:REM Vertical portal
14910   FOR j% = 0 TO 2
14920     ch% = (vertical%) * -(ASC(MID$(PV$(i%), j% + 1, 1))) + (NOT vertical%) * -B_HORZ
14930     PROC_PLOT(h% + j%, (i% = 0) * -uy% + (i% = 1) * -ly%, ch%, (ch% = BLANK_X) * -BLACK + (ch% = B_HORZ) * -wColor% + ((ch% <> BLANK_X) AND (ch% <> B_HORZ)) * -pColor%)
14940   NEXT j%
14950 NEXT i%
14960 FOR i% = 0 TO 1:REM Horizontal portal
14970   FOR j% = 0 TO 2
14980     ch% = (horizontal%) * -(ASC(MID$(PH$(i%), j% + 1, 1))) + (NOT horizontal%) * -B_VERT
14990     PROC_PLOT((i% = 0) * -ux% + (i% = 1) * -lx%, v% + j%, ch%, (ch% = BLANK_X) * -BLACK + (ch% = B_VERT) * -wColor% + ((ch% <> BLANK_X) AND (ch% <> B_VERT)) * -pColor%)
15000   NEXT j%
15010 NEXT i%
15020 ENDPROC
15030 :
15040 REM :::::::::::::::::::::::::::
15050 REM ::  Update Portal State  ::
15060 REM :::::::::::::::::::::::::::
15070 DEF PROC_UPDATE_PORTAL_STATE
15080 LOCAL horizontal%, vertical%
15090 Portal_State% = (Portal_State% + 1) MOD 4
15100 IF Portal_State% = 0 THEN horizontal% = FALSE: vertical% = FALSE
15110 IF Portal_State% = 1 THEN horizontal% = TRUE: vertical% = FALSE
15120 IF Portal_State% = 2 THEN horizontal% = TRUE: vertical% = TRUE
15130 IF Portal_State% = 3 THEN horizontal% = FALSE: vertical% = TRUE
15140 PROC_DRAW_PORTALS(horizontal%, vertical%, UX%, UY%, CW% - 2*UX%, CH% - UY%)
15150 FOR i% = 1 TO 2:PROC_SOUND(i% * 24, 1.5 * i%):NEXT i%
15160 ENDPROC
15170 :
15180 REM ::::::::::::::::::::::::::::::::
15190 REM ::       Clockwise Plot       ::
15200 REM ::::::::::::::::::::::::::::::::
15210 DEF PROC_CLOCKWISE_PLOT(pos%, color%, char%, ux%, uy%, width%, height%)
15220 LOCAL cx%, cy%, a%, b%, c%
15230 a% = width% + height% - 2:b% = a% + width%:c% = b% + height% - 2
15240 cx% = (pos% < width%) * -pos% + (pos% > (width% - 1) AND pos% < a%) * -(width% - 1)
15250 cx% = cx% + (pos% >= a% AND pos% < b%) * (pos% - (b% - 1)) + (pos% >= b%) * 0
15260 cy% = (pos% < width%) * 0 + (pos% > (width% - 1) AND pos% < a%) * -(pos% - (width% - 1))
15270 cy% = cy% + (pos% >= a% AND pos% < b%) * -(height% - 1) + (pos% >= b%) * -((height% - 2) - (pos% - b%))
15280 VDU 31,ux% + cx%,uy% + cy%,17,color%,char%:REM Plot a character on the path
15290 ENDPROC
15300 :
15310 REM :::::::::::::::::::::::
15320 REM ::  Death Animation  ::
15330 REM :::::::::::::::::::::::
15340 DEF PROC_DEATH_ANIMATION(x%, y%)
15350 LOCAL ch%, fr$, i%, n%
15360 REPEAT:Size% = Size% - 1:n% = FN_RECOIL_SNAKE:PROC_SOUND(2 * Size%, 2):PROC_SOUND(0, 0):PROC_SLEEP(10):UNTIL Size% < 2
15370 fr$ = RIGHT$("0"+STR$(SN_W), 3)+STR$(SN_U)+STR$(SN_D1)+STR$(SN_D2)+STR$(SN_D3)+STR$(SN_D4)+STR$(SN_D5)+STR$(SN_D6)+"0"+STR$(BLANK)
15380 FOR i% = 1 TO LEN(fr$) DIV 3 STEP 2
15390   ch% = VAL(MID$(fr$, 3 * (i% - 1) + 1, 3))
15400   VDU 31, x%, y%, 17, YELLOW, ch%:PROC_SOUND(i% + 8, 2):PROC_SLEEP(20)
15410 NEXT i%:PROC_SOUND(4, 3)
15420 ENDPROC
15430 :
15440 REM :::::::::::::::::::
15450 REM ::    Welcome    ::
15460 REM :::::::::::::::::::
15470 DEF PROC_WELCOME
15480 LOCAL boxh%, boxw%, c%, cc%, ch$, ex%, perimeter%, t%, t$, ux%, uy%
15490 boxh% = 18:boxw% = 38:cc% = 0:ex% = FALSE:perimeter% = 2 * (boxw% + boxh% - 2):t% = 2:ux% = (CW% - boxw%) DIV 2:uy% = 0
15500 CLS:PROC_HIDE_CURSOR
15510 PRINT TAB(0, uy% + 1);
15520 PROC_CENTER(" Welcome to " + CHR$(17)+CHR$(YELLOW) + GameName$ + CHR$(17)+CHR$(WHITE)+ "..."):PRINT:PRINT
15530 PROC_CENTER(" A nostalgic variant of the classic"):PRINT
15540 PROC_CENTER(" SNAKE game."):PRINT:PRINT
15550 PROC_CENTER(" Use the four arrow keys to maneuver"):PRINT
15560 PROC_CENTER(" the starving little snake to snack"):PRINT
15570 PROC_CENTER(" on pellets and other tasty morsels."):PRINT
15580 PROC_CENTER(" Avoid the walls and spooky little"):PRINT
15590 PROC_CENTER(" monsters while trying to avoid"):PRINT
15600 PROC_CENTER(" chomping on yourself like an"):PRINT
15610 COLOUR YELLOW:PROC_CENTER(" Ouroboros."):PRINT:PRINT
15620 COLOUR WHITE:PROC_CENTER(" Good luck and have fun!"):PRINT:PRINT
15630 COLOUR CYAN:PROC_CENTER("Hit a key to continue")
15640 REPEAT
15650   PROC_CLOCKWISE_PLOT(cc%, BLACK, BLANK, ux%, uy%, boxw%, boxh%)
15660   cc% = (cc% + 1) MOD perimeter%
15670   PROC_CLOCKWISE_PLOT(cc%, cc% MOD 6 + 1, MON_RED, ux%, uy%, boxw%, boxh%)
15680   IF SY$ = "A" THEN c% = INKEY(DL%):PROC_EMPTY_KEYBOARD_BUFFER:ELSE c% = INKEY(TK/DL%)
15690   IF c% > 0 THEN ex% = TRUE
15700 UNTIL ex%
15710 boxh% = 18:boxw% = 38:cc% = 0:ex% = FALSE:perimeter% = 2 * (boxw% + boxh% - 2):ux% = (CW% - boxw%) DIV 2:uy% = 0
15720 CLS:PROC_DEFAULT_COLORS
15730 PRINT TAB(0, uy% + 2);
15740 COLOUR YELLOW:PROC_CENTER(STRING$(t%, " ")+"....  Score"+STRING$(2, " ")+"Resize"):PRINT:PRINT
15750 t$ = STRING$(t%, " ")+CHR$(17)+CHR$(RED)+CHR$(MON_RED)+CHR$(17)+CHR$(MAGENTA)+CHR$(MON_RED)+CHR$(17)+CHR$(CYAN)+CHR$(MON_RED)+CHR$(17)+CHR$(GREEN)+CHR$(MON_RED)
15760 t$ = t$ + STRING$(2, " ")+CHR$(17)+CHR$(WHITE)+"Death"+STRING$(2, " ")+CHR$(SKULL)+STRING$(6, " "):PROC_CENTER(t$):PRINT
15770 PROC_CENTER(STRING$(t%, " ")+CHR$(17)+CHR$(BLUE)+CHR$(MON_RED)+STRING$(5, " ")+CHR$(17)+CHR$(WHITE)+"  500"+STRING$(2, " ")+"-"+CHR$(17)+CHR$(YELLOW)+CHR$(SN_W)+STRING$(4, " ")):PRINT
15780 PROC_CENTER(STRING$(t%, " ")+CHR$(17)+CHR$(WHITE)+CHR$(MON_RED)+STRING$(5, " ")+" 1000"+STRING$(2, " ")+"-"+CHR$(17)+CHR$(YELLOW)+CHR$(SN_W)+STRING$(4, " ")):PRINT
15790 PROC_CENTER(STRING$(t%, " ")+CHR$(17)+CHR$(WHITE)+CHR$(DOT)+STRING$(5, " ")+"  5x"+CHR$(17)+CHR$(YELLOW)+CHR$(SN_W)+STRING$(2, " ")+CHR$(17)+CHR$(WHITE)+"+"+CHR$(17)+CHR$(YELLOW)+CHR$(SN_W)+STRING$(4, " ")):PRINT
15800 PROC_CENTER(STRING$(t%, " ")+CHR$(17)+CHR$(WHITE)+CHR$(E_CIRC)+STRING$(5, " ")+"   25"+STRING$(2, " ")+"-"+CHR$(17)+CHR$(YELLOW)+CHR$(SN_W)+STRING$(4, " ")):PRINT
15810 PROC_CENTER(STRING$(t%, " ")+CHR$(17)+CHR$(WHITE)+CHR$(E_DISC)+STRING$(5, " ")+"   25"+STRING$(2, " ")+STRING$(6, " ")):PRINT
15820 PROC_CENTER(STRING$(t%, " ")+CHR$(17)+CHR$(RED)+CHR$(E_HEART)+STRING$(5, " ")+CHR$(17)+CHR$(WHITE)+"  100"+STRING$(2, " ")+"+"+CHR$(17)+CHR$(YELLOW)+CHR$(SN_W)+CHR$(SN_W)+STRING$(3, " ")):PRINT
15830 PROC_CENTER(STRING$(t%, " ")+CHR$(17)+CHR$(CYAN)+CHR$(E_DIAMOND)+STRING$(5, " ")+CHR$(17)+CHR$(WHITE)+"  200"+STRING$(2, " ")+"+"+CHR$(17)+CHR$(YELLOW)+CHR$(SN_W)+CHR$(SN_W)+STRING$(3, " ")):PRINT
15840 PROC_CENTER(STRING$(t%, " ")+CHR$(17)+CHR$(GREEN)+CHR$(E_FRUIT)+STRING$(5, " ")+CHR$(17)+CHR$(WHITE)+"  500"+STRING$(2, " ")+"+"+CHR$(17)+CHR$(YELLOW)+CHR$(SN_W)+CHR$(SN_W)+STRING$(3, " ")):PRINT
15850 PROC_CENTER(STRING$(t%, " ")+CHR$(17)+CHR$(YELLOW)+CHR$(E_TOADSTOOL)+STRING$(5, " ")+CHR$(17)+CHR$(WHITE)+"Death"+STRING$(2, " ")+CHR$(SKULL)+STRING$(6, " ")):PRINT:PRINT
15860 COLOUR GREEN:PROC_CENTER("Hit a key to begin playing")
15870 PROC_CLOCKWISE_BOX(ux% + 1, uy% + 1, boxw% - 2, boxh% - 2, CYAN)
15880 ch$=CHR$(SN_R)+CHR$(SN_R)+CHR$(SN_D)+CHR$(SN_D)+CHR$(SN_L)+CHR$(SN_L)+CHR$(SN_U)+CHR$(SN_U)
15890 REPEAT
15900   PROC_CLOCKWISE_PLOT(cc% - 1, BLACK, BLANK, ux%, uy%, boxw%, boxh%):PROC_CLOCKWISE_PLOT(cc%, BLACK, BLANK, ux%, uy%, boxw%, boxh%)
15910   cc% = (cc% + 1) MOD perimeter%
15920   c% = (cc% MOD 2 <> 0) * -SN_W + (cc% MOD 2 = 0) * -ASC(MID$(ch$, FN_CLOCKWISE_BOX_SIDE_INDEX(cc%, boxw%, boxh%) + 1, 1))
15930   PROC_CLOCKWISE_PLOT(cc% - 1, YELLOW, SN_W, ux%, uy%, boxw%, boxh%):PROC_CLOCKWISE_PLOT(cc%, YELLOW, c%, ux%, uy%, boxw%, boxh%)
15940   IF SY$ = "A" THEN c% = INKEY(DL%):PROC_EMPTY_KEYBOARD_BUFFER:ELSE c% = INKEY(TK/DL%)
15950   IF c% > 0 THEN ex% = TRUE
15960 UNTIL ex%
15970 PROC_DEFAULT_COLORS
15980 ENDPROC
15990 :
16000 REM :::::::::::::::::::::::
16010 REM :: Play Simple Sound ::
16020 REM :::::::::::::::::::::::
16030 DEF PROC_SOUND(index%, duration%)
16040 LOCAL constant%:constant% = 12.2
16050 SOUND 1, -10, index% * constant%, duration%
16060 ENDPROC
16070 :
16080 REM :::::::::::::::::::
16090 REM ::  CHARGE!!!!!  ::
16100 REM :::::::::::::::::::
16110 REM COUNT,PITCH,DURATION
16120 DEF PROC_CHARGE
16130 LOCAL d%, j%, l%, p%, sng$
16140 sng$ = "129001149001165001177004165002177008":l% = LEN(sng$) DIV 3
16150 FOR j% = 1 TO l% STEP 2
16160   p% = VAL(MID$(sng$, 3 * (j% - 1) + 1, 3)):d% = VAL(MID$(sng$, 3 * (j% - 1) + 4, 3))
16170   SOUND 1, -10, p%, d%
16180   SOUND 1, 0, p%, 1:REM Stacatto the currently playing sound
16190 NEXT j%
16200 ENDPROC
16210 :
16220 REM ::::::::::::::::::::::::::::::
16230 REM :: Define Custom Characters ::
16240 REM ::::::::::::::::::::::::::::::
16250 DEF PROC_REDEFINE_CHARS
16260 VDU 23,BLANK_X,0,0,0,0,0,0,0,0:REM BLANK
16270 VDU 23,DOT,0,0,0,24,24,0,0,0:REM DOT
16280 VDU 23,SN_L,0,60,30,14,14,30,60,0:REM LEFT(3)
16290 VDU 23,SN_R,0,60,120,112,112,120,60,0:REM RIGHT(3)
16300 VDU 23,SN_D,0,60,126,126,102,66,0,0:REM DOWN(3)
16310 VDU 23,SN_U,0,0,66,102,126,126,60,0:REM UP(3)
16320 VDU 23,SN_W,0,60,126,126,126,126,60,0:REM WHOLE(3)
16330 VDU 23,E_HEART,54,127,127,127,62,28,8,0:REM HEART(1)
16340 VDU 23,E_DIAMOND,8,28,62,127,62,28,8,0:REM DIAMOND(6)
16350 VDU 23,E_FRUIT,0,12,24,60,60,60,24,0:REM FRUIT(2)
16360 VDU 23,E_TOADSTOOL,0,0,24,60,60,8,24,0:REM TOADSTOOL(3)
16370 VDU 23,E_CIRC,0,60,126,102,102,126,60,0:REM CIRCLE(7)
16380 VDU 23,E_DISC,0,60,126,126,126,126,60,0:REM FILLED CIRCLE(7)
16390 VDU 23,MON_WHITE,0,60,126,90,126,126,90,0:REM WHITE(7)
16400 VDU 23,MON_BLUE,0,60,126,90,126,126,90,0:REM BLUE(4)
16410 VDU 23,MON_RED,0,60,126,90,126,126,90,0:REM RED(1)
16420 VDU 23,MON_PINK,0,60,126,90,126,126,90,0:REM MAGENTA(5)
16430 VDU 23,MON_CYAN,0,60,126,90,126,126,90,0:REM CYAN(6)
16440 VDU 23,MON_GREEN,0,60,126,90,126,126,90,0:REM GREEN(2)
16450 VDU 23,B_VERT,24,24,24,24,24,24,24,24:REM VERTICAL(4)
16460 VDU 23,B_HORZ,0,0,0,255,255,0,0,0:REM HORIZONTAL(4)
16470 VDU 23,B_UR,0,0,0,7,15,28,24,24:REM UPRIGHT C(4)
16480 VDU 23,B_UL,0,0,0,224,240,56,24,24:REM UPLEFT C(4)
16490 VDU 23,B_DL,24,24,56,240,224,0,0,0:REM DOWNLEFT C(4)
16500 VDU 23,B_DR,24,24,28,15,7,0,0,0:REM DOWN RIGHT C(4)
16510 VDU 23,SN_D1,0,0,0,102,126,126,60,0:REM DYING 1
16520 VDU 23,SN_D2,0,0,0,0,126,126,60,0:REM DYING 2
16530 VDU 23,SN_D3,0,0,0,0,126,126,60,0:REM DYING 3
16540 VDU 23,SN_D4,0,0,0,0,24,60,60,0:REM DYING 4
16550 VDU 23,SN_D5,0,0,0,0,24,24,60,0:REM DYING 5
16560 VDU 23,SN_D6,0,0,0,0,8,24,16,0:REM DYING 6
16570 VDU 23,SKULL,0,189,126,90,126,165,24,0:REM SKULL(7)
16580 ENDPROC
16590 :
16600 REM ::::::::::::::::::::::::::::::
16610 REM ::  Error Handling Routine  ::
16620 REM ::::::::::::::::::::::::::::::
16630 DEF PROC_HANDLE_ERROR
16640 IF ERR = 17 AND Dead% = FALSE THEN ERR = 0:GOTO 16680
16650 PROC_DEFAULT_COLORS:PROC_SHOW_CURSOR
16660 REPORT
16670 PRINT" @line #";ERL:STOP
16680 ENDPROC
