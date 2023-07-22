10000 REM :::::::::::::::::::::::::::::::::::::::::::::
10010 REM :: SNAC-SNAKE FOR AgonLight (BBC BASIC v3) ::
10020 REM :::::::::::::::::::::::::::::::::::::::::::::
10030 REM :: 20230722: V1.2 - fixes and improvements ::
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
10140 IF SY$ = "B" THEN LEFT = 136:RIGHT = 137:DOWN = 138:UP = 139:DL% = 10:MO% = 9:ELSE LEFT = 8:RIGHT = 21:DOWN = 10:UP = 11:DL% = 14:MO% = 2
10150 IF SY$ = "A" THEN REPEAT CLS:MO$=FN_PROMPT(0,0,"MODE (1,2,3,...):",STR$(MO%)):UNTIL VAL(MO$) > 0:MO% = VAL(MO$)
10160 MODE MO%
10170 PROC_SETUP
10180 REM ON ERROR PROC_HANDLE_ERROR:REM Handle ESC key
10190 :
10200 PROC_WELCOME
10210 PROC_NEW_GAME
10220 PROC_MAIN_LOOP:REM Invoke main loop
10230 PROC_DEATH_ANIMATION(X(P), Y(P)):PROC_GAME_OVER
10240 Resp$ = FN_PLAY_AGAIN:IF Resp$ = "Y" THEN 10210:ELSE PROC_GOODBYE(GameName$)
10250 END
10260 :
10270 REM ::::::::::::::::::::
10280 REM ::   Setup Game   ::
10290 REM ::::::::::::::::::::
10300 DEF PROC_SETUP
10310 GameName$ = "Snac-Snake"
10320 BLACK = 0:RED = 1:GREEN = 2:YELLOW = 3:BLUE = 4:MAGENTA = 5:CYAN = 6:WHITE = 7:C_ORANGE = 8 + (SY$ = "A" AND FN_COLORCOUNT = &40) *-50
10330 BLANK = 32:DOT = 38:BLANK_X = 64:SKULL = 42
10340 MON_WHITE = 134:MON_BLUE = 135:MON_RED = 136:MON_PINK = 137:MON_CYAN = 138:MON_GREEN = 139:MON_RESET = ASC("M")
10350 E_HEART = 129:E_DIAMOND = 130:E_FRUIT = 131:E_CIRC = 132:E_DISC = 133:E_TOADSTOOL = 147
10360 B_VERT = 140:B_HORZ = 141:B_UR = 142:B_UL = 143:B_DL = 144:B_DR = 145
10370 SN_W = 128:SN_L = 123:SN_R = 124:SN_D = 125:SN_U = 126
10380 SN_D1 = 150:SN_D2 = 151:SN_D3 = 152:SN_D4 = 153:SN_D5 = 154:SN_D6 = 155
10390 MAXINT% = &3B9AC9FF
10400 IF SY$ = "A" THEN CW% = FN_getByteVDP(&13):CH% = FN_getByteVDP(&14)-2:ELSE CW% = 40:CH% = 23
10410 UX% = 0:UY% = 1:LX% = CW% - UX% - 1:LY% = CH% - UY%:REM Playing Field Boundaries
10420 UB% = (LX% - UX% + 1) + (LY% - UY% + 2) * LX%
10430 HighScore% = 1500:REM Set minimum high score
10440 MAX_SIZE% = (CW% + CH% - 2) DIV 2:REM Maximum Size of Snake
10450 MAX_MONSTER_SETS% = CW% DIV 40:REM Maximum number of 4-monster sets
10460 MAX_MONSTERS% = MAX_MONSTER_SETS% * 4:REM Maximum number of monsters that can appear at one time
10470 TK = TIME:PROC_SLEEP(100):TK = TIME - TK:REM CALIBRATE TIME TICKS
10480 SP% = DL% * TK / 100:REM Speed Throttler (smaller value speeds up the game)
10490 DIM PV$(1),PH$(1)
10500 PH$(0)=CHR$(B_DL)+CHR$(BLANK_X)+CHR$(B_UL):PH$(1)=CHR$(B_DR)+CHR$(BLANK_X)+CHR$(B_UR)
10510 PV$(0)=CHR$(B_DL)+CHR$(BLANK_X)+CHR$(B_DR):PV$(1)=CHR$(B_UL)+CHR$(BLANK_X)+CHR$(B_UR)
10520 MO$=STRING$(MAX_MONSTER_SETS%,CHR$(MON_RED)+CHR$(MON_PINK)+CHR$(MON_CYAN)+CHR$(MON_GREEN))
10530 BX$=CHR$(B_UR) + CHR$(B_HORZ) + CHR$(B_UL) + CHR$(B_VERT) + CHR$(B_DL) + CHR$(B_HORZ) + CHR$(B_DR) + CHR$(B_VERT)
10540 DIM X(MAX_SIZE%-1),Y(MAX_SIZE%-1),MP%(MAX_MONSTERS% - 1),screen CW% * CH%
10550 PROC_REDEFINE_COLORS
10560 PROC_REDEFINE_CHARS
10570 PROC_HISCORE_READ(GameName$)
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
10830 PROC_DISPLAY_SCORES
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
11010 FOR i% = 0 TO FN_CENTER(game$) - 1:PRINTTAB(0, CH% DIV 2 + 2)STRING$(i%, " ")CHR$(17)CHR$(i% MOD 7 + 1)game$:PROC_SLEEP(20):NEXT i%
11020 PROC_DEFAULT_COLORS
11030 PROC_SHOW_CURSOR
11040 ENDPROC
11050 :
11060 REM ::::::::::::::::::::::
11070 REM ::     Main Loop    ::
11080 REM ::::::::::::::::::::::
11090 DEF PROC_MAIN_LOOP
11100 LOCAL dd%,prevsec%,sec%, tt%
11110 sec% = -1
11120 REPEAT
11130   TI = FN_INT_TIME
11140   prevsec% = sec%:sec% = FN_INT_TIME DIV TK
11150   PROC_MONSTER_COLOR_CHECK
11160   IF FN_IS_TIME(sec%, prevsec%, 30) THEN PROC_UPDATE_PORTAL_STATE
11170   IF SY$ <> "A" THEN dd% = INKEY(INT(TK / 100 * DL%)):PROC_EMPTY_KEYBOARD_BUFFER:ELSE dd% = FN_ASCII_KEYCODE
11180   dd% = FN_MAP_INPUT(dd%):IF dd% > 0 THEN Rvs_Dir% = FN_CHECK_FOR_REVERSE_DIRECTION(D%, dd%):D% = dd%
11190   PROC_GROW_SNAKE(D%)
11200   IF FN_IS_TIME(sec%, prevsec%, 2.5) THEN PROC_RANDOM_EVENT
11210   PROC_DISPLAY_SCORES:REM Update score display
11220   tt% = FN_INT_TIME - TI:IF SY$ = "A" THEN PROC_SLEEP(TI + SP% - FN_INT_TIME):REM Throttle if necessary// (tt% < SP%) * -(SP% - tt%)
11230 UNTIL Dead%
11240 ENDPROC
11250 :
11260 REM ::::::::::::::::::::::::::::
11270 REM :: Miscellaneous Routines ::
11280 REM ::::::::::::::::::::::::::::
11290 DEF PROC_COUT(text$, row%):VDU 31,0,CH%+row%,17,WHITE:PRINT text$:ENDPROC
11300 DEF PROC_ERASE(x%, y%):PROC_PLOT(x%, y%, BLANK, BLACK):ENDPROC
11310 DEF PROC_MON_GO_WHITE:MonGoWhite% = FN_INT_TIME + (TK+DL%)*3:MonReset% = (MonGoWhite% + (TK+DL%)*2) MOD MAXINT%:ENDPROC
11320 DEF FN_X_DELTA(d%):=(d% = 1) + (d% = 2)*-1 + (d% = 3)*0 + (d% = 4)*0
11330 DEF FN_Y_DELTA(d%):=(d% = 1)*0 + (d% = 2)*0 + (d% = 3)*-1 + (d% = 4)
11340 DEF FN_INT_TIME:=TIME MOD MAXINT%
11350 DEF FN_MAP_INPUT(n%):=(n% = LEFT)*-1 + (n% = RIGHT)*-2 + (n% = DOWN)*-3 + (n% = UP)*-4
11360 DEF FN_NEXT_POS(i%):=(i% + 1) MOD MAX_SIZE%
11370 DEF FN_PAUSED(n):=NOT((TIME - TI) > n):REM Attempt To Determine If Game Should Throttle Down
11380 DEF FN_HASH(x%, y%):=x% + y% * CW%:REM (x% - UX%) + (y% - UY%) * (LX% - UX%)
11390 DEF FN_RND_EDIBLE:LOCAL r%:r% = FN_RND_INT(1, 4):= (r% < 4)*-(E_HEART + r% - 1) + (r% = 4)*-E_TOADSTOOL
11400 DEF FN_RND_X:=FN_RND_INT(UX%, LX%):REM 1 - 38
11410 DEF FN_RND_Y:=FN_RND_INT(UY%, LY%):REM 1 - 23
11420 DEF FN_XYINDEX(x%, y%):= y% * CW% + x%
11430 :
11440 REM :::::::::::::::::::::::::::::::::
11450 REM :: Random Integer Within Range ::
11460 REM :::::::::::::::::::::::::::::::::
11470 DEF FN_RND_INT(lo%, hi%):= (RND(1) * (hi% - lo% + 1)) + lo%
11480 :
11490 REM ::::::::::::::::::::::::::::::::
11500 REM :: Return TRUE when random    ::
11510 REM :: value is below given value ::
11520 REM ::::::::::::::::::::::::::::::::
11530 DEF FN_RND_PCT(n%):=RND(1) <= (n% / 100):REM Returns TRUE or FALSE
11540 :
11550 REM ::::::::::::::::::
11560 REM ::  Frame Wait  ::
11570 REM ::::::::::::::::::
11580 DEF PROC_WAIT(frames%)
11590 LOCAL i%
11600 FOR i% = 1 TO frames%
11610   *FX 19
11620 NEXT i%
11630 ENDPROC
11640 :
11650 REM ::::::::::::::::::::::
11660 REM ::   To Uppercase   ::
11670 REM ::::::::::::::::::::::
11680 DEF FN_TO_UPPER(ch$):LOCAL ch%:ch% = ASC(ch$):ch$ = CHR$(ch% + 32 * (ch% >= 97 AND ch% <= 122)):=ch$
11690 :
11700 REM :::::::::::::::::::::::::
11710 REM :: Prompt For Response ::
11720 REM :::::::::::::::::::::::::
11730 DEF FN_PROMPT(x%, y%, text$, default$)
11740 LOCAL r$
11750 PRINT TAB(x%, y%)text$;" ";default$:PRINT TAB(x% + LEN(text$) + 1, y%);
11760 r$ = GET$:r$ = FN_TO_UPPER(r$):IF r$ = CHR$(13) THEN r$ = default$
11770 = r$
11780 :
11790 REM :::::::::::::::::::::
11800 REM ::   Center text   ::
11810 REM :::::::::::::::::::::
11820 DEF FN_CENTER(text$):= (CW% - LEN(text$)) DIV 2  + 1
11830 :
11840 REM ::::::::::::::::::::::
11850 REM :: Maximum of x & y ::
11860 REM ::::::::::::::::::::::
11870 DEF FN_MAX(x, y):= y + (x > y) * (y - x)
11880 :
11890 REM ::::::::::::::::::::::
11900 REM :: Minimum of x & y ::
11910 REM ::::::::::::::::::::::
11920 DEF FN_MIN(x, y):= y + (x < y) * (y - x)
11930 :
11940 REM :::::::::::::::::::::::::::
11950 REM ::   Bounded time ticks  ::
11960 REM :::::::::::::::::::::::::::
11970 DEF FN_INT_TIME:=TIME MOD MAXINT%
11980 :
11990 REM :::::::::::::::::::::::
12000 REM :: Has time reached  ::
12010 REM :: target seconds?   ::
12020 REM :::::::::::::::::::::::
12030 DEF FN_IS_TIME(sec%, prevSec%, targetSec%):= (sec% MOD targetSec% = 0 AND sec% <> prevSec%)
12040 :
12050 REM ::::::::::::::::::::::
12060 REM :: Retrieve a byte  ::
12070 REM :: register value   ::
12080 REM :: from VDP         ::
12090 REM ::::::::::::::::::::::
12100 DEF FN_getByteVDP(var%):A% = &A0:L% = var%:= USR(&FFF4)
12110 :
12120 REM ::::::::::::::::::::::
12130 REM :: Retrieve a word  ::
12140 REM :: register value   ::
12150 REM :: from VDP         ::
12160 REM ::::::::::::::::::::::
12170 DEF FN_getWordVDP(var%):= FN_getByteVDP(var%) + 256 * FN_getByteVDP(var% + 1)
12180 :
12190 REM ::::::::::::::::::::::
12200 REM :: Retrieve the     ::
12210 REM :: number of colors ::
12220 REM :: reported by VDP  ::
12230 REM ::::::::::::::::::::::
12240 DEF FN_COLORCOUNT:= FN_getByteVDP(&15)
12250 :
12260 REM ::::::::::::::::::::::
12270 REM :: Retrieve the     ::
12280 REM :: ASCII key code   ::
12290 REM :: reported by VDP  ::
12300 REM ::::::::::::::::::::::
12310 DEF FN_ASCII_KEYCODE:= FN_getByteVDP(&05)
12320 :
12330 REM ::::::::::::::::::::::
12340 REM :: Retrieve the     ::
12350 REM :: Virtual key code ::
12360 REM :: reported by VDP  ::
12370 REM ::::::::::::::::::::::
12380 DEF FN_VIRTUAL_KEYCODE:= FN_getByteVDP(&17)
12390 :
12400 REM :::::::::::::::::::::::::::::
12410 REM :: Retrieve the number of  ::
12420 REM :: keys as reported by VDP ::
12430 REM :::::::::::::::::::::::::::::
12440 DEF FN_ASCII_KEYCOUNT:= FN_getByteVDP(&19)
12450 :
12460 REM :::::::::::::::::::::::::::::::::
12470 REM :: Retrieve a keypress within  ::
12480 REM :: the given timeout value     ::
12490 REM :::::::::::::::::::::::::::::::::
12500 DEF FN_GET_KEY(timeout%)
12510 LOCAL i%, keycount%, r%, sync%
12520 r% = -1
12530 keycount% = FN_ASCII_KEYCOUNT
12540 i% = 0
12550 REPEAT
12560   IF keycount% <> FN_ASCII_KEYCOUNT THEN r% = FN_ASCII_KEYCODE:IF r% = 0 THEN r% = FN_VIRTUAL_KEYCODE ELSE *FX 19
12570   i% = i% + 1
12580 UNTIL i% = timeout% OR r% > 0
12590 := r%
12600 :
12610 REM :::::::::::::::::::::::::::
12620 REM :: Empty Keyboard Buffer ::
12630 REM :::::::::::::::::::::::::::
12640 DEF PROC_EMPTY_KEYBOARD_BUFFER
12650 REPEAT UNTIL INKEY(0) = -1
12660 ENDPROC
12670 :
12680 REM ::::::::::::::::::::::::::::
12690 REM :: Disable display of the ::
12700 REM :: cursor on the screen   ::
12710 REM ::::::::::::::::::::::::::::
12720 DEF PROC_HIDE_CURSOR:VDU 23,1,0;0;0;0;:ENDPROC
12730 :
12740 REM ::::::::::::::::::::::::::::
12750 REM :: Enable display of the  ::
12760 REM :: cursor on the screen   ::
12770 REM ::::::::::::::::::::::::::::
12780 DEF PROC_SHOW_CURSOR:VDU 23,1,1;0;0;0;:ENDPROC
12790 :
12800 REM :::::::::::::::::::::::::::::::::
12810 REM :: Center text both vertically ::
12820 REM :: and horizontally            ::
12830 REM :::::::::::::::::::::::::::::::::
12840 DEF PROC_FULL_CENTER_TEXT(text$):VDU 31,FN_CENTER(text$), CH% DIV 2:PRINT text$;:ENDPROC
12850 :
12860 REM :::::::::::::::::::::::::::::::::::::::
12870 REM :: Pause execution of the program    ::
12880 REM :: for a number of ticks (1/100) sec ::
12890 REM :::::::::::::::::::::::::::::::::::::::
12900 DEF PROC_SLEEP(hundredth_seconds%):LOCAL t:hundredth_seconds% = hundredth_seconds% + (hundredth_seconds% < 0) * -hundredth_seconds%:t = TIME:REPEAT UNTIL ((TIME - t) > hundredth_seconds%):ENDPROC
12910 :
12920 REM :::::::::::::::::::::::::::::
12930 REM ::  Display Centered Text  ::
12940 REM :::::::::::::::::::::::::::::
12950 DEF PROC_CENTER(text$)
12960 LOCAL i%, n%, l%
12970 l% = 0
12980 FOR i% = 1 TO LEN(text$)
12990   IF ASC(MID$(text$, i%, 1)) >= BLANK THEN l% = l% + 1
13000 NEXT i%
13010 n% = FN_CENTER(STRING$(l%, CHR$(BLANK)))
13020 i% = VPOS:VDU 31, n%, i%
13030 FOR i% = 1 TO LEN(text$)
13040   VDU ASC(MID$(text$, i%, 1))
13050 NEXT i%
13060 ENDPROC
13070 :
13080 REM ::::::::::::::::::::::::::::
13090 REM :: Restore Default Colors ::
13100 REM ::::::::::::::::::::::::::::
13110 DEF PROC_DEFAULT_COLORS
13120 COLOUR 128+BLACK:COLOUR WHITE
13130 ENDPROC
13140 :
13150 REM ::::::::::::::::::::::::
13160 REM ::   Display Scores   ::
13170 REM ::::::::::::::::::::::::
13180 DEF PROC_DISPLAY_SCORES
13190 LOCAL hs$, sc$
13200 PROC_UPDATE_HIGH_SCORE
13210 hs$ = CHR$(17)+CHR$(YELLOW)+"HIGH SCORE "+CHR$(17)+CHR$(WHITE)+STR$(HighScore%)
13220 sc$ = CHR$(17)+CHR$(RED)+"1UP "+CHR$(17)+CHR$(WHITE)+STR$(Score%)
13230 PRINT TAB(0,0)sc$:PRINT TAB(CW%-LEN(hs$)+4,0)hs$
13240 ENDPROC
13250 :
13260 REM :::::::::::::::::::::::::
13270 REM ::  Update High Score  ::
13280 REM :::::::::::::::::::::::::
13290 DEF PROC_UPDATE_HIGH_SCORE
13300 IF (HighScore% < Score%) THEN HighScore% = Score%:REM Check if new highscore has been achieved and update if needed
13310 ENDPROC
13320 :
13330 REM :::::::::::::::::::::::::
13340 REM ::   High Score Read   ::
13350 REM :::::::::::::::::::::::::
13360 DEF PROC_HISCORE_READ(game$)
13370 LOCAL f0, status%, val%
13380 status% = 0
13390 f0% = OPENIN(game$ + ".HI")
13400 IF f0% = 0 THEN status% = -1:GOTO 13420
13410 INPUT#f0%, val%
13420 CLOSE#f0%
13430 IF status% = 0 THEN HighScore% = val%
13440 ENDPROC
13450 :
13460 REM :::::::::::::::::::::::::
13470 REM ::   High Score Write  ::
13480 REM :::::::::::::::::::::::::
13490 DEF PROC_HISCORE_WRITE(game$)
13500 LOCAL f0
13510 f0% = OPENOUT(game$ + ".HI")
13520 PRINT#f0%, HighScore%
13530 CLOSE#f0%
13540 ENDPROC
13550 :
13560 REM :::::::::::::::::::::::::::
13570 REM :: Empty Keyboard Buffer ::
13580 REM :::::::::::::::::::::::::::
13590 DEF PROC_EMPTY_KEYBOARD_BUFFER
13600 REPEAT UNTIL INKEY(0) = -1
13610 ENDPROC
13620 :
13630 REM ::::::::::::::::::::::::::::::::::::::
13640 REM :: Custom "Screen Memory" Functions ::
13650 REM ::::::::::::::::::::::::::::::::::::::
13660 DEF FN_READ(x%, y%)
13670 LOCAL n%
13680 n% = FN_XYINDEX(x%, y%)
13690 := screen?n%
13700 :
13710 DEF PROC_WRITE(x%, y%, ch%)
13720 LOCAL n%
13730 n% = FN_XYINDEX(x%, y%)
13740 screen?n% = ch%
13750 ENDPROC
13760 :
13770 DEF PROC_CLEAR_SCREEN
13780 LOCAL n%, ub%
13790 ub% = CW% * CH% - 1
13800 FOR n% = 0 TO ub%
13810   screen?n% = BLANK
13820 NEXT n%
13830 ENDPROC
13840 :
13850 DEF PROC_PLOT(x%, y%, ch%, co%)
13860 PROC_WRITE(x%, y%, ch%)
13870 VDU 31, x%, y%
13880 IF co% < 0 THEN co% = FN_COLOR_MAP(ch%)
13890 VDU 17, co%, ch%
13900 ENDPROC
13910 :
13920 DEF PROC_DRAW(x%, y%, ch%, overwrite%)
13930 LOCAL f%
13940 f% = FN_READ(x%, y%):REM Is Position Currently Unoccupied?
13950 IF (f% = BLANK OR overwrite%) THEN PROC_PLOT(x%, y%, ch%, -1)
13960 ENDPROC
13970 :
13980 REM :::::::::::::::::::::::::
13990 REM ::  Map Char To Color  ::
14000 REM :::::::::::::::::::::::::
14010 DEF FN_COLOR_MAP(c%)
14020 LOCAL r%
14030 r% = (c% = E_HEART OR c% = MON_RED)*-RED + (c% = E_FRUIT OR c% = MON_GREEN)*-GREEN + ((c% >= SN_L AND c% <= SN_U) OR c% = SN_W OR c% = E_TOADSTOOL)*-YELLOW
14040 r% = r% + (c% = MON_BLUE OR c% = B_VERT OR c% = B_HORZ OR (c% >= B_UR AND c% <= B_DR))*-BLUE
14050 r% = r% + (c% = MON_PINK)*-MAGENTA + (c% = E_DIAMOND OR c% = MON_CYAN)*-CYAN + (c% = DOT OR c% = MON_WHITE OR c% = E_CIRC OR c% = E_DISC)*-WHITE
14060 := r%
14070 :
14080 REM ::::::::::::::::::::
14090 REM ::  Random Event  ::
14100 REM ::::::::::::::::::::
14110 DEF PROC_RANDOM_EVENT
14120 LOCAL c%, f, free%, i%, r, rx%, ry%
14130 REM IF FN_RND_PCT(10) THEN GOTO ENDPROC:REM No new obstacle
14140 rx% = FN_RND_X:ry% = FN_RND_Y:REM Determine random position
14150 free% = (FN_READ(rx%, ry%) = BLANK):IF NOT free% THEN 14230:REM Ensure the position is free
14160 r = RND(1)
14170 IF (r < .85) THEN 14200
14180 i% = FN_NEXT_MONSTER_SLOT
14190 IF (-1 < i%) THEN c% = ASC(MID$(MO$, i% + 1, 1)):PROC_MANAGE_MONSTER(rx%, ry%, c%, TRUE):f = 9.5:GOTO 14220:ELSE c% = E_DISC:GOTO 14210
14200 c% = (r >= 0 AND r < .5)*-DOT + (r >= .5 AND r < .7)*-E_CIRC + (r >= .7 AND r < .85)*-FN_RND_EDIBLE
14210 PROC_DRAW(rx%, ry%, c%, FALSE):f = 4.5
14220 PROC_SOUND(f, 2)
14230 ENDPROC
14240 :
14250 REM :::::::::::::::::
14260 REM ::   REM Eat   ::
14270 REM :::::::::::::::::
14280 DEF FN_EAT(x%, y%)
14290 LOCAL c%, n%, s%
14300 c% = FN_READ(x%, y%):REM PROC_COUT(STR$(x%)+" "+STR$(y%)+" "+STR$(c%)+"      ",0)
14310 n% = (c% = BLANK OR c% = BLANK_X)*0 + ((c% = SN_W AND NOT Rvs_Dir%) OR (c% >= MON_RED AND c% <= MON_GREEN) OR (c% = E_TOADSTOOL))*-1 + (c% = E_CIRC)*-2 + (c% = E_DISC)*-3
14320 n% = n% + (c% = DOT OR (c% >= E_HEART AND c% <= E_FRUIT))*-4 + (c% = MON_WHITE OR c% = MON_BLUE)*-5
14330 n% = n% + (c% = B_VERT OR c% = B_HORZ OR c% = B_UR OR c% = B_UL OR c% = B_DL OR c% = B_DR)*-6
14340 ON n% GOTO 14350,14360,14370,14390,14420,14440:ELSE 14450
14350 Dead% = TRUE:r% = FALSE:GOTO 14460:REM Collided with self, toadstool or deadly monster
14360 PROC_SHRINK_SNAKE(1):GOTO 14400:REM The open circle shrinks the snake
14370 PROC_UPDATE_MONSTER_STATE(TRUE, MON_BLUE):PROC_CHARGE:REM The filled circle makes existing monsters vulnerable
14380 GOTO 14400
14390 s% = 2 + (c% = DOT) + (c% = E_DISC)*2:PROC_INC_SIZE(s%):REM Edible increases size of snake
14400 Score% = Score% + (c% = DOT)*-(Size% * 5) + (c% = E_HEART)*-100 + (c% = E_DIAMOND)*-200 + (c% = E_FRUIT)*-500 + (c% = E_CIRC OR c% = E_DISC)*-25
14410 GOTO 14450
14420 Score% = Score% + (c% = MON_BLUE)*-500 + (c% = MON_WHITE)*-1000:PROC_MANAGE_MONSTER(x%, y%, c%, FALSE):PROC_SHRINK_SNAKE(1):REM Eating cowardly monster shrinks the snake
14430 GOTO 14450
14440 Dead% = TRUE:REM Collided with boundary
14450 IF (Dead% <> TRUE AND c% <> BLANK AND c% <> BLANK_X AND c% <> SN_W) THEN PROC_SOUND(16, 2)
14460 := (Dead%) + (NOT Dead%) * -c%
14470 :
14480 REM :::::::::::::::::::::::::::
14490 REM ::    Recoil The Snake   ::
14500 REM :::::::::::::::::::::::::::
14510 DEF FN_RECOIL_SNAKE
14520 LOCAL i%
14530 i% = P - Size%:REM Locate tail end of snake
14540 IF i% < 0 THEN i% = i% + MAX_SIZE%:REM Wrap around to the end
14550 PROC_ERASE(X(i%), Y(i%))
14560 := i%
14570 :
14580 REM ::::::::::::::::::::::::::
14590 REM ::   Shrink The Snake   ::
14600 REM ::::::::::::::::::::::::::
14610 DEF PROC_SHRINK_SNAKE(d%)
14620 LOCAL n%
14630 PROC_INC_SIZE(-d%)
14640 n% = FN_RECOIL_SNAKE
14650 ENDPROC
14660 :
14670 REM ::::::::::::::::::::::::::
14680 REM ::  Grow Out The Snake  ::
14690 REM ::::::::::::::::::::::::::
14700 DEF PROC_GROW_SNAKE(d%)
14710 LOCAL i%, ch%, dx%, dy%, nx%, ny%
14720 dx% = FN_X_DELTA(d%):dy% = FN_Y_DELTA(d%)
14730 nx% = X(P) + dx%:ny% = Y(P) + dy%
14740 ch% = FN_EAT(nx%, ny%)
14750 IF ch% < 0 THEN 14880
14760 IF nx% <= UX% THEN nx% = LX% - 1:REM Snake entered Left Portal; Exit Out Right Portal
14770 IF nx% >= LX% THEN nx% = UX% + 1:REM Snake entered Right Portal; Exit Out Left Portal
14780 IF ny% <= UY% THEN ny% = LY% - 1:REM Snake entered Top Portal; Exit Out Bottom Portal
14790 IF ny% >= LY% THEN ny% = UY% + 1:REM Snake entered Bottom Portal; Exit Out Top Portal
14800 P = FN_NEXT_POS(P)
14810 i% = FN_RECOIL_SNAKE
14820 X(P) = nx%:Y(P) = ny%
14830 i% = FN_NEXT_POS(i%):IF i% = P THEN 14850
14840 REPEAT:PROC_DRAW(X(i%), Y(i%), SN_W, TRUE):i% = FN_NEXT_POS(i%):UNTIL i% = P
14850 ch% = (AF <> 0) * -SN_W + (AF = 0) * -(d% + SN_L - 1):REM Which Animation Frame To Display For Snake's Head
14860 PROC_DRAW(X(P), Y(P), ch%, TRUE)
14870 AF = (AF + 1) MOD 2
14880 ENDPROC
14890 :
14900 REM ::::::::::::::::::::::::::::::::
14910 REM :: Increase The Size Of Snake ::
14920 REM ::::::::::::::::::::::::::::::::
14930 DEF PROC_INC_SIZE(n%)
14940 Size% = FN_MAX(FN_MIN(Size% + n%, MAX_SIZE%), 2)
14950 ENDPROC
14960 :
14970 REM :::::::::::::::::::::::::::::::::::::
14980 REM :: Check For Reversal Of Direction ::
14990 REM :::::::::::::::::::::::::::::::::::::
15000 DEF FN_CHECK_FOR_REVERSE_DIRECTION(old%, new%)
15010 REM 4 = UP; 3= DOWN; 1 = LEFT; 2 = RIGHT
15020 REM := (old% = 4 AND new% = 3) OR (old% = 3 AND new% = 4) OR (old% = 1 AND new% = 2) OR (old% = 2 AND new% = 1)
15030 := (old% OR new%) = 7 OR (old% OR new%) = 3
15040 :
15050 REM :::::::::::::::::::::::::::::::::
15060 REM :: Monster Management Routines ::
15070 REM :::::::::::::::::::::::::::::::::
15080 DEF PROC_MANAGE_MONSTER(x%, y%, c%, state%)
15090 LOCAL pos%:REM PROC_COUT(STR$(x%)+","+STR$(y%)+" "+STR$(c%)+"  ", 2)
15100 IF state% = FALSE THEN PROC_CLEAR_MONSTER(x%, y%)
15110 IF state% = TRUE THEN PROC_NEW_MONSTER(x%, y%, c%):PROC_DRAW(x%, y%, c%, TRUE)
15120 IF state% = MON_BLUE OR state% = MON_WHITE THEN PROC_DRAW(x%, y%, state%, TRUE):IF state% = MON_BLUE THEN PROC_MON_GO_WHITE
15130 IF state% = MON_RESET THEN PROC_DRAW(x%, y%, c%, TRUE)
15140 ENDPROC
15150 :
15160 DEF PROC_MANAGE_MONSTER_BY_POS(pos%, c%, state%)
15170 LOCAL x%, y%
15180 IF -1 <> pos% THEN y% = pos% DIV CW%:x% = pos% MOD CW%:PROC_MANAGE_MONSTER(x%, y%, c%, state%)
15190 ENDPROC
15200 :
15210 DEF PROC_NEW_MONSTER(x%, y%, c%)
15220 LOCAL i%
15230 IF M_Count% >= MAX_MONSTERS% THEN 15260
15240 i% = FN_NEXT_MONSTER_SLOT
15250 IF -1 <> i% THEN MP%(i%) = FN_HASH(x%, y%):M_Position% = i%:M_Count% = M_Count% + 1
15260 ENDPROC
15270 :
15280 DEF FN_NEXT_MONSTER_SLOT
15290 LOCAL found%, i%, r%
15300 found% = FALSE:i% = 0
15310 REPEAT
15320   IF -1 = MP%(i%) THEN found% = TRUE:ELSE i% = i% + 1
15330 UNTIL found% OR i% = MAX_MONSTERS%
15340 IF found% THEN r% = i%:ELSE r% = -1
15350 := r%
15360 :
15370 DEF FN_FIND_MONSTER(x%, y%)
15380 LOCAL found%, i%, r%
15390 found% = FALSE:i% = 0
15400 REPEAT
15410   IF (FN_HASH(x%, y%) = MP%(i%)) THEN found% = TRUE:ELSE i% = i% + 1
15420 UNTIL found% OR i% = MAX_MONSTERS%
15430 IF found% THEN r% = i%:ELSE r% = -1
15440 := r%
15450 :
15460 DEF FN_MONSTER_AT_POS(pos%)
15470 LOCAL r%, x%, y%
15480 r% = -1:IF -1 <> pos% THEN y% = pos% DIV CW%:x% = pos% MOD CW%:r% = FN_READ(x%, y%)
15490 := r%
15500 :
15510 DEF PROC_CLEAR_MONSTER(x%, y%)
15520 LOCAL i%
15530 i% = FN_FIND_MONSTER(x%, y%)
15540 IF -1 <> i% THEN MP%(i%) = -1:M_Count% = M_Count% - 1
15550 ENDPROC
15560 :
15570 DEF PROC_UPDATE_MONSTER_STATE(oldState%, newState%)
15580 LOCAL c%, i%
15590 FOR i% = 0 TO MAX_MONSTERS% - 1
15600   c% = (newState% = MON_RESET) * -ASC(MID$(MO$, i%+1, 1)) + (newState% = MON_WHITE AND oldState% = MON_BLUE) * -FN_MONSTER_AT_POS(MP%(i%))
15610   IF -1 <> MP%(i%) AND (oldState% = c% OR oldState% = TRUE) THEN PROC_MANAGE_MONSTER_BY_POS(MP%(i%), c%, newState%)
15620 NEXT i%
15630 ENDPROC
15640 :
15650 DEF PROC_MONSTER_COLOR_CHECK
15660 IF MonGoWhite% <> FALSE AND FN_INT_TIME >= MonGoWhite% THEN MonGoWhite% = FALSE:PROC_UPDATE_MONSTER_STATE(MON_BLUE, MON_WHITE)
15670 IF MonReset% <> FALSE AND FN_INT_TIME >= MonReset% THEN MonReset% = FALSE:PROC_UPDATE_MONSTER_STATE(TRUE, MON_RESET)
15680 ENDPROC
15690 :
15700 REM :::::::::::::::::::::::::::::::::::::::::::
15710 REM ::  Calculate type index of a clockwise  ::
15720 REM ::  position on a box's perimeter        ::
15730 REM :::::::::::::::::::::::::::::::::::::::::::
15740 DEF FN_CLOCKWISE_BOX_SIDE_INDEX(pos%, width%, height%)
15750 REM 0 = UPPER_LEFT_CORNER, 1 = UPPER_MIDDLE, 2 = UPPER_RIGHT_CORNER, 3 = MIDDLE_RIGHT, 4 = LOWER_RIGHT_CORNER, 5 = LOWER_MIDDLE, 6 = LOWER_LEFT_CORNER, 7 = MIDDLE_LEFT
15760 LOCAL r%
15770 r% = (pos% > 0 AND pos% < width% - 1) * -1 + (pos% = width% - 1) * -2 + (pos% >= width% AND pos% < width% + height% - 2) * -3 + (pos% = width% + height% - 2) * -4
15780 r% = r% + (pos% > width% + height% - 2 AND pos% < 2 * width% + height% - 3) * -5 + (pos% = 2 * width% + height% - 3) * -6 + (pos% > 2 * width% + height% - 3) * -7
15790 :=r%
15800 :
15810 REM ::::::::::::::::::::::::::
15820 REM ::  Draw Box Clockwise  ::
15830 REM ::::::::::::::::::::::::::
15840 DEF PROC_CLOCKWISE_BOX(ux%, uy%, width%, height%, color%)
15850 LOCAL aq%, bq%, ch%, i%, p%, x%, y%
15860 aq% = width% + height% - 2:bq% = aq% + width%:p% = bq% + height% - 2
15870 FOR i% = 0 TO p% - 1
15880   x% = (i% < width%) * -i% + (i% > (width%-1) AND i% < aq%) * -(width%-1) + (i% >= aq% AND i% < bq%) * (i% - (bq% - 1)) + (i% >= bq%) * 0
15890   y% = (i% < width%) * 0 + (i% > (width%-1) AND i% < aq%) * -(i% - (width%-1)) + (i% >= aq% AND i% < bq%) * -(height%-1) + (i% >= bq%) * -((height%-2) - (i% - bq%))
15900   ch% = ASC(MID$(BX$, FN_CLOCKWISE_BOX_SIDE_INDEX(i%, width%, height%) + 1, 1))
15910   PROC_PLOT(ux% + x%, uy% + y%, ch%, color%)
15920 NEXT i%
15930 ENDPROC
15940 :
15950 REM :::::::::::::::::::::::::
15960 REM ::  Draw Playing Field ::
15970 REM :::::::::::::::::::::::::
15980 DEF PROC_DRAW_PLAYING_FIELD(ux%, uy%, width%, height%)
15990 PROC_CLOCKWISE_BOX(ux%, uy%, width%, height%, BLUE)
16000 ENDPROC
16010 :
16020 REM :::::::::::::::::::
16030 REM :: Draw Portals  ::
16040 REM :::::::::::::::::::
16050 DEF PROC_DRAW_PORTALS(horizontal%, vertical%, ux%, uy%, width%, height%)
16060 LOCAL ch%, h%, i%, j%, lx%, ly%, pColor%, v%, wColor%
16070 pColor% = CYAN:wColor% = BLUE:h% = ux% + width% DIV 2 - 2:v% = uy% + height% DIV 2 - 2:lx% = ux% + width% - 1:ly% = uy% + height% - 1
16080 FOR i% = 0 TO 1:REM Vertical portal
16090   FOR j% = 0 TO 2
16100     ch% = (vertical%) * -(ASC(MID$(PV$(i%), j% + 1, 1))) + (NOT vertical%) * -B_HORZ
16110     PROC_PLOT(h% + j%, (i% = 0) * -uy% + (i% = 1) * -ly%, ch%, (ch% = BLANK_X) * -BLACK + (ch% = B_HORZ) * -wColor% + ((ch% <> BLANK_X) AND (ch% <> B_HORZ)) * -pColor%)
16120   NEXT j%
16130 NEXT i%
16140 FOR i% = 0 TO 1:REM Horizontal portal
16150   FOR j% = 0 TO 2
16160     ch% = (horizontal%) * -(ASC(MID$(PH$(i%), j% + 1, 1))) + (NOT horizontal%) * -B_VERT
16170     PROC_PLOT((i% = 0) * -ux% + (i% = 1) * -lx%, v% + j%, ch%, (ch% = BLANK_X) * -BLACK + (ch% = B_VERT) * -wColor% + ((ch% <> BLANK_X) AND (ch% <> B_VERT)) * -pColor%)
16180   NEXT j%
16190 NEXT i%
16200 ENDPROC
16210 :
16220 REM :::::::::::::::::::::::::::
16230 REM ::  Update Portal State  ::
16240 REM :::::::::::::::::::::::::::
16250 DEF PROC_UPDATE_PORTAL_STATE
16260 LOCAL horizontal%, vertical%
16270 Portal_State% = (Portal_State% + 1) MOD 4
16280 IF Portal_State% = 0 THEN horizontal% = FALSE: vertical% = FALSE
16290 IF Portal_State% = 1 THEN horizontal% = TRUE: vertical% = FALSE
16300 IF Portal_State% = 2 THEN horizontal% = TRUE: vertical% = TRUE
16310 IF Portal_State% = 3 THEN horizontal% = FALSE: vertical% = TRUE
16320 PROC_DRAW_PORTALS(horizontal%, vertical%, UX%, UY%, CW% - 2*UX%, CH% - UY%)
16330 FOR i% = 1 TO 2:PROC_SOUND(i% * 24, 1.5 * i%):NEXT i%
16340 ENDPROC
16350 :
16360 REM ::::::::::::::::::::::::::::::::
16370 REM ::       Clockwise Plot       ::
16380 REM ::::::::::::::::::::::::::::::::
16390 DEF PROC_CLOCKWISE_PLOT(pos%, color%, char%, ux%, uy%, width%, height%)
16400 LOCAL cx%, cy%, a%, b%, c%
16410 a% = width% + height% - 2:b% = a% + width%:c% = b% + height% - 2
16420 cx% = (pos% < width%) * -pos% + (pos% > (width% - 1) AND pos% < a%) * -(width% - 1)
16430 cx% = cx% + (pos% >= a% AND pos% < b%) * (pos% - (b% - 1)) + (pos% >= b%) * 0
16440 cy% = (pos% < width%) * 0 + (pos% > (width% - 1) AND pos% < a%) * -(pos% - (width% - 1))
16450 cy% = cy% + (pos% >= a% AND pos% < b%) * -(height% - 1) + (pos% >= b%) * -((height% - 2) - (pos% - b%))
16460 VDU 31,ux% + cx%,uy% + cy%,17,color%,char%:REM Plot a character on the path
16470 ENDPROC
16480 :
16490 REM :::::::::::::::::::::::
16500 REM ::  Death Animation  ::
16510 REM :::::::::::::::::::::::
16520 DEF PROC_DEATH_ANIMATION(x%, y%)
16530 LOCAL ch%, fr$, i%, n%
16540 REPEAT:Size% = Size% - 1:n% = FN_RECOIL_SNAKE:PROC_SOUND(2 * Size%, 2):PROC_SOUND(0, 0):PROC_SLEEP(10):UNTIL Size% < 2
16550 fr$ = RIGHT$("0"+STR$(SN_W), 3)+STR$(SN_U)+STR$(SN_D1)+STR$(SN_D2)+STR$(SN_D3)+STR$(SN_D4)+STR$(SN_D5)+STR$(SN_D6)+"0"+STR$(BLANK)
16560 FOR i% = 1 TO LEN(fr$) DIV 3 STEP 2
16570   ch% = VAL(MID$(fr$, 3 * (i% - 1) + 1, 3))
16580   VDU 31, x%, y%, 17, YELLOW, ch%:PROC_SOUND(i% + 8, 2):PROC_SLEEP(20)
16590 NEXT i%:PROC_SOUND(4, 3)
16600 ENDPROC
16610 :
16620 REM :::::::::::::::::::
16630 REM ::    Welcome    ::
16640 REM :::::::::::::::::::
16650 DEF PROC_WELCOME
16660 LOCAL boxh%, boxw%, c%, cc%, ch$, ex%, perimeter%, t%, t$, ux%, uy%
16670 boxh% = 18:boxw% = 38:cc% = 0:ex% = FALSE:perimeter% = 2 * (boxw% + boxh% - 2):t% = 2:ux% = (CW% - boxw%) DIV 2:uy% = 0
16680 CLS:PROC_HIDE_CURSOR
16690 PRINT TAB(0, uy% + 1);
16700 PROC_CENTER(" Welcome to " + CHR$(17)+CHR$(YELLOW) + GameName$ + CHR$(17)+CHR$(WHITE)+ "..."):PRINT:PRINT
16710 PROC_CENTER(" A nostalgic variant of the classic"):PRINT
16720 PROC_CENTER(" SNAKE game."):PRINT:PRINT
16730 PROC_CENTER(" Use the four arrow keys to maneuver"):PRINT
16740 PROC_CENTER(" the starving little snake to snack"):PRINT
16750 PROC_CENTER(" on pellets and other tasty morsels."):PRINT
16760 PROC_CENTER(" Avoid the walls and spooky little"):PRINT
16770 PROC_CENTER(" monsters while trying to avoid"):PRINT
16780 PROC_CENTER(" chomping on yourself like an"):PRINT
16790 COLOUR YELLOW:PROC_CENTER(" Ouroboros."):PRINT:PRINT
16800 COLOUR WHITE:PROC_CENTER(" Good luck and have fun!"):PRINT:PRINT
16810 COLOUR CYAN:PROC_CENTER("Hit a key to continue")
16820 REPEAT
16830   PROC_CLOCKWISE_PLOT(cc%, BLACK, BLANK, ux%, uy%, boxw%, boxh%)
16840   cc% = (cc% + 1) MOD perimeter%
16850   PROC_CLOCKWISE_PLOT(cc%, cc% MOD 6 + 1, MON_RED, ux%, uy%, boxw%, boxh%)
16860   IF SY$ = "A" THEN c% = INKEY(DL%):PROC_EMPTY_KEYBOARD_BUFFER:ELSE c% = INKEY(TK/DL%)
16870   IF c% > 0 THEN ex% = TRUE
16880 UNTIL ex%
16890 boxh% = 18:boxw% = 38:cc% = 0:ex% = FALSE:perimeter% = 2 * (boxw% + boxh% - 2):ux% = (CW% - boxw%) DIV 2:uy% = 0
16900 CLS:PROC_DEFAULT_COLORS
16910 PRINT TAB(0, uy% + 2);
16920 COLOUR YELLOW:PROC_CENTER(STRING$(t%, " ")+"....  Score"+STRING$(2, " ")+"Resize"):PRINT:PRINT
16930 t$ = STRING$(t%, " ")+CHR$(17)+CHR$(RED)+CHR$(MON_RED)+CHR$(17)+CHR$(MAGENTA)+CHR$(MON_RED)+CHR$(17)+CHR$(CYAN)+CHR$(MON_RED)+CHR$(17)+CHR$(GREEN)+CHR$(MON_RED)
16940 t$ = t$ + STRING$(2, " ")+CHR$(17)+CHR$(WHITE)+"Death"+STRING$(2, " ")+CHR$(SKULL)+STRING$(6, " "):PROC_CENTER(t$):PRINT
16950 PROC_CENTER(STRING$(t%, " ")+CHR$(17)+CHR$(BLUE)+CHR$(MON_RED)+STRING$(5, " ")+CHR$(17)+CHR$(WHITE)+"  500"+STRING$(2, " ")+"-"+CHR$(17)+CHR$(YELLOW)+CHR$(SN_W)+STRING$(4, " ")):PRINT
16960 PROC_CENTER(STRING$(t%, " ")+CHR$(17)+CHR$(WHITE)+CHR$(MON_RED)+STRING$(5, " ")+" 1000"+STRING$(2, " ")+"-"+CHR$(17)+CHR$(YELLOW)+CHR$(SN_W)+STRING$(4, " ")):PRINT
16970 PROC_CENTER(STRING$(t%, " ")+CHR$(17)+CHR$(WHITE)+CHR$(DOT)+STRING$(5, " ")+"  5x"+CHR$(17)+CHR$(YELLOW)+CHR$(SN_W)+STRING$(2, " ")+CHR$(17)+CHR$(WHITE)+"+"+CHR$(17)+CHR$(YELLOW)+CHR$(SN_W)+STRING$(4, " ")):PRINT
16980 PROC_CENTER(STRING$(t%, " ")+CHR$(17)+CHR$(WHITE)+CHR$(E_CIRC)+STRING$(5, " ")+"   25"+STRING$(2, " ")+"-"+CHR$(17)+CHR$(YELLOW)+CHR$(SN_W)+STRING$(4, " ")):PRINT
16990 PROC_CENTER(STRING$(t%, " ")+CHR$(17)+CHR$(WHITE)+CHR$(E_DISC)+STRING$(5, " ")+"   25"+STRING$(2, " ")+STRING$(6, " ")):PRINT
17000 PROC_CENTER(STRING$(t%, " ")+CHR$(17)+CHR$(RED)+CHR$(E_HEART)+STRING$(5, " ")+CHR$(17)+CHR$(WHITE)+"  100"+STRING$(2, " ")+"+"+CHR$(17)+CHR$(YELLOW)+CHR$(SN_W)+CHR$(SN_W)+STRING$(3, " ")):PRINT
17010 PROC_CENTER(STRING$(t%, " ")+CHR$(17)+CHR$(CYAN)+CHR$(E_DIAMOND)+STRING$(5, " ")+CHR$(17)+CHR$(WHITE)+"  200"+STRING$(2, " ")+"+"+CHR$(17)+CHR$(YELLOW)+CHR$(SN_W)+CHR$(SN_W)+STRING$(3, " ")):PRINT
17020 PROC_CENTER(STRING$(t%, " ")+CHR$(17)+CHR$(GREEN)+CHR$(E_FRUIT)+STRING$(5, " ")+CHR$(17)+CHR$(WHITE)+"  500"+STRING$(2, " ")+"+"+CHR$(17)+CHR$(YELLOW)+CHR$(SN_W)+CHR$(SN_W)+STRING$(3, " ")):PRINT
17030 PROC_CENTER(STRING$(t%, " ")+CHR$(17)+CHR$(YELLOW)+CHR$(E_TOADSTOOL)+STRING$(5, " ")+CHR$(17)+CHR$(WHITE)+"Death"+STRING$(2, " ")+CHR$(SKULL)+STRING$(6, " ")):PRINT:PRINT
17040 COLOUR GREEN:PROC_CENTER("Hit a key to begin playing")
17050 PROC_CLOCKWISE_BOX(ux% + 1, uy% + 1, boxw% - 2, boxh% - 2, CYAN)
17060 ch$=CHR$(SN_R)+CHR$(SN_R)+CHR$(SN_D)+CHR$(SN_D)+CHR$(SN_L)+CHR$(SN_L)+CHR$(SN_U)+CHR$(SN_U)
17070 REPEAT
17080   PROC_CLOCKWISE_PLOT(cc% - 1, BLACK, BLANK, ux%, uy%, boxw%, boxh%):PROC_CLOCKWISE_PLOT(cc%, BLACK, BLANK, ux%, uy%, boxw%, boxh%)
17090   cc% = (cc% + 1) MOD perimeter%
17100   c% = (cc% MOD 2 <> 0) * -SN_W + (cc% MOD 2 = 0) * -ASC(MID$(ch$, FN_CLOCKWISE_BOX_SIDE_INDEX(cc%, boxw%, boxh%) + 1, 1))
17110   PROC_CLOCKWISE_PLOT(cc% - 1, YELLOW, SN_W, ux%, uy%, boxw%, boxh%):PROC_CLOCKWISE_PLOT(cc%, YELLOW, c%, ux%, uy%, boxw%, boxh%)
17120   IF SY$ = "A" THEN c% = INKEY(DL%):PROC_EMPTY_KEYBOARD_BUFFER:ELSE c% = INKEY(TK/DL%)
17130   IF c% > 0 THEN ex% = TRUE
17140 UNTIL ex%
17150 PROC_DEFAULT_COLORS
17160 ENDPROC
17170 :
17180 REM :::::::::::::::::
17190 REM ::  Game Over  ::
17200 REM :::::::::::::::::
17210 DEF PROC_GAME_OVER
17220 VDU 17,RED:PROC_FULL_CENTER_TEXT("GAME OVER")
17230 PROC_SLEEP(200):VDU 17,YELLOW
17240 PROC_HISCORE_WRITE(GameName$)
17250 ENDPROC
17260 :
17270 REM :::::::::::::::::::::::
17280 REM :: Play Simple Sound ::
17290 REM :::::::::::::::::::::::
17300 DEF PROC_SOUND(index%, duration%)
17310 LOCAL constant%:constant% = 12.2
17320 SOUND 1, -10, index% * constant%, duration%
17330 ENDPROC
17340 :
17350 REM :::::::::::::::::::::::::
17360 REM :: Play Musical Phrase ::
17370 REM :::::::::::::::::::::::::
17380 DEF PROC_PLAY(notes$)
17390 LOCAL d%, j%, l%, p%
17400 l% = LEN(notes$) DIV 3
17410 FOR j% = 1 TO l% STEP 2
17420   p% = VAL(MID$(notes$, 3 * (j% - 1) + 1, 3)):d% = VAL(MID$(notes$, 3 * (j% - 1) + 4, 3))
17430   SOUND 1, -10, p%, d%
17440   SOUND 1, 0, p%, 1:REM Stacatto the currently playing sound
17450 NEXT j%
17460 ENDPROC
17470 :
17480 REM :::::::::::::::::::
17490 REM ::  CHARGE!!!!!  ::
17500 REM :::::::::::::::::::
17510 DEF PROC_CHARGE
17520 PROC_PLAY("129001149001165001177004165002177008"):REM COUNT,PITCH,DURATION
17530 ENDPROC
17540 :
17550 REM ::::::::::::::::::::::::::
17560 REM :: Define Custom Colors ::
17570 REM ::::::::::::::::::::::::::
17580 DEF PROC_REDEFINE_COLORS
17590 IF SY$="A" AND FN_COLORCOUNT < &40 THEN VDU 19,C_ORANGE,&FF,&FF,&80,&00:ELSE COLOUR C_ORANGE,&FF,&80,&00
17600 ENDPROC
17610 :
17620 REM ::::::::::::::::::::::::::::::
17630 REM :: Define Custom Characters ::
17640 REM ::::::::::::::::::::::::::::::
17650 DEF PROC_REDEFINE_CHARS
17660 VDU 23,BLANK_X,0,0,0,0,0,0,0,0:REM BLANK
17670 VDU 23,DOT,0,0,0,24,24,0,0,0:REM DOT
17680 VDU 23,SN_L,0,60,30,14,14,30,60,0:REM LEFT(3)
17690 VDU 23,SN_R,0,60,120,112,112,120,60,0:REM RIGHT(3)
17700 VDU 23,SN_D,0,60,126,126,102,66,0,0:REM DOWN(3)
17710 VDU 23,SN_U,0,0,66,102,126,126,60,0:REM UP(3)
17720 VDU 23,SN_W,0,60,126,126,126,126,60,0:REM WHOLE(3)
17730 VDU 23,E_HEART,54,127,127,127,62,28,8,0:REM HEART(1)
17740 VDU 23,E_DIAMOND,8,28,62,127,62,28,8,0:REM DIAMOND(6)
17750 VDU 23,E_FRUIT,0,12,24,60,60,60,24,0:REM FRUIT(2)
17760 VDU 23,E_TOADSTOOL,0,0,24,60,60,8,24,0:REM TOADSTOOL(3)
17770 VDU 23,E_CIRC,0,60,126,102,102,126,60,0:REM CIRCLE(7)
17780 VDU 23,E_DISC,0,60,126,126,126,126,60,0:REM FILLED CIRCLE(7)
17790 VDU 23,MON_WHITE,0,60,126,90,126,126,90,0:REM WHITE(7)
17800 VDU 23,MON_BLUE,0,60,126,90,126,126,90,0:REM BLUE(4)
17810 VDU 23,MON_RED,0,60,126,90,126,126,90,0:REM RED(1)
17820 VDU 23,MON_PINK,0,60,126,90,126,126,90,0:REM MAGENTA(5)
17830 VDU 23,MON_CYAN,0,60,126,90,126,126,90,0:REM CYAN(6)
17840 VDU 23,MON_GREEN,0,60,126,90,126,126,90,0:REM GREEN(2)
17850 VDU 23,B_VERT,24,24,24,24,24,24,24,24:REM VERTICAL(4)
17860 VDU 23,B_HORZ,0,0,0,255,255,0,0,0:REM HORIZONTAL(4)
17870 VDU 23,B_UR,0,0,0,7,15,28,24,24:REM UPRIGHT C(4)
17880 VDU 23,B_UL,0,0,0,224,240,56,24,24:REM UPLEFT C(4)
17890 VDU 23,B_DL,24,24,56,240,224,0,0,0:REM DOWNLEFT C(4)
17900 VDU 23,B_DR,24,24,28,15,7,0,0,0:REM DOWN RIGHT C(4)
17910 VDU 23,SN_D1,0,0,0,102,126,126,60,0:REM DYING 1
17920 VDU 23,SN_D2,0,0,0,0,126,126,60,0:REM DYING 2
17930 VDU 23,SN_D3,0,0,0,0,126,126,60,0:REM DYING 3
17940 VDU 23,SN_D4,0,0,0,0,24,60,60,0:REM DYING 4
17950 VDU 23,SN_D5,0,0,0,0,24,24,60,0:REM DYING 5
17960 VDU 23,SN_D6,0,0,0,0,8,24,16,0:REM DYING 6
17970 VDU 23,SKULL,0,189,126,90,126,165,24,0:REM SKULL(7)
17980 ENDPROC
17990 :
18000 REM ::::::::::::::::::::::::::::::
18010 REM ::  Error Handling Routine  ::
18020 REM ::::::::::::::::::::::::::::::
18030 DEF PROC_HANDLE_ERROR
18040 IF ERR = 17 AND Dead% = FALSE THEN ERR = 0:GOTO 18080
18050 PROC_DEFAULT_COLORS:PROC_SHOW_CURSOR
18060 REPORT
18070 PRINT" @line #";ERL:STOP
18080 ENDPROC
