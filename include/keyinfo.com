c This file was generated by keys2fort. Do not modify directly

       integer NUM_KEYS
       parameter (NUM_KEYS = 112)

       integer EPH_PSRJ
       parameter (EPH_PSRJ = 1)
       integer EPH_PSRB
       parameter (EPH_PSRB = 2)
       integer EPH_RAJ
       parameter (EPH_RAJ = 3)
       integer EPH_DECJ
       parameter (EPH_DECJ = 4)
       integer EPH_PEPOCH
       parameter (EPH_PEPOCH = 5)
       integer EPH_F
       parameter (EPH_F = 6)
       integer EPH_F1
       parameter (EPH_F1 = 7)
       integer EPH_F2
       parameter (EPH_F2 = 8)
       integer EPH_F3
       parameter (EPH_F3 = 9)
       integer EPH_F4
       parameter (EPH_F4 = 10)
       integer EPH_F5
       parameter (EPH_F5 = 11)
       integer EPH_F6
       parameter (EPH_F6 = 12)
       integer EPH_F7
       parameter (EPH_F7 = 13)
       integer EPH_F8
       parameter (EPH_F8 = 14)
       integer EPH_F9
       parameter (EPH_F9 = 15)
       integer EPH_FA
       parameter (EPH_FA = 16)
       integer EPH_FB
       parameter (EPH_FB = 17)
       integer EPH_FC
       parameter (EPH_FC = 18)
       integer EPH_PMDEC
       parameter (EPH_PMDEC = 19)
       integer EPH_PMRA
       parameter (EPH_PMRA = 20)
       integer EPH_PMRV
       parameter (EPH_PMRV = 21)
       integer EPH_POSEPOCH
       parameter (EPH_POSEPOCH = 22)
       integer EPH_PX
       parameter (EPH_PX = 23)
       integer EPH_DM
       parameter (EPH_DM = 24)
       integer EPH_DM1
       parameter (EPH_DM1 = 25)
       integer EPH_DM2
       parameter (EPH_DM2 = 26)
       integer EPH_DM3
       parameter (EPH_DM3 = 27)
       integer EPH_DM4
       parameter (EPH_DM4 = 28)
       integer EPH_DM5
       parameter (EPH_DM5 = 29)
       integer EPH_DM6
       parameter (EPH_DM6 = 30)
       integer EPH_DM7
       parameter (EPH_DM7 = 31)
       integer EPH_DM8
       parameter (EPH_DM8 = 32)
       integer EPH_DM9
       parameter (EPH_DM9 = 33)
       integer EPH_TASC
       parameter (EPH_TASC = 34)
       integer EPH_START
       parameter (EPH_START = 35)
       integer EPH_FINISH
       parameter (EPH_FINISH = 36)
       integer EPH_CLK
       parameter (EPH_CLK = 37)
       integer EPH_EPHEM
       parameter (EPH_EPHEM = 38)
       integer EPH_TZRMJD
       parameter (EPH_TZRMJD = 39)
       integer EPH_TZRFRQ
       parameter (EPH_TZRFRQ = 40)
       integer EPH_TZRSITE
       parameter (EPH_TZRSITE = 41)
       integer EPH_BINARY
       parameter (EPH_BINARY = 42)
       integer EPH_A1
       parameter (EPH_A1 = 43)
       integer EPH_A1_2
       parameter (EPH_A1_2 = 44)
       integer EPH_A1_3
       parameter (EPH_A1_3 = 45)
       integer EPH_E
       parameter (EPH_E = 46)
       integer EPH_E_2
       parameter (EPH_E_2 = 47)
       integer EPH_E_3
       parameter (EPH_E_3 = 48)
       integer EPH_T0
       parameter (EPH_T0 = 49)
       integer EPH_T0_2
       parameter (EPH_T0_2 = 50)
       integer EPH_T0_3
       parameter (EPH_T0_3 = 51)
       integer EPH_PB
       parameter (EPH_PB = 52)
       integer EPH_PB_2
       parameter (EPH_PB_2 = 53)
       integer EPH_PB_3
       parameter (EPH_PB_3 = 54)
       integer EPH_OM
       parameter (EPH_OM = 55)
       integer EPH_OM_2
       parameter (EPH_OM_2 = 56)
       integer EPH_OM_3
       parameter (EPH_OM_3 = 57)
       integer EPH_OMDOT
       parameter (EPH_OMDOT = 58)
       integer EPH_OMDOT_2
       parameter (EPH_OMDOT_2 = 59)
       integer EPH_OMDOT_3
       parameter (EPH_OMDOT_3 = 60)
       integer EPH_GAMMA
       parameter (EPH_GAMMA = 61)
       integer EPH_PBDOT
       parameter (EPH_PBDOT = 62)
       integer EPH_PBDOT_2
       parameter (EPH_PBDOT_2 = 63)
       integer EPH_PBDOT_3
       parameter (EPH_PBDOT_3 = 64)
       integer EPH_PPNGAMMA
       parameter (EPH_PPNGAMMA = 65)
       integer EPH_SINI
       parameter (EPH_SINI = 66)
       integer EPH_SINI_2
       parameter (EPH_SINI_2 = 67)
       integer EPH_SINI_3
       parameter (EPH_SINI_3 = 68)
       integer EPH_MTOT
       parameter (EPH_MTOT = 69)
       integer EPH_M2
       parameter (EPH_M2 = 70)
       integer EPH_M2_2
       parameter (EPH_M2_2 = 71)
       integer EPH_M2_3
       parameter (EPH_M2_3 = 72)
       integer EPH_DTHETA
       parameter (EPH_DTHETA = 73)
       integer EPH_XDOT
       parameter (EPH_XDOT = 74)
       integer EPH_EDOT
       parameter (EPH_EDOT = 75)
       integer EPH_XOMDOT
       parameter (EPH_XOMDOT = 76)
       integer EPH_XPBDOT
       parameter (EPH_XPBDOT = 77)
       integer EPH_DR
       parameter (EPH_DR = 78)
       integer EPH_A0
       parameter (EPH_A0 = 79)
       integer EPH_B0
       parameter (EPH_B0 = 80)
       integer EPH_BP
       parameter (EPH_BP = 81)
       integer EPH_BPP
       parameter (EPH_BPP = 82)
       integer EPH_EXPA
       parameter (EPH_EXPA = 83)
       integer EPH_EXPA_2
       parameter (EPH_EXPA_2 = 84)
       integer EPH_EXPA_3
       parameter (EPH_EXPA_3 = 85)
       integer EPH_EXPT
       parameter (EPH_EXPT = 86)
       integer EPH_EXPT_2
       parameter (EPH_EXPT_2 = 87)
       integer EPH_EXPT_3
       parameter (EPH_EXPT_3 = 88)
       integer EPH_GLEP_1
       parameter (EPH_GLEP_1 = 89)
       integer EPH_GLPH_1
       parameter (EPH_GLPH_1 = 90)
       integer EPH_GLF0_1
       parameter (EPH_GLF0_1 = 91)
       integer EPH_GLF1_1
       parameter (EPH_GLF1_1 = 92)
       integer EPH_GLF0D_1
       parameter (EPH_GLF0D_1 = 93)
       integer EPH_GLTD_1
       parameter (EPH_GLTD_1 = 94)
       integer EPH_GLEP_2
       parameter (EPH_GLEP_2 = 95)
       integer EPH_GLPH_2
       parameter (EPH_GLPH_2 = 96)
       integer EPH_GLF0_2
       parameter (EPH_GLF0_2 = 97)
       integer EPH_GLF1_2
       parameter (EPH_GLF1_2 = 98)
       integer EPH_GLF0D_2
       parameter (EPH_GLF0D_2 = 99)
       integer EPH_GLTD_2
       parameter (EPH_GLTD_2 = 100)
       integer EPH_GLEP_3
       parameter (EPH_GLEP_3 = 101)
       integer EPH_GLPH_3
       parameter (EPH_GLPH_3 = 102)
       integer EPH_GLF0_3
       parameter (EPH_GLF0_3 = 103)
       integer EPH_GLF1_3
       parameter (EPH_GLF1_3 = 104)
       integer EPH_GLF0D_3
       parameter (EPH_GLF0D_3 = 105)
       integer EPH_GLTD_3
       parameter (EPH_GLTD_3 = 106)
       integer EPH_GLEP_4
       parameter (EPH_GLEP_4 = 107)
       integer EPH_GLPH_4
       parameter (EPH_GLPH_4 = 108)
       integer EPH_GLF0_4
       parameter (EPH_GLF0_4 = 109)
       integer EPH_GLF1_4
       parameter (EPH_GLF1_4 = 110)
       integer EPH_GLF0D_4
       parameter (EPH_GLF0D_4 = 111)
       integer EPH_GLTD_4
       parameter (EPH_GLTD_4 = 112)


       character*16 parmNames(NUM_KEYS)
       data parmNames(1) /'PSRJ'/
       data parmNames(2) /'PSRB'/
       data parmNames(3) /'RAJ'/
       data parmNames(4) /'DECJ'/
       data parmNames(5) /'PEPOCH'/
       data parmNames(6) /'F0'/
       data parmNames(7) /'F1'/
       data parmNames(8) /'F2'/
       data parmNames(9) /'F3'/
       data parmNames(10) /'F4'/
       data parmNames(11) /'F5'/
       data parmNames(12) /'F6'/
       data parmNames(13) /'F7'/
       data parmNames(14) /'F8'/
       data parmNames(15) /'F9'/
       data parmNames(16) /'FA'/
       data parmNames(17) /'FB'/
       data parmNames(18) /'FC'/
       data parmNames(19) /'PMDEC'/
       data parmNames(20) /'PMRA'/
       data parmNames(21) /'PMRV'/
       data parmNames(22) /'POSEPOCH'/
       data parmNames(23) /'PX'/
       data parmNames(24) /'DM'/
       data parmNames(25) /'DM1'/
       data parmNames(26) /'DM2'/
       data parmNames(27) /'DM3'/
       data parmNames(28) /'DM4'/
       data parmNames(29) /'DM5'/
       data parmNames(30) /'DM6'/
       data parmNames(31) /'DM7'/
       data parmNames(32) /'DM8'/
       data parmNames(33) /'DM9'/
       data parmNames(34) /'TASC'/
       data parmNames(35) /'START'/
       data parmNames(36) /'FINISH'/
       data parmNames(37) /'CLK'/
       data parmNames(38) /'EPHEM'/
       data parmNames(39) /'TZRMJD'/
       data parmNames(40) /'TZRFRQ'/
       data parmNames(41) /'TZRSITE'/
       data parmNames(42) /'BINARY'/
       data parmNames(43) /'A1'/
       data parmNames(44) /'A1_2'/
       data parmNames(45) /'A1_3'/
       data parmNames(46) /'ECC'/
       data parmNames(47) /'E_2'/
       data parmNames(48) /'E_3'/
       data parmNames(49) /'T0'/
       data parmNames(50) /'T0_2'/
       data parmNames(51) /'T0_3'/
       data parmNames(52) /'PB'/
       data parmNames(53) /'PB_2'/
       data parmNames(54) /'PB_3'/
       data parmNames(55) /'OM'/
       data parmNames(56) /'OM_2'/
       data parmNames(57) /'OM_3'/
       data parmNames(58) /'OMDOT'/
       data parmNames(59) /'OMDOT_2'/
       data parmNames(60) /'OMDOT_3'/
       data parmNames(61) /'GAMMA'/
       data parmNames(62) /'PBDOT'/
       data parmNames(63) /'PBDOT_2'/
       data parmNames(64) /'PBDOT_3'/
       data parmNames(65) /'PPNGAMMA'/
       data parmNames(66) /'SINI'/
       data parmNames(67) /'SINI_2'/
       data parmNames(68) /'SINI_3'/
       data parmNames(69) /'MTOT'/
       data parmNames(70) /'M2'/
       data parmNames(71) /'M2_2'/
       data parmNames(72) /'M2_3'/
       data parmNames(73) /'DTHETA'/
       data parmNames(74) /'XDOT'/
       data parmNames(75) /'EDOT'/
       data parmNames(76) /'XOMDOT'/
       data parmNames(77) /'XPBDOT'/
       data parmNames(78) /'DR'/
       data parmNames(79) /'A0'/
       data parmNames(80) /'B0'/
       data parmNames(81) /'BP'/
       data parmNames(82) /'BPP'/
       data parmNames(83) /'EXPA'/
       data parmNames(84) /'EXPA_2'/
       data parmNames(85) /'EXPA_3'/
       data parmNames(86) /'EXPT'/
       data parmNames(87) /'EXPT_2'/
       data parmNames(88) /'EXPT_3'/
       data parmNames(89) /'GLEP_1'/
       data parmNames(90) /'GLPH_1'/
       data parmNames(91) /'GLF0_1'/
       data parmNames(92) /'GLF1_1'/
       data parmNames(93) /'GLF0D_1'/
       data parmNames(94) /'GLTD_1'/
       data parmNames(95) /'GLEP_2'/
       data parmNames(96) /'GLPH_2'/
       data parmNames(97) /'GLF0_2'/
       data parmNames(98) /'GLF1_2'/
       data parmNames(99) /'GLF0D_2'/
       data parmNames(100) /'GLTD_2'/
       data parmNames(101) /'GLEP_3'/
       data parmNames(102) /'GLPH_3'/
       data parmNames(103) /'GLF0_3'/
       data parmNames(104) /'GLF1_3'/
       data parmNames(105) /'GLF0D_3'/
       data parmNames(106) /'GLTD_3'/
       data parmNames(107) /'GLEP_4'/
       data parmNames(108) /'GLPH_4'/
       data parmNames(109) /'GLF0_4'/
       data parmNames(110) /'GLF1_4'/
       data parmNames(111) /'GLF0D_4'/
       data parmNames(112) /'GLTD_4'/

       integer parmTypes(NUM_KEYS)
       data parmTypes(1) /0/
       data parmTypes(2) /0/
       data parmTypes(3) /2/
       data parmTypes(4) /3/
       data parmTypes(5) /4/
       data parmTypes(6) /1/
       data parmTypes(7) /1/
       data parmTypes(8) /1/
       data parmTypes(9) /1/
       data parmTypes(10) /1/
       data parmTypes(11) /1/
       data parmTypes(12) /1/
       data parmTypes(13) /1/
       data parmTypes(14) /1/
       data parmTypes(15) /1/
       data parmTypes(16) /1/
       data parmTypes(17) /1/
       data parmTypes(18) /1/
       data parmTypes(19) /1/
       data parmTypes(20) /1/
       data parmTypes(21) /1/
       data parmTypes(22) /4/
       data parmTypes(23) /1/
       data parmTypes(24) /1/
       data parmTypes(25) /1/
       data parmTypes(26) /1/
       data parmTypes(27) /1/
       data parmTypes(28) /1/
       data parmTypes(29) /1/
       data parmTypes(30) /1/
       data parmTypes(31) /1/
       data parmTypes(32) /1/
       data parmTypes(33) /1/
       data parmTypes(34) /4/
       data parmTypes(35) /4/
       data parmTypes(36) /4/
       data parmTypes(37) /0/
       data parmTypes(38) /0/
       data parmTypes(39) /4/
       data parmTypes(40) /1/
       data parmTypes(41) /0/
       data parmTypes(42) /0/
       data parmTypes(43) /1/
       data parmTypes(44) /1/
       data parmTypes(45) /1/
       data parmTypes(46) /1/
       data parmTypes(47) /1/
       data parmTypes(48) /1/
       data parmTypes(49) /4/
       data parmTypes(50) /4/
       data parmTypes(51) /4/
       data parmTypes(52) /1/
       data parmTypes(53) /1/
       data parmTypes(54) /1/
       data parmTypes(55) /1/
       data parmTypes(56) /1/
       data parmTypes(57) /1/
       data parmTypes(58) /1/
       data parmTypes(59) /1/
       data parmTypes(60) /1/
       data parmTypes(61) /1/
       data parmTypes(62) /1/
       data parmTypes(63) /1/
       data parmTypes(64) /1/
       data parmTypes(65) /1/
       data parmTypes(66) /1/
       data parmTypes(67) /1/
       data parmTypes(68) /1/
       data parmTypes(69) /1/
       data parmTypes(70) /1/
       data parmTypes(71) /1/
       data parmTypes(72) /1/
       data parmTypes(73) /1/
       data parmTypes(74) /1/
       data parmTypes(75) /1/
       data parmTypes(76) /1/
       data parmTypes(77) /1/
       data parmTypes(78) /1/
       data parmTypes(79) /1/
       data parmTypes(80) /1/
       data parmTypes(81) /1/
       data parmTypes(82) /1/
       data parmTypes(83) /1/
       data parmTypes(84) /1/
       data parmTypes(85) /1/
       data parmTypes(86) /1/
       data parmTypes(87) /1/
       data parmTypes(88) /1/
       data parmTypes(89) /4/
       data parmTypes(90) /1/
       data parmTypes(91) /1/
       data parmTypes(92) /1/
       data parmTypes(93) /1/
       data parmTypes(94) /1/
       data parmTypes(95) /4/
       data parmTypes(96) /1/
       data parmTypes(97) /1/
       data parmTypes(98) /1/
       data parmTypes(99) /1/
       data parmTypes(100) /1/
       data parmTypes(101) /4/
       data parmTypes(102) /1/
       data parmTypes(103) /1/
       data parmTypes(104) /1/
       data parmTypes(105) /1/
       data parmTypes(106) /1/
       data parmTypes(107) /4/
       data parmTypes(108) /1/
       data parmTypes(109) /1/
       data parmTypes(110) /1/
       data parmTypes(111) /1/
       data parmTypes(112) /1/

       logical parmError(NUM_KEYS)
       data parmError(1) /.false./
       data parmError(2) /.false./
       data parmError(3) /.true./
       data parmError(4) /.true./
c       data parmError(5) /.false./   changed for JBO 1st dec 2000 caj
       data parmError(5) /.true./
       data parmError(6) /.true./
       data parmError(7) /.true./
       data parmError(8) /.true./
       data parmError(9) /.true./
       data parmError(10) /.true./
       data parmError(11) /.true./
       data parmError(12) /.true./
       data parmError(13) /.true./
       data parmError(14) /.true./
       data parmError(15) /.true./
       data parmError(16) /.true./
       data parmError(17) /.true./
       data parmError(18) /.true./
       data parmError(19) /.true./
       data parmError(20) /.true./
       data parmError(21) /.true./
       data parmError(22) /.false./
       data parmError(23) /.true./
       data parmError(24) /.true./
       data parmError(25) /.true./
       data parmError(26) /.true./
       data parmError(27) /.true./
       data parmError(28) /.true./
       data parmError(29) /.true./
       data parmError(30) /.true./
       data parmError(31) /.true./
       data parmError(32) /.true./
       data parmError(33) /.true./
       data parmError(34) /.false./
       data parmError(35) /.false./
       data parmError(36) /.false./
       data parmError(37) /.false./
       data parmError(38) /.false./
       data parmError(39) /.false./
       data parmError(40) /.false./
       data parmError(41) /.false./
       data parmError(42) /.false./
       data parmError(43) /.true./
       data parmError(44) /.true./
       data parmError(45) /.true./
       data parmError(46) /.true./
       data parmError(47) /.true./
       data parmError(48) /.true./
       data parmError(49) /.true./
       data parmError(50) /.true./
       data parmError(51) /.true./
       data parmError(52) /.true./
       data parmError(53) /.true./
       data parmError(54) /.true./
       data parmError(55) /.true./
       data parmError(56) /.true./
       data parmError(57) /.true./
       data parmError(58) /.true./
       data parmError(59) /.true./
       data parmError(60) /.true./
       data parmError(61) /.true./
       data parmError(62) /.true./
       data parmError(63) /.true./
       data parmError(64) /.true./
       data parmError(65) /.true./
       data parmError(66) /.true./
       data parmError(67) /.true./
       data parmError(68) /.true./
       data parmError(69) /.true./
       data parmError(70) /.true./
       data parmError(71) /.true./
       data parmError(72) /.true./
       data parmError(73) /.true./
       data parmError(74) /.true./
       data parmError(75) /.true./
       data parmError(76) /.true./
       data parmError(77) /.true./
       data parmError(78) /.false./
       data parmError(79) /.false./
       data parmError(80) /.false./
       data parmError(81) /.false./
       data parmError(82) /.false./
       data parmError(83) /.true./
       data parmError(84) /.true./
       data parmError(85) /.true./
       data parmError(86) /.true./
       data parmError(87) /.true./
       data parmError(88) /.true./
       data parmError(89) /.false./
       data parmError(90) /.true./
       data parmError(91) /.true./
       data parmError(92) /.true./
       data parmError(93) /.true./
       data parmError(94) /.true./
       data parmError(95) /.false./
       data parmError(96) /.true./
       data parmError(97) /.true./
       data parmError(98) /.true./
       data parmError(99) /.true./
       data parmError(100) /.true./
       data parmError(101) /.false./
       data parmError(102) /.true./
       data parmError(103) /.true./
       data parmError(104) /.true./
       data parmError(105) /.true./
       data parmError(106) /.true./
       data parmError(107) /.false./
       data parmError(108) /.true./
       data parmError(109) /.true./
       data parmError(110) /.true./
       data parmError(111) /.true./
       data parmError(112) /.true./


       integer maxKeyLen /8/
