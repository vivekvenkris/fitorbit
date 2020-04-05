c OEPH_KEY.COM defines name--> array index for old eph files.

       integer OEPH_RAB
       parameter (OEPH_RAB = 1)
       integer OEPH_DECB
       parameter (OEPH_DECB = 2)
       integer OEPH_RAJ
       parameter (OEPH_RAJ = 3)
       integer OEPH_DECJ
       parameter (OEPH_DECJ = 4)
       integer OEPH_PMRA
       parameter (OEPH_PMRA = 5)
       integer OEPH_PMDEC
       parameter (OEPH_PMDEC = 6)
       integer OEPH_PMEPOCH
       parameter (OEPH_PMEPOCH = 7)
       integer OEPH_P
       parameter (OEPH_P = 8)
       integer OEPH_PDOT
       parameter (OEPH_PDOT = 9)
       integer OEPH_PDDOT
       parameter (OEPH_PDDOT = 10)
       integer OEPH_VTRDOT
       parameter (OEPH_VTRDOT = 11)
       integer OEPH_PEPOCH
       parameter (OEPH_PEPOCH = 12)
       integer OEPH_DM
       parameter (OEPH_DM = 13)
       integer OEPH_RM
       parameter (OEPH_RM = 14)
       integer OEPH_WE
       parameter (OEPH_WE = 15)
       integer OEPH_W50
       parameter (OEPH_W50 = 16)
       integer OEPH_W10
       parameter (OEPH_W10 = 17)
       integer OEPH_S400
       parameter (OEPH_S400 = 18)
       integer OEPH_S600
       parameter (OEPH_S600 = 19)
       integer OEPH_S925
       parameter (OEPH_S925 = 20)
       integer OEPH_S1400
       parameter (OEPH_S1400 = 21)
       integer OEPH_S1600
       parameter (OEPH_S1600 = 22)
       integer OEPH_DMIN
       parameter (OEPH_DMIN = 23)
       integer OEPH_DIST
       parameter (OEPH_DIST = 24)
       integer OEPH_DMAX
       parameter (OEPH_DMAX = 25)
       integer OEPH_TASC
       parameter (OEPH_TASC = 26)
       integer OEPH_START
       parameter (OEPH_START = 27)
       integer OEPH_FINISH
       parameter (OEPH_FINISH = 28)
       integer OEPH_SCATTER
       parameter (OEPH_SCATTER = 29)
       integer OEPH_T0
       parameter (OEPH_T0 = 30)
       integer OEPH_PB
       parameter (OEPH_PB = 31)
       integer OEPH_A1
       parameter (OEPH_A1 = 32)
       integer OEPH_OM
       parameter (OEPH_OM = 33)
       integer OEPH_OMDOT
       parameter (OEPH_OMDOT = 34)
       integer OEPH_E
       parameter (OEPH_E = 35)
       integer OEPH_INCBIN
       parameter (OEPH_INCBIN = 36)
       integer OEPH_MCBIN
       parameter (OEPH_MCBIN = 37)
       integer OEPH_PBDOT
       parameter (OEPH_PBDOT = 38)
       integer OEPH_T0_2
       parameter (OEPH_T0_2 = 39)
       integer OEPH_PB_2
       parameter (OEPH_PB_2 = 40)
       integer OEPH_A1_2
       parameter (OEPH_A1_2 = 41)
       integer OEPH_OM_2
       parameter (OEPH_OM_2 = 42)
       integer OEPH_OMDOT_2
       parameter (OEPH_OMDOT_2 = 43)
       integer OEPH_E_2
       parameter (OEPH_E_2 = 44)
       integer OEPH_INCBIN_2
       parameter (OEPH_INCBIN_2 = 45)
       integer OEPH_MCBIN_2
       parameter (OEPH_MCBIN_2 = 46)
       integer OEPH_PBDOT_2
       parameter (OEPH_PBDOT_2 = 47)
       integer OEPH_T0_3
       parameter (OEPH_T0_3 = 48)
       integer OEPH_PB_3
       parameter (OEPH_PB_3 = 49)
       integer OEPH_A1_3
       parameter (OEPH_A1_3 = 50)
       integer OEPH_OM_3
       parameter (OEPH_OM_3 = 51)
       integer OEPH_OMDOT_3
       parameter (OEPH_OMDOT_3 = 52)
       integer OEPH_E_3
       parameter (OEPH_E_3 = 53)
       integer OEPH_INCBIN_3
       parameter (OEPH_INCBIN_3 = 54)
       integer OEPH_MCBIN_3
       parameter (OEPH_MCBIN_3 = 55)
       integer OEPH_PBDOT_3
       parameter (OEPH_PBDOT_3 = 56)
       integer OEPH_EXPA
       parameter (OEPH_EXPA = 57)
       integer OEPH_EXPT
       parameter (OEPH_EXPT = 58)
       integer OEPH_EXPA_2
       parameter (OEPH_EXPA_2 = 59)
       integer OEPH_EXPT_2
       parameter (OEPH_EXPT_2 = 60)
       integer OEPH_EXPA_3
       parameter (OEPH_EXPA_3 = 61)
       integer OEPH_EXPT_3
       parameter (OEPH_EXPT_3 = 62)
       integer OEPH_FRQTOL
       parameter (OEPH_FRQTOL = 63)
       integer OEPH_DDOT
       parameter (OEPH_DDOT = 64)

       integer OEPH_MAX
       parameter (OEPH_MAX = 64)
