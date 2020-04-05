      program conveph
*
* Converts old format ".eph" and ".per" into the new format.
* Need to be in the "fitorbit" directory.
*
* LN 01/11/1993
*---
      implicit none
c Arrays used to retrieve values from READEPH.
      DOUBLE PRECISION E_VALUES(63)
      DOUBLE PRECISION E_ERRORS(63)
*
      REAL*8 P0,PORB,TASC,ASINI,OMEGA,ECC,EPPERI,EASC,PDOT,DM
      real*8 raddeg,pi,rab,decb,raj,decj,pmra,pmdec
      integer*4 i,istat,lp,lpp,lpo,LENSTR
      integer*4 ih,im,is1,irae,id,im2,is2,idece,idm,idme
      real*4 s1,s2
      character*40 ephfil,perfil,perfilo
      character*4  ext1,ext2,rootdir
      character    ans*1,line*80,ofile*70,psrnam*12
*--
      DATA PI/3.1415926537D0/, pmra,pmdec /0.D0,0.D0/
      DATA ext1,ext2,rootdir /'.per','.eph','New/'/
*--------------------------------------------------
    5 FORMAT (A)
      RADDEG = 180D0/PI
   10 istat = 0
      call getinfile( ' ',ephfil,EXT2,ISTAT )
      if (istat .ne. 0) stop
   20 write (6,12)
   12 format (' PSR name: ',$)
      read (5,5) psrnam
*
      do 100 I = 1,63
      E_VALUES(I) = 0.D0
  100 E_ERRORS(I) = 0.D0
*
          lp=LENSTR(ephfil)
          OPEN(2,FILE=ephfil(1:lp)//ext2,STATUS='OLD')
          READ(2,*)P0
          READ(2,*)PDOT
          READ(2,*)PORB
          READ(2,*)TASC
          READ(2,*)ASINI
          READ(2,*)OMEGA
          READ(2,*)ECC
          READ(2,5)LINE
          CLOSE(2)
        EASC = 2D0*ATAND(SQRT((1-ECC)/(1+ECC))*TAND(-OMEGA/2))
        EPPERI = TASC - PORB*(EASC - 180/PI*ECC*SIND(EASC))/360D0
*
C-- Use psrinfo to get RA and DEC
*
          lpp=LENSTR(psrnam)
          call system('psrinfo -c "RAJ DECJ DM" '//psrnam(1:lpp)
     .                                        //' > coords.tmp')
         open (3,file='coords.tmp',status='old',err=20)
*
C-- Skip header lines
*
         do 110 I =1,2
  110    read(3,5,end=20,err=20)
         read(3,111,end=20,err=20)ih,im,is1,s1,irae,id,im2,is2,s2,
     .                            idece,idm,dm,idme
  111  format (i2,1x,i2,1x,i2.2,f5.4,1x,i1,1x,i3,1x,i2,1x,I2.2,f4.3,
     .         1x,i2,2x,I4.4,f4.3,1x,i2)
         close (3,err=20)
*
       s1 = is1 + s1
       s2 = is2 + s2
       dm = idm + dm
*  10:45:50.1964 7 -45:09:54.216  7    58.165  1
*  19:01:38.93   6 +07:16:36      2   253      1
c         write (6,*) s1,s2,dm
C-- RA and DEC in degrees
         raj = (ih+im/60.D0+s1/3600.D0)*15.D0
         decj = ID/ABS(ID)*(ABS(id)+im2/60.D0+s2/3600.D0)
c Now calculate B1950.
           CALL SLA_FK54Z(RAj/RADDEG,DECj/RADDEG,1950.D0,RAB,
     &       DECB,PMRA,PMDEC)
           RAB = RAB*RADDEG
           DECB = DECB*RADDEG
          write (6,*)' I read these values:'
          write (6,*)'RA (deg): ',RAB
          write (6,*)'DEC (deg): ',DECB
          write (6,*)'P: ',P0
          write (6,*)'DM: ',DM
          write (6,*)'Pdot: ',PDOT
          write (6,*)'Porb: ',PORB
          write (6,*)'Tasc: ',TASC
          write (6,*)'Asini: ',ASINI
          write (6,*)'Omega: ',OMEGA
          write (6,*)'Ecc: ',ECC
          write (6,*)'Epperi: ',Epperi
          write (6,5)line
          write (6,21)
   21  format (/' OK to proceed? (y/n) [n]: ',$)
          read (5,5) ans
          call upconv( ans )
          if (ans.ne.'Y') goto 10
*
      perfil  = ephfil
      PERFILo = ephfil
      call chkoutfile( rootdir,perfilo,EXT1,ISTAT )
      if (istat .ne. 0) stop
      ephfil=perfilo
      lpo = LENSTR(perfilo)
*
c Put values back into E_VALUES array
           E_VALUES(1) = RAB
           E_VALUES(2) = DECB
           E_VALUES(3)=RAj
           E_VALUES(4)=DECj
           E_VALUES(8)=P0
           E_VALUES(9)=PDOT
           E_VALUES(12)=TASC
           E_VALUES(13)=DM
           E_VALUES(31)=PORB
           E_VALUES(26)=TASC
c           E_VALUES(27)=MJDS
c           E_VALUES(28)=MJDF
           E_VALUES(30)=EPPERI
           E_VALUES(32)=ASINI
           E_VALUES(33)=OMEGA
           E_VALUES(35)=ECC
*
c           E_ERRORS(3)=iRAe
c           E_ERRORS(4)=iDECe
c           E_ERRORS(13)=IDMe
  
c Now calculate B1950.
c           CALL SLA_FK54Z(RA/RADDEG,DEC/RADDEG,1950.0,E_VALUES(1),
c     &       E_VALUES(2),PMRA,PMDEC)
c           E_VALUES(1) = E_VALUES(1)*RADDEG
c           E_VALUES(2) = E_VALUES(2)*RADDEG
*

      ofile = rootdir//EPHFIL(1:lpo)
      CALL WRITEPH(ofile,E_VALUES,E_ERRORS)
*
c      OPEN(2,FILE=rootdir//PERFIL(1:lp)//ext1,STATUS='unknown')
c      write (2,5) line
      call system( 'cp '//PERFIL(1:lp)//ext1//' '//rootdir
     .                 //perfilo(1:lpo)//ext1 )
c      close (2)
      goto 10
*
      end
