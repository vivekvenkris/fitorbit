C
C************************************************************************
      program testfk54
************************************************************************
C
C SUBROUTINE TO USE THE JPL DE200 EPHEMERIS TO PROVIDE BARYCENTRIC 
C   POSITION (AU) AND VELOCITY (MICROAU/SEC) OF THE CENTRE OF THE EARTH
C   IN THE B1950 MEAN EQUATORIAL REFERENCE FRAME BUT WITHOUT E-TERMS.
C THIS SUBROUTINE SHOULD MIMIC THE OLD EPV ONE FOR THE MIT EPHEMERIS.
c APPROVED BY PTW, 16JAN86.
C MODIFIED TO REMOVE SPURIOUS PM AND CHANGE EPOCH FOR E-TERMS. AGL 29SEP89.
C
	implicit none
	integer aa, bb, iepv, i, zz
      DOUBLE PRECISION SLA_EPB,EP,DET,VECTOR(6),
     &   PVEC(3),PMOD,PRA,PDEC,PMPRA,PMPDEC,
     &   VVEC(3),VMOD,VRA,VDEC,PMVRA,PMVDEC,
     &   pra4,pdec4,vra4,vdec4
      write(*,*) 'hello!'
C
      do zz=1,10
      det = 49030.5d0+2400000.5D0
      aa=3
      bb=12
      do i=1,6
         vector(i)=0.0D0
      enddo
c
C new version of JPL ephemeris requires julian date
      write(*,*) 'calling pleph' 
      write(*,*) vector(1),vector(2),vector(3)
      write(*,*) AA,BB,DET
      CALL PLEPH (DET,aa,bb,VECTOR)
      write(*,*) 'called pleph'
      write(*,*) vector(1),vector(2),vector(3)
      
      IF (VECTOR(1).EQ.0.0.AND.VECTOR(2).EQ.0.0) THEN
C        ARRIVAL TIME OUT OF RANGE OF EPHEMERIS
         IEPV=36
         stop
      ENDIF
      IEPV=0
C
C CONVERT FROM J2000 TO B1950
C
C FIRST NORMALLIZE POSITION AND VELOCITY DCS
      CALL SLA_DVN ( VECTOR(1),PVEC(1),PMOD )
      CALL SLA_DVN ( VECTOR(4),VVEC(1),VMOD)
      write(*,*) 'normalised'
C
C NEXT CONVERT POSITION AND VELOCITY DCS TO SPHERICAL COORDS
      CALL SLA_DCC2S ( PVEC(1),PRA,PDEC )
      CALL SLA_DCC2S ( VVEC(1),VRA,VDEC )
      write(*,*) 'conv position to sph coords'
C
C CONVERT POSITION AND VELOCITY FROM FK5 TO FK4
      CALL SLA_FK54Z ( PRA ,PDEC ,1950.0D0 ,PRA4 ,PDEC4 ,PMPRA,PMPDEC)
      CALL SLA_FK54Z ( VRA ,VDEC ,1950.0D0 ,VRA4 ,VDEC4 ,PMVRA,PMVDEC)
      write(*,*) 'fk5-4'
C
C APPLY SPURIOUS PROPER MOTION TO EPOCH
c      EP = SLA_EPB ( DET )
c      CALL SLA_PM (PRA,PDEC,PMPRA,PMPDEC,0.D0,0.D0,1950.0D0,EP,PRA,PDEC)
c      CALL SLA_PM (VRA,VDEC,PMVRA,PMVDEC,0.D0,0.D0,1950.0D0,EP,VRA,VDEC)
C
C E-TERMS MUST NOW BE REMOVED TO COMPARE DIRECTLY WITH MIT POSITION
C  WHICH IS FK4 LESS THE E-TERMS.
      CALL SLA_SUBET ( PRA4 ,PDEC4 ,1950D0 ,PRA ,PDEC )
      CALL SLA_SUBET ( VRA4 ,VDEC4 ,1950D0 ,VRA ,VDEC )
      write(*,*) 'removed e-terms'
C 
C FINALLY CONVERT SPHERICALS BACK TO CARTESIANS
      CALL SLA_DCS2C ( PRA,PDEC,PVEC(1))
      CALL SLA_DCS2C ( VRA,VDEC,VVEC(1))
      write(*,*) 'back to cartesians'
C
C SCALE VELOCITY VECTOR AMPLITUDE FROM AU/DAY TO MICROAU/SEC
      VMOD = VMOD/0.086400
C
C FINALLY RESCALE BOTH POSITION AND VELOCITY VECTORS
      DO I=1,3
         VECTOR(I  ) = PMOD*PVEC(I)
         VECTOR(I+3) = VMOD*VVEC(I)
      ENDDO
      write(*,*) vector(1),vector(2),vector(3)
      write(*,*) 'done!'
      enddo
C
      stop
c
      END








