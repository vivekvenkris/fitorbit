/*    Program to calculate orbital phase of a binary pulsar at given
      epochs. Given the orbital period of the binary and a reference
      epoch for the ascending node, just specify current MJD and UT 
      hh mm ss and required orbital phase and the program will return
      the next time that phase occurs */
main()
{
  float pbinary, refmjd, reqmjd, tlapse, thephase;
  float reqphase, phshift, tshift, tdays;
  int mjdno, norb, ih, im, is;
  float hh, mm, ss;

  printf("Enter orbital period of the binary (days) > ");
  scanf("%f",&pbinary);
  printf("Enter ref MDJ of phase 0.0 > ");
  scanf("%f",&refmjd);

  printf("Enter required Julian day number (integer) > ");
  scanf("%d",&mjdno);
  printf("Enter required UT on that day (hh mm ss) > ");
  scanf("%f %f %f",&hh, &mm, &ss);

/* compute required MJD from day number and UT */
  hh=hh/24.0; mm=mm/1440.0; ss=ss/86400.0;
  reqmjd = mjdno + hh + mm + ss;

/* work out the time lapse since the reference epoch */
  tlapse = reqmjd - refmjd;
  printf("Elapsed time is %0.3f days\n",tlapse);
  norb = tlapse/pbinary;
  thephase = tlapse/pbinary-norb;

  printf("There have been %d complete orbits since the ref epoch\n",norb);
  printf("The present orbital phase (MJD %0.3f) is %0.3f\n",reqmjd,thephase);

  printf("Enter required orbital phase > ");
  scanf("%f",&reqphase);

/* get next time of required phase */
  if (reqphase < thephase)
    {
      phshift = 1.0 - thephase + reqphase;
    }
  else
    { 
      phshift =  reqphase - thephase;
    }

  tshift = pbinary*phshift;
  reqmjd = reqmjd + tshift;
  mjdno  = reqmjd;

  printf("MJD of next phase %0.3f is %0.3f\n",reqphase,reqmjd);

/* Display also time of day in UT */
  tdays=reqmjd-mjdno;
  tdays=tdays*24.0; ih=tdays; tdays=tdays-ih;
  tdays=tdays*60.0; im=tdays; tdays=tdays-im;
  tdays=tdays*60.0; is=tdays; tdays=tdays-is;
  printf("i.e. UT on day %d is %d:%d:%d\n",mjdno,ih,im,is);
  printf("Goodbye! \n");
}

