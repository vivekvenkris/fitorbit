*DECK ERFC
        FUNCTION ERFC (X)
        ERFC=1.-ERF(X)
        IF (ERFC.LT.1E-37) ERFC=1E-37
        END
C
        FUNCTION ERF (X)
        T=1./(1.+.47047*ABS(X))
        E=1.-(0.3480242*T-0.0958798*T*T+0.7478556*T*T*T)*EXP(-X*X)
        ERF=0.5+0.5*SIGN(E,X)
        IF (ERF.LT.1E-37) ERF=1E-37
        END
C
        FUNCTION CUMGAUS ( X )
C COMPUTE THE CUMULATIVE PROBABILITY OF A GAUSSIAN DISTRIBUTION
C   FOR ALL DEVIATIONS EXCEEDING X SIGMAS.
        IF (X.LT.4.5) THEN
        CUMGAUS = ERFC( X/SQRT(2.) )
        ELSE
        CUMGAUS = 0.398942/X*EXP(-(X**2.)/2.)
        ENDIF
        IF (CUMGAUS.LT.1.0E-36) CUMGAUS = 1.0E-36
        END
C
        FUNCTION CUMRAYL ( X )
C COMPUTE THE CUMULATIVE PROBABILITY OF A RAYLEIGH DISTRIBUTION
C   FOR ALL DEVIATIONS EXCEEDING X SIGMAS.
        CUMRAYL = EXP ( -0.5*X**2 )
        IF ( CUMRAYL.LT.1.0E-36 ) CUMRAYL = 1.0E-36
        END
