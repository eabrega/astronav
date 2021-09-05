            PROGRAM TEST
            DOUBLE PRECISION JD,JD0,JD00,JDKON,LAMB,BET,PAR,ALF,DEL,
     *DZ,DEN,R,S(3),V,E,EP,JT,PI,Q0,Q
            REAL*4 A(20),U(20),X
            INTEGER*4 T(6),ICD(6),II(8)
            CHARACTER BUFER*70,TEXT*80,IT*5
            PI=4.0D0*DATAN(1.0D0)
            PI0=SNGL(PI)
            TEXT(1 :10)='MERKURY: '
            TEXT(11:20)='VENUS:   '
            TEXT(21:30)='MARS:     '
            TEXT(31:40)='JUPITER:   '
            TEXT(41:50)='SATURN:   '
            TEXT(51:60)='MOON:     '
            TEXT(61:70)='SUN:   '
            TEXT(71:80)='IST: '
            LN=0
            WRITE(*,'('' ДАТA И ВРЕМЯ?'')')
            READ(*,*) T
            CALL DATAJD(T(1),T(2),T(3),T(4),T(5),T(6),JD)
            CALL DATAJD(1986,7,1,0,0,0,JD00)
            WRITE(*,'('' ВВЕДИТЕ КОРДИНАТЫ'')')
         READ(*,*) DGT00,SHR00
101      FORMAT(2X,'{"name":"',A2,'", "alf":"',3I3,'", "del":"',
     *3I3,'","d":',F6.2,',"f":',F5.2,'}',A1)
            DGT=PI0*DGT00/180.0
            SHR=PI0*SHR00/180.0
            CALL DATAJD(T(1),T(2),T(3),0,0,0,JD0)
            CALL DATAJD(T(1),T(2),T(3),T(4),T(5),T(6),JD)
            CALL STAR(JD0,JD,Q0,Q)
***********-МЕРКУРИЙ-**********************************************
            CALL MERCUR(JD,LAMB,BET,ALF,DEL,DZ,DSE,DSP)
            JD=JD-DZ*5.0D0/864.0D0
            CALL MERCUR(JD,LAMB,BET,ALF,DEL,DZ,DSE,DSP)
            JD=JD+DZ*5.0D0/864.0D0
            IF(ALF.LT.0) ALF=ALF+2.0D0*PI
            CALL GRADUS(SNGL(180.0D0*ALF/PI)/15.0,ICD(1),ICD(2),ICD(3))
            CALL GRADUS(SNGL(180.0D0*DEL/PI),ICD(4),ICD(5),ICD(6))
            WRITE(LN,'(''['')')
            WRITE(LN,101) TEXT(1:10),ICD,2*DSP,DSE,","
            U(1)=U(11)
            A(1)=A(11)
***********-ВЕНЕРА-**********************************************
            CALL VENERA(JD,LAMB,BET,ALF,DEL,DZ,DSE,DSP)
            JD=JD-DZ*5.0D0/864.0D0
            CALL VENERA(JD,LAMB,BET,ALF,DEL,DZ,DSE,DSP)
            JD=JD+DZ*5.0D0/864.0D0
            IF(ALF.LT.0) ALF=ALF+2.0D0*PI
            CALL GRADUS(SNGL(180.0D0*ALF/PI)/15.0,ICD(1),ICD(2),ICD(3))
            CALL GRADUS(SNGL(180.0D0*DEL/PI),ICD(4),ICD(5),ICD(6))
            WRITE(LN,101) TEXT(11:20),ICD,2*DSP,DSE,","
            U(2)=U(12)
            A(2)=A(12)
!***********-МАРС-**********************************************
            CALL MARS(JD,LAMB,BET,ALF,DEL,DZ,DSE,DSP)
            JD=JD-DZ*5.0D0/864.0D0
            CALL MARS(JD,LAMB,BET,ALF,DEL,DZ,DSE,DSP)
            JD=JD+DZ*5.0D0/864.0D0
            IF(ALF.LT.0) ALF=ALF+2.0D0*PI
            CALL GRADUS(SNGL(180.0D0*ALF/PI)/15.0,ICD(1),ICD(2),ICD(3))
            CALL GRADUS(SNGL(180.0D0*DEL/PI),ICD(4),ICD(5),ICD(6))
            WRITE(LN,101) TEXT(21:30),ICD,2*DSP,DSE,","
            U(3)=U(13)
            A(3)=A(13)
***********-ЮПИТЕР-**********************************************
            CALL UPITER(JD,LAMB,BET,ALF,DEL,DZ,DSE,DSP)
            JD=JD-DZ*5.0D0/864.0D0
            CALL UPITER(JD,LAMB,BET,ALF,DEL,DZ,DSE,DSP)
            JD=JD+DZ*5.0D0/864.0D0
            IF(ALF.LT.0) ALF=ALF+2.0D0*PI
            CALL GRADUS(SNGL(180.0D0*ALF/PI)/15.0,ICD(1),ICD(2),ICD(3))
            CALL GRADUS(SNGL(180.0D0*DEL/PI),ICD(4),ICD(5),ICD(6))
            WRITE(LN,101) TEXT(31:40),ICD,2*DSE,1.0,","
            U(4)=U(14)
            A(4)=A(14)
***********-САТУРН-**********************************************
            CALL SATURN(JD,LAMB,BET,ALF,DEL,DZ,DSE,DSP)
            JD=JD-DZ*5.0D0/864.0D0
            CALL SATURN(JD,LAMB,BET,ALF,DEL,DZ,DSE,DSP)
            JD=JD+DZ*5.0D0/864.0D0
            IF(ALF.LT.0) ALF=ALF+2.0D0*PI
            CALL GRADUS(SNGL(180.0D0*ALF/PI)/15.0,ICD(1),ICD(2),ICD(3))
            CALL GRADUS(SNGL(180.0D0*DEL/PI),ICD(4),ICD(5),ICD(6))
            WRITE(LN,101) TEXT(41:50),ICD,2*DSE,1.0,","
            U(5)=U(15)
            A(5)=A(15)
**********-ЛУНА-**********************************************
            CALL LUNA(JD,LAMB,BET,PAR,ALF,DEL,RZ,FAS)
            ALF0=SNGL(PI*ALF/180.0D0)
            DEL0=SNGL(PI*DEL/180.0D0)
            PAR0=SNGL(PI*PAR/180.0D0)
            UT=ATAN2(0.9966472*SIN(SHR),COS(SHR))
            RSF1=0.9966472*SIN(UT)
            RCF1=COS(UT)
            H=SNGL(Q)+DGT-ALF0
            DALF=ATAN2(-RCF1*SIN(PAR0)*SIN(H),
     *     (COS(DEL0)-RCF1*SIN(PAR0)*COS(H)))
            ALF0=ALF0+DALF
            DEL0=ATAN2(COS(DALF)*(SIN(DEL0)-RSF1*SIN(PAR0)),
     *     (COS(DEL0)-RCF1*SIN(PAR0)*COS(H)))
            X=180.0*ALF0/PI0
            IF(X.LT.0) X=X+360.0
            CALL GRADUS(X/15.0,ICD(1),ICD(2),ICD(3))
            CALL GRADUS(180.0*DEL0/PI0,ICD(4),ICD(5),ICD(6))

            DL=60.0*180.0*ATAN(3476.0/RZ/6378.160)/PI0 

            WRITE(LN,101) TEXT(51:60),ICD,DL,FAS,","
            U(6)=U(16)
            A(6)=A(16)
***********-СОЛНЦЕ-**********************************************
            CALL GELIOS(JD,R,S,V,E,EP)
            JD=JD-R*5.0D0/864.0D0
            CALL GELIOS(JD,R,S,V,E,EP)
            JD=JD+R*5.0D0/864.0D0
            ALF0=SNGL(DATAN2(S(2),S(1)))
            IF(ALF0.LT.0) ALF0=ALF0+2.0*PI0
            DEL0=SNGL(DATAN2(S(3),DSQRT(S(2)**2+S(1)**2)))
            CALL GRADUS(180.0*ALF0/PI0/15.0,ICD(1),ICD(2),ICD(3))
            CALL GRADUS(180.0*DEL0/PI0,ICD(4),ICD(5),ICD(6))

            DS=60.0*180.0*ATAN(1.392/R/149.59787)/PI0  

            WRITE(LN,101) TEXT(61:70),ICD,DS,1.0,""
            WRITE(LN,'('']'')')
            U(7)=U(17)
            A(7)=A(17)
***********-ИСТОЧНИК-**********************************************
            CLOSE(1)
            END
