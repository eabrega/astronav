            PROGRAM TEST
            DOUBLE PRECISION JD,JD0,JD00,JDKON,LAMB,BET,PAR,ALF,DEL,
     *DZ,DEN,R,S(3),V,E,EP,JT,PI,Q0,Q
            REAL*4 A(20),U(20),X
            INTEGER*4 T(6),ICD(6),II(8)
            CHARACTER ISTOK*5,BUFER*70,TEXT*80,IT*5
            PI=4.0D0*DATAN(1.0D0)
            PI0=SNGL(PI)
            IT(1:1)='+'
            IT(2:2)='-'
            IT(3:3)='*'
            IT(4:5)='  '
            TEXT(1 :10)='МЕРКУРИЙ: '
            TEXT(11:20)='ВЕНЕРА:   '
            TEXT(21:30)='МАРС:     '
            TEXT(31:40)='ЮПИТЕР:   '
            TEXT(41:50)='САТУРН:   '
            TEXT(51:60)='ЛУНА:     '
            TEXT(61:70)='СОЛНЦЕ:   '
            TEXT(71:80)='ИСТОЧНИК: '
            WRITE(*,'(" РЕЖИМ ВЫВОДА?/
     *(0-ЭКРАН,1-ФАЙЛ BREGA.DAT")')
            READ(*,*) LN
            WRITE(*,'('' ДАТA И ВРЕМЯ?'')')
            READ(*,*) T
            CALL DATAJD(T(1),T(2),T(3),T(4),T(5),T(6),JD)
            WRITE(*,'('' ИНТЕРВАЛ ПРОГНОЗA?'')')
            READ(*,*) DEN
            WRITE(*,'('' ШАГ ВРЕМЕНИ?'')')
            READ(*,*) DSHAG
            WRITE(*,'('' НОМЕР СВЕТИЛА?''/1X,A10,''- 1''/
     *    1X,A10,''- 2''/1X,A10,''- 3''/1X,A10,''- 4''/
     *    1X,A10,''- 5''/1X,A10''- 6''/1X,A10,''- 7''/
     *    1X,A10,''- 8'')') TEXT(1:10),TEXT(11:20),TEXT(21:30),
     *    TEXT(31:40),TEXT(41:50),TEXT(51:60),TEXT(61:70),TEXT(71:80)
            READ(*,'(A70)') BUFER
            DO I=1,70
            IF(BUFER(I:I).EQ.'1') II(1)=1
            IF(BUFER(I:I).EQ.'2') II(2)=2
            IF(BUFER(I:I).EQ.'3') II(3)=3
            IF(BUFER(I:I).EQ.'4') II(4)=4
            IF(BUFER(I:I).EQ.'5') II(5)=5
            IF(BUFER(I:I).EQ.'6') II(6)=6
            IF(BUFER(I:I).EQ.'7') II(7)=7
            IF(BUFER(I:I).EQ.'8') II(8)=8
            ENDDO
            WRITE(*,'(''  НОМЕР ИСТОЧНИКА'')')
            READ(*,'(A5)') ISTOK
            OPEN(1,FILE='BREGA.DAT',STATUS='OLD')
            CALL DATAJD(1986,7,1,0,0,0,JD00)
            OPEN(2,FILE='IST.KAT',STATUS='OLD')
      I=0
      DO WHILE(ISTOK(1:3).NE.BUFER(1:3).AND.I.LT.346)
      I=I+1
            READ(2,'(A70)') BUFER
      ENDDO
            IF(I.LT.346) THEN
            READ(BUFER(19:70),'(6(I3),F5.2,F7.4,F6.3)') ICD,ZV,AS,DS
            CI1=(FLOAT(ICD(1))+FLOAT(ICD(2))/60.0
     *        +FLOAT(ICD(3))/3600.0)*15.0
            CI2=FLOAT(IABS(ICD(4)))+FLOAT(ICD(5))/60.0
     *        +FLOAT(ICD(6))/3600.0
            IF(ICD(4).LT.0) CI2=-CI2
            CALL VIDIST(JD00,JD,CI1,CI2,AS,DS,CI1,CI2)
            CALL GRADUS(CI1/15.0,ICD(1),ICD(2),ICD(3))
            CALL GRADUS(CI2,ICD(4),ICD(5),ICD(6))
            WRITE(LN,'(''       ВЫБРАН/
     * ИСТОЧНИК:  '',A18,A12)')
     *            BUFER(1:18),BUFER(55:66)
            WRITE(LN,'('' ЗВЕЗДНАЯ ВЕЛИЧИНА : '',F5.2)') ZV
            WRITE(LN,'('' ВОСХ. A='',3(I3),''  СКЛОН. D='',/
     *3(I3))') ICD
            CLOSE(2)
      ELSE
            CLOSE(2)
            WRITE(*,'('' КООРДИНАТЫ ИСТОЧНИКА?'')')
      WRITE(*,'('' ВОСХОЖДЕНИЕ (ЧАС,  МИН, СЕК)'')')
      WRITE(*,'('' СКЛОНЕНИЕ   (ГРАД, МИН, СЕК)'')')
          READ(*,*) ICD
          CI1=(FLOAT(ICD(1))+FLOAT(ICD(2))/60.0
     *        +FLOAT(ICD(3))/3600.0)*15.0
          CI2=FLOAT(IABS(ICD(4)))+FLOAT(IABS(ICD(5)))/60.0
     *        +FLOAT(IABS(ICD(6)))/3600.0
          IF(ICD(4).LT.0) CI2=-CI2
          WRITE(LN,'(''       ВЫБРАН ИСТОЧНИК:'')')
          WRITE(LN,'('' ВОСХ. A='',3(I3),''/
     *СКЛОН. D='',3(I3))') ICD
      ENDIF
            IF(DSHAG.GT.3600) DSHAG=3600.0
            IF(DSHAG.LT.1) DSHAG=1.0
            WRITE(*,'('' ВВЕДИТЕ КОРДИНАТЫ''/
     * '' ДОЛГОТА (ГРАД), ШИРОТА (ГРАД)'')')
         READ(*,*) DGT00,SHR00
100      FORMAT(1X,A1,A14,' T=',3I3,' A=',F6.2,' H=',F6.2)
101      FORMAT(2X,A14,' ALF=',3I3,' DEL=',3I3,' Д=',F6.2,' Ф=',F5.2)
        !   DGT00=37.8
        !   SHR00=55.9
            DGT=PI0*DGT00/180.0
            SHR=PI0*SHR00/180.0
            IFLAG=0
            CALL DATAJD(T(1),T(2),T(3),0,0,0,JD0)
            JDKON=JD+DEN
      DO WHILE(JD.LE.JDKON)
            CALL DATAJD(T(1),T(2),T(3),T(4),T(5),T(6),JD)
            CALL STAR(JD0,JD,Q0,Q)
***********-МЕРКУРИЙ-**********************************************
            CALL MERCUR(JD,LAMB,BET,ALF,DEL,DZ,DSE,DSP)
            JD=JD-DZ*5.0D0/864.0D0
            CALL MERCUR(JD,LAMB,BET,ALF,DEL,DZ,DSE,DSP)
            JD=JD+DZ*5.0D0/864.0D0
            IF(ALF.LT.0) ALF=ALF+2.0D0*PI
            IF(IFLAG.EQ.0) THEN
            WRITE(LN,'(5X,''ДАТА: '',3I4)') T(1),T(2),T(3)
          WRITE(LN,*) 180.0*DGT/PI0,180.0*SHR/PI0
            CALL GRADUS(SNGL(180.0D0*ALF/PI)/15.0,ICD(1),ICD(2),ICD(3))
            CALL GRADUS(SNGL(180.0D0*DEL/PI),ICD(4),ICD(5),ICD(6))
            WRITE(LN,101) TEXT(1:10),ICD,2*DSP,DSE
            ENDIF
            H=SNGL(Q)+DGT-SNGL(ALF)
            A(11)=180.0*(PI0+ATAN2(SIN(H),(COS(H)*SIN(SHR)-
     *TAN(SNGL(DEL))*COS(SHR))))/PI0
            SINH=SIN(SHR)*SIN(SNGL(DEL))+COS(SHR)*COS(SNGL(DEL))*COS(H)
            U(11)=180.0*ATAN(SINH/SQRT(1.0-SINH**2))/PI0
            IF(II(1).EQ.1.AND.IFLAG.NE.0) WRITE(LN,100)
     *IT(4:4),TEXT(1:10),T(4),T(5),T(6),A(11),U(11)
            IF(U(11)*U(1).LT.0.AND.A(11).LT.180.0) WRITE(LN,100)
     *IT(1:1),TEXT(1:10),T(4),T(5),T(6),A(11),U(11)
            IF(U(11)*U(1).LT.0.AND.A(11).GT.180.0) WRITE(LN,100)
     *IT(2:2),TEXT(1:10),T(4),T(5),T(6),A(11),U(11)
            IF(A(11).GT.180.0.AND.A(1).LT.180.0.AND.A(1).NE.0) 
     * WRITE(LN,100)
     *IT(3:3),TEXT(1:10),T(4),T(5),T(6),A(11),U(11)
            U(1)=U(11)
            A(1)=A(11)
***********-ВЕНЕРА-**********************************************
            CALL VENERA(JD,LAMB,BET,ALF,DEL,DZ,DSE,DSP)
            JD=JD-DZ*5.0D0/864.0D0
            CALL VENERA(JD,LAMB,BET,ALF,DEL,DZ,DSE,DSP)
            JD=JD+DZ*5.0D0/864.0D0
            IF(ALF.LT.0) ALF=ALF+2.0D0*PI
            IF(IFLAG.EQ.0) THEN
            CALL GRADUS(SNGL(180.0D0*ALF/PI)/15.0,ICD(1),ICD(2),ICD(3))
            CALL GRADUS(SNGL(180.0D0*DEL/PI),ICD(4),ICD(5),ICD(6))
            WRITE(LN,101) TEXT(11:20),ICD,2*DSP,DSE
            ENDIF
            H=SNGL(Q)+DGT-SNGL(ALF)
            A(12)=180.0*(PI0+ATAN2(SIN(H),(COS(H)*SIN(SHR)-
     *TAN(SNGL(DEL))*COS(SHR))))/PI0
            SINH=SIN(SHR)*SIN(SNGL(DEL))+COS(SHR)*COS(SNGL(DEL))*COS(H)
            U(12)=180.0*ATAN(SINH/SQRT(1.0-SINH**2))/PI0
            IF(II(2).EQ.2.AND.IFLAG.NE.0) WRITE(LN,100)
     *IT(4:4),TEXT(11:20),T(4),T(5),T(6),A(12),U(12)
            IF(U(12)*U(2).LT.0.AND.A(12).LT.180.0) WRITE(LN,100)
     *IT(1:1),TEXT(11:20),T(4),T(5),T(6),A(12),U(12)
            IF(U(12)*U(2).LT.0.AND.A(12).GT.180.0) WRITE(LN,100)
     *IT(2:2),TEXT(11:20),T(4),T(5),T(6),A(12),U(12)
            IF(A(12).GT.180.0.AND.A(2).LT.180.0.AND.A(2).NE.0) 
     *WRITE(LN,100)
     *IT(3:3),TEXT(11:20),T(4),T(5),T(6),A(12),U(12)
            U(2)=U(12)
            A(2)=A(12)
!***********-МАРС-**********************************************
            CALL MARS(JD,LAMB,BET,ALF,DEL,DZ,DSE,DSP)
            JD=JD-DZ*5.0D0/864.0D0
            CALL MARS(JD,LAMB,BET,ALF,DEL,DZ,DSE,DSP)
            JD=JD+DZ*5.0D0/864.0D0
            IF(ALF.LT.0) ALF=ALF+2.0D0*PI
            IF(IFLAG.EQ.0) THEN
            CALL GRADUS(SNGL(180.0D0*ALF/PI)/15.0,ICD(1),ICD(2),ICD(3))
            CALL GRADUS(SNGL(180.0D0*DEL/PI),ICD(4),ICD(5),ICD(6))
            WRITE(LN,101) TEXT(21:30),ICD,2*DSP,DSE
            ENDIF
            H=SNGL(Q)+DGT-SNGL(ALF)
            A(13)=180.0*(PI0+ATAN2(SIN(H),(COS(H)*SIN(SHR)-
     *TAN(SNGL(DEL))*COS(SHR))))/PI0
            SINH=SIN(SHR)*SIN(SNGL(DEL))+COS(SHR)*COS(SNGL(DEL))*COS(H)
            U(13)=180.0*ATAN(SINH/SQRT(1.0-SINH**2))/PI0
            IF(II(3).EQ.3.AND.IFLAG.NE.0) WRITE(LN,100)
     *IT(4:4),TEXT(21:30),T(4),T(5),T(6),A(13),U(13)
            IF(U(13)*U(3).LT.0.AND.A(13).LT.180.0) WRITE(LN,100)
     *IT(1:1),TEXT(21:30),T(4),T(5),T(6),A(13),U(13)
            IF(U(13)*U(3).LT.0.AND.A(13).GT.180.0) WRITE(LN,100)
     *IT(2:2),TEXT(21:30),T(4),T(5),T(6),A(13),U(13)
            IF(A(13).GT.180.0.AND.A(3).LT.180.0.AND.A(3).NE.0) 
     *WRITE(LN,100)
     *IT(3:3),TEXT(21:30),T(4),T(5),T(6),A(13),U(13)
            U(3)=U(13)
            A(3)=A(13)
***********-ЮПИТЕР-**********************************************
            CALL UPITER(JD,LAMB,BET,ALF,DEL,DZ,DSE,DSP)
            JD=JD-DZ*5.0D0/864.0D0
            CALL UPITER(JD,LAMB,BET,ALF,DEL,DZ,DSE,DSP)
            JD=JD+DZ*5.0D0/864.0D0
            IF(ALF.LT.0) ALF=ALF+2.0D0*PI
            IF(IFLAG.EQ.0) THEN
            CALL GRADUS(SNGL(180.0D0*ALF/PI)/15.0,ICD(1),ICD(2),ICD(3))
            CALL GRADUS(SNGL(180.0D0*DEL/PI),ICD(4),ICD(5),ICD(6))
            WRITE(LN,101) TEXT(31:40),ICD,2*DSE,1.0
            ENDIF
            H=SNGL(Q)+DGT-SNGL(ALF)
            A(14)=180.0*(PI0+ATAN2(SIN(H),(COS(H)*SIN(SHR)-
     *TAN(SNGL(DEL))*COS(SHR))))/PI0
            SINH=SIN(SHR)*SIN(SNGL(DEL))+COS(SHR)*COS(SNGL(DEL))*COS(H)
            U(14)=180.0*ATAN(SINH/SQRT(1.0-SINH**2))/PI0
            IF(II(4).EQ.4.AND.IFLAG.NE.0) WRITE(LN,100)
     *IT(4:4),TEXT(31:40),T(4),T(5),T(6),A(14),U(14)
            IF(U(14)*U(4).LT.0.AND.A(14).LT.180.0) WRITE(LN,100)
     *IT(1:1),TEXT(31:40),T(4),T(5),T(6),A(14),U(14)
            IF(U(14)*U(4).LT.0.AND.A(14).GT.180.0) WRITE(LN,100)
     *IT(2:2),TEXT(31:40),T(4),T(5),T(6),A(14),U(14)
            IF(A(14).GT.180.0.AND.A(4).LT.180.0.AND.A(4).NE.0) 
     *WRITE(LN,100)
     *IT(3:3),TEXT(31:40),T(4),T(5),T(6),A(14),U(14)
            U(4)=U(14)
            A(4)=A(14)
***********-САТУРН-**********************************************
            CALL SATURN(JD,LAMB,BET,ALF,DEL,DZ,DSE,DSP)
            JD=JD-DZ*5.0D0/864.0D0
            CALL SATURN(JD,LAMB,BET,ALF,DEL,DZ,DSE,DSP)
            JD=JD+DZ*5.0D0/864.0D0
            IF(ALF.LT.0) ALF=ALF+2.0D0*PI
            IF(IFLAG.EQ.0) THEN
            CALL GRADUS(SNGL(180.0D0*ALF/PI)/15.0,ICD(1),ICD(2),ICD(3))
            CALL GRADUS(SNGL(180.0D0*DEL/PI),ICD(4),ICD(5),ICD(6))
            WRITE(LN,101) TEXT(41:50),ICD,2*DSE,1.0
            ENDIF
            H=SNGL(Q)+DGT-SNGL(ALF)
            A(15)=180.0*(PI0+ATAN2(SIN(H),(COS(H)*SIN(SHR)-
     *TAN(SNGL(DEL))*COS(SHR))))/PI0
            SINH=SIN(SHR)*SIN(SNGL(DEL))+COS(SHR)*COS(SNGL(DEL))*COS(H)
            U(15)=180.0*ATAN(SINH/SQRT(1.0-SINH**2))/PI0
            IF(II(5).EQ.5.AND.IFLAG.NE.0) WRITE(LN,100)
     *IT(4:4),TEXT(41:50),T(4),T(5),T(6),A(15),U(15)
            IF(U(15)*U(5).LT.0.AND.A(15).LT.180.0) WRITE(LN,100)
     *IT(1:1),TEXT(41:50),T(4),T(5),T(6),A(15),U(15)
            IF(U(15)*U(5).LT.0.AND.A(15).GT.180.0) WRITE(LN,100)
     *IT(2:2),TEXT(41:50),T(4),T(5),T(6),A(15),U(15)
            IF(A(15).GT.180.0.AND.A(5).LT.180.0.AND.A(5).NE.0) 
     *WRITE(LN,100)
     *IT(3:3),TEXT(41:50),T(4),T(5),T(6),A(15),U(15)
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
            IF(IFLAG.EQ.0) THEN
            X=180.0*ALF0/PI0
            IF(X.LT.0) X=X+360.0
            CALL GRADUS(X/15.0,ICD(1),ICD(2),ICD(3))
            CALL GRADUS(180.0*DEL0/PI0,ICD(4),ICD(5),ICD(6))
            WRITE(LN,101) TEXT(51:60),ICD,32.5,FAS
            ENDIF
            H=SNGL(Q)+DGT-ALF0
            A(16)=180.0*(PI0+ATAN2(SIN(H),(COS(H)*SIN(SHR)-
     *TAN(DEL0)*COS(SHR))))/PI0
            SINH=SIN(SHR)*SIN(DEL0)+COS(SHR)*COS(DEL0)*COS(H)
            U(16)=180.0*ATAN(SINH/SQRT(1.0-SINH**2))/PI0
            IF(II(6).EQ.6.AND.IFLAG.NE.0) WRITE(LN,100)
     *IT(4:4),TEXT(51:60),T(4),T(5),T(6),A(16),U(16)
            IF((U(16)+0.831)*(U(6)+0.831).LT.0.AND.U(6).NE.0.
     *AND.A(16).LT.180.0)
     *WRITE(LN,100) IT(1:1),TEXT(51:60),T(4),T(5),T(6),A(16),U(16)
            IF((U(16)+0.831)*(U(6)+0.831).LT.0.AND.U(6).NE.0.
     *AND.A(16).GT.180.0)
     *WRITE(LN,100) IT(2:2),TEXT(51:60),T(4),T(5),T(6),A(16),U(16)
            IF(A(16).GT.180.0.AND.A(6).LT.180.0.AND.A(6).NE.0) 
     *WRITE(LN,100)
     *IT(3:3),TEXT(51:60),T(4),T(5),T(6),A(16),U(16)
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
            IF(IFLAG.EQ.0) THEN
            CALL GRADUS(180.0*ALF0/PI0/15.0,ICD(1),ICD(2),ICD(3))
            CALL GRADUS(180.0*DEL0/PI0,ICD(4),ICD(5),ICD(6))
            WRITE(LN,101) TEXT(61:70),ICD,32.5,1.0
            WRITE(LN,'(''******************************************'')')
            ENDIF
            H=SNGL(Q)+DGT-ALF0
            A(17)=180.0*(PI0+ATAN2(SIN(H),(COS(H)*SIN(SHR)-
     *TAN(DEL0)*COS(SHR))))/PI0
            SINH=SIN(SHR)*SIN(DEL0)+COS(SHR)*COS(DEL0)*COS(H)
            U(17)=180.0*ATAN(SINH/SQRT(1.0-SINH**2))/PI0
            IF(II(7).EQ.7.AND.IFLAG.NE.0) WRITE(LN,100)
     *IT(4:4),TEXT(61:70),T(4),T(5),T(6),A(17),U(17)
            IF((U(17)+0.831)*(U(7)+0.831).LT.0.AND.U(7).NE.0.
     *AND.A(17).LT.180.0)
     *WRITE(LN,100) IT(1:1),TEXT(61:70),T(4),T(5),T(6),A(17),U(17)
            IF((U(17)+0.831)*(U(7)+0.831).LT.0.AND.U(7).NE.0.
     *AND.A(17).GT.180.0)
     *WRITE(LN,100) IT(2:2),TEXT(61:70),T(4),T(5),T(6),A(17),U(17)
            IF(A(17).GT.180.0.AND.A(7).LT.180.0.AND.A(7).NE.0) 
     *WRITE(LN,100)
     *IT(3:3),TEXT(61:70),T(4),T(5),T(6),A(17),U(17)
            U(7)=U(17)
            A(7)=A(17)
***********-ИСТОЧНИК-**********************************************
            ALF0=PI0*CI1/180.0
            DEL0=PI0*CI2/180.
            H=SNGL(Q)+DGT-ALF0
            A(18)=180.0*(PI0+ATAN2(SIN(H),(COS(H)*SIN(SHR)-
     *TAN(DEL0)*COS(SHR))))/PI0
            SINH=SIN(SHR)*SIN(DEL0)+COS(SHR)*COS(DEL0)*COS(H)
            U(18)=180.0*ATAN(SINH/SQRT(1.0-SINH**2))/PI0
            IF(II(8).EQ.8.AND.IFLAG.NE.0) WRITE(LN,100)
     *IT(4:4),TEXT(71:80),T(4),T(5),T(6),A(18),U(18)
            IF(U(18)*U(8).LT.0.AND.A(18).LT.180.0) WRITE(LN,100)
     *IT(1:1),TEXT(71:80),T(4),T(5),T(6),A(18),U(18)
            IF(U(18)*U(8).LT.0.AND.A(18).GT.180.0) WRITE(LN,100)
     *IT(2:2),TEXT(71:80),T(4),T(5),T(6),A(18),U(18)
            IF(A(18).GT.180.0.AND.A(8).LT.180.0.AND.A(8).NE.0) 
     *WRITE(LN,100)
     *IT(3:3),TEXT(71:80),T(4),T(5),T(6),A(18),U(18)
            U(8)=U(18)
            A(8)=A(18)
            IFLAG=1
            JT=JT+DSHAG/86400.0+0.000005
            IF(JT.GT.1.0D0) THEN
            JT=0.0D0
            IFLAG=0
            WRITE(*,'(''**********'',6I4)') T
            ENDIF
            JD=JD+DSHAG/86400.0+0.000005
            CALL JDDATA(JD,T(1),T(2),T(3),T(4),T(5),T(6))
      ENDDO
            CLOSE(1)
            END
