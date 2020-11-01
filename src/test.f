      program test
            DOUBLE PRECISION JD,JD0,JD00,JDKON,LAMB,BET,PAR,ALF,DEL,
     *DZ,DEN,R,S(3),V,E,EP,JT,PI,Q0,Q
            REAL*4 A(20),U(20),X
            INTEGER*2 T(6),ICD(6),II(8)
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
            WRITE(*,'('' ,1-ФАЙЛ BRddddddddddyyyyyydEGA.DT'')')
            print *, 'РЕЖИМ ВЫВОДА?(0-ЭКРАН,
     *1-ФАЙЛ BREGA.DAT'
            READ(*,*) LN
            WRITE(*,'('' ДАТA И ВРЕМЯ?'')')
            READ(*,*) T
            !CALL DATAJD(T(1),T(2),T(3),T(4),T(5),T(6),JD)
            WRITE(*,'('' ИНТЕРВАЛ ПРОГНОЗA?'')')
            READ(*,*) DEN
            WRITE(*,'('' ШАГ ВРЕМЕНИ?'')')
            READ(*,*) DSHAG
            WRITE(*,'('' НОМЕР СВЕТИЛА?''/1X,A10,''- 1''/
     *    1X,A10,''- 2''/1X,A10,''- 3''/1X,A10,''- 4''/
     *    1X,A10,''- 5''/1X,A10''- 6''/1X,A10,''- 7''/
     *    1X,A10,''- 8'')') TEXT(1:10),TEXT(11:20),TEXT(21:30),
     *    TEXT(31:40),TEXT(41:50),TEXT(51:60),TEXT(61:70),TEXT(71:80)
            
      end
