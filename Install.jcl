//HERC01A JOB 0,'ME',MSGCLASS=A,MSGLEVEL=(1,1),
//  USER=HERC01,PASSWORD=CUL8TR
//S1  EXEC ASMFCL
//ASM.SYSIN DD *
         TITLE 'SNAPFR V1.0.0 - FORMAT STORAGE AREAS FOR PRINTING'
**********************************************************************
**                                                                  **
** SNAPFR  V1.0.0 - FORMAT STORAGE AREAS FOR PRINING A FILE DUMP    **
**                                                                  **
**             COPYRIGHT (C) 2022  EDWARD G LISS                    **
**                     EGLISS4024@GMAIL.COM                         **
**                                                                  **
** THIS PROGRAM IS FREE SOFTWARE: YOU CAN REDISTRIBUTE IT           **
** AND/OR MODIFY IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC       **
** LICENSE AS PUBLISHED BY THE FREE SOFTWARE FOUNDATION,            **
** EITHER VERSION 3 OF THE LICENSE, OR ANY LATER VERSION.           **
**                                                                  **
** THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE          **
** USEFUL, BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED       **
** WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR          **
** PURPOSE.  SEE THE GNU GENERAL PUBLIC LICENSE FOR MORE            **
** DETAILS.                                                         **
**                                                                  **
** PLEASE SEE HTTPS://WWW.GNU.ORG/LICENSES/ FOR A COPY OF THE       **
** GNU GENERAL PUBLIC LICENSE.                                      **
**                                                                  **
**********************************************************************
         PRINT OFF
         MACRO
&N       $TRTAB &FLD,&HX=,&CH=
.**********************************************************************
.*                                                                    *
.*    THIS MACRO GENERATES A BINARY EBCDIC BYTES TO PRINTABLE CHARS   *
.*    AS WELL AS THE TR INSTRUCTIONS TO EXECUTE THE TRANSLATION.      *
.*                                                                    *
.**********************************************************************
         AIF   (T'&FLD NE 'O').GENTR
**********************************************************************
*    TRANSLATE TABLE IS ALWAYS CALLED $TRTABCH.  SECOND PARM TO      *
*    MACRO GIVES TABLE AN ALIAS.                                     *
*    A HEX CHARACTER TRANSLATION TABLE IS ALWAYS $TRTABHX            *
**********************************************************************
$TRTABCH DC    16C'.'                 X'00'=X'0F'
         AIF   (T'&N EQ 'O').CONT
&N       EQU   $TRTABCH
.CONT    ANOP
         DC    16C'.'                 X'10'=X'1F'
         DC    16C'.'                 X'20'=X'2F'
         DC    16C'.'                 X'30'=X'3F'
         DC    CL16' ............(..' X'40'=X'4F'
         DC    CL16'............*)..' X'50'=X'5F'
         DC    CL16'./.........,....' X'60'=X'6F'
         DC    CL16'.............''=.' X'70'=X'7F'
         DC    CL16'.abcdefghi......' X'80'=X'8F'
         DC    CL16'.jklmnopqr......' X'90'=X'9F'
         DC    CL16'..stuvwxyz......' X'A0'=X'AF'
         DC    16C'.'                 X'B0'=X'BF'
         DC    CL16'.ABCDEFGHI......' X'C0'=X'CF'
         DC    CL16'.JKLMNOPQR......' X'D0'=X'DF'
         DC    CL16'..STUVWXYZ......' X'E0'=X'EF'
         DC    CL16'0123456789......' X'F0'=X'FF'
$TRTABHX DC    CL16'0123456789ABCDEF' X'F0'=X'FF'
         MEXIT
.GENTR   ANOP
         AIF   (T'&HX EQ 'O').TRYCH
         AIF   ('&HX' EQ '*').DEFHX
&N       TR    &FLD,&HX
         MEXIT
.DEFHX   ANOP
&N       TR    &FLD,$TRTABHX-X'F0'
         MEXIT
.TRYCH   ANOP
         AIF   (T'&CH EQ 'O').DEF
         AIF   ('&CH' EQ '*').DEF
&N       TR    &FLD,&CH
         MEXIT
.DEF     ANOP
&N       TR    &FLD,$TRTABCH
         MEND
         MACRO
&NAME    ESTART &TYPE=CSECT,&DESC=,&VER=,&BASE=,&REGS=NO,              *
               &PLIF=
         AIF   (T'&TYPE EQ 'O').TYPERR
         AIF   (T'&VER EQ 'O').VERR
         AIF   (T'&DESC EQ 'O').DERR
         AIF   ('&TYPE' EQ 'CSECT').TYPEC
         AIF   ('&TYPE' EQ 'START').TYPES
.TYPERR  MNOTE 12,'*** TYPE MUST BE CSECT OR START ***'
         MEXIT
.VERR    MNOTE 12,'*** VER OMITTED ***'
         MEXIT
.DERR    MNOTE 12,'*** DESC OMITTED ***'
         MEXIT
.TYPEC   ANOP
&NAME    CSECT
         AGO   .OK
.TYPES   ANOP
&NAME    START
.OK      ANOP
         AIF   (T'&REGS EQ 'O').REGOK
         AIF   ('&REGS' EQ 'YES').REGOK
         AIF   ('&REGS' EQ 'NO').REGOK
         MNOTE 12,'*** REGS INVALID ***'
         MEXIT
.REGOK   ANOP
*
         AIF   (T'&PLIF EQ 'O').NOPL1
         MVI   18(15),C'N'            SET NO PLI ENTRY POINT
         B     12(15)                 SKIP HEADER STUFF
         ENTRY &PLIF
&PLIF    MVI   10(15),C'Y'            SET PL1 ENTRY POINT
         BALR  15,0
ID1&SYSNDX B     ID4&SYSNDX.(15)        SKIP HEADER STUFF
ID5&SYSNDX DS   C
IDP&SYSNDX EQU  ID5&SYSNDX-ID1&SYSNDX
         AGO   .NOPL12
.NOPL1   ANOP
ID1&SYSNDX B     ID4&SYSNDX.(15)       BRANCH AROUND IDENT CONSTANTS
.NOPL12  ANOP
         DC    AL1(ID3&SYSNDX-ID2&SYSNDX)
ID2&SYSNDX DC    CL8'&NAME'
         DC    C&VER
         DC    C' &SYSDATE &SYSTIME - '
         DC    C&DESC
         DS    0F
IDS&SYSNDX EQU   *-ID1&SYSNDX
         DS    18F
ID3&SYSNDX DS    0H
         AIF   (T'&REGS EQ 'O').NOREG
         AIF   ('&REGS' EQ 'NO').NOREG
         EREGS
.NOREG   ANOP
ID4&SYSNDX EQU   *-ID1&SYSNDX
*
         STM   14,12,12(13)
*
         LR    5,13                COPY CALLER'S SAVEAREA ADDR
         LA    13,IDS&SYSNDX.(15)  ESTABLISH MY SAVEAREA
         ST    5,4(,13)            BACK CHAIN SAVE AREAS
         ST    13,8(,5)            FORWARD CHAIN SAVE AREAS
         AIF   (T'&BASE EQ 'O').NOBASE
         BALR  &BASE.,0            ESTABLISH BASE REG VALUE
         USING *,&BASE
.NOBASE  ANOP
         AIF   (T'&PLIF EQ 'O').SKIPLI
         CLI   IDP&SYSNDX.(15),C'Y'     SET CC BASED ON PLI INDICATOR
.SKIPLI  ANOP
         MEND
         MACRO
&NAME    ERETURN &RC=
.* RETURN TO SYSTEM WITH RETURN CODE
&NAME    L     13,4(0,13)          RESET TO CALLERS SAVE AREA
         RETURN (14,12),RC=&RC
         MEND
         MACRO
&NAME    EREGS
.* SET UP REGISTER EQU
         AIF   (T'&NAME EQ 'O').NONAME
&NAME    EQU   *
.NONAME  ANOP
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         MEND
         PRINT ON
***********************************************************************
*                                                                     *
*    THIS SUBROUTINE FORMATS STORAGE AREAS IN 3 DIFFERENT FORMATS.    *
*    FIRST PARM IS A FORMAT CODE (1=IBM CORE DUMP FORMAT,             *
*       1=IBM CORE DUMP FORMAT (1 CHARACTER)                          *
*       2=DOUBLE DIGIT HEX W/CHAR ABOVE ON 2 LINES                    *
*                                    A B C D                          *
*                                    C1C2C3C4                         *
*       3=DOUBLE DIGIT HEX W/CHAR ON 3 LINES                          *
*                                    ABCD                             *
*                                    CCCC                             *
*                                    1234                             *
*    SECOND PARM THE LENGTH OF THE AREA TO DUMP - HALFWORD            *
*    THIRD PARM IS THE AREA TO DUMP - FULLWORD                        *
*    FOURTH PARM IS THE FORMATED AREA TO BE RETURNED - 3 LINES        *
*      OF 134 CHARS EACH.                                             *
*    FIFTH PARM IS OPTIONAL OFFSET TO DISPLAY.  IF OMITTED, AREA IS   *
*      IS LEFT BLANK.                                                 *
*                                                                     *
***********************************************************************
*                                                                     *
*  SAMPLE CALLING SEUENCES                                            *
*                                                                     *
*  ASSEMBLER:                                                         *
*          LA    R1,P3                                                *
*          L     R15,=V(SNAPFR)                                       *
*          BALR  R14,R15                                              *
*                                                                     *
* ...                                                                 *
*                                                                     *
* P3       DC    A(T1C)            FORMAT CODE                        *
*          DC    A(T2C)            NUMBER OF BYTES TO DISPLAY         *
*          DC    A(SNAPTST)        AREA TO DISPLAY                    *
*          DC    A(PRTLINE)        PRINT LINE AREA                    *
*          DC    X'80'                                                *
*          DC    AL3(PRTOFFS)      PRINT OFFSET                       *
* T1C      DC    C'3'                                                 *
* T2C      DC    H'120'                                               *
* PRTLINE  DC    CL134' '                                             *
* PRTLINE2 DC    CL134' '                                             *
* PRTLINE3 DC    CL134' '                                             *
* PRTOFFS  DC    F'255'                                               *
*                                                                     *
***********************************************************************
*                                                                     *
*  COBOL:                                                             *
*  WORKING-STORAGE SECTION.                                           *
*  77  WS-FORMAT-TYPE         PIC  X      VALUE '3'.                  *
*  77  WS-BYTE-COUNT          PIC  S9(4)  COMP SYNC VALUE +120.       *
*  01  WS-SNAPFR-PRTLINES.                                            *
*      05  WS-PRTLINE-1       PIC  X(134).                            *
*      05  WS-PRTLINE-2       PIC  X(134).                            *
*      05  WS-PRTLINE-3       PIC  X(134).                            *
*  01  WS-OFFSET              PIC  S9(8)  COMP SYNC VALUE +255.       *
*                                                                     *
*  PROCEDURE DIVISION.                                                *
*                                                                     *
*      CALL 'SNAPFR' USING  WS-FORMAT-TYPE                            *
*                           WS-BYTE-COUNT                             *
*                           WS-AREA-TO DUMP                           *
*                           WS-SNAPFR-PRTLINES                        *
*                           WS-OFFSET.                                *
*                                                                     *
***********************************************************************
*                                                                     *
*  PL/1 F:                                                            *
*     DECLARE  1 WS_FORMAT_TYPE_1  STATIC,                            *
*                2 WS_FORMAT_TYPE      CHAR(1),                       *
*              1 WS_BYTE_COUNT_1  STATIC,                             *
*                2 WS_BYTE_COUNT       FIXED BINARY(15),              *
*              1 WS_SNAPFR_PRTLINES_1 STATIC,                         *
*                2 WS_PRTLINE_1       CHAR(134),                      *
*                2 WS_PRTLINE_2       CHAR(134),                      *
*                2 WS_PRTLINE_3       CHAR(134),                      *
*              1 WS_OFFSET_1 STATIC,                                  *
*                2 WS-OFFSET          FIXED BINARY(31);               *
*                                                                     *
*                                                                     *
*      CALL SNAPFRP (WS_FORMAT_TYPE_1,                                *
*                    WS_BYTE_COUNT_1,                                 *
*                    WS_AREA_TO_DUMP_1,                               *
*                    WS_SNAPFR_PRTLINES_1,                            *
*                    WS_OFFSET_1);                                    *
*                                                                     *
         EJECT
***********************************************************************
*                                                                     *
*    THIS SECTION INITITIALIZES THE PROGRAM AND VALIDATE PARMS        *
*                                                                     *
***********************************************************************
SNAPFR   ESTART TYPE=CSECT,DESC='SNAPPER FORMATER',VER='1.0.0',        *
               REGS=YES,BASE=R12,PLIF=SNAPFRP
         BNE   NOTPL1
         TM    0(R1),X'80'            TEST HIGH ORDER BIT PARM 1
         BO    PARMCNT                BRANCH IF ON
         TM    4(R1),X'80'            TEST HIGH ORDER BIT PARM 2
         BO    PARMCNT                BRANCH IF ON
         TM    8(R1),X'80'            TEST HIGH ORDER BIT PARM 3
         BO    PARMCNT                BRANCH IF ON
         TM    12(R1),X'80'           TEST HIGH ORDER BIT PARM 4
         BO    PL1PARM                BRANCH IF ON
         TM    16(R1),X'80'           TEST HIGH ORDER BIT PARM 5
         BZ    PARMCNT                BRANCH IF MORE THAN 5
PL1PARM  EQU   *
         L     R2,0(,R1)
         L     R2,0(,R2)
         ST    R2,STDPARM1
         L     R2,4(,R1)
         L     R2,0(,R2)
         ST    R2,STDPARM2
         L     R2,8(,R1)
         L     R2,0(,R2)
         ST    R2,STDPARM3
         L     R2,12(,R1)
         L     R2,0(,R2)
         ST    R2,STDPARM4
         TM    12(R1),X'80'
         BZ    DOPARM5
         OI    STDPARM4,X'80'          ONLY 4 PARMS
         B     NOTPL1
DOPARM5  EQU   *
         TM    16(R1),X'80'            IF THE 5TH PARM THE LAST?
         BZ    PARMCNT
         L     R2,16(,R1)
         L     R2,0(,R2)
         ST    R2,STDPARM5
         OI    STDPARM5,X'80'
         LA    R1,STDPARM
         B     NOTPL1
STDPARM  DS    0F
STDPARM1 DC    F'0'
STDPARM2 DC    F'0'
STDPARM3 DC    F'0'
STDPARM4 DC    F'0'
STDPARM5 DC    F'0'
*
NOTPL1   EQU   *
*
*   VERIFY THAT AT LEAST 4 PARMS WERE PASSED
*
*
         MVI   OFFSTWRK,C' '
         MVC   OFFSTWRK+1(L'OFFSTWRK-1),OFFSTWRK
         LM    R2,R6,0(R1)            GET THE PARM LIST
         TM    0(R1),X'80'            TEST HIGH ORDER BIT PARM 1
         BO    PARMCNT                BRANCH IF ON
         TM    4(R1),X'80'            TEST HIGH ORDER BIT PARM 2
         BO    PARMCNT                BRANCH IF ON
         TM    8(R1),X'80'            TEST HIGH ORDER BIT PARM 3
         BO    PARMCNT                BRANCH IF ON
         TM    12(R1),X'80'           TEST HIGH ORDER BIT PARM 4
         BO    PARMOK                 BRANCH IF ON
         TM    16(R1),X'80'           TEST HIGH ORDER BIT PARM 5
         BZ    PARMCNT                BRANCH IF MORE THAN 5
         UNPK  UNPKWRK,0(5,R6)        UNPACK THE OFFSET WORD
         $TRTAB UNPKWRK(8),HX=*
         MVC   OFFSTWRK,UNPKWRK       MOVE THE OFFSET WORD UNPACKED
         B     PARMOK
PARMCNT  WTO   'SNAPFR - INCORRECT PARM COUNT',ROUTCDE=(2,11)
         ABEND 1,,,USER
PARMOK   EQU   *
         CLI   0(R2),C'1'             ONE LINE
         BE    LINE1OK                YES, BRANCH
         CLI   0(R2),C'2'             TWO LINE
         BE    LINE2OK                YES, BRANCH
         CLI   0(R2),C'3'             THREE LINE
         BE    LINE3OK                YES, BRANCH
NOT3LN   EQU   *
         WTO   'SNAPFR - INVALID FUNCTION CODE',ROUTCDE=(2,11)
         ABEND 2,,,USER
LINE1OK  EQU   *
         LA    R11,TYPE1
         B     LINESOK
LINE2OK  EQU   *
         LA    R11,TYPE2
         B     LINESOK
LINE3OK  EQU   *
         LA    R11,TYPE3
LINESOK  EQU   *
         USING TYPECON,R11
*---------------------------------------------------------------------*
* VERIFY THE PARM LENGTH                                              *
*---------------------------------------------------------------------*
         LH    R6,0(0,R3)              GET DATA LENGTH
         LTR   R6,R6
         BZ    LENBAD
         CH    R6,TYPMXLEN
         BNH   LENOK                   IF SO, BRANCH
LENBAD   WTO   'SNAPFR - INCORRECT DATA LENGTH',ROUTCDE=(2,11)
         ABEND 3,,,USER
LENOK    EQU   *
*---------------------------------------------------------------------*
* NOW FORMAT THE DATA AREA   R4=SOURCE, R5=DESTINATION, R6=LEN,       *
*                            R7=CHAR DEST.                            *
*---------------------------------------------------------------------*
         MVI   0(R5),C' '             CLEAR OUTPUT LINE
         MVC   1(133,R5),0(R5)              TO SPACES
         MVC   1(8,R5),OFFSTWRK       MOVE THE OFFSET WORD UNPACKED
         CLI   0(R2),C'1'             1 LINE FORMAT?
         BNE   NOT1LN                 NO, BRANCH
         LA    R7,90(,R5)
         MVI   89(R5),C'*'
         MVI   122(R5),C'*'
         LA    R5,10(,R5)             ADJUST FOR ASA CC AND OFFSET
UNPKLUP1 EQU   *
         LA    R8,3
         LA    R9,7
         CH    R6,=H'3'
         BH    FULLMV
         LR    R8,R6
         SH    R8,=H'1'
         LR    R9,R6
         AR    R9,R6
         SH    R9,=H'1'
FULLMV   EQU   *
         EX    R8,MVC1LN1             MOVE THE RAW DATA TO CHAR DUMP
         $TRTAB 0(4,R7),CH=*          TRANSLATE
         UNPK  UNPKWRK,0(5,R4)        UNPACK THE FIRST WORD
         $TRTAB UNPKWRK(8),HX=*
         EX    R9,MVC1LN2
         LA    R4,4(,R4)              NEXT WORD
         LA    R5,9(,R5)              NEXT HEX DISPLAY
         LA    R7,4(,R7)              NEXT CHARACTER DISPLAY
         SH    R6,=H'4'
         BH    UNPKLUP1
         B     EXIT
MVC1LN1  MVC   0(0,R7),0(R4)          MOVE THE RAW DATA TO CHAR DUMP
MVC1LN2  MVC   0(0,R5),UNPKWRK
NOT1LN   EQU   *
         MVC   134(134,R5),0(R5)      CLEAR LINE 2 TO SPACES
         CLI   0(R2),C'2'             2 LINE FORMAT?
         BNE   NOT2LN                 NO, BRANCH
         LA    R5,10(,R5)             ADJUST FOR ASA CC AND OFFSET
         LR    R7,R5
         LA    R5,134(,R5)            POSITION ON NEXT LINE
UNPKLUP2 EQU   *
         MVC   0(1,R7),0(R4)          MOVE THE RAW DATA TO CHAR DUMP
         $TRTAB 0(1,R7),CH=*          TRANSLATE
         UNPK  UNPKWRK(3),0(2,R4)     UNPACK THE FIRST WORD
         $TRTAB UNPKWRK(2),HX=*
         MVC   0(2,R5),UNPKWRK
         LA    R4,1(,R4)              NEXT BYTE
         LA    R5,2(,R5)              NEXT HEX DISPLAY
         LA    R7,2(,R7)              NEXT CHARACTER DISPLAY
         BCT   R6,UNPKLUP2
         B     EXIT
NOT2LN   EQU   *
         MVC   268(134,R5),0(R5)      CLEAR LINE 3 TO SPACES
         CLI   0(R2),C'3'             2 LINE FORMAT?
         BNE   NOT3LN                 NO, BRANCH
         LA    R5,10(,R5)             ADJUST FOR ASA CC AND OFFSET
         LR    R7,R5
         LA    R5,134(,R5)            POSITION ON NEXT LINE
UNPKLUP3 EQU   *
         MVC   0(1,R7),0(R4)          MOVE THE RAW DATA TO CHAR DUMP
         $TRTAB 0(1,R7),CH=*          TRANSLATE
         UNPK  UNPKWRK(3),0(2,R4)     UNPACK THE FIRST WORD
         $TRTAB UNPKWRK(2),HX=*
         MVC   0(1,R5),UNPKWRK
         MVC   134(1,R5),UNPKWRK+1
         LA    R4,1(,R4)              NEXT BYTE
         LA    R5,1(,R5)              NEXT HEX DISPLAY
         LA    R7,1(,R7)              NEXT CHARACTER DISPLAY
         BCT   R6,UNPKLUP3
         B     EXIT
*---------------------------------------------------------------------*
* RETURN TO CALLER PROGRAM WITH ZERO RETURN CODE 0                    *
*---------------------------------------------------------------------*
EXIT     EQU   *
         ERETURN RC=0
         LTORG
*---------------------------------------------------------------------*
* COMMON WORK AREAS                                                   *
*---------------------------------------------------------------------*
OFFSTWRK DS    CL8
UNPKWRK  DS    CL9
         $TRTAB
*---------------------------------------------------------------------*
* CALL TYPE DEPENDENT CONSTANTS                                       *
*---------------------------------------------------------------------*
TYPE1    DS    0F
         DC    H'32'            TYPE 1 MAX BYTES PER CALL
TYPE2    DS    0F
         DC    H'60'            TYPE 2 MAX BYTES PER CALL
TYPE3    DS    0F
         DC    H'120'           TYPE 3 MAX BYTES PER CALL
TYPECON  DSECT
TYPMXLEN DS    H                MAX BYTES PER CALL
         END   SNAPFR
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=SHR,SPACE=
//LKED.SYSIN DD *
  ALIAS SNAPFRP
  NAME SNAPFR(R)
//*
//S2  EXEC ASMFCL,PARM.LKED='XREF,LET,LIST'
//ASM.SYSIN DD *
         TITLE 'FILEDUMP V1.0.0 - PRINT FILE DUMPS ****'
**********************************************************************
**                                                                  **
** FILEDUMP V1.0.0 - PRINT A FORMATED DUMP OF A SEQUENTIAL FILE     **
**                                                                  **
**             COPYRIGHT (C) 2022  EDWARD G LISS                    **
**                     EGLISS4024@GMAIL.COM                         **
**                                                                  **
** THIS PROGRAM IS FREE SOFTWARE: YOU CAN REDISTRIBUTE IT           **
** AND/OR MODIFY IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC       **
** LICENSE AS PUBLISHED BY THE FREE SOFTWARE FOUNDATION,            **
** EITHER VERSION 3 OF THE LICENSE, OR ANY LATER VERSION.           **
**                                                                  **
** THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE          **
** USEFUL, BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED       **
** WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR          **
** PURPOSE.  SEE THE GNU GENERAL PUBLIC LICENSE FOR MORE            **
** DETAILS.                                                         **
**                                                                  **
** PLEASE SEE HTTPS://WWW.GNU.ORG/LICENSES/ FOR A COPY OF THE       **
** GNU GENERAL PUBLIC LICENSE.                                      **
**                                                                  **
**********************************************************************
         PRINT OFF
         MACRO
&NAME    ESTART &TYPE=CSECT,&DESC=,&VER=,&BASE=,&REGS=NO,              *
               &PLIF=
         AIF   (T'&TYPE EQ 'O').TYPERR
         AIF   (T'&VER EQ 'O').VERR
         AIF   (T'&DESC EQ 'O').DERR
         AIF   ('&TYPE' EQ 'CSECT').TYPEC
         AIF   ('&TYPE' EQ 'START').TYPES
.TYPERR  MNOTE 12,'*** TYPE MUST BE CSECT OR START ***'
         MEXIT
.VERR    MNOTE 12,'*** VER OMITTED ***'
         MEXIT
.DERR    MNOTE 12,'*** DESC OMITTED ***'
         MEXIT
.TYPEC   ANOP
&NAME    CSECT
         AGO   .OK
.TYPES   ANOP
&NAME    START
.OK      ANOP
         AIF   (T'&REGS EQ 'O').REGOK
         AIF   ('&REGS' EQ 'YES').REGOK
         AIF   ('&REGS' EQ 'NO').REGOK
         MNOTE 12,'*** REGS INVALID ***'
         MEXIT
.REGOK   ANOP
*
         AIF   (T'&PLIF EQ 'O').NOPL1
         MVI   18(15),C'N'            SET NO PLI ENTRY POINT
         B     12(15)                 SKIP HEADER STUFF
         ENTRY &PLIF
&PLIF    MVI   10(15),C'Y'            SET PL1 ENTRY POINT
         BALR  15,0
ID1&SYSNDX B     ID4&SYSNDX.(15)        SKIP HEADER STUFF
ID5&SYSNDX DS   C
IDP&SYSNDX EQU  ID5&SYSNDX-ID1&SYSNDX
         AGO   .NOPL12
.NOPL1   ANOP
ID1&SYSNDX B     ID4&SYSNDX.(15)       BRANCH AROUND IDENT CONSTANTS
.NOPL12  ANOP
         DC    AL1(ID3&SYSNDX-ID2&SYSNDX)
ID2&SYSNDX DC    CL8'&NAME'
         DC    C&VER
         DC    C' &SYSDATE &SYSTIME - '
         DC    C&DESC
         DS    0F
IDS&SYSNDX EQU   *-ID1&SYSNDX
         DS    18F
ID3&SYSNDX DS    0H
         AIF   (T'&REGS EQ 'O').NOREG
         AIF   ('&REGS' EQ 'NO').NOREG
         EREGS
.NOREG   ANOP
ID4&SYSNDX EQU   *-ID1&SYSNDX
*
         STM   14,12,12(13)
*
         LR    5,13                COPY CALLER'S SAVEAREA ADDR
         LA    13,IDS&SYSNDX.(15)  ESTABLISH MY SAVEAREA
         ST    5,4(,13)            BACK CHAIN SAVE AREAS
         ST    13,8(,5)            FORWARD CHAIN SAVE AREAS
         AIF   (T'&BASE EQ 'O').NOBASE
         BALR  &BASE.,0            ESTABLISH BASE REG VALUE
         USING *,&BASE
.NOBASE  ANOP
         AIF   (T'&PLIF EQ 'O').SKIPLI
         CLI   IDP&SYSNDX.(15),C'Y'     SET CC BASED ON PLI INDICATOR
.SKIPLI  ANOP
         MEND
         MACRO
&NAME    ERETURN &RC=
.* RETURN TO SYSTEM WITH RETURN CODE
&NAME    L     13,4(0,13)          RESET TO CALLERS SAVE AREA
         RETURN (14,12),RC=&RC
         MEND
         MACRO
&NAME    EREGS
.* SET UP REGISTER EQU
         AIF   (T'&NAME EQ 'O').NONAME
&NAME    EQU   *
.NONAME  ANOP
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         MEND
         PRINT ON
***********************************************************************
*                                                                     *
*  FILEDUMP - PROGRAM TO FORMAT AND PRINT SEQUENTIAL FILE DUMPS.      *
*                                                                     *
*    THIS PROGRAM READ FIXED OR VARIABLE LENGTH RECORD DATASETS       *
*    USING THE SNAPFR SUBROUTINE.                                     *
*                                                                     *
*    THESE RECFM PRINTS ARE SUPPORTED - F,FA,FBA,V,VA,VBA             *
*                                                                     *
*  REQUIRED DD STATEMENTS:                                            *
*    SYSPRINT   RECFM=VBA,LRECL=121,BLKSIZE=1210                      *
*    SYSUT1     DCB FROM FROM INPUT DATASET                           *
*    SYSUT2     RECFM=FBA,LRECL=133,BLKSIZE=133                       *
*                                                                     *
***********************************************************************
*                                                                     *
* THIS CODE IS PLACED IN THE PUBLIC DOMAIN AND MAY BE FREELY USED     *
* AND INCORPORATED INTO DERIVED WORKS AS LONG AS ATTRIBUTION TO THE   *
* ORIGINAL AUTHORSHIP REMAINS IN ANY DISTRIBUTED COPIES OF THE ALC    *
* SOURCE.                                                             *
*                                                                     *
* RELEASE SEPTEMBER, 2008  BY ED LISS                                 *
*                                                                     *
***********************************************************************
         MACRO
&SNP     SNAPARM &FMTCD,&FMTLN,&FMTADR,&FMTLIN,&FMTOFS
**
**    WORK AREAS FOR PARMS TO SNAPRF
**
&SNP.RFP  DS    0F              EXPECTED DATA ITEMS
&SNP.RFMT DC    A(&FMTCD)          CL1    FORMAT CODE (1,2, OR 3)
&SNP.RFLN DC    A(&FMTLN)          H      NUMBER OF BYTES TO DISPLAY
&SNP.RADR DC    A(&FMTADR)         A      AREA TO DISPLAY
&SNP.ROUT DC    A(&FMTLIN)         3CL134 PRINT LINE(S) AREA
&SNP.ROFS DC    X'80'
          DC    AL3(&FMTOFS)       F      PRINT OFFSET
         MEND
FILEDUMP ESTART DESC='FILEDUMP PGM',VER='1.0.0',BASE=R12,REGS=YES
         LR    R8,R1                  GET PARM ADDRESS
         L     R8,0(,R8)
         MVC   HDRPARM,2(R8)          SAVE THE PARM
         LH    R2,0(R8)               GET THE PARM LENGTH
         LA    R3,HDRPARM(R2)
         MVI   0(R3),C''''            FLAG THE END OF PARM
         LH    R3,HDRLN1
         AR    R3,R2
         LA    R3,2(,R3)              ADD FOR THE ENDING QUOTE
         STH   R3,HDRLN1
         OPEN  (SYSPRINT,(OUTPUT))    OPEN SYSPRINT
         PUT   SYSPRINT,HDRLN1          AND PRINT HEADINGS
         PUT   SYSPRINT,HDRLN2
         LH    R1,0(,R8)
         CH    R1,=H'1'
         BL    PARMERR
         CLI   2(R8),C'1'
         BL    PARMERR
         CLI   2(R8),C'3'
         BH    PARMERR
         MVC   FMTCD,2(R8)
         CH    R1,=H'1'               ONLY FORMAT PASSED?
         BE    DEFAULTS                  IF SO, USED DEFAULTS
*
*   EXTRACT THE NUMBER OF RECORDS TO SKIP IF PRESENT
*
         CLI   3(R8),C','             SECOND POS OF PARM MUST BE ,
         BNE   PARMERR
         CLI   4(R8),C','             NO SKIP GIVEN?
         BE    NOSKIP                   BRANCH
         LA    R8,4(R8)               ADJUST PARM CHAR.
         LR    R9,R8
         LA    R10,9
FIND1    EQU   *
         CLI   0(R9),X'00'            CHECK FOR END OF PARM
         BE    ENDSKIP
         CLI   0(R9),C','             CHECK FOR COMMA
         BE    ENDSKIP
         CLI   0(R9),C'0'             VALIDATE DIGIS
         BL    BADSKIP
         CLI   0(R9),C'9'
         BH    BADSKIP
         LA    R9,1(,R9)
         BCT   R10,FIND1
         PUT   SYSPRINT,ERRLN3        DATA TOO LONG
         B     ABORT16                SKIP EXCEED MAX CHARS
BADSKIP  EQU   *
         PUT   SYSPRINT,ERRLN4        DATA TOO LONG
         B     ABORT16                SKIP HAS BAD CHARS
ENDSKIP  EQU   *
         LR    R11,R9                 SAVE ADDRESS OF , ENDING SKIP
         SR    R9,R8                  COMPUTE LENGTH OF SKIP
         BCTR  R9,R0
         LA    R10,WORKNUM+WORKNUML-1
         SR    R10,R9
         EX    R9,MVCNUM             MOVE THE SKIP NUM TO
         MVC   HDRSKIP,WORKNUM
         PACK  DWORD,WORKNUM
         ZAP   SKIPRECS,DWORD
         B     TRYDUMP
NOSKIP   EQU   *
         LA    R11,4(,R8)
*
*   EXTRACT THE NUMBER OF RECORDS TO DUMP IF PRESENT
*
TRYDUMP  EQU   *
         LA    R9,1(,R11)               ADJUST FOR DUMP COUNT
         CLI   0(R9),X'00'            NO DUMP GIVEN?
         BE    NODUMP                   BRANCH
         CLI   0(R9),C','             NO DUMP GIVEN?
         BE    NODUMP                   BRANCH
         LR    R8,R9
         LA    R10,9
FIND2    EQU   *
         CLI   0(R9),X'00'            CHECK FOR END OF PARM
         BE    ENDDUMP
         CLI   0(R9),C','             CHECK FOR COMMA
         BE    ENDDUMP
         CLI   0(R9),C'0'             VALIDATE DIGIS
         BL    BADDUMP
         CLI   0(R9),C'9'
         BH    BADDUMP
         LA    R9,1(,R9)
         BCT   R10,FIND2
         PUT   SYSPRINT,ERRLN5
         B     ABORT16                DUMP EXCEED MAX CHARS
BADDUMP  EQU   *
         PUT   SYSPRINT,ERRLN6
         B     ABORT16                DUMP BAD CHARS
ENDDUMP  EQU   *
         SR    R9,R8                  COMPUTE LENGTH OF DUMP
         BCTR  R9,R0
         MVI   WORKNUM,C'0'
         MVC   WORKNUM+1(WORKNUML-1),WORKNUM
         LA    R10,WORKNUM+WORKNUML-1
         SR    R10,R9
         EX    R9,MVCNUM             MOVE THE SKIP NUM TO
         MVC   HDRDUMP,WORKNUM
         PACK  DWORD,WORKNUM
         ZAP   MAXRECS,DWORD
         B     DEFAULTS
MVCNUM   MVC   0(0,R10),0(R8)
SKIPRECS DC    PL5'0'
MAXRECS  DC    PL5'0'
NODUMP   EQU   *
DEFAULTS EQU   *
         CLC   HDRSKIP,=CL8' '
         BNE   DEFAULT1
         MVC   HDRSKIP,=CL8'NONE'
DEFAULT1 EQU   *
         CLC   HDRDUMP,=CL8' '
         BNE   DEFAULT2
         MVC   HDRDUMP,=CL8'ALL'
DEFAULT2 EQU   *
         PUT   SYSPRINT,HDRLN3
*
         USING IHADCB,R11             ESTABLISH BASE FOR DCBD
         LA    R11,INDCB
         MVC   DTLDDN,DCBDDNAM         CAPTURE DDNAME FROM INDCB
         OPEN  (INDCB,(INPUT))
         MVC   INLRECL,DCBLRECL        CAPTURE DCB PARMS
         MVC   INBLKSI,DCBBLKSI
         MVC   INRECFM,DCBRECFM
         BAL   R10,PRTDCB
         LA    R11,OUTDCB
         MVC   DTLDDN,DCBDDNAM         CAPTURE DDNAME FROM OUTDCB
         OPEN  (OUTDCB,(OUTPUT))
         MVC   OUTLRECL,DCBLRECL       CAPTURE DCB PARMS
         MVC   OUTBLKSI,DCBBLKSI
         MVC   OUTRECFM,DCBRECFM
         BAL   R10,PRTDCB
         EJECT
***********************************************************************
*
*   GETMAIN STORAGE FOR INPUT RECORD
*
***********************************************************************
         LH    R9,INLRECL              GET INPUT RECSIZE
         ST    R9,GMAINLN
         GETMAIN R,LV=(R9)
         LR    R9,R1
         USING LND,R9
         EJECT
***********************************************************************
*
*   DETERMINE RECFM OF INPUT
*
*   PRINT AN ERROR MESSAGE AND QUIT IF AN INVALID COPY IS ATTEMPTED.
*
***********************************************************************
         LA    R11,INDCB
         TM    DCBRECFM,DCBRECF         RECFM F?
         BO    DUMPF
         TM    DCBRECFM,DCBRECV         RECFM V?
         BO    DUMPF
         PUT   SYSPRINT,ERRLN1
         B     ERREXIT
         EJECT
***********************************************************************
*
*   DUMP RECFM F
*
*   READ, COUNT AND WRITE UNTIL EOF.
*
***********************************************************************
DUMPF    EQU   *
         LA    R4,LN                   CLEAR RECORD AREA TO SPACES
         L     R5,GMAINLN
         LA    R6,SPACE
         L     R7,SPACELN
         MVCL  R4,R6
NEXTF    EQU   *
         GET   INDCB,LN
         AP    RECCNT,=P'1'
         CP    RECCNT,SKIPRECS         BYPASS RECORD
         BNH   NEXTF                     IF BEFORE NUMBER TO SKIP
         CP    MAXRECS,=P'0'
         BE    BYPASS
         CP    RECPRT,MAXRECS
         BNL   MAXHITS
BYPASS   EQU   *
         AP    RECPRT,=P'1'
**                                     CLEAR PRINT AREA TO SPACES
         UNPK  RECNUM(8),RECCNT         AND FORMAT RECORD LINE
         OI    RECNUM+7,X'F0'
         TM    DCBRECFM,DCBRECV         RECFM V?
         BO    GETRECLN
         LH    R5,INLRECL
         B     GOTRECLN
GETRECLN EQU   *
         LH    R5,LN                    GET RECLN FROM RDW
GOTRECLN EQU   *
         CVD   R5,DWORD
         UNPK  RECLEN,DWORD+5(3)
         OI    RECLEN+4,X'F0'
         MVI   FMTLN1,C' '
         MVC   FMTLN1+1(L'FMTLN1-1),FMTLN1
         MVC   FMTLN1(RECHDR1L),RECHDR1
         PUT   OUTDCB,FMTLN1
         MVI   FMTLN1,C' '
         MVC   FMTLN1+1(L'FMTLN1-1),FMTLN1
         PUT   OUTDCB,FMTLN1           PRINT BLANK LINE
*
*  SET UP TO DUMP THE RECORD
*      R4=RECORD POS,   R5=RECORD LEN,  R6=OFFSET
*      R7=MAX CHARS PER PRINT LINE
*
         LA    R4,LN
         LA    R6,0
         CLI   FMTCD,C'1'
         BNE   NOT1
         LA    R7,32
         B     FSTART
NOT1     EQU   *
         CLI   FMTCD,C'2'
         BNE   NOT2
         LA    R7,60
         B     FSTART
NOT2     EQU   *
         LA    R7,120
FSTART   EQU   *
FSHORT   EQU   *
         CR    R5,R7
         BH    FDUMP
         LR    R7,R5               SET UP SHORT LINE
FDUMP    EQU   *
         STH   R7,FMTLN
         ST    R4,SNAPRADR
         ST    R6,FMTOFS
         LA    R1,SNAPRFP
         L     R15,=V(SNAPFR)
         BALR  R14,R15
         PUT   OUTDCB,FMTLN1
         CLI   FMTCD,C'1'
         BE    PRSKIP
         PUT   OUTDCB,FMTLN2
         CLI   FMTCD,C'2'
         BE    PRSKIP
         PUT   OUTDCB,FMTLN3
PRSKIP   EQU   *
         LA    R4,0(R7,R4)            CALC NEXT CHARS TO DUMP
         LA    R6,0(R7,R6)
         SR    R5,R7
         BH    FSHORT
         B     NEXTF
         EJECT
MAXHITS  EQU   *
         PUT   SYSPRINT,FTRLN3
         B     MAXEXIT
EOFIN    EQU   *
         PUT   SYSPRINT,FTRLN1
MAXEXIT  EQU   *
         UNPK  FTRRECS,RECPRT
         OI    FTRRECS+8,X'F0'
         PUT   SYSPRINT,FTRLN2
         CLOSE (SYSPRINT,,INDCB,,OUTDCB)
         ERETURN RC=0
ERREXIT  EQU   *
         PUT   SYSPRINT,ERRLN1
         CLOSE (SYSPRINT,,INDCB,,OUTDCB)
ERREXIT2 EQU   *
PARMERR  EQU   *
         PUT   SYSPRINT,ERRLN2
ABORT16  CLOSE (SYSPRINT)
         ERETURN RC=16
PRTDCB   EQU   *
         TM    DCBRECFM,DCBRECF         RECFM F?
         BO    FIXIN
         TM    DCBRECFM,DCBRECV         RECFM V?
         BO    VARIN
         PUT   SYSPRINT,ERRLN1
         B     ERREXIT
FIXIN    EQU   *
         MVI   DTLRF1,C'F'
         B     BLKTEST
VARIN    EQU   *
         MVI   DTLRF1,C'V'
         B     BLKTEST
BLKTEST  EQU   *
         MVI   DTLRF2,C' '
         TM    DCBRECFM,DCBRECBR        BLOCKED?
         BZ    ASATST
         MVI   DTLRF2,C'B'
ASATST   EQU   *
FMTRECL  EQU   *
         LH    R2,DCBLRECL
         CVD   R2,DWORD
         OI    DWORD+7,X'0F'
         UNPK  DTLRECL,DWORD+5(3)
         LH    R2,DCBBLKSI
         CVD   R2,DWORD
         OI    DWORD+7,X'0F'
         UNPK  DTLBLKSI,DWORD+5(3)
*
         PUT   SYSPRINT,DTLLN1
         BR    R10
DWORD    DS    D
GMAINLN  DS    F
RECCNT   DC    PL5'0'
RECPRT   DC    PL5'0'
INRECFM  DS    CL1
INLRECL  DS    H
INBLKSI  DS    H
OUTRECFM DS    CL1
OUTLRECL DS    H
OUTBLKSI DS    H
SPACELN  DS    0F
SPACE    DC    C' '
         DC    AL3(0)
SNAP     SNAPARM FMTCD,FMTLN,0,FMTLN1,FMTOFS
FMTCD    DS    C
FMTLN    DS    H
FMTOFS   DS    F
FMTLN1   DS    CL134
FMTLN2   DS    CL134
FMTLN3   DS    CL134
RECHDR1  DC    C'0RECORD '
RECNUM   DS    CL8
         DC    C'  LRECL='
RECLEN   DC    CL5'*****'
RECHDR1L EQU   *-RECHDR1
         DC    C' WORKNUM='
WORKNUM  DC    CL8'00000000'
WORKNUML EQU   *-WORKNUM
**
**   SYSPRINT MESSAGES AREA
**
SYSPRINT DCB   DDNAME=SYSPRINT,MACRF=(PM),DSORG=PS,                    *
               RECFM=VBA,LRECL=121,BLKSIZE=1210
********
         DS    0H
HDRLN1   DC    AL2(HDRLN1L)
         DC    AL2(0)
         DC    C'1FILEDUMP V1.0.0'
         DC    C'   PARM='''
HDRLN1L  EQU   *-HDRLN1
HDRPARM  DS    CL102
********
HDRLN2   DC    AL2(HDRLN2L)
         DC    AL2(0)
         DC    C' '
HDRLN2L  EQU   *-HDRLN2
********
HDRLN3   DC    AL2(HDRLN3L)
         DC    AL2(0)
         DC    C' RECORDS TO SKIP '
HDRSKIP  DC    CL8' '
         DC    C',RECORDS TO PROCESS '
HDRDUMP  DC    CL8' '
HDRLN3L  EQU   *-HDRLN3
********
DTLLN1   DC    AL2(DTLLN1L)
         DC    AL2(0)
         DC    C' '
DTLDDN   DC    CL8' '
         DC    CL7' RECFM='
DTLRF1   DC    C' '
DTLRF2   DC    C' '
         DC    C' LRECL='
DTLRECL  DS    CL5
         DC    C' BLKSIZE='
DTLBLKSI DS    CL5
DTLLN1L  EQU   *-DTLLN1
********
ERRLN1   DC    AL2(ERRLN1L)
         DC    AL2(0)
         DC    C' ERROR - INPUT MUST BE RECFM F OR V'
ERRLN1L  EQU   *-ERRLN1
********
ERRLN2   DC    AL2(ERRLN2L)
         DC    AL2(0)
         DC    C' ERROR - EXEC PARM INVALID - MUST BE 1, 2 OR 3'
ERRLN2L  EQU   *-ERRLN2
********
ERRLN3   DC    AL2(ERRLN3L)
         DC    AL2(0)
         DC    C' ERROR - RECORDS TO SKIP TOO LONG'
ERRLN3L  EQU   *-ERRLN3
********
ERRLN4   DC    AL2(ERRLN4L)
         DC    AL2(0)
         DC    C' ERROR - INVALID RECORDS TO SKIP'
ERRLN4L  EQU   *-ERRLN4
********
ERRLN5   DC    AL2(ERRLN5L)
         DC    AL2(0)
         DC    C' ERROR - RECORDS TO DUMP TOO LONG'
ERRLN5L  EQU   *-ERRLN5
********
ERRLN6   DC    AL2(ERRLN6L)
         DC    AL2(0)
         DC    C' ERROR - INVALID RECORDS TO DUMP'
ERRLN6L  EQU   *-ERRLN6
********
FTRLN1   DC    AL2(FTRLN1L)
         DC    AL2(0)
         DC    C' END OF FILE ON SYSUT1'
FTRLN1L  EQU   *-FTRLN1
********
FTRLN2   DC    AL2(FTRLN2L)
         DC    AL2(0)
         DC    C' '
FTRRECS  DC    CL9'        '
         DC    C' RECORD(S) DUMPED'
FTRLN2L  EQU   *-FTRLN2
********
FTRLN3   DC    AL2(FTRLN3L)
         DC    AL2(0)
         DC    C' MAXIMUM RECORDS DUMPED'
FTRLN3L  EQU   *-FTRLN3
         PRINT NOGEN
INDCB    DCB   DDNAME=SYSUT1,MACRF=(GM),EODAD=EOFIN,DSORG=PS
OUTDCB   DCB   DDNAME=SYSUT2,MACRF=(PM),DSORG=PS,                      *
               RECFM=FBA,LRECL=133,BLKSIZE=133
         PRINT GEN
PLN      DC    CL133' '
         LTORG
         DS    0D
LND      DSECT
LN       DS    CL32767
DDCB     DCBD  DSORG=PS
         END   FILEDUMP
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB(FILEDUMP),DISP=SHR
//LKED.SYSLIB DD DSN=SYS2.LINKLIB,DISP=SHR
