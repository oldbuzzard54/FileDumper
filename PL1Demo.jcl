//HERCELP JOB 0,'ME',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//   USER=HERCEL,PASSWORD=CUL8TR
// EXEC PL1LFCG,LIB1='HERCEL.TEST.LOADLIB'
//PL1L.SYSIN DD *
 DEMO:PROC OPTIONS(MAIN);
  /******** DOES NOT WORK LIKE THIS *************
   DECLARE
       (1  WS_FORMAT_TYPE           CHAR(1) STATIC,
        1  WS_BYTE_COUNT            FIXED BIN(15) STATIC,
        1  WS_AREA_TO_DUMP          CHAR(134) STATIC,
        1  WS_SNAPPFR_PRTLINES STATIC,
           2 WS_PRTLINE_1           CHAR(134),
           2 WS_PRTLINE_2           CHAR(134),
           2 WS_PRTLINE_3           CHAR(134),
        1  WS_OFFSET                FIXED BIN(31) STATIC
       ) ALIGNED;
  **********************************************/
   DECLARE
       (1  WS_FORMAT_TYPE_1 STATIC,
           2  WS_FORMAT_TYPE           CHAR(1),
        1  WS_BYTE_COUNT_1 STATIC,
           2  WS_BYTE_COUNT            FIXED BIN(15),
        1  WS_AREA_TO_DUMP_1 STATIC,
           2  WS_AREA_TO_DUMP          CHAR(134),
        1  WS_SNAPPFR_PRTLINES STATIC,
           2 WS_PRTLINE_1              CHAR(134),
           2 WS_PRTLINE_2              CHAR(134),
           2 WS_PRTLINE_3              CHAR(134),
        1  WS_OFFSET_1 STATIC,
           2  WS_OFFSET                FIXED BIN(31)
       ) ALIGNED;

   WS_FORMAT_TYPE='1';
   WS_BYTE_COUNT=32;       /* ARBITRARY VALUE */
   WS_AREA_TO_DUMP='THIS IS A FORMAT 3 SAMPLE';
   WS_OFFSET=0;
   PUT SKIP;
   CALL SNAPFRP(WS_FORMAT_TYPE_1,
                WS_BYTE_COUNT_1,
                WS_AREA_TO_DUMP_1,
                WS_SNAPPFR_PRTLINES,
                WS_OFFSET_1);
   PUT SKIP EDIT(WS_PRTLINE_1) (A(120));
   WS_FORMAT_TYPE='1';
   WS_BYTE_COUNT=18;       /* ARBITRARY VALUE */
   WS_AREA_TO_DUMP=SUBSTR(WS_AREA_TO_DUMP,32);
   WS_OFFSET=32;
   PUT SKIP;
   CALL SNAPFRP(WS_FORMAT_TYPE_1,
                WS_BYTE_COUNT_1,
                WS_AREA_TO_DUMP_1,
                WS_SNAPPFR_PRTLINES,
                WS_OFFSET_1);
   PUT SKIP EDIT(WS_PRTLINE_1) (A(120));
   WS_FORMAT_TYPE='3';
   WS_BYTE_COUNT=50;       /* ARBITRARY VALUE */
   WS_AREA_TO_DUMP='THIS IS A FORMAT 3 SAMPLE';
   WS_OFFSET=0;
   PUT SKIP;
   CALL SNAPFRP(WS_FORMAT_TYPE_1,
                WS_BYTE_COUNT_1,
                WS_AREA_TO_DUMP_1,
                WS_SNAPPFR_PRTLINES,
                WS_OFFSET_1);
   PUT SKIP EDIT(WS_PRTLINE_1) (A(120));
   PUT SKIP EDIT(WS_PRTLINE_2) (A(120));
   PUT SKIP EDIT(WS_PRTLINE_3) (A(120));
 END DEMO;
//GO.SYSUDUMP DD SYSOUT=A
