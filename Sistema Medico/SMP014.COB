       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP014.
      ************************
      * RELATORIO DE CEP'S   *
      ************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS ENDERECO
                                   WITH DUPLICATES.
           SELECT CADCEPTX ASSIGN TO DISK
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT".
       01  REGCEP.
           03  CODIGO              PIC 9(08).
           03  ENDERECO            PIC X(30).
           03  BAIRRO              PIC X(30).
           03  CIDADE              PIC X(20).
           03  ESTADO              PIC X(02).
      *
       FD CADCEPTX
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEPRL.DOC".
       01 REGCEPTX    PIC X(100).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL      PIC 9(01) VALUE ZEROS.
       77 W-CONT     PIC 9(06) VALUE ZEROS.
       77 W-OPCAO    PIC X(01) VALUE SPACES.
       77 ST-ERRO    PIC X(02) VALUE "00". 
       77 W-ACT      PIC 9(02) VALUE ZEROS.
       77 MENS       PIC X(50) VALUE SPACES.
       77 LIMPA      PIC X(50) VALUE SPACES.
       01 CONLIN     PIC 9(03) VALUE ZEROS.
       01 CONLIN1    PIC 9(04) VALUE ZEROS.
       01 CONPAG     PIC 9(03) VALUE ZEROS.
      *

       01  CAB1.
           05  FILLER                 PIC X(057) VALUE
           "﻿FATEC - ZONA LESTE                 *** RELACAO DE CEP'".
           05  FILLER                 PIC X(022) VALUE
           "S ***          PAG. : ".
           05  NUMPAG   VALUE ZEROS  PIC 999.
           05  FILLER                 PIC X(011) VALUE
           "    DATA : ".
           05  DATAREL  VALUE ZEROS  PIC 99.99.99.

       01  CAB2.
           05  FILLER                 PIC X(057) VALUE
           "NUM. CEP              LOGRADOURO                      BAI".
           05  FILLER                 PIC X(040) VALUE
           "RRO                 CIDADE            UF".

       01  CAB3.
           05  FILLER                 PIC X(057) VALUE
           "---------    ------------------------------    ----------".
           05  FILLER                 PIC X(040) VALUE
           "----------    --------------------    --".

       01  DET.
           05  CEP-NUMCEPTX  VALUE ZEROS  PIC 99999999.
           05  FILLER                 PIC X(004) VALUE 
           "    ".
           05  CEP-LOGRATX   VALUE SPACES PIC X(030).
           05  FILLER                 PIC X(004) VALUE
           "    ".
           05  CEP-BAIRROTX  VALUE SPACES PIC X(020).
           05  FILLER                 PIC X(004) VALUE
           "    ".
           05  CEP-CIDADETX  VALUE SPACES PIC X(020).
           05  FILLER                 PIC X(004) VALUE
           "    ".
           05  CEP-UFTX  VALUE SPACES PIC X(002).

       01  LINTOT.
           05  FILLER                 PIC X(051) VALUE
           "                          *** TOTAL DE REGISTROS : ".
           05  TOTREG   VALUE ZEROS  PIC ZZZ9.
           05  FILLER                 PIC X(004) VALUE
           " ***".

      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  SMT025.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                *** CONVERSAO DE ARQUIVO".
           05  LINE 02  COLUMN 41 
               VALUE  " DE CEP PARA DOC ***".
           05  LINE 12  COLUMN 01 
               VALUE  "                         CONFIRMA IMPRES".
           05  LINE 12  COLUMN 41 
               VALUE  "SAO (S ou N ) :".
           05  LINE 22  COLUMN 01 
               VALUE  " F1 => RETORNA AO MENU".
           05  LINE 24  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  TW-OPCAO
               LINE 12  COLUMN 57  PIC X(01)
               USING  W-OPCAO
               HIGHLIGHT.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO. 
      *
       INC-001.
                MOVE SPACES TO BAIRRO ESTADO ENDERECO.
                MOVE SPACES TO CIDADE. 
                MOVE ZEROS  TO CODIGO.
                DISPLAY SMT025.
       INC-OPC.
                ACCEPT TW-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   GO TO ROT-FIM.
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
      *
       INC-OP0.
           OPEN INPUT CADCEP
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      MOVE "* ARQUIVO CEP NAO EXISTE *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CEP" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
      *
           OPEN OUTPUT CADCEPTX
           IF ST-ERRO NOT = "00"
                      MOVE "ERRO ABERTURA DO ARQUIVO CADCEPTX" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
       ROT-CABEC. 
           ADD 1 TO CONPAG
           MOVE CONPAG TO NUMPAG
           WRITE REGCEPTX FROM CAB1.
           MOVE SPACES TO REGCEPTX
           WRITE REGCEPTX.
           WRITE REGCEPTX FROM CAB2.
           WRITE REGCEPTX FROM CAB3.
      *
       LER-CEP01.
                READ CADCEP NEXT
                IF ST-ERRO NOT = "00"
                   IF ST-ERRO = "10"
                      MOVE SPACES TO REGCEPTX
                      WRITE REGCEPTX
                      MOVE CONLIN1 TO TOTREG
                      WRITE REGCEPTX FROM LINTOT                     
                      MOVE "*** FIM DO CADCEP ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      MOVE "ERRO NA LEITURA CADCEP"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
           MOVE CODIGO     TO CEP-NUMCEPTX.
           MOVE ENDERECO   TO CEP-LOGRATX.
           MOVE BAIRRO     TO CEP-BAIRROTX.
           MOVE CIDADE     TO CEP-CIDADETX.
           MOVE ESTADO     TO CEP-UFTX.

       INC-WR1.
           WRITE REGCEPTX FROM DET
                IF ST-ERRO NOT = "00"
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO CADCEPTX"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
                MOVE "*** REGISTRO GRAVADOS *** " TO MENS
                ADD 1 TO CONLIN CONLIN1
                IF CONLIN < 34
                      GO TO LER-CEP01
                ELSE
                      MOVE ZEROS TO CONLIN
                      GO TO ROT-CABEC.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
       ROT-FIM.
                DISPLAY (01, 01) ERASE
                CLOSE CADCEP CADCEPTX.
       ROT-FIMP.
                EXIT PROGRAM.

       ROT-FIMS.
                STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.
