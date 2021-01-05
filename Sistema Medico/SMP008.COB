       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP008.
      **************************************************
      * CONSULTA DE ENDERECAMENTO MEDICOS *
      **************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
       SELECT CADMED ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CRM
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME
                                   WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
      *
       FD CADMED
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMED.DAT".
       01  REGMED.
           03  CRM                 PIC 9(06).
           03  NOME                PIC X(30).
           03  ESPECIALIDADE       PIC 9(02).
           03  SEXO                PIC X(01).
           03  DATANASCIMENTO.      
               05  DIA             PIC 9(02).
               05  MES             PIC 9(02).
               05  ANO             PIC 9(04).
           03  EMAIL               PIC X(30).
           03  TELEFONE            PIC 9(09).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-SEL        PIC 9(01) VALUE ZEROS.
       01 W-CONT       PIC 9(04) VALUE ZEROS.
       01 W-OPCAO      PIC X(01) VALUE SPACES.
       01 ST-ERRO      PIC X(02) VALUE "00".
       01 W-ACT        PIC 9(02) VALUE ZEROS.
       01 MENS         PIC X(50) VALUE SPACES.
       01 LIMPA        PIC X(55) VALUE SPACES.
       01 SOLIC        PIC X(20) VALUE SPACES.
       01 CONLIN       PIC 9(03) VALUE 001.
       01 CRM2         PIC 9(06) VALUE ZEROS.
       01 AUX          PIC 9(06) VALUE ZEROS.
       01 PRIM         PIC 9(06) VALUE ZEROS.
      *-----------------------------------------------------------------
      *
      ******************
      * ROTINA DE TELA *
      ******************
      *
       SCREEN SECTION.
       01  SMT041.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                             CONSULTA DE".
           05  LINE 02  COLUMN 41 
               VALUE  " MEDICO".
           05  LINE 05  COLUMN 01 
               VALUE  "    CRM          :".
           05  LINE 07  COLUMN 01 
               VALUE  "    NOME         :".
           05  LINE 09  COLUMN 01 
               VALUE  "    ESPECIALIDADE:".
           05  LINE 11  COLUMN 01 
               VALUE  "    EMAIL        :".
           05  LINE 13  COLUMN 01 
               VALUE  "    TELEFONE     :".
           05  LINE 21  COLUMN 01 
               VALUE  "              OPCAO :   (E=ENCERRA     P".
           05  LINE 21  COLUMN 41 
               VALUE  "=PROXIMO     A=ANTERIOR )".
           05  LINE 22  COLUMN 41 
               VALUE  "   ==>            <==".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  TCRM
               LINE 05  COLUMN 20  PIC 9(06)
               USING  CRM
               HIGHLIGHT.
           05  TNOME
               LINE 07  COLUMN 20  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TESPECIALIDADE
               LINE 09  COLUMN 20  PIC 9(02)
               USING  ESPECIALIDADE
               HIGHLIGHT.
           05  TEMAIL
               LINE 11  COLUMN 20  PIC X(30)
               USING  EMAIL
               HIGHLIGHT.
           05  TTELEFONE
               LINE 13  COLUMN 20  PIC 9(09)
               USING  TELEFONE
               HIGHLIGHT.
           05  TW-OPCAO
               LINE 21  COLUMN 23  PIC X(01)
               USING  W-OPCAO
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP1.
           OPEN INPUT  CADMED
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE CADMED NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO CADMED"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM.
      *
       INC-001.
           MOVE ZEROS TO CRM CRM2 ESPECIALIDADE TELEFONE PRIM
           MOVE SPACES TO NOME EMAIL 
           DISPLAY  SMT041.
       INC-001A.
           ACCEPT TCRM
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE CADMED
                      GO TO ROT-FIM.
           IF W-ACT > 02
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
       INC-RD1. 
           START CADMED KEY IS NOT LESS CRM INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001.
           GO TO INC-RD2.
       INC-RD2.
           READ CADMED NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADMED"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.
       ROT-MONTAR.
              IF PRIM = ZEROS
                 MOVE CRM TO PRIM.
              DISPLAY SMT041.
       ROT-SOL.
           ACCEPT  TW-OPCAO  
           IF W-OPCAO = "E"
                  GO TO ROT-FIM 
           ELSE
             IF W-OPCAO = "P"
                   GO TO INC-RD4
             ELSE
                 IF W-OPCAO NOT = "A"
                     MOVE "*** OPCAO NAO DISPONIVEL ***" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO ROT-SOL
                 ELSE
                   IF CRM = PRIM 
                       MOVE "*** PRIMEIRO REGISTRO ***" TO MENS
                       PERFORM ROT-MENS THRU ROT-MENS-FIM
                       GO TO ROT-SOL.
                   MOVE CRM TO CRM2
                   MOVE ZEROS TO CRM
                   START CADMED KEY IS NOT LESS CRM INVALID KEY
                   MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
       INC-RD3.
           READ CADMED NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADMED"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
               IF CRM < CRM2
                   MOVE CRM TO AUX
                   GO TO INC-RD3
               ELSE
                   MOVE AUX TO CRM      
                   GO TO INC-RD1.
           GO TO ROT-MONTAR.

       INC-RD4.
           START CADMED KEY IS GREATER CRM INVALID KEY
                   MOVE "*** ULTIMO REGISTRO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-SOL.
           GO TO INC-RD2.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
           DISPLAY (01 01) ERASE.
           CLOSE CADMED.
           EXIT PROGRAM.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (23, 13) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 1000
              GO TO ROT-MENS2
           ELSE
              DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-MENS.
      *

      *---------------------*** FIM DE PROGRAMA ***--------------------*