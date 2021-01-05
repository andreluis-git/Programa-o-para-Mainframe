       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP003.
      *AUTHOR. ANDRE LUIS DE M RODRIGUES.
      **************************************
      *    CADASTRO DE CONVENIOS            *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCONV ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGOCONV
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOMECONV 
                       WITH DUPLICATES
                    ALTERNATE RECORD KEY IS PLANO 
                       WITH DUPLICATES.    
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCONV
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONV.DAT".
       01  REGCONV.
           03  CODIGOCONV          PIC 9(04).
           03  NOMECONV            PIC X(30).
           03  PLANO               PIC 9(02).
       
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-CONT        PIC 9(06) VALUE ZEROS.
       77 W-OPCAO       PIC X(01) VALUE SPACES.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 W-SEL         PIC 9(01) VALUE ZEROS.
       77 MENS          PIC X(50) VALUE SPACES.
       77 LIMPA         PIC X(50) VALUE SPACES.
       01 ST-ERRO       PIC X(02) VALUE "00".

       01 IND                  PIC 9(01) VALUE ZEROS.
      
      *    DISPLAY
       01 DISPPLANO    PIC X(30) VALUE SPACES.
       
      *    CARREGA O VETOR  TABPLANO
       01  TABPLANOX.
           03 FILLER  PIC X(30)   VALUE "ENFERMARIA REGINAL".
           03 FILLER  PIC X(30)   VALUE "ENFERMARIA NACIONAL".
           03 FILLER  PIC X(30)   VALUE "ENFERMARIA INTERNACIONAL".
           03 FILLER  PIC X(30)   VALUE "APTO PADRAO REGINAL".
           03 FILLER  PIC X(30)   VALUE "APTO PADRAO NACIONAL".
           03 FILLER  PIC X(30)   VALUE "APTO PADRAO INTERNACIONAL".
           03 FILLER  PIC X(30)   VALUE "EMERGENCIA REGIONAL".
           03 FILLER  PIC X(30)   VALUE "EMERGENCIA NACIONAL".
           03 FILLER  PIC X(30)   VALUE "EMERCENCIA INTERNACIONAL".
           03 FILLER  PIC X(30)   VALUE "PLANO GLOBAL".
       01  TABPLANO REDEFINES TABPLANOX.
           03  TBPLANO         PIC X(30) OCCURS 10 TIMES.
      *
      *-----------------------------------------------------------------
       SCREEN SECTION.       
       01  TELACONV.
           05  BLANK SCREEN.
           05  LINE 04  COLUMN 01 
               VALUE  "                    CADASTRO DE CONVENIOS".
           05  LINE 08  COLUMN 01 
               VALUE  "        CODIGO            :".
           05  LINE 10  COLUMN 01 
               VALUE  "        NOME              :".
           05  LINE 12  COLUMN 01 
               VALUE  "        PLANO             :".
           05  LINE 22  COLUMN 01 
               VALUE  " F1 => RETORNA AO MENU".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM:".
           05  TCODIGOCONV
               LINE 08  COLUMN 28  PIC 9(04)
               USING  CODIGOCONV
               HIGHLIGHT.
           05  TNOMECONV
               LINE 10  COLUMN 28  PIC X(30)
               USING  NOMECONV
               HIGHLIGHT.
           05  TPLANO
               LINE 12  COLUMN 28  PIC 9(02)
               USING  PLANO
               HIGHLIGHT.

      * DISPLAY ESPECIALISTA
           05  TDISPPLANO
               LINE 12  COLUMN 33  PIC X(30)
               USING DISPPLANO.

           
       01  TELAPLANO.
           05  LINE 11  COLUMN 40 VALUE  "1 - ENFERMARIA REGINAL".
           05  LINE 12  COLUMN 40 VALUE  "2 - ENFERMARIA NACIONAL".
           05  LINE 13  COLUMN 40 VALUE  "3 - ENFERMARIA INTERNACIONAL".
           05  LINE 14  COLUMN 40 VALUE  "4 - APTO PADRÃO REGINAL".
           05  LINE 15  COLUMN 40 VALUE  "5 - APTO PADRAO NACIONAL".
           05  LINE 16  COLUMN 40 
               VALUE  "6 - APTO PADRAO INTERNACIONAL".
           05  LINE 17  COLUMN 40 VALUE  "7 - EMERGENCIA REGIONAL".
           05  LINE 18  COLUMN 40 VALUE  "8 - EMERGENCIA NACIONAL".
           05  LINE 19  COLUMN 40 VALUE  "9 - EMERCENCIA INTERNACIONAL".
           05  LINE 20  COLUMN 40 VALUE  "10 - PLANO GLOBAL".
              
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
       R0.
           OPEN I-O CADCONV
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCONV
                 CLOSE CADCONV
                 MOVE "**** ARQUIVO CADCONV FOI CRIADO ****" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "**** ERRO NA ABERTURA DO ARQUIVO CADCONV ****" 
                     TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      * 
       R1.
           MOVE SPACES TO NOMECONV
           MOVE SPACES TO DISPPLANO
           MOVE ZEROS TO CODIGOCONV PLANO W-SEL
           DISPLAY TELACONV.
       R1A.
           ACCEPT TCODIGOCONV.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
           IF W-ACT = 02
                   GO TO ROT-FIM.
           IF CODIGOCONV = ZEROS
               MOVE "*** CODIGOCONV NAO PODE SER ZEROS ***" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R1A.           

       LER-CADCONV.
           READ CADCONV
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                DISPLAY TELACONV
                MOVE "*** CONVENIO JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADCONV" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.
       R1C.
           ACCEPT TNOMECONV.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R1A.
           IF W-ACT = 02
                   GO TO ROT-FIM.
           IF NOMECONV = SPACES
               MOVE "NOMECONV NAO PODE FICAR EM BRANCO" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R1C.
               
           
       R2.
           DISPLAY TELAPLANO
           ACCEPT TPLANO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R1C.
           IF W-ACT = 02
                   GO TO ROT-FIM.
           IF PLANO > 10 OR PLANO < 1
               MOVE "**** DIGITE APENAS DE 1 A 10 ****" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R2.
           MOVE TBPLANO(PLANO) TO DISPPLANO
           DISPLAY TELACONV.
                   

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R2.
                IF W-ACT = 02
                   GO TO ROT-FIM.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCONV
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "*** CONVENIO JA EXISTE, DADOS NAO GRAVADOS ***" 
                      TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R1
                ELSE
                      MOVE "*ERRO NA GRAVACAO DO ARQUIVO DE CONVENIOS*"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.


      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" AND W-OPCAO NOT = "e"
                        AND W-OPCAO NOT = "n" AND W-OPCAO NOT = "a"
                        GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N" OR W-OPCAO = "n"
                   GO TO R1
                ELSE
                   IF W-OPCAO = "A" OR W-OPCAO = "a"
                      MOVE 1 TO W-SEL
                      GO TO R1C.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADCONV RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO CONVENIO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R2.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCONV
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA ALTERACAO DO REGISTRO CONVENIO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.                


       ROT-FIM.
           CLOSE CADCONV
           EXIT PROGRAM.

      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (23, 12) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 3000
               GO TO ROT-MENS2
           ELSE
              MOVE SPACES TO MENS
              DISPLAY (23, 12) MENS.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-TEMPO.

      *    FILE STATUS
      *    00 = OPERACAO REALIZADO COM SUCESSO
      *    22 = REGISTRO JA CADASTRADO
      *    23 = REGISTRO NAO ENCONTRADO
      *    30 = ARQUIVO NAO ENCONTRADO      