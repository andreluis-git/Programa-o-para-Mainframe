       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADCEP.
      *AUTHOR. ANDRE LUIS DE M RODRIGUES.
      **************************************
      *    CADASTRO DE CEPS            *
      **************************************
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
       01 DISPESTADO    PIC X(20) VALUE SPACES.
       
      *    CARREGA O VETOR  TABESPECIALIDADE
       01  TABESTADOX.
           03 FILLER  PIC X(22)   VALUE "SPSAO PAULO".
           03 FILLER  PIC X(22)   VALUE "RJRIO DE JANEIRO".
           03 FILLER  PIC X(22)   VALUE "PRPARANA".
           03 FILLER  PIC X(22)   VALUE "PBPARAIBA".
           03 FILLER  PIC X(22)   VALUE "SCSANTA CATARINA".  
       01  TABESTADO REDEFINES TABESTADOX.
           03  TBESTADO           PIC X(22) OCCURS 5 TIMES.
       01 TXTESTADO.
          03 TXTESTADO01 PIC X(02) VALUE SPACES.
          03 TXTESTADO02 PIC X(20) VALUE SPACES. 
      *
      *-----------------------------------------------------------------
       SCREEN SECTION.   
       
       01  TELACEP.
           05  BLANK SCREEN.
           05  LINE 05  COLUMN 01 
               VALUE  "                         CADASTRO DE CEP".
           05  LINE 09  COLUMN 01 
               VALUE  "      CODIGO  :".
           05  LINE 11  COLUMN 01 
               VALUE  "      ENDERECO:".
           05  LINE 13  COLUMN 01 
               VALUE  "      BAIRRO  :".
           05  LINE 15  COLUMN 01 
               VALUE  "      CIDADE  :".
           05  LINE 17  COLUMN 01 
               VALUE  "      ESTADO  :".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM:".
           05  TCODIGO
               LINE 09  COLUMN 16  PIC 9(08)
               USING  CODIGO
               HIGHLIGHT.
           05  TENDERECO
               LINE 11  COLUMN 16  PIC X(30)
               USING  ENDERECO
               HIGHLIGHT.
           05  TBAIRRO
               LINE 13  COLUMN 16  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  TCIDADE
               LINE 15  COLUMN 16  PIC X(20)
               USING  CIDADE
               HIGHLIGHT.
           05  TESTADO
               LINE 17  COLUMN 16  PIC X(02)
               USING  ESTADO
               HIGHLIGHT.


      * DISPLAY ESPECIALISTA
           05  TDISPESTADO
               LINE 17  COLUMN 25  PIC X(20)
               USING DISPESTADO.

           
       01  TELAESTADO.
           05  LINE 14  COLUMN 62 VALUE  "SP-SAO PAULO".
           05  LINE 15  COLUMN 62 VALUE  "RJ-RIO DE JANEIRO".
           05  LINE 16  COLUMN 62 VALUE  "PR-PARANA".
           05  LINE 17  COLUMN 62 VALUE  "PB-PARAIBA".
           05  LINE 18  COLUMN 62 VALUE  "SC-SANTA CATARINA".
              
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
       R0.
           OPEN I-O CADCEP
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCEP
                 CLOSE CADCEP
                 MOVE "**** ARQUIVO CADCEP FOI CRIADO ****" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "**** ERRO NA ABERTURA DO ARQUIVO CADCEP ****" 
                     TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      * 
       R1.
           MOVE SPACES TO ENDERECO BAIRRO CIDADE ESTADO
           MOVE SPACES TO DISPESTADO
           MOVE ZEROS TO CODIGO W-SEL
           DISPLAY TELACEP.
       R1A.
           ACCEPT TCODIGO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
           IF CODIGO = ZEROS
               MOVE "*** CODIGO NAO PODE SER ZEROS ***" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R1A.           

       LER-CADCEP.
           READ CADCEP
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                DISPLAY TELACEP
                MOVE "*** CEP JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADCEP" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.
       R1B.
           ACCEPT TENDERECO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R1A.
           IF ENDERECO = SPACES
               MOVE "ENDERECO NAO PODE FICAR EM BRANCO" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R1B.              
           
       R2.
           ACCEPT TBAIRRO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R1B.
           IF BAIRRO = SPACES
               MOVE "BAIRRO NAO PODE FICAR EM BRANCO" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R2.               
       R3.
           ACCEPT TCIDADE.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.
           IF CIDADE = SPACES
               MOVE "CIDADE NAO PODE FICAR EM BRANCO" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R3.  

       R4. 
           DISPLAY TELAESTADO
           MOVE 1 TO IND
           ACCEPT TESTADO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.
       R4A.
           MOVE TBESTADO(IND) TO TXTESTADO
           IF TXTESTADO01 NOT = ESTADO
              ADD 1 TO IND
              IF IND < 6
                 GO TO R4A
              ELSE
                 MOVE "**** TIPO DE UF INCORRETA ****" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R4
           ELSE
               MOVE TXTESTADO02 TO DISPESTADO.
           DISPLAY TELACEP.
                  
      * ------------- VERICAR SE E ALTERACAO -----------------
           IF W-SEL = 1 
                GO TO ALT-OPC.             

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R4A.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "**** CEP JA EXISTE, DADOS NAO GRAVADOS ****" 
                      TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R1
                ELSE
                      MOVE "* ERRO NA GRAVACAO DO ARQUIVO DE CEPS* "
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
                      GO TO R1B.
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
                DELETE CADCEP RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO CEP EXCLUIDO ***" TO MENS
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
                IF W-ACT = 01 GO TO R4A.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA ALTERACAO DO REGISTRO CEP"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.                


       ROT-FIM.
           CLOSE CADCEP
           STOP RUN.

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