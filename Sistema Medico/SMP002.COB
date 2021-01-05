       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP003.
      *AUTHOR. ANDRE LUIS DE M RODRIGUES.
      **************************************
      *    CADASTRO DE DOENCAS            *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCID ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS DENOMINACAO 
                       WITH DUPLICATES.     
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCID
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCID.DAT".
       01  REDCID.
           03  CODIGO              PIC 9(04).
           03  DENOMINACAO         PIC X(30).
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
      
      *
      *-----------------------------------------------------------------
       SCREEN SECTION.       
       01  TELACID.
           05  BLANK SCREEN.
           05  LINE 04  COLUMN 01 
               VALUE  "                    CADASTRO DE DOENCAS".
           05  LINE 08  COLUMN 01 
               VALUE  "        CODIGO            :".
           05  LINE 10  COLUMN 01 
               VALUE  "        DENOMINACAO       :".
           05  LINE 22  COLUMN 01 
               VALUE  " F1 => RETORNA AO MENU".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM:".
           05  TCODIGO
               LINE 08  COLUMN 28  PIC 9(04)
               USING  CODIGO
               HIGHLIGHT.
           05  TDENOMINACAO
               LINE 10  COLUMN 28  PIC X(30)
               USING  DENOMINACAO
               HIGHLIGHT.
           
                    
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
       R0.
           OPEN I-O CADCID
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCID
                 CLOSE CADCID
                 MOVE "**** ARQUIVO CADCID FOI CRIADO ****" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "**** ERRO NA ABERTURA DO ARQUIVO CADCID ****" 
                     TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      * 
       R1.
           MOVE SPACES TO DENOMINACAO
           MOVE ZEROS TO CODIGO W-SEL
           DISPLAY TELACID.
       R1A.
           ACCEPT TCODIGO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
           IF W-ACT = 02
                   GO TO ROT-FIM.
           IF CODIGO = ZEROS
               MOVE "*** CODIGO NAO PODE SER ZEROS ***" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R1A.           

       LER-CADCID.
           READ CADCID
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                DISPLAY TELACID
                MOVE "*** DOENCA JA CADASTRADA ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADCID" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.
       R1C.
           ACCEPT TDENOMINACAO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R1A.
           IF W-ACT = 02
                   GO TO ROT-FIM.
           IF DENOMINACAO = SPACES
               MOVE "DENOMINACAO NAO PODE FICAR EM BRANCO" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R1C.           
        
       
                  
      * ------------- VERICAR SE E ALTERACAO -----------------
           IF W-SEL = 1 
                GO TO ALT-OPC.             

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R1C.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REDCID
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "**** DOENCA JA EXISTE, DADOS NAO GRAVADOS ****" 
                      TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R1
                ELSE
                      MOVE "* ERRO NA GRAVACAO DO ARQUIVO DE DOENCAS* "
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
                DELETE CADCID RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO DOENCA EXCLUIDO ***" TO MENS
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
                IF W-ACT = 01 GO TO R1C.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REDCID
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA ALTERACAO DO REGISTRO DOENCA"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.                


       ROT-FIM.
           CLOSE CADCID
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