       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADMEDICO.
      *AUTHOR. ANDRE LUIS DE M RODRIGUES.
      **************************************
      *    CADASTRO DE MEDICOS            *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADMED ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CRM
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME 
                       WITH DUPLICATES.     
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
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
       77 W-CONT        PIC 9(06) VALUE ZEROS.
       77 W-OPCAO       PIC X(01) VALUE SPACES.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 W-SEL         PIC 9(01) VALUE ZEROS.
       77 MENS          PIC X(50) VALUE SPACES.
       77 LIMPA         PIC X(50) VALUE SPACES.
       01 ST-ERRO       PIC X(02) VALUE "00".

       01 IND                  PIC 9(01) VALUE ZEROS.
      
      *    DISPLAY
       01 DISPESPECIALIDADE    PIC X(15) VALUE SPACES.
       
      *    CARREGA O VETOR  TABESPECIALIDADE
       01  TABESPECIALIDADEX.
           03 FILLER  PIC X(15)   VALUE "CLINICA MEDICA".
           03 FILLER  PIC X(15)   VALUE "UROLOGIA".
           03 FILLER  PIC X(15)   VALUE "GINICOLOGISTA".
           03 FILLER  PIC X(15)   VALUE "PEDIATRIA".
           03 FILLER  PIC X(15)   VALUE "CARDIOLOGISTA".
           03 FILLER  PIC X(15)   VALUE "FISIOTERAPIA".
           03 FILLER  PIC X(15)   VALUE "ANESTESISTA".
           03 FILLER  PIC X(15)   VALUE "OFTALMOLOGIA".
       01  TABESPECIALIDADE REDEFINES TABESPECIALIDADEX.
           03  TBESPECIALIDADE         PIC X(15) OCCURS 8 TIMES.
      *
      *-----------------------------------------------------------------
       SCREEN SECTION.       
       01  TELAMED.
           05  BLANK SCREEN.
           05  LINE 04  COLUMN 01 
               VALUE  "                    CADASTRO DE MEDICOS".
           05  LINE 08  COLUMN 01 
               VALUE  "        CRM               :".
           05  LINE 10  COLUMN 01 
               VALUE  "        NOME              :".
           05  LINE 12  COLUMN 01 
               VALUE  "        ESPECIALIDADE     :".
           05  LINE 14  COLUMN 01 
               VALUE  "        SEXO              :".
           05  LINE 16  COLUMN 01 
               VALUE  "        DATA DE NASCIMENTO:  /  /".
           05  LINE 18  COLUMN 01 
               VALUE  "        EMAIL             :".
           05  LINE 20  COLUMN 01 
               VALUE  "        TELEFONE          :".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM:".
           05  TCRM
               LINE 08  COLUMN 28  PIC 9(06)
               USING  CRM
               HIGHLIGHT.
           05  TNOME
               LINE 10  COLUMN 28  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TESPECIALIDADE
               LINE 12  COLUMN 28  PIC 9(02)
               USING  ESPECIALIDADE
               HIGHLIGHT.
           05  TSEXO
               LINE 14  COLUMN 28  PIC X(01)
               USING  SEXO
               HIGHLIGHT.
           05  TDIA
               LINE 16  COLUMN 28  PIC 9(02)
               USING  DIA
               AUTO HIGHLIGHT.
           05  TMES
               LINE 16  COLUMN 31  PIC 9(02)
               USING  MES
               AUTO HIGHLIGHT.
           05  TANO
               LINE 16  COLUMN 34  PIC 9(04)
               USING  ANO
               HIGHLIGHT.
           05  TEMAIL
               LINE 18  COLUMN 28  PIC X(30)
               USING  EMAIL
               HIGHLIGHT.
           05  TTELEFONE
               LINE 20  COLUMN 28  PIC 9(09)
               USING  TELEFONE
               HIGHLIGHT.

      * DISPLAY ESPECIALISTA
           05  TDISPESPECIALIDADE
               LINE 12  COLUMN 33  PIC X(15)
               USING DISPESPECIALIDADE.

           
       01  TELAESPECIALIDADE.
           05  LINE 14  COLUMN 62 VALUE  "1 - CLINICA MEDICA".
           05  LINE 15  COLUMN 62 VALUE  "2 - UROLOGIA".
           05  LINE 16  COLUMN 62 VALUE  "3 - GINICOLOGISTA".
           05  LINE 17  COLUMN 62 VALUE  "4 - PEDIATRIA".
           05  LINE 18  COLUMN 62 VALUE  "5 - CARDIOLOGISTA".
           05  LINE 19  COLUMN 62 VALUE  "6 - FISIOTERAPIA".
           05  LINE 20  COLUMN 62 VALUE  "7 - ANESTESISTA".
           05  LINE 21  COLUMN 62 VALUE  "8 - OFTALMOLOGIA".
              
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
       R0.
           OPEN I-O CADMED
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADMED
                 CLOSE CADMED
                 MOVE "**** ARQUIVO CADMED FOI CRIADO ****" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "**** ERRO NA ABERTURA DO ARQUIVO CADMED ****" 
                     TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      * 
       R1.
           MOVE SPACES TO NOME SEXO EMAIL
           MOVE SPACES TO DISPESPECIALIDADE
           MOVE ZEROS TO CRM ESPECIALIDADE DIA MES ANO 
                   TELEFONE W-SEL
           DISPLAY TELAMED.
       R1A.
           ACCEPT TCRM.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
           IF CRM = ZEROS
               MOVE "*** CRM NAO PODE SER ZEROS ***" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R1A.           

       LER-CADMED.
           READ CADMED
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                DISPLAY TELAMED
                MOVE "*** MEDICO JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADMED" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.
       R1C.
           ACCEPT TNOME.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R1A.
           IF NOME = SPACES
               MOVE "NOME NAO PODE FICAR EM BRANCO" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R1C.
               
           
       R2.
           DISPLAY TELAESPECIALIDADE
           ACCEPT TESPECIALIDADE.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R1C.
           IF ESPECIALIDADE > 8 OR ESPECIALIDADE < 1
               MOVE "**** DIGITE APENAS DE 1 A 9 ****" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R2.
           MOVE TBESPECIALIDADE(ESPECIALIDADE) TO DISPESPECIALIDADE
           DISPLAY TELAMED.
       R3.
           ACCEPT TSEXO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.
           IF SEXO = "M" OR "m"
               DISPLAY (14, 33)  "MASCULINO"
           ELSE
               IF SEXO = "F" OR "f"
                   DISPLAY (14, 33)  "FEMININO"
               ELSE
                   MOVE "*** SEXO INVALIDO DIGITE M OU F ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R3.          
       R4.
           ACCEPT TDIA.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.
           IF DIA > 31 OR DIA < 1
               MOVE "VALOR DIA PRECISA ESTAR ENTRE 1 E 31" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R4.

       R4A.
           ACCEPT TMES.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.
           IF DIA > 29 AND MES = 2
               MOVE "FEVEREIRO NAO PODE TER MAIS QUE 29 DIAS" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R4.
           IF MES > 12 OR MES < 1
               MOVE "VALOR MES PRECISA ESTAR ENTRE 1 E 12" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R4A.
       R4B.
           ACCEPT TANO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4A.
           IF ANO = ZEROS
               MOVE "VALOR ANO NAO PODE SER IGUAL A ZERO" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R4B.
           
       R5. 
           ACCEPT TEMAIL.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.
           IF EMAIL = SPACES
               MOVE "EMAIL NAO PODE FICAR EM BRANCO" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R5.
       R6. 
           ACCEPT TTELEFONE.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.
                  
      * ------------- VERICAR SE E ALTERACAO -----------------
           IF W-SEL = 1 
                GO TO ALT-OPC.             

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R6.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGMED
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "**** MEDICO JA EXISTE, DADOS NAO GRAVADOS ****" 
                      TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R1
                ELSE
                      MOVE "* ERRO NA GRAVACAO DO ARQUIVO DE MEDICOS* "
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
                DELETE CADMED RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO MEDICO EXCLUIDO ***" TO MENS
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
                IF W-ACT = 01 GO TO R6.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGMED
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA ALTERACAO DO REGISTRO MEDICO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.                


       ROT-FIM.
           CLOSE CADMED
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