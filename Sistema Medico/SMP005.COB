       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP004.
      *AUTHOR. ANDRE LUIS DE M RODRIGUES.
      **************************************
      *    CADASTRO DE PACIENTES            *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADPACI ASSIGN TO DISK
                        ORGANIZATION IS INDEXED
                        ACCESS MODE  IS DYNAMIC
                        RECORD KEY   IS CPF
                        FILE STATUS  IS ST-ERRO
                        ALTERNATE RECORD KEY IS NOME 
                           WITH DUPLICATES. 

           SELECT CADCONV ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGOCONV
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOMECONV 
                       WITH DUPLICATES
                    ALTERNATE RECORD KEY IS PLANO 
                       WITH DUPLICATES.   

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
       FD CADPACI
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADPACI.DAT".
       01  REGPACI.
           03  CPF                 PIC 9(11).
           03  NOME                PIC X(30).
           03  SEXO                PIC X(01).
           03  GENERO              PIC X(01).
           03  DATANASCIMENTO.      
               05  DIA             PIC 9(02).
               05  MES             PIC 9(02).
               05  ANO             PIC 9(04).
           03  EMAIL               PIC X(30).
           03  TELEFONE. 
               05  DDD             PIC 9(03).
               05  NUMTEL          PIC 9(08).
           03  CONVENIO            PIC 9(04).
           03  CEP                 PIC 9(08).
           03  NUMERO              PIC 9(04).
           03  COMPLEMENTO         PIC X(10). 

       FD CADCONV
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONV.DAT".
       01  REGCONV.
           03  CODIGOCONV          PIC 9(04).
           03  NOMECONV            PIC X(30).
           03  PLANO               PIC 9(02). 

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
       77 W-CONT        PIC 9(04) VALUE ZEROS.
       77 W-OPCAO       PIC X(01) VALUE SPACES.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 W-SEL         PIC 9(01) VALUE ZEROS.
       77 MENS          PIC X(50) VALUE SPACES.
       77 LIMPA         PIC X(50) VALUE SPACES.
       01 ST-ERRO       PIC X(02) VALUE "00".

       01 IND           PIC 9(01) VALUE ZEROS.

       01 DISPGENERO    PIC X(15) VALUE SPACES.
       01 DISPSEXO      PIC X(10) VALUE SPACES.
       01 DISPCONVENIO  PIC X(30) VALUE SPACES.
      
       01  TABGENEROX.
           03 FILLER  PIC X(15)   VALUE "NNAO DECLARADO".
           03 FILLER  PIC X(15)   VALUE "HHETEROSEXUAL".
           03 FILLER  PIC X(15)   VALUE "BBISEXUAL".
           03 FILLER  PIC X(15)   VALUE "TTRANSEXUAL".
           03 FILLER  PIC X(15)   VALUE "PPANSEXUAL".
           03 FILLER  PIC X(15)   VALUE "OOUTROS".
           
       01  TABGENERO REDEFINES TABGENEROX.
           03  TBGENERO           PIC X(15) OCCURS 6 TIMES.

       01 TXTGENERO.
          03 TXTGENERO01 PIC X(01) VALUE SPACES.
          03 TXTGENERO02 PIC X(14) VALUE SPACES.
      *
      *-----------------------------------------------------------------
       SCREEN SECTION.       
       01  TELAPACI.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                    CADASTRO DE PACIENTES".
           05  LINE 04  COLUMN 01 
               VALUE  "        CPF               :".
           05  LINE 05  COLUMN 01 
               VALUE  "        NOME              :".
           05  LINE 06  COLUMN 01 
               VALUE  "        SEXO              :".
           05  LINE 07  COLUMN 01 
               VALUE  "        GENERO            :".
           05  LINE 08  COLUMN 01 
               VALUE  "        DATA DE NASCIMENTO:  /  /".
           05  LINE 09  COLUMN 01 
               VALUE  "        EMAIL             :".
           05  LINE 10  COLUMN 01 
               VALUE  "        TELEFONE          :".
           05  LINE 11  COLUMN 01 
               VALUE  "        CONVENIO          :".
           05  LINE 12  COLUMN 01 
               VALUE  "        CEP               :".
           05  LINE 13  COLUMN 01 
               VALUE  "        RUA               :".
           05  LINE 14  COLUMN 01 
               VALUE  "        NUMERO            :".
           05  LINE 15  COLUMN 01 
               VALUE  "        COMPLEMENTO       :".
           05  LINE 22  COLUMN 01 
               VALUE  " F1 => RETORNA AO MENU".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM:".
           05  TCPF
               LINE 04  COLUMN 28  PIC 9(11)
               USING  CPF
               HIGHLIGHT.
           05  TNOME
               LINE 05  COLUMN 28  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TSEXO
               LINE 06  COLUMN 28  PIC X(01)
               USING  SEXO
               HIGHLIGHT.
           05  TGENERO
               LINE 07  COLUMN 28  PIC X(01)
               USING  GENERO
               HIGHLIGHT.
           05  TDIA
               LINE 08  COLUMN 28  PIC 9(02)
               USING  DIA
               AUTO HIGHLIGHT.
           05  TMES
               LINE 08  COLUMN 31  PIC 9(02)
               USING  MES
               AUTO HIGHLIGHT.
           05  TANO
               LINE 08  COLUMN 34  PIC 9(04)
               USING  ANO
               HIGHLIGHT.
           05  TEMAIL
               LINE 09  COLUMN 28  PIC X(30)
               USING  EMAIL
               HIGHLIGHT.
           05  TDDD
               LINE 10  COLUMN 28  PIC 9(03)
               USING  DDD
               AUTO HIGHLIGHT.
           05  TNUMTEL
               LINE 10  COLUMN 32  PIC 9(08)
               USING  NUMTEL
               HIGHLIGHT.
           05  TCONVENIO
               LINE 11  COLUMN 28  PIC 9(04)
               USING  CONVENIO
               HIGHLIGHT.
           05  TCEP
               LINE 12  COLUMN 28  PIC 9(08)
               USING  CEP
               HIGHLIGHT.        
           05  TENDERECO
               LINE 13  COLUMN 28  PIC X(30)
               USING  ENDERECO
               HIGHLIGHT.    
           05  TNUMERO
               LINE 14  COLUMN 28  PIC 9(04)
               USING  NUMERO
               HIGHLIGHT.
           05  TCOMPLEMENTO
               LINE 15  COLUMN 28  PIC X(10)
               USING  COMPLEMENTO
               HIGHLIGHT.

           05  TDISPSEXO
               LINE 06  COLUMN 33  PIC X(10)
               USING DISPSEXO.
           05  TDISPGENERO
               LINE 07  COLUMN 33  PIC X(15)
               USING DISPGENERO.

           05  TDISPCONVENIO
               LINE 11  COLUMN 33  PIC X(15)
               USING DISPCONVENIO.

       01  TELAGENERO.
           05  LINE 13  COLUMN 50 VALUE  "N-NAO DECLARADO".
           05  LINE 14  COLUMN 50 VALUE  "H-HETEROSEXUAL".
           05  LINE 15  COLUMN 50 VALUE  "B-BISEXUAL".
           05  LINE 16  COLUMN 50 VALUE  "T-TRANSEXUAL".
           05  LINE 17  COLUMN 50 VALUE  "P-PANSEXUAL".
           05  LINE 18  COLUMN 50 VALUE  "O-OUTROS".
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
       R0.
           OPEN I-O CADPACI
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADPACI
                 CLOSE CADPACI
                 MOVE "**** ARQUIVO CADPACI FOI CRIADO ****" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "**** ERRO NA ABERTURA DO ARQUIVO CADPACI ****" 
                     TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       R0A.
           OPEN INPUT CADCONV
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "**** ARQUIVO CADCONV FOI CRIADO ****" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "**** ERRO NA ABERTURA DO ARQUIVO CADCONV ****" 
                     TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      
       R0B.
           OPEN INPUT CADCEP
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "**** ARQUIVO CADCEP FOI CRIADO ****" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "**** ERRO NA ABERTURA DO ARQUIVO CADCEP ****" 
                     TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      * 
       R1.
           MOVE SPACES TO NOME SEXO EMAIL COMPLEMENTO GENERO
                   DISPGENERO DISPSEXO
           MOVE ZEROS TO CPF DIA MES ANO NUMERO CEP CONVENIO
                   TELEFONE W-SEL

           MOVE SPACES TO NOMECONV ENDERECO BAIRRO CIDADE ESTADO
                   DISPCONVENIO
           MOVE ZEROS TO CODIGOCONV PLANO CODIGO
           DISPLAY TELAPACI.

       R1A.
           ACCEPT TCPF.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
           IF W-ACT = 02
                   GO TO ROT-FIM.
           IF CPF = ZEROS
               MOVE "*** CPF NAO PODE SER ZEROS ***" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R1A.           

       LER-CADPACI.
           READ CADPACI
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                DISPLAY TELAPACI
                PERFORM R7A
                PERFORM R8A
                MOVE "*** PACIENTE JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM                
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADPACI" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.
       R1C.
           ACCEPT TNOME.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R1A.
           IF W-ACT = 02
                   GO TO ROT-FIM.
           IF NOME = SPACES
               MOVE "NOME NAO PODE FICAR EM BRANCO" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R1C.               
           
       R3.
           ACCEPT TSEXO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R1C.
           IF W-ACT = 02
                   GO TO ROT-FIM.
           IF SEXO = "M" OR "m"
               MOVE "MASCULINO" TO DISPSEXO
           ELSE
               IF SEXO = "F" OR "f"
                   MOVE "FEMININO" TO DISPSEXO
               ELSE
                   MOVE "*** SEXO INVALIDO DIGITE M OU F ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R3.
           DISPLAY TELAPACI.
       
       R3A.
           DISPLAY TELAGENERO
           MOVE 1 TO IND
           ACCEPT TGENERO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.
           IF W-ACT = 02
                   GO TO ROT-FIM.
       R3B.
           MOVE TBGENERO(IND) TO TXTGENERO
           IF TXTGENERO01 NOT = GENERO
              ADD 1 TO IND
              IF IND < 7
                 GO TO R3B
              ELSE
                 MOVE "**** TIPO DE GENERO INCORRETO ****" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R3A
           ELSE
               MOVE TXTGENERO02 TO DISPGENERO.
           DISPLAY TELAPACI.

       R4.
           ACCEPT TDIA.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3A.
           IF W-ACT = 02
                   GO TO ROT-FIM.
           IF DIA > 31 OR DIA < 1
               MOVE "VALOR DIA PRECISA ESTAR ENTRE 1 E 31" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R4.

       R4A.
           ACCEPT TMES.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.
           IF W-ACT = 02
                   GO TO ROT-FIM.
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
           IF W-ACT = 02
                   GO TO ROT-FIM.
           IF ANO = ZEROS
               MOVE "VALOR ANO NAO PODE SER IGUAL A ZERO" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R4B.
           
       R5. 
           ACCEPT TEMAIL.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.
           IF W-ACT = 02
                   GO TO ROT-FIM.
           IF EMAIL = SPACES
               MOVE "EMAIL NAO PODE FICAR EM BRANCO" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R5.
       R6. 
           ACCEPT TDDD.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.
           IF W-ACT = 02
                   GO TO ROT-FIM.
           IF DDD = ZEROS
               MOVE "DDD NAO PODE FICAR EM BRANCO" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R6.
       R6A.
           ACCEPT TNUMTEL.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6.
           IF W-ACT = 02
                   GO TO ROT-FIM.
           IF NUMTEL = ZEROS
               MOVE "TELEFONE NAO PODE FICAR EM BRANCO" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R6A.

       R7.
           ACCEPT TCONVENIO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6.
           IF W-ACT = 02
                   GO TO ROT-FIM.
           IF CONVENIO = ZEROS
               MOVE "CONVENIO NAO PODE FICAR EM BRANCO" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R7.
       R7A.        
           MOVE CONVENIO TO CODIGOCONV.
           READ CADCONV
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "*CONVENIO DIGITADO NAO ENCONTRADO*" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R7
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADCONV" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
               MOVE NOMECONV TO DISPCONVENIO.
               DISPLAY TELAPACI.

       R8.
           ACCEPT TCEP.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R7.
           IF W-ACT = 02
                   GO TO ROT-FIM.
           IF CEP = ZEROS
               MOVE "CEP NAO PODE FICAR EM BRANCO" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R8.
       R8A.        
           MOVE CEP TO CODIGO.
           READ CADCEP
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "*CEP DIGITADO NAO ENCONTRADO*" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R8
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADCEP" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
               DISPLAY TELAPACI.  

       R10.
           ACCEPT TNUMERO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R8.
           IF W-ACT = 02
                   GO TO ROT-FIM.
           IF NUMERO = ZEROS
               MOVE "NUMERO NAO PODE FICAR EM BRANCO" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R10. 

       R11.
           ACCEPT TCOMPLEMENTO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R10.
           IF W-ACT = 02
                   GO TO ROT-FIM.
           IF COMPLEMENTO = ZEROS
               MOVE "COMPLEMENTO NAO PODE FICAR EM BRANCO" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R11.        
                  
      * ------------- VERICAR SE E ALTERACAO -----------------
           IF W-SEL = 1 
                GO TO ALT-OPC.             

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R11.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGPACI
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "*** PACIENTE JA EXISTE, DADOS NAO GRAVADOS ***" 
                      TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R1
                ELSE
                      MOVE "*ERRO NA GRAVACAO DO ARQUIVO DE PACIENTES*"
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
                DELETE CADPACI RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO PACIENTE EXCLUIDO ***" TO MENS
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
                IF W-ACT = 01 GO TO R11.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGPACI
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA ALTERACAO DO REGISTRO PACIENTE"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.                


       ROT-FIM.
           CLOSE CADPACI CADCONV CADCEP
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