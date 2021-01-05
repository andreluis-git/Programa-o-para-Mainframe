       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP006.
      *AUTHOR. ANDRE LUIS DE M RODRIGUES.
      **************************************
      *    CADASTRO DE CONSULTAS            *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADCONS ASSIGN TO DISK
                        ORGANIZATION IS INDEXED
                        ACCESS MODE  IS DYNAMIC
                        RECORD KEY   IS KEYPRINCIPAL
                        FILE STATUS  IS ST-ERRO.
               
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

           SELECT CADMED ASSIGN TO DISK
                        ORGANIZATION IS INDEXED
                        ACCESS MODE  IS DYNAMIC
                        RECORD KEY   IS CRM
                        FILE STATUS  IS ST-ERRO
                        ALTERNATE RECORD KEY IS NOME 
                           WITH DUPLICATES. 

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
       FD CADCONS
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONS.DAT".
       01 REGCONS.
           03 KEYPRINCIPAL.
               05 CPFPACIENTE      PIC 9(11).       
               05 DATACONSULTA     PIC 9(08).
           03 CODMEDICO            PIC 9(06).
           03 CODCONVENIO          PIC 9(04).
           03 CODCID               PIC 9(04).
           03 DESCRICAO1           PIC X(60).
           03 DESCRICAO2           PIC X(60).

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
           03  TELEFONE            PIC 9(11).
           03  CONVENIO            PIC 9(04).
           03  CEP                 PIC 9(08).
           03  ENDERECOPACI        PIC X(30).
           03  NUMERO              PIC 9(04).
           03  COMPLEMENTO         PIC X(10). 

       FD CADCONV
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONV.DAT".
       01  REGCONV.
           03  CODIGOCONV          PIC 9(04).
           03  NOMECONV            PIC X(30).
           03  PLANO               PIC 9(02).

       FD CADMED
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMED.DAT".
       01  REGMED.
           03  CRM                 PIC 9(06).
           03  NOMEMEDICO          PIC X(30).
           03  ESPECIALIDADE       PIC 9(02).
           03  SEXOMEDICO          PIC X(01).
           03  DATANASCIMENTOMEDICO.      
               05  DIA             PIC 9(02).
               05  MES             PIC 9(02).
               05  ANO             PIC 9(04).
           03  EMAIL               PIC X(30).
           03  TELEFONE            PIC 9(09).
       
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

       01 IND           PIC 9(01) VALUE ZEROS.
       
       01 DISPGENERO    PIC X(15) VALUE SPACES.
       01 DISPSEXO      PIC X(10) VALUE SPACES.
       01 DISPCONVENIO  PIC X(30) VALUE SPACES.
       01 DISPPLANO     PIC X(30) VALUE SPACES.
       01 DISPSEXOMEDICO   PIC X(10) VALUE SPACES.
       01 DISPESPECIALIDADE   PIC X(15) VALUE SPACES.

      *--------------[TABELA GENEROS]---------------
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
       
      *--------------[TABELA PLANOS CONVENIO]--------------- 
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

      *--------------[TABELA ESPECIALIDADE MEDICO]---------------  
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
       
       01  TELACONSU.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                           CADASTRO DE C".
           05  LINE 02  COLUMN 41 
               VALUE  "ONSULTAS".
           05  LINE 04  COLUMN 01 
               VALUE  " PACIENTE:".
           05  LINE 04  COLUMN 41 
               VALUE  " DATA CONSULTA:".
           05  LINE 06  COLUMN 01 
               VALUE  " NOME:".
           05  LINE 07  COLUMN 01 
               VALUE  " DATA NASCIMENTO:             SEXO:".
           05  LINE 07  COLUMN 41 
               VALUE  "               GENERO:".
           05  LINE 09  COLUMN 01 
               VALUE  " CONVENIO:".
           05  LINE 10  COLUMN 01 
               VALUE  " PLANO:".
           05  LINE 12  COLUMN 01 
               VALUE  " MEDICO".
           05  LINE 13  COLUMN 01 
               VALUE  " CRM:           NOME:".
           05  LINE 13  COLUMN 41 
               VALUE  "             SEXO:".
           05  LINE 14  COLUMN 01 
               VALUE  " ESPECIALIDADE:".
           05  LINE 16  COLUMN 01 
               VALUE  " CONSULTA".
           05  LINE 17  COLUMN 01 
               VALUE  " CID:         DENOMINACAO:".
           05  LINE 18  COLUMN 01 
               VALUE  " DESCRICAO 1:".
           05  LINE 19  COLUMN 01 
               VALUE  " DESCRICAO 2:".
           05  LINE 22  COLUMN 01 
               VALUE  " F1 => RETORNA AO MENU".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM:".
           05  TPACIENTE
               LINE 04  COLUMN 12  PIC 9(11)
               USING  CPFPACIENTE
               HIGHLIGHT.
           05  TDATA
               LINE 04  COLUMN 56  PIC XX/XX/XXXX
               USING  DATACONSULTA
               HIGHLIGHT.
           05  TNOME
               LINE 06  COLUMN 08  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TDATANASC
               LINE 07  COLUMN 19  PIC XX/XX/XXXX
               USING  DATANASCIMENTO
               HIGHLIGHT.
           05  TSEXO
               LINE 07  COLUMN 37  PIC X(01)
               USING  SEXO
               HIGHLIGHT.
           05  TGENERO
               LINE 07  COLUMN 64  PIC X(01)
               USING  GENERO
               HIGHLIGHT.
           05  TCONVENIO
               LINE 09  COLUMN 12  PIC 9(04)
               USING  CONVENIO
               HIGHLIGHT.
           05  TPLANO
               LINE 10  COLUMN 09  PIC 9(02)
               USING  PLANO
               HIGHLIGHT.
           05  TCRM
               LINE 13  COLUMN 07  PIC 9(06)
               USING  CRM
               HIGHLIGHT.
           05  TNOMEMEDICO
               LINE 13  COLUMN 23  PIC X(30)
               USING  NOMEMEDICO
               HIGHLIGHT.
           05  TSEXOMEDICO
               LINE 13  COLUMN 60  PIC X(01)
               USING  SEXOMEDICO
               HIGHLIGHT.
           05  TESPECIALIDADE
               LINE 14  COLUMN 17  PIC 9(02)
               USING  ESPECIALIDADE
               HIGHLIGHT.
           05  TCID
               LINE 17  COLUMN 07  PIC X(04)
               USING  CODIGO
               HIGHLIGHT.
           05  TDENOMINACAO
               LINE 17  COLUMN 28  PIC X(30)
               USING  DENOMINACAO
               HIGHLIGHT.
           05  TDESCRICAO1
               LINE 18  COLUMN 15  PIC X(60)
               USING  DESCRICAO1
               HIGHLIGHT.
           05  TDESCRICAO2
               LINE 19  COLUMN 15  PIC X(60)
               USING  DESCRICAO2
               HIGHLIGHT.

      *--------[DESCRIÇÃO] ----------- 
           05  TDISPSEXO
               LINE 07  COLUMN 39  PIC X(10)
               USING DISPSEXO.
           05  TDISPGENERO
               LINE 07  COLUMN 66  PIC X(15)
               USING DISPGENERO.

           05  TDISPCONVENIO
               LINE 09  COLUMN 17  PIC X(15)
               USING DISPCONVENIO.
           05  TDISPPLANO
               LINE 10  COLUMN 12  PIC X(30)
               USING DISPPLANO.

           05  TDISPSEXOMEDICO
               LINE 13  COLUMN 62  PIC X(10)
               USING DISPSEXOMEDICO.
           05  TDISPESPECIALIDADE
               LINE 14  COLUMN 20  PIC X(15)
               USING DISPESPECIALIDADE.    
   
      * DISPLAY ESPECIALISTA
      *     05  TDISPESPECIALIDADE
      *         LINE 12  COLUMN 33  PIC X(15)
      *         USING DISPESPECIALIDADE.

           
      * 01  TELAESPECIALIDADE.
      *     05  LINE 14  COLUMN 62 VALUE  "1 - CLINICA MEDICA".
      *     05  LINE 15  COLUMN 62 VALUE  "2 - UROLOGIA".
      *     05  LINE 16  COLUMN 62 VALUE  "3 - GINICOLOGISTA".
      *     05  LINE 17  COLUMN 62 VALUE  "4 - PEDIATRIA".
      *     05  LINE 18  COLUMN 62 VALUE  "5 - CARDIOLOGISTA".
      *     05  LINE 19  COLUMN 62 VALUE  "6 - FISIOTERAPIA".
      *     05  LINE 20  COLUMN 62 VALUE  "7 - ANESTESISTA".
      *     05  LINE 21  COLUMN 62 VALUE  "8 - OFTALMOLOGIA".
              
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
       R0.
           OPEN I-O CADCONS
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCONS
                 CLOSE CADCONS
                 MOVE "**** ARQUIVO CADCONS FOI CRIADO ****" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "**** ERRO NA ABERTURA DO ARQUIVO CADCONS ****" 
                     TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
       
       R0A.
           OPEN INPUT CADPACI
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "**** ARQUIVO CADPACI FOI CRIADO ****" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "**** ERRO NA ABERTURA DO ARQUIVO CADPACI ****" 
                     TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE. 

       R0B.
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

       R0C.
           OPEN INPUT CADMED
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "**** ARQUIVO CADMED FOI CRIADO ****" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "**** ERRO NA ABERTURA DO ARQUIVO CADMED ****" 
                     TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE. 

       R0D.
           OPEN INPUT CADCID
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "**** ARQUIVO CADCID FOI CRIADO ****" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "**** ERRO NA ABERTURA DO ARQUIVO CADCID ****" 
                     TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
                                                    
      * 
       R1.
           MOVE SPACES TO DISPSEXO DISPGENERO DISPPLANO
           MOVE SPACES TO DESCRICAO1 DESCRICAO2
           MOVE ZEROS TO CPFPACIENTE DATACONSULTA                    
           MOVE SPACES TO GENERO SEXO NOME NOMEMEDICO SEXOMEDICO
                   DENOMINACAO 
           MOVE ZEROS TO DATANASCIMENTO CONVENIO PLANO CRM ESPECIALIDADE
                   CODIGO
           DISPLAY TELACONSU.

       R1A.
           ACCEPT TPACIENTE.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
           IF W-ACT = 02
                   GO TO ROT-FIM.
           IF CPFPACIENTE = ZEROS
               MOVE "*** CPF NAO PODE SER ZEROS ***" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R1A.

           ACCEPT TDATA.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R1A.
           IF W-ACT = 02
                   GO TO ROT-FIM.
           IF DATACONSULTA = ZEROS
               MOVE "*** DATA NAO PODE SER ZEROS ***" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R1A.               

       LER-CADCONS.
           READ CADCONS
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                DISPLAY TELACONSU
                PERFORM R1C
                PERFORM LER-CADPACI
                MOVE CODCONVENIO TO CODIGOCONV
                PERFORM LER-CADCONV
                MOVE CODMEDICO TO CRM
                PERFORM LER-CADMED
                MOVE CODCID TO CODIGO
                PERFORM LER-CADCID
                MOVE "*** CONSULTA JA CADASTRADA ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADCONS" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.
                
       R1C.
           MOVE CPFPACIENTE TO CPF.

       LER-CADPACI.
           READ CADPACI
           IF ST-ERRO NOT = "23"  
             IF ST-ERRO = "00"          
                   PERFORM DISP-SEXO
                   MOVE 1 TO IND
                   PERFORM DISP-GENERO
                   DISPLAY TELACONSU
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADPACI" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** PACIENTE NAO CADASTRADO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R1A.

           MOVE CONVENIO TO CODIGOCONV.

       LER-CADCONV.
           READ CADCONV
           IF ST-ERRO NOT = "23"  
             IF ST-ERRO = "00"                 
                 MOVE CODIGOCONV TO CODCONVENIO                 
                 MOVE NOMECONV TO DISPCONVENIO
                 MOVE TBPLANO(PLANO) TO DISPPLANO
                 DISPLAY TELACONSU
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADCONV" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.
       
       R2. 
           ACCEPT TCRM.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R1A.
           IF W-ACT = 02
                   GO TO ROT-FIM.

       LER-CADMED.
           READ CADMED
           IF ST-ERRO NOT = "23"  
             IF ST-ERRO = "00"
                 MOVE CRM TO CODMEDICO
                 PERFORM DISP-SEXOMEDICO
                 MOVE TBESPECIALIDADE(ESPECIALIDADE) 
                   TO DISPESPECIALIDADE
                 DISPLAY TELACONSU
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADMED" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** MEDICO NAO CADASTRADO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R2.

       R3. 
           ACCEPT TCID
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.
           IF W-ACT = 02
                   GO TO ROT-FIM.
       
       LER-CADCID.
           READ CADCID
           IF ST-ERRO NOT = "23"  
             IF ST-ERRO = "00"
                 MOVE CODIGO TO CODCID
                 DISPLAY TELACONSU
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADMED" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** CID NAO CADASTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R3.
       
       R4.
           ACCEPT TDESCRICAO1
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.
           IF W-ACT = 02
                   GO TO ROT-FIM.
       
       R5.
           ACCEPT TDESCRICAO2
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.   
           IF W-ACT = 02
                   GO TO ROT-FIM. 

      * ------------- VERICAR SE E ALTERACAO -----------------
           IF W-SEL = 1 
                GO TO ALT-OPC.             

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R1A.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCONS
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
                      GO TO R2.
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
                DELETE CADCONS RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO CONSULTA EXCLUIDO ***" TO MENS
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
                REWRITE REGCONS
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA ALTERACAO DO REGISTRO CONSULTA"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.                


       ROT-FIM.
           CLOSE CADCONS
           CLOSE CADPACI
           CLOSE CADCONV
           CLOSE CADMED
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


      *---------[ ROTINA DE DESCRIÇÃO ]---------------------
      *-------[PACIENTE]----------------
       DISP-SEXO.
           IF SEXO = "M" OR "m"
                       MOVE "MASCULINO" TO DISPSEXO
                   ELSE    
                       IF SEXO = "F" OR "f"
                           MOVE "FEMININO" TO DISPSEXO.
      
       DISP-GENERO.
           MOVE TBGENERO(IND) TO TXTGENERO
           IF TXTGENERO01 NOT = GENERO
              ADD 1 TO IND
              IF IND < 7
                 GO TO DISP-GENERO
              ELSE
                 MOVE "**** TIPO DE GENERO INCORRETO ****" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
               MOVE TXTGENERO02 TO DISPGENERO.
       
      *----------[MEDICO]-----------------
       DISP-SEXOMEDICO.
           IF SEXOMEDICO = "M" OR "m"
                       MOVE "MASCULINO" TO DISPSEXOMEDICO
                   ELSE    
                       IF SEXO = "F" OR "f"
                           MOVE "FEMININO" TO DISPSEXOMEDICO. 