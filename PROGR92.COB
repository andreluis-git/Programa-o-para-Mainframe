       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGR92.
      *AUTHOR. ANDRE LUIS DE M RODRIGUES.
      **************************************
      *    CADASTRO DE VEICULOS            *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADVEIC ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS PLACADOVEICULO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOMEDOPROPRIETARIO 
                       WITH DUPLICATES.     
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADVEIC
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADVEIC.DAT".
       01  REGVEIC.
           03  PLACADOVEICULO.
               05  LETRAP          PIC X(03).
               05  NUMEROP         PIC 9(04).
           03  NOMEDOPROPRIETARIO  PIC X(30).
           03  MARCA               PIC 9(02).
           03  MODELO              PIC X(20).
           03  COR                 PIC X(01).
           03  ANOFABRICACAO       PIC 9(04).  
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
       01 DISPMARCA             PIC X(10) VALUE SPACES.
       01 DISPCOR               PIC X(10) VALUE SPACES.
       
      *    CARREGA O VETOR  TABMARCA
       01  TABMARCAX.
           03 FILLER  PIC X(10)   VALUE "CHEVROLET".
           03 FILLER  PIC X(10)   VALUE "VOLKSWAGEN".
           03 FILLER  PIC X(10)   VALUE "FIAT".
           03 FILLER  PIC X(10)   VALUE "RENAULT".
           03 FILLER  PIC X(10)   VALUE "FORD".
           03 FILLER  PIC X(10)   VALUE "TOYOTA".
           03 FILLER  PIC X(10)   VALUE "HYUNDAI".
           03 FILLER  PIC X(10)   VALUE "JEEP".
       01  TABMARCA REDEFINES TABMARCAX.
           03  TBMARCA         PIC X(10) OCCURS 8 TIMES.

      *    CARREGA O VETOR TABCOR
       01  TABCORX.
           03 FILLER  PIC X(10)   VALUE "PPRETO".
           03 FILLER  PIC X(10)   VALUE "BBRANCO".
           03 FILLER  PIC X(10)   VALUE "AAZUL".
           03 FILLER  PIC X(10)   VALUE "VVERMELHO".
           03 FILLER  PIC X(10)   VALUE "CCINZA".  
       01  TABCOR REDEFINES TABCORX.
           03  TBCOR           PIC X(10) OCCURS 5 TIMES.
       01 TXTCOR.
          03 TXTCOR01 PIC X(01) VALUE SPACES.
          03 TXTCOR02 PIC X(09) VALUE SPACES. 


      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA2.
           05  BLANK SCREEN.
           05  LINE 03  COLUMN 01 
               VALUE  "                          CADASTRO DE VE".
           05  LINE 03  COLUMN 41 
               VALUE  "ICULOS".
           05  LINE 07  COLUMN 01 
               VALUE  "        PLACA DO VEICULO    :     - ".
           05  LINE 09  COLUMN 01 
               VALUE  "        NOME DO PROPRIETARIO:".
           05  LINE 11  COLUMN 01 
               VALUE  "        MARCA               :".
           05  LINE 13  COLUMN 01 
               VALUE  "        MODELO              :".
           05  LINE 15  COLUMN 01 
               VALUE  "        COR                 :".
           05  LINE 17  COLUMN 01 
               VALUE  "        ANO FABRICACAO      :".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  TLETRAP
               LINE 07  COLUMN 31  PIC X(03)
               USING  LETRAP
               AUTO HIGHLIGHT.
           05  TNUMEROP
               LINE 07  COLUMN 37  PIC 9(04)
               USING  NUMEROP
               HIGHLIGHT.
           05  TNOMEDOPROPRIETARIO
               LINE 09  COLUMN 31  PIC X(30)
               USING  NOMEDOPROPRIETARIO
               HIGHLIGHT.
           05  TMARCA
               LINE 11  COLUMN 31  PIC 9(02)
               USING  MARCA
               HIGHLIGHT.
      *    DISPLAY MARCA         
           05  TDISPMARCA
               LINE 11  COLUMN 35  PIC X(10)
               USING DISPMARCA.
           05  TMODELO
               LINE 13  COLUMN 31  PIC X(20)
               USING  MODELO
               HIGHLIGHT.
           05  TCOR
               LINE 15  COLUMN 31  PIC X(01)
               USING  COR
               HIGHLIGHT.
      *    DISPLAY COR
           05  TDISPCOR
               LINE 15  COLUMN 35  PIC X(10)
               USING DISPCOR.
           05  TANOFABRICACAO
               LINE 17  COLUMN 31  PIC 9(04)
               USING  ANOFABRICACAO
               HIGHLIGHT.
           
       01  TELAMARCA.
           05  LINE 14  COLUMN 62 VALUE  "1 - CHEVROLET".
           05  LINE 15  COLUMN 62 VALUE  "2 - VOLKSWAGEN".
           05  LINE 16  COLUMN 62 VALUE  "3 - FIAT".
           05  LINE 17  COLUMN 62 VALUE  "4 - RENAULT".
           05  LINE 18  COLUMN 62 VALUE  "5 - FORD".
           05  LINE 19  COLUMN 62 VALUE  "6 - TOYOTA".
           05  LINE 20  COLUMN 62 VALUE  "7 - HYUNDAI".
           05  LINE 21  COLUMN 62 VALUE  "8 - JEEP".

       01  TELACOR.
           05  LINE 14  COLUMN 62 VALUE  "P-PRETO".
           05  LINE 15  COLUMN 62 VALUE  "B-BRANCO".
           05  LINE 16  COLUMN 62 VALUE  "A-AZUL".
           05  LINE 17  COLUMN 62 VALUE  "V-VERMELHO".
           05  LINE 18  COLUMN 62 VALUE  "C-CINZA".
              
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
       R0.
           OPEN I-O CADVEIC
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADVEIC
                 CLOSE CADVEIC
                 MOVE "**** ARQUIVO CADVEIC FOI CRIADO ****" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "**** ERRO NA ABERTURA DO ARQUIVO CADVEIC ****" 
                     TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      * 
       R1.
           MOVE SPACES TO LETRAP NOMEDOPROPRIETARIO MODELO COR
           MOVE SPACES TO DISPCOR DISPMARCA
           MOVE ZEROS TO NUMEROP MARCA ANOFABRICACAO
           DISPLAY TELA2.
       R1A.       
           ACCEPT TLETRAP.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM. 
       R1B.
           ACCEPT TNUMEROP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R1A.     
       LER-CADVEIC.
           READ CADVEIC
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                DISPLAY TELA2
                MOVE "*** VEICULO JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R1
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADVEIC" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.
       R1C.
           ACCEPT TNOMEDOPROPRIETARIO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R1B.
       R2.
           DISPLAY TELAMARCA
           ACCEPT TMARCA
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R1C.
           IF MARCA > 8 OR MARCA < 1
               MOVE "**** DIGITE APENAS DE 1 A 9 ****" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R2.
           MOVE TBMARCA(MARCA) TO DISPMARCA
           DISPLAY TELA2.
       R2A.
           ACCEPT TMODELO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.
       R3.
           DISPLAY TELACOR
           MOVE 1 TO IND
           ACCEPT TCOR.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2A.
       R3A.
           MOVE TBCOR(IND) TO TXTCOR
           IF TXTCOR01 NOT = COR
              ADD 1 TO IND
              IF IND < 6
                 GO TO R3A
              ELSE
                 MOVE "**** TIPO DE COR INCORRETA ****" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R3
           ELSE
               MOVE TXTCOR02 TO DISPCOR.
           DISPLAY TELA2.
       R3B.
           ACCEPT TANOFABRICACAO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R3B.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGVEIC
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "**** VEICULO JA EXISTE, DADOS NAO GRAVADOS ****" 
                      TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R1
                ELSE
                      MOVE "* ERRO NA GRAVACAO DO ARQUIVO DE VEICULOS* "
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.


       ROT-FIM.
           CLOSE CADVEIC
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