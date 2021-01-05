       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP011.
      ***********************************************
      * CONVERCAO DO ARQUIVO CADCEPTX PARA DAT        *
      ***********************************************
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
               VALUE OF FILE-ID IS "CADCEPTX.DAT".
       01  REGCEP.
           03  CODIGO              PIC 9(08).
           03  ENDERECO            PIC X(30).
           03  BAIRRO              PIC X(30).
           03  CIDADE              PIC X(20).
           03  ESTADO              PIC X(02).	
      *
       FD CADCEPTX
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEPTX.TXT".
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
      *
       01 CEPTX.
                03 CEP-NUMCEPTX      PIC 9(08).
                03 FILLER            PIC X(01) VALUE ";".
                03 CEP-LOGRATX       PIC X(30).
                03 FILLER            PIC X(01) VALUE ";".
                03 CEP-BAIRROTX      PIC X(20).
                03 FILLER            PIC X(01) VALUE ";".
                03 CEP-CIDADETX      PIC X(20).
                03 FILLER            PIC X(01) VALUE ";".
                03 CEP-UFTX          PIC X(02).
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  SMT025.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                *** CONVERSAO DE ARQUIVO".
           05  LINE 02  COLUMN 41 
               VALUE  " DE CEP DE TXT DATA DAT ***".
           05  LINE 12  COLUMN 01 
               VALUE  "                         CONFIRMA CONVER".
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
           OPEN I-O CADCEP
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CADCEP
                      CLOSE CADCEP
                      MOVE "* ARQUIVO CADCEP CRIADO *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CEP" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
      *
           OPEN INPUT CADCEPTX
           IF ST-ERRO NOT = "00"
                      MOVE "ERRO ABERTURA DO ARQUIVO CADCEPTX" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
       LER-CEP01.
                READ CADCEPTX NEXT
                IF ST-ERRO NOT = "00"
                   IF ST-ERRO = "10"
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
           MOVE REGCEPTX     TO CEPTX
           MOVE CEP-NUMCEPTX TO CODIGO.
           MOVE CEP-LOGRATX  TO ENDERECO.
           MOVE CEP-BAIRROTX TO BAIRRO.
           MOVE CEP-CIDADETX TO CIDADE.
           MOVE CEP-UFTX     TO ESTADO.

       INC-WR1.
                WRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                      DISPLAY (20, 20) CODIGO
                      GO TO LER-CEP01
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO CADCEPTX"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
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
