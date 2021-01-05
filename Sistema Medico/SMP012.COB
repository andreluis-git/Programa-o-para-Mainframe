       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP012.
      ***********************************************
      * CONVERCAO DO ARQUIVO CADMED PARA TXT        *
      ***********************************************
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
           SELECT CADMEDTX ASSIGN TO DISK
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS  IS ST-ERRO.
      *
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
       FD CADMEDTX
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMEDTX.TXT".
       01 REGMEDTX    PIC X(100).
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
       01 MEDTX.
                03 CRMTX               PIC 9(06).
                03 FILLER              PIC X(01) VALUE ";".
                03 NOMETX              PIC X(30).
                03 FILLER              PIC X(01) VALUE ";".
                03 ESPECIALIDADETX     PIC 9(02).
                03 FILLER              PIC X(01) VALUE ";".
                03 SEXOTX              PIC X(01).
                03 FILLER              PIC X(01) VALUE ";".
                03 DATANASCIMENTOTX    PIC 9(08).
                03 FILLER              PIC X(01) VALUE ";".
                03 EMAILTX             PIC X(30).
                03 FILLER              PIC X(01) VALUE ";".
                03 TELEFONETX          PIC 9(09).
                
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  SMT025.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                *** CONVERSAO DE ARQUIVO".
           05  LINE 02  COLUMN 41 
               VALUE  " DE MEDICO PARA TXT ***".
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
                MOVE SPACES TO  NOME SEXO EMAIL.
                MOVE ZEROS  TO CRM ESPECIALIDADE DATANASCIMENTO
                 TELEFONE.
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
           OPEN INPUT CADMED
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      MOVE "* ARQUIVO CADMED NAO EXISTE *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CADMED" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
      *
           OPEN OUTPUT CADMEDTX
           IF ST-ERRO NOT = "00"
                      MOVE "ERRO ABERTURA DO ARQUIVO CADMEDTX" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
       LER-CEP01.
                READ CADMED NEXT
                IF ST-ERRO NOT = "00"
                   IF ST-ERRO = "10"
                      MOVE "*** FIM DO CADMED ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      MOVE "ERRO NA LEITURA CADMED"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
           MOVE CRM            TO CRMTX.
           MOVE NOME           TO NOMETX.
           MOVE ESPECIALIDADE  TO ESPECIALIDADETX.
           MOVE SEXO           TO SEXOTX.
           MOVE DATANASCIMENTO TO DATANASCIMENTOTX.
           MOVE EMAIL          TO EMAILTX.
           MOVE TELEFONE       TO TELEFONETX.
           MOVE MEDTX          TO REGMEDTX.
       INC-WR1.
                WRITE REGMEDTX
                IF ST-ERRO = "00" OR "02"
                      DISPLAY (20, 20) CRM
                      GO TO LER-CEP01
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO CADMEDTX"
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
                CLOSE CADMED CADMEDTX.
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
