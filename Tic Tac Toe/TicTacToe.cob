      ******************************************************************
      * Author: Olivier Saint-Vincent
      * Date: 7/26/2022
      * Purpose: Learning
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEARNING-COBOL.
       AUTHOR. Olivier Saint-Vincent.
       DATE-WRITTEN. July 26th 2022.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. 

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       
       01 GameBoard.
           02 S1 PIC 9.
              88 O1 VALUE 1, 4, 3.
           02 S2 PIC 9.
              88 O2 VALUE 1, 4, 3.
           02 S3 PIC 9.
              88 O3 VALUE 1, 4, 3.
           02 S4 PIC 9.
              88 O4 VALUE 1, 4, 3.
           02 S5 PIC 9.
              88 O5 VALUE 1, 4, 3.
           02 S6 PIC 9.
              88 O6 VALUE 1, 4, 3.
           02 S7 PIC 9.
              88 O7 VALUE 1, 4, 3.
           02 S8 PIC 9.
              88 O8 VALUE 1, 4, 3.
           02 S9 PIC 9.
              88 O9 VALUE 1, 4, 3.
       01 RowCheckTotal Pic 99. 
       01 Turn PIC 9 VALUE 1.
           88 1turn VALUE 1.
           88 4turn VALUE 4.
       01 PlayAgainInput PIC A. 
       01 PlayAgainFlag PIC 9. 
           88 ContinueGame VALUE 1.
       01 GameOnFlag PIC 9.
           88 GameOn VALUE 1.
       01 ROWS.
           02 RC1 PIC 99.
              88 C1 VALUE 3, 12.
           02 RC2 PIC 99.
              88 C2 VALUE 3, 12.
           02 RC3 PIC 99.
              88 C3 VALUE 3, 12.
           02 RC4 PIC 99.
              88 C4 VALUE 3, 12.
           02 RC5 PIC 99.
              88 C5 VALUE 3, 12.
           02 RC6 PIC 99.
              88 C6 VALUE 3, 12.
           02 RC7 PIC 99.
              88 C7 VALUE 3, 12.
           02 RC8 PIC 99.
              88 C8 VALUE 3, 12.
       01 GameState PIC 9.
           88 GameOver Value 1.
       01 GameMove PIC 9.
           88 M1 VALUE 1.
           88 M2 VALUE 2.
           88 M3 VALUE 3.
           88 M4 VALUE 4.
           88 M5 VALUE 5.
           88 M6 VALUE 6.
           88 M7 VALUE 7.
           88 M8 VALUE 8.
           88 M9 VALUE 9.
       01 ValidMoveBool PIC 9.
           88 ValidMove Value 1.

           



       LINKAGE SECTION.              
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

       
           DISPLAY "Hello, Welcome to COBOL Tic Tac Toe."
           DISPLAY "Want to play? Y/N"
           DISPLAY " "
           DISPLAY "1" " | " "2" " | " "3"
           DISPLAY "---------"
           DISPLAY "4" " | " "5" " | " "6"
           DISPLAY "---------"
           DISPLAY "7" " | " "8" " | " "9"
           DISPLAY " "

           ACCEPT PlayAgainInput 
           IF PlayAgainInput = "Y" THEN 
           MOVE 1 TO PlayAgainFlag 
           END-IF 


           MOVE 000000000 TO GameBoard

           
           PERFORM UNTIL Not ContinueGame

           

           PERFORM UNTIL GameOver 

           MOVE 0 TO ValidMoveBool
           DISPLAY " "
           
           
           IF 1TURN THEN

           DISPLAY "1 To Play" 

           ELSE IF 4TURN THEN


           DISPLAY "4 To Play"
           
           END-IF
           END-IF

           DISPLAY "Play a move:"


           PERFORM UNTIl ValidMove  
           DISPLAY " "
           ACCEPT GameMove 
           DISPLAY " "

           If GameMove = 0 THEN
           DISPLAY "Play a valid move!"
           ELSE IF M1 and O1 THEN 
           DISPLAY "Play a valid move!"
           ELSE IF M2 and O2 THEN 
           DISPLAY "Play a valid move!"
           ELSE IF M3 and O3 THEN 
           DISPLAY "Play a valid move!"
           ELSE IF M4 and O4 THEN 
           DISPLAY "Play a valid move!"
           ELSE IF M5 and O5 THEN 
           DISPLAY "Play a valid move!"
           ELSE IF M6 and O6 THEN 
           DISPLAY "Play a valid move!"
           ELSE IF M7 and O7 THEN 
           DISPLAY "Play a valid move!"
           ELSE IF M8 and O8 THEN 
           DISPLAY "Play a valid move!"
           ELSE IF M9 and O9 THEN 
           DISPLAY "Play a valid move!"
           ELSE

           DISPLAY "Great Move!"
           MOVE 1 TO ValidMoveBool

           IF M1 THEN
           MOVE Turn TO S1
           ELSE IF M2 THEN 
           MOVE Turn TO S2
           ELSE IF M3 THEN 
           MOVE Turn TO S3
           ELSE IF M4 THEN
           MOVE Turn TO S4 
           ELSE IF M5 THEN
           MOVE Turn TO S5 
           ELSE IF M6 THEN
           MOVE Turn TO S6 
           ELSE IF M7 THEN
           MOVE Turn TO S7 
           ELSE IF M8 THEN
           MOVE Turn TO S8 
           ELSE IF M9 THEN 
           MOVE Turn TO S9
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF 

           END-PERFORM

           IF 1TURN THEN

           MOVE 4 TO Turn

           ELSE IF 4TURN THEN

           MOVE 1 TO Turn
           
           END-IF
           END-IF 
           DISPLAY " "
           DISPLAY s1 " | " s2 " | " s3
           DISPLAY "---------"
           DISPLAY s4 " | " s5 " | " s6
           DISPLAY "---------"
           DISPLAY s7 " | " s8 " | " s9
           DISPLAY " "
           COMPUTE RC1 = S1 + S2 + S3 
           COMPUTE RC2 = S4 + S5 + S6 
           COMPUTE RC3 = S7 + S8 + S9 
           COMPUTE RC4 = S1 + S4 + S7
           COMPUTE RC5 = S2 + S5 + S8 
           COMPUTE RC6 = S3 + S6 + S9 
           COMPUTE RC7 = S1 + S5 + S9 
           COMPUTE RC8 = S3 + S5 + S7 
           
           

           IF RC1 = 3 THEN
           DISPLAY "1 Wins!"          
           MOVE 333333333 TO GameBoard
           ELSE IF RC2 = 3 THEN
           DISPLAY "1 Wins!"
           MOVE 333333333 TO GameBoard           
           ELSE IF RC3 = 3 THEN
           DISPLAY "1 Wins!"
           MOVE 333333333 TO GameBoard          
           ELSE IF RC4 = 3 THEN
           DISPLAY "1 Wins!"
           MOVE 333333333 TO GameBoard           
           ELSE IF RC5 = 3 THEN
           DISPLAY "1 Wins!"
           MOVE 333333333 TO GameBoard           
           ELSE IF RC6 = 3 THEN
           DISPLAY "1 Wins!"
           MOVE 333333333 TO GameBoard           
           ELSE IF RC7 = 3 THEN
           DISPLAY "1 Wins!"
           MOVE 333333333 TO GameBoard          
           ELSE IF RC8 = 3 THEN
           DISPLAY "1 Wins!"
           MOVE 333333333 TO GameBoard           
           ELSE IF RC1 = 12 THEN
           DISPLAY "4 Wins!"
           MOVE 333333333 TO GameBoard           
           ELSE IF RC2 = 12 THEN
           DISPLAY "4 Wins!"
           MOVE 333333333 TO GameBoard   
           ELSE IF RC3 = 12 THEN
           DISPLAY "4 Wins!"
           MOVE 333333333 TO GameBoard
           ELSE IF RC4 = 12 THEN
           DISPLAY "4 Wins!"
           MOVE 333333333 TO GameBoard
           ELSE IF RC5 = 12 THEN
           DISPLAY "4 Wins!"
           MOVE 333333333 TO GameBoard
           ELSE IF RC6 = 12 THEN
           DISPLAY "4 Wins!"
           MOVE 333333333 TO GameBoard
           ELSE IF RC7 = 12 THEN
           DISPLAY "4 Wins!"
           MOVE 333333333 TO GameBoard
           ELSE IF RC8 = 12 THEN
           DISPLAY "4 Wins!"
           MOVE 333333333 TO GameBoard
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF


            

           IF O1 AND O2 AND O3 AND O4 THEN 
           IF O5 AND O6 AND O7 AND O8 AND O9 THEN 
           DISPLAY "Game Done!"
           MOVE 1 TO GameState
           END-IF
           END-IF
           
           


           
           END-PERFORM

           MOVE 000000000 TO GameBoard
           
           DISPLAY "Want to play? Y/N"
           MOVE 0 TO PlayAgainFlag
           ACCEPT PlayAgainInput 
           IF PlayAgainInput = "Y" THEN 
           MOVE 1 TO PlayAgainFlag 
           MOVE 0 TO GameState 
      
           END-IF
           END-PERFORM
           
           STOP RUN.
       END PROGRAM LEARNING-COBOL.
