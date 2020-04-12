*> ------------------------------------------------------------------
*> -                    Babylonian Square Roots                     -
*> -                  Re-engineered with love. <3                   -
*> -                   by Jason Nguyen (XXXXXXXX)                    -
*> ------------------------------------------------------------------

*> - reworked the time-out logic so that the MAKE_NEXT_GUESS is
*>   now embedded within the GET_NEW_LINE paragraph. Will need 
*>   some polishing in my opinion

identification division.
program-id. SQRT.

environment division.

input-output section.
file-control.
    select inputFile assign to "sqrt.dat"
        organization is line sequential.
    select standardOutput assign to display.

data division.
file section.
fd inputFile.
    01 standardInput   pic X(80).
fd standardOutput.
    01 outLine         pic X(80).

working-storage section.
77 epsilon     pic v9(5).
77 radicand    pic 9(11)V9(6).

77 guess       pic 9(11)v9(6).
77 nextGuess   pic 9(11)v9(6).

77 i           pic s9999.

77 eofSwitch   pic 9 value 1.

*> formatted line to be read in
*> num: 1 (sign), 10 (characteristic), 6 (mantissa)
*> eps: 5 (epsilon needed for halt, v implied decimal)
*> the x(58) represents 58 unused spaces in the file
01 lineStruct.
    02 radicandIn      pic s9(10)v9(6) sign leading separate.
    02 epsilonIn       pic v9(5). *> reads in 00100
    02 filler pic x(58). *> reads in 58 spaces lmfao

*> ------------------------Logging Messages--------------------------
01 printLine.
    02 filler pic x value space.
    02 outZ pic z(11)9.9(6).
    02 filler pic x(5) value spaces.
    02 outY pic z(11)9.9(6).

01 errorMessage.
    02 filler pic x value space.
    02 otZ pic -(11)9.9(6).
    02 filler pic x(21) value '        Invalid Input'.

01 abortMessage.
    02 filler pic x value space.
    02 outpZ pic z(11)9.9(6).
    02 filler pic x(37) value '  Attempt aborted.Too many iterations'.
*> --------------------------Main Program----------------------------
procedure division.
    open input inputFile, output standardOutput.
    display "         Square Root Approximations".
    display "--------------------------------------------".
    display "        Number               Square Root".
    display " -------------------     ------------------"
    with no advancing.

    perform GET_NEW_LINE until eofSwitch = 0.
    close inputFile, standardOutput.
    stop run.

GET_NEW_LINE.
    read inputFile into lineStruct
        at end move 0 to eofSwitch
        exit paragraph
    end-read.

    if radicandIn is greater than zero
        move epsilonIn to epsilon
        move radicandIn to radicand
        divide 2 into radicand giving guess rounded

        perform varying i from 1 by 1 until i is greater than 1000
            compute nextGuess rounded = 0.5 * (guess + radicand / guess)
            if (function abs(guess - nextGuess)) / (nextGuess + guess)
                is not greater than epsilon
                    exit perform
            else
                move nextGuess to guess
            end-if 
        end-perform
        if i is not greater than 1000
            move radicandIn to outZ
            move nextGuess to outY
            write outLine from printLine after advancing 1 line
        else
            move radicandIn to outpZ
            write outLine from abortMessage after advancing 1 line
        end-if
    else
        move radicandIn to otZ
        write outLine from errorMessage after advancing 1 line
    end-if.

*> ------------------------------------------------------------------
