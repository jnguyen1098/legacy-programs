*> ---------------------------------------------------------------------
*> -                      Babylonian Square Roots                      -
*> -                    (reengineered with love <3)                    -
*> -                     by Jason Nguyen (1013950)                     -
*> ---------------------------------------------------------------------

identification division.
program-id. sqrtbaby.

environment division.

data division.

working-storage section.

*> `userInput` is validated and parsed into `radicand`
77 userInput   pic x(33) value is spaces.
77 radicand    pic s9(20)v9(11).

*> `guess` and `prevGuess` are used for sqrt() iteration
77 guess       pic s9(20)v9(11).
77 prevGuess   pic s9(20)v9(11).

*> Used for print formatting
77 answer      pic z(19)9.9(6).

*> --------------------------Main Program-------------------------------

procedure division.

    display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~".
    display "~       Babylonian Square Root Calculator        ~".
    display "~                                                ~".
    display "~          Enter positive numbers only!          ~".
    display "~           To quit, enter 'q' or '0'.           ~".
    display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~".
    display "Enter a number:                                   ".

    *> Parse user's input until they enter 0 or q
    perform calcSqrt until userInput is = "q" or "0".

    *> Message to signify program exit
    display "    Exiting program. Thank you for calculating!   ".
    display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~".

    *> Done!
    stop run.

*> ----------------------Calculate Square Root--------------------------

calcSqrt.

    *> User input
    display "  √" with no advancing.
    accept userInput end-accept.

    *> Clean up output (line overwriting using ANSI CSI)
    display x"1B" "[2F" x"0A" x"1B" "[K" with no advancing.

    *> Don't proceed if "q" or "0"
    if userInput is = "q" or "0" then
        exit paragraph
    end-if.

    *> Sanitize user input and parse string as number
    move function trim(userInput trailing) to radicand.
       
    *> Proceed only if parsed number is valid AND positive
    if radicand is <= 0 or function test-numval-f(userInput) is > 0 then
        display "  Invalid input: positive numbers only!" x"0A"
        exit paragraph
    else
        *> Proceed on
        perform babylon

        *> Clean up and print answer line
        display "  √" function trim(userInput)
                " = " function trim(answer leading) x"0A"
    end-if.

*> ----------------------Babylonian Algorithm---------------------------

babylon.

    *> Our initial guess will be half the input
    compute guess rounded = radicand / 2.

    *> Iterate sqrt() until desired precision
    perform with test after until
    function abs(guess - prevGuess) is < 0.000001
        *> Store last guess
        move guess to prevGuess
        *> Calculate next guess using last guess
        compute guess rounded = (prevGuess + radicand / prevGuess) / 2
    end-perform.

    *> Return output
    move guess to answer.

*> ---------------------------------------------------------------------
