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

*> `userInput` is parsed/validated into `radicand`
77 userInput   pic x(33) value is spaces.
77 radicand    pic s9(20)v9(11).

*> `guess` and `prevGuess` are used for sqrt() iteration
77 guess       pic s9(20)v9(11).
77 prevGuess   pic s9(20)v9(11).

*> Used for print formatting
77 answer      pic z(20).z(6).

*> --------------------------Main Program-------------------------------

procedure division.

    display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~".
    display "~       Babylonian Square Root Calculator        ~".
    display "~                                                ~".
    display "~          Enter positive numbers only!          ~".
    display "~           To quit, enter 'q' or '0'.           ~".
    display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~".

    *> Calculate sqrt() for user until they enter 0 or q
    display "Enter a number:".
    perform calcSqrt until userInput is = "q" or "0".

    *> Message to signify program exit
    display "    Exiting program. Thank you for calculating!   ".
    display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~".

    *> Done!
    stop run.

*> ----------------------Calculate Square Root--------------------------

calcSqrt.

    *> 1. Ask for user input
    display "  √" with no advancing.
    accept userInput end-accept.

    *> 2. ANSI escape to go back and erase previous line
    display x"1B" "[2F" x"0A" x"1B" "[K" with no advancing.

    *> 3. Exit prompt if "q" or "0"
    if userInput is = "q" or "0" then
        exit paragraph
    end-if.

    *> 4. Trim trailing spaces & parse string as number
    move function trim(userInput trailing) to radicand.
       
    *> 5. Error if negative or non-numeric input
    if radicand is <= 0 or function test-numval-f(userInput) is > 0 then
        display "  Invalid input - positive numbers only!" x"0A"
        exit paragraph
    end-if.

    *> 6. Calculate
    perform babylon.

    *> 7. Trim whitespace and display answer
    display "  √" function trim(userInput)
            " = " function trim(answer leading) x"0A".

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
