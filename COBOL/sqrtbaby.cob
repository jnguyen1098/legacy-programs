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
77 userInput   pic x(33).
77 radicand    pic s9(20)v9(11).

*> `guess` and `prevGuess` are used for sqrt() iteration
77 guess       pic s9(20)v9(11).
77 prevGuess   pic s9(20)v9(11).

*> Used for print formatting
77 answer      pic z(20).z(11).

*> --------------------------Main Program-------------------------------

procedure division.

    display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~".
    display "~       Babylonian Square Root Calculator        ~".
    display "~                by Jason Nguyen                 ~".
    display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~".

    *> Calculate sqrt() for user until they enter 0 or q
    perform calcSqrt until userInput is = "q" or "0".

    *> Message to signify program exit
    display "Exiting program. Have a great day!".
    display " ".

    *> Done!
    stop run.

*> ----------------------Calculate Square Root--------------------------

calcSqrt.

    *> 1. Ask for user input
    display "Enter number ('q' or '0' to exit): " with no advancing.
    accept userInput.

    *> 2. Exit prompt if "q" or "0"
    if userInput is = "q" or "0" then
        exit paragraph
    end-if.

    *> 3. Test input to see if numeric (0 = passes test)
    if function test-numval-f(userInput) is not = 0 then
        display "Input is non-numeric. Please re-enter!"
        display " "
        exit paragraph
    end-if.

    *> 4. Trim trailing spaces & parse string as number
    move function trim(userInput trailing) to radicand.
       
    *> 5. Check if the parsed number is negative
    if radicand is < 0 then
        display "Input can't be negative. Please re-enter!"
        display " "
    else
        *> 6. Calculate
        perform babylon

        *> 7. Trim leading spaces and display answer
        display "Square root is ", function trim(answer leading)
        display " "
    end-if.

    *> We are done!
    exit paragraph.

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

    *> Format the final guess
    move guess to answer.

    *> We are done!
    exit paragraph.

*> ---------------------------------------------------------------------
