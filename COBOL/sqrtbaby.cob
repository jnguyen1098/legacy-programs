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

*> The number is entered through `userInput`.
77 userInput   pic s9(20)v9(11).

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

    *> Repeatedly calculate sqrt() until user enters 0
    perform calcSqrt with test after until userInput = 0.

    *> Exit message for when the user enters 0
    display "Exiting program. Have a great day!".
    display " ".

    *> Done!
    stop run.

*> ----------------------Calculate Square Root--------------------------

calcSqrt.

    *> 1. Ask for user input
    display "Enter number (0 to exit): " with no advancing
    accept userInput end-accept
       
    *> 2. Check for 0 (user exit condition)
    if userInput is = 0 then
        exit paragraph
    end-if
       
    *> 3. Check for invalid (negative) input
    if userInput is < 0 then
        display "Invalid input! Re-enter please."
        display " "
    else
        *> 4. Calculate
        perform babylon

        *> Display answer. trim() removes trailing spaces
        display "Square root is ", function trim(answer leading)
        display " "
    end-if

    *> We are done!
    exit paragraph.

*> ----------------------Babylonian Algorithm---------------------------

babylon.

    *> Our initial guess will be half the input
    divide 2 into userInput giving guess rounded end-divide

    *> Iterate Babylonian sqrt until it is accurate enough
    perform with test after
    until function abs(guess - prevGuess) < 0.000001
        *> Store last guess
        move guess to prevGuess
        *> Calculate next guess
        compute guess rounded =
            (prevGuess + userInput / prevGuess) / 2
        end-compute
    end-perform

    *> Format the final guess
    move guess to answer

    *> We are done!
    exit paragraph.

*> ---------------------------------------------------------------------
