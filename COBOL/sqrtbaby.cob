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

    *> Calculate sqrt() until user enters 0, blank input, or EOF
    perform calcSqrt with test after until userInput is = 0.

    *> Exit message (will also exit when a non-number is entered)
    display "Exiting program. Have a great day!".
    display " ".

    *> Done!
    stop run.

*> ----------------------Calculate Square Root--------------------------

calcSqrt.

    *> 1. Ask for user input
    display "Enter number (or 0 to exit): " with no advancing.
    accept userInput.
       
    *> 2. Check for 0 (user exit condition)
    if userInput is = 0 then
        exit paragraph
    end-if.
       
    *> 3. Check for invalid (negative) input
    if userInput is < 0 then
        display "Input must be positive. Re-enter."
        display " "
        exit paragraph
    end-if.

    *> 4. Calculate
    perform babylon.

    *> 5. Display answer. trim() removes trailing spaces
    display "Square root is ", function trim(answer leading).
    display " ".

    *> All done!
    exit paragraph.

*> ----------------------Babylonian Algorithm---------------------------

babylon.

    *> Our initial guess will be half the input
    compute guess rounded = userInput / 2.

    *> Iterate Babylonian sqrt until it is accurate enough
    perform with test after
    until function abs(guess - prevGuess) is < 0.000001
        *> Store last guess
        move guess to prevGuess
        *> Calculate next guess
        compute guess rounded = (prevGuess + userInput / prevGuess) / 2
    end-perform.

    *> Format the final guess
    move guess to answer.

    *> We are done!
    exit paragraph.

*> ---------------------------------------------------------------------
