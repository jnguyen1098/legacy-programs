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

*> The radicand is entered through `userInput`.
77 userInput   pic s9(20)v9(10).

*> `answer` is used for formatting
77 answer      pic z(20).z(10).

*> --------------------------Main Program-------------------------------

procedure division.

    display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~".
    display "~       Babylonian Square Root Calculator        ~".
    display "~                by Jason Nguyen                 ~".
    display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~".

    *> Repeatedly calculate sqrt() until user enters 0
    perform with test after until userInput = 0

        *> 1. Ask for user input 
        display "Enter number (0 to exit): " with no advancing
        accept userInput end-accept

        *> 2. Check for 0 (user exit condition)
        if userInput is = 0 then
            exit perform
        end-if

        *> 3. Check for invalid (negative) input
        if userInput is < 0 then
            display "Invalid input! Re-enter please."
            display " "
        else
            *> 4. Calculate
            call "squareroot" using userInput, answer

            *> Display answer. trim() removes trailing spaces
            display "Square root is " function trim(answer leading)
            display " "
        end-if

    end-perform

    *> Exit message for when the user enters 0
    display "Exiting program. Have a great day!"
    display " "

    *> Done!
    stop run.
    
*> ---------------------------------------------------------------------
