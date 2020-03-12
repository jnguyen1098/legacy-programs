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
77 radicand    pic s9(20)v9(10). *> Original number, or N
77 guess       pic s9(20)v9(10). *> First guess, or R0
77 prevGuess   pic s9(20)v9(10). *> Second guess, or R1
77 answer      pic z(20).z(10).  *> Second guess, but formatted

*> --------------------------Main Program-------------------------------

procedure division.

    display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~".
    display "~       Babylonian Square Root Calculator        ~".
    display "~                by Jason Nguyen                 ~".
    display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~".

    *> Prompt user for input until they exit by entering 0
    perform with test after until radicand = 0

        *> Ask for user input 
        display "Enter number (0 to exit): " with no advancing
        accept radicand end-accept

        *> Check for 0 (exit)
        if radicand is = 0 then
            continue
        end-if

        *> Check for negative before proceeding
        if radicand is < 0 then
            display "Invalid input! Re-enter please."
            display " "
        else
            call "sqrtfunc"
                using radicand, guess, prevGuess, answer
            end-call
        end-if

    end-perform

    *> Program breaks loop and exits on 0
    display "Exiting program. Have a great day!"
    display " "

    *> Done!
    stop run.
    
*> ---------------------------------------------------------------------
