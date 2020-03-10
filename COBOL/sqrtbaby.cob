*> ------------------------------------------------------------------
*> -                    Babylonian Square Roots                     -
*> -                  Re-engineered with love. <3                   -
*> -                   by Jason Nguyen (1013950)                    -
*> ------------------------------------------------------------------

identification division.
program-id. sqrtbaby.

environment division.

data division.

working-storage section.
77 radicand    pic s9(20)v9(10). *> Original number, or N
77 guess       pic s9(20)v9(10). *> First guess, or R0
77 prevGuess   pic s9(20)v9(10). *> Second guess, or R1
77 answer      pic z(20).z(10).  *> Second guess, but formatted

*> --------------------------Main Program----------------------------

procedure division.

    display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~".
    display "~       Babylonian Square Root Calculator        ~".
    display "~                by Jason Nguyen                 ~".
    display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~".

    *> Prompt user for input until they exit by entering 0
    perform with test after until radicand = 0
        display "Enter a number (or 0 to exit): " with no advancing
        accept radicand end-accept

        *> If the input radicand is positive (> 0), calculate sqrt()
        *> If the input radicand is zero     (= 0), terminate program
        *> If the input radicand is negative (< 0), force re-prompt
        
        if radicand is > 0 then
            *> Our initial guess will be half the input
            divide 2 into radicand giving guess rounded

            *> Iterate Babylonian sqrt until it is accurate enough
            perform with test after until function
            abs(guess - prevGuess) < 0.000001
                *> Store last guess
                move guess to prevGuess
                *> Calculate next guess
                compute guess rounded =
                    (prevGuess + radicand / prevGuess) / 2
                end-compute
            end-perform

            *> Format the final guess
            move guess to answer

            *> Display it. We use the trim() function to remove spaces
            display "Square root is " with no advancing
            display function trim(answer leading)
            display " "
        else
            if radicand is = 0 then *> User exits program
                display "Good bye. Thank you for using my program!"
                display " "
            else *> User enters invalid input
                display "Invalid input! Re-try"
                display " "
            end-if
        end-if
    end-perform

    stop run.

*> ------------------------------------------------------------------
