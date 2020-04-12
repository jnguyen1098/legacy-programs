*> ------------------------------------------------------------------
*> -                    Babylonian Square Roots                     -
*> -                  Re-engineered with love. <3                   -
*> -                   by Jason Nguyen (XXXXXXXX)                    -
*> ------------------------------------------------------------------

identification division.
program-id. SQRT.
environment division.
data division.

working-storage section.
77 radicand    pic s9(12)v9(7).
77 guess       pic s9(12)v9(7).
77 prevGuess   pic s9(12)v9(7).
77 answer      pic z(11).z(6).

*> --------------------------Main Program----------------------------

procedure division.

    display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
    display "~       Babylonian Square Root Calculator        ~"
    display "~                by Jason Nguyen                 ~"    
    display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

    perform with test after until radicand = 0 *> Exit when 0 is input
        *> Prompting user for radicand input
        display "Enter a number (or 0 to exit): " with no advancing
        accept radicand

        *> Proceeds if number is valid (in sqrt(x), x must be >= 0)
        if radicand > 0
            *> Our initial guess will be half the number
            divide 2 into radicand giving guess rounded

            *> Iterate until we are below a threshold for absolute error
            perform with test after until function abs(guess - prevGuess) < 0.000001
                move guess to prevGuess
                compute guess rounded = (prevGuess + radicand / prevGuess) / 2
            end-perform

            *> Format the final guess
            move guess to answer

            *> Display it. We use the trim() function to remove spaces
            display "Square root is " with no advancing
            display function trim(answer leading)
            display " "
        else
            *> 0 is a sentinel value to check if the user wants to exit
            if radicand = 0
                display "Thank you for using my program!"
                display "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
                display " "

            *> Everything else after this is considered invalid
            else
                display "Invalid input! Re-try"
                display " "
            end-if
        end-if
    end-perform.
    
    stop run.

*> ------------------------------------------------------------------
