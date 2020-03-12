*> ---------------------------------------------------------------------
*> -                      Babylonian Square Roots                      -
*> -                         External Function                         -
*> -                     by Jason Nguyen (1013950)                     -
*> ---------------------------------------------------------------------

identification division.
program-id. sqrtfunc.

environment division.

data division.

linkage section.
77 radicand    pic s9(20)v9(10). *> Original number, or N
77 guess       pic s9(20)v9(10). *> First guess, or R0
77 prevGuess   pic s9(20)v9(10). *> Second guess, or R1
77 answer      pic z(20).z(10).  *> Second guess, but formatted

*> ---------------------------Subroutine--------------------------------

procedure division using radicand, guess, prevGuess, answer.

    *> Our initial guess will be half the input
    divide 2 into radicand giving guess rounded end-divide

    *> Iterate Babylonian sqrt until it is accurate enough
    perform with test after
    until function abs(guess - prevGuess) < 0.000001
        move guess to prevGuess *> Store last guess
        compute guess rounded = *> Calculate next guess
            (prevGuess + radicand / prevGuess) / 2
        end-compute
    end-perform
       
    *> Format the final guess
    move guess to answer
       
    *> Display it. We use the trim() function to remove spaces
    display "Square root is " function trim(answer leading)
    display " "

    exit program.

*> ---------------------------------------------------------------------
