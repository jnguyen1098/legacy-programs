*> ---------------------------------------------------------------------
*> -                      Babylonian Square Roots                      -
*> -                         External Function                         -
*> -                     by Jason Nguyen (1013950)                     -
*> ---------------------------------------------------------------------

identification division.
program-id. squareroot.

environment division.

data division.

working-storage section.

*> Temporary variables used for sqrt() iteration
77 guess       pic s9(20)v9(10).
77 prevGuess   pic s9(20)v9(10).

linkage section.

*> Radicand input and answer output
77 radicand    pic s9(20)v9(11).
77 answer      pic z(20).z(10).

*> ---------------------------Subroutine--------------------------------

procedure division using radicand, answer.

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
