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
77 userInput   pic s9(20)v9(10).
77 answer      pic z(20).z(10).

*> ---------------------------Subroutine--------------------------------

procedure division using userInput, answer.

    *> Our initial guess will be half the input
    divide 2 into userInput giving guess rounded end-divide

    *> Iterate Babylonian sqrt until it is accurate enough
    perform with test after
    until function abs(guess - prevGuess) is < 0.000001
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
