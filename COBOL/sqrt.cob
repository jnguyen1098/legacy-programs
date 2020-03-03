identification division.
program-id. SQRT.

environment division.

input-output section.
file-control.
    select inputFile assign to "sqrt.dat"
        organization is line sequential.
    select standardOutput assign to display.

data division.
file section.
fd inputFile.
    01 standardInput   pic X(80).
fd standardOutput.
    01 outLine         pic X(80).

working-storage section.
77 diff    pic v9(5).
77 z       pic 9(11)V9(6).
77 k       pic s9999.
77 x       pic 9(11)v9(6).
77 y       pic 9(11)v9(6).
77 temp    pic 9(11)v9(6).

01 inCard.
    02 inZ    pic s9(10)v9(6) sign leading separate.
    02 inDiff pic v9(5).
    02 filler pic x(58).

01 titleLine.
    02 filler pic x(9) value spaces.
    02 filler pic x(26) value 'Square Root Approximations'.

01 underLine1.
    02 filler pic x(44) value
       '--------------------------------------------'.

01 colHeads.
    02 filler pic x(8) value spaces.
    02 filler pic x(6) value 'Number'.
    02 filler pic x(15) value spaces.
    02 filler pic x(11) value 'Square Root'.

01 underLine2.
    02 filler pic x(20) value ' -------------------'.
    02 filler pic x(5) value spaces.
    02 filler pic x(19) value '------------------'.

01 printLine.
    02 filler pic x value space.
    02 outZ pic z(11)9.9(6).
    02 filler pic x(5) value spaces.
    02 outY pic z(11)9.9(6).

01 errorMessage.
    02 filler pic x value space.
    02 otZ pic -(11)9.9(6).
    02 filler pic x(21) value '        Invalid Input'.

01 abortMessage.
    02 filler pic x value space.
    02 outpZ pic z(11)9.9(6).
    02 filler pic x(37) value '  Attempt aborted.Too many iterations'.

procedure division.
    open input inputFile, output standardOutput.
    write outLine from titleLine after advancing 0 lines.
    write outLine from underLine1 after advancing 1 line.
    write outLine from colHeads after advancing 1 line.
    write outLine from underLine2 after advancing 1 line.

s1.
    read inputFile into inCard at end go to finish.
    if inZ is greater than zero go to b1.
    move inZ to otZ.
    write outLine from errorMessage after advancing 1 line.
    go to s1.

b1.
    move inDiff to diff.
    move inZ to z.
    divide 2 into z giving x rounded.
    perform s2 thru e2 varying k from 1 by 1
       until k is greater than 1000.
    move inZ to outpZ.
    write outLine from abortMessage after advancing 1 line.
    go to s1.

s2.
    compute y rounded = 0.5 * (x + z / x).
    subtract x from y giving temp.
    if temp is less than zero compute temp = - temp.
    if temp / (y + x) is greater than diff go to e2.
    move inZ to outZ.
    move y to outY.
    write outLine from printLine after advancing 1 line.
    go to s1.

e2.
    move y to x.

finish.
    close inputFile, standardOutput.
stop run.
