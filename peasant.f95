!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                         Russian Peasant Multiplication                      !
!                                   in Fortran                                !
!                              Jason Nguyen (1013950)                         !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    program russian
    implicit none

!   Array of ranges I test the functions on
    integer, dimension(0:4) :: tests = [1, 5, 50, 500, 5000]

!   Interface for my two russian algorithms
    integer(kind=8) :: russianPeasantIterative
    integer(kind=8) :: russianPeasantRecursive

!   User interface vars
    integer(kind=8) :: m, n

!   Iterator var
    integer :: i

    print *, "-------------------------------------------"
    print *, "-- Russian Peasant Multiplication in F95 --"
    print *, "--       By Jason Nguyen (1013950)       --"
    print *, "-------------------------------------------"

!   Benchmarking
    call sleep(1)
    print *, "Please wait...running startup benchmarks..."
    print *, " "

!   Calling my dedicated benchmark functions
    do i = 1, 4
        call sleep(1)
        print '(" Multiplying every number from ", I0, " to ", I0)', &
            tests(i - 1), tests(i)
        call benchmarkRecursive(int(tests(i - 1), 8), int(tests(i), 8))
        call benchmarkIterative(int(tests(i - 1), 8), int(tests(i), 8))
        print *, " "
    end do

    do
!       Ask for user input
        print *, "Enter a positive number (to quit, enter a negative):"
        read(*, *) m

!       Abort if negative 
        if (m < 0) then
            exit
        end if

!       Continue asking
        print *, " "
        print *, "Enter another positive (to quit, enter a negative):"
        read(*, *) n

!       Check for abort
        if (n < 0) then
            exit
        end if

!       Calculate and print
        print *, " "
        print '("Recursive answer: ", I0)', russianPeasantRecursive(m, n)
        print '("Iterative answer: ", I0)', russianPeasantIterative(m, n)
        print *, " "

!       Terminate program if negative
        if (m < 0 .or. n < 0) then
            exit
        end if
    end do

!   Done
    print *, " "
    print *, "Thank you for using this program!"

end program russian

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

recursive function russianPeasantRecursive(m, n) result(output)
    implicit none
    integer(kind=8), intent(in) :: m, n
    integer(kind=8) :: output

!   Base cases
    if (m == 0) then
        output = 0
    else if (m == 1) then
        output = n

!   Recursive cases
    else if (mod(m, 2) == 0) then
        output = russianPeasantRecursive(m / 2, n * 2)
    else
        output = n + russianPeasantRecursive(m / 2, n * 2)
    end if

end function russianPeasantRecursive

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function russianPeasantIterative(m, n) result(output)
    implicit none
    integer(kind=8), intent(in) :: m, n
    integer(kind=8) :: output, multiplier, multiplicand

!   Initialization
    output = 0
    multiplicand = n
    multiplier = m

!   Iterative algorithm
    do while (multiplier > 0)
        if (mod(multiplier, 2) == 1) then
            output = output + multiplicand
        end if
        multiplier = multiplier / 2
        multiplicand = multiplicand * 2
    end do
end function russianPeasantIterative

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Benchmark the recursive function
subroutine benchmarkRecursive(m, n)
    implicit none
    integer(kind=8), intent(in) :: m, n

!   Interface
    integer(kind=8) :: russianPeasantRecursive
    integer(kind=8) :: output

!   Test iterator
    integer(kind=8) :: i, j

!   Timing
    real :: start, finish
    call cpu_time(start)
    do i = m, n
        do j = m, n
            output = russianPeasantRecursive(i, j)
        end do
    end do
    call cpu_time(finish)

!   Print results
    write(*,'(A,F8.6,A)') "    Recursive took ", finish-start, " seconds"
end subroutine benchmarkRecursive

! Benchmark the iterative function
subroutine benchmarkIterative(m, n)
    implicit none
    integer(kind=8), intent(in) :: m, n

!   Interface
    integer(kind=8) :: russianPeasantIterative
    integer(kind=8) :: output

!   Test iterator
    integer(kind=8) :: i, j
    real :: start, finish

!   Timing
    call cpu_time(start)
    do i = m, n
        do j = m, n
            output = russianPeasantIterative(i, j)
        end do
    end do
    call cpu_time(finish)

!   Print results
    write(*,'(A,F8.6,A)') "    Iterative took ", finish-start, " seconds"
end subroutine benchmarkIterative

