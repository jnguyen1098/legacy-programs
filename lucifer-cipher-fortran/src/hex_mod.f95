!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                     !
!                   Hex Module for Lucifer Algorithm                  !
!                                                                     !
!                        Jason Nguyen (XXXXXXX)                       !
!                                                                     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module hex
      implicit none

      contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine readWord(w)

!   I made it 11 char. long to detect 'overflow'. Fortran refuses to
!   read past 11 char., so if I impose a maximum of 10 characters and
!   this function returns a string of length 11, I will call error.
      character (len = 11), intent(out) :: w

!   I'm sure there's a better way to format this. 
      read(*,*) w

      end subroutine readWord

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine word2hex(w, h, l)

!   w is usually the word we get from readWord(w).
!   h is the output array, which is w, but in hex.
!   l is the resultant length. Because we are going from native string
!   to array of integers, we no longer have the luxury of strlen.
      character (len = 10), intent(in) :: w
      integer, dimension(0:31), intent(out) :: h
      integer, intent(out) :: l

!   i is used to iterate through the input, from 1 to len_trim(w),
!      where len_trim(w) is the usable length of the string.
!   j is used to keep track of how many characters are being written
!      to the final output array, h. That way we can assign it to l,
!      the length of the string
!   tempH is a temporary 2-member array that holds the intermediate
!      representation of the conversion to hex (e.g. 7F). We then
!      convert the individual two members to integers in the output
      integer :: i, j = 0
      integer, dimension(1:2) :: hex
      character (len = 2) :: tempH

!   Here we iterate over every usable character of the input string
      do i = 1, len_trim(w)

!      i will be our iterator variable, so we take the character at
!      i of w ( w(i:i) ) and write two characters to the tempH array;
!      these two characters represent the left and right parts of the
!      hex representation of each character.
         write(tempH, '(Z2)') w(i:i)

!      Then, we take each of the two characters in tempH and write them
!      to another temporary array of two members, this time it's hex,
!      which is an array of two integers. They represent the cardinal
!      value of the hex. From 0-9 it's 0-9, but characters A,B,C,D,E,F
!      are actually represented as 10,11,12,13,14,15 in decimal, hence
!      why we are converting it as such
         read(tempH(1:1), '(Z1)') hex(1)
         read(tempH(2:2), '(Z1)') hex(2)

!      Every time this step completes, we increment the j variable to
!      keep track of the final length of the hex array when writing.
         h(j) = hex(1)
         j = j + 1
         h(j) = hex(2)
         j = j + 1

      end do

!   Finally, we write the final value of j to the length variable.
      l = j

      end subroutine word2hex

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine printhex(h, l)

!   h represents our input word of any length. We can't make assumptions
!      about the length as it is being passed as an argument, so...
!   l represents the length of the array
      integer, dimension(0:*), intent(in) :: h
      integer, intent(in) :: l

!   i is our iterator variable that we use to iterate over w, from 0 to
!      l - 1, thanks to the passed array length
!   chars is the lookup table I am using for the conversion from integer
!      to hex character (...9, 10, 11, 12...) => (...9, A, B, C...). I
!      am sure there's a better solution, but I liked the elegance of
!      my mapping. I hope this doesn't break anything!!!!!
      integer :: i
      character, dimension(0:15) :: chars = ['0', '1', '2', '3', '4', &
      '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F']

!   Again, we iterate over the string; for each character, we are piping
!   the output to the lookup table for its respective character. We then
!   print the resultant character out.
      do i = 0, l - 1
         write(*,'(A)',advance="no") chars(h(i))
      end do

      end subroutine printhex

      end module hex

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
