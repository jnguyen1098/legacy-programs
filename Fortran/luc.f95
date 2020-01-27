!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                     !
!                                Lucifer                              !
!                                                                     !
!                         Jason Nguyen (1013950)                      !
!                                                                     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      program luc
      implicit none

!   Temp counter used to iterate through arrays
      integer :: i

!   kb is the raw input from the user, representing the 32 bytes
!   kb is then expanded into key, from an array of bytes to binary.
!
!   Now, we need to read key as an array of 8 x 16, not just a line
!   of 128 binary integers. So we later reshape key (1 x 128) into
!   k (8 x 16) by means of reshape()
      integer, dimension (0:31) :: kb
      integer, dimension (0:127) :: key
      integer, dimension (0:7, 0:15) :: k

!   The same story applies here. We read 32 bytes into the message
!   plaintext, and then we use expand() to turn mb, an array of 32
!   bytes, into a binary representation of 128 binary digits. The
!   only difference here is that instead of the 1 x 128 array being
!   mapped onto an 8 x 16 array, we are mapping it to 8 x 8 x 2
      integer, dimension (0:31) :: mb
      integer, dimension (0:127) :: message
      integer, dimension (0:7, 0:7, 0:1) :: m

!   Error checking
      integer :: readerror = 1

!   Prompt the user for the key first
      do
         print *, ' '
         print *, 'Enter your key (0-9, A-F and a-f only):'
         read(*,'(32z1.1)', iostat = readerror) (kb(i),i=0,31)

         if (readerror == 0) then
            exit
         end if

         print *, 'Invalid input. Please try again.'
      end do

!   Prompt the user for the plaintext next
      do
         print *, ' '
         print *, 'Enter plaintext (0-9, A-F and a-f only):'
         read(*,'(32z1.1)', iostat = readerror) (mb(i),i=0,31)

         if (readerror == 0) then
            exit
         end if

         print *, 'Invalid input. Please try again.'
      end do

!   Expand both the message (mb) and key (kb) as explained above
      call expand(message, mb, 32)
      call expand(key, kb, 32)

!   Reshape key and message from (1 x 128) to their respective
!   mappings. (8 x 16) for key, (8 x 8 x 2) for message.
      k = reshape(key, [8, 16])
      m = reshape(message, [8, 8, 2])

!   First we encipher with the lucifer function. This is shown
!   by the first argument being 0.
      call lucifer(0, k, m)

!   Begin section 'Encrypted message'
      print *, ''
      print *, '~~~~~~Encrypted message~~~~~~'

!   Print ciphertext
      print *, 'Ciphertext:'
      call compress(m, mb, 32)
      print '(32z1.1)', (mb(i), i = 0, 31)

!   Next, we decipher with the lucifer function. This is shown
!   by the first argument being 1.
      call lucifer(1, k, m)

!   Finally, we compress the resultant message and key
      call compress(message, mb, 32)
      call compress(key, kb, 32)

!   Begin section 'Decrypted message'
      print *, ' '
      print *, '~~~~~~Decrypted message~~~~~~'

!   Then we print them both again for debugging purposes . . .
      print *, 'Key:'
      print '(32z1.1)', (kb(i), i = 0, 31)

!   . . . so if the program ran successfully, the key and plain
!   should be identical to the key and plain at the beginning
      print *, 'Plaintext:'
      print '(32z1.1)', (mb(i), i = 0, 31)
      print *, ' '

      end program luc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine lucifer(d,k,m)
!   This algorithm uses Feistel rounds. As in, it encrypted the data
!   by performing an encipherment step on it several times. This
!   encipherment step involved taking the key for that step, and half
!   of the block in order to produce a result to be applied to the XOR
!   of the remnant half of the block.
!
!   What made this solution particularly elegant was that the two
!   halves of the block would be switched after each step, allowing
!   each subsequent operation of the same type to evenly operate on all
!   parts of the respective block.
      implicit none

!   Function arguments
      integer, intent(in) :: d ! mode; 0 = cipher, 1 = decipher
      integer, dimension (0:7, 0:15), intent(in) :: k ! key
      integer, dimension (0:7, 0:7, 0:1), intent(inout) :: m ! message

!   Diffusion pattern
      integer, dimension (0:7) :: o = [7, 6, 2, 1, 5, 0, 3, 4]

!   Inverse of fixed permutation
      integer, dimension (0:7) :: tr, pr = [2, 5, 4, 0, 3, 1, 7, 6]

!   S-box permutations, S_0
      integer, dimension (0:15) :: s0 = [12, 15, 7, 10, 14, 13, 11, &
              0, 2, 6, 3, 1, 9, 4, 5, 8]

!   S-box permutations, S_1
      integer, dimension (0:15) :: s1 = [7, 2, 14, 9, 3, 11, 0, 4, &
              12, 13, 1, 10, 6, 15, 8, 5]

!   Other variables
      integer :: h0, h1, h, ii, jj, kc, kk, ks, l, temp, v
      integer, dimension (0:7,0:7) :: swap_temp

!   Unlike the original implementation that involved physically moving
!   information, this implementation saves the effort by using pointers
!   to indicate which part of the array(s) the algorithm works on.

!   The halves of the message byte selected are used as input to the
!   S_0 and S_1 permutations in order to create 4 v-bits each.

!   When k(jj, ks) = 0, the low-order  4 bits are used with s0
!                       the high-order 4 bits are used with s1

!   When k(jj, ks) = 1, the low-order  4 bits are used with s1
!                       the high-order 4 bits are used with s0

!      +-------------------+-----+-----+
!      |                   |  0  |  1  |
!      +-------------------+-----+-----+
!      | Low-Order 4 Bits  | s_0 | s_1 |
!      +-------------------+-----+-----+
!      | High-Order 4 Bits | s_1 | s_0 |
!      +-------------------+-----+-----+

!   Like stated earlier, we only point to the halves of the block:

      h0 = 0 !   lower half of the block
      h1 = 1 !   upper half of the block

      if (d == 1) then
         kc = 8
      else
         kc = 0
      end if

!   Sorkin's variant of Lucifer uses 16 Feistel rounds
      do ii = 1, 16

!        c-i-d (confusion, interruption, diffusion) cycle
         if (d == 1) then
            kc = mod(kc + 1, 16)
         end if

!        ks is the index of the 'transform control byte'
         ks = kc

         do jj = 0, 7
            l = 0
            h = 0

!           Here we construct the integer values of the hexdigits of
!           one byte of the message. We could have used compress(),
!           but this method is somewhat faster.

!           h and l were originally references to c(0) and c(1) as
!           per the equivalence, but I removed those out of clarity.

            do kk = 0, 3
               l = l * 2 + m(7 - kk, jj, h1)
            end do

            do kk = 4, 7
               h = h * 2 + m(7 - kk, jj, h1)
            end do

!           The document in verbatim referred to this as "controlled
!           interchange and s-box permutation, whatever that means.
            v = (s0(l) + 16*s1(h)) * (1 - k(jj, ks)) + (s0(h) + 16 * &
                    s1(l)) * k(jj, ks)

!           Here we convert v back into bit array format. We could have
!           used expand(), but this is faster.
            do kk = 0, 7
               tr(kk) = mod(v, 2)
               v = v / 2
            end do

!           Here we do key-interruption and diffusion, combined. The
!           "k + tr" term is the permuted key interruption.
!
!           mod(o(kk) + jj, 8) is the diffusion row for column kk.
!           row = byte and column = bit within byte
            do kk= 0, 7
               m(kk, mod(o(kk) + jj, 8), h0) = mod(k(pr(kk), kc) + &
                       tr(pr(kk)) + m(kk, mod(o(kk) + jj, 8), h0), 2)
            end do

            if (jj < 7 .or. d == 1) then
               kc = mod(kc + 1, 16)
            end if

         end do

!        As we established earlier, h0 and h1 are merely pointers to
!        the halves of the blocks we intend to work on. As we have
!        reached the end of the Feistel round, we have to swap h0 and
!        h1. We do so by using temp as a temporary buffer variable.
         temp = h0
         h0 = h1
         h1 = temp

      end do ! end of Feistel round

!     Here we physically swap the upper and lower halves of the message
!     after the last of the sixteen Feistel rounds completes.
      do jj = 0,7
         do kk = 0,7
            swap_temp(kk, jj) = m(kk, jj, 0)
            m(kk, jj, 0) = m(kk, jj, 1)
            m(kk, jj, 1) = swap_temp(kk, jj)
         end do
      end do

      return
      end subroutine lucifer

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine expand(a, b, l)
      implicit none

      integer, dimension(0:*), intent(out) :: a ! array in bit format
      integer, dimension(0:*), intent(in) :: b ! array in byte format
      integer, intent(in) :: l ! length of the array, b, in hexdigits

      integer :: i, j, v ! temporary variables

      do i = 0, l - 1
         v = b(i)
         do j = 0, 3
            a((3 - j) + i * 4) = mod(v, 2)
            v = v / 2
         end do
      end do

      return
      end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine compress(a,b,l)
      implicit none

      integer, dimension(0:*), intent(in) :: a   ! array in bit format
      integer, dimension(0:*), intent(out) :: b ! array in byte format
      integer, intent(in) :: l  ! length of the array, b, in hexdigits

      integer :: i, j, v

      do i=0,l-1,1
         v=0
         do j=0,3,1
            v = v * 2 + mod(a(j + i * 4), 2)
         end do
         b(i)=v
      end do

      return
      end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
