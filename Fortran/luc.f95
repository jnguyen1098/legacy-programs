#define AUTOPILOT 1
      program luc
      implicit none

      integer :: i

      integer, dimension (0:7,0:15) :: k
      integer, dimension (0:7,0:7,0:1) :: m

      integer, dimension (0:127) :: key, message

      integer, dimension (0:31) :: kb, mb

      integer, dimension (0:31) :: kbo, mbo

      write(*,*) ' key '

#if AUTOPILOT == 0
      read(*,1004) (kb(i),i=0,31)
#else
      kb = (/0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0/)
    
      mb = (/10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11/)


      kbo = (/0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0/)
    
      mbo = (/10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11/)
#endif

      write(*,*) ' plain '

#if AUTOPILOT == 0
      read(*,1006) (mb(i),i=0,31)
#endif

      call expand(message,mb,32)
      call expand(key,kb,32)

      write(*, 1000) (key(i), i = 0,127)
1000 format(' key '/16(1x, i1))

      write(*, 1001) (message(i), i = 0, 127)
1001 format(' plain '/16(1x, i1))

      k = reshape(key, (/8, 16/))
      m = reshape(key, (/8, 8, 2/))

      call lucifer(0, k, m)

      call lucifer(1, k, m)

      write(*, 1001) (message(i), i = 0, 127)

      call compress(message, mb, 32)
      call compress(key, kb, 32)

      write(*,*) ' key '
      write(*, 1007) (kb(i), i = 0, 31)

      write(*,*) ' plain '
      write(*, 1007) (mb(i), i = 0, 31)

1007 format(1x, 32z1.1)
      
#if AUTOPILOT == 1
      if(all(kb == kbo).and.all(mb == mbo)) then
         print *, "PASS"
      else
         print *, "FAIL"
      endif
#else
      print *, "PASS"
#endif

1002  format(' cipher '/16(1x,i1))
1004  format(32z1.1)
1006  format(32z1.1)
      end


      subroutine lucifer(d,k,m)
      implicit none

!     arguments
      integer, intent(in) :: d
      integer, dimension (0:7, 0:15), intent(in) :: k
      integer, dimension (0:7, 0:7, 0:1), intent(inout) :: m

!     diffusion pattern
      integer, dimension (0:7) :: o = (/7, 6, 2, 1, 5, 0, 3, 4/)

!     inverse of fixed permutation
      integer, dimension (0:7) :: tr, pr = (/2, 5, 4, 0, 3, 1, 7, 6/)

!     S-box permutations
      integer, dimension (0:15) :: s0 = (/12, 15, 7, 10, 14, 13, 11, &
              0, 2, 6, 3, 1, 9, 4, 5, 8/)
      integer, dimension (0:15) :: s1 = (/7, 2, 14, 9, 3, 11, 0, 4, &
              12, 13, 1, 10, 6, 15, 8, 5/)

!     other variables
      integer :: h0, h1, h, ii, jj, jjj, kc, kk, ks, l, v
      integer, dimension (0:7,0:7) :: sw

      h0 = 0
      h1 = 1

      kc = 0
      if (d == 1) kc = 8

      do ii = 1, 16
         if (d == 1) kc = mod(kc + 1, 16)
         ks = kc

         do jj = 0, 7
            l = 0
            h = 0

            do kk = 0, 3
               l = l * 2 + m(7 - kk, jj, h1)
            end do

            do kk = 4, 7
               h = h * 2 + m(7 - kk, jj, h1)
            end do

            v = (s0(l) + 16*s1(h)) * (1 - k(jj, ks)) + (s0(h) + 16 * &
                    s1(l)) * k(jj, ks)

            do kk = 0, 7
               tr(kk) = mod(v, 2)
               v = v / 2
            end do

            do kk=0, 7
               m(kk, mod(o(kk) + jj, 8), h0) = mod(k(pr(kk), kc) + &
                       tr(pr(kk)) + m(kk, mod(o(kk) + jj, 8), h0), 2)
            end do

            if (jj < 7 .or. d == 1) kc = mod(kc + 1, 16)

         end do

         jjj = h0
         h0 = h1
         h1 = jjj
      end do

      do jj=0,7
         do kk=0,7
            sw(kk,jj)=m(kk,jj,0)
            m(kk,jj,0)=m(kk,jj,1)
            m(kk,jj,1)=sw(kk,jj)
         end do
      end do

      return 
      end subroutine lucifer

      subroutine expand(a,b,l)
      implicit none

      integer, dimension(0:*), intent(out) :: a
      integer, dimension(0:*), intent(in) :: b
      integer, intent(in) :: l

      integer :: i, j, v

      do i = 0, l - 1
         v = b(i)
         do j = 0, 3
            a((3 - j) + i * 4) = mod(v, 2)
            v = v / 2
         end do
      end do

      return
      end

      subroutine compress(a,b,l)
      implicit none

      integer, dimension(0:*), intent(in) :: a
      integer, dimension(0:*), intent(out) :: b
      integer, intent(in) :: l

      integer :: j, v

      v = 0

      do j = 0, 3
         v = v * 2 + mod(a(j + (l - 1) * 4), 2)
      end do

      b(l - 1) = v
      return
      end subroutine compress
