      program luc
      implicit none

! variables
      integer :: i

! I am pretty certain this is a data initializer for int arrays k and m
! such that k is a 2-dimensional from [0, 7] and [0, 15]; and that
! m is a 3-dimensional from [0, 7], [0, 7], and [0, 1]
      integer, dimension (0:7,0:15) :: k
      integer, dimension (0:7,0:7,0:1) :: m

! key is an array of ints with index [0, 127]
! message is an array of ints with index [0, 127]
      integer, dimension (0:127) :: key, message

! pointer aliasing...? use reshape()
      equivalence (k(0,0),key(1)),(m(0,0,0),message(1))

! kb and mb are int arrays from [0, 31]
      integer, dimension (0:31) :: kb, mb

! debugging
      integer, dimension (0:31) :: kbo, mbo

! we print every character in the array kb
! 1003 = ' key '
      write(*,*) ' key '

! reads in the key as a series of ints into kb
! format(32z1.1)
!!      read(*,1004) (kb(i),i=0,31)

      kb = (/0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0/)
    
      mb = (/10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11/)


      kbo = (/0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0/)
    
      mbo = (/10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11/)

      write(*,*) ' plain '
!!      read(*,1006) (mb(i),i=0,31)


      call expand(message,mb,32)
      call expand(key,kb,32)

      write(*,1000) (key(i), i=0,127)
      1000 format(' key '/16(1x,i1))

      write(*,1001) (message(i), i=0,127)
      1001 format(' plain '/16(1x,i1))

! 0 in the first lucifer argument means encipher
      call lucifer(0, k, m)

! 1 in the first lucifer argument means decipher
      call lucifer(1, k, m)

      write(*,1001) (message(i),i=0,127)

      call compress(message,mb,32)
      call compress(key,kb,32)

      write(*,*) ' key '
      write(*,1007) (kb(i),i=0,31)

      write(*,*) ' plain '
      write(*,1007) (mb(i),i=0,31)

      1007 format(1x, 32z1.1)
      
      if(all(kb.eq.kbo).and.all(mb.eq.mbo)) then
      write(*,*) "PASS"
      else
      write(*,*) "FAIL"
      endif

1002  format(' cipher '/16(1x,i1))
1004  format(32z1.1)
1006  format(32z1.1)
      end


      subroutine lucifer(d,k,m)
      implicit none

      integer, intent(in) :: d

      integer, dimension (0:7, 0:7, 0:1), intent(inout) :: m
      integer, dimension (0:7, 0:15), intent(in) :: k
      integer, dimension (0:7) :: o

      integer, dimension (0:7,0:7) :: sw
      integer, dimension (0:7) :: pr, tr
      integer, dimension (0:1) :: c

      integer, dimension (0:15) :: s0, s1

      integer :: h0, h1, h, ii, jj, jjj, kc, kk, ks, l, v

      equivalence (c(0),h),(c(1),l)

!     diffusion pattern
      o = (/7, 6, 2, 1, 5, 0, 3, 4/)

!     inverse of fixed permutation
      pr = (/2, 5, 4, 0, 3, 1, 7, 6/)

!     S-box permutations
      s0 = (/12, 15, 7, 10, 14, 13, 11, 0, 2, 6, 3, 1, 9, 4, 5, 8/)
      s1 = (/7, 2, 14, 9, 3, 11, 0, 4, 12, 13, 1, 10, 6, 15, 8, 5/)

      h0 = 0
      h1 = 1

      kc = 0
      if (d .eq. 1) kc = 8

      do ii=1,16,1
      if (d.eq.1) kc=mod(kc+1,16) 
      ks=kc

      do jj=0,7,1
      l=0
      h=0
 
      do kk=0,3,1
        l = l * 2 + m(7 - kk, jj, h1)
      end do

      do kk=4,7,1
        h=h*2+m(7-kk,jj,h1)
      end do

      v = (s0(l) + 16*s1(h)) * (1 - k(jj, ks)) + (s0(h) + 16 * s1(l)) * k(jj, ks)

      do kk=0,7,1
      tr(kk)=mod(v,2)
      v=v/2
      end do

      do kk=0,7,1
      m(kk, mod(o(kk) + jj, 8), h0) = mod(k(pr(kk), kc) + tr(pr(kk)) + m(kk, mod(o(kk) + jj, 8), h0), 2)
      end do

      if (jj .lt. 7 .or. d .eq. 1) kc=mod(kc+1,16)
      end do

      jjj=h0
      h0=h1
      h1=jjj
      end do

      do jj=0,7,1
        do kk=0,7,1
          sw(kk,jj)=m(kk,jj,0)
          m(kk,jj,0)=m(kk,jj,1)
          m(kk,jj,1)=sw(kk,jj)
        end do
      end do

      return 
      end

      subroutine expand(a,b,l)
      implicit none
      integer, dimension(0:*), intent(out) :: a
      integer, dimension(0:*), intent(in) :: b

      integer :: i, j, l, v

      do i=0,l-1,1
        v=b(i)
        do j=0,3,1
          a((3-j)+i*4)=mod(v,2)
          v=v/2
        end do
      end do

      return
      end

      subroutine compress(a,b,l)
      implicit none
      integer, dimension(0:*), intent(in) :: a
      integer, dimension(0:*), intent(out) :: b

      integer :: i, j, l, v

      do i=0,l-1,1
        v = 0
      end do

      do j=0,3,1
        v = v * 2 + mod(a(j + i*4), 2)
      end do

      b(i)=v
      return
      end subroutine compress
