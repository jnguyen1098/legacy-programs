      program luc

! all integers starting with [a-z] are implicit
      implicit integer (a-z)

! I think this is a data initializer for an integer named handle
      data handle/0/

! I am pretty certain this is a data initializer for int arrays k and m
! such that k is a 2-dimensional from [0, 7] and [0, 15]; and that
! m is a 3-dimensional from [0, 7], [0, 7], and [0, 1]
      dimension k(0:7,0:15),m(0:7,0:7,0:1)

! key is an array of ints with index [0, 127]
! message is an array of ints with index [0, 127]
      dimension key(0:127), message(0:127)

! pointer aliasing...? use reshape()
      equivalence (k(0,0),key(1)),(m(0,0,0),message(1))

! kb and mb are int arrays from [0, 31]
      dimension kb(0:31),mb(0:31)

! debugging
      dimension kbo(0:31),mbo(0:31)

! we print every character in the array kb
! 1003 = ' key '
      write(*,1003)

! reads in the key as a series of ints into kb
! format(32z1.1)
!!      read(*,1004) (kb(i),i=0,31)

      kb = (/0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0/)
    
      mb = (/10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11/)


      kbo = (/0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0/)
    
      mbo = (/10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11/)

      write(*,1005)
!!      read(*,1006) (mb(i),i=0,31)


      call expand(message,mb,32)
      call expand(key,kb,32)

      write(*,1000) (key(i), i=0,127)
      write(*,1001) (message(i), i=0,127)

! 0 in the first lucifer argument means encipher
      d=0
      call lucifer(d,k,m)

! 1 in the first lucifer argument means decipher
      d=1
      call lucifer(d,k,m)

      write(*,1001) (message(i),i=0,127)

      call compress(message,mb,32)
      call compress(key,kb,32)
      write(*,1003)
      write(*,1007) (kb(i),i=0,31)
      write(*,1005)
      write(*,1007) (mb(i),i=0,31)
      
      if(all(kb.eq.kbo).and.all(mb.eq.mbo)) then
      write(*,*) "PASS"
      else
      write(*,*) "FAIL"
      endif

1000  format(' key '/16(1x,i1))
1001  format(' plain '/16(1x,i1))
1002  format(' cipher '/16(1x,i1))
1003  format(' key ')
1004  format(32z1.1)
1005  format(' plain ')
1006  format(32z1.1)
1007  format(1x,32z1.1)
      end


      subroutine lucifer(d,k,m)
      implicit integer(a-z)
      dimension m(0:7,0:7,0:1),k(0:7,0:15),o(0:7)

      dimension sw(0:7,0:7),pr(0:7),tr(0:7),c(0:1)
      dimension s0(0:15),s1(0:15)
      equivalence (c(0),h),(c(1),l)

!     diffusion pattern
      data o/7,6,2,1,5,0,3,4/

!     inverse of fixed permutation
      data pr/2,5,4,0,3,1,7,6/

!     S-box permutations
      data s0/12,15,7,10,14,13,11,0,2,6,3,1,9,4,5,8/
      data s1/7,2,14,9,3,11,0,4,12,13,1,10,6,15,8,5/

      h0=0
      h1=1

      kc=0
      if (d .eq. 1) kc=8

      do 100 ii=1,16,1

      if (d.eq.1) kc=mod(kc+1,16) 
      ks=kc

      do 200 jj=0,7,1
      l=0
      h=0
 
      do 400 kk=0,3,1
      l=l*2+m(7-kk,jj,h1)
400   continue
      do 410 kk=4,7,1
      h=h*2+m(7-kk,jj,h1)
410   continue

      v=(s0(l)+16*s1(h))*(1-k(jj,ks))+(s0(h)+16*s1(l))*k(jj,ks)

      do 500 kk=0,7,1
      tr(kk)=mod(v,2)
      v=v/2
500   continue

      do 300 kk=0,7,1
      m(kk,mod(o(kk)+jj,8),h0)=mod(k(pr(kk),kc)+tr(pr(kk))+m(kk,mod(o(kk)+jj,8),h0),2)
300   continue
      if (jj .lt. 7 .or. d .eq. 1) kc=mod(kc+1,16)
200   continue

      jjj=h0
      h0=h1
      h1=jjj
100   continue

      do 700 jj=0,7,1
      do 800 kk=0,7,1
      sw(kk,jj)=m(kk,jj,0)
      m(kk,jj,0)=m(kk,jj,1)
      m(kk,jj,1)=sw(kk,jj)
800   continue
700   continue

      return 
      end

      subroutine expand(a,b,l)
      implicit integer (a-z)
      dimension a(0:*),b(0:*)
      do 100 i=0,l-1,1
      v=b(i)
      do 200 j=0,3,1
      a((3-j)+i*4)=mod(v,2)
      v=v/2
200   continue
100   continue
      return
      end

      subroutine compress(a,b,l)
      implicit integer (a-z)
      dimension a(0:*),b(0:*)
      do 100 i=0,l-1,1
      v=0
      do 200 j=0,3,1
      v=v*2+mod(a(j+i*4),2)
200   continue
      b(i)=v
100   continue
      return
      end

