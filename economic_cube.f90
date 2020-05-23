! This program shrinks the size of usually huge Gaussian-cube format files
! The grid of the reported field becomes courses by factors s1, s2, s3
! Copyright Ali Sadeghi (ali_sadeghi@sbu.ac.ir)
implicit none
character*80  header,arg, fname
integer n1,n2,n3, Natom
integer i1,i2,i3, k
real    vec(3)
integer s1,s2,s3
real*8, allocatable :: rho(:,:,:)

!s1=4 ; s2=4 ; s3=4  ! scaling factors 
!read(*,*) fname

call get_command_argument(1,arg)
fname=arg

call get_command_argument(2,arg)
read(arg,*),s1
call get_command_argument(3,arg)
read(arg,*),s2
call get_command_argument(4,arg)
read(arg,*),s3


open(10, file=trim(fname),status = 'old')
open(20, file="economic_"//trim(fname))

read(10,'(a80)') header    ; write (20,*) trim(header)
read(10,'(a80)') header    ; write (20,*) trim(header)

read(10,'(a80)') header    ; write (20,*) trim(header)
read(header,*) Natom

read(10,'(a80)') header
read(header,*) N1, vec     ; write (20,*) N1/s1, vec*s1

read(10,'(a80)') header
read(header,*) N2, vec     ; write (20,*) N2/s2, vec*s2

read(10,'(a80)') header
read(header,*) N3, vec     ; write (20,*) N3/s3, vec*s3

do k=1,Natom
    read(10,'(a80)') header    ; write (20,*) trim(header)
enddo

print '(" reading ",i3,"x",i3,"x",i3," data points from ",a)', n1,n2,n3   &
        ,trim(fname)//" ..."
allocate (rho(n3,n2,n1))
read(10,*) rho
close(10)

print '(" writing ",i3,"x",i3,"x",i3," data points into ",a)', n1/s1,n2/s2,n3/s3 &
       , "economic_"//trim(fname)//" ..."
Do 10 I1 = 1, N1, s1
Do 10 I2 = 1, N2, s2
   write(20,'(6E13.5)') (rho(I3,I2,I1),I3=1,N3,s3)
10 Continue
end
