! practice in monte carlo sampling and binning for the uniform distribution of phi between (0,2\pi).
! m sampled values (here 2**20 ~ 10^6)
! n bins (here 32)
! mean number expected per bin is 2^15. poisson error is 1/sqrt(2^15) ~0.01 = 1%.

integer, parameter :: m=2**20,n=32
real :: phic(n),phie(n+1),bin(n),dist(n)
real :: dphi,pi,r,analytic

pi=2.0*asin(1.0)
dphi= (2.0*pi)/n

do i=1,n+1			! n+1 edges
  phie(i)=dphi*(i-1.0)
end do
do i=1,n			! n centers
  phic(i)=dphi*(i-0.5)
end do

! fast method
bin=0.0				! store number in bin i in bin(i)
do j=1,m
  call random_number(r)
  phi=2.0*pi*r
  i= 1 + int(phi/dphi)		! fast way
  if (i<1 .or. i>n) then			! just include this if statement to be careful and check
    print *,"i out of bounds. i=",i
    print *,"phi,phi/dphi=",phi,phi/dphi
    stop
  endif
  bin(i)=bin(i)+1.0
end do
dist=bin/(m*dphi)				! turn number in each bin into a distribution P(\phi).

print *
print *,"normalization of binned data=",sum(bin)/m
print *
open(1, file='phi_output.txt')
analytic=1.0/(2.0*pi)
write(*,"(4(2x,a15))") "phic","distribution","analytic","error"
do i=1,n
  write(1,"(4(2x,es15.8))") phic(i),dist(i),analytic,(dist(i)-analytic)/analytic
end do
close(1)

! slow method
bin=0.0
do j=1,m
  call random_number(r)
  phi=2.0*pi*r
  do i=1,n						! slow method is to loop over all bins
    if (phi >= phie(i) .and. phi < phie(i+1)) then	! and check if between left and right sides
      bin(i)=bin(i)+1.0
    endif
  end do
end do
dist=bin/(m*dphi)

print *
print *,"normalization of binned data=",sum(bin)/m
print *

write(*,"(4(2x,a15))") "phic","distribution","analytic","error"
do i=1,n
  write(*,"(4(2x,es15.8))") phic(i),dist(i),analytic,(dist(i)-analytic)/analytic
end do


end program
