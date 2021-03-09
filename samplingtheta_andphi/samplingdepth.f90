
integer,parameter :: m=10,n=32
real :: tauc(n),taue(n+1),bin_tau(n),dist_tau(n)
real :: dtau,r,analytic


dtau=15.0/n

do i=1,n+1			! n+1 edges
  taue(i)=dtau*(i-1.0)
  print*,"value of bin(i)=",taue(i)
end do
do i=1,n			! n centers
  tauc(i)=dtau*(i-0.5)
end do


bin_tau=0.0				! store number in bin i in bin(i)
do j=1,m
  call random_number(r)
  tau=-log(1-r)
  i= 1 + int(tau/dtau)		! fast way
  if (i<1 .or. i>n) then			! just include this if statement to be careful and check
    print *,"i out of bounds. i=",i
    print *,"phi,phi/dphi=",tau,tau/dtau
    stop
  endif
  bin_tau(i)=bin_tau(i)+1.0
end do
dist_tau=bin_tau/(m*dtau)				! turn number in each bin into a distribution P(\phi).


print *
print *,"normalization of binned data=",sum(bin_tau)/m
print *
analytic=(1.0-exp(-15.0))
write (*,"(4(2x,a15))") "tauc","distribution","analytic","error"
do i=1,n
  write (*,"(4(2x,es15.8))") tauc(i),dist_tau(i),analytic,(dist_tau(i)-analytic)/analytic
end do

end program
