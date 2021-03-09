

real, parameter :: radius_end=15.0
integer, parameter :: n=32, m=2**20
real, allocatable :: tau_c(:),tau_e(:), bin_tau(:), dist_tau(:)
real :: x,y,z,r1,r2,r3,radius,theta,phi,tau,x_step,y_step,z_step
real:: pi,dphi,dtheta
real :: bin_p(n),dist_p(n), bin_t(n),dist_t(n), thetae(n+1),phie(n+1),phic(n),thetac(n)
integer :: l


x=0.0
y=0.0
z=0.0
theta=0.0
phi=0.0
bin_p=0.0
bin_t=0.0
pi=2.0*asin(1.0)
dphi= (2.0*pi)/n
dtheta= pi/n


do j=1,n+1			! n+1 edges
  phie(j)=dphi*(j-1.0)
end do
do j=1,n			! n centers
  phic(j)=dphi*(j-0.5)
end do

do i=1, n+1 !edges
  thetae(i)= dtheta*(i-1.0)
end do

do i=1, n !middle
    thetac(i)= dtheta*(i-0.5)
end do

do l=1, m
  radius=0.0
  x=0.0
  y=0.0
  z=0.0
  do while (radius <= radius_end)

    call random_number(r1)
    call random_number(r2)
    call random_number(r3)
    theta= ACOS(1.0-2.0*r1)
    phi=2.0*pi*r2
    tau=-log(1.0-r3)
    x_step=tau*sin(theta)*cos(phi)
    x=x+x_step
    y_step=tau*sin(theta)*sin(phi)
    y=y+ y_step
    z_step=tau*cos(theta)
    z=z+ z_step

    radius=sqrt(x**2.0+y**2.0+z**2.0)

  end do
    i= 1 + int(theta/dtheta)
    j =1 + int(phi/dphi)
    if (i<1 .or. i>n) then
      print *,"i out of bounds. i=",i
      print *,"theta,theta/dtheta=",theta,theta/dtheta
      stop
    endif
    bin_t(i)=bin_t(i)+1.0
    if (j<1 .or. j>n) then			! just include this if statement to be careful and check
      print *,"j out of bounds. j=", j
      print *,"phi,phi/dphi=",phi,phi/dphi
      stop
    endif
    bin_p(j)=bin_p(j)+1.0

end do

dist_t=bin_t/(dtheta*m)
dist_p= bin_p/(dphi*m)

print *
print *,"normalization of binned path length=",sum(bin_tau)/m
print *


print *
print *,"normalization of binned theta=", sum(bin_t)/(m)
print *,"normalization of binned phi=", sum(bin_p)/(m)

print *,"distribution theta","        thetac","      dist phi","      phic"
open(3, file='graphing.txt')
do i=1,n
  write(3,*) dist_t(i), thetac(i), dist_p(i), phic(i)
end do
close(3)

end program
