real, parameter :: radius_end=40.0
integer, parameter :: m=2**20, bin_num=32
real :: bin_path_length(bin_num), dist_path(bin_num),path_c(bin_num),path_e(bin_num+1)
real :: x,y,z,r1,r2,r3,radius,theta,phi,tau,x_step,y_step,z_step
real:: pi, path, minvalue, maxvalue, start, finish
real :: array(m)
integer :: l

call cpu_time(start)

x=0.0
y=0.0
z=0.0
theta=0.0
phi=0.0
pi=2.0*asin(1.0)

do l=1, m
  radius=0.0
  x=0.0
  y=0.0
  z=0.0
  path=0.0
  do while (radius <= radius_end)

    call random_number(r1)
    call random_number(r2)
    call random_number(r3)
    theta= ACOS(1.0-2.0*r1)
    phi=2.0*pi*r2
    tau=-log(1.0-r3)
    path=path+tau
    x_step=tau*sin(theta)*cos(phi)
    x=x+x_step
    y_step=tau*sin(theta)*sin(phi)
    y=y+ y_step
    z_step=tau*cos(theta)
    z=z+ z_step

    radius=sqrt(x**2.0+y**2.0+z**2.0)

  end do
    array(l)=path
end do

!print*,"max value","    min value"
!print*
!print*,maxval(array),minval(array)
!print *
!do i=1,m
  !print(9),array(l)
!end do

minvalue=minval(array)
maxvalue=maxval(array)
bin_width= (maxvalue-minvalue)/bin_num

do i=1,bin_num+1
  path_e(i)= minvalue+bin_width*(i-1.0)
  !print*,"edges=",path_e(i)
end do

do i=1,bin_num
  path_c(i)=minvalue+bin_width*(i-.5)
end do


bin_path_length=0.0

do l=1,m
  do i=1,bin_num
    if (array(l) >= path_e(i) .and. array(l) < path_e(i+1)) then
      bin_path_length(i)=bin_path_length(i)+1.0
    endif
  end do
end do


dist_path=bin_path_length/(bin_width*m)

print *
print *,"normalization of binned path length=", sum(bin_path_length)*bin_width
print *
open(7,file="path_graph.txt")
do i=1,bin_num
  write(7,*) dist_path(i), path_c(i)
end do
close(7)
call cpu_time(finish)
print*, '("Time = ",f6.3," seconds.")',finish-start
end program
