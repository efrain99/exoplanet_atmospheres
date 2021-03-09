use direction2
use projection2
use consts
!integer, parameter :: m=10
real,parameter:: radius_end=15.0
integer :: i
double precision :: unit_vec_n(3), x_vec(3)
m=2**20
pi=2.0*asin(1.0)
open(18, file='limb_dark.txt') !file for b1 and b2
do i=1,m
  !unit_vec_n=0.0
  !x_vec=0.0
  call sampling_direction(radius_end,unit_vec_n,x_vec)
end do
close(18)
print*,"done"
end program
