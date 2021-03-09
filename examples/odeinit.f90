program odeinit
  use step
  use constants
  implicit none
  !allocate dimensions of the arrays
  real :: x
  real ::dx
  real, allocatable :: y(:)
  integer :: xmax
  n=2
  allocate (y(n))
  omega0=10.0

  xmax=10.0
  y(1)=1.0
  y(2)=0.0
  dx=0.1
  x=0.0
  open (2,file ="data.txt")
  do
    call rk4step(x,y,dx)
    write(2,*) x, y(1), cos(omega0*x)
      if (x>xmax) exit
  end do
  close(2)

end program odeinit
