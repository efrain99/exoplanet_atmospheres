

  integer, parameter:: m=2**20, n=32
  real :: thetac(n), thetae(n+1), bin(n), dist(n), analytic(n)
  real:: r, theta, dtheta, pi

  pi=2.0*asin(1.0)
  dtheta= pi/n

  do i=1, n+1 !edges
    thetae(i)= dtheta*(i-1.0)
  end do

  do i=1, n !middle
      thetac(i)= dtheta*(i-0.5)
      analytic(i)= sin(thetac(i))
  end do

  bin=0.0
  do j=1, m
    call random_number(r)
    theta= ACOS(1.0-2.0*r)
    i= 1 + int(theta/dtheta)
    if (i<1 .or. i>n) then
      print *,"i out of bounds. i=",i
      print *,"theta,theta/dtheta=",theta,theta/dtheta
      stop
    endif
    bin(i)=bin(i)+1.0
  end do
  dist=bin/(m*dtheta)

  print *
  print *,"normalization of binned data=",sum(bin)/m
  print *

  write(*,"(4(2x,a15))") "thetac","distribution","analytic","error"
  !open(2,file='theta_output.txt')
  do i=1,n
    write(*,"(4(2x,es15.8))") thetac(i),dist(i), analytic(i), (dist(i)-analytic(i))/analytic(i)
  end do
  !close(2)


end program
