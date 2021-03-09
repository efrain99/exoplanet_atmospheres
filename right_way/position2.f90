module position
  use inside_sphere
  use direction
  use consts
  implicit none
  contains
    subroutine vector_pos(radius_end)
      !implicit none

      real, intent(in) :: radius_end
      !real, INTENT(OUT) :: dist_t(i), thetac(i), dist_p(i), phic(i)
      !double precision, intent(in) :: x_vec(3)
      !double precision, intent(out) ::unit_vec_n(3), e_r(3), e_theta(3), e_phi(3), n_s(3)
      integer, parameter :: n=32
      real:: dphi,dtheta,theta,phi
      real :: bin_p(n),dist_p(n), bin_t(n),dist_t(n), thetae(n+1),phie(n+1),phic(n),thetac(n)
      double precision :: unit_vec_n(3), x_vec(3)
      integer :: l,i,j
      !double precision :: x_vec(3)

      !unit_vec_n=0.0
      !e_r=0.0
      !e_theta=0.0
      !e_phi=0.0
      !n_s= 0.0
      theta=0.0
      phi=0.0
      bin_p=0.0
      bin_t=0.0
      !pi=2.0*asin(1.0)
      dphi= (2.0*pi)/n
      dtheta= pi/n
      !print*,"dtheta=",dtheta
      !print*,"dphi=",dphi

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
      open(16, file='projecting.txt')
      do l=1, m !remove the do loop and just make same loop in main_call so that everything is done once
        !x=0.0
        !y=0.0
        !z=0.0
        !radius=0.0
        call sampling_inside(radius_end,theta,phi)
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
        call sampling_direction(radius_end,unit_vec_n,x_vec)
        !call project(x_vec,unit_vec_n,b1,b2)
        !open(16, file='projecting.txt')
        !write(16,*) b1,b2

      end do
      close(16)

      dist_t=bin_t/(dtheta*m)
      dist_p= bin_p/(dphi*m)
      open(17,file='norm_out.txt')
      !print *
      write(17,*) "normalization of binned theta=", sum(bin_t)/(m)
      write(17,*) "normalization of binned phi=", sum(bin_p)/(m)

      write(17,*) "distribution theta","        thetac","      dist phi","      phic"
      close(17)
      open(15, file='binned_dist.txt')
      do i=1,n
        write(15,*) dist_t(i), thetac(i), dist_p(i), phic(i)
      end do
      close(15)

    end subroutine vector_pos
end module position
