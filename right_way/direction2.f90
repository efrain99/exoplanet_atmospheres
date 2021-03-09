module direction2
  use projection2
  use consts
  implicit none
contains
  subroutine sampling_direction(radius_end,unit_vec_n,x_vec)

    double precision, intent(out) :: unit_vec_n(3) !unir vector n
    double precision, intent(out) :: x_vec(3) !x vector
    double precision,dimension(3) :: e_r ,e_theta,e_phi !basis vectors
    real, intent(in) :: radius_end
    double precision :: b1, b2
    real :: c,root,theta_cap,phi_cap,rand1,rand2,ans1,ans2,rand3,rand4,theta,phi

      call random_number(rand1)
      call random_number(rand2)
      call random_number(rand3)
      call random_number(rand4)

      phi_cap=2.0*pi*rand2
      theta=1.0/cos(1.0-2.0*rand3)
      phi=2.0*pi*rand4

      c=1.0+(sqrt(3.0)/2.0)-rand1-(sqrt(3.0)/2.0)*rand1 ! solving quadtatic eqn

      ans1=(1.0+sqrt(1.0-4.0*(-sqrt(3.0)/2.0)*c))/(-sqrt(3.0))

      ans2=(1.0-sqrt(1.0-4.0*(-sqrt(3.0)/2.0)*c))/(-sqrt(3.0))

      if(ans1>0.0 .and. ans2>0.0) then  !choosing root that cos(theta)>0
        print*,"error roots cant both be postive" ! u just chose ans1(2)=cos(theta)
        print*,ans1,ans2
        stop
      else if(ans1>0.0) then
          root=ans1
      else if (ans2>0.0) then
          root=ans2
      end if
      theta_cap=acos(root) !then i take acos of the postive root

      e_r =(/sin(theta)*cos(phi),sin(theta)*sin(phi),cos(theta)/)

      e_theta=(/cos(theta)*cos(phi),cos(theta)*sin(phi),(-sin(theta))/)

      e_phi=(/(-sin(phi)),cos(phi),0.0/)

      x_vec=e_r*radius_end

      unit_vec_n=e_r*cos(theta_cap)+e_theta*sin(theta_cap)*cos(phi_cap)+e_phi*(sin(theta_cap)*sin(phi_cap))

      call project(x_vec,unit_vec_n,b1,b2)

      write(18,*) b1, b2

  end subroutine sampling_direction

end module direction2
