module step
  use der
  use constants
  implicit none
    contains
      subroutine rk4step(x,y,dx)
        implicit none
        real, INTENT(INOUT) :: x
        real, INTENT(IN) :: dx
        real, INTENT(INOUT) :: y(:)
        real, dimension(size(y)) :: dydx,k1, k2, k3, k4

        call derivs(x,y,dydx)
        k1=dx*dydx
        call derivs(x+0.5*dx,y+0.5*k1,dydx)
        k2=dx*dydx
        call derivs(x+0.5*dx,y+0.5*k2,dydx)
        k3=dx*dydx
        call derivs(x+dx,y+k3,dydx)
        k4=dx*dydx
        y=y+k1/6.0 + k2/3.0 + k3/3.0 + k4/6.0
        x=x+dx
      end subroutine rk4step
end module step
