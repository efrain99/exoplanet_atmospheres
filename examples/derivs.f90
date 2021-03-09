module der
  use constants
  implicit none
  contains
    subroutine derivs(x,y,dydx)
      implicit none

      real, INTENT(IN) :: x
      real, INTENT(IN) :: y(:)
      real, INTENT(OUT) :: dydx(:)

      dydx(1)=y(2)
      dydx(2)= -omega0**2 * y(1)

    end subroutine derivs
end module der
