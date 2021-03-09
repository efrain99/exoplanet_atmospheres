module compute_n  !module to compute integral for any p(n) value
implicit NONE

real::n_i,sum,eq,null,error
integer :: i
contains
  subroutine compute_int(p,value)
  implicit NONE

    integer, INTENT(INOUT) :: p
    real, INTENT(OUT) :: value

    sum=0
    do i=1, p
      n_i=(2.0*i-1.0)/(2.0*p)
      eq=n_i*10.0
      null=(1.0/EXP(eq))
      sum=sum+null
    end do

    value =(10.0/p)*sum
    error = ABS(value-.99995)

  end subroutine compute_int
end module compute_n
