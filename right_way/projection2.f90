module projection2
  use consts
  implicit none
  contains
    subroutine project(x_vec,unit_vec_n,b1,b2)
      double precision, intent (in) :: x_vec(3), unit_vec_n(3)
      double precision, intent (out) :: b1,b2
      double precision :: ndotx,b(3),vec1(3),e1(3),vec2(3),e2(3)
      ndotx=dot_product(unit_vec_n,x_vec)
      b=x_vec-ndotx*ndotx
      vec1=(/0.d0,-unit_vec_n(3),unit_vec_n(2)/)    ! \vec{n} \cross \vec{e}_x
      e1=vec1/sqrt(sum(vec1**2))
      vec2=(/unit_vec_n(2)*e1(3)-unit_vec_n(3)*e1(2),unit_vec_n(3)*e1(1)-unit_vec_n(1)*e1(3),unit_vec_n(1)*e1(2)- &
      unit_vec_n(2)*e1(1)/)   ! \vec{n} \cross \vec{e}_1
      e2=vec2/sqrt(sum(vec2**2))
      b1=dot_product(b,e1)
      b2=dot_product(b,e2)


    end subroutine project
end module projection2
