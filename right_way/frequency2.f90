program frequency
  implicit none

    double precision :: delta,nu_s,sig_s,tau_k,a,x,sigma,nu
    real :: pi,rando1
    pi=2.0*asin(1.0)

    a=0.0004699964727173962
    delta=106075872517.26816
    tau_k=1.0e8
    sig_s=0.0
    call random_number(rando1)

    sigma= (2.0*tau_k/sqrt(pi))*((1/tanh(2.0*rando1-1.0))+sig_s)  !solved for sigma
    x=(sqrt(3.0/2.0)*(3.0*a*sigma)/pi)**1./3.
    nu= delta*x+nu_s
    print*,"sigma           ","              x","                   nu"
    print*, sigma,x,nu
    ! wasnt sure how to bin them due to not knowing the
    !probablily distrubtion integration limits since some constants change


end program frequency
