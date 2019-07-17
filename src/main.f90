program test
implicit none
    integer, parameter:: nx =31 , ny=31
    real(8) :: x1, xL     ! bounds of the domain
    real(8) :: y1, yL     ! bounds of the domain
    real(8) :: A(nx*ny,nx*ny) ! coefficient matrix
    real(8) :: phi0(nx*ny)   ! U0 is U(x,t=0) initial condition ! it is going to build the right hand side of the equation Ax=b
    real(8) :: phi(nx*ny)    ! U is U(x,t) , it is going to be the solution of Ax=b, I mean x, so we fill it with our first guess
    real(8) :: D(nx*ny)      ! D is the diffusion coefficient which is a function of location D=D(r)=D(x,y)
    real(8) :: dt,D0         ! D0 is the constant prt of the diffusion coefficient
    integer :: max_iter  ! maximum number of iteration 
    real(8) :: max_tol   ! maximum tolerance for the solution
    integer :: iter      ! the last iteration number when the tolerance criterion satisfied
    real(8) :: tol       ! the last tolerance of the solution
    integer :: flag      ! for error handeling, if flag==0 the subroutine executed without any problem
    character(LEN=*), parameter :: filebase = "phi_"
    character(LEN=*), parameter :: fileend = ".txt"
    CHARACTER(LEN=20) :: filename
    integer :: k

    D0=1.0
    dt=0.1
    x1=0.0
    xL=10.0
    y1=0.0
    yL=10.0
    max_iter = 100000
    max_tol = 1.0d-8
    iter=0
    call assemble_system_dirichlet(x1, xL, nx, y1, yL, ny, D0, D, dt, A, phi0, phi)
    k=0
    write(filename,'(A,I3.3,A)') filebase,k,fileend
    call write_phi_to_file(x1, xL, nx, y1, yL, ny, phi0,filename)

    do k=1,50
        call conjGrad(nx*ny, A , phi0, max_iter, max_tol, phi, iter, tol, flag)
        write(filename,'(A,I3.3,A)') filebase,k,fileend
        call write_phi_to_file(x1, xL, nx, y1, yL, ny, phi,filename)
        phi0=phi
        phi=0
    end do

    
    
end program test
