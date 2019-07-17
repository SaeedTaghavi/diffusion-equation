
!---------------------------------------------------------------------!
! it is working for 2D heat equation 
! Assemble  system for U_t= D*U_xx + D*U_yy 
! it converts to A * U_n+1 = U_n  -> Ax=b
! x in [x1,xL]
! y in [y1,yL]
! for nx as the number of points in the x direction
! for ny as the number of points in the y direction
! U(x,y,t=0) = 0 except for (nx/2,ny/2) that U(x=nx/2,y=ny/2,t=0)=100.0
!---------------------------------------------------------------------!
   subroutine assemble_system_dirichlet(x1, xL, nx, y1, yL, ny, D0, D, dt, A, U0, U)
    implicit none
    real(8) :: x1, xL     ! bounds of the domain
    real(8) :: y1, yL     ! bounds of the domain
    integer :: nx       ! number of grid points in x direction
    integer :: ny       ! number of grid points in x direction
    real(8) :: A(nx*ny,nx*ny) ! coefficient matrix
    real(8) :: U0(nx*ny)   ! U0 is U(x,t=0) initial condition ! it is going to build the right hand side of the equation Ax=b
    real(8) :: U(nx*ny)    ! U is U(x,t) , it is going to be the solution of Ax=b, I mean x, so we fill it with our first guess
    real(8) :: D(nx*ny)      ! D is the diffusion coefficient which is a function of location D=D(r)=D(x,y)
    real(8) :: g(nx*ny)      ! D is the diffusion coefficient which is a function of location D=D(r)=D(x,y)

    
    real(8), parameter :: PI = 3.141592653589793d0
    real(8) :: dx, dy, dt, D0, Sx, Sy
    integer :: i, j, k, ix, jy

    g=0.d0
    do i=1,nx*ny/5
        call random_integer(2,nx-1,ix)
        call random_integer(2,ny-1,jy)
        k=(jy-1)*nx+ix
        g(k)=1.0d0
    end do
    ! print*,count(g==1.0)
    g=-D0*g
    D=D0
    D=D+g
    call write_phi_to_file(x1, xL, nx, y1, yL, ny, D,'D.txt')
    ! print*,D
    ! print*,count(D-.001<0.0)
    ! pause
    dx= (xL-x1)/real(nx-1)
    dy= (yL-y1)/real(ny-1)
    Sx=dt/(dx*dx)
    Sy=dt/(dy*dy)
    
    ! write the coordinates of grid points (1-Dimentionanl problem)to file
    open(92,file='x.dat')
    do i=1,nx
       write(92,*)((i-1)*dx)+x1
    end do
    close(92)
    
    open(92,file='y.dat')
    do i=1,ny
       write(92,*)((i-1)*dy)+y1
    !    print*,((i-1)*dx)+x1
    end do
    close(92)
    
    A=0.d0
    !creating the A matrix
    do i=1,nx
        k=i             ! bottom boarder
        A(k,k)= 1.0      
        k=(ny-1)*nx+i   ! top boarder
        A(k,k)=1.0
    end do
    
    do j=1,ny
        k=(j-1)*nx+1    !left boarder
        A(k,k)=1.0
        k= (j-1)*nx+nx  ! right boarder
        A(k,k)=1.0
    end do

    do i=2,nx-1
        do j=2,ny-1
            k=(j-1)*nx+i
            ! A(k,k)=1.0+2.0*Sx+2.0*Sy  ! coefficient for phi(i,j)
            A(k,k) = 1.0 + Sx * D(k) + Sx * D(k+1) +  Sy * D(k) +  Sy * D(k+nx)
            ! A(k,k-1) = -Sx            ! coefficient for phi(i-1,j)
            A(k,k-1) = -Sx * D(k)
            ! A(k,k+1) = -Sx            ! coefficient for phi(i+1,j)
            A(k,k+1) = -Sx * D(k+1)
            ! A(k,k+nx) = -Sy           ! coefficient for phi(i,j+1)
            A(k,k+nx) = -Sy * D(k+nx)
            ! A(k,k-nx) = -Sy           ! coefficient for phi(i,j-1)
            A(k,k-nx) = -Sy * D(k)
        end do
    end do
    


    ! Assemble the U0
    U0=0.d0
    k=(((ny/2)+1)-1)*nx+((nx/2)+1)
    U0(k)=100.0
    
    
    ! do i=0,1
    !     do j=0,1
    !         k=(((ny/2)+1)-1+j)*nx+((nx/2)+1+i)
    !         U0(k)=100.0
    !         k=(((ny/2)+1)-1-j)*nx+((nx/2)+1-i)
    !         U0(k)=100.0
    !     end do
    ! end do

    
    ! Initial solution profile use sin function as a first guess
    ! do i = 1, nx
    !    u(i) =  sin(dble(i)*h*PI)
    ! end do

    ! initial solution profile use U=0 as a first guess
    U=0.d0
  end subroutine assemble_system_dirichlet
