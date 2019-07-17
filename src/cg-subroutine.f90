  !-------------------------------------------------------------------!
  ! Solve the linear system using conjugate gradient method A x = b
  ! the conjugate gradient method is an algorithm for the numerical solution of particular systems of linear equations, namely those whose matrix is symmetric and positive-definite. 
  ! A positive definite matrix is a symmetric matrix with all positive eigenvalues
  ! Note that as it’s a symmetric matrix all the eigenvalues are real, so it makes sense to talk about them being positive or negative.
  !-------------------------------------------------------------------!
   subroutine conjGrad(N, A , b, max_iter, max_tol, x, iter, tol, flag)
      implicit none
      integer :: N         ! size of A
      real(8) :: A(N,N)    ! the coefficient matrix
      real(8) :: b(N)      ! the right hand side of the equation
      integer :: max_iter  ! maximum number of iteration 
      real(8) :: max_tol   ! maximum tolerance for the solution
      real(8) :: x(N)      ! the solution of the system
      integer :: iter      ! the last iteration number when the tolerance criterion satisfied
      real(8) :: tol       ! the last tolerance of the solution
      integer :: flag      ! for error handeling, if flag==0 the subroutine executed without any problem

      ! create local data
      real(8), allocatable :: p(:), r(:), w(:), q(:)
      real(8), allocatable :: rho(:)
      real(8) :: alpha, beta , rho0
      real(8) :: bnorm, rnorm
      integer :: k

      ! Memory allocations
      allocate(r, p, q, w, mold=x)
      allocate(rho(max_iter))
      r = 0.d0 ;   p = 0.d0 ;  q = 0.d0 ;  w = 0.d0
      rho = 0.d0 ;  rho0 = 0.d0
      !!!ALLOCATE(B, MOLD=A)
      !!!The MOLD= specifier works almost in the same way as SOURCE=. If you specify MOLD= and source_expr is a variable, its value need not be defined. In addition, MOLD= does not copy the value of source_expr to the variable to be allocated.

      ! Start the iteration counter
      iter = 1

      ! Norm of the right hand side 
      bnorm = norm2(b)
      !norm2(vec) gives sqrt(dot_product(vec,vec))

      r = b ;  p = b ;  x = 0.d0
      q = matmul(A,p)

      ! Norm of the initial residual
      rnorm = norm2(r)
      rho(iter) = rnorm * rnorm

      alpha = rho(iter) / dot_product(p,q)
      x = x + ( alpha * p )
      r = r - ( alpha * q )

      open(10, file='cg.log', action='write', position='append')
      
      do k = 2 , max_iter

         rho0 = rho(k-1) 
         rnorm = norm2(r)
         rho(k) = rnorm * rnorm
         beta = rho(k) / rho0

         p = r + ( beta * p )
         q = matmul(A,p)

         alpha = rho(k)/ ( dot_product(p,q) )
         
         x = x + ( alpha * p )
         r = r - ( alpha * q )
   
         rnorm = norm2(r)
         tol = rnorm/bnorm
         
         write(10,*) iter, tol
         ! print *, k, tol

         if ( k > max_iter .or. tol < max_tol) then
            exit
         end if

      end do

      close(10)
      deallocate(r, p, w, rho)
      flag = 0
   end subroutine conjGrad
