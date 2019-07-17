subroutine write_phi_to_file(x1, xL, nx, y1, yL, ny, phi,filename)
    implicit none
    integer :: nx, ny, i, j, k
    real(8) :: phi(nx*ny)
    real(8) :: x1, xL     ! bounds of the domain
    real(8) :: y1, yL     ! bounds of the domain
    real(8) :: dx, dy, xi, yj, phi_ij
    character(20)::filename
    
    dx= (xL-x1)/real(nx-1)
    dy= (yL-y1)/real(ny-1)
    xi=((i-1)*dx)+x1
    yj=((j-1)*dy)+y1
    
    open(91,file=filename)
    do i=1,nx
        xi=((i-1)*dx)+x1
        do j=1,ny
            yj=((j-1)*dy)+y1
            k=(j-1)*nx+i
            phi_ij = phi(k)
            write(91,*) xi,yj,phi_ij
        end do
        write(91,*) ""
    end do
    close (91)

end subroutine write_phi_to_file




subroutine print_Array(myArray,Nrow,Ncol)
    implicit none
    integer::i,j
    integer::Nrow,Ncol
    real(8)::myArray(Nrow,Ncol)
    ! print*,"spinArray:"
    do i=1,Nrow
        do j=1,Ncol
            write(*,"(f7.2$)")myArray(i,j)
        end do
        write(*,*)
    end do
 end subroutine print_Array


 
 