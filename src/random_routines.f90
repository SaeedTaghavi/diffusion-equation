subroutine random_integer(mini,maxi,i)
    implicit none
    integer::i,mini,maxi
    real :: r
    call random_number(r)
    r=r*(maxi-mini)+mini+1
    i=int(r)
end subroutine random_integer
