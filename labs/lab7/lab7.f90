!lab7 code, parallel matrix multiplication
!To compile: gfortran -fopenmp -o test.exe lab7.f90
!To run: ./test.exe


program lab7
    use omp_lib
    implicit none
    integer :: i1,j1,k,M,N
    integer(kind=8) :: t1,t2,rate !timer variables
    real(kind=8) :: Csum
    real(kind=8), allocatable, dimension(:,:) :: A,B,C


    !read in problem parameters
    open(unit=10,file='data.in')
        read(10,*) M
        read(10,*) N
    close(10)


    !initialize variables
    allocate(A(M,N),B(N,M),C(M,M))


      call random_number(A)
      call random_number(B)


call system_clock(t1)

!Rewrite as loop and parallelize
!$omp parallel &
!$omp shared (M,N,A,B,C) &
!$omp private (i1,j1,k)

!$omp do
do i1 = 1,M
  do j1 = 1,N
    C(i1,j1) = 0.d0
    do k = 1,M
    C(i1,j1) = C(i1,j1) + A(i1,k)*B(k,i1)
    end do
  end do
end do
!$omp end do
!$omp end parallel

call system_clock(t2,rate)

Csum = sum(C)

print *, 'wall time:',float(t2-t1)/float(rate)
!test parallelized code:
print *, 'test C:',maxval(abs(C-matmul(A,B)))
print *, 'test Csum:',(Csum-sum(C))/Csum






end program lab7
