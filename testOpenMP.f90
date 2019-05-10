program testOpenMP
  use :: Task
  use :: mpi
  implicit none
  
  integer(4) :: matrix_size, x1,x2,y1,y2,i,j
  real(8), dimension(:,:), allocatable :: matrix
  real(8) start_time, finish_time

  integer(4) :: mpiErr, mpiRank
    
  call mpi_init(mpiErr)
call random_seed()
  allocate(matrix(4, 4))
  call mpi_comm_rank(MPI_COMM_WORLD, mpiRank, mpiErr)

  if(mpiRank == 0) then
  call random_number(matrix)
matrix = matrix*2 - 1
do i = 1, size(matrix,1)
    write(*,*) matrix(i,:)
enddo
write (*,*) "\n"
endif

call mpi_bcast(matrix, 9, MPI_INTEGER4, 0, MPI_COMM_WORLD, mpiErr)

!   matrix = rnd_matrix(3000)
  
  call GetMaxCoordinates(matrix, x1,y1,x2,y2)

  if(mpiRank == 0 ) then
  do i = x1, x2
    write(*,*) (matrix(i,j), j = y1,y2)
enddo
end if

  
  deallocate (matrix)
  
  call mpi_finalize(mpiErr)
  
end program testOpenMP