program testOpenMP
  use :: Task
  use :: mpi
  implicit none
  
  integer(4) :: matrix_size, x1,x2,y1,y2,i,j
  real(8), dimension(:,:), allocatable :: matrix
  real(8) start_time, finish_time

  integer(4) :: mpiErr
    
  call mpi_init(mpiErr)


  matrix = rnd_matrix(3000)
  
  call GetMaxCoordinates(matrix, x1,y1,x2,y2)
  
  call mpi_finalize(mpiErr)
  
end program testOpenMP