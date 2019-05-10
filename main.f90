program OpenMP
  use :: Task
  use :: omp_lib
  implicit none
  
  integer(4) :: matrix_size, x1,x2,y1,y2,i,j
  real(8), dimension(:,:), allocatable :: matrix
  real(8) start_time, finish_time, oend, ostart
  ! open(unit = 10, file= 'input.dat', action = 'read', status = 'old')
  ! read(10,*) matrix_size
  ! allocate(matrix(matrix_size, matrix_size))
  ! read(10,*) matrix
  ! close(10)
  
  ! open(unit = 30, file = 'time.dat', action = 'write', status = 'replace')
  
  matrix = rnd_matrix(3000)
  ostart = omp_get_wtime()
  !do i = 1, 30
  !call cpu_time(start_time)
  call GetMaxCoordinates(matrix, x1,y1,x2,y2)
  !call cpu_time(finish_time)
  !write(*,*) finish_time - start_time
  !write(30,*) 100*i , (finish_time - start_time)
  !enddo
  oend = omp_get_wtime()
  write(*,*) oend - ostart
  
  ! close(30)
  
  ! open(unit = 20, file= 'output.dat', action = 'write', status = 'replace')
  ! do i = y1, y2
  ! write(20,*) ( matrix(j ,i) ,j = x1, x2 )
  ! enddo
  ! close(20)
  
end program OpenMP
