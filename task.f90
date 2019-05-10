module Task
    use mpi
    implicit none
    contains
    
    subroutine GetMaxCoordinates(A, x1, y1, x2, y2)
        implicit none
        real(8), intent(in), dimension(:,:) :: A
        integer(4), intent(out) :: x1, y1, x2, y2
        integer(4) :: n, L, R, Up, Down, m, tmp
        real(8), allocatable :: currentColumn(:), B(:,:)
        real(8) :: currentSum, maxSum, totalMax
        logical :: transpos
        integer(4) :: mpiErr, mpiSize, mpiRank, localMaxTrade, globalMaxTrade 
    
        call mpi_comm_size(MPI_COMM_WORLD, mpiSize, mpiErr)
        call mpi_comm_rank(MPI_COMM_WORLD, mpiRank, mpiErr)

        m = size(A, dim=1) 
        n = size(A, dim=2) 
        transpos = .FALSE.

        if (m < n) then 
            transpos = .TRUE.   
            B = transpose(A)
            m = size(B, dim=1) 
            n = size(B, dim=2) 
        else
            B = A     
        endif
        allocate(currentColumn(m))

        maxSum=B(1,1)
        x1=1
        y1=1
        x2=1
        y2=1

        do L = mpiRank + 1, n, mpiSize

            currentColumn = B(:, L)            
            do R = L,n
 
                if (R > L) then 
                    currentColumn = currentColumn + B(:, R)
                endif
                
                call FindMaxInArray(currentColumn, currentSum, Up, Down) 

                if (currentSum > maxSum) then
                    maxSum = currentSum
                    x1 = Up
                    x2 = Down
                    y1 = L
                    y2 = R
                endif
            end do
        end do

        call MPI_Allreduce(maxSum, totalMax, 1, MPI_real8, MPI_Max, MPI_COMM_WORLD)

        localMaxTrade = mpiSize + 1

        if (maxSum == totalMax) then
            localMaxTrade = mpiRank
        endif

        call MPI_Allreduce(localMaxTrade, globalMaxTrade, 1, MPI_INTEGER4, MPI_Min, MPI_COMM_WORLD)

        call mpi_bcast(x1, 1, MPI_INTEGER4, globalMaxTrade, MPI_COMM_WORLD, mpiErr)
        call mpi_bcast(x2, 1, MPI_INTEGER4, globalMaxTrade, MPI_COMM_WORLD, mpiErr)
        call mpi_bcast(y1, 1, MPI_INTEGER4, globalMaxTrade, MPI_COMM_WORLD, mpiErr)
        call mpi_bcast(y2, 1, MPI_INTEGER4, globalMaxTrade, MPI_COMM_WORLD, mpiErr)


        deallocate(currentColumn)

        if (transpos) then  
            tmp = x1
            x1 = y1
            y1 = tmp
    
            tmp = y2
            y2 = x2
            x2 = tmp
        endif

    end subroutine

    subroutine FindMaxInArray(a, Sum, Up, Down)
        real(8), intent(in), dimension(:) :: a
        integer(4), intent(out) :: Up, Down
        real(8), intent(out) :: Sum
        real(8) :: cur_sum
        integer(4) :: minus_pos, i

        Sum = a(1)
        Up = 1
        Down = 1
        cur_sum = 0
        minus_pos = 0



        do i=1, size(a)
            cur_sum = cur_sum + a(i)
            
            if (cur_sum > Sum) then
                Sum = cur_sum
                Up = minus_pos + 1
                Down = i
            endif
            
            if (cur_sum < 0) then
                cur_sum = 0
                minus_pos = i
            endif
        enddo

    end subroutine FindMaxInArray


end module Task