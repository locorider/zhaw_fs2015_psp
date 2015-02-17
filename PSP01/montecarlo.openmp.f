program Montecarlo
    use omp_lib
    implicit none

    integer :: total_points, numthreads
    real :: calculate_pi
    real :: pi

    print*, "Enter the total amount of points for the calculation..."
    read*, total_points

    print*, "Enter the total amount of threads for the calculation..."
    read*, numthreads

    pi = calculate_pi(total_points, numthreads)

    write(*,*) 'PI = ', pi
end

real function calculate_pi(p, numthreads)
        integer :: p, numthreads
        integer*4 tid
        real in_area, x, y, z

        in_area = 0
        x = 0
        y = 0

        !$omp parallel private(x,y,z,in_area,tid,i) num_threads(numthreads)
        tid = omp_get_thread_num()

        do i=1,p/numthreads
            x = ran0(tid)
            y = ran0(tid)
            z = calculate_z(x,y)
            if (z .le. 1.) then
                !$omp critical
                in_area = in_area + 1
                !$omp end critical
                write(*,*) 'In Area = ', in_area, 'X = ', x, ' Y = ', y, ' Z = ', z
            else
                write(*,*) 'NOT In Area = X = ', x, ' Y = ', y, ' Z = ', z
            end if
        enddo
        !$omp end parallel

        calculate_pi = 4 * (p / in_area)

    return
end

real function calculate_z(x,y)
    real :: x, y
    real :: square_x, square_y
    square_x = x**2
    square_y = y**2
    calculate_z = sqrt(square_x + square_y)
end

real function ran0(seed)
    integer seed, ia, im, iq, ir, mask, k
    real ran0, am
    parameter (ia=16807, im=2147483647,am=1./im)
    iq = 127773, ir = 2836, mask = 123459876

    seed = ieor(seed, mask)

    k = seed / iq

    seed = ia * (seed - k * iq) - ir * k

    if (seed .lt. 0) then
        seed = seed + im
    end if

    ran0 = am * seed
    seed = ieor(seed,mask)

    return
end
