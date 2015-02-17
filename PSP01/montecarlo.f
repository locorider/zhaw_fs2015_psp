program Montecarlo
    implicit none

    integer :: total_points
    real :: calculate_pi
    real :: pi

    print*, "Enter the total amount of points for the calculation..."
    read*, total_points

    pi = calculate_pi(total_points)

    write(*,*) 'PI = ', pi
end

real function calculate_pi(p)
        integer :: p
        real in_area, x, y

        in_area = 0
        x = 0
        y = 0

        do i=1,p
            x = rand(0)
            y = rand(0)
            z = calculate_z(x,y)
            if (z .le. 1.) then
                in_area = in_area + 1
                write(*,*) 'In Area = ', in_area, 'X = ', x, ' Y = ', y, ' Z = ', z
            else
                write(*,*) 'NOT In Area = X = ', x, ' Y = ', y, ' Z = ', z
            end if
        enddo

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
