module tangent

    implicit none
    
    public
    !public lentz_tangent, generate_values, get_avg_error, apply_lentz_tan, apply_tan, apply_mac_laurin_tan, &
    !       generate_unbounded_values, apply_optimal_tan, mac_laurin_tagent

contains

    function apply_tan(values) result(tan_values)

        real(4), dimension(10000), intent(in) :: values
        real(4), dimension(10000) :: tan_values
        integer :: i

        tan_values = 0

        do i = 1, 10000
            tan_values(i) = tan(values(i))
        end do
    
    end function apply_tan

    function apply_mac_laurin_tan(values) result(tan_values)

        real(4), dimension(10000), intent(in) :: values
        real(4), dimension(10000) :: tan_values
        integer :: i

        tan_values = 0

        do i = 1, 10000
            tan_values(i) = mac_laurin_tagent(values(i))
        end do

    end function apply_mac_laurin_tan

    function apply_lentz_tan(values) result(tan_values)

        real(4), dimension(10000), intent(in) :: values
        real(4), dimension(10000) :: tan_values
        integer :: i
        
        tan_values = 0

        do i = 1, 10000
            tan_values(i) = lentz_tangent(values(i))
        end do
    
    end function apply_lentz_tan

    function apply_optimal_tan(values) result(tan_values)

        real(4), dimension(10000), intent(in) :: values
        real(4), dimension(10000) :: tan_values
        real(4), parameter :: boundry = 0.65_4 !pana la aceasta valoare a lui x, calculam tangenta cu aproximare prin fractii continue
        !dupa aceasta valoare, calculam folosind serii mac_laurin
        integer(2) :: i

        do i = 1, 10000
            if(values(i) < boundry) then
                tan_values(i) = lentz_tangent(values(i))
            else
                tan_values(i) = mac_laurin_tagent(values(i))
            end if
        end do

    end function apply_optimal_tan

    function generate_values() result(values)

        real(4), dimension(10000) :: values
        real(4), parameter :: pi = 4 * atan (1.0_4)  ! = 3.1416 - valori aberante pt aceasta aproximare al lui pi
        integer :: i

        call random_number(values)

        do i = 1, 10000
            values(i) = values(i) * (pi / 2) * (-1 ** (mod(i,2)))  !mapam vecctorul dat de la mult [0,1] la [-pi/2,pi/2]
        end do

    end function generate_values

    function generate_unbounded_values() result(values)

        real(4), dimension(10000)::values

        call random_number(values)
    
    end function generate_unbounded_values

    function get_avg_error(result_set_1, result_set_2) result(avg_error)

        real(4), dimension(10000), intent(in) :: result_set_1, result_set_2
        real(4) :: avg_error  !by-default == 0
        integer :: i

        do i = 1, 10000

            avg_error = avg_error + abs(result_set_1(i) - result_set_2(i))
            print *, result_set_1(i), result_set_2(i), atan(result_set_1(i))

        end do

        avg_error = avg_error / 10000.0_4


    end function get_avg_error
        
    function lentz_tangent(value) result(f_n)

        real(4), intent(in) :: value
        real(4) :: f_n, a, epsilon_1, epsilon_2, c_n, d_n, delta
        integer:: b, step
        
        a = value
        b = 0

        f_n = b
        epsilon_1 = 10.00 ** float(-12)
        epsilon_2 = 10.00 ** float(-15)

        if(f_n == 0) then
            f_n = epsilon_1
        end if

        c_n = f_n
        d_n = 0
        step = 1

        b = 2*step - 1

        d_n = b + a*d_n
        if(d_n == 0) then
            d_n = epsilon_1
        end if

        c_n = float(b) + (a / c_n)
        if(c_n == 0) then
            c_n = epsilon_1
        end if

        d_n = d_n ** (-1)
        delta = c_n * d_n
        f_n = delta * f_n
        step = step + 1
        a = - (value ** 2)
        !do while-ul fortranului este, de fapt, un while, iar un do while propriu-zis nu exista
        do while(abs(delta - 1) >= epsilon_2)

            d_n = b + a*d_n

            if(d_n == 0) then
                d_n = epsilon_1
            end if

            c_n = float(b) + (a / c_n)

            if(c_n == 0) then
                c_n = epsilon_1
            end if

            d_n = d_n ** (-1)
            delta = c_n * d_n
            f_n = delta * f_n
            step = step + 1

            b = 2*step - 1

        end do 
    end function lentz_tangent

    function aux_mac_laurin(value) result(aux_tan_value)

        real(4), intent(in)::value
        real(4) :: aux_tan_value
        real(4), parameter :: c1 = 1.0/3.0, c2 = 2.0/15.0, c3 = 17.0/315.0, c4 = 62.0/2835.0

        aux_tan_value = value + c1 * (value ** 3) + c2 * (value ** 5) + c3 * (value ** 7) + c4 * (value ** 9)

    end function aux_mac_laurin

    function mac_laurin_tagent(value) result(tan_value)
        
        integer(2) :: sign = 1, power = 1
        real(4) :: value
        real(4) :: tan_value, mod_value
        real(4), parameter :: pi = 4 * atan (1.0_4), quarter_pi = pi / 4.0_4, half_pi = pi /2.0_4


        mod_value = value

        if(value .le. 0)  then
            sign = -1
            value = value * (-1)
        end if

        if( value .ge. pi/2) then
            value = value - floor(value)*pi
        end if

        if (value .ge. pi/4) then
            value = pi/2 - value
            power = -1
        end if

        tan_value = sign * (aux_mac_laurin(value) ** power)
        value = mod_value
        return

    end function mac_laurin_tagent

end module tangent
