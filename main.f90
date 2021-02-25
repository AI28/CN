program name

    use functii
    use tangent

    implicit none

    double precision::result
    double precision, dimension(3)::mult_precision_set
    real(4), dimension(10000) :: random_numbers, fortran_tan_values, lentz_tan_values, optimal_tan_values
    real(4), dimension(10000) :: mac_laurin_tan_values
    real(4) :: avg_error
    logical::result_2
    integer::i, num

    result = get_machine_precision()
    result_2 = plus_non_assoc(result)

    print *, result, result_2

    mult_precision_set = mult_non_assoc()
    print *, mult_precision_set

    random_numbers = generate_unbounded_values()

    do i = 1, 10000
        if(random_numbers(i) > 3.1416_4 / 2 .or. random_numbers(i) < - (3.1416_4 / 2)) then 
          num = num + 1
        end if
    end do 

    print *, num

    fortran_tan_values = apply_tan(random_numbers)
    mac_laurin_tan_values = apply_mac_laurin_tan(random_numbers)
    lentz_tan_values = apply_lentz_tan(random_numbers)
    optimal_tan_values = apply_optimal_tan(random_numbers)

    avg_error = get_avg_error(fortran_tan_values, optimal_tan_values)

    print *, avg_error

end program name