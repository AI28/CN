!------------------------------------------------------------------------------
! Universitatea "Alexandru Ioan Cuza", Iasi, Romania
!------------------------------------------------------------------------------
!
! MODULE:  FunctiiTema1
!
!> @author
!> Alexandru Ichim
!
! DESCRIPTION: 
!>  Short module description
!
! REVISION HISTORY:
! dd Mmm yyyy - Initial Version
! TODO_dd_mmm_yyyy - TODO_describe_appropriate_changes - TODO_name
!------------------------------------------------------------------------------


module functii

    implicit none

    private
    public get_machine_precision, plus_non_assoc, mult_non_assoc
    
contains

   function get_machine_precision() result(machine_precision)

       double precision :: machine_precision
       integer :: max_int, min_int, m 
       
       max_int = HUGE(0)
       min_int = -max_int

       do m = 0, min_int, -1

            machine_precision = 10.0d0 ** DBLE(m) 
            if ( 1.0d0 + machine_precision == 1.0d0) then
                exit
            endif
       end do

       machine_precision = machine_precision * 10

   end function get_machine_precision

   function plus_non_assoc(machine_precision) result(is_equal)

        double precision, intent(in) :: machine_precision

        logical :: is_equal
        double precision :: a, b, c, result_1, result_2

        a = 1.00
        b = machine_precision / DBLE(10)
        c = b
        
        result_1 = (a + b) + c
        result_2 = a + (b + c)

        is_equal = result_1 == result_2

   end function plus_non_assoc

   function mult_non_assoc() result(example_set)
        double precision, dimension(3) :: example_set
        double precision :: a, b, c
        integer :: max_int, min_int, m

        a = 2.00
        max_int = HUGE(0)
        min_int = -max_int

        do m = 0, min_int, -1
           b = 10.00 ** DBLE(m)
           c = b
           if (a * ( b * c) /= ( a * b) * c) then
              exit 
           end if
        end do

        example_set(1) = a
        example_set(2) = b
        example_set(3) = c

   end function mult_non_assoc
 
end module functii