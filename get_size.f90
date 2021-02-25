program getSize
implicit none

   real (kind = 4) :: a
   real (kind = 8) :: b
   integer (kind = 2) :: i
   integer (kind = 4) :: j

   print *,'precision of real(4) =', precision(a)
   print *,'precision of real(8) =', precision(b)
   
   print *,'range of real(4) =', range(a)
   print *,'range of real(8) =', range(b)
   

   print *,'maximum exponent of real(4) =' , maxexponent(a)
   print *,'maximum exponent of real(8) =' , maxexponent(b)
  
   print *,'minimum exponent of real(4) =' , minexponent(a)
   print *,'minimum exponent of real(8) =' , minexponent(b)
   
   print *,'bits in integer(2) =' , bit_size(i)
   print *,'bits in integer(4) =' , bit_size(j)
   
end program getSize