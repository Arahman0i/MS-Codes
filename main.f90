! Last Updated on 19th Nov.
! By Ataur Rahman
! M.Sc. Physics (Computational Physics)
! Central University of Punjab, Bathinda

program main 
use xfind
implicit none


integer :: i, n
real :: a1, a2, err, rt
  
  ! write(*, *) " Enter the function:"
  ! read(*, *)
  
  write(*, *) " Enter a number to choose a method: "
  write(*, *) " Bisection method:      1"
  write(*, *) " Newton-Raphson method: 2"
  write(*, *) " Regula-Falsi method:   3"
  write(*, *) " Secant method:         4"
  write(*, *) "------------------------------------------------------------------------------"
  read(*, *) i
  
  select case(i)
  case (1)
        call bisection()
  case(2)
        call newton(rt)
  case(3)
  	call regula()
  case(4)
  	call secant()
        
  
  case default
        write(*, *) "Enter a valid number."  
        write(*, *) "------------------------------------------------------------------------------"
  
  end select
  

end program main
