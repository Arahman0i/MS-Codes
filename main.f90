! Ataur Rahman
! M.Sc. Physics (Computational Physics)
! Central University of Punjab, Bathinda

program main 
use xfind
implicit none


integer :: i,n
real::a1,a2,err,rt
  
  !write(*,*)" Enter the function:"
  !read(*,*)
  
  write(*,*)" Enter a integer for the following methods: "
  write(*,*)" Bisection method:      1"
  write(*,*)" Newton-raphson method: 2"
  write(*,*)" Regula-falsi method:   3"
  write(*,*)" Secant method:         4"
  write(*,*)"------------------------------------------------------------------------------"
  read(*,*)i
  
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
        write(*,*)"Write a valid no."
  
  end select
  

end program main

!we need to run both the module and the main program togetherly using the command
!gfortran -o find xfind.f90 main.f90
! run the ./find to execute the output.
