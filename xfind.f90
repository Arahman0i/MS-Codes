! Last Updated on 19th Nov.
! By Ataur Rahman
! M.Sc. Physics (Computational Physics)
! Central University of Punjab, Bathinda

module xfind

implicit none
contains

subroutine bisection()
        implicit none
        
        real :: a, b, error
        real :: r
        real, external :: f
        integer :: i = 0
        real :: x0, x1, x2, fa, fb, fc
        
        write(*, *) "For Bisection Method:"
        write(*, *) "------------------------------------------------------------------------------"
        write(*, *) "Enter the value of a & b: "
        read(*, *) a, b
        write(*, *) "Enter the value of tolerance:"
        read(*, *) error
        
        x0 = a
        x1 = b
        
        fa = f(x0)
        fb = f(x1)
        
        
        
        if (fa*fb < 0) then 
          write(*, *) "------------------------------------------------------------------------------"
          write(*, *) "The initial guesses are correct."

         do while ((abs(x1-x0)/x1) > error)
           x2 =(x0 + x1)/2
           fc = f(x2)

          if (fa*fc> 0) then
            x0 = x2
            fa = fc
          else
            x1 = x2
            fb = fc
          end if
          
          i = i + 1
         end do

       r = x2
       write(*, *) "------------------------------------------------------------------------------"
       write(*, *) "The number of iterations = ", i
       write(*, *) "The root = ", x2, "& fc = ", fc

        else 
         write(*, *) "Try another values of a & b."
         end if 
         write(*, *) "------------------------------------------------------------------------------"
end subroutine bisection

subroutine newton(r)
        implicit none
        
        integer :: i, n
        !real, intent(IN) :: a
        real, intent(OUT) :: r
        real :: x0, x1, f0, fprime0, ep, del
        real, external :: f
        real, external :: fprime
        !ep- epsilon prescribed relative error
        !del- delta is the prescribed lower bound for fprime
        !n- is the maximum no. of iterations to be allowed.

	write(*, *) "For Newton-Raphson Method:"

        !prompt the user for the input
        write(*, *) "------------------------------------------------------------------------------"
        write(*, *) "Enter the value of x0: "
        read(*, *) x0
        !x0 = a

        write(*, *) "Enter the maximum no. of iterations: "
        read(*, *) n
	write(*, *) "------------------------------------------------------------------------------"
        write(*, 10) "Itr. no", "x0", "x1", "f(x0)", "fprime(x0)"
        10 format(7x,A,5x,A,15x,A,15x,A,9x,A,10x,A)
        
        do i = 1, n
         f0 = f(x0)
         fprime0 = fprime(x0)

         if (abs(fprime0) <= del) then
           write(*, *) "Slope is too small", x0,f0,i
           else
             x1 = x0 -(f0/fprime0)
             if (abs((x1 - x0)/x1)>ep) then
               x0 = x1
               write(*, *) i, x0, x1, f(x0), fprime(x0)

               !11 format(11x,A,8x,A,15x,A,15x,A,15x,A,15x,A)
                else
                  write(*, *) "------------------------------------------------------------------------------"
                  write(*, *) "The roots are: ", x1,f0,i
                  write(*, *) "------------------------------------------------------------------------------"
                  exit
             end if
         end if
        end do

        end subroutine newton
subroutine regula()
	implicit none
	
	real :: x0, x1, x2, e, f0, f1, f2
	integer :: i, n
	real, external :: f
	write(*, *) "For Regula-Falsi Method: "
        write(*, *) "------------------------------------------------------------------------------"
	write(*, *) "Enter the value of x0 and x1: "
	read(*, *) x0, x1

	write(*, *) "Enter the value of maxiter and precision: "
	read(*, *) n, e

	f0 = f(x0)
	f1 = f(x1)

	write(*, 10) "i", "x0", "x1", "x2", "f0", "f1", "f2"
	10 format(11x,A,8x,A,15x,A,15x,A,15x,A,15x,A,15x,A)

	 do   i = 1, n, 1
		 x2 = (x0*f1 - x1*f0)/(f1 - f0)
		 f2 = f(x2)
		 write(*, *) i, x0, x1, x2, f0, f1, f2
		 if (abs(f2)<=e) then
		   write(*, *) "------------------------------------------------------------------------------"
	           write(*, *) "The solution is: ", x2, f2 
	           exit
	         end if
  
	        if (f2*f0<0) then
	          x1 = x2
	          f1 = f2
	        else 
	          x0 = x2
	          f0 = f2
	        end if
  
	        if (i>n) then
	          write(*, *) "------------------------------------------------------------------------------"
	          write(*, *) "does not converge in ", n, "iterations."
	        end if 
  
	end do
	write(*, *) "------------------------------------------------------------------------------"

end subroutine regula

subroutine secant()
	implicit none
	
	real, external :: f
	integer :: i, n
	real :: x0, x1, x2, e, del, f0, f1, f2
	write(*, *) "For Secant Method: "
        write(*, *) "------------------------------------------------------------------------------"
	write(*,*) "Enter the value of the x0 and x1."
	read(*, *) x0, x1

	e = 1e-6
	del = 1e-6
	n = 100

	f0 = f(x0)
	f1 = f(x1)


	do i =1, n
		if (abs(f1 - f0) <del) then
		  write(*, *) "------------------------------------------------------------------------------"
		  write(*, *) "Slope too small, i, f0, f1, x0, x1", i, f0, f1, x0, x1 
		  exit
		else
		 x2 = (x0*f1 - x1*f0)/(f1 - f0)
		 f2 = f(x2)
		  if (abs(f2)>=e) then
		    f0 = f1
		    f1 = f2
		    x0 = x1
		    x1 = x2
		  else 
		    !write(*, *)"Does not converge in n iterations", i, x0, x1, f0, f1
		    exit
		  end if 
		  
	       end if
	       
	end do
	write(*, *) "------------------------------------------------------------------------------"
	write(*, *) "The convergent solution is: itr, root, f(root)", i, x2, f2
	write(*, *) "------------------------------------------------------------------------------"

end subroutine secant

end module xfind

        FUNCTION f(a)
         real :: f
         real, INTENT(IN) :: a
         f = a**2 - 25
        END FUNCTION

        REAL FUNCTION fprime(x)
         implicit none
         
         real, intent(IN) :: x
         fprime = 2*x
        END FUNCTION
