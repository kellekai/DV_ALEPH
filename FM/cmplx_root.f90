module cmplx_alpha

contains 

Subroutine Eval(x,y,u,v,u1,v1,u2,v2,arr)

  real*8 x,y,u,v,u1,v1,u2,v2,angle,arr(3)
  real*8  :: pi = 4.0d0 * atan(1.0d0)
  real*8  :: mtau = 1.77684d0
  complex*16 C8_Hatau,C8_Has,as,atau,beta,tot,ii,C8_HasPrime

  as = dcmplx(x,y)
  angle = arr(3)
  s = arr(1)
  atau = arr(2)
  
  !write(*,*) angle,atau,s
  
  C8_Has = -0.2222222222222222d0/(as/pi) + 0.3730002895803152d0 * atan(0.1957622473346858d0 - 2.777520917064214d0*(as/pi)) - & 
		0.3950617283950617d0 * log((as/pi)) + 0.2727626771780712d0 * log(0.3539687005190283d0 + 1.0d0 * (as/pi)) + &
		0.06114952560849528d0 * log(0.13459153249825306d0 - 0.14096185280332845d0 * (as/pi) + 1.0d0 * (as/pi)**2)
		
  C8_HasPrime = (-1.036016106380334d0/(1.d0 + (0.1957622473346858d0 - 2.777520917064214d0*(as/pi))**2) + &
  0.2222222222222222d0/(as/pi)**2-0.3950617283950617d0/(as/pi) + 0.2727626771780712d0/(0.3539687005190283d0 + &
  1.d0*(as/pi)) + (0.06114952560849528d0*(-0.14096185280332845d0 + 2.d0*(as/pi)))/(0.13459153249825306d0 - &
  0.14096185280332845d0*(as/pi) +1.d0*(as/pi)**2))/pi
  
  C8_Hatau = -0.2222222222222222d0/(atau/pi) + 0.3730002895803152d0 * atan(0.1957622473346858d0 - 2.777520917064214d0*(atau/pi)) - & 
		0.3950617283950617d0 * log((atau/pi)) + 0.2727626771780712d0 * log(0.3539687005190283d0 + 1.0d0 * (atau/pi)) + &
		0.06114952560849528d0 * log(0.13459153249825306d0 - 0.14096185280332845d0 * (atau/pi) + 1.0d0 * (atau/pi)**2)
		
  beta = log(s*exp(dcmplx(0.0d0,angle-pi))/mtau**2)/2.0d0
  
  !tot = C8_Has - C8_Hatau + beta
  
  u = real(C8_Has) - C8_Hatau + real(beta)
  v = aimag(C8_Has) + aimag(beta)

  u1=real(C8_HasPrime); u2=-aimag(C8_HasPrime)
  v1=aimag(C8_HasPrime); v2=real(C8_HasPrime)
  return
end
!*********************************************************


!*************************************************
!*  Complex root seeking using Newton's method   *
!* --------------------------------------------- *
!* This routine uses the complex domain form of  *
!* Newton's method for iteratively searching     *
!* for roots. The complex function and its first *
!* partial derivatives must be available in the  *
!* form: F(Z) = U(X,Y) + I V(X,Y). The required  *
!* derivatives are DU/DX and DU/DY.              *
!* --------------------------------------------- *
!* INPUTS; initial guess, x0 and y0, convergence *
!* criteria, e, maximum number of iterations, n. *
!* OUTPUTS; approximation to the root, x and y,  *
!* number of performed iterations, k.            *
!*************************************************
Subroutine ZNewton(n,k,a,e,x0,y0,x,y,arr)
  !Label: 100
  parameter(TINY=1.d-12)  !small number
  integer n,k
  real*8 e,x0,y0,x,y,a
  real*8 u,v,u1,v1,u2,v2,arr(3)
  k=0 
100 k=k+1
  ! Get u,v and the derivatives u1,v1,u2,v2
  call Eval(x0,y0,u,v,u1,v1,u2,v2,arr)
  a=u1*u1+u2*u2
  ! Guard against a=0
  if (a < TINY) then
    print *,' '
    print *,'ZERO DIVIDE ERROR - u1*u1+u2*u2 must be <> 0.'
    print *,' '
    return
  end if
  x=x0+(v*u2-u*u1)/a
  y=y0-(v*u1+u*u2)/a
  ! Check for convergence in euclidean space
  if ((x0-x)*(x0-x)+(y0-y)*(y0-y)<=e*e) return
  if (k>=n) return
  x0=x ; y0=y
  goto 100
end

end module
