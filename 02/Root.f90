!Program name : Root.f90
!
!Goal: To searching the root of an eqution by following method. 
!Bisection method(二分法)
!The Newton method(牛顿法)
!Secant method(割线法)
!
!Yang Hang
!
!Last modiifed: 2018.03.14
!
!The eqution: f(x) = exp(x) * log(x) - x**2 = 0

PROGRAM root
    IMPLICIT NONE
    INTEGER i,j,k
    REAL(KIND = 8) x
    REAL(KIND = 8),EXTERNAL :: func
    !a = 1
    !b = 2
    !c = b**2
!输入选择方法，分为1、2、3与退出程序
    WRITE(*,"(20A)") "Method select:(Input the front number)"
    WRITE(*,"(4X,20A)") "1.Bisection."
    WRITE(*,"(4X,20A)") "2.The Newton."
    WRITE(*,"(4X,20A)") "3.Secant."
    WRITE(*,"(4X,20A)") "0.Exit the program."
    500 READ(*,*) i
    
    IF (i == 1) THEN
        CALL bisection()
    ELSE IF (i == 2) THEN
        CALL newton()
    ELSE IF (i == 3) THEN
        CALL secant()
    ELSE IF (i == 0) THEN
        STOP
    ELSE
        WRITE(*,*) "Input the right number!"
        GOTO 500
    END IF

    x = 1
    WRITE(*,*) func(x) 
END PROGRAM

FUNCTION func(x)                            !将需求解函数写成FUNCTION，方便调用与改动
    IMPLICIT NONE
    REAL(KIND = 8) x
    REAL(KIND = 8) func
    func = exp(x) * log(x) - x**2
END FUNCTION

SUBROUTINE bisection()                      !二分法求解子程序，需要时调用
    IMPLICIT NONE
    INTEGER i,j,k
    REAL(KIND = 8) x
    REAL(KIND = 8),EXTERNAL :: func
END SUBROUTINE

SUBROUTINE newton()                         !牛顿法求解子程序，需要时调用
    IMPLICIT NONE
    REAL(KIND = 8) x
    REAL(KIND = 8),EXTERNAL :: func
    
END SUBROUTINE

SUBROUTINE secant()                        !割线法求解子程序，需要时调用
    IMPLICIT NONE
    REAL(KIND = 8) x
    REAL(KIND = 8),EXTERNAL :: func

END SUBROUTINE
