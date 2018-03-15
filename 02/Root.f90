!Program name : Root.f90
!
!Goal: To searching the root of an eqution by following method. 
!Bisection method(二分法)
!The Newton method(牛顿法)
!Secant method(割线法)
!
!Yang Hang
!
!Last modiifed: 2018.03.15
!
!The eqution: f(x) = exp(x) * log(x) - x**2 = 0

MODULE global                                               !全局变量
    IMPLICIT NONE
    INTEGER i,j,k
    REAL(KIND = 8) a,b,c,n,m,x,y,z
    REAL(KIND = 8),EXTERNAL :: func,func1                   !注意此处一定要有::，否则编译报错
END MODULE

PROGRAM root
    USE global
    IMPLICIT NONE
    
    !输入精度要求
    WRITE(*,"(50A)") "Input the required significance digit:"
    READ(*,*) n
    m = 1 / 10**n
    
    !输入选择寻求方程解的范围，及判断该范围内是否有解
    500 WRITE(*,"(50A)") "Input the boundary"
    WRITE(*,"(20A)") "a -> "
    READ(*,*) a
    WRITE(*,"(20A)") "b -> "
    READ(*,*) b
    IF (func(a) * func(b) > 0) THEN
        WRITE(*,"(50A)") "There is no root in this area,input the right boundary."
        GOTO 500
    END IF
    
    !输入选择方法，分为1、2、3与退出程序
    WRITE(*,"(50A)") "Method select:(Input the front number)"
    WRITE(*,"(4X,20A)") "1.Bisection."
    WRITE(*,"(4X,20A)") "2.The Newton."
    WRITE(*,"(4X,20A)") "3.Secant."
    WRITE(*,"(4X,20A)") "0.Exit the program."
    501 READ(*,*) i
    !判断选择方法
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
        GOTO 501
    END IF

    WRITE(*,*) "a = ",a,func(a),"b = ",b,func(b),j
END PROGRAM


FUNCTION func(x)                                            !将需求解函数写成FUNCTION，方便调用与改动
    IMPLICIT NONE
    REAL(KIND = 8) x
    REAL(KIND = 8) func
    func = exp(x) * log(x) - x**2
END FUNCTION

FUNCTION func1(x)                                           !同上，写出方程的一阶导数
    IMPLICIT NONE
    REAL(KIND = 8) x
    REAL(KIND = 8) func1
    func1 = exp(x) /x + exp(x) * log(x) - 2 * x              !因为其可解析求导，不再利用数值的方法求解
END FUNCTION

SUBROUTINE bisection()                                      !二分法求解子程序，需要时调用
    USE global
    IMPLICIT NONE
    c = (a + b) / 2                                         !二点中点
    DO WHILE (abs(a - b) > m)                               !精度要求
        IF (func(a) * func(c) < 0) THEN                     !判断条件
            b = c                                           !若成立，则新的边界y为中点
            c = (a + b) / 2                                 !新边界下的新的中点
            j = j + 1                                       !迭代次数
        ELSE
            a = c                                           !若不成立，则新边界x为中点
            c = (a + b) / 2                                 !同上
            j = j + 1                                       !同上
        END IF
    END DO
END SUBROUTINE

SUBROUTINE newton()                                         !牛顿法求解子程序，需要时调用
    USE global
    IMPLICIT NONE
    DO WHILE (abs(a - b) > m)                               !精度要求
        b = a                                               !选取边界a为初始点
        a = a - func(a) / func1(a)                          !带入牛顿法公式计算切线对应的下一个点
        j = j + 1                                           !迭代次数
    END DO    
END SUBROUTINE

SUBROUTINE secant()                                         !割线法求解子程序，需要时调用
    USE global
    IMPLICIT NONE
    DO WHILE (abs(a - b) > m)                               !精度要求
        c = a - (a - b) * func(a) / (func(a) - func(b))     !带入割线法公式计算割点
        b = a                                               !得到割点后b的取值
        a = c                                               !a的取值，注意赋值的前后顺序
        j = j + 1                                           !迭代次数
    END DO
END SUBROUTINE
