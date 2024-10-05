PROGRAM KS_Test
  IMPLICIT NONE
  INTEGER :: i, n
  REAL :: D, max_diff
  REAL, ALLOCATABLE :: data(:), F_data(:), F_theory(:)
  
  ! Number of data points
  n = 10
  ALLOCATE(data(n))
  ALLOCATE(F_data(n))
  ALLOCATE(F_theory(n))
  
  ! Sample data (replace with your own)
  data = (/ 0.1, 0.15, 0.35, 0.4, 0.5, 0.6, 0.65, 0.7, 0.85, 0.9 /)
  
  ! Theoretical cumulative distribution function (Uniform in this case)
  DO i = 1, n
    F_theory(i) = data(i)  ! CDF of Uniform(0,1)
  END DO

  ! Empirical cumulative distribution function (ECDF)
  DO i = 1, n
    F_data(i) = REAL(i) / REAL(n)  ! ECDF
  END DO
  
  ! Calculate the Kolmogorov-Smirnov statistic
  max_diff = 0.0
  DO i = 1, n
    D = ABS(F_data(i) - F_theory(i))
    IF (D > max_diff) max_diff = D
  END DO

  PRINT*, 'Kolmogorov-Smirnov Statistic (D): ', max_diff

  DEALLOCATE(data)
  DEALLOCATE(F_data)
  DEALLOCATE(F_theory)

END PROGRAM KS_Test
