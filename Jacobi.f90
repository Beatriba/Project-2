program Jacobi
implicit none
   integer, parameter :: n = 10
   real(8), parameter :: pi = 3.14159265358979
   real(8) :: h, h_sq, alpha, beta
   real(8), dimension(n-1) :: d, z
   real(8), dimension(n-2) :: a
   real(8), dimension(4*(n-1)) :: work
   integer :: info, lz, i

   lz = 1
   h = 1d0 / n; h_sq = h * h
   alpha = 2 / h_sq; beta = pi / (n + 1) 
   d = 2 / h_sq; a = -1 / h_sq

   call dpteqr("n", n-1, d, a, z, lz, work, info)

   z = (/ ( alpha * (1 - cos(i * beta)), i = n - 1, 1, -1) /)

   print *, d
   print *, "  "
   print *, z
   print *, (d - z) / z
end program
