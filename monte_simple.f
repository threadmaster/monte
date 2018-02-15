      program monte
     
      double precision x, num, pi, piover4
      integer iseed(4)
      double precision func
      integer n
      external func

      pi = acos(-1.0D0)
      piover4 = pi/4.0D0

      call srand(1)

      n = 10000000
      num = 0.0D0
      do 10 i = 1, n
         if (abs(num/dble(i) - piover4) .lt. 1.0D-12) goto 100 
         x = rand()
         num = num +  func( x )
10    continue
100   print '(2x,f14.12,1x,i12,1x,f14.12)', num*4/dble(i-1), i-1, pi

      end 
     
      double precision function func ( x )
      double precision x 
      func =  sqrt(1.0D0 - x*x)
      return
      end 


     

