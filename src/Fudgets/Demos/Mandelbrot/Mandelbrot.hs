module Mandelbrot where
--import UnsafePerformIO(unsafePerformIO)

area0 = ((-2.0,2.0),(-2.0,2.0))

#if 0
-- The code generated by GHC is about as fast as this:
mandelbrot :: (Double,Double) -> Int
mandelbrot (x0,y0) =
  unsafePerformIO $
  _casm_ ``
    {int n=0;
    double x0=%0,y0=%1,x=0,y=0;
    while(n<200 && x*x+y*y<=4) n++,x=x*x-y*y+x0,y=2*x*y+y0;
    %r=n;
    };'' x0 y0
#else

mandelbrot :: (Double,Double) -> Int
mandelbrot (x0,y0) = m 0 0 0
  where m :: Int -> Double -> Double -> Int
        m n x y = if sqabs x y<= 4 && n< 200
		  then m (n+1) (x*x-y*y+x0) (2*x*y+y0)
		  else n

	sqabs :: Double -> Double -> Double
	sqabs x y = x*x+y*y

#endif
