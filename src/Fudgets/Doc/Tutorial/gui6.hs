import Fudgets

main = fudlogue (shellF "Up/Down Counter" counterF)

counterF = intDispF >==< mapstateF count 0 >==<
           (buttonF "Up" >+< buttonF "Down")

count n (Left Click) = (n+1,[n+1])
count n (Right Click) = (n-1,[n-1])

