module ImageFetchFlags where
import Fudgets

parallel = argReadKey "parallel" 5 :: Int
imglog = argFlag "imglog" False
imglogstderr = argFlag "imglogstderr" False
color = argFlag "color" True
colorCube = argReadKey "colorCube" 6 :: Int
