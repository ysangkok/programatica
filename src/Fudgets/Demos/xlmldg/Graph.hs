module Graph where
import Fudgets(Rect,Line)

data GRAPH a b = Graph [(Int , (NODE a))] [(EDGE b)]
data NODE a = Node Rect a
type EDGE a = ((a,a),Line)
