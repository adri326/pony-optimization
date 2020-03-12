use "promises"
use "collections"

type CostFn is {(Array[F64] val): F64} val

trait Minimizer // designed for actors
  fun tag minimize(prom: Promise[Array[F64] val])
  fun tag _receive_cost(cost: F64, index: USize)
