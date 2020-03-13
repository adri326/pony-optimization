use "../../optimization"
use "promises"
use "collections"
use "random"
use "time"

actor Main
  new create(env: Env) =>
    let rng = XorOshiro128StarStar(Time.nanos(), Time.cycles() xor Time.millis())
    let n_points: USize = 100
    let n_coeffs: USize = 50
    let points': Array[(F64, F64)] trn = recover trn Array[(F64, F64)](n_points) end
    for i in Range[USize](0, n_points) do
      points'.push((i.f64() - (n_coeffs.f64() / 2), (rng.real() * 10) - 5))
    end
    let points: Array[(F64, F64)] val = consume points'

    let promise = Promise[Array[F64] val]
    promise.next[None](_Fulfill(env.out))
    GradientDescent(
      recover val Array[F64].init(0, n_coeffs) end,
      {(arr: Array[F64] val): F64 =>
        var res = F64(0)
        for (x, y) in points.values() do
          var sum = F64(0)
          for (exp, coeff) in arr.pairs() do
            sum = sum + (coeff * x.pow(exp.f64()))
          end
          res = res + (y - sum).pow(2)
        end
        res.sqrt()
      },
      10000, 0.1, 0.5
    ).minimize(promise)

class _Fulfill is Fulfill[Array[F64] val, None]
  let out: OutStream

  new iso create(out': OutStream) =>
    out = out'

  fun apply(arr: Array[F64] val) =>
    for value in arr.values() do
      None
      // out.print(value.string())
    end

  fun reject() =>
    out.print("Abort!")
