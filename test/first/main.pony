use "../../optimization"
use "promises"

actor Main
  let out: OutStream

  new create(env: Env) =>
    out = env.out
    let promise = Promise[Array[F64] val]
    let a: F64 = 10
    let b: F64 = -2
    let c: F64 = 4
    let d: F64 = -2
    let e: F64 = 6
    let f: F64 = -23
    let g: F64 = 1
    promise.next[None](_Fulfill(out))
    GradientDescent(
      recover val [as F64: 0; 0; 0; 0; 0; 0; 0] end,
      {(arr: Array[F64] val): F64 =>
        try
          ((arr(0)? - a).pow(2) + (arr(1)? - b).pow(2) + (arr(2)? - c).pow(2) + (arr(3)? - d).pow(2) + (arr(4)? - e).pow(2) + (arr(5)? - f).pow(2) + (arr(6)? - g).pow(2)).sqrt()
        else out.print("Couldn't read element off cost input array"); 0 end
      },
      10000, 0.01, 0.002
    ).minimize(promise)

class _Fulfill is Fulfill[Array[F64] val, None]
  let out: OutStream

  new iso create(out': OutStream) =>
    out = out'

  fun apply(arr: Array[F64] val) =>
    for value in arr.values() do
      out.print(value.string())
    end

  fun reject() =>
    out.print("Abort!")
