use "../../optimization"
use "promises"
use "options"

actor Main
  new create(env: Env) =>
    let method: U8 = if env.args.contains("--grid", {(a: String, b: String) => a == b}) then 1 else 0 end
    let a: F64 = 10
    let b: F64 = -2
    let c: F64 = 4
    let d: F64 = -2
    let e: F64 = 6
    let f: F64 = -23
    let g: F64 = 1

    let fn: CostFn val = {(arr: Array[F64] val): F64 =>
        try
          ((arr(0)? - a).pow(2) + (arr(1)? - b).pow(2) + (arr(2)? - c).pow(2) + (arr(3)? - d).pow(2) + (arr(4)? - e).pow(2) + (arr(5)? - f).pow(2) + (arr(6)? - g).pow(2)).sqrt()
        else env.out.print("Couldn't read element off cost input array"); 0 end
      }
    let promise = Promise[Array[F64] val]
    promise.next[None](_Fulfill(env.out))
    let minimizer = match method
      | 1 => GridSearch(
        7,
        recover val Array[F64].init(-100, 7) end,
        recover val Array[F64].init(100, 7) end,
        fn,
        100,
        5,
        3
      )
      else GradientDescent(
        recover val [as F64: 0; 0; 0; 0; 0; 0; 0] end,
        fn,
        10000, 0.01, 0.002
      )
      end
    minimizer.minimize(promise)

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
