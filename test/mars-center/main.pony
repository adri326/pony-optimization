use "../../optimization"
use "promises"
use "files"

actor Main
  new create(env: Env) =>
    let method: U8 = if env.args.contains("--grid", {(a: String, b: String) => a == b}) then 1 else 0 end
    let src: String val = try
      let file = File.open(FilePath(env.root as AmbientAuth, "test/mars-center/data.txt")?)
      file.read_string(file.size())
    else
      env.out.print("Could not open file!")
      return
    end

    let data = recover val
      let res = Array[(F64, F64)]
      for line in src.split("\n").values() do
        if line == "" then continue end
        let tuple = line.split(",", 0)
        try
          res.push((tuple(0)?.trim().f64()?, tuple(1)?.trim().f64()?))
        else
          env.out.print("Syntax error in file!")
          return
        end
      end
      res
    end

    let fn: CostFn val = {(arr: Array[F64] val): F64 =>
        try
          var res = F64(0)
          for (x, y) in data.values() do
            let err = ((x - arr(0)?).pow(2) + (y - arr(1)?).pow(2)).sqrt() - arr(2)?
            res = res + (err * err)
          end
          res.sqrt()
        else env.out.print("Couldn't read element off cost input array"); 0 end
      }

    let promise = Promise[Array[F64] val]
    promise.next[None](_Fulfill(env.out))
    let minimizer = match method
      | 1 => GridSearch(
        3,
        recover val [as F64: -5; -5; 0] end,
        recover val [as F64: 5; 5; 5] end,
        fn,
        1000,
        5,
        3
      )
      else GradientDescent(
        recover val [as F64: 0; 0; 1] end, // cx, cy, r
        fn,
        100000, 0.0008, 0.0001
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
