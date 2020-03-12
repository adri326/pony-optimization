use "debug"
use "promises"
use "collections"

actor GradientDescent is Minimizer
  let guess: Array[F64] ref
  let cost_fn: CostFn
  let epsilon: F64
  let gamma: F64
  let dimensions: USize
  let target: USize
  let workers: Array[_Worker]
  var received: USize = 0
  var prom: (Promise[Array[F64] val] | None) = None
  let costs: Array[F64] ref
  let derivatives: Array[F64] ref
  var is_calculating: Bool = false
  var steps_done: USize = 0

  new create(initial: Array[F64] val, cost_fn': CostFn, target': USize, epsilon': F64 = 1e-3, gamma': F64 = 5e-4) =>
    guess = initial.clone()
    cost_fn = cost_fn'
    epsilon = epsilon'
    gamma = gamma'
    target = target'
    dimensions = guess.size()
    workers = Array[_Worker]((dimensions * 2) + 1)
    derivatives = Array[F64](dimensions)
    costs = Array[F64](workers.size())
    for i in Range[USize](0, (dimensions * 2) + 1) do
      workers.push(_Worker(this, cost_fn, i))
      costs.push(0)
      derivatives.push(0)
    end

  fun shift(index: USize, by: I32): Array[F64] iso^ =>
    let arr: Array[F64] iso = recover iso Array[F64](dimensions) end
    for (index', value) in guess.pairs() do
      arr.push(
        if index' == index then
          value + (epsilon * by.f64())
        else
          value
        end
      )
    end
    consume arr

  be _receive_cost(cost: F64, index: USize) =>
    try
      costs(index)? = cost
    else
      Debug("Couldn't write to cost array")
      _reject()
      return
    end
    received = received + 1
    if received == workers.size() then
      step()
    end

  fun distribute() =>
    for (index, worker) in workers.pairs() do
      let arr = if index == 0 then shift(0, 0)
        else
          shift(
            (index - 1) / 2,
            if (index % 2) == 0 then -1 else 1 end
          )
        end
      worker(consume arr)
    end

  be minimize(prom': Promise[Array[F64] val]) =>
    if is_calculating then prom'.reject() end
    prom = prom'
    received = 0
    distribute()

  fun box first_derivative(index: USize): F64 =>
    try
      (costs((index * 2) + 2)? - costs((index * 2) + 1)?) / 2 / epsilon
    else
      Debug("Couldn't calculate derivative")
      _reject()
      0
    end

  fun ref step() =>
    steps_done = steps_done + 1
    if steps_done >= target then
      match prom
      | (let p: Promise[Array[F64] val]) => p(shift(0, 0))
      end
      return
    end

    var sum = F64(0)

    for (index, value) in guess.pairs() do
      try
        let d = first_derivative(index)
        derivatives(index)? = d
        sum = sum + (d * d)
      else
        Debug("Couldn't write to derivatives array")
        _reject()
        return
      end
    end

    let mult_ratio = gamma / sum.sqrt()

    for (index', value') in guess.pairs() do
      try
        guess(index')? = value' + (derivatives(index')? * mult_ratio)
      else
        Debug("Couldn't write to guess array")
        _reject()
        return
      end
    end

    received = 0
    distribute()

  fun _reject() =>
    match prom
    | (let p: Promise[Array[F64] val]) => p.reject()
    end

actor _Worker
  let cost_fn: CostFn
  let index: USize
  let parent: Minimizer tag

  new create(parent': Minimizer tag, cost_fn': CostFn, index': USize) =>
    parent = parent'
    cost_fn = cost_fn'
    index = index'

  be apply(input: Array[F64] iso) =>
    let res = cost_fn(consume input)
    // Debug(index.string() + " - " + res.string())
    parent._receive_cost(res, index)
