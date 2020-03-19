use "debug"
use "promises"
use "collections"

actor GradientDescent is Minimizer
  var guess: Array[F64] val
  let cost_fn: CostFn
  let epsilon: F64
  let gamma: F64
  let dimensions: USize
  let target: USize
  let workers: Array[_GDWorker]
  var received: USize = 0
  var prom: (Promise[Array[F64] val] | None) = None
  let costs: Array[F64] ref
  let derivatives: Array[F64] ref
  var is_calculating: Bool = false
  var steps_done: USize = 0

  new create(initial: Array[F64] val, cost_fn': CostFn, target': USize, epsilon': F64 = 1e-3, gamma': F64 = 5e-4) =>
    guess = initial
    cost_fn = cost_fn'
    epsilon = epsilon'
    gamma = gamma'
    target = target'
    dimensions = guess.size()
    workers = Array[_GDWorker](dimensions + 1)
    derivatives = Array[F64](dimensions)
    costs = Array[F64]((dimensions * 2) + 1)

    for i in Range[USize](0, dimensions + 1) do
      workers.push(_GDWorker(this, cost_fn, if i == 0 then 0 else i - 1 end))
    end

    for j in Range[USize](0, (dimensions * 2) + 1) do
      costs.push(0)
    end

    for k in Range[USize](0, dimensions) do
      derivatives.push(0)
    end

  be _receive_cost(cost: F64, index: USize) =>
    try
      costs(index)? = cost
    else
      Debug("Couldn't write to cost array: " + index.string() + " > " + costs.size().string())
      _reject()
      return
    end
    received = received + 1
    if received == costs.size() then
      step()
    end

  fun distribute() =>
    for (index, worker) in workers.pairs() do
      if index == 0 then
        worker(guess, None)
      else
        worker(guess, (index - 1, gamma))
      end
    end

  be minimize(prom': Promise[Array[F64] val]) =>
    if is_calculating then prom'.reject(); return end
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
      | (let p: Promise[Array[F64] val]) => p(guess)
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

    let res: Array[F64] trn = recover trn Array[F64](guess.size()) end
    for (index', value') in guess.pairs() do
      try
        res.push(value' + (derivatives(index')? * mult_ratio))
      else
        Debug("Couldn't read derivative array")
        _reject()
        return
      end
    end
    guess = consume res

    received = 0
    distribute()

  fun _reject() =>
    match prom
    | (let p: Promise[Array[F64] val]) => p.reject()
    end

actor _GDWorker
  let cost_fn: CostFn
  let index: USize
  let parent: GradientDescent tag

  new create(parent': GradientDescent tag, cost_fn': CostFn, index': USize) =>
    parent = parent'
    cost_fn = cost_fn'
    index = index'

  be apply(guess: Array[F64] val, shift: ((USize, F64) | None)) =>
    match shift
    | None =>
      parent._receive_cost(cost_fn(guess), index)
    | (let index': USize, let by: F64) =>
      parent._receive_cost(cost_fn(recover val
        let res = Array[F64](guess.size())
        for (i, value) in guess.pairs() do
          res.push(if \unlikely\ i == index' then value + by else value end)
        end
        res
      end), (index * 2) + 1)
      parent._receive_cost(cost_fn(recover val
        let res = Array[F64](guess.size())
        for (i, value) in guess.pairs() do
          res.push(if \unlikely\ i == index' then value - by else value end)
        end
        res
      end), (index * 2) + 2)
    end
