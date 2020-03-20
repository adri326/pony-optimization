use "promises"
use "debug"
use "collections"

// TODO: move to next step, end of the scan

actor GridSearch is Minimizer
  let _from: Array[F64] val
  let _to: Array[F64] val
  let _dimensions: USize
  let _delta: USize
  let _horde_size: USize
  let _steps: USize
  var _promise: (Promise[Array[F64] val] | None) = None
  let _cost_fn: CostFn
  var is_calculating: Bool = false
  let is_valid: Bool
  var workers_ready: USize = 0
  let _n_workers: USize
  let _workers: Array[_GridWorker] ref

  new create(
    dimensions: USize,
    from: Array[F64] val,
    to: Array[F64] val,
    cost_fn: CostFn,
    steps: USize,
    delta: USize = 3,
    horde_size: USize = 2
  ) =>
    """
      A grid-based method, inspired by the searches done to find people buried under avalanches.
      A set of `delta ^ horde_size` actors is dispatched to scan the range (from `from` to `to`).
      Each actor scans `delta ^ (dimensions - horde_size)` points in the range and reports to two other workers where it found the lowest cost value.
      Result sharing is done by ordering them in a cycle. Care is put as to let them disclose their results before the others found a result.
      Once all of the actors have agreed upon a minimum, the search range is narrowed around it and the actors search once again.
      After the last narrowing step, the first actor reports to the GridSearch actor the location of the minimum.

      The search may extend the initial range by a bit.

      * `dimensions` is the number of variables for the cost function. `from` and `to` need to have as length `dimensions`.
      * `steps` is the amount of narrowing steps that should be done.
      * `delta` is the number of grid points per dimension; increase it if your cost function has a lot of bumps. It should be kept at a minimum of 2.
      * `horde_size` is the number of grid dimensions to dispatch actors on; a horde size of 1 means that only `delta` actors will be created and they will scan `delta ^ (dimensions - 1)`.
      A horde size of 2 means that `delta ^ 2` will be created and each will scan `delta ^ (dimensions - 2)` locations.
    """
    _from = from
    _to = to
    _dimensions = dimensions
    _delta = delta
    _horde_size = horde_size
    _steps = steps
    _cost_fn = cost_fn
    is_valid = (from.size() == dimensions) and (to.size() == dimensions)
    if not is_valid then
      Debug("Invalid grid search parameter!")
    end
    _n_workers = _delta.f64().pow(_horde_size.f64()).usize()
    _workers = Array[_GridWorker](_n_workers)

  be minimize(promise: Promise[Array[F64] val]) =>
    if is_calculating or not is_valid then promise.reject(); return end
    is_calculating = true
    _promise = promise
    distribute()

  fun ref distribute() =>
    distribute_rec(recover val Array[USize](0) end, 0)
    Debug(_workers.size().string() + " workers.")
    for (i, worker) in _workers.pairs() do
      try
        worker.set_friend(if i == 0 then _workers(_workers.size() - 1)? else _workers(i - 1)? end)
        Debug("Paired worker #" + i.string())
      else
        Debug("Couldn't get worker #0")
        _reject()
        return
      end
    end

  fun ref distribute_rec(pos: Array[USize] val, n: USize) =>
    if n == _horde_size then
      _workers.push(_GridWorker(
        _from,
        _to,
        _cost_fn,
        _steps,
        _delta,
        pos,
        this,
        _workers.size()
      ))
    else
      for i in Range[USize](0, _delta) do
        let pos': Array[USize] trn = recover iso Array[USize](n + 1) end
        for p in pos.values() do
          pos'.push(p)
        end
        pos'.push(i)
        distribute_rec(consume pos', n + 1)
      end
    end

  be worker_ready() =>
    workers_ready = workers_ready + 1
    Debug("Workers ready: " + workers_ready.string() + "/" + _n_workers.string())
    if workers_ready == _n_workers then
      Debug("Starting them...")
      for worker in _workers.values() do
        worker.step()
      end
    end

  be _reject() =>
    match _promise
    | (let prom: Promise[Array[F64] val]) => prom.reject()
    end

  be _resolve(result: Array[F64] val) =>
    match _promise
    | (let prom: Promise[Array[F64] val]) => prom(result)
    end

actor _GridWorker
  var _from: Array[F64] val
  var _to: Array[F64] val
  let _dimensions: USize
  let _delta: USize
  let _steps: USize
  let _cost_fn: CostFn
  let _position: Array[USize] val
  let _parent: GridSearch
  let _cell_dim: USize
  let _n_workers: USize
  let _id: USize

  var friend: (_GridWorker | None) = None
  var my_minimum: (Array[F64] val, F64) = (recover val Array[F64](0) end, F64.max_value())
  var known_minimum: (Array[F64] val, F64) = (recover val Array[F64](0) end, F64.max_value())

  var points_done: USize = 0
  var should_forward: Bool = false
  var completed_steps: USize = 0
  var has_sent_own_score: Bool = false

  new create(
    from: Array[F64] val,
    to: Array[F64] val,
    cost_fn: CostFn,
    steps: USize,
    delta: USize,
    position: Array[USize] val, // starting position of the worker
    parent: GridSearch,
    id: USize
  ) =>
    _from = from
    _to = to
    _dimensions = from.size()
    _delta = delta
    _cell_dim = _delta.f64().pow((_dimensions - position.size()).f64()).usize()
    _n_workers = _delta.f64().pow(position.size().f64()).usize()
    _steps = steps
    _cost_fn = cost_fn
    _position = position
    _parent = parent

    _id = id

  be set_friend(friend': _GridWorker) =>
    friend = friend'
    _parent.worker_ready()

  be step() =>
    points_done = 0
    has_sent_own_score = false
    should_forward = false
    rec(Array[F64](0), 0)

  fun ref rec(arr: Array[F64] ref, n: USize) =>
    if n == (_dimensions - _position.size()) then
      _process( // generate a position array to be sent to _process
        let a = recover trn Array[F64](_dimensions) end
        for (index, value) in _position.pairs() do // position to coords
          try
            a.push(_from(index)?.f64()
              + (
                (value.f64() / _delta.f64()) * (_to(index)? - _from(index)?)
              )
            )
          else
            Debug("Couldn't read to/from array at index " + index.string())
            return
          end
        end
        for value' in arr.values() do
          a.push(value')
        end
        consume a
      )
    else
      for i in Range[USize](0, _delta) do
        let a = Array[F64](n + 1)
        arr.copy_to(a, 0, 0, arr.size())
        try
          a.push(
            _from(n + _position.size())?.f64()
            + (
              (i.f64() / _delta.f64())
              * (_to(n + _position.size())? - _from(n + _position.size())?)
            )
          )
        else
          Debug("Couldn't read/write at index " + (n + _position.size()).string())
          return
        end
        rec(a, n + 1)
      end
    end

  fun ref _process(pos: Array[F64] val) => // execute the cost function and triggers the step's end condition
    let res = _cost_fn(pos)

    if res < my_minimum._2 then
      my_minimum = (pos, res)
    end

    points_done = points_done + 1
    if points_done == _cell_dim then
      _end()
    end

  fun ref _end() =>
    match friend
    | (let friend': _GridWorker) =>
      if my_minimum._2 <= known_minimum._2 then
        friend'._forward(my_minimum, completed_steps)
        has_sent_own_score = true
      elseif should_forward then
        friend'._forward(known_minimum, completed_steps)
      end
    end

  be _forward(minimum: (Array[F64] val, F64), steps: USize) =>
    Debug("Forwarded: " + minimum._2.string() + " at step " + steps.string() + " (I'm at step " + completed_steps.string() + " and have as local minimum " + my_minimum._2.string() + ")")
    if minimum._2 <= known_minimum._2 then
      known_minimum = minimum
    else return end

    if (points_done == _cell_dim) and (completed_steps == steps) then // we're ready to forward
      if ArraysEq(minimum._1, my_minimum._1) then // we're the initial author
        Debug("- I'm the author! (" + _id.string() + ")")
        if completed_steps >= (_steps - 1) then
          _parent._resolve(my_minimum._1)
        else
          (_from, _to) = _init_next_step()
          match friend
          | (let friend': _GridWorker) =>
            completed_steps = completed_steps + 1
            friend'._next_step(_from, _to, completed_steps, _n_workers - 1)
            step()
            Debug("Initiated step " + completed_steps.string() + "! (" + _id.string() + ")")
          end
        end
      else
        Debug("- Sending " + minimum._2.min(my_minimum._2).string() + " to my friend...")
        match friend
        | (let friend': _GridWorker) =>
          if my_minimum._2 < minimum._2 then
            if not (has_sent_own_score = true) then
              friend'._forward(my_minimum, completed_steps)
            end
          else
            friend'._forward(known_minimum, completed_steps)
          end
        end
      end
    else // hold onto the value
      Debug("- and I wasn't done (" + points_done.string() + "/" + _cell_dim.string() + ")")
      should_forward = true
    end

  fun _init_next_step(): (Array[F64] val, Array[F64] val) => // (from, to)
    let dist = Array[F64](_dimensions)
    for (i, from) in _from.pairs() do
      let to = try _to(i)? else F64(0) end // shouldn't error out
      dist.push((to - from) / (_delta - 1).f64() / 2)
    end
    let nfrom = recover trn Array[F64](_dimensions) end
    let nto = recover trn Array[F64](_dimensions) end
    for (i', dist') in dist.pairs() do
      let center = try known_minimum._1(i')? else F64(0) end // shouldn't error out
      nfrom.push(center - dist')
      nto.push(center + dist')
    end
    (consume nfrom, consume nto)

  be _next_step(from: Array[F64] val, to: Array[F64] val, steps: USize, n: USize) =>
    if (n > 0) and (completed_steps != steps) then
      completed_steps = steps
      if completed_steps != steps then Debug("Desync! I'm at " + completed_steps.string() + " and must do " + steps.string() + ".") end
      _from = from
      _to = to
      match friend
      | (let friend': _GridWorker) =>
        friend'._next_step(from, to, steps, n - 1)
      end
      step()
    end

primitive ArraysEq
  fun apply(a: Array[F64] val, b: Array[F64] val): Bool =>
    for (i, x) in a.pairs() do
      try
        if x != b(i)? then return false end
      else return false end
    end
    true
