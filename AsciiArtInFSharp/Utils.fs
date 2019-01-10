module Utils


#if DEBUG
let (|>) value func =
  let result = func value
  result
#endif

let modulo m n = ((n % m) + m) % m

let rec generateSequence succ state length = if length > 0 then state::(generateSequence succ (succ state)) (length-1) else []
