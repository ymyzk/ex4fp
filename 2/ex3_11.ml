let makefact = fun maker -> fun n ->
  if n < 1 then 1 else n * maker maker (n + -1);;
let fact = fun n -> makefact makefact n in fact 10;;
