
let hash a = 
  let b = (16807 * (a mod 127773)) - 2836 * (a mod 127773) in
  if b > 0 then b else b + 2147483647
