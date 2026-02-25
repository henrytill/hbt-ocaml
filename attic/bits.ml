let popcount x =
  let x = x land 0xFFFFFFFF in
  let x = x - ((x lsr 1) land 0x55555555) in
  let x = (x land 0x33333333) + ((x lsr 2) land 0x33333333) in
  let x = (x + (x lsr 4)) land 0x0f0f0f0f in
  ((x * 0x01010101) lsr 24) land 0xFF
