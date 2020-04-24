# Datatype Sketches 

    Updates to L4 in src/Lang/L4

# Interpreter Sketch

    Update of interpreter from hw05 -> src/FP

# Example Program Sketches (including given)

    let p = (1+1, 2+2) in
    fst p * fst p + snd p
    = 8

    let p = (1+1, 2+2) in
    let p2 = (3, 5) in
    fst p2 * fst p + snd p2
    = 11

    let tu1 = left 4 in
    let tu2 = right false in
    let r1 = case tu1 {left x ⇒ x * x} {right x ⇒ if x then 1 else 2} in
    let r2 = case tu2 {left x ⇒ x * x} {right x ⇒ if x then 1 else 2} in
    r1 + r2
    = 18

    let tu1 = right false in
    let tu2 = left 3 in
    let tu3 = left true in
    let r1 = case tu1 {left x ⇒ x * 10} {right x ⇒ if x then 1 else 0} in
    let r2 = case tu2 {left x ⇒ x * x} {right x ⇒ if x then 1 else 2} in
    let r3 = case tu3 {left x ⇒ if x then 4 else 0} {right x ⇒ if x then 1 else 2} in

    r1 * r2 - r3
    = -4



