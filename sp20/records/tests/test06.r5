
let v = 1 in
let w = 46 in
let x = v + 7 in
let y = x + 4 in
let z = x + w in
y + z

-- 1 (program(vwxyzt.1t.2)
-- 2 (movq (int 1) (var v))
-- 3 (movq (int 46) (var w))
-- 4 (movq (var v) (var x))
-- 5 (addq (int 7) (var x))
-- 6 (movq (var x) (var y))
-- 7 (addq (int 4) (var y))
-- 8 (movq (var x) (var z))
-- 9 (addq (var w) (var z))
-- 10 (movq (var y) (var t.1))
-- 11 (negq (var t.1))
-- 12 (movq (var z) (var t.2))
-- 13 (addq (var t.1) (var t.2))
-- 14 (movq (var t.2) (reg rax)))
-- 3. REGISTER ALLOCATION
-- {v} {v,w} {w, x} {w, x} {w, x, y} {w, x, y} {w, y, z} {y,z} {t.1, z} {t.1, z} {t.1, t.2} {t.2}
-- {}