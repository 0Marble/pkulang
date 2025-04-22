type heap_ptr = { idx : int }
type value = Number of int | Pointer of heap_ptr

let null = Pointer { idx = 0 }
