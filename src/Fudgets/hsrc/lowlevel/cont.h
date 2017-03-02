/* DECONSTR :: (*a<->*b) -> *b -> (Option *a) */
#define DECONSTR(Deconstr) (let e (Deconstr a) = Some a || e _ = None in e)
