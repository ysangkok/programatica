#if defined(HBC)

#include "heap_HBC.h"

#elif defined(NHC)

#include "heap_NHC.h"

#else

#error "Unknown compiler selected"

#endif
