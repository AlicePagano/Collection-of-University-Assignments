! Default preprocessing options:

#define Z 1
#define D 2

#ifndef ELEM
#   define ELEM Z
#endif

! Matrix elements type
#if ELEM == D
#   define ELEM_TYPE real(kind=DM_ELEM_KIND)
#elif ELEM == Z
#   define ELEM_TYPE complex(kind=ZM_ELEM_KIND)
#endif
