#include "callbackhandler.h"

// callback function pointer type
typedef int(callback_ptr)();

int callbackMessage(int (*callback)(), int* id, int* a, int* b, int flag) {
  switch (flag) {
    case 0:
      return (*callback)(*id, *a, *b);
    default:
      return (*callback)(*id, *a, b);
  }
}

#ifdef Linux
int callback_byreference_(callbackInt* callbackAddress, int* id, int* a,
                          int* b) {
  return callbackMessage((callback_ptr*)*callbackAddress, id, a, b, 1);
}
int callback_byvalue_(callbackInt* callbackAddress, int* id, int* a, int* b) {
  return callbackMessage((callback_ptr*)*callbackAddress, id, a, b, 0);
}
#else
//=======Duplicate function for different fortran compilers======
int __cdecl CALLBACK_BYREFERENCE(callbackInt* callbackAddress, int* id, int* a,
                                 int* b) {
  return callbackMessage((callback_ptr*)*callbackAddress, id, a, b, 1);
}

int __cdecl CALLBACK_BYVALUE(callbackInt* callbackAddress, int* id, int* a,
                             int* b) {
  return callbackMessage((callback_ptr*)*callbackAddress, id, a, b, 0);
}
#endif

//===============================================================
