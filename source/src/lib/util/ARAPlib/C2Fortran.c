#include "C2Fortran.h"

//=======Duplicate function for different fortran compilers======
void value_reference_(int in, int *out) {
  *out = in;
  return;
}

void VALUE_REFERENCE(int in, int *out) {
  value_reference_(in, out);
  return;
}
//=======Duplicate function for different fortran compilers======
void address_integer_(int **in, int *out) {
  *out = **in;
  return;
}

void ADDRESS_INTEGER(int **in, int *out) {
  address_integer_(in, out);
  return;
}
//===============================================================
void address_real_(float **in, float *out) {
  *out = **in;
  return;
}

void ADDRESS_REAL(float **in, float *out) {
  address_real_(in, out);
  return;
}
//=======Duplicate function for different fortran compilers======
void address_message_(struct messageT **in, struct messageT *out) {
  *out = **in;
  return;
}

void ADDRESS_MESSAGE(struct messageT **in, struct messageT *out) {
  address_message_(in, out);
  return;
}
//===============================================================

void ADDRESS_RELEASE(struct releaseT **in, struct releaseT *out) {
  *out = **in;
  return;
}

//=======Duplicate function for different fortran compilers======
void address_update_(struct updateRelT **in, struct updateRelT *out) {
  *out = **in;
  return;
}

void ADDRESS_UPDATE(struct updateRelT **in, struct updateRelT *out) {
  address_update_(in, out);
  return;
}

//=======Duplicate function for different fortran compilers======
void address_updatemc_(struct updateRelMCT **in, struct updateRelMCT *out) {
  *out = **in;
  return;
}

void ADDRESS_UPDATEMC(struct updateRelMCT **in, struct updateRelMCT *out) {
  address_updatemc_(in, out);
  return;
}

//=======Duplicate function for different fortran compilers======
void address_effect_(struct computeEffT **in, struct computeEffT *out) {
  *out = **in;
  return;
}

void ADDRESS_EFFECT(struct computeEffT **in, struct computeEffT *out) {
  address_effect_(in, out);
  return;
}

//=======Duplicate function for different fortran compilers======
void address_promptinitin_(struct casualtyPromptInitInT **in,
                           struct casualtyPromptInitInT *out) {
  *out = **in;
  return;
}

void ADDRESS_PROMPTINITIN(struct casualtyPromptInitInT **in,
                          struct casualtyPromptInitInT *out) {
  address_promptinitin_(in, out);
  return;
}

//=======Duplicate function for different fortran compilers======
void address_promptinitout(struct casualtyPromptInitOutT **in,
                           struct casualtyPromptInitOutT *out) {
  *out = **in;
  return;
}

void ADDRESS_PROMPTINITOUT(struct casualtyPromptInitOutT **in,
                           struct casualtyPromptInitOutT *out) {
  address_promptinitout(in, out);
  return;
}

//=======Duplicate function for different fortran compilers======
void address_prompt2Dinitout(struct casualtyPrompt2DInitOutT **in,
                             struct casualtyPrompt2DInitOutT *out) {
  *out = **in;
  return;
}

void ADDRESS_PROMPT2DINITOUT(struct casualtyPrompt2DInitOutT **in,
                             struct casualtyPrompt2DInitOutT *out) {
  address_prompt2Dinitout(in, out);
  return;
}

//=======Duplicate function for different fortran compilers======
void address_promptcompin_(struct casualtyPromptCompInT **in,
                           struct casualtyPromptCompInT *out) {
  *out = **in;
  return;
}

void ADDRESS_PROMPTCOMPIN(struct casualtyPromptCompInT **in,
                          struct casualtyPromptCompInT *out) {
  address_promptcompin_(in, out);
  return;
}

//=======Duplicate function for different fortran compilers======
void address_promptcompout(struct casualtyPromptCompOutT **in,
                           struct casualtyPromptCompOutT *out) {
  *out = **in;
  return;
}

void ADDRESS_PROMPTCOMPOUT(struct casualtyPromptCompOutT **in,
                           struct casualtyPromptCompOutT *out) {
  address_promptcompout(in, out);
  return;
}

//=======Duplicate function for different fortran compilers======
void address_prompt2Dcompout(struct casualtyPrompt2DCompOutT **in,
                             struct casualtyPrompt2DCompOutT *out) {
  *out = **in;
  return;
}

void ADDRESS_PROMPT2DCOMPOUT(struct casualtyPrompt2DCompOutT **in,
                             struct casualtyPrompt2DCompOutT *out) {
  address_prompt2Dcompout(in, out);
  return;
}

//=======Duplicate function for different fortran compilers======
void address_promptcompoutarray(struct casualtyPromptCompOutT **in,
                                struct casualtyPromptCompOutT *out, int *n) {
  int i;
  for (i = 0; i < *n; i++) {
    *(out + i) = *(*in + i);
  }
  return;
}

void ADDRESS_PROMPTCOMPOUTARRAY(struct casualtyPromptCompOutT **in,
                                struct casualtyPromptCompOutT *out, int *n) {
  address_promptcompoutarray(in, out, n);
  return;
}

//=======Duplicate function for different fortran compilers======
void address_prompt2Dcompoutarray(struct casualtyPrompt2DCompOutT **in,
                                  struct casualtyPrompt2DCompOutT *out,
                                  int *n) {
  int i;
  for (i = 0; i < *n; i++) {
    *(out + i) = *(*in + i);
  }
  return;
}

void ADDRESS_PROMPT2DCOMPOUTARRAY(struct casualtyPrompt2DCompOutT **in,
                                  struct casualtyPrompt2DCompOutT *out,
                                  int *n) {
  address_prompt2Dcompoutarray(in, out, n);
  return;
}

void ADDRESS_REALARRAY(float **in, float *out, int *n) {
  int i;
  for (i = 0; i < *n; i++) {
    *(out + i) = *(*in + i);
  }
  return;
}

//====Special call for Fortran strings. NB last argument is implicit string
//length added by compiler
void address_string_(char **in, char *out, int n) {
  int i;
  for (i = 0; i < n; i++) {
    *(out + i) = *(*in + i);
    if (*(out + i) == 0) {
      return;
    }
  }
  return;
}

void ADDRESS_STRING(char **in, char *out, int n) {
  address_string_(in, out, n);
  return;
}
//=======Duplicate function for different fortran compilers======
void address_integerarray_(int **in, int *out, int *n) {
  int i;
  for (i = 0; i < *n; i++) {
    *(out + i) = *(*in + i);
  }
  return;
}

void ADDRESS_INTEGERARRAY(int **in, int *out, int *n) {
  address_integerarray_(in, out, n);
  return;
}
//======================================================
void INTEGER_ADDRESS(int *in, int **out) {
  **out = *in;
  return;
}

void REAL_ADDRESS(float *in, float **out) {
  **out = *in;
  return;
}

void MESSAGE_ADDRESS(struct messageT *in, struct messageT **out) {
  **out = *in;
  return;
}

void RELEASE_ADDRESS(struct releaseT *in, struct releaseT **out) {
  **out = *in;
  return;
}

//=======Duplicate function for different fortran compilers======
void update_address_(struct updateRelT *in, struct updateRelT **out) {
  **out = *in;
  return;
}

void UPDATE_ADDRESS(struct updateRelT *in, struct updateRelT **out) {
  update_address_(in, out);
  return;
}

//=======Duplicate function for different fortran compilers======
void updatemc_address_(struct updateRelMCT *in, struct updateRelMCT **out) {
  **out = *in;
  return;
}

void UPDATEMC_ADDRESS(struct updateRelMCT *in, struct updateRelMCT **out) {
  updatemc_address_(in, out);
  return;
}

//===============================================================
void EFFECT_ADDRESS(struct computeEffT *in, struct computeEffT **out) {
  **out = *in;
  return;
}

//=======Duplicate function for different fortran compilers======
void promptinitin_address(struct casualtyPromptInitInT *in,
                          struct casualtyPromptInitInT **out) {
  **out = *in;
  return;
}

void PROMPTINITIN_ADDRESS(struct casualtyPromptInitInT *in,
                          struct casualtyPromptInitInT **out) {
  promptinitin_address(in, out);
  return;
}

//=======Duplicate function for different fortran compilers======
void promptinitout_address_(struct casualtyPromptInitOutT *in,
                            struct casualtyPromptInitOutT **out) {
  **out = *in;
  return;
}

void PROMPTINITOUT_ADDRESS(struct casualtyPromptInitOutT *in,
                           struct casualtyPromptInitOutT **out) {
  promptinitout_address_(in, out);
  return;
}

//=======Duplicate function for different fortran compilers======
void prompt2dinitout_address_(struct casualtyPrompt2DInitOutT *in,
                              struct casualtyPrompt2DInitOutT **out) {
  **out = *in;
  return;
}

void PROMPT2DINITOUT_ADDRESS(struct casualtyPrompt2DInitOutT *in,
                             struct casualtyPrompt2DInitOutT **out) {
  prompt2dinitout_address_(in, out);
  return;
}

//=======Duplicate function for different fortran compilers======
void promptcompin_address(struct casualtyPromptCompInT *in,
                          struct casualtyPromptCompInT **out) {
  **out = *in;
  return;
}

void PROMPTCOMPIN_ADDRESS(struct casualtyPromptCompInT *in,
                          struct casualtyPromptCompInT **out) {
  promptcompin_address(in, out);
  return;
}

//=======Duplicate function for different fortran compilers======
void promptcompout_address(struct casualtyPromptCompOutT *in,
                           struct casualtyPromptCompOutT **out) {
  **out = *in;
  return;
}

void PROMPTCOMPOUT_ADDRESS(struct casualtyPromptCompOutT *in,
                           struct casualtyPromptCompOutT **out) {
  promptcompout_address(in, out);
  return;
}

//=======Duplicate function for different fortran compilers======
void prompt2Dcompout_address(struct casualtyPrompt2DCompOutT *in,
                             struct casualtyPrompt2DCompOutT **out) {
  **out = *in;
  return;
}

void PROMPT2DCOMPOUT_ADDRESS(struct casualtyPrompt2DCompOutT *in,
                             struct casualtyPrompt2DCompOutT **out) {
  prompt2Dcompout_address(in, out);
  return;
}

//=======Duplicate function for different fortran compilers======
void promptcompoutarray_address_(struct casualtyPromptCompOutT *in,
                                 struct casualtyPromptCompOutT **out, int *n) {
  int i;
  for (i = 0; i < *n; i++) {
    *(*out + i) = *(in + i);
  }
  return;
}

void PROMPTCOMPOUTARRAY_ADDRESS(struct casualtyPromptCompOutT *in,
                                struct casualtyPromptCompOutT **out, int *n) {
  promptcompoutarray_address_(in, out, n);
  return;
}

//=======Duplicate function for different fortran compilers======
void prompt2dcompoutarray_address_(struct casualtyPrompt2DCompOutT *in,
                                   struct casualtyPrompt2DCompOutT **out,
                                   int *n) {
  int i;
  for (i = 0; i < *n; i++) {
    *(*out + i) = *(in + i);
  }
  return;
}

void PROMPT2DCOMPOUTARRAY_ADDRESS(struct casualtyPrompt2DCompOutT *in,
                                  struct casualtyPrompt2DCompOutT **out,
                                  int *n) {
  prompt2dcompoutarray_address_(in, out, n);
  return;
}

//===============================================================

void REALARRAY_ADDRESS(float *in, float **out, int *n) {
  int i;
  for (i = 0; i < *n; i++) {
    *(*out + i) = *(in + i);
  }
  return;
}

//=======Duplicate function for different fortran compilers======
void integerarray_address_(int *in, int **out, int *n) {
  int i;
  for (i = 0; i < *n; i++) {
    *(*out + i) = *(in + i);
  }
  return;
}

void INTEGERARRAY_ADDRESS(int *in, int **out, int *n) {
  integerarray_address_(in, out, n);
  return;
}
//=======Duplicate function for different fortran compilers======
void *ADDRESSOF(void *item) { return item; }

void *addressof_(void *item) { return item; }
//===============================================================
