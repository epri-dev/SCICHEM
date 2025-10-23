#include <stdint.h>

#define CP_MAX_PROTECT 10
#define CP_MAX_RADII 23
#define PATH_MAXLENGTH 256
// PATH_MAXLENGTH must be greater than 92
#define GENSIZE 4 + PATH_MAXLENGTH / 4
#ifdef CHEM
#define MAX_MC 200
#endif

struct messageT {
  int iParm;
  int jParm;
  char aString[128];
  char bString[128];
  char cString[128];
  char routine[80];
};

struct releaseT {
  int padding;
  int type;
  int status;
  float tRel;
  double xRel;
  double yRel;
  float zRel;
  float horzUnc;
  float vertUnc;
  float pa;
  int relData[GENSIZE];
  char material[16];
  char relName[32];
  char relDisplay[48];
#ifdef CHEM
  int nMC;
  char MCname[MAX_MC][16];
  float MCmass[MAX_MC];
#endif
};

struct enviroT {
  float z;
  float pressure;
  float potentialTemp;
  float humidity;
  float windUComp;
  float windVComp;
  float windWComp;
};

struct environmentT {
  float sfcElevation;
  float sfcPressure;
  float mixingLayerHeight;
  struct enviroT samples[250];
  int nsamp;
};

struct updateRelT {
  int mode;
  float currentTime;
  float nextUpdate;
  struct releaseT release;
  struct environmentT environment;
};

struct updateRelMCT {
  int mode;
  float currentTime;
  float nextUpdate;
  struct releaseT release;
  struct environmentT environment;
  char MCfile[PATH_MAXLENGTH];
};

struct computeEffT {
  int incidentID;
  int effectID;
  int request;
  intptr_t addressIn;
  intptr_t addressOut;
};

struct char32T {
  char string[32];
};

struct casualtyPromptInitInT {
  int nProtect;
  struct char32T ProtTypes[CP_MAX_PROTECT + 1];
};

struct casualtyPromptInitOutT {
  int nWpn;
  float probFac[CP_MAX_RADII];
};

struct casualtyPrompt2DInitOutT {
  int nWpn;
};

struct casualtyPromptCompInT {
  int nWpn;
};

struct casualtyPromptCompOutT {
  float loc[3];
  float time;
  float pa;
  float cep;
  float rCasualty[CP_MAX_RADII][CP_MAX_PROTECT + 1];
  float rFatality[CP_MAX_RADII][CP_MAX_PROTECT + 1];
  float wrCasualty[CP_MAX_PROTECT + 1];
  float wrFatality[CP_MAX_PROTECT + 1];
};

struct casualtyPrompt2DCompOutT {
  float loc[3];
  float time;
  float pa;
  float cep;
  float yield;
  float hob;
};
