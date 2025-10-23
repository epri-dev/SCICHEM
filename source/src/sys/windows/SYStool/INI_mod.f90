MODULE INI
#if defined(WININI_OPT)
  USE WININI
#elif defined(FINI_OPT)
  USE FINI
#else
  USE CINI
#endif
END MODULE INI
