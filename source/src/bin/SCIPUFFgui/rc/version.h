
/////////////////////////////////////////////////////////////////////////////
//
// Version
//

VS_VERSION_INFO VERSIONINFO
 FILEVERSION 1,0,0,2
 PRODUCTVERSION 1,0,0,2
 FILEFLAGSMASK 0x17L
#ifdef _DEBUG
 FILEFLAGS 0x1L
#else
 FILEFLAGS 0x0L
#endif
 FILEOS 0x4L
 FILETYPE 0x0L
 FILESUBTYPE 0x0L
BEGIN
    BLOCK "StringFileInfo"
    BEGIN
        BLOCK "000004b0"
        BEGIN
            VALUE "CompanyName", "Sage Management"
            VALUE "Directives", "AEROSOL,CLIMO,DEGRADE,DENSE,DREAM,FXPLOT,HAZARD,HPAC,INTERACTIVE,LIQUID,MET_PLOT,MSS,MULT,NEST,NWPN,OPVIEW,POPLIB,PROCESS,RAD,REVERSE,SKEW,SWIFT,USEDLL,VSRFEVAP,WINPAC,WININI"
            VALUE "FileDescription", "Version"
            VALUE "FileVersion", "1, 1, 0, 2"
            VALUE "InternalName", "ARAPgui"
            VALUE "LegalCopyright", "Copyright (C) 2011"
            VALUE "OriginalFilename", "Version"
#ifdef NGIC
            VALUE "ProductName", "SCIPUFF.NGIC"
#else
#ifdef _WIN64
            VALUE "ProductName", "SCIPUFF.x64"
#else
            VALUE "ProductName", "SCIPUFF"
#endif
#endif
            VALUE "ProductVersion", "1, 1, 0, 2"
        END
    END
    BLOCK "VarFileInfo"
    BEGIN
        VALUE "Translation", 0x0, 1200
    END
END
