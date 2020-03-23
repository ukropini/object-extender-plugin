#pragma once

#if !defined(DEMOPLUGIN_H_INCLUDED_)
#define DEMOPLUGIN_H_INCLUDED_

char *const scPlugInName = "Object Extender";
char *const scPlugInShortName = "Object Extender";

#ifdef _WIN64
char *const scDesc = "Object Extender";
char *const scPlugInSubName = " v1.0 - 64 bits";
#else
char *const scDesc = "Object Extender";
char *const scPlugInSubName = " v1.0 - 32 bits";
#endif
extern int siPlugInID;
extern int siAppHandle;
extern HWND siWindowHandle;

// version
char *const scVersion = "1.0.0";

extern char* gcUserName;
extern char* gcPassword;
extern char* gcDatabase;
extern BOOL  gbConnected;
extern char gcConnectionString[100];
extern char gcConnDisplay[2000];
extern char gcVersionText[1000];

#endif // !defined(DEMOPLUGIN_H_INCLUDED_)

int const siMenuExtend = 1;
int const siPopupExtend = 2;
