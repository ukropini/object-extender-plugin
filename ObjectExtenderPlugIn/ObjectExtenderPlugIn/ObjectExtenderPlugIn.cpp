#include "stdafx.h"
// PLSQLFunctions.h get the whole API.
#include "PLSQLFunctions.h"
#include "ObjectExtenderPlugIn.h"
#include <fstream>

// Global variables
int siPlugInID;
int siAppHandle;
HWND siWindowHandle;

std::string gcTypeCreationSQL;
char* gcUserName;
char* gcPassword;
char* gcDatabase;
BOOL  gbConnected;
char gcConnectionString[100];
char gcConnDisplay[2000];
char gcVersionText[1000];

bool CanExtendType(char* TypeOwner, char* TypeName);

void CreateNewType(char* TypeOwner, char* TypeName);

// This function receives a Plug - In ID from PL / SQL Developer and should return a description for
// the Plug-In. The returned description should be unique for your Plug-In and will be
// displayed in the Plug-In configuration dialog. The ID identifies your Plug-In and can be
// used in other callback functions.
char* IdentifyPlugIn(int ID)
{
	siPlugInID = ID;
	return scDesc;
}

// This function allows you to display an about dialog. You can decide to display a dialog
// yourself (in which case you should return an empty text) or just return the about text.  In
// PL/SQL Developer 3.1 there is an about button in the Plug-In configuration dialog.
char *About()
{
	sprintf_s(gcVersionText, "Object type extender Plugin. Creates subtype based on selected supertype. Version %s\n", scVersion);
	return gcVersionText;
}

// The PlugIn name (if defined) will be used for online updates, and as name for command window
// PlugIn commands. If you want your PlugIn to be handled by online updates, please contact
// support.  If this function is not defined, the PlugInName will be the dll filename.
//
// Available in version 700
char *PlugInName()
{
	return scPlugInName;
}

// The subname will be added to the PlugInName. Possible values are ‘Trial’ or ‘Beta’.
// Available in version 700
char *PlugInSubName()
{
	return scPlugInSubName;
}

// Delphi function PlugInShortName: PChar The short name is specifically for command window
// PlugIn commands. This allows you to specify a name that can be entered quickly.
//
// Available in version 700
char *PlugInShortName()
{
	return scPlugInShortName;
}

// All Callback to setup the popup menu items
void InitPopupMenu() {
	// Called on Startup and Enabled in Config menu.
	IDE_CreatePopupItem(siPlugInID, siPopupExtend, "Extend supertype", "TYPE");
	IDE_CreatePopupItem(siPlugInID, siPopupExtend, "Extend supertype", "TYPE BODY");

}

// This function is called when the Plug-In is loaded into memory. You can use it to do some
// one-time initialization. PL/SQL Developer is not logged on yet and you can't use the
// callback functions, so you are limited in the things you can do.
void OnCreate() {
	// Create the array for the callback
	OnCreateInitPtrArray();
}

// This function will be called with an Index ranging from 1 to 99. For every Index you can
// return a string that creates a new menu-item in PL/SQL Developer.
char* CreateMenuItem(int nIndex) {
	int plsqlVersion = SYS_Version();

	if (plsqlVersion < 1200) {
		if (nIndex == siMenuExtend) { return "Extend"; }
	}
	else { // PL/SQL Developer 12 use a Ribbon and no longer menu.
		if (nIndex == siMenuExtend) { return "ITEM=Extend"; }
	}
	return "";
}

void LoadTypeCreationSQL() {

	std::string filePath(IDE_GetGeneralPref("PlugInDir"));

	filePath.append("ObjectExtenderPlugIn.sql");
	std::replace(filePath.begin(), filePath.end(), '\\', '/');

	std::ifstream is(filePath, std::ifstream::in);
	if (is) {

		std::stringstream typeCreationSQL;
		typeCreationSQL << is.rdbuf();
		gcTypeCreationSQL = typeCreationSQL.str();

		is.close();


	}
	else {
		char Msg[5000];
		sprintf(Msg, "File \"%s\" cannot be found!", filePath.c_str());
		MessageBox(NULL, Msg, scPlugInName, MB_ICONEXCLAMATION);
	}

	filePath.clear();

}

// OnActivate gets called after OnCreate. However, when OnActivate is called PL/SQL Developer
// and the Plug-In are fully initialized. This function is also called when the Plug-In is
// enabled in the configuration dialog. A good point to enable/disable menus.
void OnActivate() {
	IDE_DebugLog("Called: OnActivate()");
	// Get Handle on the App and the main window and store them
	siAppHandle = IDE_GetAppHandle();
	siWindowHandle = IDE_GetWindowHandle();
	InitPopupMenu();
	LoadTypeCreationSQL();
}

bool IsOwner(const std::string& OwnerToTest) {
	char* SQLQuery = "SELECT 1 ck FROM all_users WHERE username = UPPER(:owner)";
	bool bResult = false;

	char* owner;
	owner = (char *)malloc((strlen(OwnerToTest.c_str()) + 1) * sizeof(char));
	strcpy(owner, OwnerToTest.c_str());

	SQL_SetVariable("owner", owner);
	if (SQL_Execute(SQLQuery) == 0) {

		int idx = SQL_FieldIndex("ck");
		while (!SQL_Eof()) {

			bResult = (strcmp(SQL_Field(idx), "1") == 0) ? true : false; // expected result - "1" (look at SQLQuery)
			
			SQL_Next();
		}
	}
	SQL_ClearVariables();

	return (bResult);
}

std::string FindOwnerByTypeName(const std::string& TypeName) {
	IDE_DebugLog("Called: IsCorrectType()");

	char* SQLQuery = "SELECT owner FROM all_types WHERE type_name = UPPER(:type_name)";
	std::string result;

	char* typeName;
	typeName = (char *)malloc((strlen(TypeName.c_str()) + 1) * sizeof(char));
	strcpy(typeName, TypeName.c_str());

	SQL_SetVariable("type_name", typeName);
	if (SQL_Execute(SQLQuery) == 0) {

		int idx = SQL_FieldIndex("owner");
		while (!SQL_Eof()) {

			result = SQL_Field(idx);

			SQL_Next();
		}
	}
	SQL_ClearVariables();

	return (result);
}

bool IsCorrectType(const std::string& TypeOwner, const std::string& TypeName) {
	IDE_DebugLog("Called: IsCorrectType()");

	char* SQLQuery = "SELECT 1 ck FROM all_types WHERE owner = UPPER(:type_owner) AND type_name = UPPER(:type_name)";
	bool bResult = false;

	char* typeOwner = (char *)malloc((strlen(TypeOwner.c_str()) + 1) * sizeof(char));
	char* typeName = (char *)malloc((strlen(TypeName.c_str()) + 1) * sizeof(char));
	strcpy(typeOwner, TypeOwner.c_str());
	strcpy(typeName, TypeName.c_str());

	SQL_SetVariable("type_owner", typeOwner);
	SQL_SetVariable("type_name", typeName);
	if (SQL_Execute(SQLQuery) == 0) {

		int idx = SQL_FieldIndex("ck");
		while (!SQL_Eof()) {

			bResult = (strcmp(SQL_Field(idx), "1") == 0) ? true : false; // expected result - "1" (look at SQLQuery)

			SQL_Next();
		}
	}
	SQL_ClearVariables();

	return (bResult);
}

void FindObjectType(std::string& TypeOwner, std::string& TypeName) {
	IDE_DebugLog("Called: FindObjectType()");
	
	std::string typeOwner;
	std::string typeName;

	std::string cursorWord = IDE_GetCursorWord();
	if (!cursorWord.empty()) {
		std::transform(cursorWord.begin(), cursorWord.end(), cursorWord.begin(), ::toupper);
		std::size_t dotPos = cursorWord.find(".");
		if (dotPos > 0) {
			typeOwner = cursorWord.substr(0, dotPos);
			if (IsOwner(typeOwner)) {
				typeName = cursorWord.substr(dotPos + 1);
				dotPos = typeName.find(".");
				if (dotPos > 0) {
					typeName = typeName.substr(0, dotPos);
				}
			}
			else {
				typeName = typeOwner;
				typeOwner = FindOwnerByTypeName(typeName);
			}
		}
		else {
			typeName = cursorWord;
			typeOwner = FindOwnerByTypeName(typeName);
		}

		if (IsCorrectType(typeOwner, typeName)) {
			TypeOwner = typeOwner;
			TypeName = typeName;
		}
		else {
			char Msg[500];
			sprintf(Msg, "Cannot find \"%s.%s\" type!", typeOwner.c_str(), typeName.c_str());
			MessageBox(NULL, Msg, scPlugInName, MB_ICONEXCLAMATION);
		}

	}

}

// This function is called when a user selected a menu-item created with the CreateMenuItem
// function and the Index parameter has the value (1 to 99) it is related to.
void OnMenuClick(int nIndex) {
	IDE_DebugLog("Called: OnMenuClick()");

	char* cType;
	char* cOwner;
	char* cName;
	char* cSub;

	if (nIndex == siMenuExtend) {

		std::string typeOwner;
		std::string typeName;

		FindObjectType(typeOwner, typeName);

		if (!typeOwner.empty() && !typeName.empty()) {
			
			cOwner = (char *)malloc((strlen(typeOwner.c_str()) + 1) * sizeof(char));
			cName = (char *)malloc((strlen(typeName.c_str()) + 1) * sizeof(char));
			strcpy(cOwner, typeOwner.c_str());
			strcpy(cName, typeName.c_str());

			if (CanExtendType(cOwner, cName)) {
				CreateNewType(cOwner, cName);
			}
			else {
				MessageBox(NULL, "This type cannot be extended because it has been marked as FINAL.", scPlugInName, MB_ICONEXCLAMATION);
			}
		}

	}
	else if (nIndex == siPopupExtend) {

		IDE_GetPopupObject(&cType, &cOwner, &cName, &cSub);

		if (CanExtendType(cOwner, cName)) {
			CreateNewType(cOwner, cName);
		}
		else {
			MessageBox(NULL, "This type cannot be extended because it has been marked as FINAL.", scPlugInName, MB_ICONEXCLAMATION);
		}

	}

}

bool CanExtendType(char* TypeOwner, char* TypeName) {
	IDE_DebugLog("Called: CanExtendType()");

	// only not final objects can be extended
	char* SQLQuery = "SELECT 1 ck FROM all_types t WHERE t.owner = UPPER(:type_owner) AND t.type_name = UPPER(:type_name) AND t.final = 'NO' AND t.typecode = 'OBJECT'";
	bool bResult = false;
	int iSQLResult;

	SQL_SetVariable("type_owner", TypeOwner);
	SQL_SetVariable("type_name", TypeName);
	iSQLResult = SQL_Execute(SQLQuery);
	SQL_ClearVariables();

	if (iSQLResult == 0) {

		int idx = SQL_FieldIndex("ck");
		while (!SQL_Eof()) {

			bResult = (strcmp(SQL_Field(idx), "1") == 0) ? true : false; // expected result - "1" (look at SQLQuery)

			SQL_Next();
		}
	}

	return (bResult);
}

void CreateNewType(char* TypeOwner, char* TypeName) {
	IDE_DebugLog("Called: CreateNewType()");

	if(!gcTypeCreationSQL.empty()){


		SQL_SetVariable("type_owner", TypeOwner);
		SQL_SetVariable("type_name", TypeName);

		char* SQL = (char *)malloc((strlen(gcTypeCreationSQL.c_str()) + 1) * sizeof(char));
		strcpy(SQL, gcTypeCreationSQL.c_str());

		int iSQLResult = SQL_Execute(SQL);

		if (iSQLResult == 0) {

			char* res = SQL_GetDBMSGetOutput();

			IDE_CreateWindow(PLSQL_WT_PROCEDURE, res, false);

			IDE_BeautifyWindow();

			delete[] res;
		}
		else {
			char Msg[5000];
			sprintf(Msg, "Error executing SQL: \"%s\"", SQL_ErrorMessage());
			MessageBox(NULL, Msg, scPlugInName, MB_ICONEXCLAMATION);
		}

		SQL_ClearVariables();

		delete[] SQL;
	}

}