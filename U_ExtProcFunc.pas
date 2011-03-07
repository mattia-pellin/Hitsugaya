unit U_ExtProcFunc;

interface

uses Windows, Dialogs, SysUtils, StdCtrls, U_Classes;

type
  SWList = array of THitSoft;
  WinIsWow64 = function(Handle: THandle; var Iret: BOOL): Windows.BOOL; stdcall;

function GetExBits: string;
function BuildSoftwareList(var LB_Soft: TListBox): SWList;
procedure UpdateLoading(Step: String);
procedure CreateFreeDriveList(var CB_Drives: TComboBox);
procedure BuildCategoryList(var Software: SWList; var Categories: TComboBox);

implementation

uses U_Loading;

// Update loading progress
procedure UpdateLoading(Step: string);
begin
  F_Loading.L_Loading.Caption:= Step;
  F_Loading.PB_Loading.Position:= 0;
  F_Loading.Refresh;
end;

// Check if the OS is x86 or x64
function GetExBits: string;
var
  HandleTo64BitsProcess: WinIsWow64;
  Iret                 : Windows.BOOL;
begin
  Result := 'x86';
  HandleTo64BitsProcess := GetProcAddress(GetModuleHandle('kernel32.dll'), 'IsWow64Process');
  if Assigned(HandleTo64BitsProcess) then
  begin
    if not HandleTo64BitsProcess(GetCurrentProcess, Iret) then
      Raise Exception.Create('Invalid handle');
    if Iret then
      Result := 'x64';
  end;
end;

// Returns a counter for specified files type and directory
function GetFilesCount(Folder, WildCard: string): Word;
var
  intFound: Integer;
  SearchRec: TSearchRec;
begin
  Result := 0;
  if (Folder <> '') and (Folder[Length(Folder)] <> '\') then
    Folder := Folder + '\';
  intFound := FindFirst(Folder + WildCard, faAnyFile, SearchRec);
  while (intFound = 0) do
  begin
    if not (SearchRec.Attr and faDirectory = faDirectory) then
      Inc(Result);
    intFound := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

// Create available software list
function BuildSoftwareList(var LB_Soft: TListBox): SWList;
var
    Res:      TSearchRec;
    Test:     THitSoft;
    Step:     Byte;
begin
  SetLength(Result, 0);
  LB_Soft.Items.Clear;
  Step:= 100 div GetFilesCount('config\', '*.bat');
  FindFirst('config\*.bat', faAnyFile, Res);

  repeat
    Test:= THitSoft.Create(Res.Name, Result);
    if Test.IsValid then
      begin
        if (Pos(GetExBits, Test.Name) <> 0) or
          ((Pos('x86', Test.Name) = 0) and (Pos('x64', Test.Name) = 0)) then
        begin
          SetLength(Result, Length(Result) + 1);
          Result[Length(Result) - 1]:= Test;
          LB_Soft.Items.Add(Test.Name);
        end;
      end
    else
      MessageDlg('Formato del file ' + Res.Name + ' invalido', mtWarning, [mbOK], 0);
    F_Loading.PB_Loading.Position:= F_Loading.PB_Loading.Position + Step;
  until FindNext(Res) <> 0;
  FindClose(Res);

  F_Loading.PB_Loading.Position:= 100;
end;

// Create available catagory list
procedure BuildCategoryList(var Software: SWList; var Categories: TComboBox);
var i:      SmallInt;
    Step:   Byte;
begin
  Step:= 100 div (Length(Software) + 1);

  Categories.Items.Clear;
  Categories.Items.Add('Tutte le Categorie');
  F_Loading.PB_Loading.Position:= F_Loading.PB_Loading.Position + Step;

  for i := 0 to Length(Software) - 1 do
    begin
      if Categories.Items.IndexOf(Software[i].Category) = -1 then
        Categories.Items.Add(Software[i].Category);
      F_Loading.PB_Loading.Position:= F_Loading.PB_Loading.Position + Step;
    end;

  if Categories.Items.Count > 0 then
    Categories.ItemIndex:= 0;

  F_Loading.PB_Loading.Position:= 100;
end;

// Put free drive list into ComboBox
procedure CreateFreeDriveList(var CB_Drives: TComboBox);
var
    i,j:      Integer;
    Found:    Boolean;
    Drives:   array[0..128] of Char;
    uDrive:   array of Char;
    pDrive:   PChar;
begin
  SetLength(uDrive, 0);
  i := GetLogicalDriveStrings(SizeOf(Drives), Drives);
  if i <> 0 then
  begin
    if i > SizeOf(Drives) then
      raise Exception.Create(SysErrorMessage(ERROR_OUTOFMEMORY));
    pDrive := Drives;
    while pDrive^ <> #0 do
    begin
      SetLength(uDrive, Length(uDrive) + 1);
      uDrive[Length(uDrive) - 1]:= pDrive[0];
      inc(pDrive, 4);
    end;
  end;

  for i := 65 to 90 do
  begin
    Found:= False;
    for j := 0 to Length(uDrive) - 1 do
      if Chr(i) = uDrive[j] then
      begin
        Found:= True;
        break;
      end;
    if not(Found) then
      CB_Drives.Items.Add(Chr(i) + ':');
  end;

  if CB_Drives.Items.Count > 0 then
    CB_Drives.ItemIndex:= CB_Drives.Items.Count - 1;
end;

end.
