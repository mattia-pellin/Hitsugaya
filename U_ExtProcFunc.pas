unit U_ExtProcFunc;

interface

uses Windows, Dialogs, SysUtils, StdCtrls, U_Classes;

type SWList = array of THitSoft;

function GetFileVer(sFileName:string): String;
function BuildSoftwareList(var LB_Soft: TListBox): SWList;
procedure CreateFreeDriveList(var CB_Drives: TComboBox);
procedure BuildCategoryList(var Software: SWList; var Categories: TComboBox);

implementation

// Get version of a File
function GetFileVer(sFileName:string): String;
var
  Dummy,
  VerInfoSize,
  VerValueSize: DWORD;
  VerInfo:      Pointer;
  VerValue:     PVSFixedFileInfo;
begin
  VerInfoSize := GetFileVersionInfoSize(PChar(sFileName), Dummy);
  GetMem(VerInfo, VerInfoSize);
  GetFileVersionInfo(PChar(sFileName), 0, VerInfoSize, VerInfo);
  VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
  with VerValue^ do
  begin
    Result := IntToStr(dwFileVersionMS shr 16);
    Result := Result + '.' + IntToStr(dwFileVersionMS and $FFFF);
    Result := Result + '.' + IntToStr(dwFileVersionLS shr 16);
    Result := Result + '.' + IntToStr(dwFileVersionLS and $FFFF);
  end;
  FreeMem(VerInfo, VerInfoSize);
end;

// Create available software list
function BuildSoftwareList(var LB_Soft: TListBox): SWList;
var
    Res:      TSearchRec;
    Test:     THitSoft;
begin
  SetLength(Result, 0);
  FindFirst('config\*.bat', faAnyFile, Res);

  repeat
    Test:= THitSoft.Create(Res.Name, Result);
    if Test.IsValid then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1]:= Test;
        LB_Soft.Items.Add(Test.Name);
      end
    else
      MessageDlg('Formato del file ' + Res.Name + ' invalido', mtWarning, [mbOK], 0);
  until FindNext(Res) <> 0;
  FindClose(Res);
end;

// Create available catagory list
procedure BuildCategoryList(var Software: SWList; var Categories: TComboBox);
var i: Integer;
begin
  Categories.Items.Add('Tutte le Categorie');

  for i := 0 to Length(Software) - 1 do
    if Categories.Items.IndexOf(Software[i].Category) = -1 then
      Categories.Items.Add(Software[i].Category);

  if Categories.Items.Count > 0 then
    Categories.ItemIndex:= 0;
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
