unit U_Hitsugaya;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, U_Classes, Buttons, ShellApi,
  ImgList, XMLDoc, XMLIntf, jpeg;

type
    TF_Hitsugaya = class(TForm)
    P_Spacer: TPanel;
    I_Logo: TImage;
    P_Mapping: TPanel;
    E_Path: TEdit;
    CB_Mapping: TCheckBox;
    L_Software: TListBox;
    P_Status: TPanel;
    L_Candidates: TListBox;
    B_Add: TBitBtn;
    B_Remove: TBitBtn;
    B_Info: TBitBtn;
    B_Up: TBitBtn;
    B_Down: TBitBtn;
    I_Check1: TImage;
    L_Status1: TLabel;
    I_Check2: TImage;
    L_Status2: TLabel;
    B_Start: TButton;
    CB_Drive: TComboBox;
    IL_Hitsugaya: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure B_AddClick(Sender: TObject);
    procedure B_RemoveClick(Sender: TObject);
    procedure L_SoftwareKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure L_CandidatesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure B_UpClick(Sender: TObject);
    procedure L_CandidatesClick(Sender: TObject);
    procedure B_DownClick(Sender: TObject);
    procedure B_InfoClick(Sender: TObject);
    procedure L_SoftwareClick(Sender: TObject);
    procedure B_StartClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  F_Hitsugaya: TF_Hitsugaya;
  SwList:      array of THitSoft;

implementation

{$R *.dfm}

// PROCEDURES & FUNCTIONS
// -----------------------------------------------------------------------------

function GetVersion(sFileName:string): string;
var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
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

procedure LoadIcons();
begin
  with F_Hitsugaya do
  begin
    IL_Hitsugaya.GetBitmap(0, B_Add.Glyph);
    IL_Hitsugaya.GetBitmap(1, B_Remove.Glyph);
    IL_Hitsugaya.GetBitmap(2, B_Up.Glyph);
    IL_Hitsugaya.GetBitmap(3, B_Down.Glyph);
    IL_Hitsugaya.GetBitmap(4, B_Info.Glyph);

    IL_Hitsugaya.GetIcon(4, I_Check1.Picture.Icon);
    IL_Hitsugaya.GetIcon(4, I_Check2.Picture.Icon);
  end;
end;

procedure CreateFreeDriveList();
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
      F_Hitsugaya.CB_Drive.Items.Add(Chr(i) + ':');
  end;

  if F_Hitsugaya.CB_Drive.Items.Count > 0 then
    F_Hitsugaya.CB_Drive.ItemIndex:= F_Hitsugaya.CB_Drive.Items.Count - 1;
end;

procedure BuildSoftwareList();
var
    i,j:      Word;
    Res:      TSearchRec;
    Test:     THitSoft;
    Found,
    ExtName:  Boolean;
begin
  i:= 0;
  Found:= False;
  ExtName:= False;
  SetLength(SwList, 0);
  FindFirst('config\*.bat', faAnyFile, Res);

  while FindNext(Res) = 0 do
  begin
    // Checking if already existing
    Test:= THitSoft.Create(Res.Name);
    for j := 0 to Length(SwList) - 1 do
    begin
      if SwList[j].Name = Test.Name then
        if SwList[j].Name = Copy(Res.Name, 1, Length(Res.Name) - 4) then
          begin
            Found:= True;
            break;
          end
        else
          begin
            ExtName:= True;
            Found:= True;
            break;
          end;
    end;
    // If custom name is busy, then use file name instead
    if not(Found) then
      begin
        SetLength(SwList, Length(SwList) + 1);
        SwList[i]:= Test;
      end
    else if ExtName then
      begin
        SetLength(SwList, Length(SwList) + 1);
        SwList[i]:= Test;
        SwList[i].Name:= Copy(Res.Name, 1, Length(Res.Name) - 4);
      end;

    F_Hitsugaya.L_Software.Items.Add(SwList[i].Name);
    inc(i);
  end;
  FindClose(Res);
end;

procedure SaveCurrentConfig(const FileName: String);
var
    Config:             TXmlDocument;
    MainNode,           // Hitsugaya
      MappingNode,      // Mapping
        PathNode,       // Executing Path
      SoftwareNode,
        CandidatesNode: IXMLNode;
begin
  // Save current config into XML file
  Config:= TXMLDocument.Create(nil);
  Config.Active:= True;

  MainNode:= Config.AddChild('program');
    MappingNode:= MainNode.AddChild('mapping');

    if F_Hitsugaya.CB_Mapping.Checked then
      MappingNode.Attributes['active']:= true
    else
      MappingNode.Attributes['active']:= false;

    MappingNode.Attributes['drive']:= F_Hitsugaya.CB_Drive.Items[F_Hitsugaya.CB_Drive.ItemIndex];
      PathNode:= MappingNode.AddChild('path');
      PathNode.Text:= F_Hitsugaya.E_Path.Text;

    SoftwareNode:= MainNode.AddChild('software');
      CandidatesNode:= SoftwareNode.AddChild('candidates');
      CandidatesNode.Text:= 'prova';

  Config.SaveToFile(FileName);
end;
// -----------------------------------------------------------------------------



// INITIAL SETTINGS
// -----------------------------------------------------------------------------
procedure TF_Hitsugaya.FormCreate(Sender: TObject);
var
    i,j:      Word;
    Res:      TSearchRec;
begin
  // Checking software availability
  if (FindFirst('config\*.bat', faAnyFile, Res) < 0)
      or
      not(DirectoryExists('config'))
  then
    begin
      MessageDlg('Nessun Software trovato', mtWarning, [mbOK], 0);
      Exit;
    end;
  FindClose(res);

  // Load images into components
  LoadIcons();
  // Create available drives list for mapping
  CreateFreeDriveList();
  // Build available software list
  BuildSoftwareList();
  // Show executing path
  E_Path.Text:= GetCurrentDir();

  if L_Software.Count > 0 then
  begin
    L_Software.ItemIndex:= 0;
    B_Add.Enabled:= True;
  end;
end;
// -----------------------------------------------------------------------------

// FINAL OPERATIONS
// -----------------------------------------------------------------------------
procedure TF_Hitsugaya.FormClose(Sender: TObject; var Action: TCloseAction);
var vFile: TextFile;
begin
  //SaveCurrentConfig('config.xml');

  if FileExists('P_Hitsugaya.exe') then
  begin
    AssignFile(vFile, 'version');
    Rewrite(vFile);
    Write(vFile, GetVersion('P_Hitsugaya.exe'));
    CloseFile(vFile);
  end;
end;
// -----------------------------------------------------------------------------

// MOVE BETWEEN LISTBOX ELEMENTS
// -----------------------------------------------------------------------------
procedure TF_Hitsugaya.B_AddClick(Sender: TObject);
var i:      Word;
    found:  Bool;
begin
  i:= 0;
  found:= False;
  while not(found) and (i < L_Candidates.Count) do
  begin
    if L_Candidates.Items[i] = L_Software.Items[L_Software.ItemIndex] then
      found:= True;
    inc(i);
  end;

  if not(found) then
    L_Candidates.Items.Add(L_Software.Items[L_Software.ItemIndex]);

  // Sposto la selezione sul SW appena selezionato
  L_Candidates.ItemIndex:= L_Candidates.Count - 1;

  if L_Candidates.Count > 0 then
  begin
    B_Start.Enabled:= True;
    B_Remove.Enabled:= True;
  end;
  L_CandidatesClick(Sender);
end;

procedure TF_Hitsugaya.B_RemoveClick(Sender: TObject);
var tmp: Integer;
begin
  if (L_Candidates.ItemIndex > 0) or ((L_Candidates.ItemIndex = 0) and (L_Candidates.Count = 1)) then
    tmp:= L_Candidates.ItemIndex - 1
  else if ((L_Candidates.ItemIndex + 1) <= L_Candidates.Count) then
    tmp:= L_Candidates.ItemIndex + 1
  else
    tmp:= -1;

  L_Candidates.Items.Delete(L_Candidates.ItemIndex);

  // Sposto la selezione sul SW appena sopra
  L_Candidates.ItemIndex:= tmp;

  L_CandidatesClick(Sender);
end;
// -----------------------------------------------------------------------------



// MOVE CANDIDATES LISTBOX ELEMENTS UP & DOWN
// -----------------------------------------------------------------------------
procedure TF_Hitsugaya.B_UpClick(Sender: TObject);
var S: ShortString;
begin
  S:= L_Candidates.Items[L_Candidates.ItemIndex - 1];
  L_Candidates.Items[L_Candidates.ItemIndex - 1]:= L_Candidates.Items[L_Candidates.ItemIndex];
  L_Candidates.Items[L_Candidates.ItemIndex]:= S;

  L_Candidates.ItemIndex:= L_Candidates.ItemIndex - 1;
  L_CandidatesClick(Sender);
end;

procedure TF_Hitsugaya.B_DownClick(Sender: TObject);
var S: ShortString;
begin
  S:= L_Candidates.Items[L_Candidates.ItemIndex + 1];
  L_Candidates.Items[L_Candidates.ItemIndex + 1]:= L_Candidates.Items[L_Candidates.ItemIndex];
  L_Candidates.Items[L_Candidates.ItemIndex]:= S;

  L_Candidates.ItemIndex:= L_Candidates.ItemIndex + 1;
  L_CandidatesClick(Sender);
end;
// -----------------------------------------------------------------------------



procedure TF_Hitsugaya.B_InfoClick(Sender: TObject);
begin
  MessageDlg(SwList[L_Software.ItemIndex].Version, mtInformation, [mbOK], 0);
end;



// KEYBOARD CONTROLS SETTINGS
// -----------------------------------------------------------------------------
procedure TF_Hitsugaya.L_CandidatesClick(Sender: TObject);
begin
  if L_Candidates.ItemIndex = -1 then
  begin
    B_Start.Enabled:= False;
    B_Remove.Enabled:= False;
    B_Up.Enabled:= False;
    B_Down.Enabled:= False;
  end
  else if (L_Candidates.ItemIndex = 0) and (L_Candidates.Count = 1) Then
    begin
      B_Up.Enabled:= False;
      B_Up.Enabled:= False;
    end
  else if L_Candidates.ItemIndex = (L_Candidates.Count - 1) then
    begin
      B_Up.Enabled:= True;
      B_Down.Enabled:= False;
    end
  else if L_Candidates.ItemIndex = 0 then
    begin
      B_Up.Enabled:= False;
      B_Down.Enabled:= True;
    end
  else
    begin
      B_Up.Enabled:= True;
      B_Down.Enabled:= True;
    end;
end;

procedure TF_Hitsugaya.L_CandidatesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = 8) and (L_Candidates.Count > 0) Then
    B_RemoveClick(Sender);
  L_CandidatesClick(Sender);
end;

procedure TF_Hitsugaya.L_SoftwareClick(Sender: TObject);
begin
  if SwList[L_Software.ItemIndex].Version = 'v' then
    B_Info.Enabled:= False
  else
    B_Info.Enabled:= True;
end;

procedure TF_Hitsugaya.L_SoftwareKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = 13) and (L_Candidates.Count > 0) Then
    B_AddClick(Sender);
  L_SoftwareClick(Sender);
  L_CandidatesClick(Sender);
end;
// -----------------------------------------------------------------------------



// FINAL BATCH CREATION
// -----------------------------------------------------------------------------
procedure TF_Hitsugaya.B_StartClick(Sender: TObject);
var
  i,j:            Word;
  Found:          Bool;
  HitInstallFile: TextFile;
begin
  // 1st Step -----
  I_Check1.Visible:= True;
  L_Status1.Visible:= True;
  L_Status1.Font.Style:= L_Status1.Font.Style + [fsBold];

  AssignFile(HitInstallFile, 'config\install\install.bat');
  Rewrite(HitInstallFile);
  Writeln(HitInstallFile, '@echo off');
  Writeln(HitInstallFile, 'cls');
  Writeln(HitInstallFile, 'echo Hitsugaya installation Batch');
  Writeln(HitInstallFile, 'echo ----------');

  // Remove *.exe authorization prompt
  Writeln(HitInstallFile, 'Abilitazione Esecuzione file exe...');
  Writeln(HitInstallFile, 'REG IMPORT tools\EnableRemoteExe.reg');
  Writeln(HitInstallFile, 'gpupdate /force');
  Writeln(HitInstallFile, 'echo ----------');

  if CB_Mapping.Checked then
  begin
    Writeln(HitInstallFile, 'echo Mappatura di ' + E_Path.Text + ' in ' + CB_Drive.Items[CB_Drive.ItemIndex] + ' ...');
    Writeln(HitInstallFile, 'net use ' + CB_Drive.Items[CB_Drive.ItemIndex] + ' ' + E_Path.Text + ' /PERSISTENT:NO');
    Writeln(HitInstallFile, 'echo ----------');
  end;

  for j:= 0 to (L_Candidates.Count - 1) do
  begin
    i:= 0;
    Found:= False;
    while (i < L_Software.Count) and not(Found) do
    begin
      if L_Software.Items[i] = L_Candidates.Items[j] then
        Found:= True;
      inc(i);
    end;

    if Found then
    begin
      Writeln(HitInstallFile, 'echo Installazione ' + IntToStr(j + 1) + ' di ' + IntToStr(L_Candidates.Count) + ' in corso...');
      Writeln(HitInstallFile, 'echo Installazione di ' + SwList[i - 1].Name + '...');
      Writeln(HitInstallFile, 'start /wait config\' + SwList[i - 1].Params);
      Writeln(HitInstallFile, 'echo ----------');
    end;
  end;

  // Re-enable remote exe authorization prompt
  Writeln(HitInstallFile, 'Ripristino protezioni file exe...');
  Writeln(HitInstallFile, 'REG DELETE HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Policies\Associations /f');
  Writeln(HitInstallFile, 'gpupdate /force');
  Writeln(HitInstallFile, 'echo ----------');

  Writeln(HitInstallFile, 'pause');

  CloseFile(HitInstallFile);
  IL_Hitsugaya.GetIcon(5, I_Check1.Picture.Icon);
  L_Status1.Caption:= L_Status1.Caption + ' OK';
  L_Status1.Font.Style:= L_Status1.Font.Style - [fsBold];
  //----------

  // 2nd Step -----
  I_Check2.Visible:= True;
  L_Status2.Visible:= True;
  L_Status2.Font.Style:= L_Status2.Font.Style + [fsBold];

  ShellExecute(Handle, 'open', 'config\install\install.bat', nil, nil, SW_SHOWNORMAL);

  IL_Hitsugaya.GetIcon(5, I_Check2.Picture.Icon);
  L_Status2.Caption:= L_Status2.Caption + ' OK';
  L_Status2.Font.Style:= L_Status2.Font.Style - [fsBold];
  //----------

  Sleep(3000);
  F_Hitsugaya.Close;
end;
// -----------------------------------------------------------------------------



end.
