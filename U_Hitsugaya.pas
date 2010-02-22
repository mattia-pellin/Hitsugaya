unit U_Hitsugaya;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, ShellApi,
  ImgList, XMLDoc, XMLIntf, jpeg, U_Classes, U_ExtProcFunc;

type
    TF_Hitsugaya = class(TForm)
    P_Spacer: TPanel;
    I_Logo: TImage;
    P_Mapping: TPanel;
    E_Path: TEdit;
    CB_Mapping: TCheckBox;
    LB_Software: TListBox;
    P_Status: TPanel;
    LB_Candidates: TListBox;
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
    CB_Category: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure B_AddClick(Sender: TObject);
    procedure B_RemoveClick(Sender: TObject);
    procedure LB_SoftwareKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LB_CandidatesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure B_UpClick(Sender: TObject);
    procedure LB_CandidatesClick(Sender: TObject);
    procedure B_DownClick(Sender: TObject);
    procedure B_InfoClick(Sender: TObject);
    procedure LB_SoftwareClick(Sender: TObject);
    procedure B_StartClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CB_CategoryChange(Sender: TObject);
    procedure CB_CategoryKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  SW_PATH = 'config\';

var
  F_Hitsugaya: TF_Hitsugaya;
  SwList:      U_ExtProcFunc.SWList;

implementation

{$R *.dfm}

// PROCEDURES & FUNCTIONS
// -----------------------------------------------------------------------------
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
var Res: TSearchRec;
begin
  // Checking software availability
  if (FindFirst(SW_PATH + '*.bat', faAnyFile, Res) <> 0)
      or
      not(DirectoryExists(SW_PATH))
  then
    begin
      MessageDlg('Nessun Software trovato', mtWarning, [mbOK], 0);
      Exit;
    end;
  FindClose(res);

  // Load images into components
  LoadIcons();
  // Create available drives list for mapping
  CreateFreeDriveList(CB_Drive);
  // Build available software list
  SwList:= BuildSoftwareList(LB_Software);
  // Build available categories list
  BuildCategoryList(SwList, CB_Category);
  // Show executing path
  E_Path.Text:= GetCurrentDir();

  if LB_Software.Count > 0 then
  begin
    LB_Software.ItemIndex:= 0;
    B_Add.Enabled:= True;
  end;
  LB_SoftwareClick(Sender);
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
    Write(vFile, GetFileVer('P_Hitsugaya.exe'));
    CloseFile(vFile);
  end;
end;
// -----------------------------------------------------------------------------



// MOVE BETWEEN LISTBOX ELEMENTS
// -----------------------------------------------------------------------------
procedure TF_Hitsugaya.B_AddClick(Sender: TObject);
var i:      Word;
begin
  if LB_Software.ItemIndex = -1 then
    Exit;

  // Impedisco la perdita della selezione
  i:= LB_Software.ItemIndex - 1;

  LB_Candidates.Items.Add(LB_Software.Items[LB_Software.ItemIndex]);
  LB_Software.Items.Delete(LB_Software.ItemIndex);

  // Impedisco la perdita della selezione
  LB_Software.ItemIndex:= i;
  if (LB_Software.ItemIndex = -1) and (LB_Software.Items.Count > 0) then
    LB_Software.ItemIndex:= 0;

  // Sposto la selezione sul SW appena selezionato
  LB_Candidates.ItemIndex:= LB_Candidates.Count - 1;

  if LB_Candidates.Count > 0 then
  begin
    B_Start.Enabled:= True;
    B_Remove.Enabled:= True;
  end;
  LB_CandidatesClick(Sender);
end;

procedure TF_Hitsugaya.B_RemoveClick(Sender: TObject);
var i, tmp: Integer;
begin
  if LB_Candidates.ItemIndex = -1 then
    Exit;

  if (LB_Candidates.ItemIndex > 0) or ((LB_Candidates.ItemIndex = 0) and (LB_Candidates.Count = 1)) then
    tmp:= LB_Candidates.ItemIndex - 1
  else if ((LB_Candidates.ItemIndex + 1) <= LB_Candidates.Count) then
    tmp:= LB_Candidates.ItemIndex + 1
  else
    tmp:= -1;

  // Impedisco la perdita della selezione
  i:= LB_Candidates.ItemIndex - 1;

  LB_Software.Items.Add(LB_Candidates.Items[LB_Candidates.ItemIndex]);
  LB_Software.ItemIndex:= LB_Software.Items.IndexOf(LB_Candidates.Items[LB_Candidates.ItemIndex]);

  LB_Candidates.Items.Delete(LB_Candidates.ItemIndex);

  // Impedisco la perdita della selezione
  LB_Candidates.ItemIndex:= i;
  if (LB_Candidates.ItemIndex = -1) and (LB_Candidates.Items.Count > 0) then
    LB_Candidates.ItemIndex:= 0;

  // Sposto la selezione sul SW appena sopra
  LB_Candidates.ItemIndex:= tmp;

  LB_CandidatesClick(Sender);
end;
// -----------------------------------------------------------------------------



// MOVE CANDIDATES LISTBOX ELEMENTS UP & DOWN
// -----------------------------------------------------------------------------
procedure TF_Hitsugaya.B_UpClick(Sender: TObject);
var S: String;
begin
  S:= LB_Candidates.Items[LB_Candidates.ItemIndex - 1];
  LB_Candidates.Items[LB_Candidates.ItemIndex - 1]:= LB_Candidates.Items[LB_Candidates.ItemIndex];
  LB_Candidates.Items[LB_Candidates.ItemIndex]:= S;

  LB_Candidates.ItemIndex:= LB_Candidates.ItemIndex - 1;
  LB_CandidatesClick(Sender);
end;

procedure TF_Hitsugaya.B_DownClick(Sender: TObject);
var S: String;
begin
  S:= LB_Candidates.Items[LB_Candidates.ItemIndex + 1];
  LB_Candidates.Items[LB_Candidates.ItemIndex + 1]:= LB_Candidates.Items[LB_Candidates.ItemIndex];
  LB_Candidates.Items[LB_Candidates.ItemIndex]:= S;

  LB_Candidates.ItemIndex:= LB_Candidates.ItemIndex + 1;
  LB_CandidatesClick(Sender);
end;
// -----------------------------------------------------------------------------




// MOUSE & KEYBOARD CONTROLS SETTINGS
// -----------------------------------------------------------------------------
procedure TF_Hitsugaya.LB_CandidatesClick(Sender: TObject);
begin
  if LB_Candidates.ItemIndex = -1 then
  begin
    B_Start.Enabled:= False;
    B_Remove.Enabled:= False;
    B_Up.Enabled:= False;
    B_Down.Enabled:= False;
  end
  else if (LB_Candidates.ItemIndex = 0) and (LB_Candidates.Count = 1) Then
    begin
      B_Up.Enabled:= False;
      B_Up.Enabled:= False;
    end
  else if LB_Candidates.ItemIndex = (LB_Candidates.Count - 1) then
    begin
      B_Up.Enabled:= True;
      B_Down.Enabled:= False;
    end
  else if LB_Candidates.ItemIndex = 0 then
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

procedure TF_Hitsugaya.LB_CandidatesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = 8) and (LB_Candidates.Count > 0) Then
    B_RemoveClick(Sender);
  LB_CandidatesClick(Sender);
end;

procedure TF_Hitsugaya.LB_SoftwareClick(Sender: TObject);
begin
  if SwList[LB_Software.ItemIndex].Version = 'v' then
    B_Info.Enabled:= False
  else
    B_Info.Enabled:= True;
end;

procedure TF_Hitsugaya.LB_SoftwareKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = 13) and (LB_Candidates.Count > 0) Then
    B_AddClick(Sender);
  LB_SoftwareClick(Sender);
  LB_CandidatesClick(Sender);
end;

procedure TF_Hitsugaya.B_InfoClick(Sender: TObject);
begin
  MessageDlg(SwList[HitSoftFind(LB_Software.Items[LB_Software.ItemIndex], SwList)].Version, mtInformation, [mbOK], 0);
end;

procedure TF_Hitsugaya.CB_CategoryChange(Sender: TObject);
var i: Integer;
begin
  LB_Software.Items.Clear;
  for i := 0 to Length(SwList) - 1 do
    if (SwList[i].Category = CB_Category.Items[CB_Category.ItemIndex])
        or
       (CB_Category.ItemIndex = 0)
    then
      LB_Software.Items.Add(SwList[i].Name);
end;

// Disallow user to manually modify current category
procedure TF_Hitsugaya.CB_CategoryKeyPress(Sender: TObject; var Key: Char);
begin
  Key:= #0;
end;

// -----------------------------------------------------------------------------



// FINAL BATCH CREATION
// -----------------------------------------------------------------------------
procedure TF_Hitsugaya.B_StartClick(Sender: TObject);
var
  i,j,k,y:        Integer;
  HitInstallFile: TextFile;
  StrList:        TStringList;
begin
  // 1st Step -----
  I_Check1.Visible:= True;
  L_Status1.Visible:= True;
  L_Status1.Font.Style:= L_Status1.Font.Style + [fsBold];

  AssignFile(HitInstallFile, GetEnvironmentVariable('TEMP') + '\hitsugaya.bat');
  Rewrite(HitInstallFile);

  Writeln(HitInstallFile, '@echo off');
  Writeln(HitInstallFile, 'color 0A');
  Writeln(HitInstallFile, 'cls');
  Writeln(HitInstallFile, 'echo Hitsugaya installation Batch');
  Writeln(HitInstallFile, 'echo ----------');

  // Map remote path
  if CB_Mapping.Checked then
  begin
    Writeln(HitInstallFile, 'echo Mappatura di ' + E_Path.Text + ' in ' + CB_Drive.Items[CB_Drive.ItemIndex] + ' ...');
    Writeln(HitInstallFile, 'net use ' + CB_Drive.Items[CB_Drive.ItemIndex] + ' ' + E_Path.Text + ' /PERSISTENT:NO');
    Writeln(HitInstallFile, 'echo ----------');
  end;

  // Remove *.exe authorization prompt
  Writeln(HitInstallFile, 'echo Abilitazione Esecuzione file exe...');
  if CB_Mapping.Checked then
    Writeln(HitInstallFile, 'REG IMPORT ' + CB_Drive.Items[CB_Drive.ItemIndex] + '\tools\EnableRemoteExe.reg')
  else
    Writeln(HitInstallFile, 'REG IMPORT ' + E_Path.Text + '\tools\EnableRemoteExe.reg');
  Writeln(HitInstallFile, 'gpupdate /force');
  Writeln(HitInstallFile, 'echo ----------');

  for j:= 0 to (LB_Candidates.Count - 1) do
  begin
    i:= HitSoftFind(LB_Candidates.Items[j], SwList);

    if i <> -1 then
    begin
      Writeln(HitInstallFile, 'echo Installazione ' + IntToStr(j + 1) + ' di ' + IntToStr(LB_Candidates.Count) + ' in corso...');
      Writeln(HitInstallFile, 'echo Installazione di ' + SwList[i].Name + '...');
      for k := 0 to Length(SwList[i].Commands) - 1 do
      begin
        Writeln(HitInstallFile, 'echo   Esecuzione comando ' + IntToStr(k + 1) + ' di ' + IntToStr(Length(SwList[i].Commands)) + '...');
        Write(HitInstallFile, 'start /wait');

        StrList:= Split(SwList[i].Commands[k], ' ');
        for y := 0 to StrList.Count - 1 do
          if FileExists(SW_PATH + StrList[y]) then
            Write(HitInstallFile, ' ' + SW_PATH + StrList[y])
          else
            Write(HitInstallFile, ' ' + StrList[y]);

        if CB_Mapping.Checked then
          Writeln(HitInstallFile, CB_Drive.Items[CB_Drive.ItemIndex] + SW_PATH + SwList[i].Commands[k])
        else
          Writeln(HitInstallFile, SW_PATH + SwList[i].Commands[k]);
      end;
      Writeln(HitInstallFile, 'echo ----------');
    end;
  end;

  // Re-enable remote exe authorization prompt
  Writeln(HitInstallFile, 'echo Ripristino protezioni file exe...');
  Writeln(HitInstallFile, 'REG DELETE HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Policies\Associations /f');
  Writeln(HitInstallFile, 'gpupdate /force');
  Writeln(HitInstallFile, 'echo ----------');

  // Modify windows registry to optimize startup/shutdown
  Writeln(HitInstallFile, 'echo Ottimizzazione registro di sistema...');
  if CB_Mapping.Checked then
    Writeln(HitInstallFile, 'REG IMPORT ' + CB_Drive.Items[CB_Drive.ItemIndex] + '\tools\WindowsOptimize.reg')
  else
    Writeln(HitInstallFile, 'REG IMPORT ' + E_Path.Text + '\tools\WindowsOptimize.reg');
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

  // Run installation script
  ShellExecute(Handle, 'open', PChar(GetEnvironmentVariable('TEMP') + '\hitsugaya.bat'), nil, nil, SW_SHOWNORMAL);

  IL_Hitsugaya.GetIcon(5, I_Check2.Picture.Icon);
  L_Status2.Caption:= L_Status2.Caption + ' OK';
  L_Status2.Font.Style:= L_Status2.Font.Style - [fsBold];
  //----------

  Sleep(3000);
  F_Hitsugaya.Close;
end;
// -----------------------------------------------------------------------------


end.
