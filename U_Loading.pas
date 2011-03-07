unit U_Loading;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, StrUtils;

type
  TF_Loading = class(TForm)
    PB_Loading: TProgressBar;
    L_Loading: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  F_Loading: TF_Loading;

implementation

uses U_Hitsugaya, U_ExtProcFunc;

{$R *.dfm}

procedure TF_Loading.FormCreate(Sender: TObject);
var Res: TSearchRec;
begin
  with F_Hitsugaya do
  begin
    F_Hitsugaya.Visible:= False;
    //F_Loading.Caption:= F_Hitsugaya.Caption;
    F_Loading.Top:=  (Screen.Height - F_Loading.Height) div 2;
    F_Loading.Left:= (Screen.Width  - F_Loading.Width)  div 2;

    //Show to users the application loading
    F_Loading.Show;

    // Check path and software availability
    UpdateLoading('Checking prerequisites...');
    if (FindFirst(SW_PATH + '*.bat', faAnyFile, Res) <> 0)
        or
        not(DirectoryExists(SW_PATH))
    then
      begin
        MessageDlg('Nessun Software trovato', mtWarning, [mbOK], 0);
        Application.Terminate;
      end;
    FindClose(res);
    F_Loading.PB_Loading.Position:= 100;

    // Search for x64 or x86 compatibility
    UpdateLoading('Checking system architecture...');
    L_Bits.Caption:= GetExBits;
    F_Loading.PB_Loading.Position:= 100;

    // Load images into components
    UpdateLoading('Loading decorations...');
    IL_Hitsugaya.GetBitmap(0, B_Add.Glyph);
    IL_Hitsugaya.GetBitmap(1, B_Remove.Glyph);
    IL_Hitsugaya.GetBitmap(2, B_Up.Glyph);
    IL_Hitsugaya.GetBitmap(3, B_Down.Glyph);
    IL_Hitsugaya.GetBitmap(4, B_Info.Glyph);
    IL_Hitsugaya.GetBitmap(5, B_Update.Glyph);
    F_Loading.PB_Loading.Position:= 100;

    // Create available drives list for mapping
    UpdateLoading('Checking system drives availability...');

    CreateFreeDriveList(CB_Drive);
    CB_Drive_WSUS.Items:= CB_Drive.Items;
    if CB_Drive_WSUS.Items.IndexOf('W:') = -1 then
      CB_Drive_WSUS.ItemIndex:= CB_Drive_WSUS.Items.Count - 2
    else
      CB_Drive_WSUS.ItemIndex:= CB_Drive_WSUS.Items.IndexOf('W:');

    F_Loading.PB_Loading.Position:= 100;

    // Build available software list
    UpdateLoading('Building software list...');
    SwList:= BuildSoftwareList(LB_Software);

    // Build available categories list
    UpdateLoading('Building categories list...');
    BuildCategoryList(SwList, CB_Category);

    // Set user desktop as default update directory
    UpdateLoading('Setting up variables...');
    OD_Update.InitialDir:= GetEnvironmentVariable('HOMEPATH') + '\Desktop';
    F_Loading.PB_Loading.Position:= 100;

    // Show executing path
    UpdateLoading('Setting up paths...');

    E_Path.Text:= GetCurrentDir();
    E_Path_WSUS.Text:= LeftStr(E_Path.Text, LastDelimiter('\', E_Path.Text));
    E_Path_WSUS.Text:= E_Path_WSUS.Text + 'WSUSoffline';
    if E_Path.Text[1] = '\' then
      CB_Mapping.Checked:= True;

    if LB_Software.Count > 0 then
    begin
      LB_Software.ItemIndex:= 0;
      B_Add.Enabled:= True;
    end;

    F_Loading.PB_Loading.Position:= 100;

    UpdateLoading('Completed!');
    F_Loading.PB_Loading.Position:= 100;

    Sleep(1000);
  end;
end;

end.
