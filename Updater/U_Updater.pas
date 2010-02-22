unit U_Updater;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtActns, StdCtrls, ComCtrls, ShellApi, jpeg, ExtCtrls;

type
  TF_Updater = class(TForm)
    I_Background: TImage;
    PB_Download: TProgressBar;
    L_Progress: TLabel;
    L_Yours: TLabel;
    L_yVersion: TLabel;
    L_Current: TLabel;
    L_cVersion: TLabel;
    T_Upgrade: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure T_UpgradeTimer(Sender: TObject);
  private
    procedure URL_OnDownloadProgress
        (Sender: TDownLoadURL;
         Progress, ProgressMax: Cardinal;
         StatusCode: TURLDownloadStatus;
         StatusText: String; var Cancel: Boolean);
  public
    { Public declarations }
  end;

var
  F_Updater: TF_Updater;

implementation

{$R *.dfm}

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

procedure GetOnlineVer();
begin
  with TDownLoadURL.Create(nil) do
  try
    URL:= 'http://github.com/Ebrithil/Hitsugaya/raw/master/version';
    Filename:= GetEnvironmentVariable('TEMP') + '\ver';
    try
      ExecuteTarget(nil);
    except
      F_Updater.L_cVersion.Caption:= 'Error retrieving current Version';
    end;
  finally
    Free;
  end;
end;

procedure TF_Updater.URL_OnDownloadProgress;
begin
  PB_Download.Max:= ProgressMax;
  PB_Download.Position:= Progress;
  if PB_Download.Max > 0 then
    L_Progress.Caption:= IntToStr(Trunc(Progress/ProgressMax * 100)) + '%'
  else
    L_Progress.Caption:= 'Downloading...';
  Application.ProcessMessages;
end;

procedure TF_Updater.FormCreate(Sender: TObject);
var
  vFile:   TextFile;
  version: ShortString;
begin

  if FileExists('Hitsugaya.exe') then
    L_yVersion.Caption:= 'v' + GetVersion('Hitsugaya.exe')
  else
    L_yVersion.Caption:= 'File not Found!';

  GetOnlineVer();
  if FileExists(GetEnvironmentVariable('TEMP') + '\ver') then
    begin
      AssignFile(vFile, GetEnvironmentVariable('TEMP') + '\ver');
      Reset(vFile);
      Readln(vFile, version);
      CloseFile(vFile);
      DeleteFile(GetEnvironmentVariable('TEMP') + '\ver');

      F_Updater.L_cVersion.Caption:= 'v' + version;
    end;
end;

procedure TF_Updater.T_UpgradeTimer(Sender: TObject);
begin
  T_Upgrade.Tag:= T_Upgrade.Tag + Abs(T_Upgrade.Interval);

  if T_Upgrade.Tag = 1000 then
    begin
      T_Upgrade.Enabled:= False;

      // If your version is older than current, then upgrade
      if (L_yVersion.Caption < L_cVersion.Caption)
          or
          not(FileExists('Hitsugaya.exe'))
      then
        begin
          if FileExists('Hitsugaya.exe') then
            DeleteFile('Hitsugaya.exe');
          with TDownloadURL.Create(self) do
            try
              //URL:= 'http://cdimage.debian.org/debian-cd/5.0.3/i386/iso-cd/debian-503-i386-netinst.iso';
              URL:= 'http://github.com/Ebrithil/Hitsugaya/raw/master/P_Hitsugaya.exe';
              FileName:= 'Hitsugaya.exe';
              OnDownloadProgress:= URL_OnDownloadProgress;

              try
                ExecuteTarget(nil);
              except
                L_Progress.Caption:= 'Download Error';
              end;
            finally
              Free;
              if FileExists('Hitsugaya.exe') then
                L_Progress.Caption:= 'Completed!';
          end;
        end
      else
        L_Progress.Caption:= 'No Update Needed';

      T_Upgrade.Enabled:= True;
    end
  else if T_Upgrade.Tag = 4000 then
    begin
      T_Upgrade.Enabled := False;

      // Close Updater
      if FileExists('Hitsugaya.exe') then
        ShellExecute(F_Updater.Handle, 'open', 'Hitsugaya.exe', nil, nil, SW_SHOWNORMAL)
      else
        MessageDlg('Unable to find Hitsugaya.exe. Please Retry', mtError, [mbOK], 0);
      Application.Terminate;
    end;
end;

end.
