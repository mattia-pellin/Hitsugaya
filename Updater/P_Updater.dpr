program P_Updater;

uses
  Forms,
  U_Updater in 'U_Updater.pas' {F_Updater};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Hitsugaya Updater';
  Application.CreateForm(TF_Updater, F_Updater);
  Application.Run;
end.
