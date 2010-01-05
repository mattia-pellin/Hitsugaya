program P_Hitsugaya;

uses
  Forms,
  U_Hitsugaya in 'U_Hitsugaya.pas' {F_Hitsugaya},
  U_Classes in 'U_Classes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Hitsugaya';
  Application.CreateForm(TF_Hitsugaya, F_Hitsugaya);
  Application.Run;
end.
