program P_Hitsugaya;

uses
  Forms,
  U_Loading     in 'U_Loading.pas'   {F_Loading},
  U_Hitsugaya   in 'U_Hitsugaya.pas' {F_Hitsugaya},
  U_Classes     in 'U_Classes.pas',
  U_ExtProcFunc in 'U_ExtProcFunc.pas';


{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Hitsugaya';
  Application.ShowMainForm:= False;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TF_Hitsugaya, F_Hitsugaya);
  Application.CreateForm(TF_Loading, F_Loading);
  Application.ShowMainForm:= True;
  Application.Run;
end.
