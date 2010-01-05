unit U_Classes;

interface

uses
  SysUtils;

type
  THitSoft = Class

  public
    Name,
    Params,
    Version: ShortString;

    constructor Create(F_Name: ShortString); overload;
  End;

implementation

  constructor THitSoft.Create(F_Name: ShortString);
  var
    HitComFile: TextFile;
    Buffer:     ShortString;
  begin
    if FileExists('Config\' + F_Name) then
      begin
        AssignFile(HitComFile, 'Config\' + F_Name);
        Reset(HitComFile);

        ReadLn(HitComFile, Buffer);
        Name:= Copy(Buffer, 3, Length(Buffer));
        ReadLn(HitComFile, Buffer);
        Version:= 'v' + Copy(Buffer, 3, Length(Buffer));
        ReadLn(HitComFile, Params);

        CloseFile(HitComFile);
      end;
    if Name = '' then
      Name:= Copy(F_Name, 1, Length(F_Name) - 4);
  end;

end.
