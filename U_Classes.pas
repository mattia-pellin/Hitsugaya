unit U_Classes;

interface

uses
  SysUtils, Variants;

type
  THitSoft = Class

  public
    Name,
    Version,
    Category: ShortString;
    Commands: array of ShortString;
    IsValid:  Boolean;

    constructor Create(bFileName: ShortString; SoftList: array of THitSoft); overload;
  End;

implementation

  function IsNameInto(sName: ShortString; SoftList: array of THitSoft): Boolean;
  var i: Word;
  begin
    Result:= False;
    for i := 0 to Length(SoftList) - 1 do
      if SoftList[i].Name = sName then
        begin
          Result:= True;
          break;
        end;
  end;

  // IMPORTANT:
  // Batch files format MUST be as following:
  // ----------------------------------------
  // ::ProgramName
  // ::ProgramVersion
  // ::ProgramCategory
  // filepath\executable /switch
  // filepath\executable /switch (in sequence)
  // ... (as many times you want)
  // end (no new line at the EOF)
  // ----------------------------------------

  constructor THitSoft.Create(bFileName: ShortString; SoftList: array of THitSoft);
  var
    Row:   ShortString;
    bFile: TextFile;
  begin
    if FileExists('Config\' + bFileName) then
      begin
        AssignFile(bFile, 'Config\' + bFileName);
        Reset(bFile);

        // Read FileName
        Readln(bFile, Row);
        Delete(Row, 1, 2);
        Trim(Row);
        if (Row <> '') and IsNameInto(Row, SoftList) then
          Name:= Copy(bFileName, 1, Length(bFileName) - 4)
        else
          Name:= Row;

        // Read FileVersion
        Readln(bFile, Row);
        Delete(Row, 1, 2);
        Trim(Row);
        if Row <> '' then
          Version:= 'v' + Row
        else
          Version:= Null;

        // Read FileCategory
        Readln(bFile, Row);
        Delete(Row, 1, 2);
        Trim(Row);
        if Row <> '' then
          Category:= Row
        else
          Category:= Null;

        SetLength(Commands, 0);
        while not(Eof(bFile)) do
        begin
          // Read every command
          Readln(bFile, Row);
          Trim(Row);
          if Row <> '' then
            begin
              SetLength(Commands, Length(Commands) + 1);
              Commands[Length(Commands)]:= Row;
            end;
        end;
        if Length(Commands) = 0 then
          IsValid:= False;
    end;
  end;

end.
