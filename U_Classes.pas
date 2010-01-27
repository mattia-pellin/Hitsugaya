unit U_Classes;

interface

uses
  Windows, Classes, SysUtils, Variants;

type
  THitSoft = Class

  public
    Name,
    fName,
    Version,
    Category: String;
    Commands: array of String;
    IsValid:  Boolean;

    constructor Create(bFileName: String; SoftList: array of THitSoft); overload;
  End;

  function GetFileVer(sFileName:string): String;
  function Split(StrBuf, Delimiter: String): TStringList;
  function HitSoftFind(sName: String; SoftList: array of THitSoft): Integer;

implementation

  function HitSoftFind(sName: String; SoftList: array of THitSoft): Integer;
  var i: Integer;
  begin
    Result:= -1;
    for i := 0 to Length(SoftList) - 1 do
      if SoftList[i].Name = sName then
        begin
          Result:= i;
          break;
        end;
  end;

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

  // Porting of VB Split function
  function Split(StrBuf,Delimiter: string): TStringList;
  var
    MyStrList: TStringList;
    TmpBuf:    String;
    LoopCount: Integer;
  begin
    MyStrList := TStringList.Create;
    LoopCount := 1;

    repeat
      if StrBuf[LoopCount] = Delimiter then
      begin
        MyStrList.Add(TmpBuf);
        TmpBuf := '';
      end;

      TmpBuf := TmpBuf + StrBuf[LoopCount];
      inc(LoopCount);
    until LoopCount > Length(StrBuf);
    MyStrList.Add(Trim(TmpBuf));

    Result := MyStrList;
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

  constructor THitSoft.Create(bFileName: String; SoftList: array of THitSoft);
  var
    Row:   String;
    sFile: TStringList;
    bFile: TextFile;
  begin
    fName:= bFileName;
    if FileExists('Config\' + bFileName) then
      begin
        AssignFile(bFile, 'Config\' + bFileName);
        Reset(bFile);

        // Read FileName
        Readln(bFile, Row);
        Delete(Row, 1, 2);
        Trim(Row);
        if (Row <> '') and (HitSoftFind(Row, SoftList) <> -1) then
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
          Version:= GetFileVer('Config\' + bFileName);

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
              sFile:= Split(Row, ' ');
              if FileExists('config\' + sFile[0])
                 or
                 ((sFile.Count > 1) and (FileExists('config\' + sFile[1])))
              then
                begin
                  SetLength(Commands, Length(Commands) + 1);
                  Commands[Length(Commands) - 1]:= Row;
                end;
              sFile.Free;
            end;
        end;
        CloseFile(bFile);
        if Length(Commands) = 0 then
          IsValid:= False
        else
          IsValid:= True;
    end;
  end;

end.
