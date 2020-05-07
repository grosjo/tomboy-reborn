unit Note_Lister;

{$mode objfpc}

INTERFACE

uses
		Classes, SysUtils, Grids, Forms;

type
   PNotebook=^TNotebook;
   TNotebook = record
       Name : ANSIString;      // Name of the notebook
       Template : ANSIString;  // The ID of the Template for this Notebook
       Notes : TStringList;    // A list of the IDs of notes that are members of this Notebook.
   end;

type

   { TNoteBookList }

   TNoteBookList = class(TList)
   private
     	function Get(Index : integer) : PNoteBook;
		procedure RemoveNoteBook(const NBName: AnsiString);
   public
        destructor Destroy; Override;
        { Adds the ID to the Notebook, if Notebook does not already exist, creates it }
        procedure Add(const ID, ANoteBook : ANSIString; IsTemplate : boolean);
        { Returns True if the passed ID is in the passed Notebook }
        function IDinNotebook(const ID, Notebook : ANSIstring) : boolean;
                                // Returns a PNoteBook that has a name matching passed NoteBook.
        function FindNoteBook(const NoteBook : ANSIString) : PNoteBook;
        { Removes any list entries that do not have a Template }
        procedure CleanList();
        property Items[Index : integer] : PNoteBook read Get; default;
   end;


type
  	PNote=^TNote;
  	TNote = record
        		{ will have 36 char GUI plus '.note' }
		ID : ANSIString;
        Title : ANSIString;
        		{ a 33 char date time string }
    	CreateDate : ANSIString;
                { a 33 char date time string, updateable }
    	LastChange : ANSIString;
        IsTemplate : boolean;
        OpenOnStart : boolean;
        OpenNote : TForm;
	end;

type                                 { ---------- TNoteInfoList ---------}

   { TNoteList }

   TNoteList = class(TList)   // TFPList
   private
    	function Get(Index: integer): PNote;
    public
        destructor Destroy; override;
        function Add(ANote : PNote) : integer;
        function FindID(const ID : ANSIString) : PNote;
        property Items[Index: integer]: PNote read Get; default;
    end;

type

   { NoteLister }

   { TNoteLister }

   TNoteLister = class
   private

    OpenNoteIndex : integer;        // Used for Find*OpenNote(), points to last found one, -1 meaning none found
                                { NoteList is a list of pointers. Each one points to a record that contains data
                                  about a particular note. Only Notebook info it has is whether or not its a
                                  template. The ID is stored as a 36 char GUI plus '.note'. Dates must be 33 char.}
   	NoteList : TNoteList;
   	SearchNoteList : TNoteList;
                                { NoteBookList is a list of pointers. Each one points to a record
                                  containing Name, Template ID and a List (called Notes) of IDs of
                                  notes that are members of this Notebook. }
    NoteBookList : TNoteBookList;
                                { Takes a created list and search term string. Returns with the list
                                  containing individual search terms, 1 to many }
    procedure BuildSearchList(SL: TStringList; const Term: AnsiString);
                                { Returns a simple note file name, accepts simple filename or ID }
    function CleanFileName(const FileOrID: AnsiString): ANSIString;
    //procedure DumpNoteBookList();

            // Indexes and maybe searches one note. TermList maybe nil.
   	procedure GetNoteDetails(const Dir, FileName: ANSIString; const TermList: TStringList; DontTestName: boolean=false);


    		{ Returns True if indicated note contains term in its content }
   	function NoteContains(const TermList: TStringList; FileName: ANSIString
        ): boolean;
            { Removes any complete xml tags from passed string, only matches '<' to '>' }
    function RemoveXml(const St: AnsiString): AnsiString;
	procedure RewriteBadChangeDate(const Dir, FileName, LCD: ANSIString);

   public
    XMLError : Boolean;   // Indicates a note was found with an XML (or other) error, checked by calling process.
    ErrorNotes : TStringList;
    SearchIndex : integer;

    { Changes the name associated with a Notebook in the internal data structure }
    function AlterNoteBook(const OldName, NewName: string): boolean;

    { Returns a multiline string to use in writing a notes notebook membership,
                                          knows how to do a template too. String has special XML chars 'escaped'
                                          This function expects to be passed an ID + '.note'. }
    function NoteBookTags(const NoteID: string): ANSIString;
                                        { Returns true if it has put into list one or more Note IDs that are members of NBName }
    function GetNotesInNoteBook(var NBIDList: TStringList; const NBName: string
        ): boolean;
                                       { Retuns the title of note at (zero based) index. }
    function GetTitle(Index: integer): string;
                                        { Returns the number of items in the list }
    function Count(): integer;
                                        { Returns the LastChangeDate string for ID, empty string if not found }
    function GetLastChangeDate(const ID: String): string;
                                        { Adds details of note of passed to NoteList }
    procedure IndexThisNote(const ID : String);
                                        { Returns T is ID in current list, takes 36 char GUID or simple file name }
    function IsIDPresent(ID : string) : boolean;
                                        { Removes the Notebook entry with ID=Template from Notebook datastructure }
    procedure DeleteNoteBookwithID(FileorID : AnsiString);
                                        { Returns True if passed string is the ID or short Filename of a Template }
    function IsATemplate(FileOrID : AnsiString) : boolean;
	                                    { Adds a notebook to the internal data structure, probably only used when making a new
                                          Notebook and its Template }
    procedure AddNoteBook(const ID, ANoteBook: ANSIString; IsTemplate: Boolean);
                                        { Sets the passed Notebooks as 'parents' of the passed note. Any pre existing membership
                                          will be cancelled. The list can contain zero to  many notebooks. }
    procedure SetNotebookMembership(const ID: ansistring; const MemberList: TStringList);
                                        { If ID is empty, always returns false, puts all Notebook names in strlist. If ID is not
                                          empty, list is filtered for only notebooks that have that ID  and returns True iff the
                                          passed ID is that of a Template.  A Notebook Template will have only one Notebook name in
                                          its Tags and that will be added to strlist. The StartHere template won't have a Notebook
                                          Name and therefore wont get mixed up here ???? }
    function GetNotebooks(const NBList: TStringList; const ID: ANSIString): boolean;
                                        { Loads the Notebook StringGrid up with the Notebook names we know about. Add a bool to indicate
                                          we should only show Notebooks that have one or more notes mentioned in SearchNoteList. Call after
                                          GetNotes(Term) }
	procedure LoadStGridNotebooks(const NotebookGrid: TStringGrid; SearchListOnly: boolean);
                                        { Adds a note to main list, ie when user creates a new note }
    procedure AddNote(const FileName, Title, LastChange : ANSIString);
    		                            { Read the metadata from all the notes in internal data structure,
                                          this is the main "go and do it" function.
                                          If 'term' is present we'll just search for notes with that term
                                          and store date in a different structure. And probably call LoadSearchGrid() }
   	function GetNotes(const Term: ANSIstring=''; DontTestName: boolean=false ): longint;
    		                            { Copy the internal Note data to the passed TStringGrid, empting it first.
                                          NoCols can be 2, 3 or 4 being Name, LastChange, CreateDate, ID.
                                          Special case only main List SearchMode True will get from the search list.}
   	procedure LoadStGrid(const Grid: TStringGrid; NoCols: integer;  SearchMode: boolean=false);
    		                            { Returns True if its updated the internal record as indicated,
                                          will accept either an ID or a filename. }
    function AlterNote(ID, Change : ANSIString; Title : ANSIString = '') : boolean;

    function IsThisATitle(const Title : ANSIString) : boolean;
                        		        { Returns the Form this note is open on, Nil if its not open }
    function IsThisNoteOpen(const ID : ANSIString; out TheForm : TForm) : boolean;
                        		        { Tells the list that this note is open, pass NIL to indicate its now closed }
    procedure ThisNoteIsOpen(const ID : ANSIString; const TheForm : TForm);
                        		        { Returns true if it can find a FileName to Match this Title }
    function FileNameForTitle(const Title: ANSIString; out FileName : ANSIstring): boolean;
    procedure StartSearch();
    function NextNoteTitle(out SearchTerm : ANSIString) : boolean;
    		                            { removes note from int data, accepting either an ID or Filename }
    function DeleteNote(const ID : ANSIString) : boolean;
    		                            { Copy the internal data about notes in passed Notebook to passed TStringGrid
                                          for display. So, shown would be all the notes in the nominated notebook.}
    procedure LoadNotebookGrid(const Grid : TStringGrid; const NotebookName : AnsiString);
    		                            { Returns the ID (inc .note) of the notebook Template, if an empty string we did
                                          not find a) the Entry in NotebookList or b) the entry had a blank template. }
    function NotebookTemplateID(const NotebookName : ANSIString) : AnsiString;
                                        { Returns the Form of first open note and sets internal pointer to it, Nil if none found }
    function FindFirstOpenNote(): TForm;
                                        { Call after FindFirstOpenNote(), it will return the next one or Nil if no more found }
    function FindNextOpenNote() : TForm;
                                        { Returns the ID of first note that should be opened on startup internal pointer
                                          (which is same interger as FindFirstOpenNate) to it, '' if none found }
    function FindFirstOOSNote(out NTitle, NID: ANSIstring): boolean;
                                        { Call after FindFirstOOSNote(), it will return the next one or '' if no more found }
    function FindNextOOSNote(var NTitle, NID: ANSIstring): boolean;

    constructor Create;
    destructor Destroy; override;
   end;


{ ----------------------- IMPLEMENTATION --------------- }

implementation

uses  laz2_DOM, laz2_XMLRead, LazFileUtils, LazUTF8, LazLogger, SyncUtils,
                TRcommon, TRsettings;

{ TNoteBookList }


{ ========================= N O T E B O O K L I S T ======================== }

function TNoteBookList.Get(Index: integer): PNoteBook;
begin
    Result := PNoteBook(inherited get(Index));
end;

destructor TNoteBookList.Destroy;
var
    I : Integer;
begin
        for I := 0 to Count-1 do begin
          	Items[I]^.Notes.free;
    		dispose(Items[I]);
		end;
		inherited Destroy;
end;

procedure TNoteBookList.Add(const ID, ANoteBook: ANSIString; IsTemplate: boolean
		);
var
    NB : PNoteBook;
    NewRecord : boolean = False;
    I : integer;
begin
    NB := FindNoteBook(ANoteBook);
    if NB = Nil then begin
        NewRecord := True;
        new(NB);
    	NB^.Name:= ANoteBook;
        NB^.Template := '';
        NB^.Notes := TStringList.Create;
	end;
    if IsTemplate then begin
        NB^.Template:= ID
    end else begin
        // Check its not there already ....
        I := NB^.Notes.Count;
        while I > 0 do begin
                dec(I);
                if ID = NB^.Notes[i]
                    then exit;      // cannot be there if its a new entry so no leak here
        end;
        NB^.Notes.Add(ID);
	end;
	if NewRecord then inherited Add(NB);
end;

function TNoteBookList.IDinNotebook(const ID, Notebook: ANSIstring): boolean;
var
	Index : longint;
    TheNoteBook : PNoteBook;
begin
	Result := False;
    TheNoteBook := FindNoteBook(NoteBook);
    if TheNoteBook = Nil then exit();
    for Index := 0 to TheNoteBook^.Notes.Count-1 do
        if ID = TheNoteBook^.Notes[Index] then begin
            Result := True;
            exit();
		end;
end;

function TNoteBookList.FindNoteBook(const NoteBook: ANSIString): PNoteBook;
var
        Index : longint;
begin
        Result := Nil;
        for Index := 0 to Count-1 do begin
            if Items[Index]^.Name = NoteBook then begin
                Result := Items[Index];
                exit()
    	    end;
    	end;
end;

procedure TNoteBookList.CleanList;
var
	Index : integer = 0;
begin
	while Index < Count do begin
        if Items[Index]^.Template = '' then begin
          	Items[Index]^.Notes.free;
    		dispose(Items[Index]);
            Delete(Index);
		end else
        	inc(Index);
	end;
end;

		// Don't think we use this method  ?
procedure TNoteBookList.RemoveNoteBook(const NBName: AnsiString);
var
	Index : integer;
begin
	for Index := 0 to Count-1 do
    	if Items[Index]^.Name = NBName then begin
          	Items[Index]^.Notes.free;
    		dispose(Items[Index]);
            Delete(Index);
            break;
		end;
    debugln('ERROR, asked to remove a note book that I cannot find.');
end;

// =================== DEBUG PROC ======================================
{
procedure TNoteLister.DumpNoteBookList();
var
    P : PNotebook;
    I : integer;
begin
    debugln('-----------------------');
    for P in NoteBookList do begin
        debugln('Name=' + P^.Name);
        for I := 0 to P^.Notes.Count -1 do
            debugln('     ' + P^.Notes[I]);
    end;
    debugln('-----------------------');
end;
}
{ ====================== NoteLister ============================== }

{ -------------  Things relating to NoteBooks ------------------ }

// consider changing the this works, I don't need to pass a created SL tp GetNotebooks(...)

function TNoteLister.NoteBookTags(const NoteID : string): ANSIString;
var
    SL : TStringList;
    Index : Integer;
begin
   Result := '';
   //if SearchForm.NoteLister = nil then exit;
   SL := TStringList.Create;
   try
       if GetNotebooks(SL, NoteID) then begin  // its a template
   		    Result := '  <tags>'#10'    <tag>system:template</tag>'#10;
            if SL.Count > 0 then
        	    Result := Result + '    <tag>system:notebook:' + RemoveBadXMLCharacters(SL[0], True) + '</tag>'#10'  </tags>'#10;
       end else
   		    if SL.Count > 0 then begin					// its a Notebook Member
        	    Result := '  <tags>'#10;
        	    for Index := 0 to SL.Count -1 do		// here, we auto support multiple notebooks.
        		    Result := Result + '    <tag>system:notebook:' + RemoveBadXMLCharacters(SL[Index], True) + '</tag>'#10;
        	    Result := Result + '  </tags>'#10;
		    end;
   finally
       SL.Free;
   end;
    //debugln('NoteLister : Tags for ' + NoteID + #10 + Result);
    //debugln('---------------');
end;

function TNoteLister.GetNotesInNoteBook(var NBIDList : TStringList; const NBName : string) : boolean;
var
    NB : PNoteBook;
begin
    Result := True;
    NB := NoteBookList.FindNoteBook(NBName);
    if NB <> Nil then
        NBIDList := NB^.Notes
    else Result := False;
end;

function TNoteLister.AlterNoteBook(const OldName, NewName : string) : boolean;
var
    NB : PNoteBook;
begin
    Result := True;
    NB := NoteBookList.FindNoteBook(OldName);
    if NB <> nil then
        NB^.Name:= NewName
    else Result := False;
end;

procedure TNoteLister.AddNoteBook(const ID, ANoteBook: ANSIString; IsTemplate : Boolean);
begin
    NoteBookList.Add(ID, ANoteBook, IsTemplate);
end;

procedure TNoteLister.LoadNotebookGrid(const Grid: TStringGrid; const NotebookName: AnsiString);
var
    Index : integer;
begin
    while Grid.RowCount > 1 do Grid.DeleteRow(Grid.RowCount-1);
    Index := NoteList.Count;
    while Index > 0 do begin
        dec(Index);
        if NotebookList.IDinNotebook(NoteList.Items[Index]^.ID, NoteBookName) then begin
        	Grid.InsertRowWithValues(Grid.RowCount, [NoteList.Items[Index]^.Title,
        			NoteList.Items[Index]^.LastChange]);
		end;
	end;
    {
  	Grid.Clear;
    //Grid.Clean;
    Grid.FixedRows := 0;
    Grid.InsertRowWithValues(0, ['Title', 'Last Change', 'Create Date', 'File Name']);
    Grid.FixedRows := 1;
    // Scan the main list of notes looking for ones in this Notebook.
    Index := NoteList.Count;     // start at end of list to save sorting
    while Index > 0 do begin
        dec(Index);
        if NotebookList.IDinNotebook(NoteList.Items[Index]^.ID, NoteBookName) then begin
        	Grid.InsertRowWithValues(Grid.RowCount, [NoteList.Items[Index]^.Title,
        			NoteList.Items[Index]^.LastChange, NoteList.Items[Index]^.CreateDate,
            		NoteList.Items[Index]^.ID]);
		end;
	end;
    Grid.AutoSizeColumns;  }
end;

function TNoteLister.NotebookTemplateID(const NotebookName: ANSIString): AnsiString;
var
    Index : integer;
    St : string;
begin
    for Index := 0 to NotebookList.Count - 1 do begin
        St := NotebookList.Items[Index]^.Name;
        if NotebookName = NotebookList.Items[Index]^.Name then begin
            Result := NotebookList.Items[Index]^.Template;
            exit();
		end;
	end;
    debugln('ERROR - asked for the template for a non existing Notebook');
    Result := '';
end;


procedure TNoteLister.DeleteNoteBookwithID(FileorID: AnsiString);
var
    Index : integer;
begin
    for Index := 0 to NotebookList.Count - 1 do begin
        if CleanFileName(FileorID) = NotebookList.Items[Index]^.Template then begin
          	NotebookList.Items[Index]^.Notes.free;
    		dispose(NotebookList.Items[Index]);
            NotebookList.Delete(Index);
            exit();
		end;
	end;
    debugln('ERROR - asked to delete a notebook by ID but cannot find it.');
end;


function TNoteLister.IsATemplate(FileOrID: AnsiString): boolean;
var
    SL : TStringList;
begin
	SL := TStringList.Create;
    Result := GetNotebooks(SL, CleanFileName(FileOrID));
    SL.Free;
end;

procedure TNoteLister.SetNotebookMembership(const ID : ansistring; const MemberList : TStringList);
var
    Index, BookIndex : integer;
begin
    // First, remove any mention of this ID from data structure
	for Index := 0 to NotebookList.Count - 1 do begin
        BookIndex := 0;
        while BookIndex < NotebookList.Items[Index]^.Notes.Count do begin
            if ID = NotebookList.Items[Index]^.Notes[BookIndex] then
            	NotebookList.Items[Index]^.Notes.Delete(BookIndex);
            inc(BookIndex);
        end;
	end;
	// Now, put back the ones we want there.
    for BookIndex := 0 to MemberList.Count -1 do
        for Index := 0 to NotebookList.Count - 1 do
            if MemberList[BookIndex] = NotebookList.Items[Index]^.Name then begin
                NotebookList.Items[Index]^.Notes.Add(ID);
                break;
            end;
end;

procedure TNoteLister.LoadStGridNotebooks(const NotebookGrid : TStringGrid; SearchListOnly : boolean);
var
    Index : integer;

    function FindInSearchList(NB : PNoteBook) : boolean;
    var  X : integer = 0;
    begin
        result := true;
        if Nil = SearchNoteList then
            exit;
        while X < NB^.Notes.Count do begin
            if Nil <> SearchNoteList.FindID(NB^.Notes[X]) then
                exit;
            inc(X);
        end;
        result := false;
    end;

begin
    while NotebookGrid.RowCount > 1 do NotebookGrid.DeleteRow(NotebookGrid.RowCount-1);
    for Index := 0 to NotebookList.Count - 1 do begin
        if (not SearchListOnly) or FindInSearchList(NotebookList.Items[Index]) then begin
            NotebookGrid.InsertRowWithValues(NotebookGrid.RowCount, [NotebookList.Items[Index]^.Name]);
        end;
	end;


    {
    NotebookGrid.Clear;
    NotebookGrid.FixedRows:=0;
    NotebookGrid.InsertRowWithValues(0, ['Notebooks']);
    NotebookGrid.FixedRows:=1;
    for Index := 0 to NotebookList.Count - 1 do begin
        if (not SearchListOnly) or FindInSearchList(NotebookList.Items[Index]) then begin
            NotebookGrid.InsertRowWithValues(NotebookGrid.RowCount, [NotebookList.Items[Index]^.Name]);
        end;
	end;
    NotebookGrid.AutoSizeColumns; }
end;

function TNoteLister.GetNotebooks(const NBList: TStringList; const ID: ANSIString): boolean;
var
    Index, I : Integer;
begin
	Result := false;
 	for Index := 0 to NoteBookList.Count -1 do begin
      	if ID = '' then
            NBList.Add(NotebookList.Items[Index]^.Name)
        else begin
            if ID = NotebookList.Items[Index]^.Template then begin
                // The passed ID is the ID of a Template itself, not a note.
                // debugln('Looks like we asking about a template ' + ID);
                NBList.Add(NotebookList.Items[Index]^.Name);
                if NBList.Count > 1 then
                    debugln('Error, seem to have more than one Notebook Name for template ' + ID);
                Result := True;
                exit();
			end;
            // OK, if its not a Template, its a note, what notebooks is it a member of ?
            // Each NotebookList item has a list of the notes that are members of that item.
            // if the ID is mentioned in the items note list, copy name to list.
            // Iterate over the Notes list associated with this particular Notebook entry.
			for I := 0 to NotebookList.Items[Index]^.Notes.Count -1 do
            	if ID = NotebookList.Items[Index]^.Notes[I] then
                	NBList.Add(NotebookList.Items[Index]^.Name);
            {if assigned(NBList) then debugln('ERROR, assigned SL passed to GetNotebooks')
            else  NBList := NotebookList.Items[Index]^.Notes; }
        end;
	end;
end;

{ -------------- Things relating to Notes -------------------- }

// Address of this function is passed to note list sort. Newest notes at end of list.
function LastChangeSorter( Item1: Pointer;   Item2: Pointer) : Integer;
begin
    // Also ANSICompareStr but we are just looking at date numbers here
	result := CompareStr(PNote(Item1)^.LastChange, PNote(Item2)^.LastChange);
end;

procedure TNoteLister.RewriteBadChangeDate(const Dir, FileName, LCD : ANSIString);
var
    InFile, OutFile: TextFile;
    InString : string;
    {$ifdef WINDOWS}
    ErrorMsg : ANSIString;
    {$endif}
begin
    // Bad format looks like this 2020-03-06 21:25:18
    // But it Should be like this 2020-02-15T12:07:41.0000000+00:00

    AssignFile(InFile, Dir + FileName);
    AssignFile(OutFile, Dir + Filename + '-Dated');
    try
        try
            Reset(InFile);
            Rewrite(OutFile);
            while not eof(InFile) do begin
                readln(InFile, InString);
                if (Pos('<last-change-date>', InString) > 0) then
                    writeln(OutFile, '  <last-change-date>'
                                // + copy(LCD, 1, 10) + 'T' + copy(LCD, 12, 8) + '.1000000+00:00'
                                + GetCurrentTimeStr()
                                + '</last-change-date>')
                else writeln(OutFile, InString);
		    end;
        finally
            CloseFile(OutFile);
            CloseFile(InFile);
        end;
    except
        on E: EInOutError do begin
                debugln('File handling error occurred updating clean note location. Details: ' + E.Message);
                exit;
            end;
    end;
    {$ifdef WINDOWS}
        if FileExists(Dir + FileName) then    // will not be there if its a new note.
            if not SafeWindowsDelete(Dir + FileName, ErrorMsg) then         // In syncutils, maybe over kill but ......
               exit;
    {$endif}
    RenameFileUTF8(Dir + Filename + '-Dated', Dir + FileName);    // Unix ok to over write, windows is not !
end;


procedure TNoteLister.GetNoteDetails(const Dir, FileName: ANSIString; const TermList : TStringList; DontTestName : boolean = false);
			// This is how we search for XML elements, attributes are different.
var
    NoteP : PNote;
    Doc : TXMLDocument;
	Node : TDOMNode;
    J : integer;
    TryCount : integer =0;             // only try rewriting bad last-change-date once.
    //LCD_OK : boolean = false;
begin
    // debugln('Checking note ', FileName);
    if not DontTestName then
        if not NoteIDLooksOK(copy(FileName, 1, 36)) then begin      // In syncutils !!!!
            ErrorNotes.Append(FileName + ', ' + 'Invalid ID in note filename');
            XMLError := True;
            exit;
        end;
  	if FileExistsUTF8(Dir + FileName) then begin
        if assigned(TermList) then
            if not NoteContains(TermList, FileName) then exit();
        new(NoteP);
        NoteP^.IsTemplate := False;
  	    try
	        try
                NoteP^.ID:=FileName;

                repeat
	                ReadXMLFile(Doc, Dir + FileName);
	  	            Node := Doc.DocumentElement.FindNode('title');
	      	        NoteP^.Title := Node.FirstChild.NodeValue;          // This restores & etc.
	                    //if DebugMode then Debugln('Title is [' + Node.FirstChild.NodeValue + ']');
	                Node := Doc.DocumentElement.FindNode('last-change-date');
	                NoteP^.LastChange := Node.FirstChild.NodeValue;
	                if (length(NoteP^.LastChange) <> 33) {and DebugMode} then begin
	                    RewriteBadChangeDate(Dir, FileName, NoteP^.LastChange);
                        inc(TryCount);
                        if TryCount > 2 then begin
                            debugln('Failed to fix bad last-change-date in ' +  NoteP^.Title);
                            break;     // sad but life must go on.
						end;
                        Doc.free;
					end else
                        break;
				until false;

                NoteP^.OpenNote := nil;
                Node := Doc.DocumentElement.FindNode('create-date');
                NoteP^.CreateDate := Node.FirstChild.NodeValue;
                Node := Doc.DocumentElement.FindNode('open-on-startup');
                NoteP^.OpenOnStart:= (Node.FirstChild.NodeValue = 'True');
                Node := Doc.DocumentElement.FindNode('tags');
                if Assigned(Node) then begin
                    for J := 0 to Node.ChildNodes.Count-1 do
                        if UTF8pos('system:template', Node.ChildNodes.Item[J].TextContent) > 0 then
                                NoteP^.IsTemplate := True;
                    for J := 0 to Node.ChildNodes.Count-1 do
                        if UTF8pos('system:notebook', Node.ChildNodes.Item[J].TextContent) > 0 then begin
                            NoteBookList.Add(Filename, UTF8Copy(Node.ChildNodes.Item[J].TextContent, 17, 1000), NoteP^.IsTemplate);
                            // debugln('Notelister #691 ' +  UTF8Copy(Node.ChildNodes.Item[J].TextContent, 17,1000));
                        end;
                                // Node.ChildNodes.Item[J].TextContent) may be something like -
                                // * system:notebook:DavosNotebook - this note belongs to DavosNotebook
                                // * system:template - this note is a template, if does not also have a
                                // Notebook tag its the StartHere note, otherwise its the Template for
                                // for the mentioned Notebook.
		        end;
            except 	on E: EXMLReadError do begin
                                DebugLn('XML ERROR' + E.Message);
                                XMLError := True;
                                dispose(NoteP);
                                ErrorNotes.Append(FileName + ', ' + E.Message);
                                exit();
                            end;
            		    on EAccessViolation do DebugLn('Access Violation ' + FileName);
  	        end;
                if NoteP^.IsTemplate then begin    // Don't show templates in normal note list
                    dispose(NoteP);
                    exit();
			    end;
			    if assigned(TermList) then
                    SearchNoteList.Add(NoteP)
                else NoteList.Add(NoteP);
  	    finally
      	        Doc.free;
  	    end;
    end else DebugLn('Error, found a note and lost it !');
end;


function TNoteLister.RemoveXml(const St : AnsiString) : AnsiString;
var
    X, Y : integer;
    FoundOne : boolean = false;
begin
    Result := St;
    repeat
        FoundOne := False;
        X := UTF8Pos('<', Result);
        if X > 0 then begin
            Y := UTF8Pos('>', Result);
            if Y > 0 then begin
                UTF8Delete(Result, X, Y-X+1);
                FoundOne := True;
            end;
        end;
    until not FoundOne;
end;

procedure TNoteLister.IndexThisNote(const ID: String);
begin
    GetNoteDetails(NotesDir, CleanFileName(ID), TStringList(nil));
end;


function TNoteLister.GetLastChangeDate(const ID: String) : string;
var
    index : integer;
    FileName : string;
begin
    Result := '';
    if not assigned(NoteList) then exit('');
    FileName := CleanFileName(ID);
    for Index := 0 to NoteList.Count -1 do
    if NoteList.Items[Index]^.ID = FileName then begin
        exit(NoteList.Items[Index]^.LastChange);
		debugln('NoteLister #759 from list '  + NoteList.Items[Index]^.LastChange);
    end;
end;

function TNoteLister.IsIDPresent(ID: string): boolean;
var
    FileName : string;
    index : integer;
begin
    Result := False;
    FileName := CleanFileName(ID);
    for Index := 0 to NoteList.Count -1 do
        if NoteList.Items[Index]^.ID = FileName then
            exit(True);
end;

function TNoteLister.FindFirstOpenNote(): TForm;
begin
    OpenNoteIndex:=0;
    while OpenNoteIndex < NoteList.Count do
        if NoteList.Items[OpenNoteIndex]^.OpenNote <> nil then
            exit(NoteList.Items[OpenNoteIndex]^.OpenNote)
        else inc(OpenNoteIndex);
    result := nil;
    OpenNoteIndex := -1;
end;

function TNoteLister.FindNextOpenNote(): TForm;
begin
    if OpenNoteIndex < 0 then exit(Nil);
    inc(OpenNoteIndex);
    while OpenNoteIndex < NoteList.Count do
        if NoteList.Items[OpenNoteIndex]^.OpenNote <> nil then
            exit(NoteList.Items[OpenNoteIndex]^.OpenNote)
        else inc(OpenNoteIndex);
    result := nil;
    OpenNoteIndex := -1;
end;

function TNoteLister.FindFirstOOSNote(out NTitle, NID : ANSIstring): boolean;
begin
    OpenNoteIndex:=0;
    while OpenNoteIndex < NoteList.Count do
        if NoteList.Items[OpenNoteIndex]^.OpenOnStart then begin
            NTitle := NoteList.Items[OpenNoteIndex]^.Title;
            NID := NoteList.Items[OpenNoteIndex]^.ID;
            exit(True)
        end
        else inc(OpenNoteIndex);
    result := False;
    OpenNoteIndex := -1;
end;

function TNoteLister.FindNextOOSNote(var NTitle, NID : ANSIstring): boolean;
begin
    if OpenNoteIndex < 0 then exit(False);
    inc(OpenNoteIndex);
    while OpenNoteIndex < NoteList.Count do
        if NoteList.Items[OpenNoteIndex]^.OpenOnStart then begin
            NTitle := NoteList.Items[OpenNoteIndex]^.Title;
            NID := NoteList.Items[OpenNoteIndex]^.ID;
            exit(True)
        end
        else inc(OpenNoteIndex);
    result := False;
    OpenNoteIndex := -1;
end;

procedure TNoteLister.BuildSearchList(SL : TStringList; const Term : AnsiString);
var
    I : integer = 1;
    AWord : string = '';
    InCommas : boolean = false;
begin
    // If AnyCombination is not checked, we do the allow sections in inverted commas to be treated as one word.
    while I <= length(trim(Term)) do begin
        if Term[i] = '"' then begin
            if InCommas then begin
                SL.add(AWord);
                AWord := '';
                InCommas := False;
            end else begin
                InCommas := true;
            end;
            inc(I);
            continue;
        end;
        if Term[i] = ' ' then begin
            if InCommas then
                AWord := AWord + Term[i]
            else begin
                if AWord <> '' then begin
                    SL.Add(AWord);
                    AWord := '';
                end;
            end;
            inc(I);
            continue;
        end;
        AWord := AWord + Term[i];
        inc(i);
        continue;
    end;
    if AWord <> '' then
        SL.Add(AWord);
end;

// Jan 2020, will pass this function a (possibly uncreated) TStringList rather than a string
// The list will be ready to search on.
// function TNoteLister.NoteContains(const Term, FileName: ANSIString): boolean;
function TNoteLister.NoteContains(const TermList : TStringList; FileName: ANSIString): boolean;
var
    {SL, }SLNote : TStringList;
    //Content : ANSIString;
    I, Index : integer;
begin
    Result := False;
    {SL := TStringList.Create;
    BuildSearchList(SL, Term);      }

    SLNote := TStringList.Create;
    SlNote.LoadFromFile(GetLocalNoteFile(FileName)); // MUST BE ID !

    for Index := 0 to SLNote.Count - 1 do
        SLNote.Strings[Index] := RemoveXML(SLNote.Strings[Index]);
    for I := 0 to TermList.Count -1 do begin      // Iterate over search terms
        Result := False;
        for Index := 0 to SLNote.Count - 1 do begin // Check each line of note for a match against current word.
            if  Sett.CheckCaseSensitive.Checked then begin
                if (UTF8Pos(TermList.Strings[I], SLNote.Strings[Index]) > 0) then begin
                    Result := True;
                    break;
                end;
            end else
                if (UTF8Pos(UTF8LowerString(TermList.Strings[I]), UTF8LowerString(SLNote.Strings[Index])) > 0) then begin
                    Result := True;
                    break;
                end;
        end;
        if not Result then break;  // if failed to turn Result on for first word, no point in continuing
    end;
    // when we get here, if Result is true, run finished without a fail.
    FreeandNil(SLNote);
    // FreeandNil(SL);
end;


procedure TNoteLister.AddNote(const FileName, Title, LastChange : ANSIString);
var
    NoteP : PNote;
begin
    new(NoteP);
    NoteP^.ID := CleanFilename(FileName);
    NoteP^.LastChange := LastChange; {copy(LastChange, 1, 19); }
    //NoteP^.LastChange[11] := ' ';
    NoteP^.CreateDate := LastChange; {copy(LastChange, 1, 19); }
    //NoteP^.CreateDate[11] := ' ';
    NoteP^.Title:= Title;
    NoteP^.OpenNote := nil;
    NoteList.Add(NoteP);
    // We don't need to re-sort here, the new note is added at the end, and our
    // list is sorted, newest towards the end. All good.
end;

function TNoteLister.Count(): integer;
{var
    P : pointer;     }
begin
    Result := NoteList.Count;
    {
    Result := 0;
    for P in NoteList do
      inc(Result); }
end;

function TNoteLister.GetTitle(Index : integer) : string;
begin
    Result := PNote(NoteList.get(Index))^.Title;
end;

function TNoteLister.GetNotes(const Term: ANSIstring = ''; DontTestName : boolean = false): longint;
var
    Info : TSearchRec;
    SL : TStringList;
    P : pointer;
    //Tick, Tock : qword;
begin
    SL := Nil;
    if Term <> '' then begin
        SL := TStringList.Create;
        BuildSearchList(SL, Term);
    end;
    XMLError := False;
    if Term = '' then begin
        NoteList.Free;
    	NoteList := TNoteList.Create;
        NoteBookList.Free;
        NoteBookList := TNoteBookList.Create;
        debugln('Empty Note and Book Lists created');
    end else begin
        debugln('Empty Search Lists created');
        SearchNoteList.Free;
    	SearchNoteList := TNoteList.Create;
    end;
    FreeandNil(ErrorNotes);
    ErrorNotes := TStringList.Create;
    debugln('Looking for notes in [' + NotesDir + ']');
  if FindFirst(GetLocalNoteFile('*'), faAnyFile and faDirectory, Info)=0 then begin
  		repeat
  			GetNoteDetails(NotesDir, Info.Name, SL, DontTestName);        // Note: SL may, or may not have been created
  		until FindNext(Info) <> 0;
  	end;
  	FindClose(Info);
    debugLn('Finished indexing notes');
    if Term = '' then begin
        NotebookList.CleanList();
        Result := NoteList.Count;
        NoteList.Sort(@LastChangeSorter);       // 0mS on Dell
    	end else begin
        SearchNoteList.Sort(@LastChangeSorter);
		result := NoteList.Count;
	end;
    if assigned(SL) then SL.Free;       //  needs a try statement ....
end;


procedure TNoteLister.LoadStGrid(const Grid : TStringGrid; NoCols : integer; SearchMode : boolean = false);
var
    Index : integer;
    TheList : TNoteList;
    LCDst : string;
begin
    if SearchMode then
        TheList := SearchNoteList
    else TheList := NoteList;
    while Grid.RowCount > 1 do Grid.DeleteRow(Grid.RowCount-1);
    Index := TheList.Count;
    while Index > 0 do begin
        dec(Index);
        LCDst := TheList.Items[Index]^.LastChange;
        LCDst[11] := ' ';   // looks prettier, dates are stored in ISO std with a 'T' between date and time
        case NoCols of
            2 : Grid.InsertRowWithValues(Grid.RowCount, [TheList.Items[Index]^.Title,
        	    LCDst]);
            3 : Grid.InsertRowWithValues(Grid.RowCount, [TheList.Items[Index]^.Title,
        	    LCDst, TheList.Items[Index]^.CreateDate]);
            4 : Grid.InsertRowWithValues(Grid.RowCount, [TheList.Items[Index]^.Title,
                LCDst, TheList.Items[Index]^.CreateDate, TheList.Items[Index]^.ID]);
        end;
    end;
    if Grid.SortColumn > -1 then
        Grid.SortColRow(True, Grid.SortColumn);
end;

function TNoteLister.AlterNote(ID, Change: ANSIString; Title: ANSIString): boolean;
var
    Index : integer;
begin
	result := False;
    for Index := 0 to NoteList.Count -1 do begin
        if CleanFilename(ID) = NoteList.Items[Index]^.ID then begin
        	if Title <> '' then
            	NoteList.Items[Index]^.Title := Title;
        	if Change <> '' then begin
                NoteList.Items[Index]^.LastChange := Change;  {copy(Change, 1, 19);}
//            	NoteList.Items[Index]^.LastChange[11] := ' ';                   // keep list in ISO format, make pretty when displaying
                // check if note is already at the bottom of the list, don't need to re-sort.
                if (Index < (NoteList.Count -1)) then
                    NoteList.Sort(@LastChangeSorter);
            end;
            exit(True);
		end;
	end;
end;

function TNoteLister.IsThisATitle(const Title: ANSIString): boolean;
var
    Index : integer;
begin
  	Result := False;
	for Index := 0 to NoteList.Count -1 do begin
        if Title = NoteList.Items[Index]^.Title then begin
        	Result := True;
            break;
		end;
	end;
end;

function TNoteLister.CleanFileName(const FileOrID : AnsiString) : ANSIString;
begin
  	if length(ExtractFileNameOnly(FileOrID)) = 36 then
        Result := ExtractFileNameOnly(FileOrID) + '.note'
    else
        Result := ExtractFileNameOnly(FileOrID);
end;

function TNoteLister.IsThisNoteOpen(const ID: ANSIString; out TheForm : TForm): boolean;
var
    Index : integer;
begin
  	Result := False;
    TheForm := Nil;
	for Index := 0 to NoteList.Count -1 do begin
        if CleanFileName(ID) = NoteList.Items[Index]^.ID then begin
        	TheForm := NoteList.Items[Index]^.OpenNote;
            Result := not (NoteList.Items[Index]^.OpenNote = Nil);
            break;
		end;
	end;
end;

procedure TNoteLister.ThisNoteIsOpen(const ID : ANSIString; const TheForm: TForm);
var
    Index : integer;
    //cnt : integer;
begin
    if NoteList = NIl then
        exit;
    if NoteList.Count < 1 then begin
        //DebugLn('Called ThisNoteIsOpen() with empty but not NIL list. Count is '
        //		+ inttostr(NoteList.Count) + ' ' + ID);
        // Occasionally I think we see a non reproducable error here.
        // I believe is legal to start the for loop below with an empty list but ....
        // When we are creating the very first note in a dir, this haappens. Count should be exactly zero.
	end;
	//cnt := NoteList.Count;
	for Index := 0 to NoteList.Count -1 do begin
      	//writeln('ID = ', ID, ' ListID = ', NoteList.Items[Index]^.ID);
        if CleanFileName(ID) = NoteList.Items[Index]^.ID then begin
            NoteList.Items[Index]^.OpenNote := TheForm;
            break;
		end;
	end;
    // if Index = (NoteList.Count -1) then DebugLn('Failed to find ID in List ', ID);
end;

function TNoteLister.FileNameForTitle(const Title: ANSIString; out FileName : ANSIstring): boolean;
var
    Index : integer;
begin
    FileName := '';
  	Result := False;
	for Index := 0 to NoteList.Count -1 do begin
        if lowercase(Title) = lowercase(NoteList.Items[Index]^.Title) then begin
            FileName := NoteList.Items[Index]^.ID;
        	Result := True;
            break;
		end;
	end;
end;

procedure TNoteLister.StartSearch();
begin
	SearchIndex := 0;
end;

function TNoteLister.NextNoteTitle(out SearchTerm: ANSIString): boolean;
begin
  	Result := False;
	if SearchIndex < NoteList.Count then begin
    	SearchTerm := NoteList.Items[SearchIndex]^.Title;
    	inc(SearchIndex);
        Result := True;
	end;
end;

function TNoteLister.DeleteNote(const ID: ANSIString): boolean;
var
    Index : integer;
    // TestID : ANSIString;
begin
    {if length(ID) = 36 then
        TestID := ID + '.note'
    else
    	TestID := ID;  }
	result := False;
    for Index := 0 to NoteList.Count -1 do begin
        if CleanFileName(ID) = NoteList.Items[Index]^.ID then begin
        	dispose(NoteList.Items[Index]);
        	NoteList.Delete(Index);
        	Result := True;
        	break;
		end;
	end;
    if Result = false then
        DebugLn('Failed to remove ref to note in NoteLister ', ID);
end;

constructor TNoteLister.Create;
begin
    SearchNoteList := nil;
    NoteList := nil;
    NoteBookList := Nil;
    ErrorNotes := Nil;
end;


destructor TNoteLister.Destroy;
begin
    NoteBookList.Free;
    NoteBookList := Nil;
    SearchNoteList.Free;
    SearchNoteList := Nil;
    NoteList.Free;
    NoteList := Nil;
    ErrorNotes.Free;
    ErrorNotes := Nil;
	inherited Destroy;
end;

{  =========================  TNoteList ====================== }


destructor TNoteList.Destroy;
var
  I : integer;
begin
    // DebugLn('NoteList - disposing of items x ' + inttostr(Count));
	for I := 0 to Count-1 do begin
    	dispose(Items[I]);
	end;
	inherited Destroy;
end;

function TNoteList.Add(ANote: PNote): integer;
begin
    result := inherited Add(ANote);
end;

function TNoteList.FindID(const ID: ANSIString): PNote;
var
    Index : longint;
begin
    Result := Nil;
    for Index := 0 to Count-1 do begin
        if Items[Index]^.ID = ID then begin
            Result := Items[Index];
            exit()
		end;
	end;
end;

function TNoteList.Get(Index: integer): PNote;
begin
    Result := PNote(inherited get(Index));
end;


end.
