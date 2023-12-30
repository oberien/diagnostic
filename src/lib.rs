use std::fmt;
use std::cell::RefCell;
use std::ops::{Deref, Range};
use std::borrow::Borrow;
use std::sync::atomic::{AtomicUsize, Ordering};

use codespan_reporting::files::{SimpleFile, Files, Location, Error};
use codespan_reporting::diagnostic::{Severity, Diagnostic, Label, LabelStyle};
use codespan_reporting::term::{self, Config, termcolor::{StandardStream, ColorChoice}};
use appendlist::AppendList;
use codespan_reporting::term::termcolor::Buffer;

pub trait ErrorCode: Clone {
    fn code(&self) -> String;
    fn message(&self) -> String;
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Span {
    pub file: FileId,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub const fn new(file: FileId, start: usize, end: usize) -> Span {
        Span { file, start, end }
    }

    /// Returns a new span starting from `by` bytes before the old span
    ///
    /// # Panics
    ///
    /// Panics if the new start is <0.
    #[must_use = "`Span::extend_start` returns a new `Span`"]
    pub fn extend_start(&self, by: usize) -> Span {
        Span {
            file: self.file,
            start: self.start.checked_sub(by).unwrap(),
            end: self.end,
        }
    }
    /// Returns a new span ending `by` bytes after the old span
    ///
    /// # Panics
    ///
    /// Panics if the new end doesn't fit `usize`.
    #[must_use = "`Span::extend_end` returns a new `Span`"]
    pub fn extend_end(&self, by: usize) -> Span {
        Span {
            file: self.file,
            start: self.start,
            end: self.end.checked_sub(by).unwrap(),
        }
    }

    /// Returns a new span with `end: start + new_len`.
    #[must_use = "`Span::new_len` returns a new `Span`"]
    pub fn with_len(&self, new_len: usize) -> Span {
        Span {
            file: self.file,
            start: self.start,
            end: self.start + new_len,
        }
    }

    #[must_use = "`Span::map_start` returns a new `Span`"]
    pub fn map_start<F: FnOnce(usize) -> usize>(&self, f: F) -> Span {
        Span {
            file: self.file,
            start: f(self.start),
            end: self.end,
        }
    }
    #[must_use = "`Span::map_end` returns a new `Span`"]
    pub fn map_end<F: FnOnce(usize) -> usize>(&self, f: F) -> Span {
        Span {
            file: self.file,
            start: self.start,
            end: f(self.end),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Spanned<T> {
        Spanned { value, span }
    }

    pub fn as_ref(&self) -> Spanned<&T> {
        Spanned {
            value: &self.value,
            span: self.span,
        }
    }

    pub fn map<R>(self, f: impl FnOnce(T) -> R) -> Spanned<R> {
        let Spanned  { value, span } = self;
        Spanned {
            value: f(value),
            span,
        }
    }
}

// codespan_reporting::SimpleFiles requires `&mut self` when adding a file,
// making it impossible to have an immutable interface to put a string in it
// and return an immutable &str.
// Thus, here we implement the same thing, but use an Arena instead of a Vec, to allow
// an immutable interface.
struct EvenSimplerFiles {
    /// regular files added via name and content
    files: AppendList<SimpleFile<String, String>>,
    /// synthetic named files with a `&'static str` as name
    synthetic_named_files: AppendList<(&'static str, SimpleFile<String, String>)>,
    /// synthetic numbered files in insertion order (numbers unordered and possibly with holes)
    synthetic_numbered_files: AppendList<(usize, SimpleFile<String, String>)>,
}
impl EvenSimplerFiles {
    fn new() -> Self {
        EvenSimplerFiles {
            files: AppendList::new(),
            synthetic_named_files: AppendList::new(),
            synthetic_numbered_files: AppendList::new(),
        }
    }
    fn add(&self, name: String, source: String) -> FileId {
        let idx = self.files.len();
        self.files.push(SimpleFile::new(name, source));
        FileId(FileIdKind::File(idx))
    }
    fn add_synthetic_named(&self, name: &'static str, source: String) -> FileId {
        let id = FileId(FileIdKind::SyntheticNamed(name));
        assert!(self.get_optional(id).is_none(), "synthetic named file with name {} already added previously", name);
        self.synthetic_named_files.push((name, SimpleFile::new(name.to_string(), source)));
        id
    }
    fn add_synthetic_numbered(&self, id: FileId, name: String, source: String) {
        let num = match id.0 {
            FileIdKind::SyntheticNumbered(num) => num,
            _ => panic!("tried to add synthetic numbered file but passed non-synthetic-numbered-FileId `{}`", id),
        };
        assert!(self.get_optional(id).is_none(), "synthetic numbered file {} already added previously", id);
        self.synthetic_numbered_files.push((num, SimpleFile::new(name, source)));
    }
    fn get(&self, id: FileId) -> Result<&SimpleFile<String, String>, Error> {
        self.get_optional(id).ok_or(Error::FileMissing)
    }
    fn get_optional(&self, id: FileId) -> Option<&SimpleFile<String, String>> {
        match id.0 {
            FileIdKind::File(idx) => self.files.get(idx),
            FileIdKind::SyntheticNamed(name) => self.synthetic_named_files.iter().find_map(|(n, f)| (*n == name).then_some(f)),
            FileIdKind::SyntheticNumbered(num) => self.synthetic_numbered_files.iter().find_map(|(n, f)| (*n == num).then_some(f)),
        }
    }
}
impl<'a> Files<'a> for EvenSimplerFiles {
    type FileId = FileId;
    type Name = &'a str;
    type Source = &'a str;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, Error> {
        Ok(&self.get(id)?.name())
    }

    fn source(&'a self, id: Self::FileId) -> Result<Self::Source, Error> {
        Ok(&self.get(id)?.source())
    }

    fn line_index(&'a self, id: Self::FileId, byte_index: usize) -> Result<usize, Error> {
        self.get(id)?.line_index((), byte_index)
    }

    fn line_number(&'a self, id: Self::FileId, line_index: usize) -> Result<usize, Error> {
        self.get(id)?.line_number((), line_index)
    }

    fn column_number(&'a self, id: Self::FileId, line_index: usize, byte_index: usize) -> Result<usize, Error> {
        self.get(id)?.column_number((), line_index, byte_index)
    }

    fn location(&'a self, id: Self::FileId, byte_index: usize) -> Result<Location, Error> {
        self.get(id)?.location((), byte_index)
    }

    fn line_range(&'a self, id: Self::FileId, line_index: usize) -> Result<Range<usize>, Error> {
        self.get(id)?.line_range((), line_index)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct FileId(FileIdKind);
#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
enum FileIdKind {
    File(usize),
    SyntheticNamed(&'static str),
    SyntheticNumbered(usize),
}
static SYNTHETIC_NUMBERED_COUNTER: AtomicUsize = AtomicUsize::new(0);
impl FileId {
    pub const fn synthetic_named(path: &'static str) -> FileId {
        FileId(FileIdKind::SyntheticNamed(path))
    }
    pub fn new_synthetic_numbered() -> FileId {
        FileId(FileIdKind::SyntheticNumbered(SYNTHETIC_NUMBERED_COUNTER.fetch_add(1, Ordering::SeqCst)))
    }
}
impl fmt::Display for FileId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            FileIdKind::File(id) => write!(f, "file {id}"),
            FileIdKind::SyntheticNamed(name) => write!(f, "synthetic named {name:?}"),
            FileIdKind::SyntheticNumbered(id) => write!(f, "synthetic numbered: {id}"),
        }
    }
}

pub struct Output {
    typ: OutputType,
}
enum OutputType {
    StandardStream(StandardStream),
    Buffered(Box<dyn Fn(String)>),
}
impl Output {
    pub fn stdout() -> Output {
        Output { typ: OutputType::StandardStream(StandardStream::stdout(ColorChoice::Auto)) }
    }
    pub fn stderr() -> Output {
        Output { typ: OutputType::StandardStream(StandardStream::stderr(ColorChoice::Auto)) }
    }
    pub fn buffered(f: impl Fn(String) + 'static) -> Output {
        Output { typ: OutputType::Buffered(Box::new(f)) }
    }
}

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Emitted<E: ErrorCode> {
    Bug(E),
    Error(E),
    Warning(E),
    Note(E),
    Help(E),
}

/// There are 3 different ways to add files:
/// 1. Regular files added via name and content:
///    Usually when creating diagnostics, you have a filename and file content.
///    You add those after reading the file via `Diagnostics::add_file`, which returns a reference
///    to the file content which lives as long as the diagnostics.
///    You operate on that reference, e.g. parse code creating spans for each token and expression.
///    You use those spans to generate diagnostics in case of errors or warnings.
/// 2. Synthetic Named Files:
///    In some cases it's not possible to add all of the code before getting the FileId.
///    For example in static or constant code it's impossible to bind something to the lifetime of
///    the Diagnostic, e.g. when generating code in a derive macro.
///    Instead, `FileId::synthetic(&'static str)` can be used to refer to a file that hasn't yet
///    been added.
///    However, `Diagnostics::add_synthetic_named_file` must be used to associate the synthetic
///    named file to its content / source before any access to the synthetic file's code is
///    performed - otherwise any access like printing diagnostics will result in a panic.
/// 3. Synthetic Numbered Files:
///    Another use-case is to have regular rust code generate code to be added as a future file.
///    For example when performing AST transformations, new source code is generated while new
///    expressions are created.
///    In those cases the Spans and code must be generated at the same time.
///    Regular file-ids can't be used as the code would need to be fully known before any span can
///    be created.
///    Synthetic files might not be suitable if the file-name depends on other factors.
///    In those cases synthetic numbered files can be used.
///    Using `FileId::new_synthetic_numbered` it's possible to get a file-id before the code is known.
///    Once all spans are created and the code generated, the code can be added to the diagnostics
///    via `Diagnostics::add_synthetic_numbered_file`.
///    That function must be called before any of the spans is used, as the file-id is otherwise
///    unknown and printing diagnostics will panic.
pub struct Diagnostics<E: ErrorCode> {
    files: EvenSimplerFiles,
    output: Output,
    config: Config,
    emitted: RefCell<Vec<Emitted<E>>>,
}
impl<E: ErrorCode> fmt::Debug for Diagnostics<E> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Diagnostics")
            .field("...", &"...")
            .finish()
    }
}

impl<E: ErrorCode> Diagnostics<E> {
    pub fn new() -> Diagnostics<E> {
        Self::with_output(Output::stderr())
    }
    pub fn with_output(output: Output) -> Diagnostics<E> {
        Diagnostics {
            files: EvenSimplerFiles::new(),
            output,
            config: Config::default(),
            emitted: RefCell::new(Vec::new()),
        }
    }

    pub fn add_file(&self, name: String, source: String) -> (FileId, &str) {
        let idx = self.files.add(name, source);
        let source = self.files.get(idx).unwrap().source();
        (idx, source)
    }
    pub fn add_synthetic_named_file(&self, name: &'static str, source: String) -> (FileId, &str) {
        let id = self.files.add_synthetic_named(name, source);
        let source = self.files.get(id).unwrap().source();
        (id, source)
    }
    pub fn add_synthetic_numbered_file(&self, id: FileId, name: String, source: String) -> (FileId, &str) {
        self.files.add_synthetic_numbered(id, name, source);
        let source = self.files.get(id).unwrap().source();
        (id, source)
    }

    /// Gets the file content as `&str`. Panics if the file id doesn't exist.
    pub fn get_file(&self, file_id: FileId) -> &str {
        self.files.source(file_id).unwrap()
    }

    /// Get file name of given file id. Panics if the file id doesn't exist.
    pub fn file_name(&self, file_id: FileId) -> &str {
        self.files.name(file_id).unwrap()
    }

    /// Transforms a line and column (both 1-indexed) inside a file to their byte-offset.
    /// Panics if the file id doesn't exist.
    pub fn resolve_line_column(&self, file: FileId, line: usize, column: usize) -> usize {
        let line_start = self.files.borrow().line_range(file, line.saturating_sub(1)).unwrap();
        line_start.start + column.saturating_sub(1)
    }
    /// Returns the string content of the given Span
    pub fn resolve_span(&self, span: Span) -> &str {
        &self.files.get(span.file).unwrap().source()[span.start..span.end]
    }

    /// Returns the Span of the line containing the start of the given Span (excluding the newline)
    pub fn first_line(&self, span: Span) -> Span {
        let line_index = self.files.line_index(span.file, span.start).unwrap();
        let line_range = self.files.line_range(span.file, line_index).unwrap();
        // get rid of newline
        let len = self.resolve_span(Span::new(span.file, line_range.start, line_range.end)).trim_end().len();
        Span::new(span.file, line_range.start, line_range.start + len)
    }

    pub fn emitted(&self) -> Vec<Emitted<E>> {
        self.emitted.borrow().deref().clone()
    }

    fn diagnostic(&self, severity: Severity, code: E) -> DiagnosticBuilder<'_, E> {
        DiagnosticBuilder {
            files: &self.files,
            output: &self.output,
            config: &self.config,
            severity,
            code,
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }
    pub fn bug(&self, code: E) -> DiagnosticBuilder<'_, E> {
        self.emitted.borrow_mut().push(Emitted::Bug(code.clone()));
        let mut diag =
            Some(self.diagnostic(Severity::Bug, code).with_note("please report this"));
        backtrace::trace(|frame| {
            let ip = frame.ip();
            backtrace::resolve(ip, |symbol| {
                diag = Some(diag.take().unwrap().with_note(format!(
                    "in file {:?} name {:?} line {:?} address {:?}",
                    symbol.filename(),
                    symbol.name(),
                    symbol.lineno(),
                    symbol.addr()
                )));
            });
            true
        });
        diag.unwrap()
    }

    pub fn error(&self, code: E) -> DiagnosticBuilder<'_, E> {
        self.emitted.borrow_mut().push(Emitted::Error(code.clone()));
        self.diagnostic(Severity::Error, code)
    }

    pub fn warning(&self, code: E) -> DiagnosticBuilder<'_, E> {
        self.emitted.borrow_mut().push(Emitted::Warning(code.clone()));
        self.diagnostic(Severity::Warning, code)
    }

    pub fn note(&self, code: E) -> DiagnosticBuilder<'_, E> {
        self.emitted.borrow_mut().push(Emitted::Note(code.clone()));
        self.diagnostic(Severity::Note, code)
    }

    pub fn help(&self, code: E) -> DiagnosticBuilder<'_, E> {
        self.emitted.borrow_mut().push(Emitted::Help(code.clone()));
        self.diagnostic(Severity::Help, code)
    }
}

#[must_use = "call `emit` to emit the diagnostic"]
pub struct DiagnosticBuilder<'d, E: ErrorCode> {
    files: &'d EvenSimplerFiles,
    output: &'d Output,
    config: &'d Config,

    severity: Severity,
    code: E,
    labels: Vec<Label<FileId>>,
    notes: Vec<String>,
}

impl<'d, E: ErrorCode> DiagnosticBuilder<'d, E> {
    pub fn emit(self) {
        let Self { files, output, config, severity, code, labels, notes } = self;
        let diagnostic = Diagnostic { severity, message: code.message(), code: Some(code.code()), labels, notes };

        match &output.typ {
            OutputType::StandardStream(std) => {
                term::emit(&mut std.lock(), config, files, &diagnostic)
                    .expect("error writing diagnostic to StandardStream");
            }
            OutputType::Buffered(f) => {
                let mut buffer = Buffer::ansi();
                term::emit(&mut buffer, config, files, &diagnostic)
                    .expect("error writing diagnostic to Buffer");
                let s = String::from_utf8(buffer.into_inner()).expect("error converting diagnostic Buffer to String");
                f(s);
            }
        }
    }

    fn with_label<S: Into<String>>(mut self, style: LabelStyle, span: Span, message: S) -> Self {
        self.labels.push(Label {
            style,
            file_id: span.file,
            range: span.start..span.end,
            message: message.into(),
        });
        self
    }

    pub fn with_error_label<S: Into<String>>(self, span: Span, message: S) -> Self {
        self.with_label(LabelStyle::Primary, span, message)
    }

    pub fn with_info_label<S: Into<String>>(self, span: Span, message: S) -> Self {
        self.with_label(LabelStyle::Secondary, span, message)
    }

    pub fn with_note<S: Into<String>>(mut self, message: S) -> Self {
        self.notes.push(message.into());
        self
    }
}

