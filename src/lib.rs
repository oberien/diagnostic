#![allow(dead_code)]
use std::fmt;
use std::cell::RefCell;
use std::ops::Range;
use std::borrow::Borrow;

use codespan_reporting::files::{SimpleFile, Files, Location};
use codespan_reporting::diagnostic::{Severity, Diagnostic, Label, LabelStyle};
use codespan_reporting::term::{self, Config, termcolor::{StandardStream, ColorChoice}};
use appendlist::AppendList;
use codespan_reporting::term::termcolor::Buffer;

pub trait ErrorCode {
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
}

// codespan_reporting::SimpleFiles requires `&mut self` when adding a file,
// making it impossible to have an immutable interface to put a string in it
// and return an immutable &str.
// Thus, here we implement the same thing, but using an Arena instead of a Vec, to allow
// an immutable interface.
struct EvenSimplerFiles {
    files: AppendList<SimpleFile<String, String>>,
    synthetic_files: AppendList<(&'static str, SimpleFile<String, String>)>,
}
impl EvenSimplerFiles {
    fn new() -> Self {
        EvenSimplerFiles {
            files: AppendList::new(),
            synthetic_files: AppendList::new(),
        }
    }
    fn add(&self, name: String, source: String) -> FileId {
        let idx = self.files.len();
        self.files.push(SimpleFile::new(name, source));
        FileId(FileIdKind::File(idx))
    }
    fn add_synthetic(&self, name: &'static str, source: String) -> FileId {
        let id = FileId(FileIdKind::Synthetic(name));
        assert!(self.get_optional(id).is_none(), "synthetic file with name {} already added previously", name);
        self.synthetic_files.push((name, SimpleFile::new(name.to_string(), source)));
        id
    }
    fn get(&self, id: FileId) -> &SimpleFile<String, String> {
        self.get_optional(id).unwrap()
    }
    fn get_optional(&self, id: FileId) -> Option<&SimpleFile<String, String>> {
        match id.0 {
            FileIdKind::File(idx) => self.files.get(idx),
            FileIdKind::Synthetic(name) => self.synthetic_files.iter().find_map(|(n, f)| {
                    if *n == name {
                        Some(f)
                    } else {
                        None
                    }
                })
        }
    }
}
impl<'a> Files<'a> for EvenSimplerFiles {
    type FileId = FileId;
    type Name = &'a str;
    type Source = &'a str;

    fn name(&'a self, id: Self::FileId) -> Option<Self::Name> {
        Some(&self.get(id).name())
    }

    fn source(&'a self, id: Self::FileId) -> Option<Self::Source> {
        Some(&self.get(id).source())
    }

    fn line_index(&'a self, id: Self::FileId, byte_index: usize) -> Option<usize> {
        self.get(id).line_index((), byte_index)
    }

    fn line_number(&'a self, id: Self::FileId, line_index: usize) -> Option<usize> {
        self.get(id).line_number((), line_index)
    }

    fn column_number(&'a self, id: Self::FileId, line_index: usize, byte_index: usize) -> Option<usize> {
        self.get(id).column_number((), line_index, byte_index)
    }

    fn location(&'a self, id: Self::FileId, byte_index: usize) -> Option<Location> {
        self.get(id).location((), byte_index)
    }

    fn line_range(&'a self, id: Self::FileId, line_index: usize) -> Option<Range<usize>> {
        self.get(id).line_range((), line_index)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct FileId(FileIdKind);
#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
enum FileIdKind {
    Synthetic(&'static str),
    File(usize),
}
impl FileId {
    pub const fn synthetic(path: &'static str) -> FileId {
        FileId(FileIdKind::Synthetic(path))
    }
}
impl fmt::Display for FileId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            FileIdKind::Synthetic(path) => fmt::Display::fmt(path, f),
            FileIdKind::File(id) => fmt::Display::fmt(id, f),
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

pub struct Diagnostics {
    files: EvenSimplerFiles,
    output: Output,
    config: Config,
    bugs_printed: RefCell<u32>,
    errors_printed: RefCell<u32>,
    warnings_printed: RefCell<u32>,
    notes_printed: RefCell<u32>,
    helps_printed: RefCell<u32>,
}
impl fmt::Debug for Diagnostics {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Diagnostics")
            .field("...", &"...")
            .finish()
    }
}

impl Diagnostics {
    pub fn new() -> Diagnostics {
        Self::with_output(Output::stderr())
    }
    pub fn with_output(output: Output) -> Diagnostics {
        Diagnostics {
            files: EvenSimplerFiles::new(),
            output,
            config: Config::default(),
            bugs_printed: RefCell::new(0),
            errors_printed: RefCell::new(0),
            warnings_printed: RefCell::new(0),
            notes_printed: RefCell::new(0),
            helps_printed: RefCell::new(0),
        }
    }

    pub fn add_file(&self, name: String, source: String) -> (FileId, &str) {
        let idx = self.files.add(name, source);
        let source = self.files.get(idx).source();
        (idx, source)
    }
    /// Synthetic files are files keyed with a static name / path. Each name / path can only
    /// exist once. Synthetic `FileId`s can be created via `FileId::synthetic` to refer to
    /// those files.
    pub fn add_synthetic_file(&self, name: &'static str, source: String) -> (FileId, &str) {
        let id = self.files.add_synthetic(name, source);
        let source = self.files.get(id).source();
        (id, source)
    }

    /// Gets the file content as `&str`. Panics if the file id doesn't exist.
    pub fn get_file(&self, file_id: FileId) -> &str {
        self.files.source(file_id).unwrap()
    }

    /// Transforms a line and column (both 1-indexed) inside a file to their byte-offset.
    /// Panics if the file id doesn't exist.
    pub fn resolve_line_column(&self, file: FileId, line: usize, column: usize) -> usize {
        let line_start = self.files.borrow().line_range(file, line.saturating_sub(1)).unwrap();
        line_start.start + column.saturating_sub(1)
    }
    pub fn resolve_span(&self, span: Span) -> &str {
        &self.files.get(span.file).source()[span.start..span.end]
    }

    pub fn bugs_printed(&self) -> u32 {
        *self.bugs_printed.borrow()
    }
    pub fn errors_printed(&self) -> u32 {
        *self.errors_printed.borrow()
    }
    pub fn warnings_printed(&self) -> u32 {
        *self.warnings_printed.borrow()
    }
    pub fn notes_printed(&self) -> u32 {
        *self.notes_printed.borrow()
    }
    pub fn helps_printed(&self) -> u32 {
        *self.helps_printed.borrow()
    }

    fn diagnostic<E: ErrorCode>(&self, severity: Severity, code: E) -> DiagnosticBuilder<'_, E> {
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
    pub fn bug<E: ErrorCode>(&self, code: E) -> DiagnosticBuilder<'_, E> {
        *self.bugs_printed.borrow_mut() += 1;
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

    pub fn error<E: ErrorCode>(&self, code: E) -> DiagnosticBuilder<'_, E> {
        *self.errors_printed.borrow_mut() += 1;
        self.diagnostic(Severity::Error, code)
    }

    pub fn warning<E: ErrorCode>(&self, code: E) -> DiagnosticBuilder<'_, E> {
        *self.warnings_printed.borrow_mut() += 1;
        self.diagnostic(Severity::Warning, code)
    }

    pub fn note<E: ErrorCode>(&self, code: E) -> DiagnosticBuilder<'_, E> {
        *self.notes_printed.borrow_mut() += 1;
        self.diagnostic(Severity::Note, code)
    }

    pub fn help<E: ErrorCode>(&self, code: E) -> DiagnosticBuilder<'_, E> {
        *self.helps_printed.borrow_mut() += 1;
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

