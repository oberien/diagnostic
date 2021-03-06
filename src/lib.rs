#![allow(dead_code)]
use std::fmt;
use std::cell::RefCell;
use std::ops::Range;
use std::borrow::Borrow;

use codespan_reporting::files::{SimpleFile, Files, Location};
use codespan_reporting::diagnostic::{Severity, Diagnostic, Label, LabelStyle};
use codespan_reporting::term::{self, Config, termcolor::{StandardStream, ColorChoice}};
use appendlist::AppendList;

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
}
impl EvenSimplerFiles {
    pub fn new() -> Self {
        EvenSimplerFiles {
            files: AppendList::new(),
        }
    }
    pub fn add(&self, name: String, source: String) -> FileId {
        let idx = self.files.len();
        self.files.push(SimpleFile::new(name, source));
        FileId(idx)
    }
    pub fn get(&self, idx: FileId) -> &SimpleFile<String, String> {
        self.files.get(idx.0).unwrap()
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
pub struct FileId(usize);

pub struct Diagnostics {
    files: EvenSimplerFiles,
    stderr: StandardStream,
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
        Diagnostics {
            files: EvenSimplerFiles::new(),
            stderr: StandardStream::stderr(ColorChoice::Auto),
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

    pub fn get_file(&self, file_id: FileId) -> &str {
        // An invalid FileId can only be gotten via a different instance of Diagnostics.
        // As this library supports multiple files for a single Diagnostic, there should only
        // ever be a single instance of `Diagnostics`.
        // If the user decides not only to use two different diagnostics, but also to
        // use the FileIds interchangeably, panicking here should be fine.
        self.files.source(file_id).unwrap()
    }

    /// Transforms a line and column (both 1-indexed) inside a file to their byte-offset.
    pub fn resolve_line_column(&self, file: FileId, line: usize, column: usize) -> usize {
        let line_start = self.files.borrow().line_range(file, line.saturating_sub(1)).unwrap();
        line_start.start + column.saturating_sub(1)
    }
    pub fn resolve_span(&self, span: Span) -> &str {
        &self.files.files.get(span.file.0).unwrap().source()[span.start..span.end]
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
            stderr: &self.stderr,
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
    stderr: &'d StandardStream,
    config: &'d Config,

    severity: Severity,
    code: E,
    labels: Vec<Label<FileId>>,
    notes: Vec<String>,
}

impl<'d, E: ErrorCode> DiagnosticBuilder<'d, E> {
    pub fn emit(self) {
        let Self { files, stderr, config, severity, code, labels, notes } = self;
        let diagnostic = Diagnostic { severity, message: code.message(), code: Some(code.code()), labels, notes };

        let mut stderr = stderr.lock();
        term::emit(&mut stderr, config, files, &diagnostic)
            .expect("stderr is gone???");
        // writeln!(&mut stderr).expect("stderr is gone???");
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

