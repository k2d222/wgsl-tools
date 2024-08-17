// follwing the spec at this date: https://www.w3.org/TR/2024/WD-WGSL-20240731/

#[derive(Clone, Debug)]
pub struct TranslationUnit {
    pub global_directives: Vec<GlobalDirective>,
    pub global_declarations: Vec<GlobalDeclaration>,
}

#[derive(Clone, Debug)]
pub enum GlobalDirective {
    Diagnostic,
    Enable,
    Requires,
}

#[derive(Clone, Debug)]
pub enum Severity {
    Error,
    Warning,
    Info,
    Off,
}

#[derive(Clone, Debug)]
pub struct GlobalDeclaration;
