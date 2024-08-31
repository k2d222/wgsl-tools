use ropey::Rope;

use super::resolve::Module;

#[derive(Default)]
struct Assembler {}

impl Assembler {
    pub fn new() -> Self {
        Self {}
    }
}

impl Module {
    pub fn assemble(&self) -> String {
        let mut source = Rope::new();

        source.insert(0, &format!("{}", self.source));
        // source.insert(0, &self.source);
        // source.remove(self.imports_span.clone());

        source.to_string()
    }
}
