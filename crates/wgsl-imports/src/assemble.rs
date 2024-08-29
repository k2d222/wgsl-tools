use ropey::Rope;

use super::resolve::Module;

struct Assembler {
    source: Rope,
}

impl Assembler {
    pub fn new() -> Self {
        Self {
            source: Rope::new(),
        }
    }
}

impl Module {
    pub fn assemble(&self) -> String {
        let mut source = Rope::new();

        source.insert(0, &self.source);
        source.remove(self.imports_span.clone());

        source.to_string()
    }
}
