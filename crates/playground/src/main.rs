use std::path::{Path, PathBuf};

use itertools::Itertools;
use wesl::{CompileOptions, Diagnostic, NoMangler, Resource, VirtualFileResolver};
use yew::prelude::*;

const PLACEHOLDER: &'static str = "fn main() -> u32 {\n    return 0;\n}\n";

#[derive(Clone, PartialEq)]
struct File {
    source: String,
    name: String,
}

#[derive(Properties, PartialEq)]
struct InputFilesProps {
    files: Vec<File>,
    selected: usize,
}

// #[function_component(InputFiles)]
// fn input_files(InputFilesProps { files, selected }: &InputFilesProps) -> Html {
//     let tabs = files
//         .iter()
//         .map(|file| {
//             html! {
//                     <button>{ &file.name }</button>
//             }
//         })
//         .collect_vec();

//     let selected = files
//         .get(*selected)
//         .unwrap_or_else(|| files.get(0).unwrap());

//     html! {
//         <>
//             <div class="tabs">{ tabs }</div>
//             <textarea placeholder={PLACEHOLDER} value={selected.source.clone()} />
//         </>
//     }
// }

fn compile(files: &Vec<File>) -> Result<String, Diagnostic> {
    let entry = files.get(0).unwrap();

    let mut resolver = VirtualFileResolver::new();

    for file in files {
        let resource = PathBuf::from(&file.name).into();
        resolver.add_file(resource, file.source.clone()).unwrap();
    }

    let entrypoint: Resource = PathBuf::from(&entry.name).into();

    let mangler = NoMangler::default();

    let compile_options = CompileOptions {
        use_imports: true,
        use_condcomp: true,
        strip: false,
        entry_points: None,
        features: Default::default(),
    };

    let wgsl = wesl::compile(&entrypoint, &resolver, &mangler, &compile_options)
        .map_err(|e| Diagnostic::new(e))?;
    Ok(wgsl.to_string())
}

#[function_component(App)]
fn app() -> Html {
    let files = use_state(|| {
        vec![
            File {
                name: "main.wgsl".to_string(),
                source: "import util/my_fn;\nfn main() -> u32 {\n    return my_fn();\n}\n"
                    .to_string(),
            },
            File {
                name: "util.wgsl".to_string(),
                source: "fn my_fn() -> u32 { return 42; }".to_string(),
            },
        ]
    });

    let selected_file = use_state(|| 0);
    let output = use_state(|| "".to_string());

    let on_tab_click = {
        let selected_file = selected_file.clone();
        Callback::from(move |index: usize| selected_file.set(index))
    };

    let on_content_change = {
        let selected_file = selected_file.clone();
        let files = files.clone();
        Callback::from(move |e: InputEvent| {
            let mut updated_files = (*files).clone();
            if let Some(textarea) = e.target_dyn_into::<web_sys::HtmlTextAreaElement>() {
                updated_files[*selected_file].source = textarea.value();
            }
            files.set(updated_files);
        })
    };

    let on_run = {
        let files = files.clone();
        let output = output.clone();
        Callback::from(move |_| {
            let compiled =
                compile(&*files).unwrap_or_else(|e| ansi_to_html::convert(&e.to_string()).unwrap());
            output.set(compiled);
        })
    };

    html! {
        <div id="app">
            <div id="header">
                <h3>{"WESL Sandbox"}</h3>
                <button onclick={on_run}>{ "run" }</button>
            </div>
            <div id="left">
                <div class="tabs">
                {
                    for files.iter().enumerate().map(|(index, file)| html! {
                        <button onclick={on_tab_click.reform(move |_| index)}>{ &file.name }</button>
                    })
                }
                </div>
                <textarea value={files[*selected_file].source.clone()} oninput={on_content_change} />
            </div>
            <div id="right">
                <pre><code>{ Html::from_html_unchecked((*output).clone().into()) }</code></pre>
            </div>
        </div>
    }
}

// #[function_component(App)]
// fn app() -> Html {
//     let files = vec![File {
//         source: PLACEHOLDER.to_string(),
//         name: "main.wgsl".to_string(),
//     }];
//     let files = use_state(|| files);

//     let compiled = use_state(|| "output goes here".to_string());

//     let on_run = {
//         let compiled = compiled.clone();
//         let files = files.clone();
//         Callback::from(move |_| {
//             log::info!("compiling");
//             let res = compile(&files).unwrap_or_else(|e| e.to_string());
//             log::debug!("{res}");
//             compiled.set(res);
//         })
//     };

//     html! {
//         <>
//             <div id="header">
//                 <button onclick={on_run}>{ "run" }</button>
//             </div>
//             <div id="left">
//                 <InputFiles files={(*files).clone()} selected={0} />
//             </div>
//             <div id="right">
//                 <textarea placeholder="output goes here" value={ (*compiled).clone() } />
//             </div>
//         </>
//     }
// }

fn main() {
    console_log::init_with_level(log::Level::Debug).unwrap();
    yew::Renderer::<App>::new().render();
}
