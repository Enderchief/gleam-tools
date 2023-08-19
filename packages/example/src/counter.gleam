import gleam/int

pub type HtmlElement

@external(javascript, "./ffi_dom.ts", "update")
fn update(element: HtmlElement, property: String, updater: fn(a) -> a) -> Nil

@external(javascript, "./ffi_dom.ts", "onclick")
fn onclick(element: HtmlElement, func: fn () -> Nil) -> Nil

pub fn setup_counter(btn: HtmlElement, counter: HtmlElement) -> Nil {
    onclick(btn, fn() { 
        update(counter, "innerHTML", fn(v: String) { 
            let assert Ok(count) = int.parse(v)
            int.to_string(count + 1)
         })
     })

    Nil
}
