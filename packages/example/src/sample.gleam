import gleam/io

@external(javascript, "./mathy.ts", "half")
fn half(x: Int) -> Int

pub fn stuff(n: Int) -> Int {
  io.print("hello")
  case n % 2 {
    0 -> half(n)
    1 -> 3 * n + 1
  }
}
