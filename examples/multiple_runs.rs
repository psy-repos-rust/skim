use nix::libc::malloc_trim;
use skim::{Skim, prelude::SkimOptionsBuilder};

// Hint: use `ps -T -p $(pgrep -f target/debug/examples/multiple_runs)` to watch threads while the
// different invocations run, and make sure none is leaking through
fn main() {
    for i in 0..3 {
        let opts = SkimOptionsBuilder::default()
            .header(format!("run {i}"))
            .cmd("cat benches/fixtures/10M.txt")
            .build()
            .unwrap();
        let res = Skim::run_with(opts, None).unwrap();
        unsafe {
            malloc_trim(0);
        }
        println!(
            "run {i}: {:?}, sleeping for 5 secs",
            res.selected_items.first().map(|x| x.output())
        );
        std::thread::sleep(std::time::Duration::from_secs(5));
    }
}
