use sjc::File;
use read_from::ReadFrom;

fn main() {
	setup_tracing();
	let file = File::read_from(std::fs::File::open("../parser-ruby/test.sjc").unwrap()).unwrap();
	println!("{}", file);
}


fn setup_tracing() {
	use tracing::level_filters::LevelFilter;
	use tracing_subscriber::{layer::SubscriberExt, registry::Registry};
	use tracing_tree::HierarchicalLayer;

	let loglevel = std::env::var("SJN_LOG");
	let filter = 
		match loglevel.as_deref() {
			Ok("T") | Ok("TRACE") => LevelFilter::TRACE,
			Ok("D") | Ok("DEBUG") => LevelFilter::DEBUG,
			Ok("I") | Ok("INFO") => LevelFilter::INFO,
			Ok("W") | Ok("WARN") => LevelFilter::WARN,
			Ok("E") | Ok("ERROR") => LevelFilter::ERROR,
			Ok("O") | Ok("OFF") => LevelFilter::OFF,
			Ok("TREE") => {
				let layer = HierarchicalLayer::default()
					.with_indent_lines(true)
					.with_indent_amount(2)
					.with_thread_names(true)
					.with_thread_ids(true)
					.with_verbose_exit(true)
					.with_verbose_entry(true)
					.with_targets(true);

				let subscriber = Registry::default().with(layer);
				tracing::subscriber::set_global_default(subscriber).unwrap();
				return;
			},
			_ => return
		};

	tracing_subscriber::fmt()
		.with_max_level(filter)
		.with_span_events(tracing_subscriber::fmt::format::FmtSpan::FULL)
		.init();
}
