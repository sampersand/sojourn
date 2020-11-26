use sjvm::SojournVm;
use sjef::File;
use read_from::ReadFrom;

fn main() {
	setup_tracing("debug");
	tracing::info!("Reading file in...");
	let file = File::read_from(std::fs::File::open("parser-ruby/test.sjc").unwrap()).expect("bad file");
	tracing::info!("Running vm...");
	let mut vm = SojournVm::<sjvm::register::release::Register, sjvm::heap::release::Heap>::new(file);
	vm.run();
}



fn setup_tracing(level: &str) {
	use tracing::level_filters::LevelFilter;
	use tracing_subscriber::{layer::SubscriberExt, registry::Registry};
	use tracing_tree::HierarchicalLayer;

	let filter = match level {
		"trace" => LevelFilter::TRACE,
		"debug" => LevelFilter::DEBUG,
		"info" => LevelFilter::INFO,
		"warn" => LevelFilter::WARN,
		"error" => LevelFilter::ERROR,
		"off" => LevelFilter::OFF,
		"tree" => {
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
		_ => panic!("internal error: unknown tracing level {:?}", level)
	};

	tracing_subscriber::fmt()
		.with_max_level(filter)
		.with_span_events(tracing_subscriber::fmt::format::FmtSpan::NONE)
		.init();
}
