#![allow(unused)]

#[macro_use]
extern crate tracing;

use sjef::File;
use read_from::ReadFrom;
use clap::{Arg, App, ArgMatches, SubCommand};
use std::io::{self, Write};
use std::fs::File as StdFile;

fn main() {
	let matches = 
		App::new("Sojourn Debugger")
			.version(clap::crate_version!())
			.author(clap::crate_authors!())
			.about(clap::crate_description!())
			.arg(Arg::with_name("verbose")
				.short("v")
				.help("sets the verbosity level")
				.alias("log-level")
				.env("SJ_LOG_LEVEL")
				.required(false)
				.possible_values(&["trace", "debug", "info", "warn", "error", "off", "tree"])
				.default_value("warn"))
			.subcommand(SubCommand::with_name("dis")
				.about("Disassemble the input file")
				.arg(Arg::with_name("FILE")
					.help("sets the input file to use ('-' for stdin)")
					.required(true)
					.index(1))
				.arg(Arg::with_name("output")
					.short("o")
					.help("sets the output file to use (defaults to stdout)")
					.required(false)
					.takes_value(true))
				.arg(Arg::with_name("sections")
					.help("only output the given sections (NB: this option is currently ignored)")
					.short("s")
					.multiple(true)
					.possible_values(&["header", "text", "data"])))
			.get_matches();

	setup_tracing(matches.value_of("verbose").expect("'verbose' should have a default value."));

	std::process::exit(if let Some(matches) = matches.subcommand_matches("dis") {
		if dis(matches) { 0 } else { 1 }
	} else {
		error!("no subcommand supplied");
		1
	})
}

fn dis(matches: &ArgMatches) -> bool {
	let path = matches.value_of_os("FILE").expect("'FILE' is a required parameter.");

	let read_result =
		if path == "-" {
			trace!("reading from stdin");
			File::read_from(io::stdin())
		} else {
			trace!(?path, "reading from path");
			match StdFile::open(path) {
				Ok(read_file) => File::read_from(read_file),
				Err(err) => {
					error!(%err, ?path, "unable to open input file for reading");
					return false;
				}
			}
		};

	let file =
		match read_result {
			Ok(file) => file,
			Err(err) => {
				error!(%err, ?path, "unable to disassemble file.");
				return false;
			}
		};

	let write_result =
		if let Some(output) = matches.value_of_os("output") {
			match StdFile::create(output) {
				Ok(mut output_file) => write!(output_file, "{}", file),
				Err(err) => {
					error!(%err, path = ?output, "unable to open output for writing");
					return false;
				}
			}
		} else {
			println!("{}", file);
			Ok(())
		};

	if let Err(err) = write_result {
		error!(?path, ?err, output=?matches.value_of("output").unwrap_or("<stdout>"), "unable to write output.");
		false
	} else {
		true
	}
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
		other => panic!("internal error: unknown tracing level {:?}", level)
	};

	tracing_subscriber::fmt()
		.with_max_level(filter)
		.with_span_events(tracing_subscriber::fmt::format::FmtSpan::FULL)
		.init();
}
