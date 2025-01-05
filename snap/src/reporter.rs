use crate::runner::TEST_RESULTS;
use anyhow::Result;
use colored::Colorize;
use indexmap::IndexMap;
use std::{fmt::Write, time::Instant};

pub const SUCCESS_ICON: &str = "√";
pub const ERROR_ICON: &str = "×";

pub fn report_results(start: Instant) -> Result<()> {
    let suite = TEST_RESULTS.lock().expect("Failed to lock test results");
    let mut buf = String::new();

    let grouped_by_file = suite.results.iter().fold(IndexMap::new(), |mut acc, res| {
        acc.entry(&res.location.file)
            .or_insert_with(Vec::new)
            .push(res);
        acc
    });

    for (file, results) in grouped_by_file {
        let all_success = results.iter().all(|res| res.result.is_ok());
        let icon = if all_success {
            SUCCESS_ICON.bright_green()
        } else {
            ERROR_ICON.bright_red()
        };

        writeln!(
            buf,
            "{} {} {}",
            icon,
            file.dimmed(),
            if all_success {
                "(all tests passed)".green()
            } else {
                "(some tests failed)".red()
            }
        )?;

        // get all failed tests
        let x = results
            .iter()
            .filter(|res| res.result.is_err())
            .collect::<Vec<_>>();
        if !x.is_empty() || results.len() > 1 {
            for res in results {
                let icon = if res.result.is_ok() {
                    SUCCESS_ICON.bright_green()
                } else {
                    ERROR_ICON.bright_red()
                };

                writeln!(buf, "    {} {}", icon, res.comment.dimmed())?;

                if let Err(fail) = &res.result {
                    writeln!(buf, "        {}", fail.reason.bright_red())?;
                }
            }
        }
    }

    let passed_string = format!("{} passed", suite.passed_tests)
        .bright_green()
        .bold();
    let failed = suite.total_tests - suite.passed_tests;
    let failed_string = format!("{} failed", failed).bright_red().bold();
    writeln!(
        buf,
        "\n{all:>14} {total}",
        all = "All tests".dimmed(),
        total = suite.passed_tests
    )?;
    writeln!(
        buf,
        "{passed:>14} {text}",
        passed = "Passed".dimmed(),
        text = passed_string
    )?;
    if failed > 0 {
        writeln!(
            buf,
            "{failed:>14} {text}",
            failed = "Failed".dimmed(),
            text = failed_string
        )?;
    }

    // rate
    let rate = suite.passed_tests as f64 / suite.total_tests as f64 * 100.0;
    let rate_string = format!("{:.2}%", rate);
    writeln!(
        buf,
        "{rate:>14} {rate1}",
        rate = "Success rate".dimmed(),
        rate1 = rate_string
    )?;
    writeln!(
        buf,
        "{time:>14} {elapsed}",
        time = "Finished in".dimmed(),
        elapsed = format!("{:?}", start.elapsed())
    )?;

    println!("{}", buf);

    Ok(())
}
