#![warn(clippy::pedantic)]

use std::fmt::Display;

use array_deque::StackArrayDeque;
use clap::{Args, Parser, Subcommand};
use derive_more::Display;
use jiff::{
    SignedDuration, Span, Timestamp,
    civil::{DateTime, Time},
    fmt::{
        StdFmtWrite,
        friendly::{Direction, SpanPrinter},
        temporal::DateTimePrinter,
    },
    tz::{self, Dst, Offset, TimeZone, TimeZoneTransition},
};

type Result<T, E = Box<dyn std::error::Error>> = std::result::Result<T, E>;

struct PrettyDateTime(DateTime);

impl Display for PrettyDateTime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        static PRINTER: DateTimePrinter = DateTimePrinter::new().separator(b' ');
        PRINTER
            .print_datetime(&self.0, StdFmtWrite(f))
            .map_err(|_| std::fmt::Error)
    }
}

fn list_timezones() {
    for name in tz::db().available() {
        println!("{name}");
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct AllDateTimesTheSameKey {
    first_offset: Offset,
    transitions: Vec<(Timestamp, Offset)>,
}

impl AllDateTimesTheSameKey {
    fn new(tz: &TimeZone) -> Self {
        let mut transitions = Vec::new();
        for transition in tz.following(Timestamp::MIN) {
            transitions.push((transition.timestamp(), transition.offset()));
        }
        Self {
            first_offset: tz.to_offset(Timestamp::MIN),
            transitions,
        }
    }
}

fn list_timezones_with_same_datetimes() {
    use std::collections::HashMap;

    let mut seen: HashMap<AllDateTimesTheSameKey, Vec<String>> = HashMap::new();
    for name in tz::db().available() {
        let key = AllDateTimesTheSameKey::new(&TimeZone::get(name.as_str()).unwrap());
        seen.entry(key).or_default().push(name.to_string());
    }
    for (key, names) in seen {
        if names.len() > 1 {
            println!(
                "{} timezones with the same datetimes ({} transitions):",
                names.len(),
                key.transitions.len()
            );
            for name in names {
                println!("  {name}");
            }
            println!();
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Display)]
enum TransitionType {
    #[display("DST forward")]
    DstForward,
    #[display("DST backward")]
    DstBackward,
    #[display("other")]
    Other(Dst),
}

impl TransitionType {
    fn opposite(self) -> Self {
        match self {
            Self::DstForward => Self::DstBackward,
            Self::DstBackward => Self::DstForward,
            Self::Other(_) => unimplemented!(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct TransitionEndpoint {
    time: Time,
    offset: Offset,
}

impl TransitionEndpoint {
    fn new(datetime: DateTime, transition: &TimeZoneTransition) -> Self {
        Self {
            time: datetime.time(),
            offset: transition.offset(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct TransitionInfo {
    typ: TransitionType,
    from: TransitionEndpoint,
    to: TransitionEndpoint,
}

fn list_dst_changes(name: &str) -> Result<()> {
    let timezone = TimeZone::get(name)?;

    let mut previous_dst_transitions: StackArrayDeque<(TransitionInfo, DateTime), 2> =
        StackArrayDeque::new();

    let mut previous_transition = None;
    for transition in timezone.following(Timestamp::MIN) {
        let timestamp_after = transition.timestamp();
        let timestamp_before = timestamp_after - SignedDuration::from_nanos(1);
        let datetime_before = timezone.to_datetime(timestamp_before);
        let datetime_after = transition.offset().to_datetime(timestamp_after);

        assert_eq!(datetime_after, timezone.to_datetime(timestamp_after));

        assert_eq!(datetime_before.subsec_nanosecond(), 999_999_999);
        let datetime_before = datetime_before + SignedDuration::from_nanos(1);

        let Some(previous_transition_unwrapped) = &previous_transition else {
            previous_transition = Some(transition);
            continue;
        };

        let typ = match (previous_transition_unwrapped.dst(), transition.dst()) {
            (Dst::No, Dst::Yes) => TransitionType::DstForward,
            (Dst::Yes, Dst::No) => TransitionType::DstBackward,
            (dst, _) => TransitionType::Other(dst),
        };

        let transition_info = TransitionInfo {
            typ,
            from: TransitionEndpoint::new(datetime_before, previous_transition_unwrapped),
            to: TransitionEndpoint::new(datetime_after, &transition),
        };

        if let TransitionType::Other(_) = typ {
            println!("Non-DST transition at {}:", PrettyDateTime(datetime_after));
            if transition_info.from.offset != transition_info.to.offset {
                println!(
                    " Offset changed from {} to {}",
                    transition_info.from.offset, transition_info.to.offset
                );
            }
            if transition_info.from.time != transition_info.to.time {
                println!(
                    " Time changed from {} to {}",
                    transition_info.from.time, transition_info.to.time
                );
            }
            println!();

            previous_transition = Some(transition);
            continue;
        }

        if previous_dst_transitions.len() != 2 {
            previous_dst_transitions.push_back((transition_info, datetime_after));
            previous_transition = Some(transition);
            continue;
        }

        let (previous_info, _) = previous_dst_transitions.back().unwrap();
        let (previous_previous_info, previous_previous_datetime_after) =
            previous_dst_transitions.front().unwrap();

        assert_eq!(previous_previous_info.typ, typ);
        assert_eq!(previous_info.typ, typ.opposite());

        if *previous_previous_info != transition_info {
            println!(
                "{} transition changed between {} and {}:",
                typ,
                previous_previous_datetime_after.date(),
                datetime_after.date(),
            );
            println!(
                "  Before: {}{} -> {}{}",
                previous_previous_info.from.time,
                previous_previous_info.from.offset,
                previous_previous_info.to.time,
                previous_previous_info.to.offset
            );
            println!(
                "  After:  {}{} -> {}{}",
                transition_info.from.time,
                transition_info.from.offset,
                transition_info.to.time,
                transition_info.to.offset
            );
            println!();
        }

        previous_dst_transitions.push_back((transition_info, datetime_after));
        previous_transition = Some(transition);
    }

    Ok(())
}

struct PrettyDateTimeDiff(Span);

impl Display for PrettyDateTimeDiff {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        static PRINTER: SpanPrinter = SpanPrinter::new().direction(Direction::ForceSign);
        PRINTER
            .print_span(&self.0, StdFmtWrite(f))
            .map_err(|_| std::fmt::Error)
    }
}

fn list_non_monotic_transitions() -> Result<()> {
    let mut non_monotic = 0;
    let mut total = 0;
    for name in tz::db().available() {
        let timezone = TimeZone::get(name.as_str())?;
        let mut found_non_monotic = false;
        for transition in timezone.following(Timestamp::MIN) {
            let timestamp_after = transition.timestamp();
            let timestamp_before = timestamp_after - SignedDuration::from_nanos(1);
            let datetime_before = timezone.to_datetime(timestamp_before);
            let datetime_after = transition.offset().to_datetime(timestamp_after);

            assert_eq!(datetime_after, timezone.to_datetime(timestamp_after));

            total += 1;
            if datetime_after.date() < datetime_before.date() {
                if !found_non_monotic {
                    println!("{name}:");
                    found_non_monotic = true;
                }

                non_monotic += 1;

                let datetime_before = datetime_before + SignedDuration::from_nanos(1);
                let diff = datetime_after - datetime_before;
                println!(
                    "  {} {} to {} {} ({})",
                    PrettyDateTime(datetime_before),
                    timezone.to_offset(timestamp_before),
                    PrettyDateTime(datetime_after),
                    transition.offset(),
                    PrettyDateTimeDiff(diff)
                );
            }
        }
        if found_non_monotic {
            println!();
        }
    }
    println!("Non-monotic transitions: {non_monotic} / {total}");
    Ok(())
}

#[derive(Parser)]
struct Cli {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Args)]
#[group(required = true, multiple = false)]
struct TimeZones {
    names: Option<Vec<String>>,
    #[clap(long)]
    all: bool,
}

impl TimeZones {
    fn get(self) -> Vec<String> {
        if let Some(names) = self.names {
            names
        } else {
            tz::db().available().map(|s| s.to_string()).collect()
        }
    }
}

#[derive(Subcommand)]
enum Command {
    List,
    ListSameDateTimes,
    ListNonMonoticTransitions,
    Dst(TimeZones),
}

fn main() -> Result<()> {
    let args = Cli::parse();

    match args.command {
        Command::List => list_timezones(),
        Command::ListSameDateTimes => list_timezones_with_same_datetimes(),
        Command::ListNonMonoticTransitions => list_non_monotic_transitions()?,
        Command::Dst(timezones) => {
            let timezones = timezones.get();
            for name in &timezones {
                if timezones.len() > 1 {
                    println!("=== {name} ===");
                }
                list_dst_changes(name)?;
            }
        }
    }

    Ok(())
}
