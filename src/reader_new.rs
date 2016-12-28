extern crate libc;

use std::sync::mpsc::{Receiver, Sender, SyncSender, channel};
use std::error::Error;
use item::Item;
use std::sync::{Arc, RwLock};
use std::process::{Command, Stdio, Child};
use std::io::{stdin, BufRead, BufReader};
use event::{Event, EventArg};
use std::thread::{spawn, JoinHandle};
use std::thread;
use std::time::Duration;

pub struct Reader {
    rx_cmd: Receiver<(Event, EventArg)>,
    tx_item: SyncSender<(Event, EventArg)>,
    items: Arc<RwLock<Vec<Item>>>, // all items
}

impl Reader {
    pub fn new(rx_cmd: Receiver<(Event, EventArg)>, tx_item: SyncSender<(Event, EventArg)>) -> Self {
        Reader {
            rx_cmd: rx_cmd,
            tx_item: tx_item,
            items: Arc::new(RwLock::new(Vec::new())),
        }
    }

    pub fn run(&mut self) {
        // event loop
        let mut thread_reader: Option<JoinHandle<()>> = None;
        let mut thread_sender: Option<JoinHandle<()>> = None;
        let mut tx_reader: Option<Sender<bool>> = None;
        let mut tx_sender: Option<Sender<bool>> = None;

        while let Ok((ev, arg)) = self.rx_cmd.recv() {
            match ev {
                Event::EvReaderRestart => {
                    // close existing command or file if exists
                    tx_reader.map(|tx| {tx.send(true)});
                    thread_reader.take().map(|thrd| {thrd.join()});

                    // send message to stop existing matcher
                    tx_sender.map(|tx| {tx.send(true)});
                    thread_sender.take().map(|thrd| {thrd.join()});

                    // start command with new query
                    let cmd = *arg.downcast::<String>().unwrap();
                    let items = self.items.clone();
                    let (tx, rx_reader) = channel();
                    tx_reader = Some(tx);
                    thread::spawn(move || {
                        reader(&cmd, rx_reader, items);
                    });

                    // start sending loop to matcher
                    let tx_item = self.tx_item.clone();
                    let items = self.items.clone();
                    let (tx, rx_sender) = channel();
                    tx_sender = Some(tx);
                    thread::spawn(move || {
                        sender(rx_sender, tx_item, items);
                    });
                }
                _ => {
                    // do nothing
                }
            }
        }
    }
}

fn get_command_output(cmd: &str) -> Result<(Option<Child>, Box<BufRead>), Box<Error>> {
    let mut command = try!(Command::new("sh")
                       .arg("-c")
                       .arg(cmd)
                       .stdout(Stdio::piped())
                       .stderr(Stdio::null())
                       .spawn());
    let stdout = try!(command.stdout.take().ok_or("command output: unwrap failed".to_owned()));
    Ok((Some(command), Box::new(BufReader::new(stdout))))
}

fn reader(cmd: &str, rx_cmd: Receiver<bool>, items: Arc<RwLock<Vec<Item>>>) {
    // start the command
    let istty = unsafe { libc::isatty(libc::STDIN_FILENO as i32) } != 0;

    let (command, mut source): (Option<Child>, Box<BufRead>) = if istty {
        get_command_output(cmd).expect("command not found")
    } else {
        (None, Box::new(BufReader::new(stdin())))
    };

    let (tx_control, rx_control) = channel();

    thread::spawn(move || {
        // listen to `rx` for command to quit reader
        // kill command if it is got
        loop {
            if let Ok(quit) = rx_cmd.try_recv() {
                // clean up resources
                command.map(|mut x| {
                    let _ = x.kill();
                    let _ = x.wait();
                });
                break;
            }

            if let Ok(quit) = rx_control.recv_timeout(Duration::from_millis(10)) {
                command.map(|mut x| {
                    let _ = x.kill();
                    let _ = x.wait();
                });
                break;
            }
        }
    });

    loop {
        // start reading
        let mut input = String::new();
        match source.read_line(&mut input) {
            Ok(n) => {
                if n == 0 { break; }

                if input.ends_with('\n') {
                    input.pop();
                    if input.ends_with('\r') {
                        input.pop();
                    }
                }
                let mut items = items.write().unwrap();
                items.push(Item::new_plain(input));
            }
            Err(_err) => {} // String not UTF8 or other error, skip.
        }
    }
    tx_control.send(true);
}

fn sender(rx_cmd: Receiver<bool>, tx: SyncSender<(Event, EventArg)>, items: Arc<RwLock<Vec<Item>>>) {
    let mut index = 0;
    loop {
        if let Ok(quit) = rx_cmd.try_recv() {
            tx.send((Event::EvReaderEnd, Box::new(true)));
            break;
        }

        let items = items.read().unwrap();
        if index < items.len() {
            tx.send((Event::EvReaderNewItem, Box::new(items[index].clone())));
            index += 1;
        } else if index == items.len() {
            thread::sleep(Duration::from_millis(5));
        }
    }
}
