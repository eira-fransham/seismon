use super::*;

use std::{
    collections::{vec_deque, VecDeque},
    fs::File,
    io::{self, Write},
    io::{BufRead, BufReader, BufWriter},
    iter::IntoIterator,
    ops::Index,
    ops::IndexMut,
    path::Path,
    //time::Duration,
};

const DEFAULT_MAX_SIZE: usize = 1000;

/// Structure encapsulating command history
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct History {
    // TODO: this should eventually be private
    /// Vector of buffers to store history in
    pub buffers: VecDeque<Buffer>,
    /// Store a filename to save history into; if None don't save history
    file_name: Option<String>,
    /// Maximal number of buffers stored in the memory
    /// TODO: just make this public?
    max_buffers_size: usize,
    /// Maximal number of lines stored in the file
    // TODO: just make this public?
    max_file_size: usize,
    // TODO set from environment variable?
    pub append_duplicate_entries: bool,
    /// Append each entry to history file as entered?
    pub inc_append: bool,
    /// Share history across ion's with the same history file (combine with inc_append).
    pub share: bool,
    /// Last filesize of history file, used to optimize history sharing.
    pub file_size: u64,
    /// Allow loading duplicate entries, need to know this for loading history files.
    pub load_duplicates: bool,
    /// Writes between history compaction.
    compaction_writes: usize,
}

impl Default for History {
    fn default() -> Self {
        Self::new()
    }
}

impl History {
    /// Create new History structure.
    pub fn new() -> History {
        History {
            buffers: VecDeque::with_capacity(DEFAULT_MAX_SIZE),
            file_name: None,
            max_buffers_size: DEFAULT_MAX_SIZE,
            max_file_size: DEFAULT_MAX_SIZE,
            append_duplicate_entries: false,
            inc_append: false,
            share: false,
            file_size: 0,
            load_duplicates: true,
            compaction_writes: 0,
        }
    }

    /// Clears out the history.
    pub fn clear_history(&mut self) {
        self.buffers.clear();
    }

    /// Loads the history file from the saved path and appends it to the end of the history if append
    /// is true otherwise replace history.
    pub fn load_history(&mut self, append: bool) -> io::Result<u64> {
        if let Some(path) = self.file_name.clone() {
            let file_size = self.file_size;
            self.load_history_file_test(&path, file_size, append)
                .map(|l| {
                    self.file_size = l;
                    l
                })
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "History filename not set!",
            ))
        }
    }

    /// Loads the history file from path and appends it to the end of the history if append is true.
    pub fn load_history_file<P: AsRef<Path>>(&mut self, path: P, append: bool) -> io::Result<u64> {
        self.load_history_file_test(path, 0, append)
    }

    /// Loads the history file from path and appends it to the end of the history.f append is true
    /// (replaces if false).  Only loads if length is not equal to current file size.
    fn load_history_file_test<P: AsRef<Path>>(
        &mut self,
        path: P,
        length: u64,
        append: bool,
    ) -> io::Result<u64> {
        let path = path.as_ref();
        let file = if path.exists() {
            File::open(path)?
        } else {
            let status = format!("File not found {:?}", path);
            return Err(io::Error::new(io::ErrorKind::Other, status));
        };
        let new_length = file.metadata()?.len();
        if new_length == 0 && length == 0 && !append {
            // Special case, trying to load nothing and not appending- just clear.
            self.clear_history();
        }
        if new_length != length {
            if !append {
                self.clear_history();
            }
            let reader = BufReader::new(file);
            for line in reader.lines() {
                match line {
                    Ok(line) => {
                        if !line.starts_with('#') {
                            self.buffers.push_back(Buffer::from(line));
                        }
                    }
                    Err(_) => break,
                }
            }
            self.truncate();
            if !self.load_duplicates {
                let mut tmp_buffers: Vec<Buffer> = Vec::with_capacity(self.buffers.len());
                // Remove duplicates from loaded history if we do not want it.
                while let Some(buf) = self.buffers.pop_back() {
                    self.remove_duplicates(&buf.to_string()[..]);
                    tmp_buffers.push(buf);
                }
                while let Some(buf) = tmp_buffers.pop() {
                    self.buffers.push_back(buf);
                }
            }
        }
        Ok(new_length)
    }

    /// Removes duplicates and trims a history file to max_file_size.
    /// Primarily if inc_append is set without shared history.
    /// Static because it should have no side effects on a history object.
    fn deduplicate_history_file<P: AsRef<Path>>(
        path: P,
        max_file_size: usize,
    ) -> io::Result<String> {
        let path = path.as_ref();
        let file = if path.exists() {
            File::open(path)?
        } else {
            let status = format!("File not found {:?}", path);
            return Err(io::Error::new(io::ErrorKind::Other, status));
        };
        let mut buf: VecDeque<String> = VecDeque::new();
        let reader = BufReader::new(file);
        for line in reader.lines() {
            match line {
                Ok(line) => {
                    if !line.starts_with('#') {
                        buf.push_back(line);
                    }
                }
                Err(_) => break,
            }
        }
        let org_length = buf.len();
        if buf.len() >= max_file_size {
            let pop_out = buf.len() - max_file_size;
            for _ in 0..pop_out {
                buf.pop_front();
            }
        }
        let mut tmp_buffers: Vec<String> = Vec::with_capacity(buf.len());
        // Remove duplicates from loaded history if we do not want it.
        while let Some(line) = buf.pop_back() {
            buf.retain(|buffer| *buffer != line);
            tmp_buffers.push(line);
        }
        while let Some(line) = tmp_buffers.pop() {
            buf.push_back(line);
        }

        if org_length != buf.len() {
            // Overwrite the history file with the deduplicated version if it changed.
            let mut file = BufWriter::new(File::create(&path)?);
            // Write the commands to the history file.
            for command in buf.into_iter() {
                let _ = file.write_all(&command.as_bytes());
                let _ = file.write_all(b"\n");
            }
        }
        Ok("De-duplicated history file.".to_string())
    }

    /// Set history file name and at the same time load the history.
    pub fn set_file_name_and_load_history<P: AsRef<Path>>(&mut self, path: P) -> io::Result<u64> {
        let path = path.as_ref();
        self.file_name = path.to_str().map(|s| s.to_owned());
        self.file_size = 0;
        if path.exists() {
            self.load_history_file(path, false).map(|l| {
                self.file_size = l;
                l
            })
        } else {
            File::create(path)?;
            Ok(0)
        }
    }

    /// Set maximal number of buffers stored in memory
    pub fn set_max_buffers_size(&mut self, size: usize) {
        self.max_buffers_size = size;
    }

    /// Set maximal number of entries in history file
    pub fn set_max_file_size(&mut self, size: usize) {
        self.max_file_size = size;
    }

    /// Number of items in history.
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.buffers.len()
    }

    /// Is the history empty
    pub fn is_empty(&self) -> bool {
        self.buffers.is_empty()
    }

    /// Add a command to the history buffer and remove the oldest commands when the max history
    /// size has been met. If writing to the disk is enabled, this function will be used for
    /// logging history to the designated history file.
    pub fn push(&mut self, new_item: Buffer) -> io::Result<()> {
        // buffers[0] is the oldest entry
        // the new entry goes to the end
        if !self.append_duplicate_entries
            && self.buffers.back().map(|b| b.to_string()) == Some(new_item.to_string())
        {
            return Ok(());
        }

        let item_str = String::from(new_item.clone());
        self.buffers.push_back(new_item);
        //self.to_max_size();
        while self.buffers.len() > self.max_buffers_size {
            self.buffers.pop_front();
        }

        if self.inc_append && self.file_name.is_some() {
            if !self.load_duplicates {
                // Do not want duplicates so periodically compact the history file.
                self.compaction_writes += 1;
                // Every 30 writes "compact" the history file by writing just in memory history.  This
                // is to keep the history file clean and at a reasonable size (not much over max
                // history size at it's worst).
                if self.compaction_writes > 29 {
                    if self.share {
                        // Reload history, we may be out of sync.
                        let _ = self.load_history(false);
                        // Commit the duplicated history.
                        if let Some(file_name) = self.file_name.clone() {
                            let _ = self.overwrite_history(file_name);
                        }
                    } else {
                        // Not using shared history so just de-dup the file without messing with
                        // our history.
                        if let Some(file_name) = self.file_name.clone() {
                            let _ =
                                History::deduplicate_history_file(file_name, self.max_file_size);
                        }
                    }
                    self.compaction_writes = 0;
                }
            } else {
                // If allowing duplicates then no need for compaction.
                self.compaction_writes = 1;
            }
            let file_name = self.file_name.clone().unwrap();
            if let Ok(inner_file) = std::fs::OpenOptions::new().append(true).open(&file_name) {
                // Leave file size alone, if it is not right trigger a reload later.
                if self.compaction_writes > 0 {
                    // If 0 we "compacted" and nothing to write.
                    let mut file = BufWriter::new(inner_file);
                    let _ = file.write_all(&item_str.as_bytes());
                    let _ = file.write_all(b"\n");
                    // Save the filesize after each append so we do not reload when we do not need to.
                    self.file_size += item_str.len() as u64 + 1;
                }
            }
        }
        Ok(())
    }

    /// Removes duplicate entries in the history
    pub fn remove_duplicates(&mut self, input: &str) {
        self.buffers.retain(|buffer| {
            let command = buffer.lines().concat();
            command != input
        });
    }

    fn get_match<I>(&self, vals: I, search_term: &Buffer) -> Option<usize>
    where
        I: Iterator<Item = usize>,
    {
        vals.filter_map(|i| self.buffers.get(i).map(|t| (i, t)))
            .find(|(_i, tested)| tested.starts_with(search_term))
            .map(|(i, _)| i)
    }

    /// Go through the history and try to find an index (newest to oldest) which starts the same
    /// as the new buffer given to this function as argument.  Starts at curr_position.  Does no wrap.
    pub fn get_newest_match(
        &self,
        curr_position: Option<usize>,
        new_buff: &Buffer,
    ) -> Option<usize> {
        let pos = curr_position.unwrap_or_else(|| self.buffers.len());
        if pos > 0 {
            self.get_match((0..pos).rev(), new_buff)
        } else {
            None
        }
    }

    pub fn get_history_subset(&self, search_term: &Buffer) -> Vec<usize> {
        let mut v: Vec<usize> = Vec::new();
        let mut ret: Vec<usize> = (0..self.len())
            .filter(|i| {
                if let Some(tested) = self.buffers.get(*i) {
                    let starts = tested.starts_with(search_term);
                    let contains = tested.contains(search_term);
                    if starts {
                        v.push(*i);
                    }
                    contains && !starts && tested != search_term
                } else {
                    false
                }
            })
            .collect();
        ret.append(&mut v);
        ret
    }

    pub fn search_index(&self, search_term: &Buffer) -> Vec<usize> {
        (0..self.len())
            .filter_map(|i| self.buffers.get(i).map(|t| (i, t)))
            .filter(|(_i, tested)| tested.contains(search_term))
            .map(|(i, _)| i)
            .collect()
    }

    /// Get the history file name.
    #[inline(always)]
    pub fn file_name(&self) -> Option<&str> {
        self.file_name.as_ref().map(|s| s.as_str())
    }

    fn truncate(&mut self) {
        // Find how many lines we need to move backwards
        // in the file to remove all the old commands.
        if self.buffers.len() >= self.max_file_size {
            let pop_out = self.buffers.len() - self.max_file_size;
            for _ in 0..pop_out {
                self.buffers.pop_front();
            }
        }
    }

    fn overwrite_history<P: AsRef<Path>>(&mut self, path: P) -> io::Result<String> {
        self.truncate();
        let mut file = BufWriter::new(File::create(&path)?);

        // Write the commands to the history file.
        for command in self.buffers.iter().cloned() {
            let _ = file.write_all(&String::from(command).as_bytes());
            let _ = file.write_all(b"\n");
        }
        Ok("Wrote history to file.".to_string())
    }

    pub fn commit_to_file_path<P: AsRef<Path>>(&mut self, path: P) -> io::Result<String> {
        if self.inc_append {
            Ok("Nothing to commit.".to_string())
        } else {
            self.overwrite_history(path)
        }
    }

    pub fn commit_to_file(&mut self) {
        if let Some(file_name) = self.file_name.clone() {
            let _ = self.commit_to_file_path(file_name);
        }
    }
}

impl<'a> IntoIterator for &'a History {
    type Item = &'a Buffer;
    type IntoIter = vec_deque::Iter<'a, Buffer>;

    fn into_iter(self) -> Self::IntoIter {
        self.buffers.iter()
    }
}

impl Index<usize> for History {
    type Output = Buffer;

    fn index(&self, index: usize) -> &Buffer {
        &self.buffers[index]
    }
}

impl IndexMut<usize> for History {
    fn index_mut(&mut self, index: usize) -> &mut Buffer {
        &mut self.buffers[index]
    }
}
