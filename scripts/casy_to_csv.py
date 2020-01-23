#!/usr/bin/env python3
"""
    Script for conversion of CASY RAW Data into CSV
    
    Can write two types of files:
    - CSV with counts data per size bin (--counts option)
    - CSV with meta data (WIP) (--meta option)
    
    Meta data can be added to the counts CSV using the --add-header option.
    
    Basic usage:
        casy_to_csv.py --counts YYMMDD_Sample_A.RAW
    
    Generates "YYMMDD_Sample_A.csv" with counts data.
    
    For usage information run following in console:
        casy_to_csv.py -h
"""

import os
import csv
import re
import string

__author__ = "Joeri Jongbloets <j.a.jongbloets@uva.nl>"
__version__ = "2019-07-25"

printable = set(string.printable)


class InvalidLineException(Exception):
    pass


class CASYParser(object):

    def __init__(self, write_counts_file=False, write_meta_file=False, add_meta_headers=None, od=None, **kwargs):
        """
        :param od: Output Directory
        """
        self.writes_counts_file = write_counts_file
        self.writes_meta_file = write_meta_file
        self.add_meta_headers = []
        if isinstance(add_meta_headers, list):
            self.add_meta_headers.extend(add_meta_headers)
        self._od = None
        if od is not None and os.path.exists(od):
            self._od = od
        self.state = 0
        # Meta information
        self.meta = {}
        self.counts_header = []
        self.counts = []
        self.line = None

    def reset(self):
        self.state = 0
        self.meta = {}
        self.counts_header = []
        self.counts = []
        self.line = None

    def parse(self, fp, od=None):
        fn = os.path.basename(fp)
        # reset state
        self.reset()
        # read file
        print("Reading {}".format(fn))
        if self.read_file(fp):
            return self.write_file(fp, od)
        return False

    def read_file(self, fp):
        result = True
        try:
            with open(fp, "rb") as src:
                self.read_file_header(src)
                self.read_header(src)
                self.read_counts_header(src)
                self.read_counts(src)
                self.read_footer(src)
                # quits as soon 2 empty lines are read
        except InvalidLineException:
            print("Unexpected line: {}".format(self.line))
            result = False
        return result

    def read_line(self, src):
        line = src.readline().decode()
        self.line = self.clean_string(line).strip()

    def read_file_header(self, src):
        self.read_line(src)
        if not re.match(r"CASY.+Raw Data Export File$", self.line):
            raise InvalidLineException()
        self.read_line(src)
        m = re.match(r"Software Version:\t([0-9.]+)", self.line)
        if not m:
            raise InvalidLineException()
        print("This file was written using CASY Software: {}".format(m.group(1)))

    def read_header(self, src):
        self.read_line(src)
        while not self.is_counts_header(self.line):
            k, v = "", ""
            if "\t" in self.line:
                k, v = self.line.split("\t")
                k = k.replace(" ", "_").lower()
            if "" not in (k, v):
                self.meta[k] = v
            # read next line
            self.read_line(src)
        print("Available meta headers: {}".format(list(self.meta.keys())))

    def read_counts_header(self, src):
        if not self.is_counts_header(self.line):
            raise InvalidLineException()
        # parse header
        self.counts_header = self.line.split("\t")

    def read_counts(self, src):
        self.read_line(src)
        while self.is_counts_line(self.line):
            values = [float(v) for v in self.line.split("\t")]
            counts = dict(zip(self.counts_header, values))
            # add counts
            self.counts.append(counts)
            self.read_line(src)
        print("Read {} count observations".format(len(self.counts)))
        # ignore sum
        self.read_line(src)

    def read_footer(self, src):
        self.read_line(src)
        while self.line != "":
            k, v = "", ""
            if "\t" in self.line:
                k, v = self.line.split("\t")
            if "" not in (k, v):
                self.meta[k] = v
            # read next line
            self.read_line(src)

    def is_counts_header(self, line):
        return re.match(r"Size Channel(\tCounts \(.*\))+$", line) is not None

    def is_counts_line(self, line):
        return re.match(r"[0-9]+[.,][0-9]+(\t[0-9]+)+", line) is not None

    def clean_string(self, s):
        return "".join(filter(lambda x: x in printable, s))

    def write_file(self, fp, od=None):
        if od is None:
            od = self._od
        if od is None:
            od = os.path.dirname(fp)
        if not os.path.exists(od):
            raise IOError("{} does not exist.".format(od))
        fn, ext = os.path.splitext(os.path.basename(fp))
        if self.writes_counts_file:
            # counts file
            of = os.path.join(od, "{}_counts{}{}".format(fn, os.path.extsep, "csv"))
            print("Writing counts CSV file to {}".format(of))
            self.write_counts_file(of)
        if self.writes_meta_file:
            # meta file
            of = os.path.join(od, "{}_meta{}{}".format(fn, os.path.extsep, "csv"))
            print("Writing meta CSV file to {}".format(of))
            self.write_meta_file(of)

    def write_counts_file(self, of):
        csv_header = self.counts_header.copy()

        # which headers should be added to?
        extra_info = {}
        for header in self.add_meta_headers:
            value = ""
            if header in self.meta.keys():
                value = self.meta[header]
            extra_info[header] = value
            csv_header.append(header)

        with open(of, 'w', newline='') as dest:
            w = csv.DictWriter(dest, csv_header)

            # write
            w.writeheader()
            for counts in self.counts:
                row = {}
                # add counts
                row.update(counts)
                # add extra info
                row.update(extra_info)
                w.writerow(row)

    def write_meta_file(self, of):
        pass


if __name__ == "__main__":
    import argparse

    p = argparse.ArgumentParser(
        description="Parse a HPLC Data File into a CSV File"
    )
    p.add_argument("file")
    p.add_argument(
        "-c", "--counts", dest="write_counts", action="store_true",
        help="Convert RAW file to a file with counts data"
    )
    p.add_argument(
        "-m", "--meta", dest="write_meta", action="store_true",
        help="Convert RAW file to a file with meta data"
    )
    p.add_argument(
        "-a", "--add-header", dest="add_meta_headers", action="append",
        help="Extra meta information to add to the counts file"
    )
    p.add_argument("-o", "--output", dest="output_dir", default=None)

    args = vars(p.parse_args())
    settings = {}

    file_paths = args["file"]
    output_dir = args.get("output_dir", None)

    settings['write_counts_file'] = args.get("write_counts")
    settings['write_meta_file'] = args.get("write_meta")
    settings['add_meta_headers'] = args.get("add_meta_headers")

    if output_dir is not None and not os.path.exists(output_dir):
        ValueError("Output dir {} does not exits".format(output_dir))

    # get files
    import glob

    c = CASYParser(od=output_dir, **settings)
    for fp in glob.glob(file_paths):
        if not os.path.exists(fp):
            ValueError("Input file {} does not exist".format(fp))
        c.parse(fp)
