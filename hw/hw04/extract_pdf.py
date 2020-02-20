"""
This script copies specified pages from infile PDF and write it to outfile PDF.
"""
import argparse
from PyPDF2 import PdfFileReader, PdfFileWriter

# def parse_args():
#     parser = argparse.ArgumentParser()
#     parser.add_argument("infile", help="path of input file to pad")
#     parser.add_argument("outfile", help="path of output padded PDF")
#     parser.add_argument('export_pages', nargs='*')
#     return parser.parse_args()

def export(input_path, output_path, export_pages):
    pdf_reader = PdfFileReader(input_path)
    pdf_writer = PdfFileWriter()

    try:
        for x in export_pages:
            pdf_writer.addPage(pdf_reader.getPage(int(x) - 1))
    except IndexError:
        print("Error: page number invalid")

    with open(output_path, 'wb') as fileobj:
        pdf_writer.write(fileobj)

if __name__ == "__main__":
    # args = parse_args()
    export(args.infile, args.outfile, list(args.export_pages))
