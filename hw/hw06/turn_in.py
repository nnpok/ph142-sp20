#!/usr/bin/env python3
import requests
import getpass
import subprocess
import sys
# import extract_pdf 
from os import listdir
from os.path import isfile, join

# Change these for each assignment
ASSIGNMENT_RMD_PATH = 'hw06-poisson-and-CI.Rmd'
COURSE_ID = 79434
ASSIGNMENT_ID = 378503
# FRQ_PAGES = [4, 5, 7, 8, 11, 12, 13, 16, 17, 19, 20, 23] # pages to be exported as FRQ (from knitted file)

# Don't change these
PDF_PATH = ASSIGNMENT_RMD_PATH.replace('.Rmd', '.pdf')
FRQ_PATH = ASSIGNMENT_RMD_PATH.replace('.Rmd', '_frq.pdf')
BASE_URL = 'https://www.gradescope.com'

class APIClient:
    def __init__(self):
        self.session = requests.Session()

    def post(self, *args, **kwargs):
        return self.session.post(*args, **kwargs)

    def log_in(self, email, password):
        url = "{base}/api/v1/user_session".format(base=BASE_URL)

        form_data = {
            "email": email,
            "password": password
        }
        r = self.post(url, data=form_data)

        self.token = r.json()['token']

    def upload_programming_submission(self, course_id, assignment_id, student_email, filenames):
        url = "{base}/api/v1/courses/{0}/assignments/{1}/submissions".format(
            course_id, assignment_id, base=BASE_URL
        )

        form_data = {
            "owner_email": student_email
        }
        files = [('files[]', (filename, open(filename, 'rb'))) for filename in filenames]

        request_headers = {'access-token': self.token}

        try:
            r = self.post(url, data=form_data, headers=request_headers, files=files)
            print("Submission successful!")
            return r
        except:
            print("Error occurred while uploading...\nPlease make sure you didn't change the name" + \
            " of your assignment file and try again.\n If this issue persists, contact your GSI.")

if __name__ == '__main__':
    try:
        cmd = "rmarkdown::render('{}',output_file='{}')".format(ASSIGNMENT_RMD_PATH, PDF_PATH)
        subprocess.check_output(['R', '-e', cmd], stderr = None)
    except:
        sys.exit("-----\nError... make sure you haven't changed the assignment file name.")

    # try:
    #     extract_pdf.export(PDF_PATH, FRQ_PATH, FRQ_PAGES)
    # except:
    #     sys.exit("PDF extraction for free-response questions failed. Please check that you " + \
    #         "have the correct number of pages in your submission.")

    client = APIClient()
    email = input("Please provide the email address on your Gradescope account: ")
    password = getpass.getpass('Password: ')
    try: 
        client.log_in(email, password)
    except:
        sys.exit("-----\nError... wrong username or password. Please try again.")

    # Use the APIClient to upload submissions after logging in, e.g:
    # client.upload_pdf_submission(1234, 5678, 'student@example.edu', 'submission.pdf')
    src_files = [join("src", f) for f in listdir("src") if isfile(join("src", f))]
    files = src_files + [ASSIGNMENT_RMD_PATH, PDF_PATH]
    client.upload_programming_submission(COURSE_ID, ASSIGNMENT_ID, email, files)
    # You can get course and assignment IDs from the URL, e.g.
    # https://www.gradescope.com/courses/1234/assignments/5678
    # course_id = 1234, assignment_id = 5678
