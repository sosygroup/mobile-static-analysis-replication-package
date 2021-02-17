from tika import parser
from os import path
import re
import glob
import csv

TARGET_DIR = '../svn2/gianluca/MappingStaticAnalysisApps/primaryStudies'
OUTFILE = 'emails.csv'

pdfs = glob.glob(path.join(TARGET_DIR, '*.pdf'))

results = []
for paper in pdfs:
    with open(paper) as p:
        raw = parser.from_file(paper)
        text_lines = raw['content'].split('\n')

        # print(text_lines)
        # print("--------------------")

        # Retrieve the title
        title = next(x for x in text_lines if len(x) > 0)

        # Retrieve authors names
        authors = next(x for x in text_lines if ',' in x and not x.startswith(title))
        authors = re.sub('[^A-Za-z \s]', '', authors)

        #TODO match well formed emails x@y.com
        # Retrieve authors emails
        emails_raw = [x for x in text_lines if '@' in x
                        and 'Permissions@acm.org' not in x
                        and 'permissions@acm.org' not in x]

        res = {
            'UID': path.splitext(path.basename(paper))[0],
            'title': title,
            'authors': authors,
            'emails': ','.join(emails_raw)
        }
        results.append(res)

    with open(OUTFILE, 'w+') as o:
        writer = csv.DictWriter(o, fieldnames=res.keys())
        for p in results:
            writer.writerow(p)




