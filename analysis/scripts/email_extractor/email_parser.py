import csv
import pyparsing as pp

LBRACE, RBRACE = map(pp.Suppress, "{}")
email_part = pp.quotedString | pp.Word(pp.printables, excludeChars=',{}@')

# define a compressed email, and assign names to the separate parts
# for easier processing - luckily the default delimitedList delimiter is ','
compressed_email = (LBRACE
                    + pp.Group(pp.delimitedList(email_part))('names')
                    + RBRACE
                    + '@'
                    + email_part('trailing'))


# add a parse-time callback to expand the compressed emails into a list
# of constructed emails - note how the names are used
def expand_compressed_email(t):
    return ["{}@{}".format(name, t.trailing) for name in t.names]


compressed_email.addParseAction(expand_compressed_email)

# some lists will just contain plain old uncompressed emails too
# Combine will merge the separate tokens into a single string
plain_email = pp.Combine(email_part + '@' + email_part)

# the complete list parser looks for a comma-delimited list of compressed
# or plain emails
email_list_parser = pp.delimitedList(compressed_email | plain_email)
with open('JSS Emails.csv') as in_, open('parsed_mails.csv', 'w+') as out:
    writer = csv.writer(out, delimiter=',')
    reader = csv.reader(in_, delimiter='\t')
    next(reader, None)
    for row in reader:
        if row[1] != '-':
            joined_mails = ' '.join(email_list_parser.parseString(row[1]))
        else:
            joined_mails = ''
        writer.writerow([row[0], joined_mails])


