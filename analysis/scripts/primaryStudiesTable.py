import csv
from jinja2 import Template


data = csv.reader(open("../data/rawData/data.csv"), delimiter=';')
next(data, None) 

out_file = open("Appendix.primary.studies.tex", "w")

template = Template(
"""{\\tiny 
\\begin{longtable}[htbp]{| p{.05\\textwidth} | p{.6\\textwidth} |  p{.3\\textwidth} |  p{.05\\textwidth} |} \\hline 
{\\bf ID} & {\\bf Title} & {\\bf Authors} & {\\bf Year} \\\\ \\hline
{% for paper in papers -%}
	P{{ paper[0]}} & {{paper[2]}} &  & {{paper[3]}} \\\\ \\hline
{% endfor %}
\\caption{ Primary Studies} 
\\label{tab:primarystudies}
\\end{longtable}}""")

out_file.write(template.render(papers=[paper for paper in data]))
out_file.flush()
out_file.close()
