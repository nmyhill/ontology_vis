source("airtable csv nodes process.R")
source("Network diagram.R")
htmlwidgets::saveWidget(ontology_vis, "index.html", selfcontained = TRUE)
system("git add index.html && git commit -m 'Auto update' && git push origin main")
