from datetime import date

copyright = f"{date.today().year}, deemp"

project = "flakes"

rst_epilog = f"""
.. |project| replace:: {project}
.. |dfb| replace:: Don't forget to backup before proceeding.
"""

highlight_language = "none"

html_title = project

html_theme = "classic"
