# coding: utf8

# Copyright 2016 Vincent Jacques <vincent@vincent-jacques.net>

master_doc = "index"
project = "hashids-ocaml"
version = "1.0.0"  # @todo Remove triplication of version (/opam, /META and /doc/conf.py)
release = version
author = '<a href="http://vincent-jacques.net/contact">Vincent Jacques</a>'
copyright = "2016 {}".format(author)
extensions = []

nitpicky = True
# nitpick_ignore


# https://github.com/bitprophet/alabaster
# html_theme_path
extensions.append("alabaster")
html_theme = "alabaster"
html_sidebars = {
    "**": ["about.html", "navigation.html", "searchbox.html"],
}
html_theme_options = {
    "github_user": "jacquev6",
    "github_repo": project,
    "github_banner": True,
    "travis_button": True,
}
# html_logo
