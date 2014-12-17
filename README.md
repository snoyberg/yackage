## yackage

This package installs a yackage executable that runs a simplistic hackage-like
server. It allows you to upload packages produced via cabal sdist and install
them via cabal. The trick is to add the yackage repository to your cabal config
file, with a line such as:

    remote-repo: yackage:http://localhost:3500/
