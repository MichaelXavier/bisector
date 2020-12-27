# Bisector

A slapdash tool that acts like git bisect but without git. You give it
a sequence of versions where the first verison is known to be good and
the last version is known to be bad. It will interactively binary
search the versions and tell you the first one that was bad.
