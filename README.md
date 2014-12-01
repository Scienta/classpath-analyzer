# Classpath Analyzer

A simple, run-time perusal of your classpath (or other collection of jar files) that helps detect unexpected
conflicts.

## Class analysis

Suddenly getting an unexpected IncompatibleClassChangeError or other LinkageError?  That's your classpath 
biting you. Some change has shuffled the order of the jars, or introduced a different version of some library. 
Don't fret, just run the Classpath Analyzer at startup and inspect the results.
