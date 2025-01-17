# Lambda Diagrams

*Disclaimer: this is not an official Google product*

An implementation of John Tromp's [lambda diagrams][1] graphical notation for
lambda terms. It reads a lambda term on the standard input and renders an
animation of its evaluation.

This [youtube playlist][2] showcases animations produced with this program.

Usage:

```
stack build
stack exec lambda-diagrams < prog.lam
avconv -framerate 10 -f image2 -i /tmp/out_%04d.png -c:v h264 -crf 1 lam.mp4
```

[1]: https://tromp.github.io/cl/diagrams.html
[2]: https://www.youtube.com/playlist?list=PLi8_XqluS5xc7GL-bgVrxpA2Uww6nK0gV

# What is this fork for?

I am using this mainly for my own learning- I am reading Peter Selinger's
"Lecture Notes on the Lambda Calculus" and following along with the
exercises in this repo. As I do so, I am creating my own suite of lambda
calculus functions and structures (alex.lam,) with the intent of developing
deeper intuition for the LC.
