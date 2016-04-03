# dive
The DIVE System implemented in the R language
DIVE stands for "Drawing Interactively for Visual Exploration".
It is an interactive graphics system first proposed by me in 2012.
It was the subject of a talk I gave at the COMPSTAT Conference at Limassol, Cyprus in the same year.
In DIVE, the analyst can actually "draw" point configurations, that are of interest to him.
Thus, for instance, if there are two classes in the data, which are presently mixed, the analyst may draw so that the two classes separate.
The software then tries to find data projections that conform as best as possible to the user's request.
It does by using a modification of the basic cepp algorithm.
This repository is the code for the DIVE system implemented in R, with the graphical fromt-end provided by GGobi, via the RGGobi package.
The problem with R, however, is that being single threaded, the analyst is constrained to do only one thing at a time.
That, is, she can either draw, or she must wait upon the system to find views, or she must wait for the computer to draw the views found.
What we need to do next is build this system in a high performance, real computer language like C++.
