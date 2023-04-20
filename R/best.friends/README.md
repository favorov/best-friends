best.friends test R implementation
===

This is R implementation of the gest.briends test.
The idea is: we have N elements and M communities and 
relations described as NxM matrix.
The stronger is the relation, the higher is the number.
For each element, we wnat to know the most friendly community.  

0.0.1 - initial version.  
0.1.1 - p-value calculated.  
0.2.1 - cpp based p-value, first working version.  
0.2.2 - process NAs in relation; use frankv order parameter for the direction.  
0.2.3 - returns names for the feature and the friend.  
0.2.4 - we added the calculation for n top entities - friends of the feature.  
Possibly, n is the number of the features we know, so we test each for being
the worst of best friends.  
0.2.5 - documentation updated, vignette added.  
0.3.0 - names changed, documentation updated.  
0.99.0 - we changed the terminology to elements+communities, added the friends test, prepared a vignette.  
0.99.1 - devtools::check passed with one note.  
0.99.3 - devtools::check passed with no notes or errors.   
0.99.4 - the vingnette is fixed and improved.   
0.99.5 - trigger re-check, the maiilist error fixed.   
0.99.6 - Documentation updated; non-diagonal options added. 
