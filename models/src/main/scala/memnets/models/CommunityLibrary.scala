package memnets.models

import memnets.core.Library

/**
 * if you'd like to contribute a model:
 *
 * 1) fork repository and do so here
 * 2) refer to the StandardLibrary for best practices
 * 3) if your model needs data, figure out a way to generate it in a function
 * 4) if machine learning data, then add it to ml.DataSources/ml.DataGens or use existing
 *
 * if you have a reusable component:
 *
 * 1)  add the class under the appropriate package (e.g., PredPrey is under biology)
 * 1b) if a broad category is missing, create a new package, but use existing when possible
 * 1c) if machine learning and can implement the Learner trait, then put it in memnets.ml
 * 2)  make a model here using it
 */
object CommunityLibrary extends Library {}
