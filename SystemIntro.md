# GeN - an open-source system for learning generative models of relational data #
# System Introduction (technical) #

## What do we mean by "relational data"? ##
We mean propositions that represent relationships between objects.   We represent the propositions by ordered tuples.   By convention, the first element of the tuple represents the type of relationship and the remaining elements represent the objects.  For example, **`[ MotherOf Alice Bob ]`** or **`[ CanFly Tweety ]`**.

## What do we mean by a "generative model"? ##
Imagine that we have a virtual machine that, according to random choices it makes, comes up with a set of probabilistic rules, and, according to more random choices it makes, uses those rules to decide which propositions come true.   A rule might look something like this:

**`[ MotherOf a b ],[ MarriedTo b c ] ->(0.7) [ AnnoysTheHeckOutOf a c ]`**

Which means that for any a,b, and c, as soon as the propositions on the left-hand side come true, they have a .7 chance of immediately causing the proposition on the right-hand side to come true if it hasn't already.

A model is a complete set of choices for that machine.  A model completely specifies what propositions come true and what causes them to come true.  The _likelihood_ of the model is a priori probability of this complete set of random choices. Thus, the sum of the likelihoods of all possible models is at most 1.

## What do we mean by "learning"? ##
Our system is given a _problem specification_ which consists of a set of propositions that are observed to be true and a set of propositions that are observed to be false.  [(more on problem specifications)](Spec.md).  Most propositions are neither observed to be true, nor to be false.   By learning, we mean searching for the highest likelihood model that we can find which satisfies the problem specifications.

## What good is that? ##
In addition to explaining the world (science), we expect that our system will be able to usefully predict the values of hidden tuples.  We can engineer the training data to make that something useful.  For example, our training data could contain some observed questions and answers, and other observed questions with unobserved answers.







