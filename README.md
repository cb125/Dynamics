# Dynamic Semantics: From content to uptake

## NYU semantics seminar, spring 2021

## Rolling update as of 10 Feb

### [Chris Barker](https://cb125.github.io), NYU Linguistics, chris.barker@nyu.edu

Utterances do things: they hire and inquire, they warn and they
inform.  And so dynamic theories of meaning model both the content and
some portion of the effect of an utterance.  This is a presumptuous
thing to do, since uptake depends on many factors, of which content is
just one.  Do we need dynamic theories just to compute content? (Yes!)
Should we even *try* to compute uptake? (Good luck...)

We'll begin with the classic dynamic semantics of the 80s and 90s,
working up to Groenendijk, Stokhof and Veltman's 95 Coreference and
Modality.  We'll follow with a more nuanced and skeptical phase, exploring a
rich body of work from Rutgers.  Finally, we'll consider Goodman and
Frank's Rational Speech Act framework, which provides tools for
reasoning about the relationship between content and uptake.

This course welcomes philosophers and linguists from NYU, CUNY,
Rutgers, Columbia, and beyond.

There will be plenty of linguisticy empirical data detail, but we will
keep philosophical issues in view as we evaluate proposals.

Mode and coordinates: the seminar will be conducted remotely via Zoom.
The first meeting is on Wednesday 3 February 2021 from 15:30 till
17:30 New York time.  The zoom link is https://nyu.zoom.us/j/97945726284.
If you're interested, but you can't make the
first zoom meeting for some reason, please write to me at
chris.barker@nyu.edu.

Historical note: I last taught a seminar on dynamic semantics in 2008.
We'll be engaging with the work of several people who participated in
that earlier seminar.

**Related NYU seminar**: Mandelkern, Conditionals and epistemic modals, Thu 11-1.
Matt's slogan for the seminar: "there's something dynamic about language, 
but we can't account for that by folding those dynamics into the truth/update 
conditions of our connectives."

*Thanks to Dan Harris, Jeremy Kuhn, and Matt Mandelkern for comments on this syllabus.*

## Tentative schedule

See [bibliography](https://github.com/cb125/Dynamics/blob/main/bibliography.md) for full citation details.

1. 3 Feb. **Overview**: sketch of how the seminar might go.  I'll present
   the simple system of Heim 1983, which aims at anaphora and
   presupposition projection.  I'll say what it means for a semantics to be dynamic,
   and I'll articulate the Dynamic Quandry: either we compute content based on 
   an ever-changing context beyond our control, or we try to control the context, and fail.
    * Slides [here](https://github.com/cb125/Dynamics/blob/main/Materials/01-heim-slides.pdf).
    * Assigned reading: [Heim 1983](https://github.com/cb125/Dynamics/blob/main/Papers/heim-1983-projection-problem.pdf).  
    * Secondary readings: [Lewis 1979](https://github.com/cb125/Dynamics/blob/main/Papers/lewis-scorekeeping.pdf) (Scorekeeping); [Harris 2019](https://github.com/cb125/Dynamics/blob/main/Papers/harris-2019_WeTalkToPeopleNotContexts.pdf).  
    * Code: 
      * An implementation of Heim's fragment with problem set [here](https://github.com/cb125/Dynamics/blob/main/Materials/01-heim.hs).  
        - Notes on the implementation [here](https://github.com/cb125/Dynamics/blob/main/Materials/01-heim-fragment.pdf).
      * A refactoring of Heim's fragment that is both eliminative and distributive (pointwise) [here](https://github.com/cb125/Dynamics/blob/main/Materials/01-heim-fragment-pointwise.hs).

### Unit 1: Rewind

2. 10 Feb. [Dynamic Predicate Logic](https://github.com/cb125/Dynamics/blob/main/Papers/groenendijk-stokhof-dpl.pdf).  1991.  Gronendijk and Stokhof.
   Truth conditions of donkey sentences as a dynamic setpiece.  Now update is primary, and truth is derived.  Predecessor and inspiration: Pratt's Dynamic Logic.  Important variants: Dekker's PLA, Musken's CDRT, van den Berg's Plural Information States, de Groote's continuation-based Montagovian dynamics.
      * Slides [here](https://github.com/cb125/Dynamics/blob/main/Materials/02-dpl-slides.pdf).
      * Notes on evaluation order from chapter 12 of Barker and Shan 2014 [here](https://github.com/cb125/Dynamics/blob/main/Materials/02-notes-eval-order.pdf).
      * Implementation and problem set [here](https://github.com/cb125/Dynamics/blob/main/Materials/02-dpl.hs).

3. 17 Feb. [Coreference and Modality](https://github.com/cb125/Dynamics/blob/main/Papers/gsv-coreference-and-modality.pdf).  1996. Gronendijk, Stokhof and
   Veltmann.  Dynamic anaphora meets epistemic update.  A high water
   mark in the old school style of dynamical systems.  Fascinating
   fragment that makes subtle predictions about how epistemic state
   influences truth.  I'll present a refactored fragment based on work
   of Jim Pryor.  
   * Slides [here](https://github.com/cb125/Dynamics/blob/main/Materials/03-gsv-slides.pdf).
   * [Notes on GSV from a seminar Jim Pryor and I taught in 2015](http://lambda.jimpryor.net/topics/week10_gsv/).
   * Implementation and problem set [here](https://github.com/cb125/Dynamics/blob/main/Materials/03-gsv.hs)

4. 24 Feb. A case for, and a case against, dynamic semantics, both sides argued by Philippe Schlenker:
   * [Anti-dynamics](https://github.com/cb125/Dynamics/blob/main/Papers/schlenker2007_Anti-dynamicsPresuppositionPro.pdf). 2007. Validating
     classical logic should be a design goal.  Case study: 
     accounting for presupposition projection in a
     static semantics.  
   * [The view from sign language](https://github.com/cb125/Dynamics/blob/main/Papers/schlenker-2011_DonkeyAnaphoraTheViewFromSignL.pdf). 2011.
     On the other hand, ASL provides an argument in favor of doing donkeys dynamically.

5. 3 Mar. [No general meeting; individual conferences with enrolled students]

### Unit 2: Fast Forward

6. 10 Mar. [Discourse dynamics, pragmatics, and
   indefinites](https://github.com/cb125/Dynamics/blob/main/Papers/lewis2012_DiscourseDynamicsPragmaticsAnd.pdf). 2012. Karen Lewis.  Even the ability of an indefinite to contribute a new discourse referent is negotiable.

7. 17 Mar. [Varieties of update](https://github.com/cb125/Dynamics/blob/main/Papers/murray-2014-varieties-of-update.pdf). 2014.  Sarah Murray. Some update is
   non-negotiable, including evidential inferences in Cheyenne.

8. 24 Mar. [Appositive impositions in discourse](https://github.com/cb125/Dynamics/blob/main/Papers/anderbois-brasoveanu-henderson-2015-appositive-dynamics.pdf). 2015. AnderBois,
   Brasoveanu, and Henderson. Appositives non-negotiably impose their
   content on the context state.  Getting at-issue content right
   depends on tracking appositive content.  This approach is explored 
   at length in Todor Koev's forthcoming OUP book.

9. 31 Mar. [A preference semantics for imperatives](https://github.com/cb125/Dynamics/blob/main/Papers/starr-2020-dynamic-imperatives.pdf). 2020. Will Starr.
   "I will argue that the only way for the non-representationalist to 
   meet these three challenges is to adopt a dynamic semantics. 
   Such a dynamic semantics is proposed here: imperatives 
   introduce preferences between alternatives."

10. 7 Apr. [Post-suppositions and semantic theory](https://github.com/cb125/Dynamics/blob/main/Papers/charlow-2016-Post-suppositions.pdf). 2018ish. Simon Charlow.
    Two ways of climbing the type hierarchy in a dynamic theory in order to
    get cummulative readings right.  One way lifts the type of generalized quantifiers 
    (split scope).  The other lifts the type of propositions (a kind of update semantics).
    Other dynamic split-scope analyses: Bumford 2018 (*the rabbit in the hat*), Law 2018 (distributivity),
    Kuhn to appear (negative concord).

11. 14 Apr.  [The dynamics of loose talk](https://github.com/cb125/Dynamics/blob/main/Papers/carter-loose-talk.pdf). To appear.  Sam Carter.  
    A dynamic account of certain assertions that are not literally true, 
    along with arguments that the treatment must be semantic rather than pragmatic.

### Unit 3: Rational Speech Act theory

12. 21 Apr. Two papers in apposition (the second paper is 1 page long):
    * [Semantics without semantic content](https://github.com/cb125/Dynamics/blob/main/Papers/harris-2020-SemanticsWithoutSemanticContent.pdf). 2020.  Dan Harris.  "A sentence's semantic value is not its content but a partial and defeasible constraint on what it can be used to say."  
    * [Predicting pragmatic reasoning in language games](https://github.com/cb125/Dynamics/blob/main/Papers/frank-goodman-2012.pdf). 2012. Michael Frank and Noah Goodman.  A quantitative theory of how general cognition selects among possible contents.

13. 28 Apr.  [Adjectival vagueness in a Baysian model of interpretation](https://github.com/cb125/Dynamics/blob/main/Papers/lassiter-goodman-2017_AdjectivalVaguenessInABayesian.pdf). 2017. Dan Lassiter and Noah Goodman. How vagueness can emerge from reasoning about adjective extensions.

14. 5 May. [Student presentations]

