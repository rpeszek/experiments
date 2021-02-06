# alternative

The purpose of this repo is described in my blog post (TODO provide link)

Contains examples `Alternative` instances that care about errors.  

* `Alternative.Instances.ErrWarn` contains the blueprint `Either e (w,_)` instance
* `Alternative.Instances.REW` contains `r -> Either e (w,_)` instance
* `Alternative.Instances.WarnParser` contains  `s -> (s, Either e (w,_))` parser instance
* `Alternative.Instances.Annotate` is about annotating existing applicatives with static errors

Comparison definitions

* `Alternative.Instances.TraditionalParser`
* _attoparsec_ is used for comparison as well

Proof of concept work on alternatives to Alternative

* `Vlternative`
* `WonadPlus`

Examples:

* `Alternative.Examples`
* `Vlternative.Examples`



