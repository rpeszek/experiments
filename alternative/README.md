# alternative

Experiments in preventing error information loss related to use of `Alternative`.   
Details are described in my blog post (TODO provide link)  

Contains examples `Alternative` instances that care about errors.  

* `Alternative.Instances.ErrWarn` contains the blueprint `Either e (w,_)` instance
* `Alternative.Instances.ErrWarnT` transformer based on `ErrWarn`
* `Alternative.Instances.WarnParser` contains  `s -> (s, Either e (w,_))` parser instance


Comparison instances

* `Alternative.Instances.TraditionalParser`
* _attoparsec_ is used for comparison as well

Proof of concept conceptual work on Alternative and MonadPlus replacements 

* `Prototype.Recover` - recover errors and warnings in the same Functor, ends up being stronger version of `MonadPlus`
* `Prototype.Vlternative` - straightforward Alternative replacements (Semigroup2, etc)
* `Prototype.WonadPlus` - `many` `some` return error information

Examples:

* `Alternative.Examples` - examples supporting blog post and more
* `Prototype.Vlternative.Examples` - not much here yet



