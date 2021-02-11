# alternative

Experiments in preventing error information loss related to use of `Alternative`.   
Details are described in my blog post (TODO provide link)  

Contains examples `Alternative` instances that care about errors.  

* `Alternative.Instances.ErrWarn` contains the blueprint `Either e (w,_)` instance
* `Alternative.Instances.ErrWarnT` transformer based on `ErrWarn`
* `Alternative.Instances.WarnParser` contains  `s -> (s, Either e (w,_))` parser instance


Comparison definitions

* `Alternative.Instances.TraditionalParser`
* _attoparsec_ is used for comparison as well

Proof of concept work on Alternative and MonadPlus replacements 

* `Prototype.Recover` - stronger version of `MonadPlus` with semantics focused on errors and warnings.
* `Prototype.Vlternative`
* `Prototype.WonadPlus`

Examples:

* `Alternative.Examples`
* `Prototype.Vlternative.Examples`



