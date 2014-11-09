// Generated by psc version 0.6.0.1
var PS = PS || {};
PS.Prelude = (function () {
    "use strict";
    function Semigroupoid($less$less$less) {
        this["<<<"] = $less$less$less;
    };
    function Category(__superclass_Prelude$dotSemigroupoid_0, id) {
        this["__superclass_Prelude.Semigroupoid_0"] = __superclass_Prelude$dotSemigroupoid_0;
        this.id = id;
    };
    function Show(show) {
        this.show = show;
    };
    function Functor($less$dollar$greater) {
        this["<$>"] = $less$dollar$greater;
    };
    function Apply($less$times$greater, __superclass_Prelude$dotFunctor_0) {
        this["<*>"] = $less$times$greater;
        this["__superclass_Prelude.Functor_0"] = __superclass_Prelude$dotFunctor_0;
    };
    function Applicative(__superclass_Prelude$dotApply_0, pure) {
        this["__superclass_Prelude.Apply_0"] = __superclass_Prelude$dotApply_0;
        this.pure = pure;
    };
    function Bind($greater$greater$eq, __superclass_Prelude$dotApply_0) {
        this[">>="] = $greater$greater$eq;
        this["__superclass_Prelude.Apply_0"] = __superclass_Prelude$dotApply_0;
    };
    function Monad(__superclass_Prelude$dotApplicative_0, __superclass_Prelude$dotBind_1) {
        this["__superclass_Prelude.Applicative_0"] = __superclass_Prelude$dotApplicative_0;
        this["__superclass_Prelude.Bind_1"] = __superclass_Prelude$dotBind_1;
    };
    function Num($percent, $times, $plus, $minus, $div, negate) {
        this["%"] = $percent;
        this["*"] = $times;
        this["+"] = $plus;
        this["-"] = $minus;
        this["/"] = $div;
        this.negate = negate;
    };
    function Eq($div$eq, $eq$eq) {
        this["/="] = $div$eq;
        this["=="] = $eq$eq;
    };
    function Semigroup($less$greater) {
        this["<>"] = $less$greater;
    };
    function cons(e) {  return function(l) {    return [e].concat(l);  };};
    function showStringImpl(s) {  return JSON.stringify(s);};
    function showNumberImpl(n) {  return n.toString();};
    function showArrayImpl(f) {  return function(xs) {    var ss = [];    for (var i = 0, l = xs.length; i < l; i++) {      ss[i] = f(xs[i]);    }    return '[' + ss.join(',') + ']';  };};
    function numAdd(n1) {  return function(n2) {    return n1 + n2;  };};
    function numSub(n1) {  return function(n2) {    return n1 - n2;  };};
    function numMul(n1) {  return function(n2) {    return n1 * n2;  };};
    function numDiv(n1) {  return function(n2) {    return n1 / n2;  };};
    function numMod(n1) {  return function(n2) {    return n1 % n2;  };};
    function numNegate(n) {  return -n;};
    function refEq(r1) {  return function(r2) {    return r1 === r2;  };};
    function refIneq(r1) {  return function(r2) {    return r1 !== r2;  };};
    function concatString(s1) {  return function(s2) {    return s1 + s2;  };};
    var $greater$greater$eq = function (dict) {
        return dict[">>="];
    };
    var $eq$eq = function (dict) {
        return dict["=="];
    };
    var $less$greater = function (dict) {
        return dict["<>"];
    };
    var $less$times$greater = function (dict) {
        return dict["<*>"];
    };
    var $less$dollar$greater = function (dict) {
        return dict["<$>"];
    };
    var $colon = cons;
    var $div$eq = function (dict) {
        return dict["/="];
    };
    var $minus = function (dict) {
        return dict["-"];
    };
    var $plus$plus = function (__dict_Semigroup_1) {
        return $less$greater(__dict_Semigroup_1);
    };
    var $plus = function (dict) {
        return dict["+"];
    };
    var $dollar = function (f) {
        return function (x) {
            return f(x);
        };
    };
    var showString = new Show(showStringImpl);
    var showNumber = new Show(showNumberImpl);
    var show = function (dict) {
        return dict.show;
    };
    var showArray = function (__dict_Show_2) {
        return new Show(showArrayImpl(show(__dict_Show_2)));
    };
    var semigroupoidArr = new Semigroupoid(function (f) {
        return function (g) {
            return function (x) {
                return f(g(x));
            };
        };
    });
    var semigroupString = new Semigroup(concatString);
    var pure = function (dict) {
        return dict.pure;
    };
    var $$return = function (__dict_Monad_4) {
        return pure(__dict_Monad_4["__superclass_Prelude.Applicative_0"]());
    };
    var numNumber = new Num(numMod, numMul, numAdd, numSub, numDiv, numNegate);
    var liftA1 = function (__dict_Applicative_6) {
        return function (f) {
            return function (a) {
                return $less$times$greater(__dict_Applicative_6["__superclass_Prelude.Apply_0"]())(pure(__dict_Applicative_6)(f))(a);
            };
        };
    };
    var id = function (dict) {
        return dict.id;
    };
    var eqString = new Eq(refIneq, refEq);
    var eqNumber = new Eq(refIneq, refEq);
    var categoryArr = new Category(function () {
        return semigroupoidArr;
    }, function (x) {
        return x;
    });
    var ap = function (__dict_Monad_14) {
        return function (f) {
            return function (a) {
                return $greater$greater$eq(__dict_Monad_14["__superclass_Prelude.Bind_1"]())(f)(function (_2) {
                    return $greater$greater$eq(__dict_Monad_14["__superclass_Prelude.Bind_1"]())(a)(function (_1) {
                        return $$return(__dict_Monad_14)(_2(_1));
                    });
                });
            };
        };
    };
    return {
        "$": $dollar, 
        "+": $plus, 
        "++": $plus$plus, 
        "-": $minus, 
        "/=": $div$eq, 
        ":": $colon, 
        "<$>": $less$dollar$greater, 
        "<*>": $less$times$greater, 
        "<>": $less$greater, 
        "==": $eq$eq, 
        ">>=": $greater$greater$eq, 
        Applicative: Applicative, 
        Apply: Apply, 
        Bind: Bind, 
        Category: Category, 
        Eq: Eq, 
        Functor: Functor, 
        Monad: Monad, 
        Num: Num, 
        Semigroup: Semigroup, 
        Semigroupoid: Semigroupoid, 
        Show: Show, 
        ap: ap, 
        categoryArr: categoryArr, 
        cons: cons, 
        eqNumber: eqNumber, 
        eqString: eqString, 
        id: id, 
        liftA1: liftA1, 
        numNumber: numNumber, 
        pure: pure, 
        refEq: refEq, 
        refIneq: refIneq, 
        "return": $$return, 
        semigroupString: semigroupString, 
        semigroupoidArr: semigroupoidArr, 
        show: show, 
        showArray: showArray, 
        showNumber: showNumber, 
        showString: showString
    };
})();
var PS = PS || {};
PS.Kanren_Var = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Var = {
        create: function (value) {
            return value;
        }
    };
    var zero = 0;
    var succ = function (_34) {
        return Var.create(_34 + 1);
    };
    var showVar = new Prelude.Show(function (_39) {
        return "(Var " + (Prelude.show(Prelude.showNumber)(_39) + ")");
    });
    var eqVar = new Prelude.Eq(function (_37) {
        return function (_38) {
            return _37 !== _38;
        };
    }, function (_35) {
        return function (_36) {
            return _35 === _36;
        };
    });
    return {
        Var: Var, 
        eqVar: eqVar, 
        showVar: showVar, 
        succ: succ, 
        zero: zero
    };
})();
var PS = PS || {};
PS.Kanren_Obj = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Obj = {
        create: function (value) {
            return value;
        }
    };
    var showObj = new Prelude.Show(function (_44) {
        return "(Obj " + (_44 + ")");
    });
    var eqObj = new Prelude.Eq(function (_42) {
        return function (_43) {
            return _42 !== _43;
        };
    }, function (_40) {
        return function (_41) {
            return _40 === _41;
        };
    });
    return {
        Obj: Obj, 
        eqObj: eqObj, 
        showObj: showObj
    };
})();
var PS = PS || {};
PS.Kanren_Term = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Kanren_Var = PS.Kanren_Var;
    var Kanren_Obj = PS.Kanren_Obj;
    function TmVar(value0) {
        this.value0 = value0;
    };
    TmVar.create = function (value0) {
        return new TmVar(value0);
    };
    function TmObj(value0) {
        this.value0 = value0;
    };
    TmObj.create = function (value0) {
        return new TmObj(value0);
    };
    function TmPair(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    TmPair.create = function (value0) {
        return function (value1) {
            return new TmPair(value0, value1);
        };
    };
    var showTerm = new Prelude.Show(function (_45) {
        if (_45 instanceof TmVar) {
            return "#" + Prelude.show(Kanren_Var.showVar)(_45.value0);
        };
        if (_45 instanceof TmObj) {
            return Prelude.show(Kanren_Obj.showObj)(_45.value0);
        };
        if (_45 instanceof TmPair) {
            return "(" + (Prelude.show(showTerm)(_45.value0) + (", " + (Prelude.show(showTerm)(_45.value1) + ")")));
        };
        throw new Error("Failed pattern match");
    });
    return {
        TmObj: TmObj, 
        TmPair: TmPair, 
        TmVar: TmVar, 
        showTerm: showTerm
    };
})();
var PS = PS || {};
PS.Kanren_Goal = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Kanren_Term = PS.Kanren_Term;
    function Done() {

    };
    Done.value = new Done();
    function Unify(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Unify.create = function (value0) {
        return function (value1) {
            return new Unify(value0, value1);
        };
    };
    function Fresh(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Fresh.create = function (value0) {
        return function (value1) {
            return new Fresh(value0, value1);
        };
    };
    function Disj(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Disj.create = function (value0) {
        return function (value1) {
            return new Disj(value0, value1);
        };
    };
    function Conj(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Conj.create = function (value0) {
        return function (value1) {
            return new Conj(value0, value1);
        };
    };
    var showGoal = new Prelude.Show(function (_46) {
        if (_46 instanceof Done) {
            return "Done";
        };
        if (_46 instanceof Unify) {
            return "(Unify" + (" " + (Prelude.show(Kanren_Term.showTerm)(_46.value0) + (" " + (Prelude.show(Kanren_Term.showTerm)(_46.value1) + ")"))));
        };
        if (_46 instanceof Fresh) {
            return "(Fresh" + (" " + (Prelude.show(Prelude.showString)(_46.value0) + (" " + (Prelude.show(showGoal)(_46.value1) + ")"))));
        };
        if (_46 instanceof Disj) {
            return "(Disj" + (" " + (Prelude.show(showGoal)(_46.value0) + (" " + (Prelude.show(showGoal)(_46.value1) + ")"))));
        };
        if (_46 instanceof Conj) {
            return "(Conj" + (" " + (Prelude.show(showGoal)(_46.value0) + (" " + (Prelude.show(showGoal)(_46.value1) + ")"))));
        };
        throw new Error("Failed pattern match");
    });
    return {
        Conj: Conj, 
        Disj: Disj, 
        Done: Done, 
        Fresh: Fresh, 
        Unify: Unify, 
        showGoal: showGoal
    };
})();
var PS = PS || {};
PS.Kanren_Stack = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Kanren_Goal = PS.Kanren_Goal;
    function Empty() {

    };
    Empty.value = new Empty();
    function Push(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Push.create = function (value0) {
        return function (value1) {
            return new Push(value0, value1);
        };
    };
    var showStack = new Prelude.Show(function (_47) {
        if (_47 instanceof Empty) {
            return "Empty";
        };
        if (_47 instanceof Push) {
            return "(Push" + (" " + (Prelude.show(Kanren_Goal.showGoal)(_47.value0) + (" " + (Prelude.show(showStack)(_47.value1) + ")"))));
        };
        throw new Error("Failed pattern match");
    });
    return {
        Empty: Empty, 
        Push: Push, 
        showStack: showStack
    };
})();
var PS = PS || {};
PS.Control_Monad_Eff = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function returnE(a) {  return function() {    return a;  };};
    function bindE(a) {  return function(f) {    return function() {      return f(a())();    };  };};
    var monadEff = new Prelude.Monad(function () {
        return applicativeEff;
    }, function () {
        return bindEff;
    });
    var bindEff = new Prelude.Bind(bindE, function () {
        return applyEff;
    });
    var applyEff = new Prelude.Apply(Prelude.ap(monadEff), function () {
        return functorEff;
    });
    var applicativeEff = new Prelude.Applicative(function () {
        return applyEff;
    }, returnE);
    var functorEff = new Prelude.Functor(Prelude.liftA1(applicativeEff));
    return {
        applicativeEff: applicativeEff, 
        applyEff: applyEff, 
        bindE: bindE, 
        bindEff: bindEff, 
        functorEff: functorEff, 
        monadEff: monadEff, 
        returnE: returnE
    };
})();
var PS = PS || {};
PS.Data_Maybe = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function Nothing() {

    };
    Nothing.value = new Nothing();
    function Just(value0) {
        this.value0 = value0;
    };
    Just.create = function (value0) {
        return new Just(value0);
    };
    var maybe = function (_77) {
        return function (_78) {
            return function (_79) {
                if (_79 instanceof Nothing) {
                    return _77;
                };
                if (_79 instanceof Just) {
                    return _78(_79.value0);
                };
                throw new Error("Failed pattern match");
            };
        };
    };
    var fromMaybe = function (a) {
        return maybe(a)(Prelude.id(Prelude.categoryArr));
    };
    return {
        Just: Just, 
        Nothing: Nothing, 
        fromMaybe: fromMaybe, 
        maybe: maybe
    };
})();
var PS = PS || {};
PS.Data_Array = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function append (l1) {  return function (l2) {    return l1.concat(l2);  };};
    function map (f) {  return function (arr) {    var l = arr.length;    var result = new Array(l);    for (var i = 0; i < l; i++) {      result[i] = f(arr[i]);    }    return result;  };};
    var semigroupArray = new Prelude.Semigroup(append);
    var functorArray = new Prelude.Functor(map);
    return {
        append: append, 
        functorArray: functorArray, 
        map: map, 
        semigroupArray: semigroupArray
    };
})();
var PS = PS || {};
PS.Control_Monad_JQuery = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    
  function parent(ob) {
    return function () {
      return ob.parent();
    };
  }
  ;
    
  function create(html) {
    return function () {
      return jQuery(html);
    };
  }
  ;
    
  function append(ob1) {
    return function(ob) {
      return function () {
        return ob.append(ob1);
      };
    };
  }
  ;
    
  function appendText(s) {
    return function(ob) {
      return function () {
        return ob.append(s);
      };
    };
  }
  ;
    
  function body() {
    return jQuery(document.body);
  }
  ;
    
  function on(evt) {
    return function(act) {
      return function(ob) {
        return function() {
          return ob.on(evt, function(e) {
            act(e)(jQuery(this))();
          });
        };
      };
    };
  }
  ;
    
  function preventDefault(e) {
    return function() {
      e.preventDefault();
    };
  }
  ;
    return {
        append: append, 
        appendText: appendText, 
        body: body, 
        create: create, 
        on: on, 
        parent: parent, 
        preventDefault: preventDefault
    };
})();
var PS = PS || {};
PS.Data_Monoid = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function Monoid(__superclass_Prelude$dotSemigroup_0, mempty) {
        this["__superclass_Prelude.Semigroup_0"] = __superclass_Prelude$dotSemigroup_0;
        this.mempty = mempty;
    };
    var mempty = function (dict) {
        return dict.mempty;
    };
    return {
        Monoid: Monoid, 
        mempty: mempty
    };
})();
var PS = PS || {};
PS.Data_Tuple = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function Tuple(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Tuple.create = function (value0) {
        return function (value1) {
            return new Tuple(value0, value1);
        };
    };
    var showTuple = function (__dict_Show_71) {
        return function (__dict_Show_72) {
            return new Prelude.Show(function (_194) {
                return "Tuple (" + (Prelude.show(__dict_Show_71)(_194.value0) + (") (" + (Prelude.show(__dict_Show_72)(_194.value1) + ")")));
            });
        };
    };
    return {
        Tuple: Tuple, 
        showTuple: showTuple
    };
})();
var PS = PS || {};
PS.Data_Monoid_First = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Maybe = PS.Data_Maybe;
    var Data_Monoid = PS.Data_Monoid;
    var First = {
        create: function (value) {
            return value;
        }
    };
    var semigroupFirst = new Prelude.Semigroup(function (_228) {
        return function (_229) {
            if (_228 instanceof Data_Maybe.Just) {
                return _228;
            };
            return _229;
        };
    });
    var runFirst = function (_220) {
        return _220;
    };
    var monoidFirst = new Data_Monoid.Monoid(function () {
        return semigroupFirst;
    }, Data_Maybe.Nothing.value);
    return {
        First: First, 
        monoidFirst: monoidFirst, 
        runFirst: runFirst, 
        semigroupFirst: semigroupFirst
    };
})();
var PS = PS || {};
PS.Data_Foldable = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Monoid = PS.Data_Monoid;
    var Data_Monoid_First = PS.Data_Monoid_First;
    var Data_Tuple = PS.Data_Tuple;
    var Data_Maybe = PS.Data_Maybe;
    function Foldable(foldMap, foldl, foldr) {
        this.foldMap = foldMap;
        this.foldl = foldl;
        this.foldr = foldr;
    };
    function foldrArray(f) {  return function(z) {    return function(xs) {      var acc = z;      for (var i = xs.length - 1; i >= 0; --i) {        acc = f(xs[i])(acc);      }      return acc;    }  }};
    function foldlArray(f) {  return function(z) {    return function(xs) {      var acc = z;      for (var i = 0, len = xs.length; i < len; ++i) {        acc = f(acc)(xs[i]);      }      return acc;    }  }};
    var foldr = function (dict) {
        return dict.foldr;
    };
    var foldableArray = new Foldable(function (__dict_Monoid_111) {
        return function (f) {
            return function (xs) {
                return foldr(foldableArray)(function (x) {
                    return function (acc) {
                        return Prelude["<>"](__dict_Monoid_111["__superclass_Prelude.Semigroup_0"]())(f(x))(acc);
                    };
                })(Data_Monoid.mempty(__dict_Monoid_111))(xs);
            };
        };
    }, function (f) {
        return function (z) {
            return function (xs) {
                return foldlArray(f)(z)(xs);
            };
        };
    }, function (f) {
        return function (z) {
            return function (xs) {
                return foldrArray(f)(z)(xs);
            };
        };
    });
    var foldMap = function (dict) {
        return dict.foldMap;
    };
    var lookup = function (__dict_Eq_112) {
        return function (__dict_Foldable_113) {
            return function (a) {
                return function (f) {
                    return Data_Monoid_First.runFirst(foldMap(__dict_Foldable_113)(Data_Monoid_First.monoidFirst)(function (_230) {
                        return Prelude["=="](__dict_Eq_112)(a)(_230.value0) ? new Data_Maybe.Just(_230.value1) : Data_Maybe.Nothing.value;
                    })(f));
                };
            };
        };
    };
    return {
        Foldable: Foldable, 
        foldMap: foldMap, 
        foldableArray: foldableArray, 
        foldlArray: foldlArray, 
        foldr: foldr, 
        foldrArray: foldrArray, 
        lookup: lookup
    };
})();
var PS = PS || {};
PS.Data_Traversable = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Array = PS.Data_Array;
    var Data_Foldable = PS.Data_Foldable;
    function Traversable(__superclass_Data$dotFoldable$dotFoldable_1, __superclass_Prelude$dotFunctor_0, sequence, traverse) {
        this["__superclass_Data.Foldable.Foldable_1"] = __superclass_Data$dotFoldable$dotFoldable_1;
        this["__superclass_Prelude.Functor_0"] = __superclass_Prelude$dotFunctor_0;
        this.sequence = sequence;
        this.traverse = traverse;
    };
    var traverse = function (dict) {
        return dict.traverse;
    };
    var sequence = function (dict) {
        return dict.sequence;
    };
    var traversableArray = new Traversable(function () {
        return Data_Foldable.foldableArray;
    }, function () {
        return Data_Array.functorArray;
    }, function (__dict_Applicative_136) {
        return function (_277) {
            if (_277.length === 0) {
                return Prelude.pure(__dict_Applicative_136)([  ]);
            };
            if (_277.length >= 1) {
                var _357 = _277.slice(1);
                return Prelude["<*>"](__dict_Applicative_136["__superclass_Prelude.Apply_0"]())(Prelude["<$>"]((__dict_Applicative_136["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Prelude[":"])(_277[0]))(sequence(traversableArray)(__dict_Applicative_136)(_357));
            };
            throw new Error("Failed pattern match");
        };
    }, function (__dict_Applicative_135) {
        return function (_275) {
            return function (_276) {
                if (_276.length === 0) {
                    return Prelude.pure(__dict_Applicative_135)([  ]);
                };
                if (_276.length >= 1) {
                    var _361 = _276.slice(1);
                    return Prelude["<*>"](__dict_Applicative_135["__superclass_Prelude.Apply_0"]())(Prelude["<$>"]((__dict_Applicative_135["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Prelude[":"])(_275(_276[0])))(traverse(traversableArray)(__dict_Applicative_135)(_275)(_361));
                };
                throw new Error("Failed pattern match");
            };
        };
    });
    var $$for = function (__dict_Applicative_138) {
        return function (__dict_Traversable_139) {
            return function (x) {
                return function (f) {
                    return traverse(__dict_Traversable_139)(__dict_Applicative_138)(f)(x);
                };
            };
        };
    };
    return {
        Traversable: Traversable, 
        "for": $$for, 
        sequence: sequence, 
        traversableArray: traversableArray, 
        traverse: traverse
    };
})();
var PS = PS || {};
PS.Kanren_Subst = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Kanren_Term = PS.Kanren_Term;
    var Data_Maybe = PS.Data_Maybe;
    var Data_Foldable = PS.Data_Foldable;
    var Kanren_Var = PS.Kanren_Var;
    var Data_Tuple = PS.Data_Tuple;
    var walk = function (_290) {
        return function (_291) {
            if (_291 instanceof Kanren_Term.TmVar) {
                return Data_Maybe.fromMaybe(new Kanren_Term.TmVar(_291.value0))(Data_Foldable.lookup(Kanren_Var.eqVar)(Data_Foldable.foldableArray)(_291.value0)(_290));
            };
            return _291;
        };
    };
    var ext = function (v) {
        return function (t) {
            return function (s) {
                return Prelude[":"](new Data_Tuple.Tuple(v, t))(s);
            };
        };
    };
    return {
        ext: ext, 
        walk: walk
    };
})();
var PS = PS || {};
PS.Kanren_State = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Kanren_Goal = PS.Kanren_Goal;
    var Data_Tuple = PS.Data_Tuple;
    var Kanren_Var = PS.Kanren_Var;
    var Kanren_Term = PS.Kanren_Term;
    var Kanren_Stack = PS.Kanren_Stack;
    function State(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    State.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new State(value0, value1, value2, value3);
                };
            };
        };
    };
    var showState = new Prelude.Show(function (_296) {
        return "(State" + (" " + (Prelude.show(Kanren_Goal.showGoal)(_296.value0) + (" " + (Prelude.show(Prelude.showArray(Data_Tuple.showTuple(Kanren_Var.showVar)(Kanren_Term.showTerm)))(_296.value1) + (" " + (Prelude.show(Kanren_Var.showVar)(_296.value2) + (" " + (Prelude.show(Kanren_Stack.showStack)(_296.value3) + ")"))))))));
    });
    return {
        State: State, 
        showState: showState
    };
})();
var PS = PS || {};
PS.Kanren_Unify = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Kanren_Term = PS.Kanren_Term;
    var Kanren_Var = PS.Kanren_Var;
    var Data_Maybe = PS.Data_Maybe;
    var Kanren_Subst = PS.Kanren_Subst;
    var Kanren_Obj = PS.Kanren_Obj;
    var unify = function (u) {
        return function (v) {
            return function (s) {
                var go = function (_298) {
                    return function (_299) {
                        if (_298 instanceof Kanren_Term.TmVar && (_299 instanceof Kanren_Term.TmVar && Prelude["=="](Kanren_Var.eqVar)(_298.value0)(_299.value0))) {
                            return new Data_Maybe.Just(s);
                        };
                        if (_298 instanceof Kanren_Term.TmVar) {
                            return Data_Maybe.Just.create(Kanren_Subst.ext(_298.value0)(_299)(s));
                        };
                        if (_299 instanceof Kanren_Term.TmVar) {
                            return Data_Maybe.Just.create(Kanren_Subst.ext(_299.value0)(_298)(s));
                        };
                        if (_298 instanceof Kanren_Term.TmObj && (_299 instanceof Kanren_Term.TmObj && Prelude["=="](Kanren_Obj.eqObj)(_298.value0)(_299.value0))) {
                            return new Data_Maybe.Just(s);
                        };
                        if (_298 instanceof Kanren_Term.TmPair && _299 instanceof Kanren_Term.TmPair) {
                            var _378 = unify(_298.value0)(_299.value0)(s);
                            if (_378 instanceof Data_Maybe.Nothing) {
                                return Data_Maybe.Nothing.value;
                            };
                            if (_378 instanceof Data_Maybe.Just) {
                                return unify(_298.value1)(_299.value1)(_378.value0);
                            };
                            throw new Error("Failed pattern match");
                        };
                        return Data_Maybe.Nothing.value;
                    };
                };
                return go(Kanren_Subst.walk(s)(u))(Kanren_Subst.walk(s)(v));
            };
        };
    };
    return {
        unify: unify
    };
})();
var PS = PS || {};
PS.Kanren_Eval = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Kanren_State = PS.Kanren_State;
    var Kanren_Goal = PS.Kanren_Goal;
    var Kanren_Stack = PS.Kanren_Stack;
    var Kanren_Term = PS.Kanren_Term;
    var Kanren_Obj = PS.Kanren_Obj;
    var Kanren_Unify = PS.Kanren_Unify;
    var Data_Maybe = PS.Data_Maybe;
    var Kanren_Var = PS.Kanren_Var;
    var Data_Array = PS.Data_Array;
    var step = function (_300) {
        var unwind = function (_302) {
            if (_302.value0 instanceof Kanren_Goal.Done && _302.value3 instanceof Kanren_Stack.Push) {
                return new Kanren_State.State(_302.value3.value0, _302.value1, _302.value2, _302.value3.value1);
            };
            return _302;
        };
        var replace = function (nm) {
            return function (r) {
                var onTerms = function (_304) {
                    if (_304 instanceof Kanren_Term.TmObj && nm === _304.value0) {
                        return r;
                    };
                    if (_304 instanceof Kanren_Term.TmPair) {
                        return new Kanren_Term.TmPair(onTerms(_304.value0), onTerms(_304.value1));
                    };
                    return _304;
                };
                var onGoals = function (_303) {
                    if (_303 instanceof Kanren_Goal.Done) {
                        return Kanren_Goal.Done.value;
                    };
                    if (_303 instanceof Kanren_Goal.Unify) {
                        return new Kanren_Goal.Unify(onTerms(_303.value0), onTerms(_303.value1));
                    };
                    if (_303 instanceof Kanren_Goal.Fresh) {
                        return nm === _303.value0 ? _303 : new Kanren_Goal.Fresh(_303.value0, onGoals(_303.value1));
                    };
                    if (_303 instanceof Kanren_Goal.Disj) {
                        return new Kanren_Goal.Disj(onGoals(_303.value0), onGoals(_303.value1));
                    };
                    if (_303 instanceof Kanren_Goal.Conj) {
                        return new Kanren_Goal.Conj(onGoals(_303.value0), onGoals(_303.value1));
                    };
                    throw new Error("Failed pattern match");
                };
                return onGoals;
            };
        };
        var go = function (_301) {
            if (_301 instanceof Kanren_Goal.Done) {
                return [  ];
            };
            if (_301 instanceof Kanren_Goal.Unify) {
                var _406 = Kanren_Unify.unify(_301.value0)(_301.value1)(_300.value1);
                if (_406 instanceof Data_Maybe.Nothing) {
                    return [  ];
                };
                if (_406 instanceof Data_Maybe.Just) {
                    return [ new Kanren_State.State(Kanren_Goal.Done.value, _406.value0, _300.value2, _300.value3) ];
                };
                throw new Error("Failed pattern match");
            };
            if (_301 instanceof Kanren_Goal.Fresh) {
                return [ new Kanren_State.State(replace(_301.value0)(new Kanren_Term.TmVar(_300.value2))(_301.value1), _300.value1, Kanren_Var.succ(_300.value2), _300.value3) ];
            };
            if (_301 instanceof Kanren_Goal.Disj) {
                return [ new Kanren_State.State(_301.value0, _300.value1, _300.value2, _300.value3), new Kanren_State.State(_301.value1, _300.value1, _300.value2, _300.value3) ];
            };
            if (_301 instanceof Kanren_Goal.Conj) {
                return [ new Kanren_State.State(_301.value0, _300.value1, _300.value2, new Kanren_Stack.Push(_301.value1, _300.value3)) ];
            };
            throw new Error("Failed pattern match");
        };
        return Prelude["<$>"](Data_Array.functorArray)(unwind)(go(_300.value0));
    };
    var example = (function () {
        var obj = function (nm) {
            return new Kanren_Term.TmObj(nm);
        };
        var g2 = new Kanren_Goal.Conj(new Kanren_Goal.Unify(obj("x"), obj("b")), new Kanren_Goal.Unify(obj("x"), obj("y")));
        var g1 = new Kanren_Goal.Conj(new Kanren_Goal.Unify(obj("x"), obj("a")), new Kanren_Goal.Unify(obj("x"), obj("y")));
        var goal = Kanren_Goal.Fresh.create("x")(Kanren_Goal.Fresh.create("y")(new Kanren_Goal.Disj(g1, g2)));
        return new Kanren_State.State(goal, [  ], Kanren_Var.zero, Kanren_Stack.Empty.value);
    })();
    return {
        example: example, 
        step: step
    };
})();
var PS = PS || {};
PS.Kanren_Render = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Kanren_State = PS.Kanren_State;
    var Kanren_Goal = PS.Kanren_Goal;
    var Control_Monad_Eff = PS.Control_Monad_Eff;
    var Control_Monad_JQuery = PS.Control_Monad_JQuery;
    var Kanren_Term = PS.Kanren_Term;
    var Kanren_Var = PS.Kanren_Var;
    var Kanren_Obj = PS.Kanren_Obj;
    var Data_Array = PS.Data_Array;
    var Data_Traversable = PS.Data_Traversable;
    var Kanren_Eval = PS.Kanren_Eval;
    var render = function (_305) {
        if (_305.value0 instanceof Kanren_Goal.Done) {
            return Prelude[">>="](Control_Monad_Eff.bindEff)(Control_Monad_JQuery.create("<li>"))(Control_Monad_JQuery.appendText(Prelude.show(Kanren_State.showState)(_305)));
        };
        var spaces = (function () {
            var go = function (__copy__309) {
                return function (__copy__310) {
                    var _309 = __copy__309;
                    var _310 = __copy__310;
                    tco: while (true) {
                        if (_310 === 0) {
                            return _309;
                        };
                        var __tco__309 = _309 + "&nbsp;&nbsp;";
                        var __tco__310 = _310 - 1;
                        _309 = __tco__309;
                        _310 = __tco__310;
                        continue tco;
                    };
                };
            };
            return go("");
        })();
        var renderTerm = function (_308) {
            if (_308 instanceof Kanren_Term.TmVar) {
                return "#" + Prelude.show(Prelude.showNumber)(_308.value0);
            };
            if (_308 instanceof Kanren_Term.TmObj) {
                return _308.value0;
            };
            if (_308 instanceof Kanren_Term.TmPair) {
                return "(" + (renderTerm(_308.value0) + (", " + (renderTerm(_308.value1) + ")")));
            };
            throw new Error("Failed pattern match");
        };
        var renderGoal = function (_306) {
            return function (_307) {
                if (_307 instanceof Kanren_Goal.Fresh) {
                    return Prelude[":"](spaces(_306) + ("fresh " + (_307.value0 + "\n")))(renderGoal(_306)(_307.value1));
                };
                if (_307 instanceof Kanren_Goal.Unify) {
                    return [ spaces(_306) + (renderTerm(_307.value0) + (" == " + renderTerm(_307.value1))) ];
                };
                if (_307 instanceof Kanren_Goal.Disj) {
                    return Prelude["++"](Data_Array.semigroupArray)(Prelude[":"](spaces(_306) + "disj")(renderGoal(_306 + 1)(_307.value0)))(renderGoal(_306 + 1)(_307.value1));
                };
                if (_307 instanceof Kanren_Goal.Conj) {
                    return Prelude["++"](Data_Array.semigroupArray)(Prelude[":"](spaces(_306) + "conj")(renderGoal(_306 + 1)(_307.value0)))(renderGoal(_306 + 1)(_307.value1));
                };
                throw new Error("Failed pattern match");
            };
        };
        var expand = function (e) {
            return function (jq) {
                return function __do() {
                    var _7 = Control_Monad_JQuery.parent(jq)();
                    var _6 = Control_Monad_JQuery.create("<ul>")();
                    Data_Traversable["for"](Control_Monad_Eff.applicativeEff)(Data_Traversable.traversableArray)(Kanren_Eval.step(_305))(function (st$prime) {
                        return function __do() {
                            var _5 = render(st$prime)();
                            Control_Monad_JQuery.append(_5)(_6)();
                            return Control_Monad_JQuery.preventDefault(e)();
                        };
                    })();
                    return Control_Monad_JQuery.append(_6)(_7)();
                };
            };
        };
        return function __do() {
            var _10 = Control_Monad_JQuery.create("<li>")();
            Data_Traversable["for"](Control_Monad_Eff.applicativeEff)(Data_Traversable.traversableArray)(renderGoal(0)(_305.value0))(function (s) {
                return function __do() {
                    var _8 = Prelude[">>="](Control_Monad_Eff.bindEff)(Control_Monad_JQuery.create("<div>"))(Control_Monad_JQuery.appendText(s))();
                    return Control_Monad_JQuery.append(_8)(_10)();
                };
            })();
            var _9 = Control_Monad_JQuery.create("<a href='#'>More</a>")();
            Control_Monad_JQuery.on("click")(expand)(_9)();
            return Control_Monad_JQuery.append(_9)(_10)();
        };
    };
    return {
        render: render
    };
})();
var PS = PS || {};
PS.Main = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Control_Monad_Eff = PS.Control_Monad_Eff;
    var Control_Monad_JQuery = PS.Control_Monad_JQuery;
    var Kanren_Render = PS.Kanren_Render;
    var Kanren_Eval = PS.Kanren_Eval;
    var main = function __do() {
        var _12 = Control_Monad_JQuery.create("<ul>")();
        var _11 = Kanren_Render.render(Kanren_Eval.example)();
        Control_Monad_JQuery.append(_11)(_12)();
        return Prelude[">>="](Control_Monad_Eff.bindEff)(Control_Monad_JQuery.body)(Control_Monad_JQuery.append(_12))();
    };
    return {
        main: main
    };
})();