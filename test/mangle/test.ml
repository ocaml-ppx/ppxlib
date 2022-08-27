open Ppxlib;;

Mangle.mangle (`Prefix "pre") "foo";;
[%%expect{|
- : string = "pre_foo"
|}]

Mangle.mangle (`Suffix "suf") "foo";;
[%%expect{|
- : string = "foo_suf"
|}]

Mangle.mangle (`PrefixSuffix ("pre", "suf")) "foo";;
[%%expect{|
- : string = "pre_foo_suf"
|}]

Mangle.mangle (`Prefix "pre") "t";;
[%%expect{|
- : string = "pre"
|}]

Mangle.mangle (`Suffix "suf") "t";;
[%%expect{|
- : string = "suf"
|}]

Mangle.mangle (`PrefixSuffix ("pre", "suf")) "t";;
[%%expect{|
- : string = "pre_suf"
|}]

Mangle.mangle ~fixpoint:"foo" (`Prefix "pre") "foo";;
[%%expect{|
- : string = "pre"
|}]

Mangle.mangle ~fixpoint:"foo" (`Suffix "suf") "foo";;
[%%expect{|
- : string = "suf"
|}]

Mangle.mangle ~fixpoint:"foo" (`PrefixSuffix ("pre", "suf")) "foo";;
[%%expect{|
- : string = "pre_suf"
|}]
