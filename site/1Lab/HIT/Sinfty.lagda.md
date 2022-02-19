```
open import 1Lab.Path.Groupoid
open import 1Lab.HLevel
open import 1Lab.Path
open import 1Lab.Type

module 1Lab.HIT.Sinfty where
```

# The Infinite Sphere

The $(n+1)$-sphere can be constructed from the $n$-sphere via suspension.
By writing a recursive HIT, we can define a type which is its own
suspension. It stands to reason that this definition is a good
candidate for being conisdered the infinitary limit of the process of
iterated suspension and is thus referred to as the $\infty$-sphere.

```agda
data S∞ : Type where
  N S   : S∞
  merid : S∞ → N ≡ S
```

In classical topology, there are several definitions of the $\infty$-sphere.
However, independently of the approach taken, the resulting space is always
contractible. In Homotopy Type Theory, then, the only meaningful statement that
can be made of `S∞`{.Agda} is that it is contractible. We prove this in two
different ways.

## The Book HoTT Approach

```agda
open isContr

private
  pathsS∞′ : (x : S∞) → N ≡ x
  pathsS∞′ N = refl
  pathsS∞′ S = merid N
  pathsS∞′ (merid x i) =
```

First we reduce the problem from constructing a dependent path over
`(λ i → N ≡ merid x i)`{.Agda} from `refl`{.Agda} to `merid N`{.Agda}
to the problem of constructing a path in `N ≡ S`{.Agda} from
`transport (λ j → N ≡ merid x j) refl`{.Agda} to `merid N`{.Agda}.

```agda
    toPathP (λ j → N ≡ merid x j) refl (merid N)
```

The proof goes as follows: by the characterisation of transport in path
types the LHS is identified with `refl ∙ merid x`{.Agda}. We get rid of
the `refl`{.Agda} and then a a path between `merid x`{.Agda} and `merid
N`{.Agda} can be obtained from applying `merid`{.Agda} to the recursive
call `pathsS∞′ x`{.Agda}.

```agda
      (transport (λ j → N ≡ merid x j) refl ≡⟨ subst-path-right refl (merid x) ⟩
      refl ∙ merid x                        ≡⟨ ∙-id-l (merid x) ⟩
      merid x                               ≡⟨ ap merid (sym (pathsS∞′ x)) ⟩
      merid N                               ∎) i

isContrS∞′ : isContr S∞
isContrS∞′ .centre = N
isContrS∞′ .paths = pathsS∞′
```

## The Cubical Approach

The cubical approach essentially acomplishes the same thing as the previous
proof, without using any helper lemmas, by way of drawing a slightly clever
cube. The case of the defenition for the higher constructor requires a
square in which two of the sides are `merid N` and `merid x`. We start with
a square in which both of these sides are `merid N` (specifically
`merid N (i ∧ j)`), and then construct a cube in which one of the faces morphs
`merid N ` into `merid x`. This is something that we can easilly do since we
have a path `N ≡ x` via the recursive call `pathsS∞ x`.

```agda
private
  pathsS∞ : (x : S∞) → N ≡ x
  pathsS∞ N = refl
  pathsS∞ S = merid N
  pathsS∞ (merid x i) j =
    hcomp
      (λ k → λ { (i = i0) → N
               ; (i = i1) → merid N j
               ; (j = i0) → N
               ; (j = i1) → merid (pathsS∞ x k) i})
      (merid N (i ∧ j))

isContrS∞ : isContr S∞
isContrS∞ .centre = N
isContrS∞ .paths = pathsS∞
```
