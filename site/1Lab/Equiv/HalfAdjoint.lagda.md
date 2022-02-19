```agda
open import 1Lab.HLevel.Retracts
open import 1Lab.Path.Groupoid
open import 1Lab.Equiv.Biinv
open import 1Lab.Type.Sigma
open import 1Lab.Univalence
open import 1Lab.HLevel
open import 1Lab.Equiv
open import 1Lab.Path
open import 1Lab.Type

module 1Lab.Equiv.HalfAdjoint where
```

# Adjoint Equivalences

An **adjoint equivalence** is an [isomorphism] $(f, g, \eta,
\varepsilon)$ where the [homotopies] ($\eta$, $\varepsilon$) satisfy the
[triangle identities], thus witnessing $f$ and $g$ as [adjoint
functors]. In Homotopy Type Theory, we can use a _half_ adjoint
equivalence - satisfying only _one_ of the triangle identities - as a
[good notion of equivalence].

[isomorphism]: 1Lab.Equiv.html#isomorphisms-from-equivalences
[homotopies]: 1Lab.Path.html#dependent-functions
[triangle identities]: https://ncatlab.org/nlab/show/triangle+identities
[adjoint functors]: https://ncatlab.org/nlab/show/adjoint+functor
[good notion of equivalence]: 1Lab.Equiv.html#equivalences

```agda
isHAE : ∀ {ℓ₁ ℓ₂} {A : Type ℓ₁} {B : Type ℓ₂} (f : A → B) → Type _
isHAE {A = A} {B = B} f =
  Σ[ g ∈ (B → A) ]
  Σ[ η ∈ ((x : A) → g (f x) ≡ x) ]
  Σ[ ε ∈ ((y : B) → f (g y) ≡ y) ]
  ((x : A) → ap f (η x) ≡ ε (f x))
```

The argument is an adaptation of a standard result of both category
theory and homotopy theory - where we can "improve" an equivalence of
categories into an adjoint equivalence, or a homotopy equivalence into a
strong homotopy equivalence (Vogt's lemma). In HoTT, we show this
synthetically for equivalences between $\infty$-groupoids.

```agda
isIso→isHAE : ∀ {ℓ₁ ℓ₂} {A : Type ℓ₁} {B : Type ℓ₂} {f : A → B}
            → isIso f → isHAE f
isIso→isHAE {A = A} {B} {f} iiso = g , η , ε' , λ x → sym (zig x) where
  open isIso iiso renaming (inv to g ; linv to η ; rinv to ε)
```

For $g$ and $\eta$, we can take the values provided by `isIso`{.Agda}.
However, if we want $(\eta, \varepsilon)$ to satisfy the triangle
identities, we can not in general take $\varepsilon' = \varepsilon$.  We
can, however, alter it like thus:

```agda
  ε' : (y : B) → f (g y) ≡ y
  ε' y = sym (ε (f (g y))) ∙ ap f (η (g y)) ∙ ε y
```

Drawn as a diagram, the path above factors like:

~~~{.quiver}
\[\begin{tikzcd}
  {f(g(y))} && y \\
  {f(g(f(g(y))))} && {f(g(y))}
  \arrow["{\mathrm{sym}\ (\varepsilon(f(g(y))))}"', from=1-1, to=2-1]
  \arrow["{\mathrm{ap}\ f\ (\eta(g(y)))}"', from=2-1, to=2-3]
  \arrow["{\varepsilon \ y}"', from=2-3, to=1-3]
  \arrow["{\varepsilon'\ y}", dashed, from=1-1, to=1-3]
\end{tikzcd}\]
~~~

There is a great deal of redundancy in this definition, given that
$\varepsilon y$ and $\varepsilon' y$ have the same boundary! The point
is that while the definition of $\varepsilon$ is entirely opaque to us,
$\varepsilon'$ is written in such a way that we can use properties of
paths to make the $\mathrm{sym}\ (\varepsilon ...)$ and $\varepsilon$
cancel:

```agda
  zig : (x : A) → ε' (f x) ≡ ap f (η x)
  zig x =
    ε' (f x)                                                    ≡⟨⟩
    sym (ε (f (g (f x))))  ∙ ap f (η (g (f x)))   ∙ ε (f x)     ≡⟨ ap₂ _∙_ refl (ap₂ _∙_ (ap (ap f) (homotopy-invert η)) refl) ⟩
    sym (ε (f (g (f x))))  ∙ ap (f ∘ g ∘ f) (η x) ∙ ε (f x)     ≡⟨ ap₂ _∙_ refl (sym (homotopy-natural ε _)) ⟩
    sym (ε (f (g (f x))))  ∙ ε (f (g (f x)))      ∙ ap f (η x)  ≡⟨ ∙-cancel-l (ε (f (g (f x)))) (ap f (η x)) ⟩
    ap f (η x)                                                  ∎
```

The notion of `half-adjoint equivalence`{.Agda ident=isHAE} is a useful
stepping stone in writing a more comprehensible proof that `isomorphisms
are equivalences`{.Agda ident=Iso→Equiv}. Since this result is
fundamental, the proof we actually use is written with efficiency of
computation in mind - hence, cubically. The proof here is intended to be
more educational.

First, we give an equivalent characterisation of paths in
`fibre`{.Agda}s, which will be used in proving that `half adjoint
equivalences are equivalences`{.Agda ident=isHAE→isEquiv}.

```agda
fibre-paths : ∀ {ℓ₁ ℓ₂} {A : Type ℓ₁} {B : Type ℓ₂} {f : A → B} {y : B}
            → {f1 f2 : fibre f y}
            → (f1 ≡ f2)
            ≃ (Σ[ γ ∈ f1 .fst ≡ f2 .fst ] (ap f γ ∙ f2 .snd ≡ f1 .snd))
```

<details>
<summary>The proof of this is not very enlightening, but it's included
here (rather than being completely invisible) for
completeness:</summary>
```
fibre-paths {f = f} {y} {f1} {f2} =
  Path (fibre f y) f1 f2                                                       ≃⟨ Iso→Equiv Σ-Path-iso e⁻¹ ⟩
  (Σ[ γ ∈ f1 .fst ≡ f2 .fst ] (subst (λ x₁ → f x₁ ≡ _) γ (f1 .snd) ≡ f2 .snd)) ≃⟨ Σ-ap-snd (λ x → pathToEquiv (lemma x)) ⟩
  (Σ[ γ ∈ f1 .fst ≡ f2 .fst ] (ap f γ ∙ f2 .snd ≡ f1 .snd))                    ≃∎
  where
    helper : (p' : f (f1 .fst) ≡ y)
           → (subst (λ x → f x ≡ y) refl (f1 .snd) ≡ p')
           ≡ (ap f refl ∙ p' ≡ f1 .snd)
    helper p' =
      subst (λ x → f x ≡ y) refl (f1 .snd) ≡ p' ≡⟨ ap₂ _≡_ (transport-refl _) refl ⟩
      (f1 .snd) ≡ p'                            ≡⟨ Iso→path (sym , iso sym (λ x → refl) (λ x → refl)) ⟩
      p' ≡ f1 .snd                              ≡⟨ ap₂ _≡_ (sym (∙-id-l _)) refl ⟩
      refl ∙ p' ≡ f1 .snd                       ≡⟨⟩
      ap f refl ∙ p' ≡ f1 .snd                  ∎

    lemma : ∀ {x'} {p'} → (γ : f1 .fst ≡ x')
          → (subst (λ x → f x ≡ _) γ (f1 .snd) ≡ p')
          ≡ (ap f γ ∙ p' ≡ f1 .snd)
    lemma {x'} {p'} p =
      J (λ x' γ → ∀ p' → (subst (λ x → f x ≡ _) γ (f1 .snd) ≡ p')
                       ≡ (ap f γ ∙ p' ≡ f1 .snd))
        helper p p'
```
</details>

Then, given an element $y : B$, we can construct a fibre of of $f$, and,
using the above characterisation of paths, prove that this fibre is a
centre of contraction:

```agda
isHAE→isEquiv : ∀ {ℓ₁ ℓ₂} {A : Type ℓ₁} {B : Type ℓ₂} {f : A → B}
              → isHAE f → isEquiv f
isHAE→isEquiv {A = A} {B} {f} (g , η , ε , zig) .isEqv y = contr fib contract where
  fib : fibre f y
  fib = g y , ε y
```

The fibre is given by $(g(y), ε(y))$, which we can prove identical to
another $(x, p)$ using a very boring calculation:

```agda
  contract : (fib₂ : fibre f y) → fib ≡ fib₂
  contract (x , p) = (fibre-paths e⁻¹) .fst (x≡gy , path) where
    x≡gy = ap g (sym p) ∙ η x

    path : ap f (ap g (sym p) ∙ η x) ∙ p ≡ ε y
    path =
      ap f (ap g (sym p) ∙ η x) ∙ p               ≡⟨ ap₂ _∙_ (ap-comp-path f (ap g (sym p)) (η x)) refl ∙ sym (∙-assoc _ _ _) ⟩
      ap (f ∘ g) (sym p) ∙ ap f (η x) ∙ p         ≡⟨ ap₂ _∙_ refl (ap₂ _∙_ (zig _) refl) ⟩ -- by the triangle identity
      ap (f ∘ g) (sym p) ∙ ε (f x)    ∙ p         ≡⟨ ap₂ _∙_ refl (homotopy-natural ε p)  ⟩ -- by naturality of ε
```

The calculation of `path`{.Agda} factors as a bunch of boring
adjustments to paths using the groupoid structure of types, and the two
interesting steps above: The triangle identity says that
$\mathrm{ap}(f)(\eta x) = \varepsilon(f x)$, and naturality of
$\varepsilon$ lets us "push it past $p$" to get something we can cancel:

```agda
      ap (f ∘ g) (sym p) ∙ ap (f ∘ g) p ∙ ε y     ≡⟨ ∙-assoc _ _ _ ⟩
      (ap (f ∘ g) (sym p) ∙ ap (f ∘ g) p) ∙ ε y   ≡⟨ ap₂ _∙_ (sym (ap-comp-path (f ∘ g) (sym p) p)) refl ⟩
      ap (f ∘ g) (sym p ∙ p) ∙ ε y                ≡⟨ ap₂ _∙_ (ap (ap (f ∘ g)) (∙-inv-r _)) refl ⟩
      ap (f ∘ g) refl ∙ ε y                       ≡⟨⟩
      refl ∙ ε y                                  ≡⟨ ∙-id-l (ε y) ⟩
      ε y                                         ∎
```

Putting these together, we get an alternative definition of
`isIso→isEquiv`{.Agda}:

```agda
isIso→isEquiv' : ∀ {ℓ₁ ℓ₂} {A : Type ℓ₁} {B : Type ℓ₂} {f : A → B}
               → isIso f → isEquiv f
isIso→isEquiv' = isHAE→isEquiv ∘ isIso→isHAE
```

<!--
```agda
_ = isIso→isEquiv

equiv→zig : ∀ {ℓ₁ ℓ₂} {A : Type ℓ₁} {B : Type ℓ₂} {f : A → B}
          → (eqv : isEquiv f) (a : A)
          → ap f (equiv→retraction eqv a) ≡ equiv→section eqv (f a)
equiv→zig {f = f} eqv = commPathIsEq where
  commSqIsEq : ∀ a → Square (sym (ap f (equiv→retraction eqv a)))
                            refl
                            (equiv→section eqv (f a))
                            refl
  commSqIsEq a i = eqv .isEqv (f a) .paths (a , refl) (~ i) .snd

  commPathIsEq : ∀ a → ap f (equiv→retraction eqv a) ≡ equiv→section eqv (f a)
  commPathIsEq a i j =
    hcomp
      (λ k → λ
        { (i = i1) → equiv→section eqv (f a) j
        ; (i = i0) → f (equiv→retraction eqv a (j ∨ ~ k))
        ; (j = i0) → f (equiv→retraction eqv a (~ i ∧ ~ k))
        ; (j = i1) → f a
        })
      (commSqIsEq a i j)

isEquiv→isHAE : ∀ {ℓ₁ ℓ₂} {A : Type ℓ₁} {B : Type ℓ₂} {f : A → B}
              → isEquiv f → isHAE f
isEquiv→isHAE {f = f} eqv =
    equiv→inverse eqv
  , equiv→retraction eqv
  , equiv→section eqv
  , equiv→zig eqv
```
-->
