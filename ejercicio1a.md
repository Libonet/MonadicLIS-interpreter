---
title: "TP1: Lenguaje Imperativo Simple"
author: Grillo (G-5811/4), Libonati (L-3256/5), Maiza (M-7116/1)
date: 03/12/2024
geometry: margin=2cm
output: pdf_document
header-includes:
  - \usepackage{graphicx}
  - \usepackage{ebproof}
  - \usepackage{amsmath}
  - \usepackage{amssymb}
---

## EJERCICIO 1 a)

Vamos a demostrar que State es una mónada probando las tres leyes de mónadas para la instancia dada.

### Monad.1: return a >>= k = k a

$return~x >>= f$

$=~<\text{return.1}>$

$State~(\text{\textbackslash} s \rightarrow (x~\text{:!:}~s)) >>= f$

$=~<\text{(>>=).1}>$

$State~(\text{\textbackslash} s'' \rightarrow let (v~\text{:!:}~s') = runState~(State~(\text{\textbackslash} s \rightarrow (x~\text{:!:}~s)))~s'' \\
~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ in~runState~(f~v)~s'
)$

$=~<\text{Lema 1: } (runState.State) = (State.runState) = id\text{, id.1, Def. (.)}>$

$State~(\text{\textbackslash} s'' \rightarrow let (v~\text{:!:}~s') = (\text{\textbackslash} s \rightarrow (x~\text{:!:}~s))~s'' \\
~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ in~runState~(f~v)~s'
)$

$=~<\text{Aplicación}>$

$State~(\text{\textbackslash} s'' \rightarrow let (v~\text{:!:}~s') = (x~\text{:!:}~s'') \\
~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ in~runState~(f~v)~s'
)$

$=~<\text{Def. let}>$

$State~(\text{\textbackslash} s'' \rightarrow runState~(f~x)~s'')$

$=~<\text{Lema 2: (\textbackslash}x \rightarrow f x) = f>$

$State~(runState~(f~x))$

$=~<\text{Lema 1, id.1, Def. (.)}>$

$f x$